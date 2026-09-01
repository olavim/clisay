use crate::compiler_error;
use crate::middle::hir::{HirCatchClause, HirExpr, HirSayDecl, HirId, HirMatcher, HirStmt};
use crate::middle::ir::Inst;
use crate::middle::bind::FnKind;

use super::{Compiler, PendingDefer, TryCatchPosition, TryFrame};

/// Whether a statement's name takes a slot the hoisting run reserves at the top of its scope.
fn reserves_a_slot(stmt: &HirStmt) -> bool {
    match stmt {
        HirStmt::Fn(_) => true,
        HirStmt::Type(decl) => decl.builtin.is_none(),
        _ => false,
    }
}

impl<'a> Compiler<'a> {
    pub (super) fn statement(&mut self, stmt_id: &HirId<HirStmt>) -> Result<(), anyhow::Error> {
        self.frame_slot_count = self.bindings.frame_slot_count_at(stmt_id) as usize;

        match self.hir.get(stmt_id) {
            HirStmt::Nop => {},
            HirStmt::Return(expr) => {
                // If returning from a try or catch block with a finally block, inline the finally block before returning.
                if let Some(TryFrame {
                    position: pos @ (TryCatchPosition::Try | TryCatchPosition::Catch),
                    finally: Some(finally)
                }) = self.try_frames.last().cloned() {
                    if matches!(pos, TryCatchPosition::Try) {
                        self.emit(Inst::PopTry, stmt_id);
                    }

                    let idx = self.try_frames.len() - 1;
                    self.try_frames[idx].position = TryCatchPosition::Finally;
                    self.inline_block(&finally)?;
                    self.try_frames[idx].position = pos;
                }

                // A factory populates the pre-allocated `this` and hands it back via `RETURN_FAC`,
                // which seals it per the frame bit.
                if let FnKind::Factory = *self.fn_kinds.last().unwrap() {
                    if expr.is_some() {
                        compiler_error!(self, stmt_id, "Cannot return a value from a factory");
                    }
                    self.emit_pending_defers()?;
                    self.emit(Inst::LoadLocal(0), stmt_id);
                    self.emit(Inst::ReturnFac, stmt_id);
                } else {
                    if let Some(expr) = expr {
                        if !self.emit_as_tail_call(expr)? {
                            self.expression(expr)?;
                            self.emit_defers_over_return_value(stmt_id)?;
                        }
                    } else {
                        self.emit_pending_defers()?;
                        self.emit(Inst::PushNull, stmt_id);
                    }
                    self.emit(Inst::Return, stmt_id);
                }
            },
            HirStmt::Throw(expr) => {
                if let Some(TryFrame {
                    position: TryCatchPosition::Catch,
                    finally: Some(finally)
                }) = self.try_frames.last().cloned() {
                    self.compile_finally(&finally)?;
                }

                self.expression(expr)?;
                self.emit(Inst::Throw, stmt_id);
            },
            HirStmt::Try(try_body, catch, finally) => {
                self.try_frames.push(TryFrame {
                    position: TryCatchPosition::Try,
                    finally: *finally
                });

                let HirExpr::Block(stmts) = self.hir.get(try_body) else { unreachable!() };
                if !stmts.is_empty() {
                    let node_id = &stmts[0];
                    let handler = self.ir.new_label();
                    self.emit(Inst::PushTry(handler), node_id);

                    self.expression_stmt(try_body)?;

                    self.emit(Inst::PopTry, node_id);
                    let end = self.ir.new_label();
                    self.emit(Inst::Jump(end), node_id);
                    self.ir.bind(handler);

                    if let Some(catch) = catch {
                        self.compile_catch(catch)?;
                    } else {
                        if let Some(finally) = finally {
                            self.compile_finally(finally)?;
                        }
                        self.emit(Inst::Throw, node_id);
                    }

                    self.ir.bind(end);
                }

                if let Some(finally) = finally {
                    self.compile_finally(finally)?;
                }

                self.try_frames.pop();
            },
            HirStmt::Fn(decl) => {
                // The slot was reserved by hoisting so forward references resolve.
                let slot = self.bindings.slot(stmt_id);

                let const_idx = self.function(stmt_id, (*stmt_id).into(), decl, FnKind::Function, self.declared_masks(stmt_id, decl))?;
                self.emit(Inst::PushClosure(const_idx), stmt_id);

                // Store the closure into the reserved slot and discard the placeholder.
                self.emit(Inst::StoreLocal(slot), stmt_id);
                self.emit(Inst::Pop, stmt_id);
            },
            HirStmt::Type(decl) => self.type_declaration(stmt_id, decl)?,
            // Traits emit no runtime type; they exist only for self-containment validation in resolve.
            HirStmt::Trait(_) => {},
            HirStmt::Say(field @ HirSayDecl { value, .. }) => {
                let slot = self.bindings.slot(stmt_id);
                let accepts = self.accepts_index(&field.clause.owed(), field.nullable)?;
                self.ir.record_slot_accepts(self.slot_table, slot, accepts);

                let inst = if let Some(expr) = value {
                    self.expression(expr)?;
                    Inst::StoreLocal(slot)
                } else {
                    Inst::LoadLocal(slot)
                };
                self.emit(inst, stmt_id);
                if let (Some(pattern), Some(value)) = (&field.pattern, value) {
                    self.compile_say_pattern(pattern, value, stmt_id, &field.otherwise)?;
                }
            },
            HirStmt::Expression(expr) | HirStmt::Discard(expr) => {
                self.expression_stmt(expr)?;
            },
            HirStmt::While(cond, body) => {
                let binders = self.hir.condition_pattern_binders(cond);
                if !binders.is_empty() {
                    return self.compile_binding_while(cond, body, binders.len(), stmt_id);
                }
                let loop_start = self.ir.new_label();
                self.ir.bind(loop_start);

                let exit = self.emit_conditional_jump(cond, stmt_id)?;

                self.expression_stmt(body)?;

                self.emit(Inst::Jump(loop_start), stmt_id);
                self.ir.bind(exit);
            },
            HirStmt::If(cond, then, otherwise) => {
                let binders = self.hir.condition_pattern_binders(cond);
                if !binders.is_empty() {
                    return self.compile_binding_if(cond, then, otherwise, binders.len(), stmt_id);
                }
                let else_target = self.emit_conditional_jump(cond, stmt_id)?;

                self.expression_stmt(then)?;

                if let Some(otherwise) = otherwise {
                    let end = self.ir.new_label();
                    self.emit(Inst::Jump(end), stmt_id);
                    self.ir.bind(else_target);
                    self.statement(otherwise)?;
                    self.ir.bind(end);
                } else {
                    self.ir.bind(else_target);
                }
            },
            HirStmt::Block(body) => {
                self.expression_stmt(body)?;
            },
            HirStmt::Defer(_) => unreachable!("a defer is emitted by the block that registered it"),
            HirStmt::Match(scrutinee, arms) => self.compile_match(scrutinee, arms, stmt_id)?,
        };

        Ok(())
    }

    /// Compiles an `if` whose condition binds names.
    fn compile_binding_if(&mut self, cond: &HirId<HirExpr>, then: &HirId<HirExpr>, otherwise: &Option<HirId<HirStmt>>, binder_count: usize, stmt_id: &HirId<HirStmt>) -> Result<(), anyhow::Error> {
        self.reserve_slots(binder_count, stmt_id);
        self.expression(cond)?;
        let else_target = self.ir.new_label();
        self.emit(Inst::JumpIfFalse(else_target), stmt_id);
        self.expression_stmt(then)?;
        match otherwise {
            Some(otherwise) => {
                self.exit_scope(cond)?;
                let end = self.ir.new_label();
                self.emit(Inst::Jump(end), stmt_id);
                self.ir.bind(else_target);
                self.exit_scope(cond)?;
                self.statement(otherwise)?;
                self.ir.bind(end);
            },
            // Both paths converge before the single cleanup.
            None => {
                self.ir.bind(else_target);
                self.exit_scope(cond)?;
            },
        }
        Ok(())
    }

    /// Compiles a `while` whose condition binds names.
    fn compile_binding_while(&mut self, cond: &HirId<HirExpr>, body: &HirId<HirExpr>, binder_count: usize, stmt_id: &HirId<HirStmt>) -> Result<(), anyhow::Error> {
        let loop_start = self.ir.new_label();
        self.ir.bind(loop_start);
        self.reserve_slots(binder_count, stmt_id);
        self.expression(cond)?;
        let exit = self.ir.new_label();
        self.emit(Inst::JumpIfFalse(exit), stmt_id);
        self.expression_stmt(body)?;
        self.exit_scope(cond)?;
        self.emit(Inst::Jump(loop_start), stmt_id);
        self.ir.bind(exit);
        self.exit_scope(cond)?;
        Ok(())
    }

    pub(super) fn reserve_slots<T: 'static>(&mut self, count: usize, node: &HirId<T>) {
        for _ in 0..count {
            self.emit(Inst::PushNull, node);
        }
    }

    fn statement_body(&mut self, body: &Vec<HirId<HirStmt>>) -> Result<(), anyhow::Error> {
        self.hoist_declarations(body)?;

        // A declaration is built as soon as the locals its closures capture are live. One that
        // captures nothing is built at the top of the scope, so its name works above its own line.
        let mut waiting: Vec<(usize, u8)> = body.iter().enumerate()
            .filter(|(_, s)| reserves_a_slot(self.hir.get(s)))
            .map(|(i, s)| (i, self.captured_slots(s).max().unwrap_or(0)))
            .collect();

        // The hoisting run above reserved every declaration's slot, so those are already live.
        let mut live = body.iter().filter(|s| reserves_a_slot(self.hir.get(s)))
            .map(|s| self.bindings.slot(s)).max().unwrap_or(0);

        self.build_ready_declarations(body, &mut waiting, live)?;
        let defer_mark = self.defers.len();
        for (i, stmt_id) in body.iter().enumerate() {
            match waiting.iter().position(|&(w, _)| w == i) {
                // A declaration still waiting at its own line is built here.
                Some(pos) => { waiting.remove(pos); },
                // A declaration missing from the list was built above.
                None if reserves_a_slot(self.hir.get(stmt_id)) => continue,
                None => {},
            }
            if let HirStmt::Defer(defer_body) = self.hir.get(stmt_id) {
                let handler = self.ir.new_label();
                // Catches an error.
                self.emit(Inst::PushDeferTry(handler), stmt_id);
                self.defers.push(PendingDefer {
                    stmt: *stmt_id,
                    body: *defer_body,
                    handler,
                    unwind_slot_count: self.frame_slot_count,
                });
                continue;
            }
            self.statement(stmt_id)?;
            if let HirStmt::Say(_) = self.hir.get(stmt_id) {
                live = live.max(self.bindings.slot(stmt_id));
                self.build_ready_declarations(body, &mut waiting, live)?;
            }
        }

        // Defers run at scope end, in reverse order.
        for i in (defer_mark..self.defers.len()).rev() {
            let pending = self.defers[i];
            self.emit(Inst::PopTry, &pending.stmt);
            self.emit_defer_body(&pending)?;
        }
        self.emit_defer_try_handlers(defer_mark)?;
        self.defers.truncate(defer_mark);
        Ok(())
    }

    fn emit_as_tail_call(&mut self, expr: &HirId<HirExpr>) -> Result<bool, anyhow::Error> {
        let HirExpr::Call(callee, args) = self.hir.get(expr) else { return Ok(false) };
        if !self.defers.is_empty() || !self.try_frames.is_empty() {
            return Ok(false);
        }
        if let Some(FnKind::Factory) = self.fn_kinds.last() {
            return Ok(false);
        }
        self.call_expression(callee, args, false, true)?;
        Ok(true)
    }

    fn compile_say_pattern(&mut self, pattern: &HirId<HirMatcher>, value: &HirId<HirExpr>, stmt_id: &HirId<HirStmt>, otherwise: &Option<HirId<HirExpr>>) -> Result<(), anyhow::Error> {
        let binders = self.bindings.match_binders(value).unwrap_or_default();
        self.reserve_slots(binders.len(), stmt_id);
        self.emit(Inst::LoadLocal(self.bindings.slot(stmt_id)), stmt_id);
        self.compile_binding_matcher(pattern, binders, value)?;
        match (self.hir.get(pattern).is_irrefutable(self.hir), otherwise) {
            (true, _) => self.emit(Inst::Pop, stmt_id),
            (false, Some(otherwise)) => {
                let matched_label = self.emit_pattern_mismatch_jumps(stmt_id);
                self.expression_stmt(otherwise)?;
                self.ir.bind(matched_label);
            },
            (false, None) => self.abort_on_pattern_mismatch(
                format!("value does not match `{}`", self.hir.pos(pattern).snippet()), stmt_id)?,
        }
        Ok(())
    }

    fn emit_defer_body(&mut self, pending: &PendingDefer) -> Result<(), anyhow::Error> {
        // The frame slots live when the defer runs.
        let body_base = self.bindings.frame_slot_count_at(&pending.stmt) as usize;
        self.set_frame_slot_count(body_base, &pending.stmt);
        self.expression_stmt(&pending.body)?;
        self.frame_slot_count = body_base;
        Ok(())
    }

    fn emit_defer_try_handlers(&mut self, defer_mark: usize) -> Result<(), anyhow::Error> {
        let Some(&PendingDefer { stmt: site, .. }) = self.defers.get(defer_mark) else { return Ok(()) };
        let slot = self.parked_slot();
        let past = self.ir.new_label();
        self.emit(Inst::Jump(past), &site);
        let after_block = self.frame_slot_count;
        for i in (defer_mark..self.defers.len()).rev() {
            let pending = self.defers[i];
            self.ir.bind(pending.handler);
            // The unwind left the error where the body's own locals go, so park it first.
            self.emit(Inst::StoreLocalPop(slot), &pending.stmt);
            self.frame_slot_count = pending.unwind_slot_count;
            self.emit_defer_body(&pending)?;
            self.emit(Inst::LoadLocal(slot), &pending.stmt);
            self.emit(Inst::Throw, &pending.stmt);
        }
        self.frame_slot_count = after_block;
        self.ir.bind(past);
        Ok(())
    }

    fn parked_slot(&self) -> u8 {
        self.defer_parked_slot.expect("a frame holding a defer reserved a slot to park values in")
    }

    pub(super) fn open_defer_frame(&mut self, body: &HirId<HirExpr>) {
        self.defer_parked_slot = self.bindings.defer_parked_slot(body);
        if self.defer_parked_slot.is_some() {
            self.reserve_slots(1, body);
        }
    }

    fn emit_pending_defers(&mut self) -> Result<(), anyhow::Error> {
        let caller_height = self.frame_slot_count;

        for i in (0..self.defers.len()).rev() {
            let pending = self.defers[i];
            self.emit(Inst::PopTry, &pending.stmt);
            self.emit_defer_body(&pending)?;
        }

        self.frame_slot_count = caller_height;
        Ok(())
    }

    fn set_frame_slot_count(&mut self, want: usize, node: &HirId<HirStmt>) {
        let have = self.frame_slot_count;
        if want > have {
            self.reserve_slots(want - have, node);
        } else {
            for _ in want..have {
                self.emit(Inst::Pop, node);
            }
        }
    }

    /// Runs the pending `defer` bodies with the returned value parked in the frame's reserved slot.
    pub(super) fn emit_defers_over_return_value<T: 'static>(&mut self, node: &HirId<T>) -> Result<(), anyhow::Error> {
        if self.defers.is_empty() {
            return Ok(());
        }
        let slot = self.parked_slot();
        self.emit(Inst::StoreLocalPop(slot), node);
        self.emit_pending_defers()?;
        self.emit(Inst::LoadLocal(slot), node);
        Ok(())
    }

    fn build_ready_declarations(&mut self, body: &[HirId<HirStmt>], waiting: &mut Vec<(usize, u8)>, live: u8) -> Result<(), anyhow::Error> {
        while let Some(pos) = waiting.iter().position(|&(_, needs)| needs <= live) {
            let (index, _) = waiting.remove(pos);
            self.statement(&body[index])?;
        }
        Ok(())
    }

    /// The frame slots a declaration's closures capture. An upvalue of the enclosing frame reads
    /// the closure's own array rather than a slot, so it never holds a declaration back.
    fn captured_slots(&self, stmt: &HirId<HirStmt>) -> impl Iterator<Item = u8> + '_ {
        let bodies: Vec<HirId<HirExpr>> = match self.hir.get(stmt) {
            HirStmt::Fn(decl) => vec![decl.body],
            HirStmt::Type(decl) => std::iter::once(&decl.init).chain(&decl.methods)
                .filter_map(|s| match self.hir.get(s) {
                    HirStmt::Fn(decl) => Some(decl.body),
                    _ => None,
                })
                .collect(),
            _ => Vec::new(),
        };
        bodies.into_iter()
            .flat_map(|b| self.bindings.upvalues(&b).to_vec())
            .filter(|u| u.is_local)
            .map(|u| u.location)
    }

    pub (super) fn scoped_body<T: 'static>(&mut self, body: &Vec<HirId<HirStmt>>, node_id: &HirId<T>) -> Result<(), anyhow::Error> {
        self.statement_body(body)?;
        self.exit_scope(node_id)?;
        Ok(())
    }

    fn inline_block(&mut self, body: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let HirExpr::Block(stmts) = self.hir.get(body) else { unreachable!() };
        self.statement_body(stmts)
    }

    fn compile_catch(&mut self, catch: &HirCatchClause) -> Result<(), anyhow::Error> {
        let idx = self.try_frames.len() - 1;
        self.try_frames[idx].position = TryCatchPosition::Catch;

        self.inline_block(&catch.body)?;
        self.exit_scope(&catch.body)?;
        Ok(())
    }

    fn compile_finally(&mut self, finally: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let idx = self.try_frames.len() - 1;
        self.try_frames[idx].position = TryCatchPosition::Finally;
        self.expression_stmt(finally)
    }

    fn hoist_declarations(&mut self, body: &Vec<HirId<HirStmt>>) -> Result<(), anyhow::Error> {
        for stmt_id in body {
            if reserves_a_slot(self.hir.get(stmt_id)) {
                self.emit(Inst::PushNull, stmt_id);
            }
        }
        Ok(())
    }
}

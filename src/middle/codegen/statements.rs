use std::collections::HashMap;

use crate::compiler_error;
use crate::middle::hir::{HirCatchClause, HirExpr, HirFnDecl, HirSayDecl, HirId, HirMatcher, HirParam, HirStmt, Symbol};
use crate::middle::ir::{self, Inst};
use crate::middle::obligations::Obligations;
use crate::middle::obligations::ObligationRule;
use crate::middle::bind::{FnKind, Place};
use crate::middle::check::CheckedSlot;

use super::{Compiler, DeferDecl, QueuedDefer, Returning, TryCatchPosition, TryFrame};


impl<'a> Compiler<'a> {
    pub (super) fn statement(&mut self, stmt_id: &HirId<HirStmt>) -> Result<(), anyhow::Error> {
        self.expect_frame_stack_height(self.bindings.frame_stack_height_at(stmt_id) as usize, "statement start", stmt_id);

        match self.hir.get(stmt_id) {
            HirStmt::Nop => {},
            HirStmt::Return(expr) => {
                let is_factory = matches!(self.compiled_fns.last().unwrap().kind, FnKind::Factory);
                if is_factory && expr.is_some() {
                    compiler_error!(self, stmt_id, "Cannot return a value from a factory");
                }

                let returns_nothing = self.compiled_fns.last().is_some_and(|f| !self.barriers.returns_value(f.callable));
                if let Some(returned) = expr.as_ref().filter(|_| returns_nothing) {
                    self.discard_result_of(returned);
                }

                if let Some(expr) = expr {
                    if self.emit_tail_return(expr, stmt_id)? {
                        return Ok(());
                    }
                }

                if let Some(expr) = expr {
                    self.expression(expr)?;
                }

                self.emit_return_with_cleanup(expr.is_some(), stmt_id, |c| match expr {
                    Some(expr) => c.emit_return(Returning::Value(expr), stmt_id),
                    None if is_factory => c.emit_factory_return(stmt_id),
                    None => {
                        c.emit(Inst::PushNull, stmt_id);
                        c.emit_return(Returning::Nothing, stmt_id);
                    },
                })?;
            },
            HirStmt::Throw(expr) => {
                self.expression(expr)?;
                self.emit(Inst::Throw, stmt_id);
            },
            HirStmt::Try(try_body, catch, finally) => {
                self.try_frames.push(TryFrame {
                    position: TryCatchPosition::Try,
                    finally: *finally,
                    stmt: *stmt_id,
                    pending_defer_count: self.defers.len(),
                });

                let HirExpr::Block(stmts) = self.hir.get(try_body) else { unreachable!() };
                if !stmts.is_empty() {
                    let node_id = &stmts[0];
                    let handler = self.ir.new_label();
                    self.push_handler(handler, node_id)?;
                    self.expression_stmt(try_body)?;
                    self.pop_handler();
                    let end = self.ir.new_label();
                    self.emit(Inst::Jump(end), node_id);
                    self.bind_label(handler);

                    if let Some(catch) = catch {
                        self.compile_catch(catch)?;
                    } else {
                        if finally.is_some() {
                            self.compile_finally(true, node_id)?;
                        }
                        self.emit(Inst::Throw, node_id);
                    }

                    self.bind_label(end);
                }

                if finally.is_some() {
                    self.compile_finally(false, stmt_id)?;
                }

                self.try_frames.pop();
            },
            HirStmt::Fn(_) => unreachable!("a function is emitted by the block that hoists it"),
            HirStmt::Type(decl) => { self.type_template(stmt_id, decl)?; },
            HirStmt::Trait(_) => {},
            HirStmt::Say(field @ HirSayDecl { value, .. }) => {
                let slot = self.bindings.slot(stmt_id);
                self.record_slot_witness_set(slot, self.checked_slot_clause(CheckedSlot::Say(*stmt_id)))?;

                let inst = if let Some(expr) = value {
                    self.expression(expr)?;
                    match self.barriers.builds_its_value(expr) {
                        true => Inst::StoreLocalFresh(slot),
                        false => Inst::StoreLocal(slot),
                    }
                } else {
                    Inst::PushUnassigned
                };
                self.emit(inst, stmt_id);
                if let (Some(pattern), Some(value)) = (&field.pattern, value) {
                    self.compile_say_pattern(pattern, value, stmt_id, &field.otherwise)?;
                }
            },
            HirStmt::Expression(expr) => self.expression_stmt(expr)?,
            HirStmt::Discard(expr) => self.discard_stmt(expr)?,
            HirStmt::While(cond, body) => {
                let binders = self.hir.condition_pattern_binders(cond);
                if !binders.is_empty() {
                    return self.compile_binding_while(cond, body, binders.len(), stmt_id);
                }
                let loop_start = self.ir.new_label();
                self.bind_label(loop_start);

                let exit = self.emit_conditional_jump(cond, stmt_id)?;

                self.expression_stmt(body)?;

                self.emit(Inst::Jump(loop_start), stmt_id);
                self.bind_label(exit);
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
                    self.bind_label(else_target);
                    self.statement(otherwise)?;
                    self.bind_label(end);
                } else {
                    self.bind_label(else_target);
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
                self.bind_label(else_target);
                self.exit_scope(cond)?;
                self.statement(otherwise)?;
                self.bind_label(end);
            },
            // Both paths converge before the single cleanup.
            None => {
                self.bind_label(else_target);
                self.exit_scope(cond)?;
            },
        }
        Ok(())
    }

    /// Compiles a `while` whose condition binds names.
    fn compile_binding_while(&mut self, cond: &HirId<HirExpr>, body: &HirId<HirExpr>, binder_count: usize, stmt_id: &HirId<HirStmt>) -> Result<(), anyhow::Error> {
        let loop_start = self.ir.new_label();
        self.bind_label(loop_start);
        self.reserve_slots(binder_count, stmt_id);
        self.expression(cond)?;
        let exit = self.ir.new_label();
        self.emit(Inst::JumpIfFalse(exit), stmt_id);
        self.expression_stmt(body)?;
        self.exit_scope(cond)?;
        self.emit(Inst::Jump(loop_start), stmt_id);
        self.bind_label(exit);
        self.exit_scope(cond)?;
        Ok(())
    }

    pub(super) fn reserve_slots<T: 'static>(&mut self, count: usize, node: &HirId<T>) {
        for _ in 0..count {
            self.emit(Inst::PushNull, node);
        }
    }

    fn statement_body(&mut self, body: &Vec<HirId<HirStmt>>) -> Result<(), anyhow::Error> {
        let binds = self.hoist_declarations(body)?;
        let pending_defer_count = self.defers.len();
        for stmt_id in body {
            if self.hir.get(stmt_id).declares_slot() {
                match binds.get(&stmt_id.index()) {
                    Some(Some(bind)) => self.emit(*bind, stmt_id),
                    Some(None) => {},
                    None => self.build_declaration(stmt_id)?,
                }
                continue;
            }
            if let HirStmt::Defer(defer_body) = self.hir.get(stmt_id) {
                let handler = self.ir.new_label();
                self.push_handler(handler, stmt_id)?;
                self.defers.push(DeferDecl {
                    stmt: *stmt_id,
                    body: *defer_body,
                    handler,
                });
                continue;
            }
            self.statement(stmt_id)?;
        }

        // Defers run at scope end, in reverse order.
        let block_handlers = self.handlers.clone();
        for i in (pending_defer_count..self.defers.len()).rev() {
            let pending = self.defers[i];
            self.pop_handler();
            self.emit_defer_body(&pending)?;
        }

        // A throw from a defer's handler still runs the defers registered before it.
        let outer = block_handlers.len() - (self.defers.len() - pending_defer_count);
        for i in pending_defer_count..self.defers.len() {
            self.queued_defers.push(QueuedDefer {
                decl: self.defers[i],
                outer_handlers: block_handlers[..outer + i - pending_defer_count].to_vec(),
                handle_binder_slots: self.handle_binder_slots.clone(),
            });
        }
        self.defers.truncate(pending_defer_count);
        Ok(())
    }

    /// Compiles `return expr` as a tail call where it can, and says whether it did.
    fn emit_tail_return(&mut self, expr: &HirId<HirExpr>, stmt_id: &HirId<HirStmt>) -> Result<bool, anyhow::Error> {
        let HirExpr::Call(callee, args) = self.hir.get(expr) else { return Ok(false) };

        // A value tested on its way out needs the frame.
        if self.return_needs_check(Returning::Value(expr)) {
            return Ok(false);
        }

        if !self.defers.is_empty() || !self.try_frames.is_empty() {
            return Ok(false);
        }

        // A failed safe access jumps past the call, so the call cannot be a tail call.
        if self.hir.path_has_safe_access(callee) {
            return Ok(false);
        }

        if !args.iter().all(|arg| self.anchor_survives_tail_call(arg)) {
            return Ok(false);
        }

        if let Some(FnKind::Factory) = self.compiled_fns.last().map(|f| f.kind) {
            return Ok(false);
        }

        if !self.call_expression(callee, args, true)? {
            self.emit_anchor_copy_outs(stmt_id);
        }
        self.emit(Inst::Return, stmt_id);
        Ok(true)
    }

    fn compile_say_pattern(&mut self, pattern: &HirId<HirMatcher>, value: &HirId<HirExpr>, stmt_id: &HirId<HirStmt>, otherwise: &Option<HirId<HirExpr>>) -> Result<(), anyhow::Error> {
        let binders = self.bindings.match_binders(value).unwrap_or_default();
        self.record_pattern_binder_slot_witness_sets(pattern, binders)?;
        self.reserve_slots(binders.len(), stmt_id);
        self.emit(Inst::LoadLocal(self.bindings.slot(stmt_id)), stmt_id);
        self.compile_binding_matcher(pattern, binders, value)?;
        match (self.hir.get(pattern).is_irrefutable(self.hir), otherwise) {
            (true, _) => self.emit(Inst::Pop, stmt_id),
            (false, Some(otherwise)) => {
                let matched_label = self.emit_pattern_mismatch_jumps(stmt_id);
                // The `else` leaves the frame, so the value and binder slots go before it runs.
                self.set_frame_stack_height(self.bindings.frame_stack_height_at(stmt_id) as usize, stmt_id);
                self.expression_stmt(otherwise)?;
                self.bind_label(matched_label);
            },
            (false, None) => self.abort_on_pattern_mismatch(
                format!("value does not match `{}`", self.hir.pos(pattern).snippet()), stmt_id)?,
        }
        Ok(())
    }

    fn emit_defer_body(&mut self, pending: &DeferDecl) -> Result<(), anyhow::Error> {
        // The frame slots live when the defer runs.
        let body_base = self.bindings.frame_stack_height_at(&pending.stmt) as usize;
        self.set_frame_stack_height(body_base, &pending.stmt);
        self.expression_stmt(&pending.body)?;
        self.expect_frame_stack_height(body_base, "defer body end", &pending.stmt);
        Ok(())
    }

    /// Emits the frame's queued defer handlers. A body may queue more as it is emitted.
    pub(super) fn emit_queued_defers(&mut self) -> Result<(), anyhow::Error> {
        while let Some(queued) = self.queued_defers.pop() {
            debug_assert!(matches!(self.ir.code().last(), Some(Inst::Return | Inst::ReturnShared | Inst::ReturnFac | Inst::Halt | Inst::Throw)),
                "the code before a defer's handler should end in a return, halt or throw");
            // Only a throw reaches the handler, so the height is the one the throw arrives with.
            self.frame_stack_height = None;
            let slot = self.require_defer_slot();
            let pending = queued.decl;
            self.replace_handlers(queued.outer_handlers);
            self.handle_binder_slots = queued.handle_binder_slots;
            self.bind_label(pending.handler);
            self.emit(Inst::StoreTempPop(slot), &pending.stmt);
            self.emit_defer_body(&pending)?;
            self.emit(Inst::LoadLocal(slot), &pending.stmt);
            self.emit(Inst::Throw, &pending.stmt);
        }
        self.replace_handlers(Vec::new());
        Ok(())
    }

    pub(super) fn require_defer_slot(&self) -> u8 {
        self.defer_slot.expect("a frame running deferred work reserved a slot for the value leaving it")
    }

    fn record_slot_witness_set(&mut self, slot: u8, owed: &Obligations) -> Result<(), anyhow::Error> {
        let witness_set_pool_id = self.witness_set_pool_id(owed)?;
        self.ir.push_slot_witness_set(self.current_slot_witness_set_pool_id, slot, witness_set_pool_id);
        if self.slot_witness_set_pool_ids.len() <= slot as usize {
            self.slot_witness_set_pool_ids.resize(slot as usize + 1, ir::NO_WITNESS_SET);
        }
        self.slot_witness_set_pool_ids[slot as usize] = witness_set_pool_id;
        Ok(())
    }

    pub(super) fn slot_witness_set_pool_id(&self, slot: u8) -> u16 {
        self.slot_witness_set_pool_ids.get(slot as usize).copied().unwrap_or(ir::NO_WITNESS_SET)
    }

    fn checked_slot_clause(&self, slot: CheckedSlot) -> &'a Obligations {
        self.barriers.checked_slot_clause(slot).expect("the check pass records what every checked slot owes")
    }

    pub(super) fn record_param_slot_witness_sets(&mut self, params: &[HirParam]) -> Result<(), anyhow::Error> {
        for param in params {
            let Some(Place::Local(slot)) = self.bindings.place_of(&param.name) else { continue };
            self.record_slot_witness_set(slot, self.checked_slot_clause(CheckedSlot::Param(param.name)))?;
        }
        Ok(())
    }

    pub(super) fn record_pattern_binder_slot_witness_sets(&mut self, pattern: &HirId<HirMatcher>, binders: &[(Symbol, u8)]) -> Result<(), anyhow::Error> {
        for &(name, slot) in binders {
            self.record_slot_witness_set(slot, self.checked_slot_clause(CheckedSlot::PatternBinding(*pattern, name)))?;
        }
        Ok(())
    }

    pub(super) fn record_receiver_slot_witness_set(&mut self, decl: &HirFnDecl) -> Result<(), anyhow::Error> {
        if decl.receiver.is_none() {
            return Ok(());
        }
        let copy = self.bindings.anchor_params(&decl.body).iter().find(|param| param.anchor_slot == 0);
        self.record_slot_witness_set(copy.map_or(0, |copy| copy.value_slot), self.checked_slot_clause(CheckedSlot::Receiver(decl.body)))
    }

    fn param_admits_no_persist(&self, position: u8) -> bool {
        let callable = self.compiled_fns.last().expect("a parameter belongs to a function").callable;
        let Some(owed) = self.sigs.fn_sig_of(callable).and_then(|sig| sig.param_clauses.get(position as usize)) else { return false };
        owed.iter().any(|ob| ObligationRule::NoPersist.holds(&self.sigs.obligation_rules_of(*ob)))
    }

    pub(super) fn emit_anchor_copy_ins(&mut self, body: &HirId<HirExpr>, decl_params: &[HirParam]) -> Result<(), anyhow::Error> {
        let params = self.bindings.anchor_params(body);
        for param in params {
            let mut flags = if param.written { ir::COPY_IN_WRITTEN } else { 0 };
            let (at, witness_set_pool_id) = match param.anchor_slot {
                0 => (*body, ir::NO_WITNESS_SET),
                slot => {
                    if self.param_admits_no_persist(slot - 1) {
                        flags |= ir::COPY_IN_ADMITS_NO_PERSIST;
                    }
                    (decl_params[slot as usize - 1].name, self.slot_witness_set_pool_id(param.value_slot))
                },
            };

            self.emit(Inst::CopyAnchorIn(param.anchor_slot, flags, witness_set_pool_id), &at);

            if param.captured {
                self.emit(Inst::ShareLocal(param.value_slot), body);
            }
        }

        let pairs = params.iter().filter(|param| param.written).map(|param| (param.anchor_slot, param.value_slot)).collect();
        self.ir.record_anchor_params(self.current_slot_witness_set_pool_id, pairs);
        self.anchor_params = params;
        Ok(())
    }

    pub(super) fn emit_anchor_copy_outs<T: 'static>(&mut self, node: &HirId<T>) {
        let params = self.anchor_params;
        for param in params.iter().filter(|param| param.written) {
            self.emit(Inst::CopyAnchorOut(param.anchor_slot, param.value_slot), node);
        }
    }

    /// Reserves the run of slots a frame needs beyond its bindings: one for the value a `defer`
    /// interrupts, and one per subscript plus one per value for each path anchor in the body.
    pub(super) fn open_frame_temp_slots(&mut self, body: &HirId<HirExpr>) {
        self.current_body = Some(*body);
        let Some(temps) = self.bindings.frame_temps(body) else { return };
        self.defer_slot = temps.defer_slot;
        self.reserve_slots(temps.count, body);
    }

    fn set_frame_stack_height<T: 'static>(&mut self, want: usize, node: &HirId<T>) {
        // Code no path reaches can start at any height.
        let Some(have) = self.frame_stack_height else {
            self.frame_stack_height = Some(want);
            return;
        };
        if want > have {
            self.reserve_slots(want - have, node);
        } else {
            for _ in want..have {
                self.emit(Inst::Pop, node);
            }
        }
    }

    /// Emits every `defer` and `finally` a return from here leaves, innermost first, then the return itself.
    pub(super) fn emit_return_with_cleanup<T: 'static>(&mut self, carries_value: bool, node: &HirId<T>, emit_return: impl FnOnce(&mut Self)) -> Result<(), anyhow::Error> {
        let handlers = self.handlers.clone();
        let try_frames = self.try_frames.clone();
        let defers = self.defers.clone();
        while let Some(frame) = self.try_frames.last().copied() {
            self.emit_defers_from(frame.pending_defer_count, carries_value, node)?;
            if frame.has_handler() {
                self.pop_handler();
            }
            if frame.finally.is_some() && frame.position != TryCatchPosition::Finally {
                self.compile_finally(carries_value, node)?;
            }
            self.try_frames.pop();
        }
        self.emit_defers_from(0, carries_value, node)?;
        emit_return(self);
        self.try_frames = try_frames;
        self.defers = defers;
        self.replace_handlers(handlers);
        Ok(())
    }

    /// Emits the `defer` bodies pending beyond the first `pending_defer_count`, innermost first, and
    /// forgets them. A returned value waits in the frame's defer slot while they run.
    fn emit_defers_from<T: 'static>(&mut self, pending_defer_count: usize, carries_value: bool, node: &HirId<T>) -> Result<(), anyhow::Error> {
        if self.defers.len() <= pending_defer_count {
            return Ok(());
        }
        let slot = carries_value.then(|| self.require_defer_slot());
        if let Some(slot) = slot {
            self.emit(Inst::StoreTempPop(slot), node);
        }
        for i in (pending_defer_count..self.defers.len()).rev() {
            let pending = self.defers[i];
            self.pop_handler();
            self.emit_defer_body(&pending)?;
        }
        if let Some(slot) = slot {
            self.emit(Inst::LoadLocal(slot), node);
        }
        self.defers.truncate(pending_defer_count);
        Ok(())
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

        // An error thrown out of the `catch` still runs the `finally`, so the `catch` gets a handler.
        let handler = self.try_frames[idx].finally.map(|_| self.ir.new_label());
        if let Some(handler) = handler {
            self.push_handler(handler, &catch.body)?;
        }
        self.inline_block(&catch.body)?;
        if handler.is_some() {
            self.pop_handler();
        }
        self.exit_scope(&catch.body)?;

        if let Some(handler) = handler {
            let past = self.ir.new_label();
            self.emit(Inst::Jump(past), &catch.body);
            self.bind_label(handler);
            self.compile_finally(true, &catch.body)?;
            self.emit(Inst::Throw, &catch.body);
            self.bind_label(past);
        }
        Ok(())
    }

    fn compile_finally<T: 'static>(&mut self, carries_value: bool, node: &HirId<T>) -> Result<(), anyhow::Error> {
        let idx = self.try_frames.len() - 1;
        let TryFrame { position, finally: Some(finally), stmt: try_stmt, .. } = self.try_frames[idx] else {
            unreachable!("only a `try` with a `finally` runs one")
        };
        let slot = self.bindings.finally_slot(&try_stmt);
        if carries_value {
            self.emit(Inst::StoreTempPop(slot), node);
        }
        self.set_frame_stack_height(self.bindings.frame_stack_height_at(&try_stmt) as usize, node);
        self.try_frames[idx].position = TryCatchPosition::Finally;
        self.expression_stmt(&finally)?;
        self.try_frames[idx].position = position;
        if carries_value {
            self.emit(Inst::LoadLocal(slot), node);
        }
        Ok(())
    }

    /// Reserves a slot for each declaration in a block. A forward-referenced one is also allocated
    /// here, so the body above it has an object to capture. Its own captures are bound at its line,
    /// so this returns each forward-referenced one with the instruction that binds them, if any.
    fn hoist_declarations(&mut self, body: &Vec<HirId<HirStmt>>) -> Result<HashMap<usize, Option<Inst>>, anyhow::Error> {
        let mut binds = HashMap::new();
        for stmt_id in body {
            if !self.hir.get(stmt_id).declares_slot() {
                continue;
            }
            if !self.barriers.is_forward_referenced(stmt_id) {
                self.emit(Inst::PushUnassigned, stmt_id);
                continue;
            }

            let slot = self.bindings.slot(stmt_id);
            let (idx, captures) = self.declaration_constant(stmt_id)?;
            let build = self.build_inst(stmt_id, idx, captures, false);
            self.emit(build, stmt_id);

            let bind = captures.then(|| match self.hir.get(stmt_id) {
                HirStmt::Fn(_) => Inst::BindClosureCaptures(slot, idx),
                _ => Inst::BindTypeCaptures(slot, idx),
            });
            binds.insert(stmt_id.index(), bind);
        }
        Ok(binds)
    }

    /// Builds a declaration that is not forward-referenced.
    fn build_declaration(&mut self, stmt: &HirId<HirStmt>) -> Result<(), anyhow::Error> {
        let (idx, captures) = self.declaration_constant(stmt)?;
        self.emit(self.build_inst(stmt, idx, captures, true), stmt);
        self.emit(Inst::StoreLocal(self.bindings.slot(stmt)), stmt);
        self.emit(Inst::Pop, stmt);
        Ok(())
    }

    fn build_inst(&self, stmt: &HirId<HirStmt>, idx: u8, captures: bool, bind: bool) -> Inst {
        match (self.hir.get(stmt), captures, bind) {
            (HirStmt::Fn(_), _, true) => Inst::BuildClosure(idx),
            (HirStmt::Fn(_), _, false) => Inst::BuildClosureUnbound(idx),
            (_, true, true) => Inst::BuildType(idx),
            (_, true, false) => Inst::BuildTypeUnbound(idx),
            (_, false, _) => Inst::PushType(idx),
        }
    }

    /// Compiles a declaration into its constant, and says whether it captures anything.
    fn declaration_constant(&mut self, stmt: &HirId<HirStmt>) -> Result<(u8, bool), anyhow::Error> {
        match self.hir.get(stmt) {
            HirStmt::Fn(decl) => {
                let idx = self.function(stmt, (*stmt).into(), decl, FnKind::Function)?;
                Ok((idx, !self.bindings.captures(&decl.body).is_empty()))
            },
            HirStmt::Type(decl) => self.type_template(stmt, decl),
            _ => unreachable!("only a declaration reserves a slot"),
        }
    }
}

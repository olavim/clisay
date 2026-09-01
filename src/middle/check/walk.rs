//! The walk: `stmt` and `expr` dispatch over the HIR, and each site gathers the
//! context its rules need and calls them in a fixed order.

use crate::core::objects::TypeMember;
use crate::middle::diagnose::Diagnose;
use crate::middle::hir::{BinOp, HirExpr, HirSayDecl, HirFnDecl, HirId, HirLiteral, HirMatcher, HirStmt, HirTypeDecl, ReturnShape, Symbol, UnOp};
use crate::middle::bind::Place;
use crate::middle::native::{self, Container};
use crate::middle::obligations::Obligations;
use crate::middle::signatures::CallableId;
use crate::middle::signatures::{Mutability, TypeTag};

use super::scope::FlowSnapshot;

#[derive(Clone, Copy, PartialEq)]
enum Dropped {
    /// `say _ = e;`
    OnPurpose,
    Silently
}

use super::{PatternBinderSource, Checker, Ctx, Debt, FnContext, Guard, Local, ReceiverFacts, ValueState};

impl<'a> Ctx<'a> {
    pub(super) fn arg_display_name(&self, arg: &HirId<HirExpr>) -> String {
        match self.hir.get(arg) {
            HirExpr::Identifier(name) => format!("`{}`", self.hir.text(*name)),
            _ => "this value".to_string(),
        }
    }

    pub(super) fn callee_display_name(&self, callee: &HirId<HirExpr>) -> String {
        match self.hir.get(callee) {
            HirExpr::Identifier(name) => format!("`{}`", self.hir.text(*name)),
            HirExpr::Index(_, member, true) => self.member_display_name(member).map_or_else(|| "this function".to_string(), |m| format!("`{m}`")),
            _ => "this function".to_string(),
        }
    }

    pub(super) fn construct_tag(&self, callee: &HirId<HirExpr>) -> TypeTag {
        self.resolved().constructed_tag(callee)
    }

    pub(super) fn member_display_name(&self, member: &HirId<HirExpr>) -> Option<&'a str> {
        match self.hir.get(member) {
            HirExpr::Literal(HirLiteral::String(name)) => Some(name),
            _ => None,
        }
    }

    /// The `Type.field` name shown in field diagnostics.
    pub(super) fn qualified_field_display_name(&self, decl: &HirId<HirStmt>, field: Symbol) -> String {
        let owner = self.type_name_of(decl).map_or("", |name| self.hir.text(name));
        format!("{owner}.{}", self.hir.text(field))
    }

    pub(super) fn quoted_subject(&self, node: &HirId<HirExpr>) -> String {
        match self.hir.get(node) {
            HirExpr::Identifier(name) => format!("'{}'", self.hir.text(*name)),
            _ => "this value".to_string(),
        }
    }

    pub(super) fn string_member(&self, member: &HirId<HirExpr>) -> Option<Symbol> {
        self.member_display_name(member).and_then(|name| self.hir.symbol_of(name))
    }
}

impl<'a> Checker<'a> {
    pub(super) fn stmt(&mut self, stmt: &HirId<HirStmt>) -> Result<(), anyhow::Error> {
        match self.ctx.hir.get(stmt) {
            HirStmt::Nop => {},
            HirStmt::Fn(decl) => {
                // Register the name first so the body may call itself.
                self.locals.push(Local::func(decl.name, *stmt));
                self.function((*stmt).into(), decl)?;
            },
            HirStmt::Type(decl) => self.type_decl(stmt, Some(*stmt), decl)?,
            HirStmt::Trait(decl) => self.type_decl(stmt, None, decl)?,
            HirStmt::Say(field) => self.say(stmt.index(), field)?,
            HirStmt::Expression(e) => self.statement_result(e, Dropped::Silently)?,
            HirStmt::Discard(e) => self.statement_result(e, Dropped::OnPurpose)?,
            HirStmt::Block(e) => { self.expr(e)?; },
            HirStmt::Defer(e) => {
                let outer = std::mem::replace(&mut self.fn_ctx.in_defer, true);
                let result = self.expr(e);
                self.fn_ctx.in_defer = outer;
                result?;
            },
            HirStmt::Return(_) if self.fn_ctx.in_defer => {
                return Err(self.error_help(
                    "A 'defer' body cannot return".to_string(), stmt,
                    "a 'defer' runs while its block is already leaving, so there is no return left to make"));
            },
            HirStmt::Return(opt) => match opt {
                Some(e) => {
                    let state = self.expr(e)?;
                    self.check_return_field_move(e)?;
                    self.check_return_mutability(&state, e)?;
                    self.check_return(&state.debt, self.fn_ctx.return_shape, e)?;
                },
                // A `!` function falls back to null on a bare return, which it may not.
                None if self.fn_ctx.return_shape == ReturnShape::NonNull => {
                    return Err(self.error("A '!' function must return a value, but this 'return' yields null".to_string(), stmt));
                },
                None => {},
            },
            HirStmt::Throw(e) => { self.expr(e)?; },
            HirStmt::While(cond, body) => {
                let (body_narrow, _) = self.condition_narrowings(cond)?;
                let scope = self.condition_pattern_binders(cond)?;
                let pre = self.snapshot();
                // Check the body twice. A rebind late in the body drops a narrowing an earlier read
                // relied on, and only the second pass reads that line under a later iteration's
                // state.
                for _ in 0..2 {
                    self.apply_narrowings(&body_narrow);
                    self.with_binders(&scope, body, |c| c.expr(body))?;
                    let after_body = self.snapshot();
                    self.restore_flow(&pre);
                    // The body may have run, so a narrowing a rebind inside it invalidated stays
                    // invalidated after the loop.
                    self.restore_narrowings(&after_body);
                }
            },
            HirStmt::If(cond, then, otherwise) => {
                let (then_narrow, else_narrow) = self.condition_narrowings(cond)?;
                let scope = self.condition_pattern_binders(cond)?;
                let then_snap = self.narrow_branch(&then_narrow, |c| -> Result<FlowSnapshot, anyhow::Error> {
                    c.with_binders(&scope, then, |c| c.expr(then))?;
                    Ok(c.snapshot())
                })?;
                let else_snap = self.narrow_branch(&else_narrow, |c| -> Result<FlowSnapshot, anyhow::Error> {
                    if let Some(otherwise) = otherwise {
                        c.stmt(otherwise)?;
                    }
                    Ok(c.snapshot())
                })?;

                // A branch that returns or throws never reaches the code after the if, so its end
                // state is not merged.
                let then_diverges = self.ctx.hir.definitely_returns(then);
                let else_diverges = otherwise.as_ref().is_some_and(|o| self.ctx.hir.stmt_returns(o));
                match (then_diverges, else_diverges) {
                    // Joining two branches is restoring one and folding the other into it.
                    (false, false) => { self.restore_flow(&then_snap); self.merge_flow_into_current(&else_snap); },
                    (true, false) => self.restore_flow(&else_snap),
                    (false, true) => self.restore_flow(&then_snap),
                    (true, true) => {},
                }
            },
            HirStmt::Try(body, catch, finally) => {
                let pre = self.snapshot();
                self.expr(body)?;
                let after_body = self.snapshot();
                if let Some(catch) = catch {
                    // A throw can leave the body anywhere, so nothing the body narrowed holds here.
                    self.restore_narrowings(&pre);
                    let mark = self.locals.len();
                    if let Some(param) = catch.param {
                        let name = self.ctx.hir.ident_sym(&param);
                        let mut local = Local::catch(name, self.ctx.nullable_to_obligations(true));
                        local.decl = Some(param.index());
                        self.locals.push(local);
                    }
                    self.expr(&catch.body)?;
                    self.close_scope(mark, &catch.body)?;
                }
                // Either the body or the catch reaches the code below, so only what both leave
                // standing survives.
                self.restore_narrowings(&after_body);
                if let Some(finally) = finally { self.expr(finally)?; }
            },
            HirStmt::Match(scrutinee, arms) => {
                let state = self.expr(scrutinee)?;
                if state.debt.is_void() {
                    return Err(self.error("This call returns no value, so its result cannot be matched here".to_string(), scrutinee));
                }

                // A match discharges by ruling out witnesses. A guard-free arm total over a witness
                // clears it for the arms below.
                let mut remaining = self.ctx.obligations_of(&state.debt);

                // Arms are mutually exclusive, so each runs from the same pre-match state and only
                // the arms that fall through decide the state after the match.
                let baseline = self.snapshot();
                let mut fallthrough: Vec<FlowSnapshot> = Vec::new();
                let mut exhaustive = false;
                for arm in arms {
                    self.restore_flow(&baseline);
                    let scope = self.match_arm_binders(arm, &remaining, stmt)?;
                    self.with_binders(&scope, &arm.body, |c| -> Result<(), anyhow::Error> {
                        if let Some(guard) = &arm.guard { c.expr(guard)?; }
                        c.expr(&arm.body)?;
                        Ok(())
                    })?;

                    // A returning or throwing arm never reaches the code after the match.
                    if !self.ctx.hir.definitely_returns(&arm.body) {
                        fallthrough.push(self.snapshot());
                    }

                    // An irrefutable guardless arm always matches, so no value slips past unmatched.
                    exhaustive |= arm.guard.is_none() && self.ctx.hir.get(&arm.matcher).is_irrefutable(self.ctx.hir);
                    let ruled = self.ctx.obligations_ruled_out_by_match_arm(arm, &remaining);
                    remaining.retain(|w| !ruled.contains(w));
                }

                // A non-exhaustive match can fall through with no arm matching, keeping the pre-match
                // state. Narrowing does not cross a match, so the pre-match narrowings stay.
                let mut outcomes = fallthrough;
                if !exhaustive {
                    outcomes.push(baseline.clone());
                }

                match outcomes.split_first() {
                    Some((first, rest)) => {
                        self.restore_flow(first);
                        for snap in rest { self.merge_flow_into_current(snap); }
                        self.restore_narrowings(&baseline);
                    },
                    None => self.restore_flow(&baseline),
                }
            },
        }
        Ok(())
    }

    pub(super) fn expr(&mut self, expr: &HirId<HirExpr>) -> Result<ValueState, anyhow::Error> {
        Ok(match self.ctx.hir.get(expr) {
            HirExpr::Literal(HirLiteral::Null) => ValueState::of(self.ctx.opt_debt(true), TypeTag::Unknown).with_mutability(Mutability::Immutable),
            HirExpr::Literal(lit) => {
                self.literal_children(lit, expr)?;
                ValueState::nonnull().with_mutability(Mutability::Immutable)
            },
            HirExpr::Identifier(name) => self.identifier(*name, expr)?,
            HirExpr::This => self.this_valuestate(),
            HirExpr::Assign(lhs, rhs) => self.assign(lhs, rhs)?.as_stored(),
            HirExpr::CompoundAssign(lhs, _, rhs) => {
                self.expr(lhs)?;
                self.assign(lhs, rhs)?
            },
            HirExpr::Call(callee, args) => {
                let state = self.call(callee, args)?;
                self.invalidate_rebound_bindings(callee);
                state
            },
            HirExpr::Construct(callee, brace) => {
                let tag = self.ctx.construct_tag(callee);
                for (name, v) in brace {
                    let state = self.expr(v)?;
                    if let TypeTag::Concrete(decl) = &tag {
                        self.check_into_brace_field(&decl.clone(), *name, &state.debt, v)?;
                    }
                }

                let debt = match &tag {
                    TypeTag::Concrete(decl) => self.ctx.construction_debt(decl),
                    _ => Debt::Clean,
                };
                ValueState::of(debt, tag).with_mutability(Mutability::Immutable)
            },
            HirExpr::Mut(inner) => self.expr(inner)?.with_mutability(Mutability::Mutable),
            HirExpr::Index(target, member, _) => self.member_access(target, member)?,
            HirExpr::Binary(op, l, r) => self.binary(*op, l, r)?,
            HirExpr::Unary(op, x) => self.unary(*op, x)?,
            HirExpr::Match(scrutinee, _) => {
                let state = self.expr(scrutinee)?;
                if state.debt.is_void() {
                    return Err(self.error("This call returns no value, so its result cannot be matched here".to_string(), scrutinee));
                }
                ValueState::nonnull()
            },
            HirExpr::Block(stmts) => {
                let mark = self.locals.len();
                for s in stmts { self.stmt(s)?; }
                let dropped = self.check_dropped(mark, expr);
                self.truncate_locals(mark);
                dropped?;
                ValueState::unknown()
            },
            // `a ?? b` discharges the whole obligation set: the fallback runs on any bad value, so
            // a possibly-bad left crosses with no barrier. The result is `a` when clean, else `b`.
            HirExpr::Coalesce(l, r) => {
                let left = self.expr(l)?;
                self.ctx.require_witnessed_operand(&left.debt, l)?;
                if self.ctx.owes_object_witness(&left.debt) { self.record_witness_test(expr, &left.debt); }
                let right = self.expr(r)?;
                let tag = if left.tag == right.tag { left.tag.clone() } else { TypeTag::Unknown };
                let debt = if matches!(left.debt, Debt::Clean) { Debt::Clean } else { right.debt };
                ValueState::of(debt, tag)
            },
            // `a?.b` / `a?[i]` short-circuits on a bad operand, so the result carries the operand's
            // obligations.
            HirExpr::SafeAccess(target_id, member, _) => {
                let target = self.expr(target_id)?;
                self.ctx.require_witnessed_operand(&target.debt, target_id)?;
                let yielded = self.member_access_of(&target, target_id, member)?;
                self.chain_result_with(&target.debt, &yielded.debt, expr)
            },
            // `cb?(args)` short-circuits on a bad callee, carrying its obligations.
            HirExpr::SafeCall(callee_id, args) => {
                let callee = self.expr(callee_id)?;
                self.ctx.require_witnessed_operand(&callee.debt, callee_id)?;
                let arg_types: Vec<ValueState> = args.iter().map(|a| self.expr(a)).collect::<Result<_, _>>()?;
                let resolved = self.resolved_call(callee_id, args, &arg_types)?;
                if resolved.is_none() {
                    self.note_opaque_call_args(callee_id, &arg_types);
                }
                self.invalidate_rebound_bindings(callee_id);
                let yielded = resolved.map_or(Debt::Clean, |state| state.debt);
                self.chain_result_with(&callee.debt, &yielded, expr)
            },
            HirExpr::Propagate(operand) if self.fn_ctx.in_defer => {
                return Err(self.error_help(
                    "A 'defer' body cannot propagate with '?!'".to_string(), expr,
                    "'?!' returns the bad value, and the block is already leaving; handle it with '??' or '!' instead"));
            },
            HirExpr::Propagate(operand) => {
                let state = self.expr(operand)?;
                self.ctx.require_witnessed_operand(&state.debt, operand)?;
                if self.ctx.owes_object_witness(&state.debt) { self.record_witness_test(expr, &state.debt); }
                ValueState::of(self.ctx.discharged_debt(&state.debt), state.tag)
            },
            // `a ?? p => h` binds the caught bad value to `p`, which still owes what `a` owed. A
            // single type witness narrows `p`'s tag, so a caught `Err` is usable as one.
            HirExpr::Handle(left_id, binder, handler) => {
                let left = self.expr(left_id)?;
                self.ctx.require_witnessed_operand(&left.debt, left_id)?;
                if self.ctx.owes_object_witness(&left.debt) { self.record_witness_test(expr, &left.debt); }
                let caught = self.ctx.obligations_of(&left.debt);
                let tag = self.ctx.handle_caught_tag(&caught);
                let mut binder_local = Local::binder_owing(*binder, caught, PatternBinderSource::Handler).as_used();
                binder_local.decl = Some(expr.index());
                binder_local.tag = tag;
                let mark = self.locals.len();
                self.locals.push(binder_local);
                let h = self.expr(handler)?;
                self.close_scope(mark, handler)?;
                let tag = if left.tag == h.tag { left.tag.clone() } else { TypeTag::Unknown };
                let debt = if matches!(left.debt, Debt::Clean) { Debt::Clean } else { h.debt };
                ValueState::of(debt, tag)
            },
            // `a!` asserts the value is clean, keeping its type tag. A barrier guards it unless
            // the operand is already proven clean.
            HirExpr::Assert(x) => {
                let state = self.expr(x)?;
                self.ctx.require_witnessed_operand(&state.debt, x)?;
                if self.ctx.owes_object_witness(&state.debt) {
                    self.record_witness_test(expr, &state.debt);
                } else if matches!(state.debt, Debt::Unknown) {
                    // An unknown value could be any witness, so `!` must assert against them all.
                    self.record_boundary_barrier(expr, &Obligations::new());
                } else if !matches!(state.debt, Debt::Clean) {
                    self.record_guard(expr, Guard::NonNull);
                } else {
                    self.record_elision(expr, Guard::NonNull);
                }
                ValueState::of(self.ctx.discharged_debt(&state.debt), state.tag)
            },
        })
    }

    fn statement_result(&mut self, e: &HirId<HirExpr>, dropped: Dropped) -> Result<(), anyhow::Error> {
        let state = self.expr(e)?;
        if dropped == Dropped::OnPurpose || state.stored {
            return Ok(());
        }
        self.ctx.check_unused_must_use(&state.debt, e)
    }

    fn callable_named_by(&self, value: &HirId<HirExpr>) -> Option<CallableId> {
        match self.ctx.hir.get(value) {
            HirExpr::Identifier(name) => self.callable_of(*name),
            HirExpr::Literal(HirLiteral::Lambda(_)) => Some((*value).into()),
            _ => None,
        }
    }

    pub(super) fn say(&mut self, decl: usize, field: &'a HirSayDecl) -> Result<(), anyhow::Error> {
        let (name, mutable, value, pattern) = (field.name, field.reassignable, &field.value, &field.pattern);
        let owed = field.clause.owed();
        let (assigned, tag, mutability) = if let Some(value) = value {
            let state = self.expr(value)?;
            self.check_into_slot(&state.debt, &owed, name, value)?;
            (true, state.tag, state.mutability)
        } else {
            (false, TypeTag::Unknown, Mutability::Unknown)
        };
        let mut local = Local::value(name, owed, mutable, assigned, tag);
        local.container = field.clause.container;
        local.site = *value;
        local.decl = Some(decl);
        local.mutability = mutability;
        local.resolved_callable = value.and_then(|v| self.callable_named_by(&v));
        let tag = local.tag.clone();
        let binder_mutability = local.mutability;
        self.locals.push(local);

        if let (Some(pattern), Some(value)) = (pattern, value) {
            self.check_say_pattern_else(pattern, value, &tag, &field.otherwise)?;
            let scope = self.ctx.say_pattern_binders(pattern, value, binder_mutability, mutable)?;
            self.push_binders(&scope);
        }

        Ok(())
    }

    fn check_say_pattern_else(&mut self, pattern: &HirId<HirMatcher>, value: &HirId<HirExpr>, tag: &TypeTag, otherwise: &Option<HirId<HirExpr>>) -> Result<(), anyhow::Error> {
        let Some(otherwise) = otherwise else {
            if self.ctx.pattern_always_matches(pattern, tag) {
                return Ok(());
            }
            return Err(self.ctx.error_help("a refutable `say` pattern statement must have an `else` branch".to_string(), pattern,
                format!("this pattern does not match every value `{}` might be, so `else` is needed to say what to do when it doesn't match",
                    self.ctx.hir.pos(value).snippet())));
        };
        self.expr(otherwise)?;
        if !self.ctx.hir.definitely_returns(otherwise) {
            return Err(self.ctx.error_help("the `else` branch of a `say` pattern statement must diverge".to_string(), otherwise,
                "the binding's names do not exist below it, so it has to `return` or `throw`"));
        }
        Ok(())
    }

    fn literal_children(&mut self, lit: &HirLiteral, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        match lit {
            HirLiteral::Array(elems) => for e in elems {
                let t = self.expr(e)?;
                self.store_into_container(&t.debt, e)?;
            },
            HirLiteral::Dict(pairs) => for (k, v) in pairs {
                self.expr(k)?;
                let t = self.expr(v)?;
                self.store_into_container(&t.debt, v)?;
            },
            HirLiteral::Lambda(decl) => self.lambda(decl, node)?,
            _ => {},
        }
        Ok(())
    }

    fn note_read(&mut self, i: usize) {
        self.locals[i].used = true;
    }

    pub(super) fn identifier(&mut self, name: Symbol, expr: &HirId<HirExpr>) -> Result<ValueState, anyhow::Error> {
        let Some(i) = self.frame_index_of(name) else {
            // A read that resolves to an enclosing frame is a closure capture.
            if let Some(j) = self.upvalue_index(name) {
                self.note_read(j);
            }
            self.refuse_capture_of_persisting_value(name, expr)?;
            return Ok(self.captured_read(name));
        };

        self.note_read(i);

        if self.locals[i].fn_decl {
            return Ok(ValueState::unknown());
        }

        if !self.locals[i].assigned && !self.locals[i].owed.contains(&self.ctx.sigs.opt) {
            let text = self.ctx.binding_display_name(name);
            let subject = if self.ctx.is_factory_field(name) { format!("Field '{text}'") } else { format!("'{text}'") };
            return Err(self.error(format!("{subject} is used before it is assigned"), expr));
        }

        let owed: Obligations = self.locals[i].owed.difference(&self.locals[i].discharged).copied().collect();
        let debt = self.locals[i].read_debt(owed);

        Ok(ValueState::of(debt, self.locals[i].tag.clone())
            .with_mutability(self.locals[i].mutability))
    }

    /// Member or data access `target.member` / `target[member]`.
    pub(super) fn member_access(&mut self, target: &HirId<HirExpr>, member: &HirId<HirExpr>) -> Result<ValueState, anyhow::Error> {
        let receiver = self.receiver(target)?;
        self.member_access_of(&receiver, target, member)
    }

    pub(super) fn member_access_of(&mut self, receiver_state: &ValueState, receiver: &HirId<HirExpr>, member: &HirId<HirExpr>) -> Result<ValueState, anyhow::Error> {
        let Some(name) = self.ctx.member_display_name(member) else {
            self.expr(member)?;
            // Reading a container yields a pending element. Presence is tracked, not depth, so the
            // read stays a container.
            if let Debt::Owed { obligations, container: true, .. } = &receiver_state.debt {
                return Ok(ValueState::of(Debt::Owed { obligations: obligations.clone(), definite: false, container: true }, TypeTag::Unknown));
            }
            return Ok(ValueState::unknown());
        };

        if matches!(receiver_state.tag, TypeTag::SelfType) {
            return self.trait_member(name, member);
        }

        self.require_member_exists(receiver_state, receiver, name)?;

        let Some(field) = self.ctx.hir.symbol_of(name) else { return Ok(ValueState::unknown()) };
        let narrowing = self.narrowable_field(receiver, field);
        if let TypeTag::Concrete(decl) = &receiver_state.tag {
            if let Some(layout) = self.ctx.layout_of(decl) {
                if let Some(member_kind) = layout.members.get(&field).copied() {
                    let debt = match member_kind {
                        TypeMember::Field(_) => {
                            let clause = layout.clause_of(field);
                            let mut owed = self.ctx.field_owes(decl, field);
                            if let Some(narrowing) = narrowing {
                                owed.retain(|ob| !self.discharged(&narrowing, *ob));
                            }
                            match owed.is_empty() {
                                true => Debt::Clean,
                                // A `[obl]` clause puts the debt on the elements, so reading the
                                // member yields a container and reading an element yields the debt.
                                false => Debt::Owed { obligations: owed, definite: false, container: clause.is_some_and(|c| c.container) },
                            }
                        },
                        // A method reference is a non-null value.
                        TypeMember::Method(_) => Debt::Clean,
                    };

                    return Ok(match receiver_state.mutability {
                        Mutability::Immutable => ValueState::of(debt, TypeTag::Unknown).with_mutability(Mutability::Immutable),
                        _ => ValueState::of(debt, TypeTag::Unknown),
                    });
                }
            }
        }
        Ok(ValueState::unknown())
    }

    fn require_member_exists(&self, receiver_state: &ValueState, receiver: &HirId<HirExpr>, name: &str) -> Result<(), anyhow::Error> {
        if self.ctx.may_have_member(&receiver_state.tag, name) {
            return Ok(());
        }
        // A value confirmed to be the witness it owes reports a missed discharge instead
        if self.ctx.is_obligation_witness(receiver_state) {
            self.ctx.require_discharged(receiver_state, receiver)?;
        }
        Err(self.ctx.absent_member_error(&receiver_state.tag, name, receiver))
    }

    pub(super) fn receiver(&mut self, receiver: &HirId<HirExpr>) -> Result<ValueState, anyhow::Error> {
        let state = match self.ctx.hir.get(receiver) {
            HirExpr::This => self.this_valuestate(),
            _ => self.expr(receiver)?,
        };
        // A value confirmed to be a witness it owes is usable by that type, even while it owes.
        if self.ctx.is_obligation_witness(&state) {
            return Ok(state);
        }
        // A container is indexable and its methods callable even while it owes element obligations.
        if Self::is_container(&state.debt) {
            return Ok(state);
        }
        self.ctx.require_usable_value(&state, receiver)?;
        Ok(state)
    }

    pub(super) fn binary(&mut self, op: BinOp, l: &HirId<HirExpr>, r: &HirId<HirExpr>) -> Result<ValueState, anyhow::Error> {
        match op {
            // Short-circuit operators narrow their left operand into the right operand.
            BinOp::And | BinOp::Or => {
                self.expr(l)?;
                let into_right = self.narrowings(l, matches!(op, BinOp::And));
                self.narrow_branch(&into_right, |c| c.expr(r))?;
                Ok(ValueState::nonnull())
            },
            BinOp::Equal | BinOp::NotEqual => {
                self.expr(l)?;
                self.expr(r)?;
                Ok(ValueState::nonnull())
            },
            _ => {
                let ln = self.expr(l)?;
                let rn = self.expr(r)?;
                if self.ctx.is_obligation_witness(&ln) || self.ctx.is_obligation_witness(&rn) {
                    return Err(self.ctx.invalid_operands_error(op, l, &ln, r, &rn));
                }
                self.ctx.require_usable_value(&ln, l)?;
                self.ctx.require_usable_value(&rn, r)?;
                Ok(ValueState::nonnull())
            },
        }
    }

    pub(super) fn unary(&mut self, op: UnOp, x: &HirId<HirExpr>) -> Result<ValueState, anyhow::Error> {
        let state = self.expr(x)?;
        if matches!(op, UnOp::Negate | UnOp::BitNot) {
            if let Some(witness) = self.ctx.obligation_witness_name(&state) {
                return Err(self.ctx.witness_use_error(format!("invalid operand of `{op}`: {witness}"), x, witness));
            }
            self.ctx.require_usable_value(&state, x)?;
        }
        Ok(ValueState::nonnull())
    }

    pub(super) fn captured_read(&self, name: Symbol) -> ValueState {
        let Some(i) = self.upvalue_index(name) else {
            return ValueState::unknown();
        };
        let local = &self.locals[i];
        if local.fn_decl {
            return ValueState::unknown();
        }

        let owed: Obligations = match local.reassignable {
            true => local.owed.clone(),
            false => local.owed.difference(&local.discharged).copied().collect(),
        };

        let debt = local.read_debt(owed);

        match local.reassignable {
            true => ValueState::of(debt, TypeTag::Unknown),
            false => ValueState::of(debt, local.tag.clone()).with_mutability(local.mutability),
        }
    }

    pub(super) fn type_decl(&mut self, _node: &HirId<HirStmt>, type_stmt: Option<HirId<HirStmt>>, decl: &HirTypeDecl) -> Result<(), anyhow::Error> {
        let saved_type = self.current_type;
        let saved_surface = self.current_trait_surface.take();
        let saved_factory = std::mem::replace(&mut self.checking_factory, false);
        self.current_type = type_stmt;
        if type_stmt.is_some() {
            // The factory's field-locals carry definite assignment, and writing an immutable field
            // in it is initialization. A factory-less type has a `Nop` init to skip.
            self.checking_factory = true;
            self.method_stmt(&decl.init)?;
            self.checking_factory = false;
        } else {
            // A trait method reaches only the trait's declared surface through `this`.
            self.current_trait_surface = Some(decl.surface.clone());
        }
        for method in &decl.methods {
            self.method_stmt(method)?;
        }
        self.current_type = saved_type;
        self.current_trait_surface = saved_surface;
        self.checking_factory = saved_factory;
        Ok(())
    }

    pub(super) fn function(&mut self, callable: CallableId, decl: &HirFnDecl) -> Result<(), anyhow::Error> {
        // An unmarked return is inferred whole from the body. When it can both finish with no value
        // and return a bad value, the mixed shape must be declared.
        let unmarked = decl.is_unmarked();
        if unmarked {
            if let Some(ret) = self.ctx.sigs.fn_sig_of(callable)
                .map(|s| &s.ret).filter(|r| r.void && !r.obligations.is_empty())
            {
                return Err(self.ctx.mixed_void_error(callable, decl, &ret.obligations));
            }
        }

        self.ctx.reject_receiver_witnessed_obligations(decl)?;

        let ctx = FnContext {
            receiver: self.receiver_facts(decl),
            return_shape: decl.ret,
            return_owes: !decl.clause.names.is_empty(),
            return_unmarked: unmarked,
            return_mut: decl.clause.capability.is_mut(),
            return_admits: self.ctx.sigs.fn_sig_of(callable).map(|s| s.ret.obligations.clone()),
            returns_void: self.ctx.sigs.fn_sig_of(callable).is_some_and(|s| s.ret.void),
            name: Some(decl.name),
            return_clause: decl.clause.pos.clone(),
            in_defer: false,
        };

        let saved = std::mem::replace(&mut self.fn_ctx, ctx);

        let result = self.with_frame(&decl.params, &decl.body, |c| {
            c.expr(&decl.body)?;
            // A non-null return must be produced on every path.
            if c.fn_ctx.return_shape == ReturnShape::NonNull && !c.ctx.hir.body_returns_a_value(&decl.body) {
                return Err(c.error("This function can finish without returning a value; a '!' return must produce one on every path".to_string(), &decl.body));
            }
            Ok(())
        });

        self.fn_ctx = saved;
        result
    }

    pub(super) fn method_stmt(&mut self, stmt: &HirId<HirStmt>) -> Result<(), anyhow::Error> {
        if let HirStmt::Fn(decl) = self.ctx.hir.get(stmt) {
            self.function((*stmt).into(), decl)?;
        }
        Ok(())
    }

    pub(super) fn lambda(&mut self, decl: &HirFnDecl, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        self.function((*node).into(), decl)
    }

    pub(super) fn receiver_facts(&self, decl: &HirFnDecl) -> ReceiverFacts {
        match &decl.receiver {
            Some(_) if self.checking_factory => ReceiverFacts::default(),
            Some(clause) => ReceiverFacts {
                mutability: Mutability::of(clause.capability),
                writable: clause.capability.is_mut(),
                owed: clause.owed(),
            },
            None => self.fn_ctx.receiver.clone(),
        }
    }

    pub(super) fn call(&mut self, callee: &HirId<HirExpr>, args: &[HirId<HirExpr>]) -> Result<ValueState, anyhow::Error> {
        let arg_types: Vec<ValueState> = args.iter().map(|a| self.expr(a)).collect::<Result<_, _>>()?;

        match self.resolved_call(callee, args, &arg_types)? {
            Some(state) => Ok(state),
            None => {
                self.note_opaque_call_args(callee, &arg_types);
                self.indirect_call(callee)
            },
        }
    }

    /// The checks for a callee this pass can resolve, with the arguments already walked. `None`
    /// means it resolved nothing, which leaves validating the callee to the form that called it:
    /// a plain call requires a usable value, a `?` call tolerates a null one.
    fn resolved_call(&mut self, callee: &HirId<HirExpr>, args: &[HirId<HirExpr>], arg_types: &[ValueState]) -> Result<Option<ValueState>, anyhow::Error> {
        match self.ctx.hir.get(callee) {
            HirExpr::Identifier(name) => {
                let name = *name;
                if let Some(decl) = self.ctx.resolved().type_named(callee) {
                    // A factory-less type is built only by brace, so a paren call has nothing to run.
                    if !self.ctx.type_has_factory(&decl) {
                        let t = self.ctx.hir.text(name);
                        return Err(self.error_help(
                            format!("cannot construct '{t}' with '{t}(..)': '{t}' has no factory"),
                            callee,
                            format!("build it with a brace like '{t}{{ .. }}', or give every field a default or add an 'init'")));
                    }

                    if let Some(init) = self.ctx.constructor_init(callee) {
                        self.check_call_args(callee, init.into(), arg_types, args)?;
                    }

                    return Ok(Some(ValueState::of(self.ctx.construction_debt(&decl), TypeTag::Concrete(decl)).with_mutability(Mutability::Immutable)));
                }
                if let Some(callable) = self.callable_of(name) {
                    self.check_call_args(callee, callable, arg_types, args)?;
                    return Ok(Some(self.ctx.call_result(callable, &TypeTag::Unknown)));
                }
                // A built-in global resolves by name when no local or function shadows it.
                if self.frame_index_of(name).is_none() {
                    if let Some(sig) = native::builtin(self.ctx.hir.text(name)) {
                        self.check_native_args(callee, &sig, arg_types, args)?;
                        let result = ValueState::of(self.ctx.native_ret_debt(sig.ret), TypeTag::Unknown);
                        return Ok(Some(result));
                    }
                }
                Ok(None)
            },
            HirExpr::Index(receiver, member, _) => Ok(Some(self.method_call(callee, receiver, member, arg_types, args)?)),
            _ => Ok(None),
        }
    }

    pub(super) fn method_call(&mut self, callee: &HirId<HirExpr>, receiver: &HirId<HirExpr>, member: &HirId<HirExpr>, arg_types: &[ValueState], args: &[HirId<HirExpr>]) -> Result<ValueState, anyhow::Error> {
        let Some(name) = self.ctx.member_display_name(member) else { return self.indirect_call(callee) };
        let receiver_typed = self.receiver(receiver)?;
        if matches!(receiver_typed.tag, TypeTag::SelfType) {
            return self.trait_member(name, member);
        }
        self.require_member_exists(&receiver_typed, receiver, name)?;
        if let (TypeTag::Concrete(decl), Some(method)) = (&receiver_typed.tag, self.ctx.hir.symbol_of(name)) {
            if let Some(stmt) = self.ctx.sigs.methods_by_type.get(&(*decl, method)).copied() {
                self.check_call_args(callee, stmt.into(), arg_types, args)?;
                return Ok(self.ctx.call_result(stmt.into(), &receiver_typed.tag));
            }
        }
        // A native-type method resolves by name when no user method matches the receiver.
        if let Some(sig) = native::native_method(name) {
            self.check_native_args(callee, &sig, arg_types, args)?;
            if sig.container == Container::Preserves {
                for (state, arg) in arg_types.iter().zip(args) {
                    self.store_into_container(&state.debt, arg)?;
                }
                self.transfer_obligations_into_receiver(receiver, arg_types);
            }
            return Ok(ValueState::of(self.ctx.native_ret_debt(sig.ret), TypeTag::Unknown));
        }
        Ok(ValueState::unknown())
    }

    pub(super) fn check_call_args(&mut self, callee: &HirId<HirExpr>, callable: CallableId, arg_types: &[ValueState], args: &[HirId<HirExpr>]) -> Result<(), anyhow::Error> {
        self.resolved_callees.insert(*callee, callable);
        // Read the params through the shared signatures borrow so the later check can take &mut self.
        let sigs = self.ctx.sigs;
        let Some(sig) = sigs.fn_sig_of(callable) else { return Ok(()) };
        self.check_arg_mutability(callee, &sig.param_markers, arg_types, args)?;
        self.check_arg_obligations(callee, &sig.param_clauses, arg_types, args)?;
        self.check_args(callee, &sig.param_clauses, arg_types, args)?;
        Ok(())
    }

    /// A call through a value whose declaration is not visible here.
    pub(super) fn indirect_call(&mut self, callee: &HirId<HirExpr>) -> Result<ValueState, anyhow::Error> {
        let callee_typed = self.expr(callee)?;
        if let Some(witness) = self.ctx.obligation_witness_name(&callee_typed) {
            let subject = self.ctx.arg_display_name(callee);
            return Err(self.ctx.witness_use_error(format!("{subject} is not callable"), callee, witness));
        }
        self.ctx.require_usable_value(&callee_typed, callee)?;
        Ok(ValueState::unknown())
    }

    fn used_before_read(&self, expr: &HirId<HirExpr>) -> bool {
        let HirExpr::Identifier(name) = self.ctx.hir.get(expr) else { return false };
        self.frame_index_of(*name).is_some_and(|i| self.locals[i].used)
    }

    pub(super) fn assign(&mut self, lhs: &HirId<HirExpr>, rhs: &HirId<HirExpr>) -> Result<ValueState, anyhow::Error> {
        if self.checking_factory {
            if let Some(field) = self.never_initialized_factory_field(lhs, rhs) {
                return Err(self.error_help(format!("Non-null field '{}' is never initialized", self.ctx.hir.text(field)), lhs,
                    "assign it in the factory, or give the field a default"));
            }
        }

        let used = self.used_before_read(rhs);
        let state = self.expr(rhs)?;
        match self.ctx.hir.get(lhs) {
            HirExpr::Identifier(name) => {
                let name = *name;
                if let Some(i) = self.frame_index_of(name) {
                    self.check_reassignable(i, name, lhs)?;
                    let owed = self.locals[i].owed.clone();
                    self.check_into_slot(&state.debt, &owed, name, lhs)?;
                    self.locals[i].assigned = true;
                    self.locals[i].resolved_callable = self.callable_named_by(rhs);
                    self.locals[i].tag = state.tag.clone();
                    self.locals[i].mutability = state.mutability;
                    self.locals[i].used = used;
                    self.reset_narrowing(i, matches!(state.debt, Debt::Clean));
                } else if self.ctx.sigs.is_type(name) {
                    // A type binding names a declaration, not a reassignable slot.
                    return Err(self.error(format!("Cannot reassign `{}`; it names a type", self.ctx.hir.text(name)), lhs));
                } else if matches!(self.ctx.bindings.place_of(lhs), Some(Place::Upvalue(_))) {
                    if let Some(i) = self.upvalue_index(name) {
                        self.check_reassignable(i, name, lhs)?;
                    }
                } else {
                    // `field = ...` is implicitly `this.field = ...`
                    self.assign_field_this(name, &state.debt, lhs, rhs)?;
                }
            },
            HirExpr::Index(target, member, is_dot) => self.assign_index(target, member, *is_dot, &state.debt, lhs, rhs)?,
            _ => {},
        }
        // A store hands the value to a new holder, so a mutable right side moves.
        Ok(state)
    }

    fn check_reassignable(&self, i: usize, name: Symbol, lhs: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let text = self.ctx.hir.text(name);
        if self.locals[i].fn_decl {
            return Err(self.error(format!("Cannot reassign `{text}`; it names a function"), lhs));
        }
        if self.locals[i].reassignable || !self.locals[i].assigned {
            return Ok(());
        }
        if self.locals[i].pattern_binder_source.is_some_and(|source| !source.can_be_var()) {
            return Err(self.error_help(format!("Cannot reassign matcher binder `{text}`"), lhs,
                format!("copy it into a `say var {text}` first to change it")));
        }
        Err(self.error_help(format!("Cannot reassign binding `{text}`"), lhs,
            format!("you can make `{text}` reassignable by declaring it as `say var {text}`")))
    }

    pub(super) fn assign_index(&mut self, target: &HirId<HirExpr>, member: &HirId<HirExpr>, is_dot: bool, value: &Debt, lhs: &HirId<HirExpr>, rhs: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        if matches!(self.ctx.hir.get(target), HirExpr::This) {
            if let Some(field) = self.ctx.string_member(member) {
                self.assign_field_this(field, value, lhs, rhs)?;
            }
            return Ok(());
        }

        let slot = self.local_of(target);

        let binding = slot.or_else(|| self.upvalue_binding_of(target));
        if let Some(i) = binding.filter(|&i| !self.binding_is_writable(i)) {
            return Err(self.immutable_mutation_error(target, i));
        }

        if slot.is_none() && self.is_readonly(target) {
            return Err(self.readonly_write_error(target));
        }

        // A bracket index `obj[expr] = ...` is the dynamic data path.
        if !is_dot {
            if slot.is_none() {
                self.receiver(target)?;
            }
            return Ok(());
        }

        let receiver = self.receiver(target)?;
        let Some(field) = self.ctx.string_member(member) else { return Ok(()) };
        if let TypeTag::Concrete(decl) = &receiver.tag {
            self.assign_field_external(&decl.clone(), field, value, lhs, rhs)?;
        }
        Ok(())
    }

    pub(super) fn assign_field_this(&mut self, field: Symbol, debt: &Debt, lhs: &HirId<HirExpr>, rhs: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let Some(type_stmt) = self.current_type else { return Ok(()) };
        let (member, nullable, mutable) = match self.ctx.layout_of(&type_stmt) {
            Some(layout) => (layout.members.get(&field).copied(), layout.is_nullable(field), layout.is_reassignable(field)),
            None => return Ok(()),
        };

        match member {
            Some(TypeMember::Field(_)) => {},
            Some(TypeMember::Method(_)) => return Err(self.ctx.method_assign_error(field, lhs)),
            None => return Ok(()),
        }

        if !mutable && !self.checking_factory {
            return Err(self.ctx.non_var_field_error(&type_stmt, field, lhs));
        }

        let this = self.this_valuestate();
        self.ctx.require_usable_value(&this, lhs)?;

        if !self.this_is_writable() {
            return Err(self.readonly_receiver_error(&type_stmt, field, lhs));
        }

        self.check_into_field(debt, nullable, field, rhs)
    }

    /// Checks an external write `obj.field = value` on a known type.
    pub(super) fn assign_field_external(&mut self, type_stmt: &HirId<HirStmt>, field: Symbol, debt: &Debt, lhs: &HirId<HirExpr>, rhs: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let field_info = match self.ctx.layout_of(type_stmt) {
            Some(layout) => match layout.members.get(&field) {
                Some(TypeMember::Field(_)) => Some((layout.is_public(field), layout.is_nullable(field), layout.is_reassignable(field))),
                Some(TypeMember::Method(_)) => return Err(self.ctx.method_assign_error(field, lhs)),
                None => None,
            },
            None => None,
        };
        let Some((public, nullable, mutable)) = field_info else { return Ok(()) };
        if !public {
            return Ok(());
        }
        if !mutable {
            return Err(self.ctx.non_var_field_error(&type_stmt, field, lhs));
        }
        self.check_into_field(debt, nullable, field, rhs)
    }

}

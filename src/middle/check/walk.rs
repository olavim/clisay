//! The walk: `stmt` and `expr` dispatch over the HIR, and each site gathers the
//! context its rules need and calls them in a fixed order.

use std::collections::HashSet;

use crate::core::objects::TypeMember;
use crate::middle::diagnose::Diagnose;
use crate::middle::hir::{BinOp, Capability, HirExpr, HirFnDecl, HirId, HirLiteral, HirSlotClause, HirStmt, HirTypeDecl, ReturnShape, Symbol, UnOp};
use crate::middle::bind::Place;
use crate::middle::native::{self, Container};
use crate::middle::obligations::Obligations;
use crate::middle::signatures::{Mutability, TypeTag};

use super::scope::FlowSnapshot;
use super::{BinderSource, Checker, Ctx, Debt, FnContext, Guard, Local, ReceiverFacts, ValueState};

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

    /// Per parameter, whether the escape summary clears it of ever leaving the call.
    pub(super) fn fn_confined(&self, stmt: &HirId<HirStmt>, arity: usize) -> Vec<bool> {
        (0..arity).map(|i| !self.sigs.escapes_beyond_return_at(stmt, i)).collect()
    }

    /// Per parameter, whether the escape summary clears it of ever leaving the call.
    pub(super) fn lambda_confined(&self, node: &HirId<HirExpr>, arity: usize) -> Vec<bool> {
        let escapes = self.sigs.lambda_param_escapes.get(node);
        (0..arity).map(|i| escapes.is_some_and(|row| row.get(i) == Some(&false))).collect()
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

    /// Backtick-quoted name of a method call's receiver.
    pub(super) fn receiver_subject(&self, receiver: &HirId<HirExpr>) -> String {
        match self.hir.get(receiver) {
            HirExpr::This => "`this`".to_string(),
            _ => self.arg_display_name(receiver),
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
                let confined = self.ctx.fn_confined(stmt, decl.params.len());
                self.function(Some(*stmt), self.ctx.sigs.writes.get(stmt), confined, decl)?;
            },
            HirStmt::Type(decl) => self.type_decl(stmt, Some(*stmt), decl)?,
            HirStmt::Trait(decl) => self.type_decl(stmt, None, decl)?,
            HirStmt::Say(field) => self.say(stmt.index(), field.name, &field.clause, field.reassignable, &field.value)?,
            HirStmt::Expression(e) => {
                let state = self.expr(e)?;
                self.ctx.check_dropped_result(&state.debt, e)?;
            },
            HirStmt::Block(e) => { self.expr(e)?; },
            HirStmt::Return(opt) => match opt {
                Some(e) => {
                    let state = self.expr(e)?;
                    self.check_return_field_move(e)?;
                    self.check_return_mutability(&state, e)?;
                    self.check_return(&state.debt, self.fn_ctx.return_shape, e)?;
                    // Returning a mutable value moves it out to the caller.
                    self.transfer_write_ownership(e)?;
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
                let scope = self.condition_scope(cond)?;
                let pre = self.snapshot();
                // Check the body twice. The second pass sees the first pass's moves, so a value the
                // body reads after moving it is caught as a cross-iteration use. A rebind before the
                // move clears it first, so rebind-then-move is still accepted.
                for _ in 0..2 {
                    self.apply_narrowings(&body_narrow);
                    self.with_binders(&scope, body, |c| c.expr(body))?;
                    let after_body = self.snapshot();
                    self.restore_keeping_moves(&pre);
                    // The body may have run, so a narrowing a rebind inside it invalidated stays
                    // invalidated after the loop.
                    self.restore_narrowings(&after_body);
                }
            },
            HirStmt::If(cond, then, otherwise) => {
                let (then_narrow, else_narrow) = self.condition_narrowings(cond)?;
                let scope = self.condition_scope(cond)?;
                let then_snap = self.narrow_branch(&then_narrow, |c| -> Result<FlowSnapshot, anyhow::Error> {
                    c.with_binders(&scope, then, |c| c.expr(then))?;
                    Ok(c.snapshot())
                }).0?;
                let else_snap = self.narrow_branch(&else_narrow, |c| -> Result<FlowSnapshot, anyhow::Error> {
                    if let Some(otherwise) = otherwise {
                        c.stmt(otherwise)?;
                    }
                    Ok(c.snapshot())
                }).0?;

                // A branch that returns or throws never reaches the code after the if, so its end
                // state is not merged.
                let then_diverges = self.ctx.hir.definitely_returns(then);
                let else_diverges = otherwise.as_ref().is_some_and(|o| self.ctx.hir.stmt_returns(o));
                match (then_diverges, else_diverges) {
                    // Joining two branches is restoring one and folding the other into it.
                    (false, false) => { self.restore(&then_snap); self.join_in(&else_snap); },
                    (true, false) => self.restore(&else_snap),
                    (false, true) => self.restore(&then_snap),
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
                let mut settled = Obligations::new();

                // Arms are mutually exclusive, so each runs from the same pre-match state and only
                // the arms that fall through decide the state after the match.
                let baseline = self.snapshot();
                let mut fallthrough: Vec<FlowSnapshot> = Vec::new();
                let mut exhaustive = false;
                for arm in arms {
                    self.restore(&baseline);
                    let scope = self.arm_scope(arm, &remaining, stmt, scrutinee)?;
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
                    settled.extend(self.ctx.obligations_examined_by_match_arm(arm, &remaining));
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
                        self.restore(first);
                        for snap in rest { self.join_in(snap); }
                        self.restore_narrowings(&baseline);
                    },
                    None => self.restore(&baseline),
                }

                // After the join, since restoring the arms' snapshots would undo it. The match
                // settles what its arms actually ruled out; a lone catch-all rules out nothing.
                self.mark_settled(scrutinee, &settled);
            },
        }
        Ok(())
    }

    pub(super) fn expr(&mut self, expr: &HirId<HirExpr>) -> Result<ValueState, anyhow::Error> {
        Ok(match self.ctx.hir.get(expr) {
            HirExpr::Literal(HirLiteral::Null) => ValueState::of(self.ctx.opt_debt(true), TypeTag::Unknown).with_mutability(Mutability::Immutable),
            HirExpr::Literal(lit) => {
                // A plain container literal is immutable by default, so its children are checked
                // for deep immutability. Every literal is an immutable value.
                self.literal_children(lit, expr, true)?;
                ValueState::nonnull().with_mutability(Mutability::Immutable)
            },
            HirExpr::Identifier(name) => self.identifier(*name, expr)?,
            HirExpr::This => self.this_typed(),
            HirExpr::Assign(lhs, rhs) => self.assign(lhs, rhs)?,
            HirExpr::Call(callee, args) => {
                let state = self.call(expr, callee, args)?;
                self.invalidate_rebound_fields(callee);
                state
            },
            HirExpr::Construct(callee, brace) => {
                // A plain brace is immutable, so a mutable field value is refused here.
                let immutable = !std::mem::take(&mut self.mut_construction);
                let tag = self.ctx.construct_tag(callee);
                for (name, v) in brace {
                    let state = self.expr(v)?;
                    self.check_construct_field(immutable, &state, v)?;
                    if !immutable {
                        self.check_stored_element(v)?;
                    }
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
            HirExpr::Mut(inner) => {
                // A mutable container may hold mutable elements, so its children skip the
                // immutable-container check that a plain literal applies.
                match self.ctx.hir.get(inner) {
                    HirExpr::Literal(lit @ (HirLiteral::Array(_) | HirLiteral::Dict(_))) => {
                        self.literal_children(lit, inner, false)?;
                        ValueState::nonnull().with_mutability(Mutability::Mutable)
                    },
                    _ => {
                        let saved = std::mem::replace(&mut self.mut_construction, true);
                        let state = self.expr(inner);
                        self.mut_construction = saved;
                        state?.with_mutability(Mutability::Mutable)
                    },
                }
            },
            HirExpr::Index(target, member, _) => self.member_access(target, member)?,
            HirExpr::Binary(op, l, r) => self.binary(*op, l, r)?,
            HirExpr::Unary(op, x) => self.unary(*op, x)?,
            HirExpr::Match(scrutinee, matcher) => {
                let state = self.expr(scrutinee)?;
                if let Debt::Owed { obligations, .. } = &state.debt {
                    let settled = self.ctx.obligations_settled_by_matcher(matcher, obligations);
                    self.mark_settled(scrutinee, &settled);
                }
                if state.debt.is_void() {
                    return Err(self.error("This call returns no value, so its result cannot be matched here".to_string(), scrutinee));
                }
                ValueState::nonnull()
            },
            HirExpr::Block(stmts) => {
                let mark = self.locals.len();
                let handed_over = self.elements_handed_over;
                for s in stmts { self.stmt(s)?; }
                let dropped = self.check_dropped(mark, expr);
                if self.scope_holds_write_ownership(mark, handed_over) {
                    self.record_write_scope(expr);
                }
                self.truncate_locals(mark);
                dropped?;
                ValueState::unknown()
            },
            // `a ?? b` discharges the whole obligation set: the fallback runs on any bad value, so
            // a possibly-bad left crosses with no barrier. The result is `a` when clean, else `b`.
            HirExpr::Coalesce(l, r) => {
                self.mark_handled(l);
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
                self.expr(member)?;
                self.chain_result(&target.debt, expr)
            },
            // `cb?(args)` short-circuits on a bad callee, carrying its obligations.
            HirExpr::SafeCall(callee_id, args) => {
                let callee = self.expr(callee_id)?;
                self.ctx.require_witnessed_operand(&callee.debt, callee_id)?;
                for a in args { self.expr(a)?; }
                self.invalidate_rebound_fields(callee_id);
                self.chain_result(&callee.debt, expr)
            },
            // `a?!` discharges the operand on its fall-through path. The enclosing function carries
            // the obligation instead, recorded in signatures. The yielded value is clean.
            HirExpr::Propagate(operand) => {
                self.mark_handled(operand);
                let state = self.expr(operand)?;
                self.ctx.require_witnessed_operand(&state.debt, operand)?;
                if self.ctx.owes_object_witness(&state.debt) { self.record_witness_test(expr, &state.debt); }
                ValueState::of(self.ctx.discharged_debt(&state.debt), state.tag)
            },
            // `a ?? p => h` binds the caught bad value to `p`, which still owes what `a` owed. A
            // single type witness narrows `p`'s tag, so a caught `Err` is usable as one.
            HirExpr::Handle(left_id, binder, handler) => {
                self.mark_handled(left_id);
                let left = self.expr(left_id)?;
                self.ctx.require_witnessed_operand(&left.debt, left_id)?;
                if self.ctx.owes_object_witness(&left.debt) { self.record_witness_test(expr, &left.debt); }
                let caught = self.ctx.obligations_of(&left.debt);
                let tag = self.ctx.handle_caught_tag(&caught);
                let mut binder_local = Local::binder_owing(*binder, caught, BinderSource::Handler);
                binder_local.decl = Some(expr.index());
                binder_local.tag = tag;
                binder_local.handled = binder_local.owed.clone();
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
                self.mark_handled(x);
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

    pub(super) fn say(&mut self, decl: usize, name: Symbol, clause: &HirSlotClause, mutable: bool, value: &Option<HirId<HirExpr>>) -> Result<(), anyhow::Error> {
        let owed = clause.owed();
        let (assigned, tag, mutability, provenance) = if let Some(value) = value {
            let state = self.expr(value)?;
            self.check_into_slot(&state.debt, &owed, name, value)?;
            // The move records the sources feeding the value, so the slot reuses that walk for its
            // provenance and adds the closure captures a bare walk would miss.
            self.mark_settled(value, &owed);
            let mut provenance = self.transfer_write_ownership(value)?;
            provenance.extend(self.captured_sources(value));
            (true, state.tag, state.mutability, provenance)
        } else {
            (false, TypeTag::Unknown, Mutability::Unknown, Vec::new())
        };
        let mut local = Local::value(name, owed, mutable, assigned, tag);
        local.container = clause.container;
        local.site = *value;
        local.decl = Some(decl);
        local.alias.mutability = mutability;
        local.alias.unproven_borrow = value.is_some_and(|v| self.holds_unproven_borrow(&v));
        local.alias.confined = value.is_some_and(|v| self.value_is_confined(&v));
        local.alias.borrowed = value.is_some_and(|v| self.arg_is_borrowed(&v));
        local.alias.provenance = provenance;
        local.alias.extracted_from = value.and_then(|v| self.extraction_of(&v)).into_iter().collect();
        local.alias.shared_origin = value.is_some_and(|v| self.shared_origin(&v));
        local.alias.may_be_shared = local.alias.shared_origin || local.alias.borrowed
            || !local.alias.extracted_from.is_empty() || value.is_none();
        self.locals.push(local);
        Ok(())
    }

    /// Checks a literal's children. `immutable` is set for a plain container literal: an immutable
    /// container is immutable all the way down, so a mutable element is rejected, and an element of
    /// unknown capability records a runtime seal-check.
    pub(super) fn literal_children(&mut self, lit: &HirLiteral, node: &HirId<HirExpr>, immutable: bool) -> Result<(), anyhow::Error> {
        match lit {
            HirLiteral::Array(elems) => for e in elems {
                let t = self.expr(e)?;
                self.check_container_element(immutable, &t, e, node)?;
                self.store_into_container(&t.debt, e)?;
            },
            HirLiteral::Dict(pairs) => for (k, v) in pairs {
                self.expr(k)?;
                let t = self.expr(v)?;
                self.check_container_element(immutable, &t, v, node)?;
                self.store_into_container(&t.debt, v)?;
            },
            HirLiteral::Lambda(decl) => self.lambda(decl, node)?,
            _ => {},
        }
        Ok(())
    }

    pub(super) fn identifier(&mut self, name: Symbol, expr: &HirId<HirExpr>) -> Result<ValueState, anyhow::Error> {
        let Some(i) = self.frame_index_of(name) else {
            // A read that resolves to an enclosing frame is a closure capture.
            if let Some(j) = self.enclosing_index(name) {
                self.value_may_have_escaped(j);
            }
            self.reject_capture_escape(name, expr)?;
            self.capture_enclosing(name, expr);
            return Ok(self.captured_read(name));
        };

        // Every read but a path base hands the value somewhere this pass does not follow.
        if self.path_base.take() != Some(*expr) {
            self.value_may_have_escaped(i);
        }

        if self.locals[i].func.is_some() {
            return Ok(ValueState::unknown());
        }

        // A read needs no write-ownership, so giving a value away leaves the name readable. Only an
        // opaque give is settled here, since its runtime proof rides the read.
        self.settle_unknown_transfer(i, expr);

        if !self.locals[i].assigned && !self.locals[i].owed.contains(&self.ctx.sigs.opt) {
            let text = self.ctx.binding_display_name(name);
            let subject = if self.ctx.is_factory_field(name) { format!("Field '{text}'") } else { format!("'{text}'") };
            return Err(self.error(format!("{subject} is used before it is assigned"), expr));
        }

        let owed: Obligations = self.locals[i].owed.difference(&self.locals[i].discharged).copied().collect();
        let debt = self.locals[i].read_debt(owed);

        Ok(ValueState::of(debt, self.locals[i].tag.clone())
            .with_mutability(self.locals[i].alias.mutability)
            .with_writable(self.write_permission(i)))
    }

    /// Member or data access `target.member` / `target[member]`. Resolves a field on a known-type
    /// receiver to its declared nullability; any other access is a dynamic-boundary read.
    pub(super) fn member_access(&mut self, target: &HirId<HirExpr>, member: &HirId<HirExpr>) -> Result<ValueState, anyhow::Error> {
        self.path_base = Some(*target);
        let receiver = self.receiver(target)?;
        self.path_base = None;
        let Some(name) = self.ctx.member_display_name(member) else {
            self.expr(member)?;
            // Reading a container yields a pending element. Presence is tracked, not depth, so the
            // read stays a container.
            if let Debt::Owed { obligations, container: true, .. } = &receiver.debt {
                return Ok(ValueState::of(Debt::Owed { obligations: obligations.clone(), definite: false, container: true }, TypeTag::Unknown));
            }
            return Ok(ValueState::unknown());
        };
        if matches!(receiver.tag, TypeTag::SelfType) {
            return self.trait_member(name, member);
        }
        // A member name never interned as an identifier names no declared member.
        let Some(field) = self.ctx.hir.symbol_of(name) else { return Ok(ValueState::unknown()) };
        let narrowing = self.narrowable_field(target, field);
        if let TypeTag::Concrete(decl) = &receiver.tag {
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
                    // A member of an immutable value is reached only through it, so it cannot be
                    // mutated either.
                    return Ok(match receiver.mutability {
                        Mutability::Immutable => ValueState::of(debt, TypeTag::Unknown).with_mutability(Mutability::Immutable),
                        _ => ValueState::of(debt, TypeTag::Unknown),
                    });
                }
            }
        }
        Ok(ValueState::unknown())
    }

    /// Evaluates a receiver, requiring it to be non-null.
    pub(super) fn receiver(&mut self, receiver: &HirId<HirExpr>) -> Result<ValueState, anyhow::Error> {
        let state = match self.ctx.hir.get(receiver) {
            HirExpr::This => self.this_typed(),
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
            // Short-circuit operators narrow their left operand into the right operand. `and`
            // narrows where the left holds (true), `or` where it fails (false).
            BinOp::And | BinOp::Or => {
                self.expr(l)?;
                let runs_when = matches!(op, BinOp::And);
                let (_, on_skip) = self.narrow_branch(&self.narrowings(l, !runs_when), |_| ());
                let into_right = self.narrowings(l, runs_when);
                let (result, on_run) = self.narrow_branch_keeping_moves(&into_right, |c| c.expr(r));
                result?;
                // A left that cannot skip the right leaves one path through the condition.
                match self.ctx.has_truthiness(l, runs_when) {
                    true => self.mark_obligations_handled_when_resolved_on_every_path(&[on_run]),
                    false => self.mark_obligations_handled_when_resolved_on_every_path(&[on_skip, on_run]),
                }
                Ok(ValueState::nonnull())
            },
            // Equality is a boolean context; a possibly-null operand is fine.
            BinOp::Equal | BinOp::NotEqual => {
                self.expr(l)?;
                self.expr(r)?;
                Ok(ValueState::nonnull())
            },
            _ => {
                let ln = self.expr(l)?;
                let rn = self.expr(r)?;
                // A confirmed-witness operand makes the operation invalid whatever the other side
                // is, so name both operand types like the runtime's operand error does.
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
        // `!` is a boolean context; negation and bitwise-not require a value.
        if matches!(op, UnOp::Negate | UnOp::BitNot) {
            if let Some(witness) = self.ctx.obligation_witness_name(&state) {
                return Err(self.ctx.witness_use_error(format!("invalid operand of `{op}`: {witness}"), x, witness));
            }
            self.ctx.require_usable_value(&state, x)?;
        }
        Ok(ValueState::nonnull())
    }

    /// A read of a binding from an enclosing frame. Capturing a value discharges nothing, so the
    /// binding keeps what it was declared owing. A flow fact travels with it only from an immutable
    /// slot, which cannot be rebound, so the value tested is the value the nested body sees.
    pub(super) fn captured_read(&self, name: Symbol) -> ValueState {
        let Some(i) = self.enclosing_index(name) else {
            return ValueState::unknown();
        };
        let local = &self.locals[i];
        if local.func.is_some() {
            return ValueState::unknown();
        }

        // A mutable binding may have been written since the narrowing, which the enclosing frame
        // cannot see, so only an immutable one keeps what was proved about it.
        let owed: Obligations = match local.reassignable {
            true => local.owed.clone(),
            false => local.owed.difference(&local.discharged).copied().collect(),
        };

        let debt = local.read_debt(owed);

        // A rebindable slot may hold a different value by then, so only an immutable one carries its
        // type and mutability in.
        match local.reassignable {
            true => ValueState::of(debt, TypeTag::Unknown),
            false => ValueState::of(debt, local.tag.clone()).with_mutability(local.alias.mutability),
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

    pub(super) fn function(&mut self, stmt: Option<HirId<HirStmt>>, writes: Option<&'a HashSet<Symbol>>, confined: Vec<bool>, decl: &HirFnDecl) -> Result<(), anyhow::Error> {
        // An unmarked return is inferred whole from the body. When it can both finish with no value
        // and return a bad value, the mixed shape must be named, not inferred.
        let unmarked = decl.is_unmarked();
        if unmarked {
            if let Some(ret) = stmt.and_then(|s| self.ctx.sigs.fns.get(&s))
                .map(|s| &s.ret).filter(|r| r.void && !r.obligations.is_empty())
            {
                return Err(self.ctx.mixed_void_error(decl, &ret.obligations));
            }
        }

        // A `mut` parameter borrows its argument, so it may not persist it. `*mut` takes the
        // argument's write-ownership and may persist it.
        if let Some(stmt) = stmt {
            for (i, param) in decl.params.iter().enumerate() {
                let cap = param.clause.capability;
                if !cap.is_retain() && self.ctx.sigs.escapes_beyond_return_at(&stmt, i) {
                    let text = self.ctx.hir.text(self.ctx.hir.ident_sym(&param.name));
                    let barred = param.clause.names.iter().copied().find(|&o| self.ctx.sigs.obligation_rules_of(o).no_persist);
                    let help = match barred {
                        Some(owed) => format!("`{}` declares `no persist`, so `*{text}` cannot help; freeze or copy it before persisting", self.ctx.hir.text(owed)),
                        None => format!("declare it `*{text}` to retain it, or freeze or copy it before persisting"),
                    };
                    let Some(site) = self.ctx.sigs.escape_site_at(&stmt, i) else {
                        return Err(self.error_labeled_help(
                            "cannot retain a borrowed argument".to_string(),
                            &param.name,
                            format!("`{text}` is borrowed"),
                            help));
                    };
                    return Err(self.error_ctx_help(
                        "cannot retain a borrowed argument",
                        self.ctx.hir.pos(&site),
                        format!("`{text}` is retained here"),
                        self.ctx.hir.pos(&param.name),
                        format!("`{text}` is borrowed here"),
                        help));
                }
            }

            // A `mut` receiver is lent for the call, so a body that captures it into a value
            // outliving the call keeps writing through a borrow the caller has taken back.
            let cap = decl.receiver.as_ref().map_or(Capability::None, |r| r.capability);
            if decl.receiver.is_some() && !cap.is_retain() && self.ctx.sigs.escapes_beyond_return_at(&stmt, decl.params.len()) {
                let own = match cap.is_mut() {
                    true => "*mut this",
                    false => "*this",
                };
                return Err(self.error_help(
                    "a borrowing receiver cannot let the instance escape".to_string(),
                    &decl.body,
                    format!("take the receiver by `{own}` to own it, or capture the values it holds instead of `this`")));
            }
        }
        self.ctx.reject_receiver_witnessed_obligations(decl)?;
        // A lambda's shape is inferred, so it is not checked against a declaration.
        let ctx = FnContext {
            receiver: self.receiver_facts(decl),
            return_shape: decl.ret,
            return_owes: !decl.clause.names.is_empty(),
            return_unmarked: unmarked,
            return_mut: decl.clause.capability.is_mut(),
            return_admits: stmt.and_then(|s| self.ctx.sigs.fns.get(&s)).map(|s| s.ret.obligations.clone()),
            name: Some(decl.name),
            return_clause: decl.clause.pos.clone(),
            params: decl.params.iter().map(|p| (self.ctx.hir.ident_sym(&p.name), p.pos.clone())).collect(),
            param_confined: confined,
            writes,
        };
        let saved = std::mem::replace(&mut self.fn_ctx, ctx);
        let result = self.with_frame(&decl.params, &decl.body, |c| {
            c.expr(&decl.body)?;
            // A non-null return must be produced on every path.
            if c.fn_ctx.return_shape == ReturnShape::NonNull && !c.ctx.hir.definitely_returns(&decl.body) {
                return Err(c.error("This function can finish without returning a value; a '!' return must produce one on every path".to_string(), &decl.body));
            }
            Ok(())
        });
        self.fn_ctx = saved;
        result
    }

    pub(super) fn method_stmt(&mut self, stmt: &HirId<HirStmt>) -> Result<(), anyhow::Error> {
        if let HirStmt::Fn(decl) = self.ctx.hir.get(stmt) {
            let confined = self.ctx.fn_confined(stmt, decl.params.len());
            self.function(Some(*stmt), self.ctx.sigs.writes.get(stmt), confined, decl)?;
        }
        Ok(())
    }

    /// Checks a lambda body.
    pub(super) fn lambda(&mut self, decl: &HirFnDecl, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let confined = self.ctx.lambda_confined(node, decl.params.len());
        self.function(None, self.ctx.sigs.lambda_writes.get(node), confined, decl)
    }

    /// What the declared `this` says about the receiver.
    pub(super) fn receiver_facts(&self, decl: &HirFnDecl) -> ReceiverFacts {
        match &decl.receiver {
            Some(_) if self.checking_factory => ReceiverFacts::default(),
            Some(clause) => ReceiverFacts {
                mutability: Mutability::param(clause.capability),
                owed: clause.owed(),
            },
            None => self.fn_ctx.receiver.clone(),
        }
    }

    pub(super) fn call(&mut self, expr: &HirId<HirExpr>, callee: &HirId<HirExpr>, args: &[HirId<HirExpr>]) -> Result<ValueState, anyhow::Error> {
        let immutable = !std::mem::take(&mut self.mut_construction);
        let arg_types: Vec<ValueState> = args.iter().map(|a| self.expr(a)).collect::<Result<_, _>>()?;
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
                        self.check_call_args(callee, init, &arg_types, args)?;
                        for (i, (state, arg)) in arg_types.iter().zip(args).enumerate() {
                            if self.ctx.sigs.param_escapes_at(&init, i) {
                                self.check_construct_field(immutable, state, arg)?;
                                if !immutable {
                                    self.check_stored_element(arg)?;
                                }
                            }
                        }
                    }

                    // Record the construction so codegen tells `mut K(..)` (CALL_MUT) from `mut f()`.
                    self.record_construction(expr);
                    return Ok(ValueState::of(self.ctx.construction_debt(&decl), TypeTag::Concrete(decl)).with_mutability(Mutability::Immutable));
                }
                if let Some(stmt) = self.func_of(name) {
                    self.check_call_args(callee, stmt, &arg_types, args)?;
                    return Ok(self.ctx.call_result(stmt, &TypeTag::Unknown));
                }
                // A built-in global resolves by name when no local or function shadows it.
                if self.frame_index_of(name).is_none() {
                    if let Some(sig) = native::builtin(self.ctx.hir.text(name)) {
                        self.check_native_args(callee, &sig, &arg_types, args)?;
                        let result = ValueState::of(self.ctx.native_ret_debt(sig.ret), TypeTag::Unknown);
                        // `freeze(x)` also discharges the mutation capability, handing its
                        // argument back immutable.
                        if self.ctx.hir.text(name) == "freeze" {
                            if let Some(arg) = args.first() { self.discharge_freeze(arg); }
                            return Ok(result.with_mutability(Mutability::Immutable));
                        }
                        return Ok(result);
                    }
                }
                self.check_opaque_call(callee, args, &arg_types)
            },
            HirExpr::Index(receiver, member, _) => self.method_call(callee, receiver, member, &arg_types, args),
            _ => self.check_opaque_call(callee, args, &arg_types),
        }
    }

    /// A method call `receiver.name(args)`. Resolves against the receiver's type when it is
    /// known, then falls back to a native-type method, and finally to a dynamic boundary.
    pub(super) fn method_call(&mut self, callee: &HirId<HirExpr>, receiver: &HirId<HirExpr>, member: &HirId<HirExpr>, arg_types: &[ValueState], args: &[HirId<HirExpr>]) -> Result<ValueState, anyhow::Error> {
        let Some(name) = self.ctx.member_display_name(member) else { return self.indirect_call(callee) };
        let receiver_typed = self.receiver(receiver)?;
        if matches!(receiver_typed.tag, TypeTag::SelfType) {
            return self.trait_member(name, member);
        }
        if let (TypeTag::Concrete(decl), Some(method)) = (&receiver_typed.tag, self.ctx.hir.symbol_of(name)) {
            if let Some(stmt) = self.ctx.sigs.methods_by_type.get(&(*decl, method)).copied() {
                self.check_receiver(callee, receiver, stmt, &receiver_typed)?;
                self.check_call_args(callee, stmt, arg_types, args)?;
                return Ok(self.ctx.call_result(stmt, &receiver_typed.tag));
            }
        }
        // A native-type method resolves by name when no user method matches the receiver.
        if let Some(sig) = native::native_method(name) {
            if sig.effect.mutates_receiver {
                if receiver_typed.writable == Mutability::Immutable {
                    return Err(self.immutable_receiver_error(callee, receiver, "mutates its receiver"));
                }
                self.claim_receiver_write_ownership(receiver)?;
            }
            self.check_native_args(callee, &sig, arg_types, args)?;
            if sig.container == Container::Preserves {
                for (state, arg) in arg_types.iter().zip(args) {
                    self.store_into_container(&state.debt, arg)?;
                }
                self.preserve_into_receiver(receiver, arg_types);
            }
            return Ok(ValueState::of(self.ctx.native_ret_debt(sig.ret), TypeTag::Unknown));
        }
        Ok(ValueState::unknown())
    }

    /// Matches the receiver against the capability the method declares on its `this`, the way an
    /// argument is matched against its parameter marker. A `mut` receiver is borrowed for the call;
    /// a `*mut` one is consumed by it.
    pub(super) fn check_receiver(&mut self, callee: &HirId<HirExpr>, receiver: &HirId<HirExpr>, callee_fn: HirId<HirStmt>, receiver_typed: &ValueState) -> Result<(), anyhow::Error> {
        let Some(sig) = self.ctx.sigs.fns.get(&callee_fn) else { return Ok(()) };
        let (marker, params) = (sig.receiver_marker, sig.param_markers.len());
        if !marker.is_some_and(|m| m.is_retain())
            && receiver_typed.mutability == Mutability::Mutable
            && self.ctx.sigs.param_stored_at(&callee_fn, params) {
            return Err(self.ctx.keeps_receiver_error(callee, receiver));
        }
        let Some(marker) = marker else { return Ok(()) };
        if !marker.is_mut() {
            return Ok(());
        }
        if receiver_typed.writable == Mutability::Immutable {
            return Err(self.immutable_receiver_error(callee, receiver, "declares `mut this`"));
        }
        self.claim_receiver_write_ownership(receiver)?;
        if marker.is_retain() {
            // A borrow cannot be given away, so it may not feed a consuming receiver.
            if self.arg_is_borrowed(receiver) {
                return Err(self.consumes_borrow_error(callee, receiver));
            }
            self.transfer_write_ownership(receiver)?;
        }
        Ok(())
    }

    /// Checks a user call's arguments against the resolved function's declared parameters.
    pub(super) fn check_call_args(&mut self, callee: &HirId<HirExpr>, callee_fn: HirId<HirStmt>, arg_types: &[ValueState], args: &[HirId<HirExpr>]) -> Result<(), anyhow::Error> {
        self.resolved_callees.insert(*callee, callee_fn);
        // Read the params through the shared signatures borrow so the later check can take &mut self.
        let sigs = self.ctx.sigs;
        let Some(sig) = sigs.fns.get(&callee_fn) else { return Ok(()) };
        self.check_arg_mutability(callee, &sig.param_markers, arg_types, args)?;
        self.check_arg_obligations(callee, &sig.param_clauses, arg_types, args)?;
        self.check_args(callee, &sig.param_clauses, arg_types, args)?;
        self.consume_move_args(&sig.param_markers, args)?;
        Ok(())
    }

    /// A call through a value: the callee must be non-null and its result is a dynamic boundary.
    pub(super) fn indirect_call(&mut self, callee: &HirId<HirExpr>) -> Result<ValueState, anyhow::Error> {
        let callee_typed = self.expr(callee)?;
        if let Some(witness) = self.ctx.obligation_witness_name(&callee_typed) {
            let subject = self.ctx.arg_display_name(callee);
            return Err(self.ctx.witness_use_error(format!("{subject} is not callable"), callee, witness));
        }
        self.ctx.require_usable_value(&callee_typed, callee)?;
        Ok(ValueState::unknown())
    }

    pub(super) fn assign(&mut self, lhs: &HirId<HirExpr>, rhs: &HirId<HirExpr>) -> Result<ValueState, anyhow::Error> {
        // A factory's epilogue copies each field-local onto `this` with `this.<field> = $<field>`.
        // When the factory forgets a field, that local is never assigned, so report the copy as the
        // missing field rather than the synthetic local used before assignment.
        if self.checking_factory {
            if let Some(field) = self.never_initialized_factory_field(lhs, rhs) {
                return Err(self.error_help(format!("Non-null field '{}' is never initialized", self.ctx.hir.text(field)), lhs,
                    "assign it in the factory, or give the field a default"));
            }
        }
        let state = self.expr(rhs)?;
        match self.ctx.hir.get(lhs) {
            HirExpr::Identifier(name) => {
                let name = *name;
                if let Some(i) = self.frame_index_of(name) {
                    self.check_reassignable(i, name, lhs)?;
                    let owed = self.locals[i].owed.clone();
                    self.check_into_slot(&state.debt, &owed, name, lhs)?;
                    self.locals[i].assigned = true;
                    self.locals[i].tag = state.tag.clone();
                    // The mutability follows the value, so a rebind takes the new value's.
                    self.locals[i].alias.mutability = state.mutability;
                    self.locals[i].alias.unproven_borrow = self.holds_unproven_borrow(rhs);
                    self.locals[i].alias.confined = self.value_is_confined(rhs);
                    self.locals[i].alias.borrowed = self.arg_is_borrowed(rhs);
                    // The old value is dropped here, so whatever lent it gets its write-ownership back.
                    self.reclaim_on_rebind(i);
                    // A rebind installs a fresh value, so any earlier move of the slot is undone.
                    self.locals[i].alias.transfer_site = None;
                    // The slot takes on whatever sources the new value reaches, and names whatever
                    // element the new value came out of. Any writer slot the old value held is let go.
                    self.locals[i].alias.provenance = self.provenance_of(rhs);
                    self.locals[i].alias.extracted_from = self.extraction_of(rhs).into_iter().collect();
                    self.locals[i].alias.shared_origin = self.shared_origin(rhs);
                    // A rebind does not undo where the old value already went, so the flag only
                    // ever gains reasons to be set.
                    self.locals[i].alias.may_be_shared |= self.locals[i].alias.shared_origin
                        || self.locals[i].alias.borrowed || !self.locals[i].alias.extracted_from.is_empty();
                    // The old value keeps its runtime slot until told otherwise, so a rebind that
                    // drops a held slot has to give it back where the value changes.
                    if self.locals[i].alias.wrote_at.take().is_some() {
                        self.locals[i].alias.slot_taken = false;
                    }
                    self.reset_narrowing(i, matches!(state.debt, Debt::Clean));
                } else if self.ctx.sigs.is_type(name) {
                    // A type binding names a declaration, not a reassignable slot.
                    return Err(self.error(format!("Cannot reassign `{}`; it names a type", self.ctx.hir.text(name)), lhs));
                } else if matches!(self.ctx.bindings.place_of(lhs), Some(Place::Upvalue(_))) {
                    if let Some(i) = self.enclosing_index(name) {
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
        self.transfer_write_ownership(rhs)?;
        Ok(state)
    }

    /// Refuses a rebind of a binding that was not declared reassignable. A slot with no value yet
    /// is being initialized rather than reassigned, which every binding permits once.
    fn check_reassignable(&self, i: usize, name: Symbol, lhs: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let text = self.ctx.hir.text(name);
        if self.locals[i].func.is_some() {
            return Err(self.error(format!("Cannot reassign `{text}`; it names a function"), lhs));
        }
        if self.locals[i].reassignable || !self.locals[i].assigned {
            return Ok(());
        }
        if self.locals[i].binder.is_some() {
            return Err(self.error_help(format!("Cannot reassign matcher binder `{text}`"), lhs,
                format!("copy it into a `say var {text}` first to change it")));
        }
        Err(self.error_help(format!("Cannot reassign binding `{text}`"), lhs,
            format!("you can make `{text}` reassignable by declaring it as `say var {text}`")))
    }

    /// Checks an assignment `target.member = value`.
    pub(super) fn assign_index(&mut self, target: &HirId<HirExpr>, member: &HirId<HirExpr>, is_dot: bool, value: &Debt, lhs: &HirId<HirExpr>, rhs: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        // `this.field = ...` and `this["field"] = ...` both assign a field of the enclosing type.
        if matches!(self.ctx.hir.get(target), HirExpr::This) {
            if let Some(field) = self.ctx.string_member(member) {
                self.assign_field_this(field, value, lhs, rhs)?;
            }
            return Ok(());
        }

        // Storing into a local's field or element gives that local the value's sources, so the
        // value flows back to them when it dies.
        self.attach_field_provenance(target, rhs);

        let slot = self.local_of(target);

        // An immutable value rejects mutation at compile time. A binding that gave its writing to
        // a closure may not be written through this name either, which `write_permission` folds in.
        if let Some(i) = slot.filter(|&i| self.write_permission(i) == Mutability::Immutable) {
            return Err(self.immutable_mutation_error(target, i));
        }

        // A captured name is written through an upvalue.
        if slot.is_none() {
            let captured = self.enclosing_of(target)
                .filter(|&i| self.locals[i].alias.mutability == Mutability::Immutable);
            if let Some(i) = captured {
                return Err(self.immutable_mutation_error(target, i));
            }
        }

        // An immutable base seals every place under it, so `this.arr[0] = 1` in a read-only method is refused.
        if slot.is_none() && self.sealed_base(target) {
            return Err(self.sealed_write_error(target));
        }

        // Writing through an index is a use of the target, so a moved binding is a use after move.
        match slot {
            Some(i) => {
                self.settle_unknown_transfer(i, target);
                self.claim_element_write_ownership(i, target)?;
                self.record_sole_write(i, target);
            },
            // A claim is recorded against a frame-local, and this target is not one. The store
            // carries its root, so it asks the question for itself.
            None => {},
        }

        // A bracket index `obj[expr] = ...` is the dynamic data path. It bypasses the field rules,
        // but writing through the base still captures it, so an enclosing binding it names may move.
        if !is_dot {
            // A target that names no binding is an expression in its own right, so its own checks
            // run only if it is walked. The dot path below reaches it through the receiver.
            if slot.is_none() {
                self.receiver(target)?;
            }
            if let (None, HirExpr::Identifier(name)) = (slot, self.ctx.hir.get(target)) {
                self.capture_enclosing(*name, target);
            }
            return Ok(());
        }

        self.path_base = Some(*target);
        let receiver = self.receiver(target)?;
        self.path_base = None;
        let Some(field) = self.ctx.string_member(member) else { return Ok(()) };
        if let TypeTag::Concrete(decl) = &receiver.tag {
            self.assign_field_external(&decl.clone(), field, value, lhs, rhs)?;
        }
        Ok(())
    }

    /// Checks an assignment `this.field = value`.
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

        // Writing an immutable field in a factory is its initialization. Elsewhere it is a method
        // mutating a finished value, which an immutable field rejects.
        if !mutable && !self.checking_factory {
            return Err(self.ctx.non_var_field_error(&type_stmt, field, lhs));
        }

        // Writing a field is a use of the receiver, so an owing `this` has to be discharged first.
        let this = self.this_typed();
        self.ctx.require_usable_value(&this, lhs)?;

        // Mutating a field is mutating the receiver, so the method has to have asked for one.
        if !self.checking_factory && this.mutability == Mutability::Immutable {
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
        // A non-field or non-public member is invisible to external code.
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

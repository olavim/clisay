//! The walk: `stmt` and `expr` dispatch over the HIR, and each site gathers the
//! context its rules need and calls them in a fixed order.


use crate::core::objects::TypeMember;
use crate::middle::diagnose::Diagnose;
use crate::middle::hir::{access_path_steps, BinOp, HirCatchClause, HirExpr, HirSayDecl, HirFnDecl, HirId, HirLiteral, HirMatcher, HirStmt, HirTypeDecl, Symbol, UnOp, WritePlace};
use crate::middle::bind::Place;
use crate::middle::anchors;
use crate::middle::obligations::{ObligationRule, Site, obligation_atoms, quoted_obligation_list};
use crate::middle::native::{self, Container};
use crate::middle::obligations::Obligations;
use crate::middle::signatures::CallableId;
use crate::middle::signatures::{NativeType, TypeTag};

use super::scope::FlowSnapshot;
use super::write_order;
use super::paths::possible_step;

#[derive(Clone, Copy, PartialEq)]
enum Dropped {
    /// `say _ = e;`
    OnPurpose,
    Silently
}

use super::{BadCallee, Callee, CallableState, PatternBinderSource, Checker, Ctx, Debt, FlowPath, FnContext, Guard, Local, OperandKind, ProvenFacts, PathMap, PathStep, PossibleFacts, Route, UnroutedValueState, ValueState, WriteRoot, ELEMENTS};
use super::conform::ANCHOR_OWES_ONLY_WITNESSED;
use super::barriers::CheckedSlot;

#[derive(Clone, Copy)]
enum WriteTarget { Local(usize), Path(HirId<HirExpr>), Anchor(HirId<HirExpr>), Receiver }

impl<'a> Ctx<'a> {
    pub(super) fn refuse_non_var_field(&self, decl: &HirId<HirStmt>, field: Symbol, lhs: &HirId<HirExpr>, anchored: bool) -> Result<(), anyhow::Error> {
        let name = self.qualified_field_display_name(decl, field);
        let message = match anchored {
            true => format!("Cannot anchor non-var field `{name}`"),
            false => format!("Cannot reassign field `{name}`"),
        };
        // A built-in has no declaration the program can reach, so there is nothing to suggest.
        if self.declares_a_builtin(decl) {
            return Err(self.error(message, lhs));
        }
        let hint = self.var_decl_error_hint(decl, field);
        let help = match anchored {
            true => format!("declare it as `{hint};`"),
            false => format!("you can make `{name}` reassignable by declaring it as `{hint};`"),
        };
        Err(self.error_help(message, lhs, help))
    }

    pub(super) fn declares_a_builtin(&self, decl: &HirId<HirStmt>) -> bool {
        matches!(self.hir.get(decl), HirStmt::Type(decl) if decl.builtin.is_some())
    }

    pub(super) fn method_assign_error(&self, field: Symbol, lhs: &HirId<HirExpr>) -> anyhow::Error {
        self.error(format!("Cannot assign to method '{}'", self.hir.text(field)), lhs)
    }

    fn var_decl_error_hint(&self, decl: &HirId<HirStmt>, field: Symbol) -> String {
        let visibility = self.layout_of(decl).map_or("", |layout| {
            if layout.is_public(field) { "pub " } else if layout.is_inner(field) { "inner " } else { "" }
        });
        format!("{visibility}var {}", self.hir.text(field))
    }

    pub(super) fn arg_display_name(&self, arg: &HirId<HirExpr>) -> String {
        match self.hir.get(arg) {
            HirExpr::Identifier(name) => format!("`{}`", self.hir.text(*name)),
            _ => "this value".to_string(),
        }
    }

    pub(super) fn callee_display_name(&self, callee: &HirId<HirExpr>) -> String {
        match self.hir.get(callee) {
            HirExpr::Identifier(name) => format!("`{}`", self.hir.text(*name)),
            HirExpr::Index { member, is_dot: true, .. } => self.member_display_name(member).map_or_else(|| "this function".to_string(), |m| format!("`{m}`")),
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
        self.hir.member_symbol(member)
    }

    pub(super) fn member_step(&self, member: &HirId<HirExpr>) -> Option<PathStep> {
        match self.hir.get(member) {
            HirExpr::Literal(HirLiteral::String(name)) => self.hir.symbol_of(name).map(PathStep::Field),
            HirExpr::Literal(HirLiteral::Number(at)) => index_step_of(*at),
            _ => None,
        }
    }
}

/// The step a literal's position names. Only the first 256 are recorded.
fn index_step(at: usize) -> Option<PathStep> {
    u8::try_from(at).ok().map(|offset| PathStep::Index { offset, from_back: false })
}

fn index_step_of(at: f64) -> Option<PathStep> {
    if at < 0.0 || at.fract() != 0.0 {
        return None;
    }
    index_step(at as usize)
}

impl<'a> Checker<'a> {
    pub(super) fn stmt(&mut self, stmt: &HirId<HirStmt>) -> Result<(), anyhow::Error> {
        match self.ctx.hir.get(stmt) {
            HirStmt::Nop => {},
            HirStmt::Fn(decl) => self.function((*stmt).into(), decl)?,
            HirStmt::Type(decl) => self.type_decl(stmt, Some(*stmt), decl)?,
            HirStmt::Trait(decl) => self.type_decl(stmt, None, decl)?,
            HirStmt::Say(field) => self.say(stmt.index(), field)?,
            HirStmt::Expression(e) => self.statement_result(e, Dropped::Silently)?,
            HirStmt::Discard(e) => self.statement_result(e, Dropped::OnPurpose)?,
            HirStmt::Block(e) => { self.expr(e)?.route(self, e, Route::Body)?; },
            HirStmt::Defer(e) => {
                let outer = std::mem::replace(&mut self.fn_ctx.in_defer, true);
                let walked = self.effects_of(|c| c.expr(e)?.route(c, e, Route::Body));
                self.fn_ctx.in_defer = outer;
                let (_, effects) = walked?;
                self.defer_reads.push(effects.reads.clone());
                self.add_effects(effects);
            },
            HirStmt::Return(_) if self.fn_ctx.in_defer => {
                return Err(self.error_help(
                    "A 'defer' body cannot return".to_string(), stmt,
                    "a 'defer' runs while its block is already leaving, so there is no return left to make"));
            },
            HirStmt::Return(opt) => match opt {
                Some(e) => {
                    let state = self.expr(e)?.route(self, e, Route::Returned)?;
                    self.check_return(&state.debt, e)?;
                },
                None if self.bare_return_refused() => {
                    return Err(self.error("This 'return' returns null, which the declared return does not admit".to_string(), stmt));
                },
                None => {},
            },
            HirStmt::Throw(e) => {
                self.expr(e)?.route(self, e, Route::Thrown)?;
            },
            HirStmt::While(cond, body) => {
                let (body_narrow, _) = self.condition_narrowings(cond)?;
                let scope = self.condition_pattern_binders(cond)?;
                let pre = self.snapshot();
                // Check the body twice. A rebind late in the body drops a narrowing an earlier read
                // relied on, and only the second pass reads that line under a later iteration's
                // state.
                for _ in 0..2 {
                    self.apply_narrowings(&body_narrow);
                    self.with_binders(&scope, body, |c| c.expr(body)?.route(c, body, Route::Body))?;
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
                    c.with_binders(&scope, then, |c| c.expr(then)?.route(c, then, Route::Body))?;
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
            HirStmt::Try(body, catch, finally) => self.try_statement(body, catch, finally)?,
            HirStmt::Match(scrutinee, arms) => {
                let state = self.expr(scrutinee)?.route(self, scrutinee, Route::Matched)?;
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
                        if let Some(guard) = &arm.guard { c.expr(guard)?.route(c, guard, Route::Condition)?; }
                        c.expr(&arm.body)?.route(c, &arm.body, Route::Body)?;
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

    pub(super) fn expr(&mut self, expr: &HirId<HirExpr>) -> Result<UnroutedValueState, anyhow::Error> {
        Ok(UnroutedValueState::new(match self.ctx.hir.get(expr) {
            HirExpr::Literal(HirLiteral::Null) => ValueState::of(self.ctx.opt_debt(true), TypeTag::Unknown),
            HirExpr::Literal(HirLiteral::Lambda(decl)) => self.lambda_value(decl, expr)?,
            HirExpr::Literal(lit) => {
                let tag = match lit {
                    HirLiteral::Array(_) => Some(NativeType::Array),
                    HirLiteral::Dict(_) => Some(NativeType::Dict),
                    HirLiteral::String(_) => Some(NativeType::String),
                    _ => None,
                };
                if matches!(tag, Some(NativeType::Array | NativeType::Dict)) {
                    self.out.fresh_values.insert(*expr);
                }
                let tag = tag.map_or(TypeTag::Unknown, TypeTag::Native);
                let (proven, possible) = self.literal_children(lit)?;
                ValueState::of(Debt::Clean, tag).proving(proven).possibly(possible)
            },
            HirExpr::Identifier(name) => self.identifier(*name, expr)?,
            HirExpr::RefValue { holder, .. } => self.ref_value(holder)?,
            HirExpr::This => self.this_valuestate(),
            HirExpr::Assign(lhs, rhs) => self.safe_access_write(lhs, |c, safe_access| {
                let (value, skipped_in_walk) = c.assign(lhs, rhs, safe_access)?;
                Ok((value.as_stored(), skipped_in_walk))
            })?,
            HirExpr::CompoundAssign(lhs, _, rhs) => self.safe_access_write(lhs, |c, safe_access| {
                c.expr(lhs)?.route(c, lhs, Route::CompoundAssignTarget)?;
                c.assign(lhs, rhs, safe_access)
            })?,
            HirExpr::Call(callee, args) => {
                if self.ctx.resolved().type_named(callee).is_some() {
                    self.out.fresh_values.insert(*expr);
                }
                self.call(expr, callee, args)?
            },
            HirExpr::Construct(callee, brace) => {
                self.out.fresh_values.insert(*expr);
                self.expr(callee)?.route(self, callee, Route::Callee)?;
                let tag = self.ctx.construct_tag(callee);
                let mut possible: PathMap<PossibleFacts> = PathMap::new();
                for (name, v) in brace {
                    let state = self.expr(v)?.route(self, v, Route::BraceField)?;
                    if let TypeTag::Concrete(decl) = &tag {
                        self.check_into_brace_field(&decl.clone(), *name, &state.debt, v)?;
                    }
                    possible.join(&state.possible.under(&[PathStep::Field(*name)]));
                }

                let debt = match &tag {
                    TypeTag::Concrete(decl) => self.ctx.construction_debt(decl),
                    _ => Debt::Clean,
                };
                ValueState::of(debt, tag).possibly(possible)
            },
            HirExpr::Index { base, member, is_dot, safe: false } => self.member_access(base, member, *is_dot)?,
            // `a?.b` and `a?[i]` short-circuit on a bad operand.
            HirExpr::Index { base, member, is_dot, safe: true } => {
                let target = self.expr(base)?.route(self, base, Route::MemberBase)?;
                self.ctx.require_witnessed_operand(&target.debt, base)?;
                let yielded = self.member_access_of(&target, base, member, *is_dot)?;
                self.chained_result(&target.debt, &yielded.debt).possibly(yielded.possible)
            },
            HirExpr::Binary(op, l, r) => self.binary(*op, l, r)?,
            HirExpr::Unary(op, x) => self.unary(*op, x)?,
            HirExpr::Match(scrutinee, _) => {
                let state = self.expr(scrutinee)?.route(self, scrutinee, Route::Matched)?;
                if state.debt.is_void() {
                    return Err(self.error("This call returns no value, so its result cannot be matched here".to_string(), scrutinee));
                }
                ValueState::nonnull()
            },
            HirExpr::Block(stmts) => {
                let mark = self.locals.len();
                let defers = self.defer_reads.len();
                self.hoist_declarations(stmts);
                let walked = stmts.iter().try_for_each(|s| self.walk_block_stmt(s));
                self.defer_reads.truncate(defers);
                walked?;
                let dropped = self.check_dropped(mark, expr);
                self.truncate_locals(mark);
                dropped?;
                ValueState::unknown()
            },
            // `a ?? b` discharges the whole obligation set.
            HirExpr::Coalesce(l, r) => {
                let left = self.expr(l)?.route(self, l, Route::CoalesceOperand)?;
                self.refuse_void_operand(&left.debt, "??", l)?;
                self.ctx.require_witnessed_operand(&left.debt, l)?;
                let right = self.expr(r)?.route(self, r, Route::CoalesceOperand)?;
                ValueState::coalesced(left, right)
            },
            HirExpr::SafeCall(callee, args) => self.safe_call(expr, callee, args)?,
            HirExpr::Propagate(operand) if self.fn_ctx.in_defer => {
                return Err(self.error_help(
                    "A 'defer' body cannot propagate with '?!'".to_string(), expr,
                    "'?!' returns the bad value, and the block is already leaving; handle it with '??' or '!' instead"));
            },
            HirExpr::Propagate(operand) if self.fn_ctx.returns_void => {
                return Err(self.error_help(
                    "Cannot propagate from a void function".to_string(), expr,
                    "'?!' returns the witness value, so the function has to return a value on every path"));
            },
            HirExpr::Propagate(operand) => {
                let state = self.expr(operand)?.route(self, operand, Route::Propagated)?;
                self.refuse_void_operand(&state.debt, "?!", operand)?;
                self.ctx.require_witnessed_operand(&state.debt, operand)?;
                ValueState::of(self.ctx.discharged_debt(&state.debt), state.tag).possibly(state.possible)
            },
            // `a ?? p => h` binds the caught bad value to `p`, which still owes what `a` owed. A
            // single type witness narrows `p`'s tag, so a caught `Err` is usable as one.
            HirExpr::Handle(left_id, binder, handler) => {
                let left = self.expr(left_id)?.route(self, left_id, Route::CoalesceOperand)?;
                self.refuse_void_operand(&left.debt, "??", left_id)?;
                self.ctx.require_witnessed_operand(&left.debt, left_id)?;
                let caught = self.ctx.obligations_of(&left.debt);
                let tag = self.ctx.handle_caught_tag(&caught);
                let held = match left.debt {
                    Debt::Unknown => ValueState::unknown(),
                    _ => ValueState::of(Debt::Owed { obligations: caught, definite: false }, tag),
                };

                let mut binder_local = Local::pattern_binder(*binder, &held, PatternBinderSource::Handler).as_used();
                binder_local.decl = Some(expr.index());
                let mark = self.locals.len();
                self.locals.push(binder_local);
                let h = self.expr(handler)?.route(self, handler, Route::CoalesceOperand)?;
                self.close_scope(mark, handler)?;
                ValueState::coalesced(left, h)
            },
            HirExpr::Anchor(_) => return Err(self.error_help(
                "An anchor is not a value".to_string(), expr,
                "`&x` can only be passed to a call or bound with `say var`")),
            // `a!` asserts the value is clean, keeping its type tag. A barrier guards it unless
            // the operand is already proven clean.
            HirExpr::Assert(x) => {
                let state = self.expr(x)?.route(self, x, Route::Asserted)?;
                self.ctx.require_witnessed_operand(&state.debt, x)?;
                if self.ctx.owes_object_witness(&state.debt) || matches!(state.debt, Debt::Unknown) {
                    self.record_witness_assert(expr);
                } else if !matches!(state.debt, Debt::Clean) {
                    self.record_guard(expr, Guard::NonNull);
                } else {
                    self.record_elision(expr, Guard::NonNull);
                }
                ValueState::of(self.ctx.discharged_debt(&state.debt), state.tag).possibly(state.possible)
            },
        }))
    }

    fn statement_result(&mut self, e: &HirId<HirExpr>, dropped: Dropped) -> Result<(), anyhow::Error> {
        let state = self.expr(e)?.route(self, e, Route::StatementResult)?;
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
        let (name, reassignable, value, pattern) = (field.name, field.reassignable, &field.value, &field.pattern);
        let owed = field.clause.owed();
        // With no initializer, or one whose debt isn't known, all that can be said is what the slot declares.
        let declared = match owed.is_empty() {
            true => Debt::Clean,
            false => Debt::Owed { obligations: owed.clone(), definite: false },
        };
        let (assigned, held) = if let Some(value) = value {
            let value_flow = self.expr_or_anchor(value)?;
            self.check_into_slot(value_flow.debt(), &owed, name, value)?;
            let route = match pattern {
                Some(_) => Route::Matched,
                None => Route::NewLocal,
            };
            let state = value_flow.route(self, value, route)?;
            let held = match state.debt {
                Debt::Unknown => state.with_debt(declared),
                _ => state,
            };
            (true, held)
        } else {
            (false, ValueState::of(declared, TypeTag::Unknown))
        };
        let anchor = value.filter(|v| matches!(self.ctx.hir.get(v), HirExpr::Anchor(_)));
        if let Some(anchor) = anchor {
            self.check_anchor_binding_var(name, reassignable, &owed, &anchor)?;
        }
        let mut local = Local::say(name, owed.clone(), &held, reassignable, assigned);
        local.is_anchor = anchor.is_some();
        local.site = *value;
        local.decl = Some(decl);
        self.out.checked_slot_clauses.insert(CheckedSlot::Say(HirId::from_index(decl)), local.clause_owed().clone());
        local.resolved_callable = value.and_then(|v| self.callable_named_by(&v));
        let tag = local.tag().clone();
        self.locals.push(local);

        if let (Some(pattern), Some(value)) = (pattern, value) {
            self.check_say_pattern_else(pattern, value, &tag, &field.otherwise)?;
            let scope = self.ctx.say_pattern_binders(pattern, value, reassignable)?;
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
        self.expr(otherwise)?.route(self, otherwise, Route::Body)?;
        if !self.ctx.hir.definitely_returns(otherwise) {
            return Err(self.ctx.error_help("the `else` branch of a `say` pattern statement must diverge".to_string(), otherwise,
                "the binding's names do not exist below it, so it has to `return` or `throw`"));
        }
        Ok(())
    }

    fn literal_children(&mut self, lit: &HirLiteral) -> Result<(PathMap<ProvenFacts>, PathMap<PossibleFacts>), anyhow::Error> {
        let mut proven: PathMap<ProvenFacts> = PathMap::new();
        let mut possible: PathMap<PossibleFacts> = PathMap::new();
        let mut shared: Option<Obligations> = None;
        let mut any_unknown = false;
        let mut element = |c: &mut Self, step: Option<PathStep>, value: &HirId<HirExpr>| -> Result<(), anyhow::Error> {
            let state = c.expr(value)?.route(c, value, Route::ContainerElement)?;
            c.store_into_container(&state.debt, value)?;
            let owed = match state.debt {
                Debt::Unknown => None,
                _ => Some(c.ctx.obligations_of(&state.debt)),
            };
            any_unknown |= owed.is_none();
            possible.join(&state.possible.under(&[possible_step(step)]));
            if let Some(step) = step {
                proven.insert(vec![step], ProvenFacts { owed: owed.clone(), tag: state.tag.clone() });
                proven.extend(state.proven.under(&[step]));
            }
            if let Some(owed) = owed {
                shared = Some(match shared.take() {
                    None => owed,
                    Some(mut common) => { common.retain(|o| owed.contains(o)); common },
                });
            }
            Ok(())
        };
        match lit {
            HirLiteral::Array(elems) => for (at, e) in elems.iter().enumerate() {
                element(self, index_step(at), e)?;
            },
            HirLiteral::Dict(pairs) => for (k, v) in pairs {
                self.expr(k)?.route(self, k, Route::MemberKey)?;
                let step = self.ctx.member_step(k);
                element(self, step, v)?;
            },
            _ => {},
        }
        // One unknown element leaves every position unknown.
        let positional = matches!(lit, HirLiteral::Array(_));
        if let Some(shared) = shared.filter(|s| !s.is_empty() && !any_unknown && positional) {
            proven.insert(ELEMENTS.to_vec(), ProvenFacts { owed: Some(shared), tag: TypeTag::Unknown });
        }
        Ok((proven, possible))
    }

    /// Walks a `try` statement and leaves the flow the code below it starts from.
    fn try_statement(&mut self, body: &HirId<HirExpr>, catch: &Option<HirCatchClause>, finally: &Option<HirId<HirExpr>>) -> Result<(), anyhow::Error> {
        // Two states track how control leaves the try and the catch. `normal_exit` is the state after
        // the try body runs to completion, joined with the state after the catch runs to completion.
        // `any_exit` also covers leaving early. A throw or a return can happen at any point in the try
        // body or the catch, so `any_exit` keeps only what is true throughout them.
        //
        // The catch starts from `any_exit`, because the try body may have thrown at any point. The finally
        // runs on every path, so it starts from `any_exit` too. Only a normal exit reaches the code below
        // the whole try-catch statement, so that code starts from `normal_exit` plus what the finally proved
        // and wrote.
        let mut any_exit = self.walk_and_join(self.snapshot(), |c| c.expr(body)?.route(c, body, Route::Body).map(drop))?;
        let mut normal_exit = self.snapshot();

        if let Some(catch) = catch {
            // The catch can start from any point the body threw from.
            any_exit = self.walk_and_join(any_exit, |c| c.catch_clause(catch))?;

            // A catch that leaves the scope does not reach the code below.
            if !self.ctx.hir.definitely_returns(&catch.body) {
                self.join_current_into(&mut normal_exit);
            }
        }

        match finally {
            None => self.restore_flow(&normal_exit),
            Some(finally) => {
                self.restore_flow(&any_exit);
                let (_, effects) = self.effects_of(|c| c.expr(finally)?.route(c, finally, Route::Body))?;
                self.leave_finally(&normal_exit, &effects);
                self.add_effects(effects);
            },
        }
        Ok(())
    }

    /// Walks one subtree from `start`, and returns `start` joined with the flow after each of
    /// the subtree's writes and the flow at its end. The returned flow holds at every point the
    /// subtree might have exited from.
    fn walk_and_join(&mut self, start: FlowSnapshot, walk: impl FnOnce(&mut Self) -> Result<(), anyhow::Error>) -> Result<FlowSnapshot, anyhow::Error> {
        self.restore_flow(&start);
        let (_, effects) = self.effects_of(walk)?;
        let mut joined = start;
        joined.join_writes(&effects);
        self.add_effects(effects);
        self.join_current_into(&mut joined);
        Ok(joined)
    }

    fn catch_clause(&mut self, catch: &HirCatchClause) -> Result<(), anyhow::Error> {
        let mark = self.locals.len();
        if let Some(param) = catch.param {
            let name = self.ctx.hir.ident_sym(&param);
            let mut local = Local::catch(name);
            local.decl = Some(param.index());
            self.locals.push(local);
        }
        self.expr(&catch.body)?.route(self, &catch.body, Route::Body)?;
        self.close_scope(mark, &catch.body)
    }

    /// Records what may now be at `path` under local `i`.
    fn write_possible(&mut self, i: usize, path: FlowPath, value: &PathMap<PossibleFacts>) {
        let possible = &mut self.locals[i].possible;
        if !path.contains(&PathStep::EveryElement) {
            possible.retain(|at, _| !at.starts_with(&path));
        }
        possible.join(&value.under(&path));
    }

    fn note_read(&mut self, i: usize) {
        self.locals[i].used = true;
    }

    fn refuse_unassigned_read(&self, i: usize, name: Symbol, expr: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        if self.locals[i].assigned || self.locals[i].fn_decl {
            return Ok(());
        }
        let text = self.ctx.binding_display_name(name);
        let subject = if self.ctx.is_factory_field(name) { format!("Field '{text}'") } else { format!("'{text}'") };
        Err(self.error(format!("{subject} is used before it is assigned"), expr))
    }

    fn read_name(&mut self, name: Symbol, at: &HirId<HirExpr>) -> Result<Option<usize>, anyhow::Error> {
        let named = self.note_declaration_use(at);
        if let Some(i) = self.local_read_at(at) {
            self.refuse_unassigned_read(i, name, at)?;
            self.note_local_read(i);
        }
        Ok(named)
    }

    pub(super) fn identifier(&mut self, name: Symbol, expr: &HirId<HirExpr>) -> Result<ValueState, anyhow::Error> {
        let Some(i) = self.frame_index_of(name) else {
            // A read that resolves to an enclosing frame is a closure capture.
            if let Some(j) = self.capture_index(name) {
                self.note_read(j);
            }
            let named = self.read_name(name, expr)?;
            self.refuse_anchor_capture(name, expr)?;
            self.refuse_capture_of_persisting_value(name, expr)?;
            return Ok(self.captured_read(name).possibly_callable(named));
        };

        self.note_read(i);
        let named = self.read_name(name, expr)?;
        if self.locals[i].fn_decl {
            return Ok(ValueState::unknown().possibly_callable(named));
        }

        let owed = self.locals[i].owed().clone();
        let debt = self.locals[i].read_debt(owed);

        Ok(ValueState::of(debt, self.locals[i].tag().clone())
            .proving(self.locals[i].proven.below_root())
            .possibly(self.locals[i].possible.clone()))
    }

    fn refuse_void_operand(&self, debt: &Debt, op: &str, at: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        if !debt.is_void() {
            return Ok(());
        }
        Err(self.ctx.error_help(
            format!("Unexpected void operand"), at,
            format!("The call's result cannot be used as an operand of '{op}', because it returns no value.")))
    }

    pub(super) fn ref_value(&mut self, holder: &HirId<HirExpr>) -> Result<ValueState, anyhow::Error> {
        let stored = self.ctx.ref_debt();
        if self.ctx.hir.path_has_safe_access(holder) {
            let base = self.expr(holder)?.route(self, holder, Route::RefHolder)?;
            return Ok(self.chained_result(&base.debt, &stored));
        }
        self.discharged_or_narrowed_base(holder)?.route(self, holder, Route::RefHolder)?;
        Ok(ValueState::of(stored, TypeTag::Unknown))
    }

    /// Member or data access `target.member` / `target[member]`.
    pub(super) fn member_access(&mut self, target: &HirId<HirExpr>, member: &HirId<HirExpr>, is_dot: bool) -> Result<ValueState, anyhow::Error> {
        if self.ctx.hir.path_has_safe_access(target) {
            let base = self.expr(target)?.route(self, target, Route::MemberBase)?;
            let stepped = self.member_access_of(&base.as_clean(), target, member, is_dot)?;
            return Ok(self.chained_result(&base.debt, &stepped.debt).possibly(stepped.possible));
        }
        let base = self.discharged_or_narrowed_base(target)?.route(self, target, Route::MemberBase)?;
        self.member_access_of(&base, target, member, is_dot)
    }

    pub(super) fn member_access_of(&mut self, receiver_state: &ValueState, receiver: &HirId<HirExpr>, member: &HirId<HirExpr>, is_dot: bool) -> Result<ValueState, anyhow::Error> {
        let possible = receiver_state.possible.inside(possible_step(self.ctx.member_step(member)));
        Ok(self.proven_member(receiver_state, receiver, member, is_dot)?.possibly(possible))
    }

    fn proven_member(&mut self, receiver_state: &ValueState, receiver: &HirId<HirExpr>, member: &HirId<HirExpr>, is_dot: bool) -> Result<ValueState, anyhow::Error> {
        // A value that knows what is inside it answers for the step itself.
        let step = self.ctx.member_step(member);
        let named = step.and_then(|s| receiver_state.proven.at(s).map(|f| (s, f.clone())));
        let known = named.or_else(|| {
            (!is_dot).then(|| receiver_state.proven.at(PathStep::EveryElement).map(|f| (PathStep::EveryElement, f.clone()))).flatten()
        });
        if let Some((step, facts)) = known {
            if self.ctx.member_display_name(member).is_none() {
                self.expr(member)?.route(self, member, Route::MemberKey)?;
            }
            return Ok(ValueState::of(facts.debt(), facts.tag).proving(receiver_state.proven.inside(step)));
        }

        let Some(name) = self.ctx.member_display_name(member) else {
            self.expr(member)?.route(self, member, Route::MemberKey)?;
            return Ok(ValueState::unknown());
        };

        if matches!(receiver_state.tag, TypeTag::SelfType) {
            return self.trait_member(name, member);
        }

        if is_dot || !receiver_state.tag.keys_can_shadow_members() {
            self.require_member_exists(receiver_state, receiver, name)?;
            self.refuse_binding_anchored_method(&receiver_state.tag, name, member)?;
        }

        let Some(field) = self.ctx.hir.symbol_of(name) else { return Ok(ValueState::unknown()) };
        let narrowing = self.narrowable_field(receiver, field);
        if let TypeTag::Concrete(decl) = &receiver_state.tag {
            if let Some(layout) = self.ctx.layout_of(decl) {
                if let Some(member_kind) = layout.members.get(&field).copied() {
                    let debt = match member_kind {
                        TypeMember::Field(_) => {
                            let owed = match narrowing.as_ref().and_then(|t| self.proved_owed(t)) {
                                Some(recorded) => recorded,
                                None => self.ctx.field_owes(decl, field),
                            };
                            match owed.is_empty() {
                                true => Debt::Clean,
                                false => Debt::Owed { obligations: owed, definite: false },
                            }
                        },
                        // A method reference is a non-null value.
                        TypeMember::Method(_) => Debt::Clean,
                    };

                    let tag = narrowing.as_ref().map(|t| self.proved_tag(t))
                        .filter(|t| *t != TypeTag::Unknown)
                        .or_else(|| self.ctx.resolved().given_trait(*decl, field).map(TypeTag::Concrete))
                        .unwrap_or(TypeTag::Unknown);
                    return Ok(ValueState::of(debt, tag));
                }
            }
        }

        // The type says nothing, but a pattern may have proved what this path is and owes.
        if let Some(target) = narrowing {
            if let Some(owed) = self.proved_owed(&target) {
                let tag = self.proved_tag(&target);
                let debt = match owed.is_empty() {
                    true => Debt::Clean,
                    false => Debt::Owed { obligations: owed, definite: false },
                };
                return Ok(ValueState::of(debt, tag));
            }
        }

        Ok(ValueState::unknown())
    }

    fn refuse_binding_anchored_method(&self, tag: &TypeTag, name: &str, member: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let takes_anchor = match tag {
            TypeTag::Concrete(decl) => self.ctx.hir.symbol_of(name)
                .and_then(|m| self.ctx.sigs.method_of(*decl, m))
                .is_some_and(|method| self.ctx.sigs.wants_anchor_receiver(method)),
            TypeTag::Native(ty) => ty.method_wants_anchor_receiver(name),
            _ => false,
        };
        if !takes_anchor {
            return Ok(());
        }
        Err(self.error_help(format!("`{name}` wants an anchor receiver, so it cannot be bound as a value"), member,
            format!("a bound method keeps a copy of its receiver, so `{name}` would write only the copy")))
    }

    fn require_member_exists(&self, receiver_state: &ValueState, receiver: &HirId<HirExpr>, name: &str) -> Result<(), anyhow::Error> {
        if self.ctx.may_have_member(&receiver_state.tag, name) {
            return Ok(());
        }
        if self.ctx.is_obligation_witness(receiver_state) {
            self.ctx.refuse_blocking_debt(&receiver_state.debt, receiver)?;
        }
        Err(self.ctx.absent_member_error(&receiver_state.tag, name, receiver))
    }

    pub(super) fn discharged_or_narrowed_base(&mut self, base: &HirId<HirExpr>) -> Result<UnroutedValueState, anyhow::Error> {
        let state = self.expr(base)?;
        self.ctx.require_discharged_or_narrowed(state.debt(), state.tag(), base, OperandKind::Base)?;
        Ok(state)
    }

    pub(super) fn binary(&mut self, op: BinOp, l: &HirId<HirExpr>, r: &HirId<HirExpr>) -> Result<ValueState, anyhow::Error> {
        match op {
            // Short-circuit operators narrow their left operand into the right operand.
            BinOp::And | BinOp::Or => {
                self.expr(l)?.route(self, l, Route::Condition)?;
                let into_right = self.narrowings(l, matches!(op, BinOp::And));
                self.narrow_branch(&into_right, |c| c.expr(r)?.route(c, r, Route::Condition))?;
                Ok(ValueState::nonnull())
            },
            BinOp::Equal | BinOp::NotEqual => {
                self.expr(l)?.route(self, l, Route::BinaryOperand)?;
                self.expr(r)?.route(self, r, Route::BinaryOperand)?;
                Ok(ValueState::nonnull())
            },
            _ => {
                let ln = self.expr(l)?.route(self, l, Route::BinaryOperand)?;
                let rn = self.expr(r)?.route(self, r, Route::BinaryOperand)?;
                if self.ctx.is_obligation_witness(&ln) || self.ctx.is_obligation_witness(&rn) {
                    return Err(self.ctx.invalid_operands_error(op, l, &ln, r, &rn));
                }
                self.ctx.require_discharged_or_narrowed(&ln.debt, &ln.tag, l, OperandKind::Whole)?;
                self.ctx.require_discharged_or_narrowed(&rn.debt, &rn.tag, r, OperandKind::Whole)?;
                Ok(ValueState::nonnull())
            },
        }
    }

    pub(super) fn unary(&mut self, op: UnOp, x: &HirId<HirExpr>) -> Result<ValueState, anyhow::Error> {
        let state = self.expr(x)?.route(self, x, Route::UnaryOperand)?;
        if matches!(op, UnOp::Negate | UnOp::BitNot) {
            if let Some(witness) = self.ctx.obligation_witness_name(&state) {
                return Err(self.ctx.witness_use_error(format!("invalid operand of `{op}`: {witness}"), x, witness));
            }
            self.ctx.require_discharged_or_narrowed(&state.debt, &state.tag, x, OperandKind::Whole)?;
        }
        Ok(ValueState::nonnull())
    }

    pub(super) fn captured_read(&self, name: Symbol) -> ValueState {
        let Some(i) = self.capture_index(name) else {
            return ValueState::unknown();
        };
        let local = &self.locals[i];
        if local.fn_decl {
            return ValueState::unknown();
        }

        let owed: Obligations = match local.reassignable {
            // A rebind elsewhere can put anything the slot admits here, so read the clause.
            true => local.clause_owed().clone(),
            false => local.owed().clone(),
        };

        let debt = local.read_debt(owed);

        let state = match local.reassignable {
            true => ValueState::of(debt, TypeTag::Unknown),
            false => ValueState::of(debt, local.tag().clone()),
        };
        state.possibly(local.possible.clone())
    }

    pub(super) fn type_decl(&mut self, node: &HirId<HirStmt>, type_stmt: Option<HirId<HirStmt>>, decl: &HirTypeDecl) -> Result<(), anyhow::Error> {
        let saved_type = self.current_type;
        let witnesses = self.ctx.sigs.obligations_witnessed_by_decl(decl);
        let saved_witnesses = std::mem::replace(&mut self.current_type_witnesses, witnesses);
        let saved_surface = self.current_trait_surface.take();
        let saved_trait = self.current_trait.take();
        let saved_factory = std::mem::replace(&mut self.checking_factory, false);
        self.current_type = type_stmt;
        if type_stmt.is_some() {
            self.checking_factory = true;
            self.method_stmt(&decl.init)?;
            self.checking_factory = false;
        } else {
            // A trait method reaches only the trait's declared surface through `this`.
            self.current_trait_surface = Some(decl.surface.clone());
            self.current_trait = Some(*node);
        }
        for method in &decl.methods {
            self.method_stmt(method)?;
        }
        self.current_type = saved_type;
        self.current_type_witnesses = saved_witnesses;
        self.current_trait_surface = saved_surface;
        self.current_trait = saved_trait;
        self.checking_factory = saved_factory;
        Ok(())
    }

    pub(super) fn function(&mut self, callable: CallableId, decl: &HirFnDecl) -> Result<(), anyhow::Error> {
        // An unmarked return is inferred whole from the body. When it can both finish with no value
        // and return a bad value, the mixed shape must be declared.
        let unmarked = decl.has_no_return_clause();
        if unmarked {
            if let Some(ret) = self.ctx.sigs.fn_sig_of(callable)
                .map(|s| &s.ret).filter(|r| r.void && !r.obligations.is_empty())
            {
                return Err(self.ctx.mixed_void_error(callable, decl, &ret.obligations));
            }
        }

        self.ctx.reject_receiver_witnessed_obligations(decl)?;
        self.ctx.reject_anchor_unwitnessed_obligations(decl)?;

        let receiver = self.receiver_facts(decl);
        if decl.receiver.is_some() {
            self.out.checked_slot_clauses.insert(CheckedSlot::Receiver(decl.body), receiver.clone());
        }
        let ctx = FnContext {
            callable: Some(callable),
            receiver,
            receiver_reassignable: self.receiver_reassignable(decl),
            wants_anchor_receiver: decl.receiver.as_ref().is_some_and(|receiver| receiver.anchor),
            declares_void: decl.clause.void,
            return_owes: !decl.clause.is_empty(),
            return_undeclared: unmarked,
            return_admits: self.ctx.sigs.fn_sig_of(callable).map(|s| s.ret.obligations.clone()),
            returns_void: self.ctx.sigs.fn_sig_of(callable).is_some_and(|s| s.ret.void),
            name: Some(decl.name),
            return_clause: decl.clause.pos.clone(),
            in_defer: false,
        };

        let saved = std::mem::replace(&mut self.fn_ctx, ctx);

        let result = self.with_frame(&decl.params, &decl.body, |c| {
            c.expr(&decl.body)?.route(c, &decl.body, Route::Body)?;
            Ok(())
        });

        self.fn_ctx = saved;
        result?;
        // An unmarked function is whatever its returns make it, so the disagreement is only
        // visible now. One that hands back nothing on every path is simply void.
        let hands_back_nothing = self.out.void_returns.contains(&callable);
        let hands_back_value = self.out.valued_returns.contains(&callable);
        if unmarked && hands_back_nothing && hands_back_value {
            return Err(self.ctx.mixed_return_error(callable, decl));
        }
        // A factory's body returns nothing, but its call hands back the instance it built.
        let hands_back = self.checking_factory
            || (!self.ctx.sigs.fn_sig_of(callable).is_some_and(|s| s.ret.void)
                && !self.out.void_returns.contains(&callable));
        if hands_back {
            self.out.value_returns.insert(callable);
        }
        Ok(())
    }

    pub(super) fn method_stmt(&mut self, stmt: &HirId<HirStmt>) -> Result<(), anyhow::Error> {
        if let HirStmt::Fn(decl) = self.ctx.hir.get(stmt) {
            self.function((*stmt).into(), decl)?;
        }
        Ok(())
    }

    fn lambda_value(&mut self, decl: &HirFnDecl, node: &HirId<HirExpr>) -> Result<ValueState, anyhow::Error> {
        self.out.fresh_values.insert(*node);
        let (_, mut effects) = self.effects_of(|c| c.function((*node).into(), decl))?;
        let uses = effects.replace_uses_with(node.index());
        let state = CallableState { unreached: None, frame: self.current_frame_id, uses };
        self.callables.insert(node.index(), state);
        self.add_effects(effects);
        Ok(ValueState::nonnull().possibly_callable(Some(node.index())))
    }

    pub(super) fn receiver_reassignable(&self, decl: &HirFnDecl) -> bool {
        match &decl.receiver {
            Some(receiver) => receiver.reassignable,
            None => self.fn_ctx.receiver_reassignable,
        }
    }

    /// What `this` owes in `decl`: its spelled clause, and what its type witnesses.
    pub(super) fn receiver_facts(&self, decl: &HirFnDecl) -> Obligations {
        match &decl.receiver {
            Some(_) if self.checking_factory => Obligations::default(),
            Some(receiver) => {
                let mut owed = receiver.clause.owed();
                owed.extend(self.current_type_witnesses.iter().copied());
                owed
            },
            None => self.fn_ctx.receiver.clone(),
        }
    }

    fn owed_description(&self, owed: &Obligations) -> String {
        match owed.is_empty() {
            true => "nothing".to_string(),
            false => quoted_obligation_list(self.ctx.hir, owed),
        }
    }

    fn check_argument_anchor_variance(&self, arg: &HirId<HirExpr>, takes_anchor: bool) -> Result<(), anyhow::Error> {
        match (takes_anchor, matches!(self.ctx.hir.get(arg), HirExpr::Anchor(_))) {
            (true, false) => Err(self.error_help("This parameter takes an anchor".to_string(), arg,
                "the parameter is declared `&var`, so pass `&` and the name")),
            (false, true) => Err(self.error_help("This parameter does not take an anchor".to_string(), arg,
                "declare the parameter `&var` to let it write the caller's value")),
            _ => Ok(()),
        }
    }

    fn check_anchor_root(&self, path: &HirId<HirExpr>, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let (root, _) = access_path_steps(self.ctx.hir, path);
        match self.ctx.bindings.place_of(&root) {
            Some(Place::Local(_)) => Ok(()),
            Some(Place::Capture(_)) => {
                let what = match self.ctx.hir.get(&root) {
                    HirExpr::Identifier(name) => format!("`{}`", self.ctx.hir.text(*name)),
                    _ => "`this`".to_string(),
                };
                Err(self.error_help(format!("Cannot take an anchor into {what}; it is captured"), node,
                    format!("an anchor names a binding of this function, and {what} is a copy of one")))
            },
            _ => Err(self.error_help("An anchor names a binding or a path into one".to_string(), node,
                "`&` takes a name, a field of one, or an element of one")),
        }
    }

    /// Refuses an anchor to a slot that owes an obligation without a witness. An anchor can owe
    /// only obligations with a witness.
    fn refuse_anchor_owing_unwitnessed(&self, path: &HirId<HirExpr>, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let Some((name, owed)) = self.anchored_clause(path) else { return Ok(()) };
        let Some(obligation) = self.ctx.sigs.first_unwitnessed(owed) else { return Ok(()) };
        Err(self.error_help(format!("Cannot anchor `{name}`; it owes '{}', which has no witness", self.ctx.hir.text(obligation)), node,
            ANCHOR_OWES_ONLY_WITNESSED))
    }

    /// The name and clause of the slot `&path` names, when `path` is a binding or `this`.
    fn anchored_clause(&self, path: &HirId<HirExpr>) -> Option<(&'a str, &Obligations)> {
        match self.ctx.hir.get(path) {
            HirExpr::Identifier(root) => {
                let i = self.frame_index_of(*root)?;
                Some((self.ctx.hir.text(*root), self.locals[i].clause_owed()))
            },
            HirExpr::This => Some(("this", &self.fn_ctx.receiver)),
            _ => None,
        }
    }

    fn check_anchor_binding_var(&self, name: Symbol, reassignable: bool, owed: &Obligations, anchor: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let text = self.ctx.hir.text(name);
        if !reassignable {
            return Err(self.error_help(format!("Anchor binding `{text}` must be `var`"), anchor,
                format!("declare it as `say var {text} = ...`")));
        }
        self.check_anchor_obligations_match(owed, anchor, &format!("binding `{text}`"))
    }

    fn check_anchor_obligations_match(&self, wanted: &Obligations, anchor: &HirId<HirExpr>, subject: &str) -> Result<(), anyhow::Error> {
        let HirExpr::Anchor(path) = self.ctx.hir.get(anchor) else { return Ok(()) };
        if !matches!(self.ctx.hir.get(path), HirExpr::Identifier(_) | HirExpr::This) {
            return self.refuse_path_anchor_owing_no_persist(wanted, anchor, subject);
        }
        // An anchor hands over the slot, so the two clauses have to agree, not the values.
        let Some((name, clause)) = self.anchored_clause(path) else { return Ok(()) };
        if clause == wanted {
            return Ok(());
        }
        let declared_as = match name {
            "this" => "&var this".to_string(),
            _ => format!("say var {name}"),
        };
        let clause_text = self.owed_description(clause);
        let wanted_text = self.owed_description(wanted);
        let help = match wanted.is_empty() {
            true => format!("discharge {clause_text} on `{name}` first, or declare it on the {subject}"),
            false => format!("declare it as `{declared_as}: {}`", obligation_atoms(self.ctx.hir, wanted)),
        };
        Err(self.error_help(
            format!("`{name}` owes {clause_text}, but the {subject} it fills owes {wanted_text}"), anchor, help))
    }

    /// A path anchor names storage inside a value, which never admits a value that may not persist,
    /// so it cannot fill a slot that owes one.
    fn refuse_path_anchor_owing_no_persist(&self, wanted: &Obligations, anchor: &HirId<HirExpr>, subject: &str) -> Result<(), anyhow::Error> {
        let blocked = self.ctx.obligations_having_rule(wanted, ObligationRule::NoPersist);
        if blocked.is_empty() {
            return Ok(());
        }
        let owed = quoted_obligation_list(self.ctx.hir, &blocked);
        let help = self.ctx.obligation_rule_prevents_help(&blocked, ObligationRule::NoPersist, Site::Container);
        Err(self.error_help(format!("a path anchor cannot fill a {subject} owing {owed}"), anchor, help))
    }

    fn check_anchor_paths_do_not_overlap(&self, args: &[HirId<HirExpr>]) -> Result<(), anyhow::Error> {
        let pairs = anchors::path_overlaps(self.ctx.hir, self.ctx.bindings, args);
        match pairs.iter().find(|pair| pair.maybe_overlapping_keys.is_empty()) {
            Some(pair) => Err(self.error_help("Two anchors in one call name the same storage".to_string(),
                &pair.second, "pass one anchor, or pass paths that cannot overlap")),
            None => Ok(()),
        }
    }

    fn expr_or_anchor(&mut self, operand: &HirId<HirExpr>) -> Result<UnroutedValueState, anyhow::Error> {
        match self.ctx.hir.get(operand) {
            HirExpr::Anchor(path) => {
                let path = *path;
                self.check_anchor_path(&path, operand)?;
                self.expr(&path)
            },
            _ => self.expr(operand),
        }
    }

    fn check_anchor_path(&mut self, path: &HirId<HirExpr>, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        self.check_anchor_root(path, node)?;
        self.refuse_anchor_owing_unwitnessed(path, node)?;
        self.check_writable(WriteTarget::Anchor(*path), node)
    }

    fn walk_call(&mut self, call: &HirId<HirExpr>, callee: &HirId<HirExpr>, args: &[HirId<HirExpr>], bad: BadCallee) -> Result<ValueState, anyhow::Error> {
        let kind = self.classify_callee(callee);

        if matches!(self.ctx.hir.get(callee), HirExpr::Identifier(_)) && !matches!(kind, Callee::Value) {
            self.expr(callee)?.route(self, callee, Route::Callee)?;
        }

        self.check_anchor_paths_do_not_overlap(&self.ctx.hir.call_anchors(callee, args))?;
        let walk_args = |c: &mut Self| args.iter()
            .map(|a| c.expr_or_anchor(a)?.route(c, a, Route::Argument))
            .collect::<Result<Vec<ValueState>, _>>();

        // A failed safe access skips the arguments, so what they assign may not have happened.
        let arg_types = match matches!(bad, BadCallee::ShortCircuit) || self.ctx.hir.path_has_safe_access(callee) {
            true => self.narrow_branch(&[], walk_args)?,
            false => walk_args(self)?,
        };

        let yielded = self.apply_callee(kind, bad, callee, args, &arg_types)?;
        self.invalidate_anchor_arguments(call, args);
        Ok(yielded)
    }

    /// `cb(args)`.
    pub(super) fn call(&mut self, call: &HirId<HirExpr>, callee: &HirId<HirExpr>, args: &[HirId<HirExpr>]) -> Result<ValueState, anyhow::Error> {
        self.walk_call(call, callee, args, BadCallee::Refuse)
    }

    /// `cb?(args)`.
    pub(super) fn safe_call(&mut self, call: &HirId<HirExpr>, callee: &HirId<HirExpr>, args: &[HirId<HirExpr>]) -> Result<ValueState, anyhow::Error> {
        self.walk_call(call, callee, args, BadCallee::ShortCircuit)
    }

    fn classify_callee(&mut self, callee: &HirId<HirExpr>) -> Callee {
        match self.ctx.hir.get(callee) {
            HirExpr::Identifier(name) => {
                if let Some(decl) = self.ctx.resolved().type_named(callee) {
                    return Callee::Construct(decl);
                }
                if let Some(callable) = self.callable_of(*name) {
                    return Callee::Callable(callable);
                }
                match self.frame_index_of(*name).is_none().then(|| native::builtin(self.ctx.hir.text(*name))).flatten() {
                    Some(sig) => Callee::Builtin(sig),
                    None => Callee::Value,
                }
            },
            HirExpr::Index { base, member, safe, .. } => Callee::Method { receiver: *base, member: *member, safe_receiver: *safe },
            _ => Callee::Value,
        }
    }

    /// Checks the arguments against what the callee accepts, and answers with what the call yields.
    fn apply_callee(&mut self, kind: Callee, bad: BadCallee, callee: &HirId<HirExpr>, args: &[HirId<HirExpr>], arg_types: &[ValueState]) -> Result<ValueState, anyhow::Error> {
        match kind {
            Callee::Construct(decl) => {
                // A factory-less type is built only by brace, so a paren call has nothing to run.
                if !self.ctx.type_has_factory(&decl) {
                    let t = self.ctx.hir.pos(callee).snippet().to_string();
                    return Err(self.error_help(
                        format!("cannot construct '{t}' with '{t}(..)': '{t}' has no factory"),
                        callee,
                        format!("build it with a brace like '{t}{{ .. }}', or give every field a default or add an 'init'")));
                }
                if let Some(init) = self.ctx.constructor_init(callee) {
                    self.check_call_args(callee, init.into(), arg_types, args)?;
                }
                Ok(ValueState::of(self.ctx.construction_debt(&decl), TypeTag::Concrete(decl)))
            },
            Callee::Callable(callable) => {
                self.check_call_args(callee, callable, arg_types, args)?;
                Ok(self.ctx.call_result(callable, &TypeTag::Unknown))
            },
            Callee::Builtin(sig) => {
                self.check_native_args(callee, &sig, arg_types, args)?;
                Ok(ValueState::of(self.ctx.native_ret_debt(sig.ret), TypeTag::Unknown))
            },
            Callee::Method { receiver, member, safe_receiver } => self.method_call(callee, &receiver, &member, safe_receiver, bad, arg_types, args),
            Callee::Value => self.indirect_call(callee, bad),
        }
    }

    /// `a.m(args)`. `safe_receiver` is the `?` in `a?.m(args)`, `bad` is the `?` in `a.m?(args)`.
    pub(super) fn method_call(&mut self, callee: &HirId<HirExpr>, receiver: &HirId<HirExpr>, member: &HirId<HirExpr>, safe_receiver: bool, bad: BadCallee, arg_types: &[ValueState], args: &[HirId<HirExpr>]) -> Result<ValueState, anyhow::Error> {
        let Some(name) = self.ctx.member_display_name(member) else { return self.indirect_call(callee, bad) };
        // `&x.bump()` passes `x` as an anchor.
        let receiver = &match self.ctx.hir.get(receiver) {
            HirExpr::Anchor(path) => *path,
            _ => *receiver,
        };
        if !safe_receiver && !self.ctx.hir.path_has_safe_access(receiver) {
            let base = self.discharged_or_narrowed_base(receiver)?.route(self, receiver, Route::Receiver)?;
            return self.method_call_on(name, base, callee, receiver, member, arg_types, args);
        }
        let base = self.expr(receiver)?.route(self, receiver, Route::Receiver)?;
        let yielded = self.method_call_on(name, base.as_clean(), callee, receiver, member, arg_types, args)?;
        Ok(self.chained_result(&base.debt, &yielded.debt))
    }

    fn method_call_on(&mut self, name: &'a str, receiver_typed: ValueState, callee: &HirId<HirExpr>, receiver: &HirId<HirExpr>, member: &HirId<HirExpr>, arg_types: &[ValueState], args: &[HirId<HirExpr>]) -> Result<ValueState, anyhow::Error> {
        if matches!(receiver_typed.tag, TypeTag::SelfType) {
            return self.trait_member(name, member);
        }

        // A bracket into a dict names an element, so this calls a value the dict contains.
        let bracket = matches!(self.ctx.hir.get(callee), HirExpr::Index { is_dot: false, .. });
        if bracket && receiver_typed.tag.keys_can_shadow_members() {
            return Ok(ValueState::unknown());
        }

        let passes_anchor = self.ctx.hir.passes_anchor_receiver(callee);

        if !matches!(self.ctx.hir.get(receiver), HirExpr::This) {
            self.require_member_exists(&receiver_typed, receiver, name)?;
        }

        let method = self.ctx.string_member(member);
        if let (TypeTag::Concrete(decl), Some(m)) = (&receiver_typed.tag, method) {
            if let Some(stmt) = self.ctx.sigs.method_of(*decl, m) {
                let callable: CallableId = stmt.into();
                self.check_receiver_spelling(self.ctx.sigs.wants_anchor_receiver(callable), passes_anchor, receiver, callee, name)?;
                self.check_call_args(callee, callable, arg_types, args)?;
                return Ok(self.ctx.call_result(callable, &receiver_typed.tag));
            }
            // A field holds a value, and a function value can't take `&var this`.
            if passes_anchor && self.ctx.layout_of(decl).is_some_and(|layout| layout.is_field(m)) {
                self.check_receiver_spelling(false, passes_anchor, receiver, callee, name)?;
            }
        }

        let sig = match &receiver_typed.tag {
            TypeTag::Native(ty) => native::native_method(*ty, name),
            _ => None,
        };

        if let Some(sig) = sig {
            self.check_receiver_spelling(sig.wants_anchor_receiver, passes_anchor, receiver, callee, name)?;
            self.check_native_args(callee, &sig, arg_types, args)?;
            if sig.container == Container::Preserves {
                for (state, arg) in arg_types.iter().zip(args) {
                    self.store_into_container(&state.debt, arg)?;
                }
                self.transfer_obligations_into_receiver(receiver, arg_types);
            }
            return Ok(ValueState::of(self.ctx.native_ret_debt(sig.ret), TypeTag::Unknown));
        }

        if passes_anchor {
            self.pass_anchor_receiver(receiver, callee)?;
        }

        Ok(ValueState::unknown())
    }

    fn check_receiver_spelling(&mut self, wants_anchor: bool, passes_anchor: bool, receiver: &HirId<HirExpr>, callee: &HirId<HirExpr>, method: &str) -> Result<(), anyhow::Error> {
        match (wants_anchor, passes_anchor) {
            (true, true) => self.pass_anchor_receiver(receiver, callee),
            (true, false) => {
                let (root, _) = access_path_steps(self.ctx.hir, receiver);
                let help = match self.ctx.bindings.place_of(&root) {
                    Some(_) => "write `&` before the receiver".to_string(),
                    None => "an anchor names a binding or a path into one, and this receiver is a value".to_string(),
                };
                Err(self.error_help(format!("`{method}` wants an anchor receiver"), callee, help))
            },
            (false, true) => Err(self.error_help(format!("`{method}` does not want an anchor receiver"), callee,
                "call it without `&`")),
            (false, false) => Ok(()),
        }
    }

    fn pass_anchor_receiver(&mut self, receiver: &HirId<HirExpr>, callee: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        self.check_anchor_path(receiver, callee)?;
        self.invalidate_anchor_receiver_fields(receiver);
        Ok(())
    }

    pub(super) fn check_call_args(&mut self, callee: &HirId<HirExpr>, callable: CallableId, arg_types: &[ValueState], args: &[HirId<HirExpr>]) -> Result<(), anyhow::Error> {
        self.resolved_callees.insert(*callee, callable);
        // Read the params through the shared signatures borrow so the later check can take &mut self.
        let sigs = self.ctx.sigs;
        let Some(sig) = sigs.fn_sig_of(callable) else { return Ok(()) };
        let admits: Vec<Obligations> = sig.param_clauses.clone();
        self.check_arg_obligations(callee, &admits, arg_types, args)?;
        self.check_args(callee, &admits, arg_types, args)?;

        let every_anchor_matched = sig.param_anchors.len() == args.len();

        for (position, arg) in args.iter().enumerate() {
            let takes_anchor = sig.param_anchors.get(position).copied().unwrap_or(false);
            self.check_argument_anchor_variance(arg, takes_anchor)?;
            let is_anchor = matches!(self.ctx.hir.get(arg), HirExpr::Anchor(_));

            if let Some(wanted) = sig.param_clauses.get(position).filter(|_| is_anchor) {
                self.check_anchor_obligations_match(wanted, arg, "parameter")?;
            }
        }

        let every_debt_known = arg_types.iter().all(|state| !matches!(state.debt, Debt::Unknown));

        if every_anchor_matched {
            self.out.proven_arg_kinds.insert(*callee);
            if every_debt_known {
                self.out.settled_args.insert(*callee);
            }
        }

        Ok(())
    }

    /// A call through a value whose declaration is not visible here.
    pub(super) fn indirect_call(&mut self, callee: &HirId<HirExpr>, bad: BadCallee) -> Result<ValueState, anyhow::Error> {
        let callee_typed = self.expr(callee)?.route(self, callee, Route::Callee)?;
        match bad {
            BadCallee::Refuse => {
                if let Some(witness) = self.ctx.obligation_witness_name(&callee_typed) {
                    let subject = self.ctx.arg_display_name(callee);
                    return Err(self.ctx.witness_use_error(format!("{subject} is not callable"), callee, witness));
                }
                self.ctx.require_discharged_or_narrowed(&callee_typed.debt, &callee_typed.tag, callee, OperandKind::Whole)?;
                Ok(ValueState::unknown())
            },
            BadCallee::ShortCircuit => {
                self.ctx.require_witnessed_operand(&callee_typed.debt, callee)?;
                Ok(self.chained_result(&callee_typed.debt, &Debt::Unknown))
            },
        }
    }

    

    fn used_before_read(&self, expr: &HirId<HirExpr>) -> bool {
        let HirExpr::Identifier(name) = self.ctx.hir.get(expr) else { return false };
        self.frame_index_of(*name).is_some_and(|i| self.locals[i].used)
    }

    pub(super) fn assign(&mut self, lhs: &HirId<HirExpr>, rhs: &HirId<HirExpr>, safe_access: bool) -> Result<(ValueState, Option<FlowSnapshot>), anyhow::Error> {
        let strategy = write_order::assign_strategy(self.ctx.hir, self.ctx.bindings, self.fn_ctx.wants_anchor_receiver, *lhs, *rhs);
        self.out.assign_strategies.insert(*lhs, strategy);
        if self.checking_factory {
            if let Some(field) = self.never_initialized_factory_field(lhs, rhs) {
                return Err(self.error_help(format!("Non-null field '{}' is never initialized", self.ctx.hir.text(field)), lhs,
                    "assign it in the factory, or give the field a default"));
            }
        }

        let used = self.used_before_read(rhs);
        let place = self.ctx.hir.write_place_of(lhs);
        let value = self.expr(rhs)?;

        let skipped_in_walk = match safe_access {
            true => {
                let clean = self.safe_access_clean_facts(lhs);
                Some(self.narrow_after_snapshot(&clean))
            },
            false => None,
        };

        let routed_value = match place {
            WritePlace::Name(name) => {
                if let Some(i) = self.frame_index_of(name) {
                    self.check_writable(WriteTarget::Local(i), lhs)?;
                    let admits = self.locals[i].clause_owed().clone();
                    self.check_into_slot(value.debt(), &admits, name, lhs)?;
                    self.locals[i].assigned = true;
                    self.locals[i].resolved_callable = self.callable_named_by(rhs);
                    self.locals[i].used = used;

                    let routed_value = match self.locals[i].is_anchor {
                        true => value.route(self, rhs, Route::WrittenThroughAnchor)?,
                        false => value.route(self, lhs, Route::Reassigned(i))?,
                    };

                    match &routed_value.debt {
                        Debt::Unknown => {
                            self.reset_owed(i);
                            self.locals[i].possible = routed_value.possible.clone();
                        },
                        _ => self.rebind(i, &routed_value),
                    }
                    self.note_local_write(i);
                    routed_value
                } else if self.ctx.sigs.is_type(name) {
                    // A type binding names a declaration, not a reassignable slot.
                    return Err(self.error(format!("Cannot reassign `{}`; it names a type", self.ctx.hir.text(name)), lhs));
                } else if matches!(self.ctx.bindings.place_of(lhs), Some(Place::Capture(_))) {
                    self.refuse_anchor_capture(name, lhs)?;
                    return Err(self.error_help(
                        format!("Cannot assign to `{}`; a closure captures its value, not its binding", self.ctx.hir.text(name)), lhs,
                        "hold a `Ref` in the enclosing binding and write it with `@`"));
                } else {
                    unreachable!("a write to a name that binds nothing is refused in bind")
                }
            },
            WritePlace::Ref(holder) => {
                self.discharged_or_narrowed_base(&holder)?.route(self, &holder, Route::RefHolder)?;
                self.check_into_ref(value.debt(), rhs)?;
                value.route(self, rhs, Route::StoredInRef)?
            },
            WritePlace::Path { base, member, is_dot, .. } => {
                self.assign_index(&base, &member, is_dot, value.debt(), lhs, rhs)?;
                match self.write_root(lhs) {
                    WriteRoot::Local(i) => {
                        let state = value.route(self, lhs, Route::StoredInside(i))?;
                        let (_, steps) = access_path_steps(self.ctx.hir, lhs);
                        let path = steps.iter().map(|step| possible_step(self.ctx.member_step(&step.key))).collect();
                        self.write_possible(i, path, &state.possible);
                        self.note_local_write(i);
                        state
                    },
                    root => {
                        let state = value.route(self, rhs, root.untracked_store_route())?;
                        if let Some(written) = root.narrow_root() {
                            self.note_write_to(written);
                        }
                        state
                    },
                }
            },
            WritePlace::Receiver => {
                self.check_writable(WriteTarget::Receiver, lhs)?;
                self.check_into_this(value.debt(), lhs)?;
                value.route(self, rhs, Route::ThisReassigned)?
            },
            WritePlace::Wrapped(_) | WritePlace::Value => value.route(self, rhs, Route::AssignmentResult)?,
        };

        Ok((routed_value, skipped_in_walk))
    }

    fn safe_access_write(&mut self, lhs: &HirId<HirExpr>, write: impl FnOnce(&mut Self, bool) -> Result<(ValueState, Option<FlowSnapshot>), anyhow::Error>) -> Result<ValueState, anyhow::Error> {
        let Some(&checked) = self.ctx.hir.safe_access_checked_steps(lhs).first() else {
            return Ok(write(self, false)?.0);
        };

        let tested = self.expr(&checked)?.route(self, &checked, Route::Condition)?;
        let clean = self.safe_access_clean_facts(lhs);
        let skipped = self.narrow_after_snapshot(&clean);

        let (written, skipped_in_walk) = write(self, true)?;

        if let Some(skipped_in_walk) = skipped_in_walk {
            self.merge_flow_into_current(&skipped_in_walk);
        }

        self.merge_flow_into_current(&skipped);

        Ok(match tested.debt {
            Debt::Clean => written,
            _ => self.chained_result(&tested.debt, &written.debt),
        })
    }

    fn check_writable(&mut self, target: WriteTarget, lhs: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let rebind = matches!(target, WriteTarget::Local(_));
        let anchored = matches!(target, WriteTarget::Anchor(_));
        let (root, path) = match target {
            WriteTarget::Local(i) => (WriteRoot::Local(i), None),
            // A nested body's `this` is a copy it captured, which a write cannot reach through.
            WriteTarget::Receiver => match self.write_root(lhs) {
                captured @ WriteRoot::Capture { .. } => (captured, None),
                _ => (WriteRoot::Receiver, None),
            },
            WriteTarget::Path(node) | WriteTarget::Anchor(node) => (self.write_root(&node), Some(node)),
        };
        let (doing, then) = match (rebind, anchored) {
            (true, _) => ("reassign", "change"),
            (false, true) => ("anchor", "anchor"),
            (false, false) => ("write through", "write"),
        };

        let binding_to_reassign = match root {
            WriteRoot::Local(i) | WriteRoot::Anchor(i) if !rebind || self.locals[i].assigned => Some(i),
            WriteRoot::Capture { local: Some(i), .. } if self.locals[i].fn_decl => Some(i),
            // A write names its root binding, and a closure only holds a copy of a captured one. The
            // write could never reach the binding it names, so the language refuses it.
            WriteRoot::Capture { node, .. } => {
                let name = self.ctx.hir.pos(&node).snippet();
                return Err(self.error_help(format!("Cannot write through `{name}`; a closure captures its value, not its binding"), lhs,
                    "hold a `Ref` in the enclosing binding and write it with `@`"));
            },
            // Both `var this` and `&var this` let a method anchor its receiver.
            WriteRoot::Receiver if !self.fn_ctx.receiver_reassignable => {
                return Err(match anchored {
                    true => self.error_help("Cannot anchor non-var binding `this`".to_string(), lhs,
                        "declare the receiver as `var this` or `&var this`"),
                    false => self.error_help("Cannot write through `this`".to_string(), lhs,
                        "declare the receiver with `var`, as `fn name(var this)`"),
                });
            },
            _ => None,
        };
        if let Some(i) = binding_to_reassign {
            let name = self.ctx.hir.text(self.locals[i].name);
            if self.locals[i].fn_decl {
                return Err(self.error(format!("Cannot {doing} `{name}`; it names a function"), lhs));
            }
            if !self.locals[i].reassignable {
                let local = &self.locals[i];
                let kind = if local.pattern_binder_source.is_some() { "pattern binding" } else { "binding" };
                let message = match (rebind, anchored, local.pattern_binder_source.is_some()) {
                    (true, _, _) => format!("Cannot reassign {kind} `{name}`"),
                    (false, true, _) => format!("Cannot anchor non-var {kind} `{name}`"),
                    (false, false, true) => format!("Cannot write through pattern binding `{name}`"),
                    (false, false, false) => format!("Cannot write through `{name}`"),
                };
                // A pattern takes `var` where it is declared, and a match or catch binding takes none.
                let help = match (local.pattern_binder_source, local.is_catch, local.is_param) {
                    (Some(PatternBinderSource::Param), _, _) => "declare the parameter with `var`, which reaches every name its pattern binds".to_string(),
                    (Some(PatternBinderSource::Say), _, _) => "declare it with `say var`, which reaches every name the pattern binds".to_string(),
                    (Some(_), _, _) | (None, true, _) => format!("copy it into a `say var {name}` first to {then} it"),
                    (None, false, true) if rebind => format!("you can make `{name}` reassignable by declaring the parameter as `var {name}`"),
                    (None, false, true) if anchored => format!("declare the parameter as `var {name}` or `&var {name}`"),
                    (None, false, false) if rebind => format!("you can make `{name}` reassignable by declaring it as `say var {name}`"),
                    (None, false, _) => format!("declare `{name}` with `var`"),
                };
                return Err(self.error_help(message, lhs, help));
            }
        }

        match path {
            Some(node) => self.check_path_fields_reassignable(&node, lhs, anchored),
            None => Ok(()),
        }
    }

    fn check_path_fields_reassignable(&mut self, target: &HirId<HirExpr>, lhs: &HirId<HirExpr>, anchored: bool) -> Result<(), anyhow::Error> {
        let (base, member) = match self.ctx.hir.write_place_of(target) {
            WritePlace::Path { base, member, .. } => (base, member),
            WritePlace::Wrapped(inner) => return self.check_path_fields_reassignable(&inner, lhs, anchored),
            _ => return Ok(()),
        };
        self.check_path_fields_reassignable(&base, lhs, anchored)?;
        let Some(field) = self.ctx.string_member(&member) else { return Ok(()) };
        let tag = match self.ctx.hir.get(&base) {
            HirExpr::This => self.this_tag(),
            _ => self.expr(&base)?.route(self, &base, Route::WriteTarget)?.tag,
        };
        let TypeTag::Concrete(decl) = tag else { return Ok(()) };
        let Some(layout) = self.ctx.layout_of(&decl) else { return Ok(()) };
        if matches!(layout.members.get(&field), Some(TypeMember::Field(_))) && !layout.is_reassignable(field) {
            self.ctx.refuse_non_var_field(&decl, field, lhs, anchored)?;
        }
        Ok(())
    }

    pub(super) fn assign_index(&mut self, target: &HirId<HirExpr>, member: &HirId<HirExpr>, is_dot: bool, value: &Debt, lhs: &HirId<HirExpr>, rhs: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let named = self.ctx.string_member(member);
        if matches!(self.ctx.hir.get(target), HirExpr::This) {
            if let Some(field) = named {
                self.rebind_field(target, field, value);
                self.assign_field_this(field, value, lhs, rhs)?;
            }
            return Ok(());
        }

        if matches!(self.write_root(target), WriteRoot::Value) {
            return Err(self.error_help("Cannot assign to an expression result".to_string(), lhs,
                "store the result in a `say var` binding first, and assign to that"));
        }

        self.check_writable(WriteTarget::Path(*target), lhs)?;
        self.invalidate_proven_at_write(target, member);
        if let Some(field) = named {
            self.rebind_field(target, field, value);
        }
        let base = self.discharged_or_narrowed_base(target)?.route(self, target, Route::WriteTarget)?;

        if !is_dot {
            self.expr(member)?.route(self, member, Route::MemberKey)?;
        }

        if let (Some(field), TypeTag::Concrete(decl)) = (named, &base.tag) {
            return self.assign_field_external(&decl.clone(), field, value, lhs, rhs);
        }

        let site = if is_dot { Site::Field } else { Site::Slot };
        self.ctx.obligation_rule_reject_at(value, ObligationRule::NoPersist, site, rhs)
    }

    fn assign_field_this(&mut self, field: Symbol, debt: &Debt, lhs: &HirId<HirExpr>, rhs: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        if let Some(trait_stmt) = self.current_trait {
            return self.assign_req_member(trait_stmt, field, lhs);
        }
        let Some(type_stmt) = self.current_type else { return Ok(()) };
        let (member, admits, reassignable) = match self.ctx.layout_of(&type_stmt) {
            Some(layout) => (layout.members.get(&field).copied(), layout.owed(field), layout.is_reassignable(field)),
            None => return Ok(()),
        };

        match member {
            Some(TypeMember::Field(_)) => {},
            Some(TypeMember::Method(_)) => return Err(self.ctx.method_assign_error(field, lhs)),
            None => return Ok(()),
        }

        self.check_writable(WriteTarget::Receiver, lhs)?;

        if !reassignable && !self.checking_factory {
            self.ctx.refuse_non_var_field(&type_stmt, field, lhs, false)?;
        }

        let this = self.this_valuestate();
        self.ctx.require_discharged_or_narrowed(&this.debt, &this.tag, lhs, OperandKind::Whole)?;

        self.check_into_field(debt, &admits, field, rhs)
    }

    fn assign_req_member(&mut self, trait_stmt: HirId<HirStmt>, field: Symbol, lhs: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        self.check_writable(WriteTarget::Receiver, lhs)?;
        let HirStmt::Trait(decl) = self.ctx.hir.get(&trait_stmt) else { return Ok(()) };
        let Some(req) = decl.req_members.iter().find(|r| r.name == field) else { return Ok(()) };
        if req.reassignable {
            return Ok(());
        }
        let (owner, name) = (self.ctx.hir.text(req.trait_name), self.ctx.hir.text(field));
        Err(self.error_help(format!("Cannot reassign required member `{owner}.{name}`"), lhs,
            format!("a trait that writes a member must require it to be `var`, so declare it as `req var {name};`")))
    }

    /// Checks an external write `obj.field = value` on a known type.
    pub(super) fn assign_field_external(&mut self, type_stmt: &HirId<HirStmt>, field: Symbol, debt: &Debt, lhs: &HirId<HirExpr>, rhs: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let field_info = match self.ctx.layout_of(type_stmt) {
            Some(layout) => match layout.members.get(&field) {
                Some(TypeMember::Field(_)) => Some((layout.is_public(field), layout.owed(field), layout.is_reassignable(field))),
                Some(TypeMember::Method(_)) => return Err(self.ctx.method_assign_error(field, lhs)),
                None => None,
            },
            None => None,
        };
        let Some((public, admits, reassignable)) = field_info else { return Ok(()) };
        if !public {
            return Ok(());
        }
        if !reassignable {
            self.ctx.refuse_non_var_field(type_stmt, field, lhs, false)?;
        }
        self.check_into_field(debt, &admits, field, rhs)
    }

}

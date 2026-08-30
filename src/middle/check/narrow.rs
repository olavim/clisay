//! What a condition or a match arm proves, so the branch it guards may assume it.

use std::collections::HashMap;

use crate::core::objects::TypeMember;
use crate::middle::hir::{BinOp, Hir, HirExpr, HirId, HirLiteral, HirMatchArm, HirMatcher, HirStmt, HirTypeDecl, Symbol, UnOp};
use crate::middle::native;
use crate::middle::obligations::Obligations;
use crate::middle::signatures::Witness;

use super::scope::FlowSnapshot;
use super::{Checker, Ctx, Local, NarrowFact, NarrowTarget, TypeTag};

/// What the compiler can tell about a condition's truth without running it.
#[derive(Clone, Copy, PartialEq)]
enum Truthiness {
    Truthy,
    Falsy,
    Unknown,
}

impl<'a> Ctx<'a> {
    pub(super) fn field_owes(&self, decl: &HirId<HirStmt>, field: Symbol) -> Obligations {
        match self.layout_of(decl) {
            Some(layout) => layout.owed(field, self.sigs.opt),
            None => Obligations::new(),
        }
    }

    /// Whether no value the matcher accepts can be in the obligation's bad state.
    pub(super) fn matcher_disjoint_from_obligation(&self, matcher: &HirId<HirMatcher>, obligation: Symbol) -> bool {
        match self.hir.get(matcher) {
            HirMatcher::As(_, inner) => self.matcher_disjoint_from_obligation(inner, obligation),
            HirMatcher::Or(alternatives) => alternatives.iter().all(|m| self.matcher_disjoint_from_obligation(m, obligation)),
            HirMatcher::And(parts) => parts.iter().any(|m| self.matcher_disjoint_from_obligation(m, obligation)),
            HirMatcher::Literal(_) | HirMatcher::Array(_) => true,
            HirMatcher::Type { nominal: true, .. } => match self.matcher_type_decl(matcher) {
                Some((_, decl)) => !self.hir.is_trait(decl.id)
                    && !self.sigs.obligations_witnessed_by_decl(decl).contains(&obligation),
                None => false,
            },
            HirMatcher::Type { nominal: false, .. } => self.surface_rules_out_obligation(matcher, obligation),
            HirMatcher::Shape(fields) => match self.witness_decl_of_obligation(obligation) {
                Some(witness) => fields.iter().any(|field| self.type_lacks_public_member(&witness, &field.key)),
                None => false,
            },
            HirMatcher::Binder(_) | HirMatcher::Wildcard => false,
        }
    }

    /// The declaration node and declaration a type test names.
    fn matcher_type_decl(&self, matcher: &HirId<HirMatcher>) -> Option<(HirId<HirStmt>, &'a HirTypeDecl)> {
        let stmt = self.bindings.type_ref(matcher)?;
        match self.hir.get(&stmt) {
            HirStmt::Type(decl) | HirStmt::Trait(decl) => Some((stmt, decl)),
            _ => None,
        }
    }

    fn witness_decl_of_obligation(&self, obligation: Symbol) -> Option<HirId<HirStmt>> {
        let Some(Witness::Type(id)) = self.sigs.witness_of(obligation) else { return None };
        self.sigs.type_decl_of_id(*id)
    }

    fn surface_rules_out_obligation(&self, matcher: &HirId<HirMatcher>, obligation: Symbol) -> bool {
        let Some(witness) = self.witness_decl_of_obligation(obligation) else { return false };
        let Some(tested) = self.bindings.type_ref(matcher) else { return false };
        let Some(members) = self.bindings.surface(&tested) else { return false };
        members.iter().any(|member| self.type_lacks_public_member_named(&witness, *member))
    }

    fn type_lacks_public_member(&self, decl: &HirId<HirStmt>, key: &HirLiteral) -> bool {
        let HirLiteral::String(field) = key else { return false };
        match self.hir.symbol_of(field) {
            None => true,
            Some(field) => self.type_lacks_public_member_named(decl, field),
        }
    }

    fn type_lacks_public_member_named(&self, decl: &HirId<HirStmt>, field: Symbol) -> bool {
        match self.bindings.layout_of_decl(decl) {
            Some(layout) => !layout.members.contains_key(&field) || !layout.is_public(field),
            None => false,
        }
    }

    pub(super) fn test_proves_declared_member(&self, test: &HirId<HirMatcher>, member: Symbol) -> bool {
        let HirMatcher::Type { nominal, .. } = self.hir.get(test) else { return false };
        let Some(layout) = self.bindings.type_ref(test).and_then(|decl| self.layout_of(&decl)) else { return false };
        *nominal || layout.is_field(member)
    }

    pub(super) fn obligations_ruled_out_by_matcher(&self, matcher: &HirId<HirMatcher>, remaining: &Obligations) -> Obligations {
        remaining.iter().copied().filter(|o| self.matcher_total_over_obligation(matcher, *o)).collect()
    }

    pub(super) fn matcher_total_over_obligation(&self, matcher: &HirId<HirMatcher>, obligation: Symbol) -> bool {
        self.sigs.witness_of(obligation).is_some_and(|w| self.matcher_total_over_witness(matcher, w))
    }

    pub(super) fn matcher_total_over_witness(&self, matcher: &HirId<HirMatcher>, witness: &Witness) -> bool {
        match self.hir.get(matcher) {
            HirMatcher::As(_, inner) => self.matcher_total_over_witness(inner, witness),
            HirMatcher::Or(alternatives) => alternatives.iter().any(|m| self.matcher_total_over_witness(m, witness)),
            HirMatcher::And(parts) => parts.iter().all(|m| self.matcher_total_over_witness(m, witness)),
            HirMatcher::Literal(HirLiteral::Null) => matches!(witness, Witness::Null),
            HirMatcher::Type { nominal: false, .. } => false,
            HirMatcher::Type { nominal: true, shape, .. } => {
                let Some((stmt, decl)) = self.matcher_type_decl(matcher) else { return false };
                match witness {
                    Witness::Type(id) => decl.id == *id
                        && shape.as_ref().is_none_or(|s| self.matcher_total_over_type(s, &stmt)),
                    Witness::Trait(id) => decl.id == *id && shape.is_none(),
                    Witness::Null => false,
                }
            },
            _ => false,
        }
    }

    pub(super) fn matcher_total_over_type(&self, matcher: &HirId<HirMatcher>, decl: &HirId<HirStmt>) -> bool {
        let HirMatcher::Shape(fields) = self.hir.get(matcher) else { return false };
        fields.iter().all(|field| {
            self.is_public_field(decl, &field.key)
                && matches!(self.hir.get(&field.value), HirMatcher::Binder(_) | HirMatcher::Wildcard)
        })
    }

    pub(super) fn is_public_field(&self, decl: &HirId<HirStmt>, key: &HirLiteral) -> bool {
        let HirLiteral::String(field) = key else { return false };
        match self.bindings.layout_of_decl(decl) {
            Some(layout) => match self.hir.symbol_of(field) {
                Some(field) => matches!(layout.members.get(&field), Some(TypeMember::Field(_))) && layout.is_public(field),
                None => false,
            },
            None => false,
        }
    }

    fn match_arm_always_runs(&self, arm: &HirMatchArm) -> bool {
        arm.guard.as_ref().is_none_or(|guard| self.is_literal_true(guard))
    }

    pub(super) fn obligations_ruled_out_by_match_arm(&self, arm: &HirMatchArm, remaining: &Obligations) -> Obligations {
        match self.match_arm_always_runs(arm) {
            true => self.obligations_ruled_out_by_matcher(&arm.matcher, remaining),
            false => Obligations::new(),
        }
    }

    pub(super) fn obligations_examined_by_match_arm(&self, arm: &HirMatchArm, remaining: &Obligations) -> Obligations {
        match self.match_arm_always_runs(arm) {
            true => self.obligations_settled_by_matcher(&arm.matcher, remaining),
            false => Obligations::new(),
        }
    }

    fn eval_truthiness(&self, cond: &HirId<HirExpr>) -> Truthiness {
        use Truthiness::{Falsy, Truthy, Unknown};
        match self.hir.get(cond) {
            HirExpr::Literal(HirLiteral::Null | HirLiteral::Boolean(false)) => Falsy,
            HirExpr::Literal(_) => Truthy,
            HirExpr::Unary(UnOp::Not, x) => self.eval_truthiness(x).negate(),
            HirExpr::Construct(..) => Truthy,
            HirExpr::Call(callee, _) if self.names_type(callee) => Truthy,
            HirExpr::Mut(inner) => self.eval_truthiness(inner),
            HirExpr::Assign(_, rhs) => self.eval_truthiness(rhs),
            HirExpr::Binary(BinOp::And, l, r) => match (self.eval_truthiness(l), self.eval_truthiness(r)) {
                (Falsy, _) | (_, Falsy) => Falsy,
                (Truthy, Truthy) => Truthy,
                _ => Unknown,
            },
            HirExpr::Binary(BinOp::Or, l, r) => match (self.eval_truthiness(l), self.eval_truthiness(r)) {
                (Truthy, _) | (_, Truthy) => Truthy,
                (Falsy, Falsy) => Falsy,
                _ => Unknown,
            },
            _ => Unknown,
        }
    }

    pub(super) fn field_is_reassignable(&self, decl: &HirId<HirStmt>, field: Symbol) -> bool {
        self.layout_of(decl).is_some_and(|layout| layout.is_reassignable(field))
    }

    pub(super) fn is_literal_true(&self, guard: &HirId<HirExpr>) -> bool {
        matches!(self.hir.get(guard), HirExpr::Literal(HirLiteral::Boolean(true)))
    }
    pub(super) fn is_null(&self, expr: &HirId<HirExpr>) -> bool {
        matches!(self.hir.get(expr), HirExpr::Literal(HirLiteral::Null))
    }

    fn obligations_asked_by_matcher(&self, matcher: &HirId<HirMatcher>, remaining: &Obligations) -> Obligations {
        match self.hir.get(matcher) {
            HirMatcher::As(_, inner) => self.obligations_asked_by_matcher(inner, remaining),
            // `And` stops at the first part that fails, so only that one is sure to run.
            HirMatcher::And(parts) => parts.first()
                .map_or_else(Obligations::new, |part| self.obligations_asked_by_matcher(part, remaining)),
            // `Or` tries alternatives until one matches, so whichever it stops at has to answer.
            // An alternative answers by asking, or by ruling the witness out if it matches. An
            // alternative that only rules out asked nothing, so at least one has to ask.
            HirMatcher::Or(parts) => remaining.iter().copied()
                .filter(|o| {
                    let answers = |p: &HirId<HirMatcher>| self.obligations_asked_by_matcher(p, remaining).contains(o);
                    parts.iter().any(answers)
                        && parts.iter().all(|p| self.matcher_disjoint_from_obligation(p, *o) || answers(p))
                })
                .collect(),
            HirMatcher::Type { nominal: true, shape, .. } => {
                let Some((stmt, decl)) = self.matcher_type_decl(matcher) else { return Obligations::new() };
                // A shape that can fail on a real witness may never run, so it asks nothing.
                if !shape.as_ref().is_none_or(|s| self.matcher_total_over_type(s, &stmt)) {
                    return Obligations::new();
                }
                let witnessed = self.sigs.obligations_witnessed_by_decl(decl);
                remaining.iter().copied().filter(|o| witnessed.contains(o)).collect()
            },
            _ => Obligations::new(),
        }
    }

    pub(super) fn obligations_settled_by_matcher(&self, matcher: &HirId<HirMatcher>, remaining: &Obligations) -> Obligations {
        let mut out = self.obligations_ruled_out_by_matcher(matcher, remaining);
        out.extend(self.obligations_asked_by_matcher(matcher, remaining));
        out
    }

    fn names_type(&self, callee: &HirId<HirExpr>) -> bool {
        matches!(self.hir.get(callee), HirExpr::Identifier(name) if self.sigs.is_type(*name))
    }

    fn is_truthy(&self, cond: &HirId<HirExpr>) -> bool {
        self.eval_truthiness(cond) == Truthiness::Truthy
    }

    fn is_falsy(&self, cond: &HirId<HirExpr>) -> bool {
        self.eval_truthiness(cond) == Truthiness::Falsy
    }

    /// Whether a call could rebind this binding.
    fn reachable_by_a_rebind(&self, local: &Local) -> bool {
        self.sigs.any_rebind.contains(&local.name)
            && local.decl.is_none_or(|decl| self.bindings.is_captured(decl))
    }

    pub(super) fn collect_matcher_obligations_by_binding(&self, test: &HirId<HirMatcher>, shape: &HirId<HirMatcher>) -> HashMap<Symbol, Obligations> {
        let mut out = HashMap::new();
        let (HirMatcher::Shape(fields), Some(decl)) = (self.hir.get(shape), self.bindings.type_ref(test)) else { return out };
        for field in fields {
            let HirLiteral::String(key) = &field.key else { continue };
            let Some(sym) = self.hir.symbol_of(key) else { continue };
            if !self.test_proves_declared_member(test, sym) {
                continue;
            }
            let owed = self.field_owes(&decl, sym);
            for name in collect_whole_value_binders(self.hir, &field.value) {
                out.entry(name).or_default().extend(owed.iter().copied());
            }
        }
        out
    }

    pub(super) fn has_truthiness(&self, expr: &HirId<HirExpr>, truthy: bool) -> bool {
        match truthy {
            true => self.is_truthy(expr),
            false => self.is_falsy(expr),
        }
    }
}

impl Truthiness {
    fn negate(self) -> Truthiness {
        match self {
            Truthiness::Truthy => Truthiness::Falsy,
            Truthiness::Falsy => Truthiness::Truthy,
            Truthiness::Unknown => Truthiness::Unknown,
        }
    }
}

impl<'a> Checker<'a> {
    /// A reassignment drops the binding's narrowing facts. The slot is non-null again only if
    /// the new value is.
    pub(super) fn reset_narrowing(&mut self, i: usize, now_non_null: bool) {
        self.locals[i].discharged = Obligations::new();
        self.locals[i].field_discharged.clear();
        if now_non_null {
            self.locals[i].discharged.insert(self.ctx.sigs.opt);
        }
    }

    /// Whether `obligation` is discharged for a place on the current path.
    pub(super) fn discharged(&self, target: &NarrowTarget, obligation: Symbol) -> bool {
        let set = match target {
            NarrowTarget::Local(i) => Some(&self.locals[*i].discharged),
            NarrowTarget::ThisField(field) => self.this_narrowed.get(field),
            NarrowTarget::LocalField(i, field) => self.locals[*i].field_discharged.get(field),
        };
        set.is_some_and(|set| set.contains(&obligation))
    }

    /// Records that a place no longer owes an obligation on this path.
    pub(super) fn discharge(&mut self, target: NarrowTarget, obligation: Symbol) {
        match target {
            NarrowTarget::Local(i) => { self.locals[i].discharged.insert(obligation); },
            NarrowTarget::ThisField(field) => { self.this_narrowed.entry(field).or_default().insert(obligation); },
            NarrowTarget::LocalField(i, field) => { self.locals[i].field_discharged.entry(field).or_default().insert(obligation); },
        }
    }

    /// Where `target.field`'s narrowing lands when the place can be narrowed.
    pub(super) fn narrowable_field(&self, target: &HirId<HirExpr>, field: Symbol) -> Option<NarrowTarget> {
        match self.ctx.hir.get(target) {
            HirExpr::This => {
                let decl = self.current_type?;
                (!self.ctx.field_is_reassignable(&decl, field)).then_some(NarrowTarget::ThisField(field))
            },
            // A rebindable binding narrows too. What a rebind can reach is invalidated where the
            // rebind happens, rather than refused here.
            HirExpr::Identifier(name) => {
                let i = self.frame_index_of(*name)?;
                if self.locals[i].fn_decl {
                    return None;
                }
                let TypeTag::Concrete(decl) = &self.locals[i].tag else { return None };
                (!self.ctx.field_is_reassignable(decl, field)).then_some(NarrowTarget::LocalField(i, field))
            },
            _ => None,
        }
    }

    /// The facts a condition establishes. `positive` selects the branch where it holds versus
    /// the branch where it fails.
    pub(super) fn narrowings(&self, cond: &HirId<HirExpr>, positive: bool) -> Vec<NarrowFact> {
        match self.ctx.hir.get(cond) {
            // A bare truthiness test narrows in the truthy branch.
            HirExpr::Identifier(_) | HirExpr::Index(_, _, _) if positive => self.narrow_non_null(cond),
            HirExpr::Match(scrutinee, matcher) if positive => self.narrow_match_positive_branch(scrutinee, matcher),
            // The false branch of `x ~ M` rules out every witness `M` covers.
            HirExpr::Match(scrutinee, matcher) if !positive => self.narrow_match_negative_branch(scrutinee, matcher),
            // `x != null` narrows when true; `x == null` narrows when false.
            HirExpr::Binary(BinOp::NotEqual, l, r) if positive => self.narrow_null_compare(l, r),
            HirExpr::Binary(BinOp::Equal, l, r) if !positive => self.narrow_null_compare(l, r),
            // `x == <clean>` narrows when true; `x != <clean>` narrows when false.
            HirExpr::Binary(BinOp::Equal, l, r) if positive => self.narrow_equal_compare(l, r),
            HirExpr::Binary(BinOp::NotEqual, l, r) if !positive => self.narrow_equal_compare(l, r),
            // Both sides of a conjunction hold, so their facts combine.
            HirExpr::Binary(BinOp::And, l, r) if positive => {
                let mut narrow = self.narrowings(l, true);
                narrow.extend(self.narrowings(r, true));
                narrow
            },
            HirExpr::Binary(BinOp::And, l, r) if !positive => match (self.ctx.is_truthy(l), self.ctx.is_truthy(r)) {
                (true, _) => self.narrowings(r, false),
                (_, true) => self.narrowings(l, false),
                _ => intersect_narrow_facts(self.narrowings(l, false), &self.narrowings(r, false)),
            },
            HirExpr::Binary(BinOp::Or, l, r) if positive => match (self.ctx.is_falsy(l), self.ctx.is_falsy(r)) {
                (true, _) => self.narrowings(r, true),
                (_, true) => self.narrowings(l, true),
                _ => intersect_narrow_facts(self.narrowings(l, true), &self.narrowings(r, true)),
            },
            HirExpr::Binary(BinOp::Or, l, r) if !positive => {
                let mut narrow = self.narrowings(l, false);
                narrow.extend(self.narrowings(r, false));
                narrow
            },
            HirExpr::Unary(UnOp::Not, x) => self.narrowings(x, !positive),
            _ => Vec::new(),
        }
    }

    pub(super) fn narrow_null_compare(&self, l: &HirId<HirExpr>, r: &HirId<HirExpr>) -> Vec<NarrowFact> {
        let place = if self.ctx.is_null(l) { r } else if self.ctx.is_null(r) { l } else { return Vec::new() };
        self.narrow_non_null(place)
    }

    /// What `x == <clean>` proves, where the other side is a literal or a binding owing nothing.
    pub(super) fn narrow_equal_compare(&self, l: &HirId<HirExpr>, r: &HirId<HirExpr>) -> Vec<NarrowFact> {
        let place = match (self.proves_clean(l), self.proves_clean(r)) {
            (true, false) => r,
            (false, true) => l,
            _ => return Vec::new(),
        };
        let Some(target) = self.narrow_target(place) else { return Vec::new() };
        let mut facts = vec![NarrowFact::Discharge(target, self.ctx.sigs.opt)];
        facts.extend(self.object_witnessed(&target).map(|o| NarrowFact::Discharge(target, o)));
        facts
    }

    /// The object-witnessed obligation names a place owes.
    fn object_witnessed(&self, target: &NarrowTarget) -> impl Iterator<Item = Symbol> + '_ {
        self.owed_at(target).into_iter()
            .filter(|o| matches!(self.ctx.sigs.witness_of(*o), Some(Witness::Type(_) | Witness::Trait(_))))
    }

    /// Whether being equal to this operand proves a value is in no witness's bad state.
    fn proves_clean(&self, expr: &HirId<HirExpr>) -> bool {
        match self.ctx.hir.get(expr) {
            HirExpr::Literal(HirLiteral::Number(_) | HirLiteral::String(_) | HirLiteral::Boolean(_)) => true,
            HirExpr::Identifier(name) => self.frame_index_of(*name).is_some_and(|i| {
                let local = &self.locals[i];
                !local.fn_decl && local.owed.difference(&local.discharged).next().is_none()
            }),
            _ => false,
        }
    }

    /// Drops the narrowings of every binding a call may rebind.
    pub(super) fn invalidate_rebound_fields(&mut self, callee: &HirId<HirExpr>) {
        if self.ctx.sigs.any_rebind.is_empty() || self.callee_is_builtin(callee) {
            return;
        }
        let hit: Vec<usize> = self.locals.iter().enumerate()
            .filter(|(_, l)| !l.fn_decl && self.ctx.reachable_by_a_rebind(l))
            .map(|(i, _)| i)
            .collect();
        for i in hit {
            // A rebind replaces the whole value, so what was proven of the slot goes with it. This
            // is the same reset a direct rebind performs.
            self.reset_narrowing(i, false);
            self.rebound_in_expr.insert(i);
        }
    }

    fn callee_is_builtin(&self, callee: &HirId<HirExpr>) -> bool {
        let HirExpr::Identifier(name) = self.ctx.hir.get(callee) else { return false };
        self.frame_index_of(*name).is_none() && native::builtin(self.ctx.hir.text(*name)).is_some()
    }

    /// Walks a condition, then answers what it proves on each outcome.
    pub(super) fn condition_narrowings(&mut self, cond: &HirId<HirExpr>) -> Result<(Vec<NarrowFact>, Vec<NarrowFact>), anyhow::Error> {
        // A condition can hold a lambda whose body has a condition of its own, so the outer walk's
        // set is put back rather than dropped.
        let outer = std::mem::take(&mut self.rebound_in_expr);
        self.expr(cond)?;
        let rebound = std::mem::replace(&mut self.rebound_in_expr, outer);
        self.rebound_in_expr.extend(&rebound);
        let keep = |facts: Vec<NarrowFact>| -> Vec<NarrowFact> {
            facts.into_iter()
                .filter(|f| !matches!(f,
                    NarrowFact::Discharge(NarrowTarget::LocalField(i, _) | NarrowTarget::Local(i), _)
                        if rebound.contains(i)))
                .collect()
        };
        Ok((keep(self.narrowings(cond, true)), keep(self.narrowings(cond, false))))
    }

    pub(super) fn narrow_target(&self, expr: &HirId<HirExpr>) -> Option<NarrowTarget> {
        match self.ctx.hir.get(expr) {
            HirExpr::Identifier(name) => self.frame_index_of(*name)
                .filter(|&i| !self.locals[i].fn_decl)
                .map(NarrowTarget::Local),
            HirExpr::Index(target, member, _) => self.ctx.string_member(member)
                .and_then(|field| self.narrowable_field(target, field)),
            _ => None,
        }
    }

    pub(super) fn narrow_non_null(&self, expr: &HirId<HirExpr>) -> Vec<NarrowFact> {
        self.narrow_target(expr)
            .map(|target| vec![NarrowFact::Discharge(target, self.ctx.sigs.opt)])
            .unwrap_or_default()
    }

    fn owed_at(&self, target: &NarrowTarget) -> Obligations {
        match target {
            NarrowTarget::Local(i) => self.locals[*i].owed.clone(),
            NarrowTarget::ThisField(field) => match self.current_type {
                Some(decl) => self.ctx.field_owes(&decl, *field),
                None => Obligations::new(),
            },
            NarrowTarget::LocalField(i, field) => match &self.locals[*i].tag {
                TypeTag::Concrete(decl) => self.ctx.field_owes(decl, *field),
                _ => Obligations::new(),
            },
        }
    }

    pub(super) fn narrow_match_positive_branch(&self, scrutinee: &HirId<HirExpr>, matcher: &HirId<HirMatcher>) -> Vec<NarrowFact> {
        let mut facts = match self.ctx.hir.get(matcher).rejects_null(self.ctx.hir) {
            true => self.narrow_non_null(scrutinee),
            false => Vec::new(),
        };
        let Some(target) = self.narrow_target(scrutinee) else { return facts };
        // A nominal test confirms which type the value has.
        if let (NarrowTarget::Local(i), HirMatcher::Type { nominal: true, .. }) = (target, self.ctx.hir.get(matcher)) {
            if let Some((stmt, _)) = self.ctx.matcher_type_decl(matcher).filter(|(_, d)| !self.ctx.hir.is_trait(d.id)) {
                facts.push(NarrowFact::Tag(i, TypeTag::Concrete(stmt)));
            }
        }
        facts.extend(self.object_witnessed(&target)
            .filter(|o| self.ctx.matcher_disjoint_from_obligation(matcher, *o))
            .map(|o| NarrowFact::Discharge(target, o)));
        facts
    }

    pub(super) fn narrow_match_negative_branch(&self, scrutinee: &HirId<HirExpr>, matcher: &HirId<HirMatcher>) -> Vec<NarrowFact> {
        let Some(target) = self.narrow_target(scrutinee) else { return Vec::new() };
        self.ctx.obligations_ruled_out_by_matcher(matcher, &self.owed_at(&target)).iter()
            .map(|obligation| NarrowFact::Discharge(target, *obligation))
            .collect()
    }

    pub(super) fn apply_narrowings(&mut self, narrowings: &[NarrowFact]) {
        for fact in narrowings {
            match fact {
                NarrowFact::Discharge(target, obligation) => self.discharge(*target, *obligation),
                NarrowFact::Tag(i, tag) => self.locals[*i].tag = tag.clone(),
            }
        }
    }

    pub(super) fn obligations_handled_by_local(&self) -> Vec<Obligations> {
        self.frame_locals().iter()
            .map(|l| l.owed.iter().copied().filter(|o| l.handled.contains(o) || l.discharged.contains(o)).collect())
            .collect()
    }

    pub(super) fn mark_obligations_handled_when_resolved_on_every_path(&mut self, paths: &[Vec<Obligations>]) {
        for (i, local) in self.frame_locals_mut().iter_mut().enumerate() {
            local.handled.extend(obligations_resolved_on_every_path(paths, i));
        }
    }

    /// Applies flow facts, runs `f` under them, then restores the prior flow state.
    pub(super) fn narrow_branch<R>(&mut self, facts: &[NarrowFact], f: impl FnOnce(&mut Self) -> R) -> (R, Vec<Obligations>) {
        self.narrow_under(facts, f, Checker::restore_flow)
    }

    /// Applies flow facts, runs `f` under them, then restores the prior flow state but keeps each
    /// local's move site and give-back sources.
    pub(super) fn narrow_branch_keeping_moves<R>(&mut self, facts: &[NarrowFact], f: impl FnOnce(&mut Self) -> R) -> (R, Vec<Obligations>) {
        self.narrow_under(facts, f, Checker::restore_flow_keeping_write_ownership_transfers)
    }

    fn narrow_under<R>(&mut self, facts: &[NarrowFact], f: impl FnOnce(&mut Self) -> R,
                       unwind: fn(&mut Self, &FlowSnapshot)) -> (R, Vec<Obligations>) {
        let pre = self.snapshot();
        self.apply_narrowings(facts);
        let r = f(self);
        let resolved = self.obligations_handled_by_local();
        unwind(self, &pre);
        (r, resolved)
    }

}

fn intersect_narrow_facts(left: Vec<NarrowFact>, right: &[NarrowFact]) -> Vec<NarrowFact> {
    left.into_iter().filter(|fact| right.contains(fact)).collect()
}

pub(crate) fn collect_whole_value_binders(hir: &Hir, matcher: &HirId<HirMatcher>) -> Vec<Symbol> {
    match hir.get(matcher) {
        HirMatcher::Binder(name) => vec![*name],
        HirMatcher::As(name, inner) => {
            let mut out = vec![*name];
            out.extend(collect_whole_value_binders(hir, inner));
            out
        },
        HirMatcher::And(parts) => {
            let mut out = Vec::new();
            for part in parts {
                out.extend(collect_whole_value_binders(hir, part));
            }
            out
        },
        // Binding alternatives agree on their names, so the first that binds stands for all.
        HirMatcher::Or(alternatives) => {
            if let Some(binding) = alternatives.iter().find(|a| hir.get(*a).binds_anything(hir)) {
                collect_whole_value_binders(hir, binding)
            } else {
                Vec::new()
            }
        },
        _ => Vec::new(),
    }
}

fn obligations_resolved_on_every_path(paths: &[Vec<Obligations>], i: usize) -> Obligations {
    let Some((first, rest)) = paths.split_first() else { return Obligations::new() };
    let Some(resolved) = first.get(i) else { return Obligations::new() };
    resolved.iter().copied()
        .filter(|o| rest.iter().all(|p| p.get(i).is_some_and(|r| r.contains(o))))
        .collect()
}

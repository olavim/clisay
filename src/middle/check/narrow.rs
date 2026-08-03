//! What a condition or a match arm proves, so the branch it guards may assume it.

use std::collections::HashMap;

use crate::core::objects::TypeMember;
use crate::middle::hir::{BinOp, Hir, HirExpr, HirId, HirLiteral, HirMatchArm, HirMatcher, HirStmt, HirTypeDecl, Symbol, UnOp};
use crate::middle::obligations::Obligations;
use crate::middle::signatures::Witness;

use super::{Checker, NarrowFact, NarrowTarget, TypeTag};

/// What the compiler can tell about a condition's truth without running it.
#[derive(Clone, Copy, PartialEq)]
enum Truthiness {
    Truthy,
    Falsy,
    Unknown,
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
            self.locals[i].discharged.insert(self.sigs.opt);
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

    /// Where `target.field`'s narrowing lands when the place can be narrowed: a `this` field, or a
    /// field of an immutable local.
    pub(super) fn narrowable_field(&self, target: &HirId<HirExpr>, field: Symbol) -> Option<NarrowTarget> {
        match self.hir.get(target) {
            HirExpr::This => {
                let decl = self.current_type?;
                (!self.field_is_mutable(&decl, field)).then_some(NarrowTarget::ThisField(field))
            },
            HirExpr::Identifier(name) => {
                let i = self.frame_index_of(*name)?;
                if self.locals[i].func.is_some() || self.locals[i].mutable {
                    return None;
                }
                let TypeTag::Concrete(decl) = &self.locals[i].tag else { return None };
                (!self.field_is_mutable(decl, field)).then_some(NarrowTarget::LocalField(i, field))
            },
            _ => None,
        }
    }

    /// The facts a condition establishes. `positive` selects the branch where it holds versus
    /// the branch where it fails.
    pub(super) fn narrowings(&self, cond: &HirId<HirExpr>, positive: bool) -> Vec<NarrowFact> {
        match self.hir.get(cond) {
            // A bare truthiness test narrows in the truthy branch.
            HirExpr::Identifier(_) | HirExpr::Index(_, _, _) if positive => self.narrow_non_null(cond),
            HirExpr::Match(scrutinee, matcher) if positive => self.narrow_match_positive(scrutinee, matcher),
            // The false branch of `x ~ M` rules out every witness `M` covers.
            HirExpr::Match(scrutinee, matcher) if !positive => self.narrow_match_negative(scrutinee, matcher),
            // `x != null` narrows when true; `x == null` narrows when false.
            HirExpr::Binary(BinOp::NotEqual, l, r) if positive => self.narrow_null_compare(l, r),
            HirExpr::Binary(BinOp::Equal, l, r) if !positive => self.narrow_null_compare(l, r),
            // Both sides of a conjunction hold, so their facts combine.
            HirExpr::Binary(BinOp::And, l, r) if positive => {
                let mut narrow = self.narrowings(l, true);
                narrow.extend(self.narrowings(r, true));
                narrow
            },
            HirExpr::Binary(BinOp::And, l, r) if !positive => match (self.never_fails(l), self.never_fails(r)) {
                (true, _) => self.narrowings(r, false),
                (_, true) => self.narrowings(l, false),
                _ => intersect_facts(self.narrowings(l, false), &self.narrowings(r, false)),
            },
            HirExpr::Binary(BinOp::Or, l, r) if positive => match (self.never_holds(l), self.never_holds(r)) {
                (true, _) => self.narrowings(r, true),
                (_, true) => self.narrowings(l, true),
                _ => intersect_facts(self.narrowings(l, true), &self.narrowings(r, true)),
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

    /// What a condition's truthiness is, as far as the compiler can tell without running it.
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

    /// Whether a callee names a declared type, so calling it constructs. Case makes a value name
    /// and a type name distinct, so a local cannot shadow one here.
    fn names_type(&self, callee: &HirId<HirExpr>) -> bool {
        matches!(self.hir.get(callee), HirExpr::Identifier(name) if self.sigs.is_type(*name))
    }

    /// Whether a condition can never hold. An unknown condition might, so it answers no.
    fn never_holds(&self, cond: &HirId<HirExpr>) -> bool {
        self.eval_truthiness(cond) == Truthiness::Falsy
    }

    /// Whether a condition can never fail. An unknown condition might, so it answers no.
    fn never_fails(&self, cond: &HirId<HirExpr>) -> bool {
        self.eval_truthiness(cond) == Truthiness::Truthy
    }

    pub(super) fn narrow_null_compare(&self, l: &HirId<HirExpr>, r: &HirId<HirExpr>) -> Vec<NarrowFact> {
        let place = if self.is_null(l) { r } else if self.is_null(r) { l } else { return Vec::new() };
        self.narrow_non_null(place)
    }

    /// The place an expression names, when a narrowing can land on one.
    pub(super) fn narrow_target(&self, expr: &HirId<HirExpr>) -> Option<NarrowTarget> {
        match self.hir.get(expr) {
            HirExpr::Identifier(name) => self.frame_index_of(*name)
                .filter(|&i| self.locals[i].func.is_none())
                .map(NarrowTarget::Local),
            HirExpr::Index(target, member, _) => self.string_member(member)
                .and_then(|field| self.narrowable_field(target, field)),
            _ => None,
        }
    }

    /// A place proven non-null on this branch.
    pub(super) fn narrow_non_null(&self, expr: &HirId<HirExpr>) -> Vec<NarrowFact> {
        self.narrow_target(expr)
            .map(|target| vec![NarrowFact::Discharge(target, self.sigs.opt)])
            .unwrap_or_default()
    }

    pub(super) fn field_is_mutable(&self, decl: &HirId<HirStmt>, field: Symbol) -> bool {
        self.layout_of(decl).is_some_and(|layout| layout.is_mutable(field))
    }

    /// The true branch of `x ~ M`.
    pub(super) fn narrow_match_positive(&self, scrutinee: &HirId<HirExpr>, matcher: &HirId<HirMatcher>) -> Vec<NarrowFact> {
        let mut facts = match self.matcher_implies_non_null(matcher) {
            true => self.narrow_non_null(scrutinee),
            false => Vec::new(),
        };
        let HirExpr::Identifier(name) = self.hir.get(scrutinee) else { return facts };
        let Some(i) = self.frame_index_of(*name) else { return facts };
        if self.locals[i].func.is_some() {
            return facts;
        }
        // A nominal test confirms which type the value has, which later field reads resolve against.
        if let HirMatcher::Type { nominal: true, .. } = self.hir.get(matcher) {
            if let Some((stmt, _)) = self.tested_decl(matcher).filter(|(_, d)| !self.leaves_type_open(d)) {
                facts.push(NarrowFact::Tag(i, TypeTag::Concrete(stmt)));
            }
        }
        facts.extend(self.locals[i].owed.iter()
            .filter(|o| matches!(self.sigs.witness(**o), Some(Witness::Type(_) | Witness::Trait(_))))
            .filter(|o| self.matcher_disjoint_from(matcher, **o))
            .map(|o| NarrowFact::Discharge(NarrowTarget::Local(i), *o)));
        facts
    }

    /// Whether no value the matcher accepts can be in the obligation's bad state.
    pub(super) fn matcher_disjoint_from(&self, matcher: &HirId<HirMatcher>, obligation: Symbol) -> bool {
        match self.hir.get(matcher) {
            HirMatcher::As(_, inner) => self.matcher_disjoint_from(inner, obligation),
            HirMatcher::Or(alternatives) => alternatives.iter().all(|m| self.matcher_disjoint_from(m, obligation)),
            HirMatcher::And(parts) => parts.iter().any(|m| self.matcher_disjoint_from(m, obligation)),
            // A literal and an array are not instances, so they carry no object witness.
            HirMatcher::Literal(_) | HirMatcher::Array(_) => true,
            // A concrete type is exactly itself. A trait leaves the concrete type open, so another
            // type providing it may provide the witness too.
            HirMatcher::Type { nominal: true, .. } => match self.tested_decl(matcher) {
                Some((_, decl)) => !self.leaves_type_open(decl)
                    && !self.sigs.obligations_witnessed_by_decl(decl).contains(&obligation),
                None => false,
            },
            // A structural test reads a surface rather than an identity, so it rules out only a
            // witness that cannot expose that surface.
            HirMatcher::Type { nominal: false, .. } => self.surface_rules_out(matcher, obligation),
            // A shape matches an instance only if its type exposes every member the shape names, so
            // a witness missing one cannot be what matched.
            HirMatcher::Shape(fields) => match self.witness_decl(obligation) {
                Some(witness) => fields.iter().any(|field| self.lacks_member(&witness, &field.key)),
                None => false,
            },
            // A wildcard and a binder match anything, so they rule nothing out.
            _ => false,
        }
    }

    /// The declaration a type test names. A name that resolved to nothing settles nothing.
    fn tested_decl(&self, matcher: &HirId<HirMatcher>) -> Option<(HirId<HirStmt>, &'a HirTypeDecl)> {
        let stmt = self.bindings.type_ref(matcher)?;
        match self.hir.get(&stmt) {
            HirStmt::Type(decl) | HirStmt::Trait(decl) => Some((stmt, decl)),
            _ => None,
        }
    }

    /// Whether a declaration leaves the concrete type open, which a trait does and a type does not.
    fn leaves_type_open(&self, decl: &HirTypeDecl) -> bool {
        self.hir.type_info(decl.id).is_some_and(|info| info.is_trait)
    }

    /// The declaration an obligation's type witness names.
    fn witness_decl(&self, obligation: Symbol) -> Option<HirId<HirStmt>> {
        let Some(Witness::Type(id)) = self.sigs.witness(obligation) else { return None };
        self.sigs.decl_of_id(*id)
    }

    /// Whether the surface a structural test reads is one the obligation's witness cannot expose.
    fn surface_rules_out(&self, matcher: &HirId<HirMatcher>, obligation: Symbol) -> bool {
        let Some(witness) = self.witness_decl(obligation) else { return false };
        let Some(tested) = self.bindings.type_ref(matcher) else { return false };
        let Some(members) = self.bindings.surface(&tested) else { return false };
        members.iter().any(|member| self.lacks_member_named(&witness, *member))
    }

    /// Whether a type provably does not expose a member.
    fn lacks_member(&self, decl: &HirId<HirStmt>, key: &HirLiteral) -> bool {
        let HirLiteral::String(field) = key else { return false };
        match self.hir.symbol_of(field) {
            // A name no program interned is a member of nothing.
            None => true,
            Some(field) => self.lacks_member_named(decl, field),
        }
    }

    fn lacks_member_named(&self, decl: &HirId<HirStmt>, field: Symbol) -> bool {
        match self.bindings.layout_of_decl(decl) {
            Some(layout) => !layout.members.contains_key(&field) || !layout.is_public(field),
            // A trait declares no member shape, so it rules nothing out.
            None => false,
        }
    }

    /// The witnesses a failed `x ~ M` rules out.
    pub(super) fn narrow_match_negative(&self, scrutinee: &HirId<HirExpr>, matcher: &HirId<HirMatcher>) -> Vec<NarrowFact> {
        let HirExpr::Identifier(name) = self.hir.get(scrutinee) else { return Vec::new() };
        let Some(i) = self.frame_index_of(*name) else { return Vec::new() };
        if self.locals[i].func.is_some() {
            return Vec::new();
        }
        self.matcher_rules_out(matcher, &self.locals[i].owed).iter()
            .map(|obligation| NarrowFact::Discharge(NarrowTarget::Local(i), *obligation))
            .collect()
    }

    pub(super) fn is_null(&self, expr: &HirId<HirExpr>) -> bool {
        matches!(self.hir.get(expr), HirExpr::Literal(HirLiteral::Null))
    }

    pub(super) fn apply_narrowings(&mut self, narrowings: &[NarrowFact]) {
        for fact in narrowings {
            match fact {
                NarrowFact::Discharge(target, obligation) => self.discharge(*target, *obligation),
                NarrowFact::Tag(i, tag) => self.locals[*i].tag = tag.clone(),
            }
        }
    }

    /// Applies flow facts, runs `f` under them, then restores the prior flow state. Returns
    /// `f`'s result so a branch can snapshot its end state before the restore.
    pub(super) fn narrow_branch<R>(&mut self, facts: &[NarrowFact], f: impl FnOnce(&mut Self) -> R) -> R {
        let pre = self.snapshot();
        self.apply_narrowings(facts);
        let r = f(self);
        self.restore(&pre);
        r
    }

    /// Whether matching this matcher proves the scrutinee non-null. A bare binder, a wildcard, and
    /// a `null` literal each admit null, so they prove nothing.
    fn matcher_implies_non_null(&self, matcher: &HirId<HirMatcher>) -> bool {
        match self.hir.get(matcher) {
            HirMatcher::Wildcard | HirMatcher::Binder(_) => false,
            HirMatcher::Literal(HirLiteral::Null) => false,
            HirMatcher::Literal(_) => true,
            HirMatcher::Type { .. } | HirMatcher::Shape(_) | HirMatcher::Array(_) => true,
            HirMatcher::As(_, inner) => self.matcher_implies_non_null(inner),
            HirMatcher::And(parts) => parts.iter().any(|p| self.matcher_implies_non_null(p)),
            HirMatcher::Or(alternatives) => alternatives.iter().all(|a| self.matcher_implies_non_null(a)),
        }
    }

    /// Recovers a nominal destructure's declared field facts onto the names its shape binds. A
    /// nullable field makes its binder owe `opt`, exactly as reading `x.field` would. A structural
    /// shape names no type, so it reaches this with nothing to resolve against.
    pub(super) fn recover_shape_fields(&self, decl: &HirId<HirStmt>, shape: &HirId<HirMatcher>, out: &mut HashMap<Symbol, Obligations>) {
        let (HirMatcher::Shape(fields), Some(layout)) = (self.hir.get(shape), self.layout_of(decl)) else { return };
        for field in fields {
            let HirLiteral::String(key) = &field.key else { continue };
            let Some(sym) = self.hir.symbol_of(key) else { continue };
            let mut owed = layout.clause_of(sym).map(|c| c.owed.clone()).unwrap_or_default();
            if layout.is_nullable(sym) {
                owed.insert(self.sigs.opt);
            }
            for name in whole_value_binders(self.hir, &field.value) {
                let out = out.entry(name).or_default();
                for ob in owed.iter() { out.insert(*ob); }
            }
        }
    }

    /// Whether an arm always runs once reached. A guarded arm may not.
    fn arm_always_runs(&self, arm: &HirMatchArm) -> bool {
        arm.guard.as_ref().is_none_or(|guard| self.is_literal_true(guard))
    }

    /// The witnesses a match arm rules out for the arms below it.
    pub(super) fn arm_rules_out(&self, arm: &HirMatchArm, remaining: &Obligations) -> Obligations {
        match self.arm_always_runs(arm) {
            true => self.matcher_rules_out(&arm.matcher, remaining),
            false => Obligations::new(),
        }
    }

    /// The obligations a match arm counts as examined on the scrutinee.
    pub(super) fn arm_settles(&self, arm: &HirMatchArm, remaining: &Obligations) -> Obligations {
        match self.arm_always_runs(arm) {
            true => self.matcher_settles(&arm.matcher, remaining),
            false => Obligations::new(),
        }
    }

    /// The obligations this test counts as examined, which is what `discharge before drop` reports
    /// on at scope end.
    pub(super) fn matcher_settles(&self, matcher: &HirId<HirMatcher>, remaining: &Obligations) -> Obligations {
        let mut out = self.matcher_rules_out(matcher, remaining);
        out.extend(self.matcher_examines(matcher, remaining));
        out
    }

    /// The obligations a test asks about, whatever it concludes.
    fn matcher_examines(&self, matcher: &HirId<HirMatcher>, remaining: &Obligations) -> Obligations {
        match self.hir.get(matcher) {
            HirMatcher::As(_, inner) => self.matcher_examines(inner, remaining),
            HirMatcher::Or(parts) | HirMatcher::And(parts) => parts.iter()
                .flat_map(|part| self.matcher_examines(part, remaining))
                .collect(),
            HirMatcher::Type { nominal: true, shape, .. } => {
                let Some((stmt, decl)) = self.tested_decl(matcher) else { return Obligations::new() };
                // A shape that can fail on a real witness may never run, so it asks nothing.
                if !shape.as_ref().is_none_or(|s| self.destructure_total(&stmt, s)) {
                    return Obligations::new();
                }
                let witnessed = self.sigs.obligations_witnessed_by_decl(decl);
                remaining.iter().copied().filter(|o| witnessed.contains(o)).collect()
            },
            _ => Obligations::new(),
        }
    }

    /// The witnesses a bare matcher rules out, for the `~` one-liner, which has no arms to consult.
    pub(super) fn matcher_rules_out(&self, matcher: &HirId<HirMatcher>, remaining: &Obligations) -> Obligations {
        remaining.iter().copied().filter(|w| self.matcher_total_over_witness(matcher, *w)).collect()
    }

    /// Whether a matcher matches every value in a witness's bad state.
    pub(super) fn matcher_total_over_witness(&self, matcher: &HirId<HirMatcher>, witness: Symbol) -> bool {
        self.sigs.witness(witness).is_some_and(|w| self.total_over_witness(matcher, w))
    }

    /// Whether a matcher matches every value the witness names.
    pub(super) fn total_over_witness(&self, matcher: &HirId<HirMatcher>, witness: &Witness) -> bool {
        match self.hir.get(matcher) {
            HirMatcher::As(_, inner) => self.total_over_witness(inner, witness),
            HirMatcher::Or(alternatives) => alternatives.iter().any(|m| self.total_over_witness(m, witness)),
            HirMatcher::And(parts) => parts.iter().all(|m| self.total_over_witness(m, witness)),
            HirMatcher::Literal(HirLiteral::Null) => matches!(witness, Witness::Null),
            HirMatcher::Type { shape, .. } => {
                let Some((stmt, decl)) = self.tested_decl(matcher) else { return false };
                match witness {
                    Witness::Type(id) => decl.id == *id
                        && shape.as_ref().is_none_or(|s| self.destructure_total(&stmt, s)),
                    Witness::Trait(id) => decl.id == *id && shape.is_none(),
                    Witness::Null => false,
                }
            },
            _ => false,
        }
    }

    /// Whether an `is Type { ... }` destructure matches every value of the type: every named field
    /// is public and binds irrefutably.
    pub(super) fn destructure_total(&self, decl: &HirId<HirStmt>, shape: &HirId<HirMatcher>) -> bool {
        let HirMatcher::Shape(fields) = self.hir.get(shape) else { return false };
        fields.iter().all(|field| {
            self.is_public_field(decl, &field.key)
                && matches!(self.hir.get(&field.value), HirMatcher::Binder(_) | HirMatcher::Wildcard)
        })
    }

    /// Whether `key` names a public field of the declaration.
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

    pub(super) fn is_literal_true(&self, guard: &HirId<HirExpr>) -> bool {
        matches!(self.hir.get(guard), HirExpr::Literal(HirLiteral::Boolean(true)))
    }
}

/// The facts both branches of a disjunction establish.
fn intersect_facts(left: Vec<NarrowFact>, right: &[NarrowFact]) -> Vec<NarrowFact> {
    left.into_iter().filter(|fact| right.contains(fact)).collect()
}

/// The names a matcher binds to the whole matched value: a top-level binder or an `as` name. A
/// shape, array, or type destructure binds sub-values, which are clean payloads.
pub(super) fn whole_value_binders(hir: &Hir, matcher: &HirId<HirMatcher>) -> Vec<Symbol> {
    let mut out = Vec::new();
    collect_whole_value_binders(hir, matcher, &mut out);
    out
}

fn collect_whole_value_binders(hir: &Hir, matcher: &HirId<HirMatcher>, out: &mut Vec<Symbol>) {
    match hir.get(matcher) {
        HirMatcher::Binder(name) => out.push(*name),
        HirMatcher::As(name, inner) => { out.push(*name); collect_whole_value_binders(hir, inner, out); },
        HirMatcher::And(parts) => for part in parts { collect_whole_value_binders(hir, part, out); },
        // Binding alternatives agree on their names, so the first that binds stands for all.
        HirMatcher::Or(alternatives) => if let Some(binding) = alternatives.iter().find(|a| hir.get(*a).binds_anything(hir)) { collect_whole_value_binders(hir, binding, out); },
        _ => {},
    }
}

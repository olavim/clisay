//! What a condition or a match arm proves, so the branch it guards may assume it.

use std::collections::HashMap;

use crate::core::objects::TypeMember;
use crate::middle::hir::{BinOp, HirExpr, HirId, HirLiteral, HirMatchArm, HirMatcher, Symbol, UnOp};
use crate::middle::obligations::Obligations;
use crate::middle::signatures::Witness;

use super::{Checker, NarrowFact, NarrowTarget, TypeTag};

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
                let type_name = self.current_type?;
                (!self.field_is_mutable(type_name, field)).then_some(NarrowTarget::ThisField(field))
            },
            HirExpr::Identifier(name) => {
                let i = self.frame_index_of(*name)?;
                if self.locals[i].func.is_some() || self.locals[i].mutable {
                    return None;
                }
                let TypeTag::Concrete(type_name) = &self.locals[i].tag else { return None };
                (!self.field_is_mutable(*type_name, field)).then_some(NarrowTarget::LocalField(i, field))
            },
            _ => None,
        }
    }

    /// The facts a condition establishes. `positive` selects the branch where it holds versus
    /// the branch where it fails.
    pub(super) fn narrowings(&self, cond: &HirId<HirExpr>, positive: bool) -> Vec<NarrowFact> {
        match self.hir.get(cond) {
            // A bare truthiness test narrows in the truthy branch.
            HirExpr::Identifier(_) | HirExpr::Index(_, _, _) if positive => self.narrow_place(cond),
            HirExpr::Is(target, type_name) if positive => self.narrow_is(target, *type_name),
            // The false branch of `x is W` rules out `W`'s obligation. This is the direction flip a witness needs.
            HirExpr::Is(target, type_name) if !positive => self.narrow_is_negative(target, *type_name),
            // A match against a structural/type/array shape proves the scrutinee non-null.
            HirExpr::Match(scrutinee, matcher) if positive && matcher_implies_non_null(matcher) => self.narrow_place(scrutinee),
            // `x != null` narrows when true; `x == null` narrows when false.
            HirExpr::Binary(BinOp::NotEqual, l, r) if positive => self.narrow_null_compare(l, r),
            HirExpr::Binary(BinOp::Equal, l, r) if !positive => self.narrow_null_compare(l, r),
            // Conjunction narrows both sides when true. By De Morgan, disjunction narrows both when false.
            HirExpr::Binary(BinOp::And, l, r) if positive => {
                let mut narrow = self.narrowings(l, true);
                narrow.extend(self.narrowings(r, true));
                narrow
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
        let place = if self.is_null(l) { r } else if self.is_null(r) { l } else { return Vec::new() };
        self.narrow_place(place)
    }

    pub(super) fn narrow_place(&self, expr: &HirId<HirExpr>) -> Vec<NarrowFact> {
        match self.hir.get(expr) {
            HirExpr::Identifier(name) => match self.frame_index_of(*name) {
                Some(i) if self.locals[i].func.is_none() => vec![NarrowFact::Discharge(NarrowTarget::Local(i), self.sigs.opt)],
                _ => Vec::new(),
            },
            HirExpr::Index(target, member, _) => {
                let narrowing = self.string_member(member).and_then(|field| self.narrowable_field(target, field));
                narrowing.map(|n| vec![NarrowFact::Discharge(n, self.sigs.opt)]).unwrap_or_default()
            },
            _ => Vec::new(),
        }
    }

    pub(super) fn field_is_mutable(&self, type_name: Symbol, field: Symbol) -> bool {
        self.layout_of(type_name).is_some_and(|layout| layout.is_mutable(field))
    }

    /// The positive branch of `x is W` narrows a local to non-null and to the tested concrete
    /// type. When `W` witnesses an obligation the value keeps owing it. The tag just records that
    /// it is confirmed to be `W`.
    pub(super) fn narrow_is(&self, target: &HirId<HirExpr>, type_name: Symbol) -> Vec<NarrowFact> {
        let HirExpr::Identifier(name) = self.hir.get(target) else { return Vec::new() };
        let Some(i) = self.frame_index_of(*name) else { return Vec::new() };
        if self.locals[i].func.is_some() {
            return Vec::new();
        }
        let mut facts = vec![NarrowFact::Discharge(NarrowTarget::Local(i), self.sigs.opt)];
        if self.sigs.is_type(type_name) || self.sigs.is_witness_type(type_name) {
            facts.push(NarrowFact::Tag(i, TypeTag::Concrete(type_name)));
        }
        facts
    }

    /// The false branch of `x is W` discharges the obligation `W` witnesses, if `W` names one.
    pub(super) fn narrow_is_negative(&self, target: &HirId<HirExpr>, type_name: Symbol) -> Vec<NarrowFact> {
        let HirExpr::Identifier(name) = self.hir.get(target) else { return Vec::new() };
        let Some(i) = self.frame_index_of(*name) else { return Vec::new() };
        if self.locals[i].func.is_some() {
            return Vec::new();
        }
        match self.sigs.obligation_for_witness(type_name) {
            Some(obligation) => vec![NarrowFact::Discharge(NarrowTarget::Local(i), obligation)],
            None => Vec::new(),
        }
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
}

/// Whether matching this matcher proves the scrutinee non-null. A bare binder, a wildcard, and a
/// `null` literal each admit null, so they prove nothing.
fn matcher_implies_non_null(matcher: &HirMatcher) -> bool {
    match matcher {
        HirMatcher::Wildcard | HirMatcher::Binder(_) => false,
        HirMatcher::Literal(HirLiteral::Null) => false,
        HirMatcher::Literal(_) => true,
        HirMatcher::Type { .. } | HirMatcher::Shape(_) | HirMatcher::Array(_) => true,
        HirMatcher::As(_, inner) => matcher_implies_non_null(inner),
        HirMatcher::And(parts) => parts.iter().any(matcher_implies_non_null),
        HirMatcher::Or(alternatives) => alternatives.iter().all(matcher_implies_non_null),
    }
}

impl<'a> Checker<'a> {
    /// Recovers a nominal destructure's declared field facts onto the names its shape binds. A
    /// nullable field makes its binder owe `opt`, exactly as reading `x.field` would. A structural
    /// shape names no type, so it reaches this with nothing to resolve against.
    pub(super) fn recover_shape_fields(&self, type_name: Symbol, shape: &HirMatcher, out: &mut HashMap<Symbol, Obligations>) {
        let (HirMatcher::Shape(fields), Some(layout)) = (shape, self.layout_of(type_name)) else { return };
        for field in fields {
            let HirLiteral::String(key) = &field.key else { continue };
            let Some(sym) = self.hir.symbol_of(key) else { continue };
            let mut owed = layout.clause_of(sym).map(|c| c.owed.clone()).unwrap_or_default();
            if layout.is_nullable(sym) {
                owed.insert(self.sigs.opt);
            }
            for name in whole_value_binders(&field.value) {
                let out = out.entry(name).or_default();
                for ob in owed.iter() { out.insert(*ob); }
            }
        }
    }

    /// The witnesses a match arm rules out for the arms below it.
    pub(super) fn arm_rules_out(&self, arm: &HirMatchArm, remaining: &Obligations) -> Obligations {
        if let Some(guard) = &arm.guard {
            // A guarded arm may not run, so it cannot be trusted to rule out a witness.
            // Only a literal `true` guard always runs.
            if !self.is_literal_true(guard) {
                return Obligations::new();
            }
        }
        self.matcher_rules_out(&arm.matcher, remaining)
    }

    /// The witnesses a bare matcher rules out, for the `~` one-liner, which has no arms to consult.
    pub(super) fn matcher_rules_out(&self, matcher: &HirMatcher, remaining: &Obligations) -> Obligations {
        remaining.iter().copied().filter(|w| self.matcher_total_over_witness(matcher, *w)).collect()
    }

    /// Whether a matcher matches every value in a witness's bad state.
    pub(super) fn matcher_total_over_witness(&self, matcher: &HirMatcher, witness: Symbol) -> bool {
        self.sigs.witness(witness).is_some_and(|w| self.total_over_witness(matcher, w))
    }

    /// Whether a matcher matches every value the witness names.
    pub(super) fn total_over_witness(&self, matcher: &HirMatcher, witness: &Witness) -> bool {
        match matcher {
            HirMatcher::As(_, inner) => self.total_over_witness(inner, witness),
            HirMatcher::Or(alternatives) => alternatives.iter().any(|m| self.total_over_witness(m, witness)),
            HirMatcher::And(parts) => parts.iter().all(|m| self.total_over_witness(m, witness)),
            HirMatcher::Literal(HirLiteral::Null) => matches!(witness, Witness::Null),
            HirMatcher::Type { name: tested, shape, .. } => match witness {
                Witness::Type(name) if tested == name => shape.as_ref().is_none_or(|s| self.destructure_total(*name, s)),
                Witness::Trait(name) => tested == name && shape.is_none(),
                _ => false,
            },
            _ => false,
        }
    }

    /// Whether an `is Type { ... }` destructure matches every value of the type: every named field
    /// is public and binds irrefutably.
    pub(super) fn destructure_total(&self, type_name: Symbol, shape: &HirMatcher) -> bool {
        let HirMatcher::Shape(fields) = shape else { return false };
        fields.iter().all(|field| {
            self.is_public_field(type_name, &field.key)
                && matches!(field.value, HirMatcher::Binder(_) | HirMatcher::Wildcard)
        })
    }

    /// Whether `key` names a public field of the type. A built-in witness type carries no layout,
    /// so its public surface is answered directly.
    pub(super) fn is_public_field(&self, type_name: Symbol, key: &HirLiteral) -> bool {
        let HirLiteral::String(field) = key else { return false };
        match self.layout_of(type_name) {
            Some(layout) => match self.hir.symbol_of(field) {
                Some(field) => matches!(layout.members.get(&field), Some(TypeMember::Field(_))) && layout.is_public(field),
                None => false,
            },
            None => builtin_public_field(self.hir.text(type_name), field),
        }
    }

    pub(super) fn is_literal_true(&self, guard: &HirId<HirExpr>) -> bool {
        matches!(self.hir.get(guard), HirExpr::Literal(HirLiteral::Boolean(true)))
    }
}

/// The public fields of a built-in witness type, which carries no layout. `Err` exposes `value`.
fn builtin_public_field(type_name: &str, field: &str) -> bool {
    matches!((type_name, field), ("Err", "value"))
}

/// The names a matcher binds to the whole matched value: a top-level binder or an `as` name. A
/// shape, array, or type destructure binds sub-values, which are clean payloads.
pub(super) fn whole_value_binders(matcher: &HirMatcher) -> Vec<Symbol> {
    let mut out = Vec::new();
    collect_whole_value_binders(matcher, &mut out);
    out
}

fn collect_whole_value_binders(matcher: &HirMatcher, out: &mut Vec<Symbol>) {
    match matcher {
        HirMatcher::Binder(name) => out.push(*name),
        HirMatcher::As(name, inner) => { out.push(*name); collect_whole_value_binders(inner, out); },
        HirMatcher::And(parts) => for part in parts { collect_whole_value_binders(part, out); },
        // Binding alternatives agree on their names, so the first that binds stands for all.
        HirMatcher::Or(alternatives) => if let Some(binding) = alternatives.iter().find(|a| a.binds_anything()) { collect_whole_value_binders(binding, out); },
        _ => {},
    }
}

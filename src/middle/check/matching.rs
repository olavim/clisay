//! Match-arm discharge: which witnesses an arm rules out for the arms below it.

use std::collections::HashSet;

use crate::core::objects::TypeMember;
use crate::middle::signatures::Witness;
use crate::middle::hir::{HirExpr, HirId, HirLiteral, HirMatchArm, HirMatcher, Symbol};

use super::Checker;

impl<'a> Checker<'a> {
    /// The witnesses a match arm rules out for the arms below it.
    pub(super) fn arm_rules_out(&self, arm: &HirMatchArm, remaining: &HashSet<Symbol>) -> HashSet<Symbol> {
        if let Some(guard) = &arm.guard {
            // A guarded arm may not run, so it cannot be trusted to rule out a witness.
            // Only a literal `true` guard always runs.
            if !self.is_literal_true(guard) {
                return HashSet::new();
            }
        }
        remaining.iter().copied().filter(|w| self.matcher_total_over_witness(&arm.matcher, *w)).collect()
    }

    /// Whether a matcher matches every value in a witness's bad state.
    fn matcher_total_over_witness(&self, matcher: &HirMatcher, witness: Symbol) -> bool {
        self.sigs.witness(witness).is_some_and(|w| self.total_over_witness(matcher, w))
    }

    /// Whether a matcher matches every value the witness names.
    fn total_over_witness(&self, matcher: &HirMatcher, witness: &Witness) -> bool {
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
    fn destructure_total(&self, type_name: Symbol, shape: &HirMatcher) -> bool {
        let HirMatcher::Shape(fields) = shape else { return false };
        fields.iter().all(|field| {
            self.is_public_field(type_name, &field.key)
                && matches!(field.value, HirMatcher::Binder(_) | HirMatcher::Wildcard)
        })
    }

    /// Whether `key` names a public field of the type. A built-in witness type carries no layout,
    /// so its public surface is answered directly.
    fn is_public_field(&self, type_name: Symbol, key: &HirLiteral) -> bool {
        let HirLiteral::String(field) = key else { return false };
        match self.layout_of(type_name) {
            Some(layout) => match self.hir.symbol_of(field) {
                Some(field) => matches!(layout.members.get(&field), Some(TypeMember::Field(_))) && layout.is_public(field),
                None => false,
            },
            None => builtin_public_field(self.hir.text(type_name), field),
        }
    }

    fn is_literal_true(&self, guard: &HirId<HirExpr>) -> bool {
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
        // Alternatives bind the same names, so the first one stands for all.
        HirMatcher::Or(alternatives) => if let Some(first) = alternatives.first() { collect_whole_value_binders(first, out); },
        _ => {},
    }
}

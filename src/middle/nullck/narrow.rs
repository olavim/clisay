//! Flow-sensitive narrowing.

use crate::middle::hir::{BinOp, HirExpr, HirId, HirLiteral, Symbol, UnOp};

use super::{Checker, FlowSnapshot, NarrowFact, NarrowKey, TypeTag};

impl<'a> Checker<'a> {
    /// A reassignment drops the binding's narrowing facts. The slot is non-null again only if
    /// the new value is.
    pub(super) fn reset_narrowing(&mut self, i: usize, now_non_null: bool) {
        self.narrowed.retain(|key| !matches!(key, NarrowKey::Local(j) | NarrowKey::LocalField(j, _) if *j == i));
        if now_non_null {
            self.narrowed.insert(NarrowKey::Local(i));
        }
    }

    /// The narrow key for `target.field` when the place can be narrowed: a `this` field, or a
    /// field of an immutable local.
    pub(super) fn narrowable_field_key(&self, target: &HirId<HirExpr>, field: Symbol) -> Option<NarrowKey> {
        match self.hir.get(target) {
            HirExpr::This => {
                let type_name = self.current_type?;
                (!self.field_is_mutable(type_name, field)).then_some(NarrowKey::ThisField(field))
            },
            HirExpr::Identifier(name) => {
                let i = self.frame_index_of(*name)?;
                if self.locals[i].func.is_some() || self.locals[i].mutable {
                    return None;
                }
                let TypeTag::Concrete(type_name) = &self.locals[i].tag else { return None };
                (!self.field_is_mutable(*type_name, field)).then_some(NarrowKey::LocalField(i, field))
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

    fn narrow_null_compare(&self, l: &HirId<HirExpr>, r: &HirId<HirExpr>) -> Vec<NarrowFact> {
        let place = if self.is_null(l) { r } else if self.is_null(r) { l } else { return Vec::new() };
        self.narrow_place(place)
    }

    fn narrow_place(&self, expr: &HirId<HirExpr>) -> Vec<NarrowFact> {
        match self.hir.get(expr) {
            HirExpr::Identifier(name) => match self.frame_index_of(*name) {
                Some(i) if self.locals[i].func.is_none() => vec![NarrowFact::NonNull(NarrowKey::Local(i))],
                _ => Vec::new(),
            },
            HirExpr::Index(target, member, _) => {
                let key = self.string_member(member).and_then(|field| self.narrowable_field_key(target, field));
                key.map(|k| vec![NarrowFact::NonNull(k)]).unwrap_or_default()
            },
            _ => Vec::new(),
        }
    }

    fn field_is_mutable(&self, type_name: Symbol, field: Symbol) -> bool {
        self.layout_of(type_name).is_some_and(|layout| layout.is_mutable(field))
    }

    /// An `is`-test narrows a local to non-null, and to the tested concrete type when known.
    fn narrow_is(&self, target: &HirId<HirExpr>, type_name: Symbol) -> Vec<NarrowFact> {
        let HirExpr::Identifier(name) = self.hir.get(target) else { return Vec::new() };
        let Some(i) = self.frame_index_of(*name) else { return Vec::new() };
        if self.locals[i].func.is_some() {
            return Vec::new();
        }
        let mut facts = vec![NarrowFact::NonNull(NarrowKey::Local(i))];
        if self.sigs.types_by_name.contains_key(&type_name) {
            facts.push(NarrowFact::Tag(i, TypeTag::Concrete(type_name)));
        }
        facts
    }

    fn is_null(&self, expr: &HirId<HirExpr>) -> bool {
        matches!(self.hir.get(expr), HirExpr::Literal(HirLiteral::Null))
    }

    pub(super) fn apply_narrowings(&mut self, narrowings: &[NarrowFact]) {
        for fact in narrowings {
            match fact {
                NarrowFact::NonNull(key) => { self.narrowed.insert(key.clone()); },
                NarrowFact::Tag(i, tag) => self.locals[*i].tag = tag.clone(),
            }
        }
    }

    pub(super) fn snapshot(&self) -> FlowSnapshot {
        FlowSnapshot {
            assigned: self.locals.iter().map(|l| l.assigned).collect(),
            tags: self.locals.iter().map(|l| l.tag.clone()).collect(),
            narrowed: self.narrowed.clone(),
        }
    }

    pub(super) fn restore(&mut self, flow: &FlowSnapshot) {
        for (local, (assigned, tag)) in self.locals.iter_mut().zip(flow.assigned.iter().zip(&flow.tags)) {
            local.assigned = *assigned;
            local.tag = tag.clone();
        }
        self.narrowed = flow.narrowed.clone();
    }

    /// Merges two branch snapshots.
    pub(super) fn join(&mut self, then_snap: &FlowSnapshot, else_snap: &FlowSnapshot) {
        for i in 0..self.locals.len() {
            self.locals[i].assigned = then_snap.assigned[i] && else_snap.assigned[i];
            self.locals[i].tag = if then_snap.tags[i] == else_snap.tags[i]
                { then_snap.tags[i].clone() } else
                { TypeTag::Unknown };
        }
    }
}

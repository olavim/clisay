//! Flow-sensitive narrowing.

use crate::middle::hir::{BinOp, HirExpr, HirId, HirLiteral, HirMatcher, Symbol, UnOp};

use super::{Mutability, Checker, FlowSnapshot, LocalFlow, NarrowFact, NarrowKey, TypeTag};

impl<'a> Checker<'a> {
    /// A reassignment drops the binding's narrowing facts. The slot is non-null again only if
    /// the new value is.
    pub(super) fn reset_narrowing(&mut self, i: usize, now_non_null: bool) {
        self.narrowed.retain(|key, _| !matches!(key, NarrowKey::Local(j) | NarrowKey::LocalField(j, _) if *j == i));
        if now_non_null {
            self.narrowed.entry(NarrowKey::Local(i)).or_default().insert(self.sigs.opt);
        }
    }

    /// Whether `obligation` is discharged for a place on the current path.
    pub(super) fn discharged(&self, key: &NarrowKey, obligation: Symbol) -> bool {
        self.narrowed.get(key).is_some_and(|set| set.contains(&obligation))
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

    fn narrow_null_compare(&self, l: &HirId<HirExpr>, r: &HirId<HirExpr>) -> Vec<NarrowFact> {
        let place = if self.is_null(l) { r } else if self.is_null(r) { l } else { return Vec::new() };
        self.narrow_place(place)
    }

    fn narrow_place(&self, expr: &HirId<HirExpr>) -> Vec<NarrowFact> {
        match self.hir.get(expr) {
            HirExpr::Identifier(name) => match self.frame_index_of(*name) {
                Some(i) if self.locals[i].func.is_none() => vec![NarrowFact::Discharge(NarrowKey::Local(i), self.sigs.opt)],
                _ => Vec::new(),
            },
            HirExpr::Index(target, member, _) => {
                let key = self.string_member(member).and_then(|field| self.narrowable_field_key(target, field));
                key.map(|k| vec![NarrowFact::Discharge(k, self.sigs.opt)]).unwrap_or_default()
            },
            _ => Vec::new(),
        }
    }

    fn field_is_mutable(&self, type_name: Symbol, field: Symbol) -> bool {
        self.layout_of(type_name).is_some_and(|layout| layout.is_mutable(field))
    }

    /// The positive branch of `x is W` narrows a local to non-null and to the tested concrete
    /// type. When `W` witnesses an obligation the value keeps owing it. The tag just records that
    /// it is confirmed to be `W`.
    fn narrow_is(&self, target: &HirId<HirExpr>, type_name: Symbol) -> Vec<NarrowFact> {
        let HirExpr::Identifier(name) = self.hir.get(target) else { return Vec::new() };
        let Some(i) = self.frame_index_of(*name) else { return Vec::new() };
        if self.locals[i].func.is_some() {
            return Vec::new();
        }
        let mut facts = vec![NarrowFact::Discharge(NarrowKey::Local(i), self.sigs.opt)];
        if self.sigs.is_type(type_name) || self.sigs.is_witness_type(type_name) {
            facts.push(NarrowFact::Tag(i, TypeTag::Concrete(type_name)));
        }
        facts
    }

    /// The false branch of `x is W` discharges the obligation `W` witnesses, if `W` names one.
    fn narrow_is_negative(&self, target: &HirId<HirExpr>, type_name: Symbol) -> Vec<NarrowFact> {
        let HirExpr::Identifier(name) = self.hir.get(target) else { return Vec::new() };
        let Some(i) = self.frame_index_of(*name) else { return Vec::new() };
        if self.locals[i].func.is_some() {
            return Vec::new();
        }
        match self.sigs.obligation_for_witness(type_name) {
            Some(obligation) => vec![NarrowFact::Discharge(NarrowKey::Local(i), obligation)],
            None => Vec::new(),
        }
    }

    fn is_null(&self, expr: &HirId<HirExpr>) -> bool {
        matches!(self.hir.get(expr), HirExpr::Literal(HirLiteral::Null))
    }

    pub(super) fn apply_narrowings(&mut self, narrowings: &[NarrowFact]) {
        for fact in narrowings {
            match fact {
                NarrowFact::Discharge(key, obligation) => { self.narrowed.entry(key.clone()).or_default().insert(*obligation); },
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

    pub(super) fn snapshot(&self) -> FlowSnapshot {
        FlowSnapshot {
            locals: self.locals.iter().map(|l| LocalFlow {
                assigned: l.assigned,
                tag: l.tag.clone(),
                mutability: l.mutability,
                move_site: l.move_site,
                provenance: l.provenance.clone(),
            }).collect(),
            narrowed: self.narrowed.clone(),
        }
    }

    pub(super) fn restore(&mut self, flow: &FlowSnapshot) {
        self.restore_keeping_moves(flow);
        for (local, snap) in self.locals.iter_mut().zip(&flow.locals) {
            local.move_site = snap.move_site;
            local.provenance = snap.provenance.clone();
        }
    }

    /// Restores flow but keeps each local's move site and give-back sources. A loop body's moves
    /// persist to the next iteration and past the loop, while its narrowings and assignments do not
    /// survive zero runs. A value moved before the loop stays moved on the zero-run path, so its
    /// pre-loop move merges back in rather than being masked by an in-body rebind.
    pub(super) fn restore_keeping_moves(&mut self, flow: &FlowSnapshot) {
        for (local, snap) in self.locals.iter_mut().zip(&flow.locals) {
            local.assigned = snap.assigned;
            local.tag = snap.tag.clone();
            local.mutability = snap.mutability;
            local.move_site = local.move_site.or(snap.move_site);
        }
        self.narrowed = flow.narrowed.clone();
    }

    /// Merges another branch's end state into the current local flow.
    pub(super) fn join_in(&mut self, other: &FlowSnapshot) {
        for (local, o) in self.locals.iter_mut().zip(&other.locals) {
            local.assigned = local.assigned && o.assigned;
            local.tag = if local.tag == o.tag { local.tag.clone() } else { TypeTag::Unknown };
            local.mutability = if local.mutability == o.mutability { local.mutability } else { Mutability::Unknown };
            local.move_site = local.move_site.or(o.move_site);
            local.provenance.retain(|s| o.provenance.contains(s));
        }
    }

    /// Merges two branch snapshots.
    pub(super) fn join(&mut self, then_snap: &FlowSnapshot, else_snap: &FlowSnapshot) {
        debug_assert!(then_snap.locals.len() == self.locals.len() && else_snap.locals.len() == self.locals.len());
        for (i, local) in self.locals.iter_mut().enumerate() {
            let (then_local, else_local) = (&then_snap.locals[i], &else_snap.locals[i]);
            local.assigned = then_local.assigned && else_local.assigned;
            local.tag = if then_local.tag == else_local.tag { then_local.tag.clone() } else { TypeTag::Unknown };
            local.mutability = if then_local.mutability == else_local.mutability
                { then_local.mutability } else
                { Mutability::Unknown };
            local.move_site = then_local.move_site.or(else_local.move_site);
            local.provenance = then_local.provenance.iter().copied().filter(|s| else_local.provenance.contains(s)).collect();
        }
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

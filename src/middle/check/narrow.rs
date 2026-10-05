//! What a condition or a match arm proves, so the branch it guards may assume it.

use std::collections::HashMap;

use crate::core::objects::TypeMember;
use crate::middle::hir::{access_path_steps, BinOp, Hir, HirExpr, HirId, HirLiteral, HirMatchArm, HirMatcher, HirStmt, HirTypeDecl, Symbol, UnOp};
use crate::middle::obligations::Obligations;
use crate::middle::signatures::Witness;

use super::{Checker, Ctx, Debt, FlowPath, AnchorArgument, NarrowFact, NarrowRoot, NarrowTarget, PathMap, ProvenFacts, PathStep, Route, TypeTag, ValueState, WriteRoot};
use super::scope::FlowSnapshot;

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
            Some(layout) => layout.owed(field),
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
            HirMatcher::Dict(shape) => self.matcher_disjoint_from_obligation(shape, obligation),
            HirMatcher::Type { nominal: true, .. } => match self.matcher_type_decl(matcher) {
                Some((_, decl)) => !self.hir.is_trait(decl.id)
                    && !self.sigs.obligations_witnessed_by_decl(decl).contains(&obligation),
                None => false,
            },
            HirMatcher::Type { nominal: false, .. } => self.surface_rules_out_obligation(matcher, obligation),
            HirMatcher::Shape { fields, .. } => match self.witness_decl_of_obligation(obligation) {
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
        let Witness::Type(id) = self.sigs.witness_of(obligation) else { return None };
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
        self.matcher_total_over_witness(matcher, self.sigs.witness_of(obligation))
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

    pub(super) fn pattern_always_matches(&self, pattern: &HirId<HirMatcher>, tag: &TypeTag) -> bool {
        if self.hir.get(pattern).is_irrefutable(self.hir) {
            return true;
        }

        let TypeTag::Concrete(decl) = tag else { return false };
        match self.hir.get(pattern) {
            HirMatcher::Type { shape, .. } => self.bindings.type_ref(pattern) == Some(*decl)
                && shape.as_ref().is_none_or(|s| self.matcher_total_over_type(s, decl)),
            HirMatcher::Shape { .. } => self.matcher_total_over_type(pattern, decl),
            HirMatcher::Dict(_) => false,
            HirMatcher::And(parts) => parts.iter().all(|p| self.pattern_always_matches(p, tag)),
            HirMatcher::Or(parts) => parts.iter().any(|p| self.pattern_always_matches(p, tag)),
            HirMatcher::As(_, inner) => self.pattern_always_matches(inner, tag),
            HirMatcher::Literal(_) | HirMatcher::Array(_) => false,
            HirMatcher::Binder(_) | HirMatcher::Wildcard => true,
        }
    }

    pub(super) fn matcher_total_over_type(&self, matcher: &HirId<HirMatcher>, decl: &HirId<HirStmt>) -> bool {
        let HirMatcher::Shape { fields, .. } = self.hir.get(matcher) else { return false };
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

    fn eval_truthiness(&self, cond: &HirId<HirExpr>) -> Truthiness {
        use Truthiness::{Falsy, Truthy, Unknown};
        match self.hir.get(cond) {
            HirExpr::Literal(HirLiteral::Null | HirLiteral::Boolean(false)) => Falsy,
            HirExpr::Literal(_) => Truthy,
            HirExpr::Unary(UnOp::Not, x) => self.eval_truthiness(x).negate(),
            HirExpr::Construct(..) => Truthy,
            HirExpr::Call(callee, _) if self.names_type(callee) => Truthy,
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

    pub(super) fn is_literal_true(&self, guard: &HirId<HirExpr>) -> bool {
        matches!(self.hir.get(guard), HirExpr::Literal(HirLiteral::Boolean(true)))
    }
    pub(super) fn is_null(&self, expr: &HirId<HirExpr>) -> bool {
        matches!(self.hir.get(expr), HirExpr::Literal(HirLiteral::Null))
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

    pub(super) fn collect_matcher_obligations_by_binding(&self, test: &HirId<HirMatcher>, shape: &HirId<HirMatcher>) -> HashMap<Symbol, Obligations> {
        let mut out = HashMap::new();
        let (HirMatcher::Shape { fields, .. }, Some(decl)) = (self.hir.get(shape), self.bindings.type_ref(test)) else { return out };
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
    pub(super) fn rebind(&mut self, i: usize, held: &ValueState) {
        self.forget_anchor_argument_record(&NarrowTarget::local(i));
        self.invalidate_proven_at_subtree(&NarrowTarget::local(i));
        self.locals[i].set_value(held);
    }

    pub(super) fn rebind_field(&mut self, target: &HirId<HirExpr>, field: Symbol, value: &Debt) {
        let Some(written) = self.narrowable_field(target, field) else { return };
        self.forget_anchor_argument_record(&written);
        self.invalidate_proven_at_subtree(&written);
        let owed = self.ctx.obligations_of(value);
        if !owed.is_empty() {
            self.add_owed(&written, &owed);
        }
    }

    fn forget_anchor_argument_record(&mut self, target: &NarrowTarget) {
        self.anchor_arguments.retain(|anchor, _| anchor.root != target.root || !anchor.path.starts_with(&target.path));
    }

    /// Puts the binding back to owing what its clause says.
    pub(super) fn reset_owed(&mut self, i: usize) {
        let debt = self.locals[i].read_debt(self.locals[i].clause_owed().clone());
        let tag = self.locals[i].tag().clone();
        self.rebind(i, &ValueState::of(debt, tag));
    }

    fn proven_at(&self, root: NarrowRoot) -> &PathMap<ProvenFacts> {
        match root {
            NarrowRoot::Local(i) => &self.locals[i].proven,
            NarrowRoot::This => &self.this_narrowed,
        }
    }

    fn proven_at_mut(&mut self, root: NarrowRoot) -> &mut PathMap<ProvenFacts> {
        match root {
            NarrowRoot::Local(i) => &mut self.locals[i].proven,
            NarrowRoot::This => &mut self.this_narrowed,
        }
    }

    fn update_proven(&mut self, target: &NarrowTarget, change: impl FnOnce(&mut ProvenFacts)) {
        self.proven_at_mut(target.root).update(target.path.clone(), change);
    }

    /// Drops what was proved about a path and everything under it.
    pub(super) fn invalidate_proven_at_subtree(&mut self, target: &NarrowTarget) {
        let under = target.path.clone();
        self.invalidate_proven_matching(target.root, |path| path.starts_with(&under));
    }

    /// Drops what was proved under a path, leaving what was proved about the path itself.
    pub(super) fn invalidate_proven_below(&mut self, target: &NarrowTarget) {
        let under = target.path.clone();
        self.invalidate_proven_matching(target.root, |path| path.len() > under.len() && path.starts_with(&under));
    }

    fn invalidate_proven_matching(&mut self, root: NarrowRoot, reached: impl Fn(&FlowPath) -> bool) {
        self.proven_at_mut(root).retain(|path, facts| {
            if reached(path) {
                facts.clear();
            }
            !facts.is_empty()
        });
    }

    pub(super) fn proved_owed(&self, target: &NarrowTarget) -> Option<Obligations> {
        self.proven_at(target.root).get(&target.path)?.owed.clone()
    }

    pub(super) fn proved_tag(&self, target: &NarrowTarget) -> TypeTag {
        self.proven_at(target.root).get(&target.path).map_or(TypeTag::Unknown, |f| f.tag.clone())
    }

    fn set_tag(&mut self, target: &NarrowTarget, tag: TypeTag) {
        self.update_proven(target, |facts| facts.tag = tag);
    }

    pub(super) fn add_owed(&mut self, target: &NarrowTarget, owed: &Obligations) {
        let mut seeded = self.owed_at(target);
        seeded.extend(owed.iter().copied());
        self.update_proven(target, |facts| facts.owed = Some(seeded));
    }

    /// Records that an anchor no longer owes an obligation on this path.
    pub(super) fn discharge(&mut self, target: NarrowTarget, obligation: Symbol) {
        let mut seeded = self.owed_at(&target);
        seeded.retain(|o| *o != obligation);
        self.update_proven(&target, |facts| facts.owed = Some(seeded));
    }

    /// Where `target.field`'s narrowing lands when the anchor can be narrowed.
    pub(super) fn narrowable_field(&self, target: &HirId<HirExpr>, field: Symbol) -> Option<NarrowTarget> {
        match self.ctx.hir.get(target) {
            HirExpr::This => {
                self.current_type?;
                Some(NarrowTarget::this_root().child(PathStep::Field(field)))
            },
            HirExpr::Identifier(name) => {
                let i = self.frame_index_of(*name)?;
                if self.locals[i].fn_decl {
                    return None;
                }
                Some(NarrowTarget::local(i).child(PathStep::Field(field)))
            },
            HirExpr::Index { base: inner, member, .. } => {
                let inner_field = self.ctx.string_member(member)?;
                let base = self.narrowable_field(inner, inner_field)?;
                Some(base.child(PathStep::Field(field)))
            },
            _ => None,
        }
    }

    /// The facts a condition establishes. `positive` selects the branch where it holds versus
    /// the branch where it fails.
    pub(super) fn narrowings(&self, cond: &HirId<HirExpr>, positive: bool) -> Vec<NarrowFact> {
        match self.ctx.hir.get(cond) {
            // A bare truthiness test narrows in the truthy branch.
            HirExpr::Identifier(_) | HirExpr::Index { safe: false, .. } if positive => self.narrow_non_null(cond),
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
        let operand = if self.ctx.is_null(l) { r } else if self.ctx.is_null(r) { l } else { return Vec::new() };
        self.narrow_non_null(operand)
    }

    /// What `x == <clean>` proves, where the other side is a literal or a binding owing nothing.
    pub(super) fn narrow_equal_compare(&self, l: &HirId<HirExpr>, r: &HirId<HirExpr>) -> Vec<NarrowFact> {
        let operand = match (self.proves_clean(l), self.proves_clean(r)) {
            (true, false) => r,
            (false, true) => l,
            _ => return Vec::new(),
        };
        let Some(target) = self.narrow_target(operand) else { return Vec::new() };
        let witnessed: Vec<Symbol> = self.object_witnessed(&target).collect();
        let mut facts = vec![NarrowFact::Discharge(target.clone(), self.ctx.sigs.opt)];
        facts.extend(witnessed.into_iter().map(|o| NarrowFact::Discharge(target.clone(), o)));
        facts
    }

    /// The object-witnessed obligation names an anchor owes.
    fn object_witnessed(&self, target: &NarrowTarget) -> impl Iterator<Item = Symbol> + '_ {
        self.owed_at(target).into_iter()
            .filter(|o| matches!(self.ctx.sigs.witness_of(*o), Witness::Type(_) | Witness::Trait(_)))
    }

    /// Whether being equal to this operand proves a value is in no witness's bad state.
    fn proves_clean(&self, expr: &HirId<HirExpr>) -> bool {
        match self.ctx.hir.get(expr) {
            HirExpr::Literal(HirLiteral::Number(_) | HirLiteral::String(_) | HirLiteral::Boolean(_)) => true,
            HirExpr::Identifier(name) => self.frame_index_of(*name).is_some_and(|i| {
                let local = &self.locals[i];
                !local.fn_decl && local.owed().is_empty()
            }),
            _ => false,
        }
    }

    pub(super) fn invalidate_anchor_arguments(&mut self, call: &HirId<HirExpr>, args: &[HirId<HirExpr>]) {
        let targets: Vec<NarrowTarget> = args.iter()
            .filter_map(|arg| match self.ctx.hir.get(arg) {
                HirExpr::Anchor(path) => self.narrow_target(path),
                _ => None,
            })
            .collect();
        for target in targets {
            let owed = self.owed_at(&target);
            match (target.root, target.path.is_empty()) {
                (NarrowRoot::Local(i), true) => self.reset_owed(i),
                _ => self.invalidate_proven_at_subtree(&target),
            }
            self.note_write_to(target.root);
            // Remember what the anchor owed before, so a refusal below can say the call is why.
            self.anchor_arguments.insert(target.clone(), AnchorArgument { at: *call, owed });
        }
    }

    pub(super) fn invalidated_narrowing_note(&self, node: &HirId<HirExpr>, unmet: &Obligations) -> Option<String> {
        let target = self.narrow_target(node)?;
        let handed = self.anchor_arguments.get(&target)?;
        if unmet.iter().any(|o| handed.owed.contains(o)) {
            return None;
        }
        let subject = self.ctx.hir.pos(node).snippet().to_string();
        let call = self.ctx.hir.pos(&handed.at).snippet().to_string();
        Some(format!("`{call}` dropped what was proved about `{subject}`; check it again after the call"))
    }

    /// A method that wants an anchor receiver may write any field of it, so what was proved about
    /// them goes. The receiver itself is still the value it was.
    pub(super) fn invalidate_anchor_receiver_fields(&mut self, receiver: &HirId<HirExpr>) {
        if matches!(self.ctx.hir.get(receiver), HirExpr::This) {
            self.invalidate_proven_at_subtree(&NarrowTarget::this_root());
            self.note_write_to(NarrowRoot::This);
            return;
        }
        let Some(target) = self.narrow_target(receiver) else { return };
        match (target.root, target.path.is_empty()) {
            (NarrowRoot::Local(i), true) => {
                let debt = self.locals[i].read_debt(self.locals[i].owed().clone());
                let tag = self.locals[i].tag().clone();
                self.rebind(i, &ValueState::of(debt, tag));
            },
            _ => self.invalidate_proven_at_subtree(&target),
        }
        self.note_write_to(target.root);
    }

    /// Drops what was proved about the slot a write lands in.
    pub(super) fn invalidate_proven_at_write(&mut self, target: &HirId<HirExpr>, member: &HirId<HirExpr>) {
        let Some((base, named)) = self.written_base(target) else { return };
        let Some(step) = named.then(|| self.ctx.member_step(member)).flatten() else {
            // A position the write does not name could be any of them, so everything under it goes.
            return self.invalidate_proven_below(&base);
        };
        self.invalidate_proven_at_subtree(&base.child(PathStep::EveryElement));
        self.invalidate_proven_at_subtree(&base.child(step));
    }

    pub(super) fn written_base(&self, expr: &HirId<HirExpr>) -> Option<(NarrowTarget, bool)> {
        let mut base = match self.write_root(expr) {
            WriteRoot::Local(i) | WriteRoot::Anchor(i) => NarrowTarget::local(i),
            WriteRoot::Receiver if self.current_type.is_some() => NarrowTarget::this_root(),
            _ => return None,
        };
        let (_, steps) = access_path_steps(self.ctx.hir, expr);
        for step in &steps {
            // A position the write does not name could be any of them, so the base stops there.
            let Some(named) = self.ctx.member_step(&step.key) else { return Some((base, false)) };
            base = base.child(named);
        }
        Some((base, true))
    }

    pub(super) fn condition_narrowings(&mut self, cond: &HirId<HirExpr>) -> Result<(Vec<NarrowFact>, Vec<NarrowFact>), anyhow::Error> {
        self.expr(cond)?.route(self, cond, Route::Condition)?;
        Ok((self.narrowings(cond, true), self.narrowings(cond, false)))
    }

    pub(super) fn narrow_target(&self, expr: &HirId<HirExpr>) -> Option<NarrowTarget> {
        match self.ctx.hir.get(expr) {
            HirExpr::Identifier(name) => self.frame_index_of(*name)
                .filter(|&i| !self.locals[i].fn_decl)
                .map(NarrowTarget::local),
            HirExpr::Index { base: target, member, .. } => self.ctx.string_member(member)
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
        if let Some(stored) = self.proven_at(target.root).get(&target.path).and_then(|f| f.owed.clone()) {
            return stored;
        }

        let Some(PathStep::Field(field)) = target.path.last() else {
            return match target.root {
                NarrowRoot::Local(i) => self.locals[i].owed().clone(),
                NarrowRoot::This => Obligations::new(),
            };
        };

        // A field reads its clause from the type holding it, which is the type known one step up.
        let decl = target.parent().and_then(|parent| self.type_at(&parent));
        decl.map_or_else(Obligations::new, |decl| self.ctx.field_owes(&decl, *field))
    }

    fn type_at(&self, target: &NarrowTarget) -> Option<HirId<HirStmt>> {
        if target.path.is_empty() && matches!(target.root, NarrowRoot::This) {
            return self.current_type;
        }
        match self.proved_tag(target) {
            TypeTag::Concrete(decl) => Some(decl),
            _ => None,
        }
    }

    pub(super) fn narrow_match_positive_branch(&self, scrutinee: &HirId<HirExpr>, matcher: &HirId<HirMatcher>) -> Vec<NarrowFact> {
        let Some(target) = self.narrow_target(scrutinee) else { return Vec::new() };
        let mut facts = self.discharges_at(&target, matcher);
        self.matcher_path_facts(&target, matcher, &mut facts);
        facts
    }

    fn discharges_at(&self, at: &NarrowTarget, matcher: &HirId<HirMatcher>) -> Vec<NarrowFact> {
        let mut out = Vec::new();
        if self.ctx.hir.get(matcher).rejects_null(self.ctx.hir) {
            out.push(NarrowFact::Discharge(at.clone(), self.ctx.sigs.opt));
        }
        let ruled_out: Vec<Symbol> = self.object_witnessed(at)
            .filter(|o| self.ctx.matcher_disjoint_from_obligation(matcher, *o))
            .collect();
        out.extend(ruled_out.into_iter().map(|o| NarrowFact::Discharge(at.clone(), o)));
        out
    }

    fn matcher_path_facts(&self, target: &NarrowTarget, matcher: &HirId<HirMatcher>, out: &mut Vec<NarrowFact>) {
        match self.ctx.hir.get(matcher) {
            HirMatcher::As(_, inner) => self.matcher_path_facts(target, inner, out),
            HirMatcher::Dict(shape) => self.matcher_path_facts(target, shape, out),
            HirMatcher::Type { nominal, shape, .. } => {
                let named = self.ctx.matcher_type_decl(matcher).filter(|(_, d)| !self.ctx.hir.is_trait(d.id));
                if let Some((stmt, _)) = named.filter(|_| *nominal) {
                    out.push(NarrowFact::Tag(target.clone(), TypeTag::Concrete(stmt)));
                }
                if let Some(shape) = shape {
                    self.matcher_path_facts(target, shape, out);
                }
            },
            HirMatcher::Shape { fields, .. } => for field in fields {
                let HirLiteral::String(text) = &field.key else { continue };
                let Some(name) = self.ctx.hir.symbol_of(text) else { continue };
                let at = target.child(PathStep::Field(name));
                let owed = self.ctx.resolved().admitted_obligations(&field.value);
                out.push(NarrowFact::Owes(at.clone(), owed));
                out.extend(self.discharges_at(&at, &field.value));
                self.matcher_path_facts(&at, &field.value, out);
            },
            HirMatcher::And(..) | HirMatcher::Or(..)
            | HirMatcher::Wildcard | HirMatcher::Literal(..) | HirMatcher::Binder(..)
            | HirMatcher::Array(..) => {},
        }
    }

    pub(super) fn narrow_match_negative_branch(&self, scrutinee: &HirId<HirExpr>, matcher: &HirId<HirMatcher>) -> Vec<NarrowFact> {
        let Some(target) = self.narrow_target(scrutinee) else { return Vec::new() };
        self.ctx.obligations_ruled_out_by_matcher(matcher, &self.owed_at(&target)).iter()
            .map(|obligation| NarrowFact::Discharge(target.clone(), *obligation))
            .collect()
    }

    pub(super) fn apply_narrowings(&mut self, narrowings: &[NarrowFact]) {
        for fact in narrowings {
            match fact {
                NarrowFact::Discharge(target, obligation) => self.discharge(target.clone(), *obligation),
                NarrowFact::Tag(target, tag) => self.set_tag(target, tag.clone()),
                NarrowFact::Owes(target, obligations) => self.add_owed(target, obligations),
            }
        }
    }

    pub(super) fn safe_access_clean_facts(&self, path: &HirId<HirExpr>) -> Vec<NarrowFact> {
        self.ctx.hir.safe_access_checked_steps(path).iter()
            .filter_map(|base| self.narrow_target(base))
            .flat_map(|target| self.owed_at(&target).into_iter().map(move |o| NarrowFact::Discharge(target.clone(), o)))
            .collect()
    }

    pub(super) fn narrow_branch<R>(&mut self, facts: &[NarrowFact], f: impl FnOnce(&mut Self) -> R) -> R {
        let pre = self.narrow_after_snapshot(facts);
        let r = f(self);
        self.roll_back_flow(&pre);
        r
    }

    /// Applies `facts`, returning the flow from before them.
    pub(super) fn narrow_after_snapshot(&mut self, facts: &[NarrowFact]) -> FlowSnapshot {
        self.record_owed_before_narrowing(facts);
        let pre = self.snapshot();
        self.apply_narrowings(facts);
        pre
    }

    fn record_owed_before_narrowing(&mut self, facts: &[NarrowFact]) {
        let unrecorded: Vec<NarrowTarget> = facts.iter()
            .map(|fact| match fact {
                NarrowFact::Discharge(target, _)
                | NarrowFact::Tag(target, _)
                | NarrowFact::Owes(target, _) => target.clone(),
            })
            .filter(|target| self.proven_at(target.root).get(&target.path).is_none_or(|f| f.owed.is_none()))
            .collect();
        for target in unrecorded {
            let owed = self.owed_at(&target);
            self.update_proven(&target, |facts| facts.owed = Some(owed));
        }
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


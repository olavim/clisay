//! Where a binding lives and how long it lasts: the locals stack, function frames, the binders a
//! condition or arm introduces, and what a branch saves and merges back.

use std::collections::HashMap;

use crate::middle::hir::{Capability, HirExpr, HirId, HirMatchArm, HirParam, HirStmt, Symbol};
use crate::middle::obligations::Obligations;
use crate::middle::signatures::{Mutability, TypeTag};

use super::narrow::whole_value_binders;
use super::{ElementKey, MovedAt};
use super::{Checker, Local};

/// The binders a condition or match arm introduces, paired with the obligations each owes.
#[derive(Default)]
pub(super) struct BinderScope {
    pub(super) names: Vec<Symbol>,
    pub(super) owed: HashMap<Symbol, Obligations>,
    /// The slot each binder was destructured out of.
    pub(super) sources: HashMap<Symbol, usize>,
}

/// The flow state of one local.
#[derive(Clone)]
pub(super) struct LocalFlow {
    pub(super) assigned: bool,
    pub(super) tag: TypeTag,
    pub(super) mutability: Mutability,
    pub(super) move_site: Option<MovedAt>,
    pub(super) provenance: Vec<usize>,
    pub(super) extracted_from: Option<(usize, Option<ElementKey>)>,
    pub(super) handled: Obligations,
    pub(super) discharged: Obligations,
    pub(super) field_discharged: HashMap<Symbol, Obligations>,
}

/// A snapshot of flow facts that branches widen back at a join.
#[derive(Clone)]
pub(super) struct FlowSnapshot {
    pub(super) locals: Vec<LocalFlow>,
    pub(super) this_narrowed: HashMap<Symbol, Obligations>,
}


impl<'a> Checker<'a> {
    pub(super) fn frame_index_of(&self, name: Symbol) -> Option<usize> {
        self.locals[self.frame_start..].iter().rposition(|l| l.name == name)
            .map(|i| self.frame_start + i)
    }

    /// The slot a name resolves to outside the current frame, which would get captured
    /// if a nested function reads it.
    pub(super) fn enclosing_index(&self, name: Symbol) -> Option<usize> {
        self.locals[..self.frame_start].iter().rposition(|l| l.name == name)
    }

    pub(super) fn close_scope(&mut self, mark: usize, at: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let dropped = self.check_dropped(mark, at);
        self.truncate_locals(mark);
        dropped
    }

    /// Drops every local a scope introduced.
    pub(super) fn truncate_locals(&mut self, mark: usize) {
        self.revive_scoped_sources(mark);
        self.locals.truncate(mark);
    }

    /// Declares a scope's binders as immutable locals, each owing what the scope recorded for it.
    pub(super) fn push_binders(&mut self, scope: &BinderScope) {
        for &name in &scope.names {
            let mut local = Local::binder_owing(name, scope.owed.get(&name).cloned().unwrap_or_default());
            // The matcher shape could name which element this is, but it does not have to: the
            // runtime slot tells one element from another by identity.
            local.alias.extracted_from = scope.sources.get(&name).map(|&source| (source, None));
            self.locals.push(local);
        }
    }

    /// Declares a scope's binders as immutable locals for the duration of `f`, then drops them.
    pub(super) fn with_binders<T>(&mut self, scope: &BinderScope, at: &HirId<HirExpr>, f: impl FnOnce(&mut Self) -> Result<T, anyhow::Error>) -> Result<T, anyhow::Error> {
        let mark = self.locals.len();
        self.push_binders(scope);
        let r = f(self);
        match r.is_ok() {
            true => self.close_scope(mark, at)?,
            false => self.truncate_locals(mark),
        }
        r
    }

    /// The binders a `~` condition introduces, each owing the witnesses of a bindingless alternative
    /// sharing its or-group.
    pub(super) fn condition_scope(&self, cond: &HirId<HirExpr>) -> Result<BinderScope, anyhow::Error> {
        Ok(BinderScope {
            names: self.hir.condition_binders(cond),
            owed: self.condition_witness_obligations(cond)?,
            sources: self.binder_sources(cond),
        })
    }

    /// The slot each of a condition's binders was destructured out of, for the mutable ones. An
    /// immutable scrutinee has no writer slot to hand out.
    pub(super) fn binder_sources(&self, cond: &HirId<HirExpr>) -> HashMap<Symbol, usize> {
        self.hir.condition_binder_sources(cond).into_iter()
            .filter_map(|(name, scrutinee)| {
                let source = self.local_of(&scrutinee).filter(|&i| self.holds_mutable(i))?;
                Some((name, source))
            })
            .collect()
    }

    /// The binders a parameter's pattern introduces, each owing the witnesses on its or-path plus
    /// whatever its field declares. A parameter with no pattern introduces none.
    pub(super) fn param_scope(&self, param: &HirParam) -> Result<BinderScope, anyhow::Error> {
        let Some(pattern) = &param.pattern else { return Ok(BinderScope::default()) };
        Ok(BinderScope {
            names: self.hir.get(pattern).binders(self.hir),
            owed: self.matcher_witness_obligations(pattern, &param.name)?,
            // A parameter is lent for the call, and a borrow hands out no writer slot.
            sources: HashMap::new(),
        })
    }

    /// The binders a match arm introduces: its matcher binders and any guard binders. A whole-value
    /// binder owes what the scrutinee still owes. A destructure binder owes the witnesses on its
    /// or-path, as in `Node { next } | null`.
    pub(super) fn arm_scope(&self, arm: &HirMatchArm, remaining: &Obligations, at: &HirId<HirStmt>, scrutinee: &HirId<HirExpr>) -> Result<BinderScope, anyhow::Error> {
        let whole = whole_value_binders(self.hir, &arm.matcher);
        let witness = self.matcher_witness_obligations(&arm.matcher, at)?;
        let mut names = self.hir.get(&arm.matcher).binders(self.hir);
        if let Some(guard) = &arm.guard {
            names.extend(self.hir.condition_binders(guard));
        }
        let owed = names.iter().map(|&name| {
            let mut set = if whole.contains(&name) { remaining.clone() } else { Obligations::new() };
            if let Some(obligations) = witness.get(&name) { set.extend(obligations); }
            (name, set)
        }).collect();
        // A matcher binder names part of the scrutinee. A guard's binders name part of whatever
        // that guard matched, which it knows itself.
        let mut sources: HashMap<Symbol, usize> = HashMap::new();
        if let Some(source) = self.local_of(scrutinee).filter(|&i| self.holds_mutable(i)) {
            sources.extend(self.hir.get(&arm.matcher).binders(self.hir).into_iter().map(|name| (name, source)));
        }
        if let Some(guard) = &arm.guard {
            sources.extend(self.binder_sources(guard));
        }
        Ok(BinderScope { names, owed, sources })
    }

    /// Runs `body` in a fresh function frame whose locals are the given params. `frame_start` is
    /// moved past the enclosing locals so value reads do not cross into them, then restored.
    pub(super) fn with_frame<R>(&mut self, params: &[HirParam], at: &HirId<HirExpr>, body: impl FnOnce(&mut Self) -> Result<R, anyhow::Error>) -> Result<R, anyhow::Error> {
        let saved_frame = self.frame_start;
        let mark = self.locals.len();
        self.frame_start = mark;
        let saved_this_narrowed = std::mem::take(&mut self.this_narrowed);

        for param in params {
            let name = self.hir.ident_sym(&param.name);
            let mut owed = param.clause.owed();

            // A witness alternative is the real obligation, so `x @ Node | null` owes `opt` exactly
            // as `x: opt` does.
            if let Some(pattern) = &param.pattern {
                owed.extend(self.sigs.admitted_obligations(self.hir, self.bindings, pattern));
            }

            let mut local = Local::param(name, owed, param.mutable);
            local.container = param.clause.container;
            local.param = true;
            local.alias.mutability = Mutability::param(param.clause.capability);
            // A plain `mut` parameter borrows its argument; `*mut` owns it.
            local.alias.borrowed = param.clause.capability == Capability::Mut;
            local.site = Some(param.name);

            // A pattern tests the argument on entry, which is a discharge of the slot it names.
            if param.pattern.is_some() {
                local.handled = local.owed.clone();
            }

            self.locals.push(local);
        }

        // A pattern's binders live for the whole body, so they are pushed beside the parameters
        // rather than scoped to a branch.
        for param in params {
            let scope = self.param_scope(param)?;
            self.push_binders(&scope);
        }

        let result = body(self);
        // A body that already failed has nothing to say about undischarged bindings.
        let dropped = match result.is_ok() {
            true => self.check_dropped(mark, at),
            false => Ok(()),
        };

        self.truncate_locals(mark);
        self.frame_start = saved_frame;
        self.this_narrowed = saved_this_narrowed;
        dropped?;
        result
    }

    pub(super) fn snapshot(&self) -> FlowSnapshot {
        FlowSnapshot {
            locals: self.locals.iter().map(|l| LocalFlow {
                assigned: l.assigned,
                tag: l.tag.clone(),
                mutability: l.alias.mutability,
                move_site: l.alias.move_site,
                provenance: l.alias.provenance.clone(),
                extracted_from: l.alias.extracted_from,
                handled: l.handled.clone(),
                discharged: l.discharged.clone(),
                field_discharged: l.field_discharged.clone(),
            }).collect(),
            this_narrowed: self.this_narrowed.clone(),
        }
    }

    pub(super) fn restore(&mut self, flow: &FlowSnapshot) {
        self.restore_keeping_moves(flow);
        for (local, snap) in self.locals.iter_mut().zip(&flow.locals) {
            local.alias.move_site = snap.move_site;
            local.alias.provenance = snap.provenance.clone();
            local.alias.extracted_from = snap.extracted_from;
        }
    }

    /// Restores flow but keeps each local's move site and give-back sources.
    pub(super) fn restore_keeping_moves(&mut self, flow: &FlowSnapshot) {
        for (local, snap) in self.locals.iter_mut().zip(&flow.locals) {
            local.assigned = snap.assigned;
            local.tag = snap.tag.clone();
            local.alias.mutability = snap.mutability;
            local.alias.move_site = local.alias.move_site.or(snap.move_site);
            local.handled = snap.handled.clone();
            local.discharged = snap.discharged.clone();
            local.field_discharged = snap.field_discharged.clone();
        }
        self.this_narrowed = flow.this_narrowed.clone();
    }

    /// Puts each local's narrowings back.
    pub(super) fn restore_narrowings(&mut self, flow: &FlowSnapshot) {
        for (local, snap) in self.locals.iter_mut().zip(&flow.locals) {
            local.discharged = snap.discharged.clone();
            local.field_discharged = snap.field_discharged.clone();
        }
    }

    /// Merges another branch's end state into the current local flow.
    pub(super) fn join_in(&mut self, other: &FlowSnapshot) {
        for (local, o) in self.locals.iter_mut().zip(&other.locals) {
            local.assigned = local.assigned && o.assigned;
            local.tag = if local.tag == o.tag { local.tag.clone() } else { TypeTag::Unknown };
            local.alias.mutability = if local.alias.mutability == o.mutability { local.alias.mutability } else { Mutability::Unknown };
            local.alias.move_site = local.alias.move_site.or(o.move_site);
            local.alias.provenance.retain(|s| o.provenance.contains(s));
            local.handled.retain(|ob| o.handled.contains(ob));
        }
    }

    /// Merges two branch snapshots.
    pub(super) fn join(&mut self, then_snap: &FlowSnapshot, else_snap: &FlowSnapshot) {
        debug_assert!(then_snap.locals.len() == self.locals.len() && else_snap.locals.len() == self.locals.len());
        for (i, local) in self.locals.iter_mut().enumerate() {
            let (then_local, else_local) = (&then_snap.locals[i], &else_snap.locals[i]);
            local.assigned = then_local.assigned && else_local.assigned;
            local.tag = if then_local.tag == else_local.tag { then_local.tag.clone() } else { TypeTag::Unknown };
            local.alias.mutability = if then_local.mutability == else_local.mutability
                { then_local.mutability } else
                { Mutability::Unknown };
            local.alias.move_site = then_local.move_site.or(else_local.move_site);
            local.alias.provenance = then_local.provenance.iter().copied().filter(|s| else_local.provenance.contains(s)).collect();
            local.handled = then_local.handled.intersection(&else_local.handled).copied().collect();
        }
    }
}

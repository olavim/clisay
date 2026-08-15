//! Where a binding lives and how long it lasts: the locals stack, function frames, the binders a
//! condition or arm introduces, and what a branch saves and merges back.

use std::collections::HashMap;

use crate::middle::hir::{HirExpr, HirId, HirMatchArm, HirParam, HirStmt, Symbol};
use crate::middle::obligations::Obligations;
use crate::middle::signatures::{Mutability, TypeTag};

use super::narrow::whole_value_binders;
use super::alias::WriteOwnershipTransfer;
use super::{ElementKey, TransferSite};
use super::{Checker, Local};

/// The binders a condition or match arm introduces, paired with the obligations each owes.
#[derive(Default)]
pub(super) struct BinderScope {
    pub(super) names: Vec<Symbol>,
    pub(super) owed: HashMap<Symbol, Obligations>,
    /// The slot each binder was destructured out of.
    pub(super) sources: HashMap<Symbol, usize>,
    /// The node `bind` declared each binder from.
    pub(super) decls: HashMap<Symbol, usize>,
}

/// The flow state of one local.
#[derive(Clone, PartialEq)]
pub struct LocalFlow {
    pub assigned: bool,
    pub tag: TypeTag,
    pub mutability: Mutability,
    pub transfer_site: Option<TransferSite>,
    pub provenance: Vec<usize>,
    pub extracted_from: Vec<(usize, Option<ElementKey>)>,
    pub handled: Obligations,
    pub discharged: Obligations,
    pub field_discharged: HashMap<Symbol, Obligations>,
}

/// A snapshot of flow facts that branches widen back at a join.
#[derive(Clone)]
pub(super) struct FlowSnapshot {
    pub(super) locals: Vec<LocalFlow>,
    pub(super) this_narrowed: HashMap<Symbol, Obligations>,
}


impl<'a> Checker<'a> {
    /// The locals of the current frame.
    pub(super) fn frame_locals(&self) -> &[Local] {
        &self.locals[self.frame_start..]
    }

    pub(super) fn frame_locals_mut(&mut self) -> &mut [Local] {
        &mut self.locals[self.frame_start..]
    }

    pub(super) fn frame_index_of(&self, name: Symbol) -> Option<usize> {
        self.frame_locals().iter().rposition(|l| l.name == name)
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
        self.reclaim_scoped_write_ownership(mark);
        self.reroot_provenance(mark);
        self.drop_dead_extractions(mark);
        self.locals.truncate(mark);
    }

    /// Declares a scope's binders as locals that cannot be reassigned, each owing what the scope
    /// recorded for it.
    pub(super) fn push_binders(&mut self, scope: &BinderScope) {
        for &name in &scope.names {
            let mut local = Local::binder_owing(name, scope.owed.get(&name).cloned().unwrap_or_default());
            local.decl = scope.decls.get(&name).copied();
            // The matcher shape could name which element this is, but it does not have to: the
            // runtime slot tells one element from another by identity.
            local.alias.extracted_from = scope.sources.get(&name).map(|&source| (source, None)).into_iter().collect();
            self.locals.push(local);
        }
    }

    /// Declares a scope's binders for the duration of `f`, then drops them.
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
        let names = self.hir.condition_binders(cond);
        Ok(BinderScope {
            decls: names.iter().map(|&name| (name, cond.index())).collect(),
            names,
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
        let names = self.hir.get(pattern).binders(self.hir);
        Ok(BinderScope {
            decls: names.iter().map(|&name| (name, pattern.index())).collect(),
            names,
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
        let mut decls: HashMap<Symbol, usize> = names.iter().map(|&n| (n, arm.matcher.index())).collect();
        if let Some(guard) = &arm.guard {
            let guard_names = self.hir.condition_binders(guard);
            decls.extend(guard_names.iter().map(|&n| (n, guard.index())));
            names.extend(guard_names);
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
        Ok(BinderScope { names, owed, sources, decls })
    }

    /// Runs `body` in a fresh function frame whose locals are the given params. `frame_start` is
    /// moved past the enclosing locals so value reads do not cross into them, then restored.
    pub(super) fn with_frame<R>(&mut self, params: &[HirParam], at: &HirId<HirExpr>, body: impl FnOnce(&mut Self) -> Result<R, anyhow::Error>) -> Result<R, anyhow::Error> {
        let saved_frame = self.frame_start;
        let mark = self.locals.len();
        self.frame_start = mark;
        let saved_this_narrowed = std::mem::take(&mut self.this_narrowed);

        for (position, param) in params.iter().enumerate() {
            let name = self.hir.ident_sym(&param.name);
            let mut owed = param.clause.owed();

            // A witness alternative is the real obligation, so `x @ Node | null` owes `opt` exactly
            // as `x: opt` does.
            if let Some(pattern) = &param.pattern {
                owed.extend(self.resolved().admitted_obligations(pattern));
            }

            let mut local = Local::param(name, owed, param.reassignable);
            local.decl = Some(param.name.index());
            local.container = param.clause.container;
            local.param = true;
            local.alias.mutability = Mutability::param(param.clause.capability);
            local.alias.borrowed = !param.clause.capability.is_retain();
            local.alias.unproven_borrow = !param.clause.capability.is_mut() && !param.clause.capability.is_retain();
            local.alias.confined = self.fn_ctx.param_confined.get(position).copied().unwrap_or(false);
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
            locals: self.locals.iter().map(flow_of).collect(),
            this_narrowed: self.this_narrowed.clone(),
        }
    }

    /// Puts flow back exactly as the snapshot had it.
    pub(super) fn restore(&mut self, flow: &FlowSnapshot) {
        for (local, snap) in self.locals.iter_mut().zip(&flow.locals) {
            restore_flow(local, snap);
        }
        self.this_narrowed = flow.this_narrowed.clone();
    }

    /// Restores flow but keeps each local's move site and give-back sources. A branch that ran
    /// still moved what it moved, whatever the restore says.
    pub(super) fn restore_keeping_moves(&mut self, flow: &FlowSnapshot) {
        for (local, snap) in self.locals.iter_mut().zip(&flow.locals) {
            let LocalFlow {
                assigned, tag, mutability, transfer_site, provenance: _kept,
                extracted_from: _also_kept, handled, discharged, field_discharged,
            } = snap;
            local.assigned = *assigned;
            local.tag = tag.clone();
            local.alias.mutability = *mutability;
            local.alias.transfer_site = local.alias.transfer_site.or(*transfer_site);
            local.handled = handled.clone();
            local.discharged = discharged.clone();
            local.field_discharged = field_discharged.clone();
        }
        self.this_narrowed = flow.this_narrowed.clone();
    }

    /// Keeps only the narrowings the snapshot also had, leaving every other fact where it is.
    pub(super) fn restore_narrowings(&mut self, flow: &FlowSnapshot) {
        for (local, snap) in self.locals.iter_mut().zip(&flow.locals) {
            let LocalFlow {
                assigned: _, tag: _, mutability: _, transfer_site: _, provenance: _,
                extracted_from: _, handled: _, discharged, field_discharged,
            } = snap;
            local.discharged.retain(|ob| discharged.contains(ob));
            intersect_narrowings(&mut local.field_discharged, field_discharged);
        }
    }

    /// Merges one outcome's end state into the current flow. Every join is a fold of this, so a
    /// field it fails to merge is one outcome's fact surviving as if all of them proved it.
    pub(super) fn join_in(&mut self, other: &FlowSnapshot) {
        debug_assert!(other.locals.len() == self.locals.len());
        for (local, snap) in self.locals.iter_mut().zip(&other.locals) {
            let mut merged = flow_of(local);
            merge_flow(&mut merged, &local.owed, snap);
            restore_flow(local, &merged);
        }
        intersect_narrowings(&mut self.this_narrowed, &other.this_narrowed);
    }
}

/// The flow facts a local carries right now.
pub(super) fn flow_of(local: &Local) -> LocalFlow {
    LocalFlow {
        assigned: local.assigned,
        tag: local.tag.clone(),
        mutability: local.alias.mutability,
        transfer_site: local.alias.transfer_site,
        provenance: local.alias.provenance.clone(),
        extracted_from: local.alias.extracted_from.clone(),
        handled: local.handled.clone(),
        discharged: local.discharged.clone(),
        field_discharged: local.field_discharged.clone(),
    }
}

/// Puts flow facts onto a local. The inverse of `flow_of`, and the pair has to round-trip.
pub(super) fn restore_flow(local: &mut Local, flow: &LocalFlow) {
    let LocalFlow {
        assigned, tag, mutability, transfer_site, provenance,
        extracted_from, handled, discharged, field_discharged,
    } = flow;
    local.assigned = *assigned;
    local.tag = tag.clone();
    local.alias.mutability = *mutability;
    local.alias.transfer_site = *transfer_site;
    local.alias.provenance = provenance.clone();
    local.alias.extracted_from = extracted_from.clone();
    local.handled = handled.clone();
    local.discharged = discharged.clone();
    local.field_discharged = field_discharged.clone();
}

/// Merges one outcome into another, for a single local.
pub fn merge_flow(into: &mut LocalFlow, owed: &Obligations, other: &LocalFlow) {
    let LocalFlow {
        assigned, tag, mutability, transfer_site, provenance,
        extracted_from, handled, discharged, field_discharged,
    } = other;
    into.assigned = into.assigned && *assigned;
    into.tag = if into.tag == *tag { into.tag.clone() } else { TypeTag::Unknown };
    into.mutability = if into.mutability == *mutability { into.mutability } else { Mutability::Unknown };
    into.transfer_site = merge_transfer_sites(into.transfer_site, *transfer_site);
    // Either branch could have run, so the binding may have come out of any origin either of them
    // named. An origin is a restriction, so the join keeps them all.
    for origin in extracted_from {
        if !into.extracted_from.contains(origin) {
            into.extracted_from.push(*origin);
        }
    }
    // A source is where the write-ownership goes back when the binding dies, and the join above
    // keeps a move either branch made.
    for source in provenance {
        if !into.provenance.contains(source) {
            into.provenance.push(*source);
        }
    }
    // An outcome resolves an obligation either by handling it or by proving the value is not in its
    // bad state. Only what every outcome resolved survives the join.
    let both: Obligations = owed.iter().copied()
        .filter(|ob| (into.handled.contains(ob) || into.discharged.contains(ob))
            && (handled.contains(ob) || discharged.contains(ob)))
        .collect();
    into.handled = both;
    into.discharged.retain(|ob| discharged.contains(ob));
    intersect_narrowings(&mut into.field_discharged, field_discharged);
}

/// Merges the move sites of two outcomes.
fn merge_transfer_sites(into: Option<TransferSite>, other: Option<TransferSite>) -> Option<TransferSite> {
    match (into, other) {
        (None, site) | (site, None) => site,
        (Some(a), Some(b)) if a == b => Some(a),
        (Some(a), Some(b)) => {
            // The lower node keeps the blame, so the answer does not depend on which outcome the
            // caller restored first.
            let blame = if a.node.index() <= b.node.index() { a.node } else { b.node };
            Some(TransferSite { node: blame, transfer: WriteOwnershipTransfer::Transferred })
        },
    }
}

/// Keeps only the field narrowings both sides proved. A field the other side says nothing about
/// was not narrowed there, so it does not survive.
pub fn intersect_narrowings(into: &mut HashMap<Symbol, Obligations>, other: &HashMap<Symbol, Obligations>) {
    into.retain(|field, obligations| match other.get(field) {
        Some(theirs) => {
            obligations.retain(|ob| theirs.contains(ob));
            !obligations.is_empty()
        },
        None => false,
    });
}

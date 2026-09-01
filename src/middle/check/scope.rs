//! Where a binding lives and how long it lasts: the locals stack, function frames, the binders a
//! condition or arm introduces, and what a branch saves and merges back.

use std::collections::{HashMap, HashSet};

use crate::middle::hir::{HirExpr, HirId, HirMatchArm, HirMatcher, HirParam, HirStmt, Symbol};
use crate::middle::obligations::Obligations;
use crate::middle::signatures::{CallableId, Mutability, TypeTag};

use super::narrow::collect_whole_value_binders;
use super::alias::WriteOwnershipTransfer;
use super::{PatternBinderSource, Checker, Ctx, ElementKey, Local, TransferSite};

#[derive(Default)]
pub(super) struct PatternBinderScope {
    pub(super) names: Vec<Symbol>,
    pub(super) owed: HashMap<Symbol, Obligations>,
    /// The slot each binder was destructured out of.
    pub(super) sources: HashMap<Symbol, usize>,
    /// The node `bind` declared each binder from.
    pub(super) decls: HashMap<Symbol, usize>,
    /// The binders that read as dynamic-boundary values, no test having proved what they hold.
    pub(super) unknown: HashSet<Symbol>,
    pub(super) mutability: Mutability,
    pub(super) reassignable: bool,
    pub(super) source: PatternBinderSource,
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
    pub discharged: Obligations,
    pub field_discharged: HashMap<Symbol, Obligations>,
    pub resolved_callable: Option<CallableId>,
}

/// A snapshot of flow facts that branches widen back at a join.
#[derive(Clone)]
pub(super) struct FlowSnapshot {
    pub(super) locals: Vec<LocalFlow>,
    pub(super) this_narrowed: HashMap<Symbol, Obligations>,
}


impl<'a> Ctx<'a> {
    pub(super) fn param_pattern_binders(&self, param: &HirParam) -> Result<PatternBinderScope, anyhow::Error> {
        let Some(pattern) = &param.pattern else { return Ok(PatternBinderScope::default()) };
        self.pattern_binders(pattern, &param.name, Mutability::param(param.clause.capability), false, PatternBinderSource::Param)
    }

    fn pattern_binders(&self, pattern: &HirId<HirMatcher>, at: &HirId<HirExpr>, mutability: Mutability, reassignable: bool, source: PatternBinderSource) -> Result<PatternBinderScope, anyhow::Error> {
        let names = self.hir.get(pattern).binders(self.hir);
        Ok(PatternBinderScope {
            decls: names.iter().map(|&name| (name, pattern.index())).collect(),
            names,
            owed: self.collect_matcher_witnessed_obligations(pattern, at)?,
            unknown: self.collect_matcher_unknown_binders(pattern),
            sources: HashMap::new(),
            mutability,
            reassignable,
            source,
        })
    }

    pub(super) fn say_pattern_binders(&self, pattern: &HirId<HirMatcher>, value: &HirId<HirExpr>, mutability: Mutability, reassignable: bool) -> Result<PatternBinderScope, anyhow::Error> {
        self.pattern_binders(pattern, value, mutability, reassignable, PatternBinderSource::Say)
    }
}

impl<'a> Checker<'a> {
    pub(super) fn frame_locals(&self) -> &[Local] {
        &self.locals[self.frame_start..]
    }

    pub(super) fn frame_index_of(&self, name: Symbol) -> Option<usize> {
        self.frame_locals().iter().rposition(|l| l.name == name)
            .map(|i| self.frame_start + i)
    }

    pub(super) fn upvalue_index(&self, name: Symbol) -> Option<usize> {
        self.locals[..self.frame_start].iter().rposition(|l| l.name == name)
    }

    pub(super) fn close_scope(&mut self, mark: usize, at: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let dropped = self.check_dropped(mark, at);
        self.truncate_locals(mark);
        dropped
    }

    pub(super) fn truncate_locals(&mut self, mark: usize) {
        self.settle_unshared_stores(mark);
        self.reclaim_scoped_write_ownership(mark);
        self.reroot_provenance(mark);
        self.drop_dead_extractions(mark);
        self.locals.truncate(mark);
    }

    pub(super) fn push_binders(&mut self, scope: &PatternBinderScope) {
        for &name in &scope.names {
            let mut local = Local::binder_owing(name, scope.owed.get(&name).cloned().unwrap_or_default(), scope.source);
            local.decl = scope.decls.get(&name).copied();
            local.alias.extracted_from = scope.sources.get(&name).map(|&source| (source, None)).into_iter().collect();
            local.alias.mutability = scope.mutability;
            local.unknown = scope.unknown.contains(&name);
            local.reassignable = scope.reassignable;
            self.locals.push(local);
        }
    }

    pub(super) fn with_binders<T>(&mut self, scope: &PatternBinderScope, at: &HirId<HirExpr>, f: impl FnOnce(&mut Self) -> Result<T, anyhow::Error>) -> Result<T, anyhow::Error> {
        let mark = self.locals.len();
        self.push_binders(scope);
        let r = f(self);
        match r.is_ok() {
            true => self.close_scope(mark, at)?,
            false => self.truncate_locals(mark),
        }
        r
    }

    pub(super) fn condition_pattern_binders(&self, cond: &HirId<HirExpr>) -> Result<PatternBinderScope, anyhow::Error> {
        let names = self.ctx.hir.condition_pattern_binders(cond);
        Ok(PatternBinderScope {
            decls: names.iter().map(|&name| (name, cond.index())).collect(),
            names,
            owed: self.ctx.collect_condition_witness_obligations(cond)?,
            unknown: self.ctx.collect_condition_unknown_binders(cond),
            sources: self.mutable_condition_pattern_binder_sources(cond),
            mutability: Mutability::Unknown,
            reassignable: false,
            source: PatternBinderSource::Condition,
        })
    }

    pub(super) fn mutable_condition_pattern_binder_sources(&self, cond: &HirId<HirExpr>) -> HashMap<Symbol, usize> {
        self.ctx.hir.condition_pattern_binder_sources(cond).into_iter()
            .filter_map(|(name, scrutinee)| {
                let source = self.local_of(&scrutinee).filter(|&i| self.holds_mutable(i))?;
                Some((name, source))
            })
            .collect()
    }

    pub(super) fn match_arm_binders(&self, arm: &HirMatchArm, remaining: &Obligations, at: &HirId<HirStmt>, scrutinee: &HirId<HirExpr>) -> Result<PatternBinderScope, anyhow::Error> {
        let whole = collect_whole_value_binders(self.ctx.hir, &arm.matcher);
        let witness = self.ctx.collect_matcher_witnessed_obligations(&arm.matcher, at)?;
        let mut names = self.ctx.hir.get(&arm.matcher).binders(self.ctx.hir);
        let mut decls: HashMap<Symbol, usize> = names.iter().map(|&n| (n, arm.matcher.index())).collect();
        if let Some(guard) = &arm.guard {
            let guard_names = self.ctx.hir.condition_pattern_binders(guard);
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
            sources.extend(self.ctx.hir.get(&arm.matcher).binders(self.ctx.hir).into_iter().map(|name| (name, source)));
        }
        if let Some(guard) = &arm.guard {
            sources.extend(self.mutable_condition_pattern_binder_sources(guard));
        }
        let mut unknown = self.ctx.collect_matcher_unknown_binders(&arm.matcher);
        if let Some(guard) = &arm.guard {
            unknown.extend(self.ctx.collect_condition_unknown_binders(guard));
        }
        Ok(PatternBinderScope { names, owed, sources, decls, unknown, mutability: Mutability::Unknown, reassignable: false, source: PatternBinderSource::Arm })
    }

    pub(super) fn with_frame<R>(&mut self, params: &[HirParam], at: &HirId<HirExpr>, body: impl FnOnce(&mut Self) -> Result<R, anyhow::Error>) -> Result<R, anyhow::Error> {
        let saved_frame = self.frame_start;
        let mark = self.locals.len();
        self.frame_start = mark;
        let saved_this_narrowed = std::mem::take(&mut self.this_narrowed);

        for (position, param) in params.iter().enumerate() {
            let name = self.ctx.hir.ident_sym(&param.name);
            let mut owed = param.clause.owed();

            // A witness alternative is the real obligation, so `x @ Node | null` owes `opt`.
            if let Some(pattern) = &param.pattern {
                owed.extend(self.ctx.resolved().admitted_obligations(pattern));
            }

            let mut local = Local::param(name, owed, param.reassignable);
            local.decl = Some(param.name.index());
            local.container = param.clause.container;
            local.param = true;
            local.alias.mutability = Mutability::param(param.clause.capability);
            local.alias.borrowed = !param.clause.capability.is_retain();
            local.alias.borrowed_maybe_mutable = !param.clause.capability.is_mut() && !param.clause.capability.is_retain();
            local.alias.confined = self.fn_ctx.param_confined.get(position).copied().unwrap_or(false);
            local.site = Some(param.name);

            if param.pattern.is_some() {
                local = local.as_used();
            }

            self.locals.push(local);
        }

        // A pattern's binders live for the whole body, so they are pushed beside the parameters
        // rather than scoped to a branch.
        for param in params {
            let scope = self.ctx.param_pattern_binders(param)?;
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
            locals: self.locals.iter().map(local_flow_of).collect(),
            this_narrowed: self.this_narrowed.clone(),
        }
    }

    /// Puts flow back exactly as the snapshot had it.
    pub(super) fn restore_flow(&mut self, flow: &FlowSnapshot) {
        for (local, snap) in self.locals.iter_mut().zip(&flow.locals) {
            restore_local_flow(local, snap);
        }
        self.this_narrowed = flow.this_narrowed.clone();
    }

    pub(super) fn restore_flow_keeping_write_ownership_transfers(&mut self, flow: &FlowSnapshot) {
        for (local, snap) in self.locals.iter_mut().zip(&flow.locals) {
            let LocalFlow {
                assigned, tag, mutability, transfer_site, provenance: _kept,
                extracted_from: _also_kept, discharged, field_discharged, resolved_callable,
            } = snap;

            if local.resolved_callable != *resolved_callable {
                local.resolved_callable = None;
            }

            local.assigned = *assigned;
            local.tag = tag.clone();
            local.alias.mutability = *mutability;
            local.alias.transfer_site = local.alias.transfer_site.or(*transfer_site);
            local.discharged = discharged.clone();
            local.field_discharged = field_discharged.clone();
        }
        self.this_narrowed = flow.this_narrowed.clone();
    }

    pub(super) fn restore_narrowings(&mut self, flow: &FlowSnapshot) {
        for (local, snap) in self.locals.iter_mut().zip(&flow.locals) {
            let LocalFlow {
                assigned: _, tag: _, mutability: _, transfer_site: _, provenance: _,
                extracted_from: _, discharged, field_discharged, resolved_callable: _,
            } = snap;
            local.discharged.retain(|ob| discharged.contains(ob));
            intersect_narrowings(&mut local.field_discharged, field_discharged);
        }
    }

    /// Merges one outcome's end state into the current flow. Every join is a fold of this, so a
    /// field it fails to merge is one outcome's fact surviving as if all of them proved it.
    pub(super) fn merge_flow_into_current(&mut self, other: &FlowSnapshot) {
        debug_assert!(other.locals.len() == self.locals.len());
        for (local, snap) in self.locals.iter_mut().zip(&other.locals) {
            let mut merged = local_flow_of(local);
            merge_local_flow(&mut merged, snap);
            restore_local_flow(local, &merged);
        }
        intersect_narrowings(&mut self.this_narrowed, &other.this_narrowed);
    }
}

pub(super) fn local_flow_of(local: &Local) -> LocalFlow {
    LocalFlow {
        assigned: local.assigned,
        tag: local.tag.clone(),
        mutability: local.alias.mutability,
        transfer_site: local.alias.transfer_site,
        provenance: local.alias.mutable_provenance.clone(),
        extracted_from: local.alias.extracted_from.clone(),
        discharged: local.discharged.clone(),
        field_discharged: local.field_discharged.clone(),
        resolved_callable: local.resolved_callable,
    }
}

pub(super) fn restore_local_flow(local: &mut Local, flow: &LocalFlow) {
    let LocalFlow {
        assigned, tag, mutability, transfer_site, provenance,
        extracted_from, discharged, field_discharged, resolved_callable,
    } = flow;
    local.assigned = *assigned;
    local.tag = tag.clone();
    local.alias.mutability = *mutability;
    local.alias.transfer_site = *transfer_site;
    local.alias.mutable_provenance = provenance.clone();
    local.alias.extracted_from = extracted_from.clone();
    local.discharged = discharged.clone();
    local.field_discharged = field_discharged.clone();
    local.resolved_callable = *resolved_callable;
}

pub fn merge_local_flow(into: &mut LocalFlow, other: &LocalFlow) {
    let LocalFlow {
        assigned, tag, mutability, transfer_site, provenance,
        extracted_from, discharged, field_discharged, resolved_callable,
    } = other;
    into.assigned = into.assigned && *assigned;
    into.tag = if into.tag == *tag { into.tag.clone() } else { TypeTag::Unknown };
    into.mutability = if into.mutability == *mutability { into.mutability } else { Mutability::Unknown };
    into.transfer_site = merge_transfer_sites(into.transfer_site, *transfer_site);

    // Either path could have run, so a name resolves only where both paths reach the same callable.
    if into.resolved_callable != *resolved_callable {
        into.resolved_callable = None;
    }

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

    into.discharged.retain(|ob| discharged.contains(ob));
    intersect_narrowings(&mut into.field_discharged, field_discharged);
}

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

pub fn intersect_narrowings(into: &mut HashMap<Symbol, Obligations>, other: &HashMap<Symbol, Obligations>) {
    into.retain(|field, obligations| match other.get(field) {
        Some(theirs) => {
            obligations.retain(|ob| theirs.contains(ob));
            !obligations.is_empty()
        },
        None => false,
    });
}

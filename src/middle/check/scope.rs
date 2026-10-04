//! Where a binding lives and how long it lasts.

use std::collections::{BTreeMap, BTreeSet, HashMap};

use crate::middle::bind::Place;
use crate::middle::hir::{access_path_steps, HirExpr, HirId, HirLiteral, HirMatchArm, HirMatcher, HirParam, HirStmt, Symbol};
use crate::middle::obligations::Obligations;
use super::ProvenFacts;
use crate::middle::signatures::CallableId;

use super::barriers::CheckedSlot;
use super::conform::MatcherFacts;
use super::narrow::collect_whole_value_binders;
use super::{CallableState, PatternBinderSource, Checker, Ctx, Local, NarrowRoot, PathMap, PossibleFacts, WriteRoot};
use crate::middle::diagnose::Diagnose;

#[derive(Default)]
pub(super) struct PatternBinderScope {
    pub(super) names: Vec<Symbol>,
    pub(super) facts: MatcherFacts,
    /// The node `bind` declared each binder from.
    pub(super) decls: HashMap<Symbol, usize>,
    pub(super) reassignable: bool,
    pub(super) source: PatternBinderSource,
}

/// Lambdas, functions and types.
pub type Callables = BTreeSet<usize>;

/// The flow state of one local.
#[derive(Clone, PartialEq)]
pub struct LocalFlow {
    pub assigned: bool,
    pub proven: PathMap<ProvenFacts>,
    pub possible: PathMap<PossibleFacts>,
    pub resolved_callable: Option<CallableId>,
}

/// What one statement or expression does, counting everything nested in it.
#[derive(Default)]
pub(super) struct SubtreeEffects {
    /// Callables used.
    pub(super) uses: Callables,
    /// Locals read.
    pub(super) reads: BTreeSet<usize>,
    /// Locals written, each with the flows after the writes were merged.
    pub(super) writes: BTreeMap<usize, LocalFlow>,
    /// What was proven about `this` after the writes to it were merged.
    pub(super) this_writes: Option<PathMap<ProvenFacts>>,
}

impl SubtreeEffects {
    fn absorb(&mut self, inner: SubtreeEffects) {
        self.uses.extend(inner.uses);
        self.reads.extend(inner.reads);
        for (i, flow) in inner.writes {
            self.note_write(i, flow);
        }
        if let Some(this) = inner.this_writes {
            self.note_this_write(this);
        }
    }

    fn note_this_write(&mut self, now: PathMap<ProvenFacts>) {
        match &mut self.this_writes {
            Some(merged) => merged.join(&now),
            None => self.this_writes = Some(now),
        }
    }

    pub(super) fn replace_uses_with(&mut self, callable: usize) -> Callables {
        std::mem::replace(&mut self.uses, Callables::from([callable]))
    }

    fn note_write(&mut self, i: usize, flow: LocalFlow) {
        match self.writes.get_mut(&i) {
            Some(merged) => merge_local_flow(merged, &flow),
            None => { self.writes.insert(i, flow); },
        }
    }
}


/// A snapshot of flow facts that branches widen back at a join.
#[derive(Clone)]
pub(super) struct FlowSnapshot {
    pub(super) locals: Vec<LocalFlow>,
    pub(super) this_narrowed: PathMap<ProvenFacts>,
}

impl FlowSnapshot {
    /// Joins the flow after each write in `effects`.
    pub(super) fn join_writes(&mut self, effects: &SubtreeEffects) {
        for (&i, flow) in &effects.writes {
            if let Some(local) = self.locals.get_mut(i) {
                merge_local_flow(local, flow);
            }
        }
        if let Some(this) = &effects.this_writes {
            self.this_narrowed.join(this);
        }
    }
}


impl<'a> Ctx<'a> {
    pub(super) fn param_pattern_binders(&self, param: &HirParam) -> Result<PatternBinderScope, anyhow::Error> {
        let Some(pattern) = &param.pattern else { return Ok(PatternBinderScope::default()) };
        self.pattern_binders(pattern, &param.name, param.reassignable, PatternBinderSource::Param)
    }

    fn pattern_binders(&self, pattern: &HirId<HirMatcher>, at: &HirId<HirExpr>, reassignable: bool, source: PatternBinderSource) -> Result<PatternBinderScope, anyhow::Error> {
        let names = self.hir.get(pattern).binders(self.hir);
        let facts = self.matcher_facts(pattern, at)?;
        Ok(PatternBinderScope {
            decls: names.iter().map(|&name| (name, pattern.index())).collect(),
            names,
            facts,
            reassignable,
            source,
        })
    }

    pub(super) fn say_pattern_binders(&self, pattern: &HirId<HirMatcher>, value: &HirId<HirExpr>, reassignable: bool) -> Result<PatternBinderScope, anyhow::Error> {
        self.pattern_binders(pattern, value, reassignable, PatternBinderSource::Say)
    }
}

impl<'a> Checker<'a> {
    pub(super) fn frame_locals(&self) -> &[Local] {
        &self.locals[self.frame_start..]
    }

    /// Where the write path `target` starts, as `a` in `a.b[0]` or `this` in `this!.b`.
    pub(super) fn write_root(&self, target: &HirId<HirExpr>) -> WriteRoot {
        let (root, _) = access_path_steps(self.ctx.hir, target);
        let is_this = match self.ctx.hir.get(&root) {
            HirExpr::Identifier(_) => false,
            HirExpr::This => true,
            HirExpr::RefValue { .. } => return WriteRoot::Ref,
            _ => return WriteRoot::Value,
        };
        match self.ctx.bindings.place_of(&root) {
            Some(Place::Local(_)) if is_this => WriteRoot::Receiver,
            Some(Place::Local(_)) => match self.local_read_at(&root) {
                Some(i) if self.locals[i].is_anchor => WriteRoot::Anchor(i),
                Some(i) => WriteRoot::Local(i),
                None => WriteRoot::Temporary,
            },
            Some(Place::Capture(_)) =>
                WriteRoot::Capture { node: root, local: self.local_read_at(&root) },
            Some(Place::Global(_)) | None => WriteRoot::Global,
        }
    }

    pub(super) fn hoist_declarations(&mut self, stmts: &[HirId<HirStmt>]) {
        for stmt in stmts {
            if !self.ctx.hir.get(stmt).declares_slot() {
                continue;
            }
            let name = match self.ctx.hir.get(stmt) {
                HirStmt::Fn(decl) => {
                    self.locals.push(Local::func(decl.name, *stmt));
                    decl.name
                },
                HirStmt::Type(decl) => decl.name,
                _ => unreachable!("only functions and types declare slots"),
            };
            let state = CallableState { unreached: Some(name), frame: self.current_frame_id, uses: Callables::new() };
            self.callables.insert(stmt.index(), state);
        }
    }

    /// What must be ready before `callables` can be used, which is them and what they use.
    fn transitively_needed_callables_of(&self, callables: &Callables) -> Callables {
        // Callables made in enclosing frames don't need to be mentioned, because this frame
        // cannot run before they are ready anyway. Frames nested in this one have larger ids.
        let in_frame = |c: &usize| self.callables.get(c).is_some_and(|s| s.frame >= self.current_frame_id);
        let mut all: Callables = callables.iter().copied().filter(in_frame).collect();
        let mut unvisited: Vec<usize> = all.iter().copied().collect();
        while let Some(callable) = unvisited.pop() {
            for &used in &self.callables[&callable].uses {
                if in_frame(&used) && all.insert(used) {
                    unvisited.push(used);
                }
            }
        }
        all
    }

    /// The first of what `callables` needs to be ready that the walk has not reached yet.
    pub(super) fn first_unreached_needed_decl(&self, callables: &Callables) -> Option<Symbol> {
        self.transitively_needed_callables_of(callables).into_iter()
            .find_map(|callable| self.callables[&callable].unreached)
    }

    /// Records a use of a declaration's name, and returns the declaration.
    pub(super) fn note_declaration_use(&mut self, at: &HirId<HirExpr>) -> Option<usize> {
        let decl = self.ctx.bindings.declaring_node(at)?;
        let state = self.callables.get(&decl)?;
        // Used above its line, it has to exist there, so codegen builds it at the top of its scope.
        if state.unreached.is_some() {
            self.out.forward_referenced.insert(decl);
        }
        if state.frame != self.current_frame_id {
            self.effects.uses.insert(decl);
        }
        Some(decl)
    }

    pub(super) fn not_ready_message(&self, subject: &str, unreached: Symbol) -> String {
        let text = self.ctx.binding_display_name(unreached);
        format!("{subject} cannot be used yet, because it uses '{text}', which is declared below")
    }

    pub(super) fn local_read_at(&self, at: &HirId<HirExpr>) -> Option<usize> {
        let node_id = self.ctx.bindings.declaring_node(at)?;
        let found = self.locals.iter().rposition(|local| local.decl == Some(node_id));

        debug_assert!(found.is_some()
            || matches!(self.ctx.hir.stmt_at(node_id), Some(HirStmt::Type(_)))
            || matches!(self.ctx.hir.expr_at(node_id), Some(HirExpr::Match(..) | HirExpr::Binary(..))),
            "a read of a local found no checker local for it");

        found
    }

    pub(super) fn note_local_read(&mut self, i: usize) {
        self.effects.reads.insert(i);
        if i < self.frame_start {
            self.effects.uses.extend(self.locals[i].possible.values().flat_map(|facts| facts.callables.iter().copied()));
        }
    }

    pub(super) fn note_local_write(&mut self, i: usize) {
        let now = local_flow_of(&self.locals[i]);
        self.effects.note_write(i, now);
    }

    /// Records a change to what is known about `root`. Any change that can drop a fact counts as a
    /// write, since a throw right after it leaves with the fact gone.
    pub(super) fn note_write_to(&mut self, root: NarrowRoot) {
        match root {
            NarrowRoot::Local(i) => self.note_local_write(i),
            NarrowRoot::This => self.effects.note_this_write(self.this_narrowed.clone()),
        }
    }

    pub(super) fn refuse_unready(&self, callables: &Callables, at: &HirId<HirExpr>, help: &str) -> Result<(), anyhow::Error> {
        let Some(unreached) = self.first_unreached_needed_decl(callables) else { return Ok(()) };
        let message = match self.ctx.hir.get(at) {
            HirExpr::Identifier(name) if *name == unreached =>
                format!("'{}' is used before its declaration", self.ctx.binding_display_name(unreached)),
            _ => self.not_ready_message(&self.unready_subject(at), unreached),
        };
        Err(self.error_help(message, at, help.to_string()))
    }

    fn unready_subject(&self, at: &HirId<HirExpr>) -> String {
        match self.ctx.hir.get(at) {
            HirExpr::Identifier(name) => format!("'{}'", self.ctx.binding_display_name(*name)),
            HirExpr::Literal(HirLiteral::Lambda(_)) => "This function".to_string(),
            HirExpr::Anchor(inner) | HirExpr::Assign(inner, _) => self.unready_subject(inner),
            _ => "This value".to_string(),
        }
    }

    pub(super) fn refuse_unready_for_defer(&self, i: usize, possible: &PathMap<PossibleFacts>, at: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        if !self.defer_reads.iter().any(|reads| reads.contains(&i)) {
            return Ok(());
        }
        let Some(unreached) = self.first_unreached_needed_decl(&possible.callables()) else { return Ok(()) };
        let local = self.ctx.binding_display_name(self.locals[i].name);
        let text = self.ctx.binding_display_name(unreached);
        Err(self.error_help(self.not_ready_message("This function", unreached), at,
            format!("a 'defer' reads '{local}', and it can run before '{text}' is declared")))
    }

    /// Walks one statement of a block. For a declaration, records what its bodies use.
    pub(super) fn walk_block_stmt(&mut self, stmt: &HirId<HirStmt>) -> Result<(), anyhow::Error> {
        let (_, mut effects) = self.effects_of(|c| c.stmt(stmt))?;
        if let Some(state) = self.callables.get_mut(&stmt.index()) {
            state.unreached = None;
            state.uses = effects.replace_uses_with(stmt.index());
        }
        self.add_effects(effects);
        Ok(())
    }

    pub(super) fn effects_of<T>(&mut self, walk: impl FnOnce(&mut Self) -> Result<T, anyhow::Error>) -> Result<(T, SubtreeEffects), anyhow::Error> {
        let outer = std::mem::take(&mut self.effects);
        let walked = walk(self);
        let effects = std::mem::replace(&mut self.effects, outer);
        Ok((walked?, effects))
    }

    pub(super) fn add_effects(&mut self, inner: SubtreeEffects) {
        self.effects.absorb(inner);
    }

    pub(super) fn frame_index_of(&self, name: Symbol) -> Option<usize> {
        self.frame_locals().iter().rposition(|l| l.name == name)
            .map(|i| self.frame_start + i)
    }

    pub(super) fn capture_index(&self, name: Symbol) -> Option<usize> {
        self.locals[..self.frame_start].iter().rposition(|l| l.name == name)
    }

    pub(super) fn close_scope(&mut self, mark: usize, at: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let dropped = self.check_dropped(mark, at);
        self.truncate_locals(mark);
        dropped
    }

    pub(super) fn truncate_locals(&mut self, mark: usize) {
        self.locals.truncate(mark);
        self.effects.reads.split_off(&mark);
        self.effects.writes.split_off(&mark);
    }

    pub(super) fn push_binders(&mut self, scope: &PatternBinderScope) {
        for &name in &scope.names {
            let facts = scope.facts.get(&name).cloned().unwrap_or_default();
            let mut local = Local::pattern_binder(name, &facts.state(), scope.source);
            local.decl = scope.decls.get(&name).copied();
            local.reassignable = scope.reassignable;
            let checked = matches!(scope.source, PatternBinderSource::Say | PatternBinderSource::Param);
            if let Some(pattern) = local.decl.filter(|_| checked) {
                self.out.checked_slot_clauses.insert(CheckedSlot::PatternBinding(HirId::from_index(pattern), name), local.clause_owed().clone());
            }
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
        let facts = self.ctx.condition_facts(cond)?;
        Ok(PatternBinderScope {
            decls: names.iter().map(|&name| (name, cond.index())).collect(),
            names,
            facts,
            reassignable: false,
            source: PatternBinderSource::Condition,
        })
    }

    pub(super) fn match_arm_binders(&self, arm: &HirMatchArm, remaining: &Obligations, at: &HirId<HirStmt>) -> Result<PatternBinderScope, anyhow::Error> {
        let whole = collect_whole_value_binders(self.ctx.hir, &arm.matcher);
        let mut facts = self.ctx.matcher_facts(&arm.matcher, at)?;
        let mut names = self.ctx.hir.get(&arm.matcher).binders(self.ctx.hir);
        let mut decls: HashMap<Symbol, usize> = names.iter().map(|&n| (n, arm.matcher.index())).collect();

        if let Some(guard) = &arm.guard {
            let guard_names = self.ctx.hir.condition_pattern_binders(guard);
            decls.extend(guard_names.iter().map(|&n| (n, guard.index())));
            names.extend(guard_names);
        }

        if let Some(guard) = &arm.guard {
            facts.merge(self.ctx.condition_facts(guard)?);
        }

        for &name in &names {
            if whole.contains(&name) {
                facts.add_owed(name, remaining);
            }
        }

        Ok(PatternBinderScope { names, decls, facts, reassignable: false, source: PatternBinderSource::Arm })
    }

    pub(super) fn with_frame<R>(&mut self, params: &[HirParam], at: &HirId<HirExpr>, body: impl FnOnce(&mut Self) -> Result<R, anyhow::Error>) -> Result<R, anyhow::Error> {
        let saved_frame = self.frame_start;
        let saved_frame_id = self.current_frame_id;
        self.frames_opened += 1;
        self.current_frame_id = self.frames_opened;
        let mark = self.locals.len();
        self.frame_start = mark;
        let saved_this_narrowed = std::mem::take(&mut self.this_narrowed);
        let saved_this_writes = self.effects.this_writes.take();

        for param in params.iter() {
            let name = self.ctx.hir.ident_sym(&param.name);
            let mut owed = param.clause.owed();

            // A witness alternative is the real obligation, so `x @ Node | null` owes `opt`.
            if let Some(pattern) = &param.pattern {
                owed.extend(self.ctx.resolved().admitted_obligations(pattern));
            }

            let mut local = Local::param(name, owed, param.reassignable);
            local.decl = Some(param.name.index());
            local.site = Some(param.name);
            local.is_anchor = param.anchor;
            local.is_param = true;
            self.out.checked_slot_clauses.insert(CheckedSlot::Param(param.name), local.clause_owed().clone());

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
        self.current_frame_id = saved_frame_id;
        self.this_narrowed = saved_this_narrowed;
        self.effects.this_writes = saved_this_writes;
        dropped?;
        result
    }

    pub(super) fn snapshot(&self) -> FlowSnapshot {
        FlowSnapshot {
            locals: self.locals.iter().map(local_flow_of).collect(),
            this_narrowed: self.this_narrowed.clone(),
        }
    }

    pub(super) fn join_current_into(&self, flow: &mut FlowSnapshot) {
        for (snap, local) in flow.locals.iter_mut().zip(&self.locals) {
            merge_local_flow(snap, &local_flow_of(local));
        }
        flow.this_narrowed.join(&self.this_narrowed);
    }

    /// Puts flow back exactly as the snapshot had it.
    pub(super) fn restore_flow(&mut self, flow: &FlowSnapshot) {
        for (local, snap) in self.locals.iter_mut().zip(&flow.locals) {
            restore_local_flow(local, snap);
        }
        self.this_narrowed = flow.this_narrowed.clone();
    }

    /// Undoes flow facts that were applied for a branch, keeping what the branch wrote.
    pub(super) fn roll_back_flow(&mut self, flow: &FlowSnapshot) {
        for (local, snap) in self.locals.iter_mut().zip(&flow.locals) {
            roll_back_local_flow(local, snap);
        }
        self.this_narrowed = flow.this_narrowed.clone();
    }

    pub(super) fn restore_narrowings(&mut self, flow: &FlowSnapshot) {
        for (local, snap) in self.locals.iter_mut().zip(&flow.locals) {
            let LocalFlow { assigned: _, proven, possible, resolved_callable: _ } = snap;
            local.proven.join(proven);
            local.possible.join(possible);
        }
    }

    /// Leaves a `finally` on the paths that go on to the code below. What it wrote stays as its walk
    /// left it. Everything else is as `fallthrough` had it, together with what the finally proved.
    pub(super) fn leave_finally(&mut self, fallthrough: &FlowSnapshot, effects: &SubtreeEffects) {
        for (i, (local, before)) in self.locals.iter_mut().zip(&fallthrough.locals).enumerate() {
            if !effects.writes.contains_key(&i) {
                let proved_by_finally = std::mem::take(&mut local.proven);
                restore_local_flow(local, before);
                local.proven.add_proofs(&proved_by_finally);
            }
        }
        if effects.this_writes.is_none() {
            let proved_by_finally = std::mem::replace(&mut self.this_narrowed, fallthrough.this_narrowed.clone());
            self.this_narrowed.add_proofs(&proved_by_finally);
        }
    }

    /// Merges one outcome's end state into the current flow.
    pub(super) fn merge_flow_into_current(&mut self, other: &FlowSnapshot) {
        debug_assert!(other.locals.len() == self.locals.len());
        for (local, snap) in self.locals.iter_mut().zip(&other.locals) {
            let mut merged = local_flow_of(local);
            merge_local_flow(&mut merged, snap);
            restore_local_flow(local, &merged);
        }
        self.this_narrowed.join(&other.this_narrowed);
    }
}

pub(super) fn local_flow_of(local: &Local) -> LocalFlow {
    LocalFlow {
        assigned: local.assigned,
        proven: local.proven.clone(),
        possible: local.possible.clone(),
        resolved_callable: local.resolved_callable,
    }
}

/// Undoes a local's flow facts that were applied for a branch, keeping what the branch wrote.
pub(super) fn roll_back_local_flow(local: &mut Local, flow: &LocalFlow) {
    let mut rolled = flow.proven.clone();
    for (path, now) in &local.proven {
        let Some(now_owed) = &now.owed else { continue };
        let entry = rolled.entry(path.clone()).or_default();
        entry.owed = Some(match &entry.owed {
            Some(before) => before.union(now_owed),
            None => now_owed.clone(),
        });
    }
    rolled.retain(|_, facts| !facts.is_empty());
    local.assigned = flow.assigned;
    // The branch may not have run, so the local may hold what either state put in it.
    local.possible.join(&flow.possible);
    local.proven = rolled;
    local.resolved_callable = flow.resolved_callable;
}

pub(super) fn restore_local_flow(local: &mut Local, flow: &LocalFlow) {
    let LocalFlow { assigned, proven, possible, resolved_callable } = flow;
    local.assigned = *assigned;
    local.proven = proven.clone();
    local.possible = possible.clone();
    local.resolved_callable = *resolved_callable;
}

pub fn merge_local_flow(into: &mut LocalFlow, other: &LocalFlow) {
    let LocalFlow { assigned, proven, possible, resolved_callable } = other;
    into.assigned = into.assigned && *assigned;

    // Either path could have run, so the local may hold what either put in it.
    into.possible.join(possible);

    // Either path could have run, so a name resolves only where both paths reach the same callable.
    if into.resolved_callable != *resolved_callable {
        into.resolved_callable = None;
    }

    into.proven.join(proven);
}


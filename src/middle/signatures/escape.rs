//! Escape analysis: per parameter, whether a function persists its argument. A persist is a store, a
//! construct, or a forward to a callee that itself persists. A return hands the value back to the
//! caller, and a capture persists only when the closure holding it does.
//!
//! Functions are summarised callee-first, so one analysis walk of a body settles it. Recursion is the
//! one case that cannot be ordered, so a recursive group is walked until it stops changing.

use std::collections::{HashMap, HashSet};

use crate::middle::bind::Place;
use crate::middle::hir::{HirExpr, HirFnDecl, HirId, HirLiteral, HirStmt, Symbol};
use crate::middle::native;

use super::{Collector, ParamFact};
use crate::middle::walk::Child;
use crate::middle::walk;

/// How the escape walk counts a parameter reference.
#[derive(Clone, Copy, PartialEq)]
enum EscapeCollectMode {
    /// Counts only references that escape the current function.
    Escape,
    /// Counts every reference, to mark a parameter as captured by a nested function or lambda.
    Capture,
}

/// Whether a name may be the value it is tied to, or merely holds it.
#[derive(Clone, Copy, PartialEq)]
enum AliasKind {
    Identity,
    Containment,
}

/// One name a value keeps reachable: the name, how the value relates to it, and the node it was read at.
type Reached = (Symbol, AliasKind, HirId<HirExpr>);

/// One name tied to another by a `say` or an assignment.
struct Alias {
    local: Symbol,
    source: Symbol,
    kind: AliasKind,
}

/// Names recorded with the node that put them there.
#[derive(Default)]
struct Sites(HashMap<Symbol, HirId<HirExpr>>);

impl Sites {
    fn note(&mut self, name: Symbol, at: HirId<HirExpr>) {
        match self.0.get(&name) {
            Some(prev) if prev.index() <= at.index() => {},
            _ => { self.0.insert(name, at); },
        }
    }

    fn note_all(&mut self, names: impl IntoIterator<Item = Symbol>, at: HirId<HirExpr>) {
        for name in names { self.note(name, at); }
    }

    fn names(&self) -> impl Iterator<Item = &Symbol> {
        self.0.keys()
    }

    fn into_names(self) -> impl Iterator<Item = Symbol> {
        self.0.into_keys()
    }

    fn iter(&self) -> impl Iterator<Item = (&Symbol, &HirId<HirExpr>)> {
        self.0.iter()
    }

    fn drop_names(&mut self, names: &HashSet<Symbol>) {
        self.0.retain(|name, _| !names.contains(name));
    }

    fn merge(&mut self, other: Sites) {
        for (name, at) in other.0 { self.note(name, at); }
    }
}

/// Keeps the earliest of two escape sites, for the same reason `Sites` does.
fn note_site(slot: &mut Option<HirId<HirExpr>>, at: HirId<HirExpr>) {
    match slot {
        Some(prev) if prev.index() <= at.index() => {},
        _ => *slot = Some(at),
    }
}

/// The escape structure of one analyzed function, collected by name. A name here is any identifier
/// the body mentions, not only a parameter. `Carriers` maps each back to the parameters it may
/// hold, and drops the ones that reach none.
#[derive(Default)]
struct EscapeFacts {
    /// Names persisted where the caller cannot see them again: stored, or held by a persisted closure.
    direct: Sites,
    /// Names the function returns. The caller gets these back, so they never leave its reach.
    returned: Sites,
    /// Names mutated in place through an index or field write.
    mutates: HashSet<Symbol>,
    /// Names stored somewhere where a second name can write them.
    stored_away: HashSet<Symbol>,
    /// Names a call writes through its own effect: a native mutate or persist, or a conservative
    /// write by an opaque callee. A known callee's writes ride `forwards` instead.
    call_writes: HashSet<Symbol>,
    /// Forwarding edges to known callees, whose persist depends on the callee's own summary.
    forwards: Vec<EscapeForward>,
    /// What each local is tied to, from `say` and assignment.
    aliases: Vec<Alias>,
    /// `(container, value, site)` from a store into a name the body may own.
    stores: Vec<(Symbol, Symbol, HirId<HirExpr>)>,
    /// Names the body rebinds that belong to an enclosing scope. A rebind replaces what the name
    /// denotes, which is not a write to the value and so is none of the sets above.
    rebound: HashSet<Symbol>,
    /// Names this body declares.
    bound: HashSet<Symbol>,
}

/// One forwarding edge: the argument named `arg` escapes if `callee` persists it at its
/// `callee_param` position.
struct EscapeForward {
    callee: HirId<HirStmt>,
    callee_param: usize,
    arg: Symbol,
    /// The argument node, so a refusal can point at what the call retains.
    at: HirId<HirExpr>,
}

/// One function's body, resolved into what its summaries are computed from.
struct FnAnalysis {
    func: HirId<HirStmt>,
    /// The names the rows are indexed by.
    params: Vec<Symbol>,
    /// Which parameters each name in the body may hold.
    carriers: Carriers,
    /// What the body does with each name it mentions.
    facts: EscapeFacts,
}

/// Which parameters each name in a body may hold.
#[derive(Default)]
struct Carriers {
    /// The parameters a name may hold directly.
    held: HashMap<Symbol, HashSet<Symbol>>,
    /// The parameters a name may hold through a container.
    contained: HashMap<Symbol, HashSet<Symbol>>,
}

impl Carriers {
    /// Grows both maps until stable, so a chain of aliases carries a parameter all the way through.
    /// Every parameter holds itself, and an alias makes its target hold whatever its source holds.
    fn of(params: &[Symbol], aliases: &[Alias]) -> Carriers {
        let mut held: HashMap<Symbol, HashSet<Symbol>> = params.iter().map(|p| (*p, HashSet::from([*p]))).collect();
        let mut contained: HashMap<Symbol, HashSet<Symbol>> = HashMap::new();
        loop {
            let mut changed = false;
            for alias in aliases {
                let Some(source_held) = held.get(&alias.source).cloned() else { continue };
                // A container puts everything its source reaches out of the caller's reach. A
                // pass-through only carries on whatever was already inside a container.
                let promoted = match alias.kind {
                    AliasKind::Containment => source_held.clone(),
                    AliasKind::Identity => contained.get(&alias.source).cloned().unwrap_or_default(),
                };
                let entry = held.entry(alias.local).or_default();
                for p in source_held {
                    changed |= entry.insert(p);
                }
                let entry = contained.entry(alias.local).or_default();
                for p in promoted {
                    changed |= entry.insert(p);
                }
            }
            if !changed { break; }
        }
        Carriers { held, contained }
    }

    /// The parameters a name may hold.
    fn held(&self, name: &Symbol) -> impl Iterator<Item = &Symbol> {
        self.held.get(name).into_iter().flatten()
    }

    /// Whether a name reaches a parameter only from inside a container.
    fn contains(&self, name: &Symbol, param: &Symbol) -> bool {
        self.contained.get(name).is_some_and(|c| c.contains(param))
    }
}

/// Tarjan's strongly connected components over the call graph. Components come out in reverse
/// topological order, so a component is emitted only after everything it calls.
struct Components {
    index: HashMap<HirId<HirStmt>, usize>,
    low: HashMap<HirId<HirStmt>, usize>,
    on_stack: HashSet<HirId<HirStmt>>,
    stack: Vec<HirId<HirStmt>>,
    next: usize,
    out: Vec<Vec<HirId<HirStmt>>>,
}

impl Components {
    fn of(nodes: &[HirId<HirStmt>], edges: &HashMap<HirId<HirStmt>, Vec<HirId<HirStmt>>>) -> Vec<Vec<HirId<HirStmt>>> {
        let mut run = Components {
            index: HashMap::new(),
            low: HashMap::new(),
            on_stack: HashSet::new(),
            stack: Vec::new(),
            next: 0,
            out: Vec::new(),
        };
        for node in nodes {
            if !run.index.contains_key(node) {
                run.visit(*node, edges);
            }
        }
        run.out
    }

    fn visit(&mut self, v: HirId<HirStmt>, edges: &HashMap<HirId<HirStmt>, Vec<HirId<HirStmt>>>) {
        self.index.insert(v, self.next);
        self.low.insert(v, self.next);
        self.next += 1;
        self.stack.push(v);
        self.on_stack.insert(v);

        for w in edges.get(&v).into_iter().flatten().copied() {
            let reachable = match self.index.get(&w) {
                None => {
                    self.visit(w, edges);
                    self.low[&w]
                },
                // A callee still on the stack is part of this component, so its index bounds the root.
                Some(index) if self.on_stack.contains(&w) => *index,
                Some(_) => continue,
            };
            let bound = self.low[&v].min(reachable);
            self.low.insert(v, bound);
        }

        if self.low[&v] != self.index[&v] {
            return;
        }

        let mut component = Vec::new();
        while let Some(w) = self.stack.pop() {
            self.on_stack.remove(&w);
            component.push(w);
            if w == v { break; }
        }
        self.out.push(component);
    }
}

/// Turns each recorded store into either an escape or an alias. A parameter and the receiver are
/// containers the caller owns, so a store into one leaves the call. The receiver is named apart from
/// the row, since a lambda in a method reaches `this` without taking it as a parameter.
fn resolve_stores(params: &[Symbol], this: Symbol, facts: &mut EscapeFacts) {
    for (container, source, at) in std::mem::take(&mut facts.stores) {
        match container == this || params.contains(&container) {
            true => facts.direct.note(source, at),
            false => facts.aliases.push(Alias { local: container, source, kind: AliasKind::Containment }),
        }
    }
}

/// The position of `param` in the analyzed function's parameter list.
fn param_position(params: &[Symbol], param: Symbol) -> usize {
    // Every resolved escape fact names a parameter of that function, so it's always present.
    params.iter().position(|p| *p == param).expect("escape fact names a parameter")
}

impl<'a> Collector<'a> {
    /// The names the escape rows are indexed by: the declared parameters, then the receiver under
    /// its reserved name so the declared positions are unchanged.
    fn escape_params(&self, decl: &HirFnDecl) -> Vec<Symbol> {
        let mut params: Vec<Symbol> = decl.params.iter().map(|p| self.param_sym(&p.name)).collect();
        if decl.receiver.is_some() {
            params.push(self.this);
        }
        params
    }

    /// The enclosing names a closure body reads.
    fn captures_of(&self, decl: &HirFnDecl) -> Sites {
        let mut facts = EscapeFacts::default();
        for param in &decl.params {
            if let HirExpr::Identifier(slot) = self.hir.get(&param.name) {
                facts.bound.insert(*slot);
            }
            if let Some(pattern) = &param.pattern {
                facts.bound.extend(self.hir.get(pattern).binders(self.hir));
            }
        }
        self.walk_escapes(&decl.body, &mut facts, EscapeCollectMode::Capture, None);
        facts.direct.drop_names(&facts.bound);
        facts.direct
    }

    /// Records escape facts for every lambda.
    pub(super) fn collect_lambda_captures(&mut self) {
        for id in self.hir.lambda_ids() {
            let HirExpr::Literal(HirLiteral::Lambda(decl)) = self.hir.get(&id) else { continue };
            self.lambda_captures.insert(id, self.captures_of(decl).into_names().collect());
        }
    }

    /// Summarises every function: which arguments it hands back, which it keeps beyond the caller's
    /// reach, and which it mutates. Callees are summarised first, so one visit of a body settles it.
    pub(super) fn infer_escape_summaries(&mut self) {
        let funcs: Vec<HirId<HirStmt>> = self.sigs.fns.keys().copied().collect();
        let edges: HashMap<HirId<HirStmt>, Vec<HirId<HirStmt>>> = funcs.iter()
            .map(|func| (*func, self.callees_of(*func)))
            .collect();

        for component in Components::of(&funcs, &edges) {
            // A lone function that does not call itself sees final callee summaries on its first
            // visit. A recursive group has to settle, since its members feed each other.
            let recursive = component.len() > 1 || edges[&component[0]].contains(&component[0]);
            let mut analyses = Vec::new();
            loop {
                analyses.clear();
                let mut changed = false;
                for func in &component {
                    let analysis = self.analyze(*func);
                    changed |= self.record(&analysis);
                    analyses.push(analysis);
                }
                if !recursive || !changed { break; }
            }

            // A write set reads its callees' finished summaries, which reverse-topological order has
            // already settled by the time this component is done.
            for analysis in &analyses {
                let writes = self.body_writes(&analysis.facts);
                self.sigs.writes.insert(analysis.func, writes);
                self.sigs.any_rebind.extend(&analysis.facts.rebound);
            }
        }
    }

    /// Walks one body and resolves its facts. Every summary it consults belongs to a callee which
    /// the visit order has already settled.
    fn analyze(&self, func: HirId<HirStmt>) -> FnAnalysis {
        let HirStmt::Fn(decl) = self.hir.get(&func) else { unreachable!("every collected signature is a fn") };
        let params = self.escape_params(decl);
        let owner = self.sigs.method_owner.get(&func).copied();
        let mut facts = EscapeFacts::default();
        self.walk_escapes(&decl.body, &mut facts, EscapeCollectMode::Escape, owner);
        resolve_stores(&params, self.this, &mut facts);
        let carriers = Carriers::of(&params, &facts.aliases);
        FnAnalysis { func, params, carriers, facts }
    }

    /// Folds one body's facts into a row of one entry per parameter. A forwarded argument undergoes
    /// whatever its callee does to it, which the visit order has already settled.
    fn fold_facts(&self, params: &[Symbol], carriers: &Carriers, facts: &EscapeFacts) -> Vec<ParamFact> {
        let mut row = vec![ParamFact::default(); params.len()];
        // A directly persisted or mutated argument is kept whatever any callee does.
        for (name, at) in facts.direct.iter() {
            for p in carriers.held(name) {
                let fact = &mut row[param_position(params, *p)];
                fact.escapes = true;
                fact.escapes_beyond_return = true;
                note_site(&mut fact.escape_site, *at);
            }
        }
        for name in &facts.mutates {
            for p in carriers.held(name) { row[param_position(params, *p)].mutates = true; }
        }
        for name in &facts.stored_away {
            for p in carriers.held(name) { row[param_position(params, *p)].stored_away = true; }
        }
        for forward in &facts.forwards {
            let callee_fact = self.sigs.param_fact(&forward.callee, forward.callee_param);
            for p in carriers.held(&forward.arg) {
                let fact = &mut row[param_position(params, *p)];
                // A callee that only hands the argument back has not let it out of this body. Where
                // the result then goes is the value walk's answer, not the edge's, and it reads the
                // call through the same `hands_back` fact.
                if callee_fact.escapes_beyond_return {
                    fact.escapes = true;
                    fact.escapes_beyond_return = true;
                    note_site(&mut fact.escape_site, forward.at);
                }
                fact.mutates |= callee_fact.mutates;
                fact.stored_away |= callee_fact.stored_away;
            }
        }
        row
    }

    /// Writes one function's row, answering whether it gained a bit. The row is rebuilt rather than
    /// patched, which is safe because every input only ever grows.
    fn record(&mut self, a: &FnAnalysis) -> bool {
        let mut row = self.fold_facts(&a.params, &a.carriers, &a.facts);
        let mut free: Vec<Symbol> = Vec::new();
        // Handing an argument back still counts as keeping it, which is what bars passing a mutable
        // value to a function that returns it.
        for (name, at) in a.facts.returned.iter() {
            for p in a.carriers.held(name) {
                let fact = &mut row[param_position(&a.params, *p)];
                fact.escapes = true;
                // A name that reaches the argument through a container hands back the container,
                // not the argument. The caller gets no way back to what it lent.
                if a.carriers.contains(name, p) {
                    fact.escapes_beyond_return = true;
                    note_site(&mut fact.escape_site, *at);
                }
            }
        }

        for ret in self.returns.get(&a.func).into_iter().flatten() {
            // A function hands back an argument when its result keeps that argument reachable.
            // `return x`, `return [x]` and `return () => x.n` all count, as does returning a call
            // that itself hands the argument back.
            for (name, kind, at) in self.reachable_kinds(ret) {
                let carried: Vec<Symbol> = a.carriers.held(&name).copied().collect();
                for p in &carried {
                    let fact = &mut row[param_position(&a.params, *p)];
                    fact.hands_back = true;
                    // The narrower fact: the result may be the argument itself, not merely
                    // something holding it. `return x` counts and `return [x]` does not.
                    fact.hands_back_itself |= kind == AliasKind::Identity && !a.carriers.contains(&name, p);
                }
                // A name no parameter carries is worth reporting only if the caller can hold it too.
                let free_here = matches!(self.bindings.place_of(&at), Some(Place::Upvalue(_)));
                if carried.is_empty() && kind == AliasKind::Identity && free_here {
                    free.push(name);
                }
            }
        }

        free.sort_unstable();
        free.dedup();
        // Every input only ever grows, so a row that differs from the stored one has gained a bit.
        let grew = self.sigs.params.get(&a.func) != Some(&row)
            || self.sigs.returns_free.get(&a.func) != Some(&free);
        self.sigs.params.insert(a.func, row);
        self.sigs.returns_free.insert(a.func, free);
        grew
    }

    /// The functions a body calls.
    fn callees_of(&self, func: HirId<HirStmt>) -> Vec<HirId<HirStmt>> {
        let HirStmt::Fn(decl) = self.hir.get(&func) else { unreachable!("every collected signature is a fn") };
        let owner = self.sigs.method_owner.get(&func).copied();
        let mut out = Vec::new();

        walk::visit_body(self.hir, &decl.body, &mut |node| {
            if let Child::Expr(e) = node {
                if let HirExpr::Call(callee, _) = self.hir.get(&e) {
                    // A constructor is an edge to the factory, so the factory settles first and
                    // the forwarded arguments read a finished summary.
                    if let Some(target) = self.call_target(callee, owner) { out.push(target); }
                }
            }
        });

        // One edge per callee, however many times the body calls it.
        out.sort_unstable_by_key(|f| f.index());
        out.dedup();
        out
    }

    /// The declaration a callee expression names, when the pass can name one.
    fn resolved_callee(&self, callee: &HirId<HirExpr>, owner: Option<HirId<HirStmt>>) -> Option<HirId<HirStmt>> {
        if self.resolved().type_named(callee).is_some() {
            return None;
        }
        match self.hir.get(callee) {
            HirExpr::Identifier(name) => self.sigs.fns_by_name.get(name).copied(),
            // A `this.method` call resolves within the enclosing type.
            HirExpr::Index(receiver, member, _) => {
                let owner = owner.filter(|_| matches!(self.hir.get(receiver), HirExpr::This))?;
                self.sigs.methods_by_type.get(&(owner, self.member_symbol(member)?)).copied()
            },
            _ => None,
        }
    }

    /// The names a body writes. A `say` alias carries a persisted local back to its source,
    /// so a value aliased then written still counts.
    fn body_writes(&self, facts: &EscapeFacts) -> HashSet<Symbol> {
        // Seed every mentioned name with identity, then grow through aliases so a persisted alias
        // carries its source.
        let mut seeds: HashSet<Symbol> = HashSet::new();

        for alias in &facts.aliases {
            seeds.insert(alias.local);
            seeds.insert(alias.source);
        }

        for f in &facts.forwards {
            seeds.insert(f.arg);
        }

        seeds.extend(facts.direct.names().copied());
        seeds.extend(&facts.mutates);
        seeds.extend(&facts.call_writes);

        let seeds: Vec<Symbol> = seeds.into_iter().collect();
        let carriers = Carriers::of(&seeds, &facts.aliases);

        let mut writes = HashSet::new();
        for n in facts.direct.names().chain(&facts.mutates).chain(&facts.call_writes) {
            writes.extend(carriers.held(n).copied());
        }
        for f in &facts.forwards {
            if self.sigs.param_escapes_at(&f.callee, f.callee_param) || self.sigs.param_mutates_at(&f.callee, f.callee_param) {
                writes.extend(carriers.held(&f.arg).copied());
            }
        }
        writes
    }

    /// Infers each lambda's definite-persist mask and the names its body writes.
    pub(super) fn infer_lambda_escapes(&mut self) {
        for id in self.hir.lambda_ids() {
            let HirExpr::Literal(HirLiteral::Lambda(decl)) = self.hir.get(&id) else { continue };
            let params: Vec<Symbol> = decl.params.iter().map(|p| self.param_sym(&p.name)).collect();
            let mut facts = EscapeFacts::default();
            self.walk_escapes(&decl.body, &mut facts, EscapeCollectMode::Escape, None);
            resolve_stores(&params, self.this, &mut facts);

            let carriers = Carriers::of(&params, &facts.aliases);
            // A lambda publishes only whether it keeps an argument. Returning one hands it back, and
            // a lambda is never resolved at a call site, so the return does not enter this row.
            let row = self.fold_facts(&params, &carriers, &facts);
            self.sigs.lambda_param_escapes.insert(id, row.iter().map(|f| f.escapes).collect());
            let writes = self.body_writes(&facts);
            self.sigs.lambda_writes.insert(id, writes);
            self.sigs.any_rebind.extend(&facts.rebound);
        }
    }

    fn param_sym(&self, id: &HirId<HirExpr>) -> Symbol {
        match self.hir.get(id) {
            HirExpr::Identifier(sym) => *sym,
            _ => unreachable!("parameter is an identifier"),
        }
    }

    /// The names a value keeps reachable, without saying how.
    fn reachable_names(&self, value: &HirId<HirExpr>) -> Vec<Symbol> {
        self.reachable_kinds(value).into_iter().map(|(name, _, _)| name).collect()
    }

    /// The names a value keeps reachable, each said to be the value or held by it.
    fn reachable_kinds(&self, value: &HirId<HirExpr>) -> Vec<Reached> {
        if let Some(kinds) = self.denoted_kinds(value) {
            return kinds;
        }
        // A closure holds the names its body reads, so persisting it persists them.
        if let Some(captured) = self.lambda_captures.get(value) {
            return captured.iter().map(|name| (*name, AliasKind::Containment, *value)).collect();
        }
        let (children, holds) = self.held_children(value);
        children.iter()
            .flat_map(|c| self.reachable_kinds(c))
            .map(|(name, kind, at)| (name, if holds { AliasKind::Containment } else { kind }, at))
            .collect()
    }

    /// The child expressions a value's ownership flows through, and whether it holds them rather
    /// than being one of them.
    fn held_children(&self, value: &HirId<HirExpr>) -> (Vec<HirId<HirExpr>>, bool) {
        match self.hir.get(value) {
            HirExpr::Construct(_, brace) => (brace.iter().map(|(_, v)| *v).collect(), true),
            HirExpr::Literal(HirLiteral::Array(_) | HirLiteral::Dict(_)) => (self.hir.ownership_children(value), true),
            _ => (self.hir.ownership_children(value), false),
        }
    }

    /// The names a value denotes rather than holds, when it denotes any. A call answers through its
    /// own summary, since its result is whatever it handed back.
    fn denoted_kinds(&self, value: &HirId<HirExpr>) -> Option<Vec<Reached>> {
        match self.hir.get(value) {
            HirExpr::Identifier(s) => Some(vec![(*s, AliasKind::Identity, *value)]),
            HirExpr::This => Some(vec![(self.this, AliasKind::Identity, *value)]),
            HirExpr::Call(callee, args) => Some(self.call_result_kinds(callee, args)),
            _ => None,
        }
    }

    /// The names a call's result keeps reachable, for a callee the pass can resolve. An opaque
    /// callee answers nothing, leaving its result to the runtime borrow check.
    fn call_result_kinds(&self, callee: &HirId<HirExpr>, args: &[HirId<HirExpr>]) -> Vec<Reached> {
        let Some(func) = self.resolved_callee(callee, None) else { return Vec::new() };
        let mut out = Vec::new();
        for (i, arg) in args.iter().enumerate() {
            let fact = self.sigs.param_fact(&func, i);
            if !fact.hands_back { continue; }
            // The result is the argument itself only where the callee hands that argument back
            // rather than a container it built around it.
            out.extend(self.reachable_kinds(arg).into_iter()
                .map(|(name, kind, at)| match fact.hands_back_itself {
                    true => (name, kind, at),
                    false => (name, AliasKind::Containment, at),
                }));
        }
        out
    }

    /// Records every name a persisted value keeps reachable as a direct escape.
    fn mark_persisted(&self, value: &HirId<HirExpr>, facts: &mut EscapeFacts) {
        facts.direct.note_all(self.reachable_names(value), *value);
        facts.stored_away.extend(self.reachable_names(value));
    }

    /// Records a store of `value` into `target`.
    fn mark_stored(&self, target: &HirId<HirExpr>, value: &HirId<HirExpr>, facts: &mut EscapeFacts) {
        let Some(container) = self.store_root(target) else { return self.mark_persisted(value, facts) };
        for source in self.reachable_names(value) {
            facts.stores.push((container, source, *value));
        }
        self.mark_held(value, facts);
    }

    /// Records that a container took the value.
    fn mark_held(&self, value: &HirId<HirExpr>, facts: &mut EscapeFacts) {
        // A container is a second name for what it holds, whoever ends up reaching it.
        facts.stored_away.extend(self.reachable_names(value));
    }

    /// The name a store's destination is rooted at.
    fn store_root(&self, target: &HirId<HirExpr>) -> Option<Symbol> {
        match self.hir.get(target) {
            HirExpr::This => Some(self.this),
            // A bare field name is a field of `this`, so it roots where an explicit access would.
            HirExpr::Identifier(name) => match self.bindings.place_of(target) {
                Some(Place::Local(_)) => Some(*name),
                Some(Place::Field(..)) => Some(self.this),
                _ => None,
            },
            HirExpr::Index(base, _, _) => self.store_root(base),
            _ => None,
        }
    }

    /// Records an edge from each argument to the matching parameter of the callee's declaration.
    fn forward_call_args(&self, callee: &HirId<HirExpr>, args: &[HirId<HirExpr>], facts: &mut EscapeFacts, owner: Option<HirId<HirStmt>>) {
        let Some(target) = self.call_target(callee, owner) else { return };
        for (callee_param, arg) in args.iter().enumerate() {
            for arg_name in self.reachable_names(arg) {
                facts.forwards.push(EscapeForward { callee: target, callee_param, arg: arg_name, at: *arg });
            }
        }
    }

    /// The declaration a call runs.
    fn call_target(&self, callee: &HirId<HirExpr>, owner: Option<HirId<HirStmt>>) -> Option<HirId<HirStmt>> {
        let Some(ty) = self.resolved().type_named(callee) else { return self.resolved_callee(callee, owner) };
        let HirStmt::Type(decl) = self.hir.get(&ty) else { return None };
        Some(decl.init).filter(|init| self.sigs.fns.contains_key(init))
    }

    /// The interned symbol of a member name node inside an index.
    fn member_symbol(&self, member: &HirId<HirExpr>) -> Option<Symbol> {
        match self.hir.get(member) {
            HirExpr::Literal(HirLiteral::String(name)) => self.hir.symbol_of(name),
            _ => None,
        }
    }

    /// The text of a member name node inside an index. A native method name is not always interned
    /// as a symbol, so match on the text rather than a symbol.
    fn member_text(&self, member: &HirId<HirExpr>) -> Option<&str> {
        match self.hir.get(member) {
            HirExpr::Literal(HirLiteral::String(name)) => Some(name),
            _ => None,
        }
    }

    /// Adds the names a value keeps reachable to a set.
    fn reachable_into(&self, value: &HirId<HirExpr>, out: &mut HashSet<Symbol>) {
        out.extend(self.reachable_names(value));
    }

    /// Records the names a call writes, for the body-writes summary the capture check reads. A
    /// native call may mutate its receiver or persist its argument, and an opaque callee may write
    /// any argument. A known function, constructor, or `this.method` is left to the escape forwarding.
    fn mark_call_writes(&self, callee: &HirId<HirExpr>, args: &[HirId<HirExpr>], owner: Option<HirId<HirStmt>>, facts: &mut EscapeFacts) {
        match self.hir.get(callee) {
            HirExpr::Index(recv, member, _) => {
                let Some(text) = self.member_text(member) else {
                    self.reachable_into(recv, &mut facts.call_writes);
                    for arg in args { self.reachable_into(arg, &mut facts.call_writes); }
                    return;
                };
                if let Some(sig) = native::native_method(text) {
                    if sig.effect.mutates_receiver { self.reachable_into(recv, &mut facts.call_writes); }
                    if sig.effect.writes_args {
                        for arg in args { self.reachable_into(arg, &mut facts.call_writes); }
                    }
                    if sig.effect.persists_args {
                        for arg in args { self.mark_stored(recv, arg, facts); }
                    }
                    return;
                }
                // A resolved `this.method` is forwarded by the escape walk. Any other method is
                // opaque, so its receiver and arguments may be written.
                if !matches!((owner, self.hir.get(recv)), (Some(_), HirExpr::This)) {
                    self.reachable_into(recv, &mut facts.call_writes);
                    for arg in args { self.reachable_into(arg, &mut facts.call_writes); }
                }
            },
            HirExpr::Identifier(name) => {
                // A read-only builtin does not write its arguments, so a captured value handed to
                // one stays a borrow. Any other unknown callee may write its arguments.
                let reads_only = native::builtin(self.hir.text(*name)).is_some_and(|s| !s.effect.writes_args);
                let known = self.sigs.is_type(*name) || self.sigs.fns_by_name.contains_key(name) || reads_only;
                if !known {
                    for arg in args { self.reachable_into(arg, &mut facts.call_writes); }
                }
            },
            _ => for arg in args { self.reachable_into(arg, &mut facts.call_writes); },
        }
    }

    /// Collects the escape facts by name. In `Escape` mode a persist site (a return, a store into a
    /// field or container, or a forward to a callee) records the name. A nested function or lambda is
    /// walked in `Capture` mode, where every reference records the name, since a capture persists it.
    /// `owner` is the enclosing type of a method body, so a `this.method` call resolves.
    fn walk_escapes(&self, expr: &HirId<HirExpr>, facts: &mut EscapeFacts, mode: EscapeCollectMode, owner: Option<HirId<HirStmt>>) {
        // Record what this node contributes, then recurse through the shared child structure.
        match self.hir.get(expr) {
            // A closure captures what it reads from an enclosing frame. A name it binds itself is
            // not a capture, however it is spelled, so `bind` decides it rather than the name.
            HirExpr::Identifier(s) => if mode == EscapeCollectMode::Capture
                && !matches!(self.bindings.place_of(expr), Some(Place::Local(_))) {
                facts.direct.note(*s, *expr);
            },
            HirExpr::This => if mode == EscapeCollectMode::Capture { facts.direct.note(self.this, *expr); },
            HirExpr::Assign(lhs, rhs) => if mode == EscapeCollectMode::Escape {
                // Writing through an index mutates the base value in place.
                if let HirExpr::Index(base, _, _) = self.hir.get(lhs) {
                    facts.mutates.extend(self.reachable_names(base));
                }
                // A rebind of the body's own local cannot reach a caller, and one of an upvalue or
                // a global can. `bind` already decided which, so this asks rather than guesses.
                if let HirExpr::Identifier(local) = self.hir.get(lhs) {
                    if !matches!(self.bindings.place_of(lhs), Some(Place::Local(_))) {
                        facts.rebound.insert(*local);
                    }
                }
                if self.assign_persists(lhs) {
                    self.mark_stored(lhs, rhs, facts);
                } else if let HirExpr::Identifier(local) = self.hir.get(lhs) {
                    // Rebinding a local aliases it to the value.
                    let local = *local;
                    for (source, kind, _) in self.reachable_kinds(rhs) { facts.aliases.push(Alias { local, source, kind }); }
                }
            },
            HirExpr::Call(callee, args) => if mode == EscapeCollectMode::Escape {
                self.forward_call_args(callee, args, facts, owner);
                self.mark_call_writes(callee, args, owner, facts);
            },
            HirExpr::Construct(_, brace) => if mode == EscapeCollectMode::Escape {
                for (_, value) in brace { self.mark_held(value, facts); }
            },
            // A closure holds what a nested one captures, since it holds the nested one.
            HirExpr::Literal(HirLiteral::Lambda(decl)) if mode == EscapeCollectMode::Capture => {
                facts.direct.merge(self.captures_of(decl));
            },
            HirExpr::Handle(_, binder, _) => { facts.bound.insert(*binder); },
            _ => {},
        }
        for child in walk::children_of_expr(self.hir, expr) {
            match child {
                Child::Expr(e) => self.walk_escapes(&e, facts, mode, owner),
                Child::Stmt(s) => self.walk_escapes_stmt(&s, facts, mode, owner),
            }
        }
    }

    /// Whether storing into `lhs` persists the value. An indexed target stores into a field or
    /// container. A bare name persists only when it binds to a field, upvalue, or global; a bare
    /// local just rebinds and is handled as an alias by the caller.
    fn assign_persists(&self, lhs: &HirId<HirExpr>) -> bool {
        match self.hir.get(lhs) {
            HirExpr::Index(..) => true,
            HirExpr::Identifier(_) => !matches!(self.bindings.place_of(lhs), Some(Place::Local(_))),
            _ => false,
        }
    }

    fn walk_escapes_stmt(&self, stmt: &HirId<HirStmt>, facts: &mut EscapeFacts, mode: EscapeCollectMode, owner: Option<HirId<HirStmt>>) {
        match self.hir.get(stmt) {
            HirStmt::Return(Some(e)) => if mode == EscapeCollectMode::Escape {
                for (name, kind, _) in self.reachable_kinds(e) {
                    match kind {
                        AliasKind::Identity => facts.returned.note(name, *e),
                        AliasKind::Containment => facts.direct.note(name, *e),
                    };
                }
            },
            HirStmt::Say(field) => {
                facts.bound.insert(field.name);
                if let Some(value) = field.value {
                    // Binding a local to a value aliases it, so a parameter is tracked through the local.
                    let local = field.name;
                    for (source, kind, _) in self.reachable_kinds(&value) { facts.aliases.push(Alias { local, source, kind }); }
                }
            },
            // A nested function is a closure bound to a name. What it captures leaves only as far
            // as that name does, which is how a lambda's captures are already read.
            HirStmt::Fn(decl) if mode == EscapeCollectMode::Escape => {
                facts.bound.insert(decl.name);
                for source in self.captures_of(decl).into_names() {
                    facts.aliases.push(Alias { local: decl.name, source, kind: AliasKind::Containment });
                }
            },
            HirStmt::Fn(decl) => {
                facts.bound.insert(decl.name);
                facts.direct.merge(self.captures_of(decl));
            },
            HirStmt::Match(_, arms) => for arm in arms {
                facts.bound.extend(self.hir.get(&arm.matcher).binders(self.hir));
            },
            HirStmt::Try(_, Some(catch), _) => if let Some(param) = catch.param {
                if let HirExpr::Identifier(name) = self.hir.get(&param) { facts.bound.insert(*name); }
            },
            HirStmt::While(cond, _) => facts.bound.extend(self.hir.condition_binders(cond)),
            HirStmt::If(cond, ..) => facts.bound.extend(self.hir.condition_binders(cond)),
            _ => {},
        }
        for child in walk::children_of_stmt(self.hir, stmt) {
            match child {
                Child::Expr(e) => self.walk_escapes(&e, facts, mode, owner),
                Child::Stmt(s) => self.walk_escapes_stmt(&s, facts, mode, owner),
            }
        }
    }
}

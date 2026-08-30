//! Escape analysis: per parameter, whether a function persists its argument.

use std::collections::{HashMap, HashSet};

use crate::middle::bind::Place;
use crate::middle::hir::{HirCatchClause, HirExpr, HirFnDecl, HirId, HirLiteral, HirStmt, Symbol, ValueSource};
use crate::middle::native;

use super::CallableId;
use super::Signatures;
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

#[derive(Clone, Copy, PartialEq)]
enum AliasKind {
    Identity,
    Containment,
}

/// One binding a value carries out with it: the name, whether the value is that binding's value or
/// merely holds it, and the node it was read at.
type CarriedName = (Symbol, AliasKind, HirId<HirExpr>);

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

fn take_earlier_expr(slot: Option<HirId<HirExpr>>, at: HirId<HirExpr>) -> Option<HirId<HirExpr>> {
    match slot {
        Some(prev) if prev.index() <= at.index() => Some(prev),
        _ => Some(at),
    }
}

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
    /// Borrowed names this body hands to a call that might retain them. An unresolvable callee counts,
    /// since nothing here knows what it does with an argument.
    needs_borrow_mark: HashSet<Symbol>,
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
    callee: CallableId,
    callee_param: usize,
    arg: Symbol,
    /// The argument node.
    at: HirId<HirExpr>,
}

struct FnAnalysis {
    callable: CallableId,
    params: Vec<Symbol>,
    /// Which parameters each name in the body may hold.
    carriers: Carriers,
    escape_facts: EscapeFacts,
}

/// Which parameters each name in a body may hold.
#[derive(Default)]
struct Carriers {
    held: HashMap<Symbol, HashSet<Symbol>>,
    contained: HashMap<Symbol, HashSet<Symbol>>,
}

impl Carriers {
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

    fn held(&self, name: &Symbol) -> impl Iterator<Item = &Symbol> {
        self.held.get(name).into_iter().flatten()
    }

    fn contains(&self, name: &Symbol, param: &Symbol) -> bool {
        self.contained.get(name).is_some_and(|c| c.contains(param))
    }
}

/// Tarjan's strongly connected components over the call graph. Components come out in reverse
/// topological order, so a component is emitted only after everything it calls.
struct Components {
    index: HashMap<CallableId, usize>,
    low: HashMap<CallableId, usize>,
    on_stack: HashSet<CallableId>,
    stack: Vec<CallableId>,
    next: usize,
    out: Vec<Vec<CallableId>>,
}

impl Components {
    fn of(nodes: &[CallableId], edges: &HashMap<CallableId, Vec<CallableId>>) -> Vec<Vec<CallableId>> {
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

    fn visit(&mut self, v: CallableId, edges: &HashMap<CallableId, Vec<CallableId>>) {
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

/// Turns each recorded store into either an escape or an alias.
fn resolve_stores(params: &[Symbol], this: Symbol, facts: &mut EscapeFacts) {
    for (container, source, at) in std::mem::take(&mut facts.stores) {
        match container == this || params.contains(&container) {
            true => facts.direct.note(source, at),
            false => facts.aliases.push(Alias { local: container, source, kind: AliasKind::Containment }),
        }
    }
}

fn param_position(params: &[Symbol], param: Symbol) -> usize {
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
        self.walk_escapes(&decl.body, &mut facts, EscapeCollectMode::Capture, None, false);
        facts.direct.drop_names(&facts.bound);
        facts.direct
    }

    pub(super) fn collect_lambda_captures(&mut self) {
        for id in self.hir.lambda_ids() {
            let HirExpr::Literal(HirLiteral::Lambda(decl)) = self.hir.get(&id) else { continue };
            self.lambda_captures.insert(id, self.captures_of(decl).into_names().collect());
        }
    }

    pub(super) fn infer_escape_summaries(&mut self) {
        let callables: Vec<CallableId> = self.sigs.fns.keys().copied().collect();
        let edges: HashMap<CallableId, Vec<CallableId>> = callables.iter()
            .map(|callable| (*callable, self.called_by(*callable)))
            .collect();

        for component in Components::of(&callables, &edges) {
            // A lone function that does not call itself sees final callee summaries on its first
            // visit. A recursive group has to settle, since its members feed each other.
            let recursive = component.len() > 1 || edges[&component[0]].contains(&component[0]);
            let mut analyses = Vec::new();
            loop {
                analyses.clear();
                let mut changed = false;
                for callable in &component {
                    let analysis = self.analyze_callable(*callable);
                    changed |= self.record(&analysis);
                    analyses.push(analysis);
                }
                if !recursive || !changed { break; }
            }

            // A write set reads its callees' finished summaries, which reverse-topological order has
            // already settled by the time this component is done.
            for analysis in &analyses {
                let writes = self.names_written_by_body(&analysis.escape_facts);
                self.sigs.writes.insert(analysis.callable, writes);
                self.sigs.any_rebind.extend(&analysis.escape_facts.rebound);
            }
        }
    }

    fn method_owner_of(&self, callable: CallableId) -> Option<HirId<HirStmt>> {
        match callable {
            CallableId::Fn(stmt) => self.sigs.method_owner.get(&stmt).copied(),
            CallableId::Lambda(_) => None,
        }
    }

    fn analyze_callable(&self, callable: CallableId) -> FnAnalysis {
        let decl = Signatures::decl_of(self.hir, callable).expect("every collected signature has a declaration");
        let params = self.escape_params(decl);
        let owner = self.method_owner_of(callable);
        let mut facts = EscapeFacts::default();
        self.walk_escapes(&decl.body, &mut facts, EscapeCollectMode::Escape, owner, false);
        resolve_stores(&params, self.this, &mut facts);
        let carriers = Carriers::of(&params, &facts.aliases);
        FnAnalysis { callable, params, carriers, escape_facts: facts }
    }

    fn fold_param_facts(&self, params: &[Symbol], carriers: &Carriers, facts: &EscapeFacts) -> Vec<ParamFact> {
        let mut param_facts = vec![ParamFact::default(); params.len()];
        for name in &facts.needs_borrow_mark {
            for p in carriers.held(name) {
                param_facts[param_position(params, *p)].needs_borrow_mark = true;
            }
        }
        // A directly persisted or mutated argument is kept whatever any callee does.
        for (name, at) in facts.direct.iter() {
            for p in carriers.held(name) {
                let fact = &mut param_facts[param_position(params, *p)];
                fact.escapes = true;
                fact.escapes_beyond_return = true;
                fact.escape_site = take_earlier_expr(fact.escape_site, *at);
            }
        }
        for name in &facts.mutates {
            for p in carriers.held(name) { param_facts[param_position(params, *p)].mutates = true; }
        }
        for name in &facts.stored_away {
            for p in carriers.held(name) { param_facts[param_position(params, *p)].stored_away = true; }
        }
        for forward in &facts.forwards {
            let callee_fact = self.sigs.param_fact(forward.callee, forward.callee_param);
            for p in carriers.held(&forward.arg) {
                let fact = &mut param_facts[param_position(params, *p)];
                // A callee that only hands the argument back has not let it out of this body. Where
                // the result then goes is the value walk's answer, not the edge's, and it reads the
                // call through the same `hands_back` fact.
                if callee_fact.escapes_beyond_return {
                    fact.escapes = true;
                    fact.escapes_beyond_return = true;
                    fact.escape_site = take_earlier_expr(fact.escape_site, forward.at);
                }
                fact.mutates |= callee_fact.mutates;
                fact.stored_away |= callee_fact.stored_away;
            }
        }
        param_facts
    }

    fn record(&mut self, a: &FnAnalysis) -> bool {
        let mut param_facts = self.fold_param_facts(&a.params, &a.carriers, &a.escape_facts);
        let mut returns_upvalues: Vec<Symbol> = Vec::new();

        for (name, at) in a.escape_facts.returned.iter() {
            for p in a.carriers.held(name) {
                let fact = &mut param_facts[param_position(&a.params, *p)];
                fact.escapes = true;
                if a.carriers.contains(name, p) {
                    fact.escapes_beyond_return = true;
                    fact.escape_site = take_earlier_expr(fact.escape_site, *at);
                }
            }
        }

        for ret in self.returns.get(&a.callable).into_iter().flatten() {
            for (name, kind, at) in self.carried_names(ret) {
                let carried: Vec<Symbol> = a.carriers.held(&name).copied().collect();
                for p in &carried {
                    let fact = &mut param_facts[param_position(&a.params, *p)];
                    fact.hands_back = true;
                    fact.hands_back_itself |= kind == AliasKind::Identity && !a.carriers.contains(&name, p);
                }

                let is_upvalue = matches!(self.bindings.place_of(&at), Some(Place::Upvalue(_)));
                if carried.is_empty() && kind == AliasKind::Identity && is_upvalue {
                    returns_upvalues.push(name);
                }
            }
        }

        returns_upvalues.sort_unstable();
        returns_upvalues.dedup();

        // Every input only ever grows, so param facts that differ from the stored ones have grown.
        let grew = self.sigs.params.get(&a.callable) != Some(&param_facts)
            || self.sigs.returns_upvalues.get(&a.callable) != Some(&returns_upvalues);
        self.sigs.params.insert(a.callable, param_facts);
        self.sigs.returns_upvalues.insert(a.callable, returns_upvalues);
        grew
    }

    fn called_by(&self, callable: CallableId) -> Vec<CallableId> {
        let decl = Signatures::decl_of(self.hir, callable).expect("every collected signature has a declaration");
        let owner = self.method_owner_of(callable);
        let mut out = Vec::new();

        walk::visit_body(self.hir, &decl.body, &mut |node| {
            if let Child::Expr(e) = node {
                if let HirExpr::Call(callee, _) | HirExpr::SafeCall(callee, _) = self.hir.get(&e) {
                    // A constructor is an edge to the factory, so the factory settles first and
                    // the forwarded arguments read a finished summary.
                    if let Some(target) = self.call_target(callee, owner) { out.push(target); }
                }
            }
        });

        // One edge per callee, however many times the body calls it.
        out.sort_unstable_by_key(|c| c.sort_key());
        out.dedup();
        out
    }

    fn declared_callable(&self, callee: &HirId<HirExpr>) -> Option<CallableId> {
        let stmt = self.bindings.declaring_node(callee).and_then(|i| self.hir.stmt_at(i))?;
        match self.hir.get(&stmt) {
            HirStmt::Fn(_) => Some(stmt.into()),
            HirStmt::Say(field) if !field.reassignable => {
                let value = field.value?;
                matches!(self.hir.get(&value), HirExpr::Literal(HirLiteral::Lambda(_))).then(|| value.into())
            },
            _ => None,
        }
    }

    fn resolved_callee(&self, callee: &HirId<HirExpr>, owner: Option<HirId<HirStmt>>) -> Option<CallableId> {
        if self.resolved().type_named(callee).is_some() {
            return None;
        }
        match self.hir.get(callee) {
            HirExpr::Identifier(name) => self.declared_callable(callee)
                .or_else(|| self.sigs.fns_by_name.get(name).copied().map(Into::into)),
            // A `this.method` call resolves within the enclosing type.
            HirExpr::Index(receiver, member, _) => {
                let owner = owner.filter(|_| matches!(self.hir.get(receiver), HirExpr::This))?;
                self.sigs.methods_by_type.get(&(owner, self.member_symbol(member)?)).copied().map(Into::into)
            },
            _ => None,
        }
    }

    fn names_written_by_body(&self, facts: &EscapeFacts) -> HashSet<Symbol> {
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
            if self.sigs.param_escapes_at(f.callee, f.callee_param) || self.sigs.param_mutates_at(f.callee, f.callee_param) {
                writes.extend(carriers.held(&f.arg).copied());
            }
        }
        writes
    }

    fn param_sym(&self, id: &HirId<HirExpr>) -> Symbol {
        match self.hir.get(id) {
            HirExpr::Identifier(sym) => *sym,
            _ => unreachable!("parameter is an identifier"),
        }
    }

    fn carried_symbols(&self, value: &HirId<HirExpr>) -> Vec<Symbol> {
        self.carried_names(value).into_iter().map(|(name, _, _)| name).collect()
    }

    fn carried_names(&self, value: &HirId<HirExpr>) -> Vec<CarriedName> {
        match self.hir.value_source(value) {
            ValueSource::Name(name) => vec![(name, AliasKind::Identity, *value)],
            ValueSource::Receiver => vec![(self.this, AliasKind::Identity, *value)],
            ValueSource::Call(callee, args) => self.call_result_names(callee, args),
            ValueSource::Closure => self.lambda_captures.get(value).into_iter()
                .flat_map(|c| c.iter().map(|name| (*name, AliasKind::Containment, *value)))
                .collect(),
            ValueSource::Yields(children) => children.iter().flat_map(|c| self.carried_names(c)).collect(),
            ValueSource::Holds(children) => children.iter()
                .flat_map(|c| self.carried_names(c))
                .map(|(name, _, at)| (name, AliasKind::Containment, at))
                .collect(),
            ValueSource::Element | ValueSource::Fresh => Vec::new(),
        }
    }

    fn call_result_names(&self, callee: &HirId<HirExpr>, args: &[HirId<HirExpr>]) -> Vec<CarriedName> {
        let Some(func) = self.resolved_callee(callee, None) else { return Vec::new() };
        let mut out = Vec::new();
        for (i, arg) in args.iter().enumerate() {
            let fact = self.sigs.param_fact(func, i);
            if !fact.hands_back { continue; }
            // The result is the argument itself only where the callee hands that argument back
            // rather than a container it built around it.
            out.extend(self.carried_names(arg).into_iter()
                .map(|(name, kind, at)| match fact.hands_back_itself {
                    true => (name, kind, at),
                    false => (name, AliasKind::Containment, at),
                }));
        }
        out
    }

    fn mark_persisted(&self, value: &HirId<HirExpr>, facts: &mut EscapeFacts) {
        facts.direct.note_all(self.carried_symbols(value), *value);
        facts.stored_away.extend(self.carried_symbols(value));
    }

    fn mark_stored(&self, target: &HirId<HirExpr>, value: &HirId<HirExpr>, facts: &mut EscapeFacts) {
        let Some(container) = self.store_root(target) else { return self.mark_persisted(value, facts) };
        for source in self.carried_symbols(value) {
            facts.stores.push((container, source, *value));
        }
        self.mark_held(value, facts);
    }

    fn mark_held(&self, value: &HirId<HirExpr>, facts: &mut EscapeFacts) {
        // A container is a second name for what it holds, whoever ends up reaching it.
        facts.stored_away.extend(self.carried_symbols(value));
    }

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

    fn forward_call_args(&self, callee: &HirId<HirExpr>, args: &[HirId<HirExpr>], facts: &mut EscapeFacts, owner: Option<HirId<HirStmt>>) {
        let Some(target) = self.call_target(callee, owner) else {
            // Nothing is known about what this call takes, so every argument might be retained.
            for arg in args {
                facts.needs_borrow_mark.extend(self.carried_symbols(arg));
            }
            return;
        };
        let markers = self.sigs.fn_sig_of(target).map(|sig| sig.param_markers.as_slice()).unwrap_or(&[]);
        for (callee_param, arg) in args.iter().enumerate() {
            let taken = markers.get(callee_param).is_some_and(|m| m.is_retain());
            for arg_name in self.carried_symbols(arg) {
                if taken {
                    facts.needs_borrow_mark.insert(arg_name);
                }
                facts.forwards.push(EscapeForward { callee: target, callee_param, arg: arg_name, at: *arg });
            }
        }
    }

    fn call_target(&self, callee: &HirId<HirExpr>, owner: Option<HirId<HirStmt>>) -> Option<CallableId> {
        let Some(ty) = self.resolved().type_named(callee) else { return self.resolved_callee(callee, owner) };
        let HirStmt::Type(decl) = self.hir.get(&ty) else { return None };
        Some(decl.init).filter(|init| self.sigs.fn_sig_of(*init).is_some()).map(Into::into)
    }

    fn member_symbol(&self, member: &HirId<HirExpr>) -> Option<Symbol> {
        match self.hir.get(member) {
            HirExpr::Literal(HirLiteral::String(name)) => self.hir.symbol_of(name),
            _ => None,
        }
    }

    fn member_text(&self, member: &HirId<HirExpr>) -> Option<&str> {
        match self.hir.get(member) {
            HirExpr::Literal(HirLiteral::String(name)) => Some(name),
            _ => None,
        }
    }

    fn reachable_into(&self, value: &HirId<HirExpr>, out: &mut HashSet<Symbol>) {
        out.extend(self.carried_symbols(value));
    }

    fn record_names_written_by_call(&self, callee: &HirId<HirExpr>, args: &[HirId<HirExpr>], owner: Option<HirId<HirStmt>>, facts: &mut EscapeFacts) {
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

    fn walk_escapes(&self, expr: &HirId<HirExpr>, facts: &mut EscapeFacts, mode: EscapeCollectMode, owner: Option<HirId<HirStmt>>, caught: bool) {
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
                    facts.mutates.extend(self.carried_symbols(base));
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
                    for (source, kind, _) in self.carried_names(rhs) { facts.aliases.push(Alias { local, source, kind }); }
                }
            },
            HirExpr::Call(callee, args) | HirExpr::SafeCall(callee, args) => if mode == EscapeCollectMode::Escape {
                self.forward_call_args(callee, args, facts, owner);
                self.record_names_written_by_call(callee, args, owner, facts);
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
                Child::Expr(e) => self.walk_escapes(&e, facts, mode, owner, caught),
                Child::Stmt(s) => self.walk_escapes_stmt(&s, facts, mode, owner, caught),
            }
        }
    }

    fn assign_persists(&self, lhs: &HirId<HirExpr>) -> bool {
        match self.hir.get(lhs) {
            HirExpr::Index(..) => true,
            HirExpr::Identifier(_) => !matches!(self.bindings.place_of(lhs), Some(Place::Local(_))),
            _ => false,
        }
    }

    fn walk_escapes_stmt(&self, stmt: &HirId<HirStmt>, facts: &mut EscapeFacts, mode: EscapeCollectMode, owner: Option<HirId<HirStmt>>, caught: bool) {
        match self.hir.get(stmt) {
            HirStmt::Return(Some(e)) => if mode == EscapeCollectMode::Escape {
                for (name, kind, _) in self.carried_names(e) {
                    match kind {
                        AliasKind::Identity => facts.returned.note(name, *e),
                        AliasKind::Containment => facts.direct.note(name, *e),
                    };
                }
            },
            HirStmt::Throw(e) => if mode == EscapeCollectMode::Escape && !caught {
                for (name, _, _) in self.carried_names(e) {
                    facts.direct.note(name, *e);
                }
            },
            HirStmt::Say(field) => {
                facts.bound.insert(field.name);
                if let Some(value) = field.value {
                    // Binding a local to a value aliases it, so a parameter is tracked through the local.
                    let local = field.name;
                    for (source, kind, _) in self.carried_names(&value) { facts.aliases.push(Alias { local, source, kind }); }
                }
            },
            // A nested function is a closure bound to a name. What it captures leaves only as far
            // as that name does.
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
            HirStmt::Try(body, catch, finally) => {
                if let Some(HirCatchClause { param: Some(param), .. }) = catch {
                    if let HirExpr::Identifier(name) = self.hir.get(&param) {
                        facts.bound.insert(*name);
                    }
                }
                self.walk_escapes(body, facts, mode, owner, caught || catch.is_some());
                if let Some(catch) = catch {
                    self.walk_escapes(&catch.body, facts, mode, owner, caught);
                }
                if let Some(finally) = finally {
                    self.walk_escapes(finally, facts, mode, owner, caught);
                }
                return;
            },
            HirStmt::While(cond, _) => facts.bound.extend(self.hir.condition_pattern_binders(cond)),
            HirStmt::If(cond, ..) => facts.bound.extend(self.hir.condition_pattern_binders(cond)),
            _ => {},
        }
        for child in walk::children_of_stmt(self.hir, stmt) {
            match child {
                Child::Expr(e) => self.walk_escapes(&e, facts, mode, owner, caught),
                Child::Stmt(s) => self.walk_escapes_stmt(&s, facts, mode, owner, caught),
            }
        }
    }
}

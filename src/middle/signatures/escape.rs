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

/// The escape structure of one analyzed function, collected by name. A name here is any identifier
/// the body mentions, not only a parameter. `param_carriers` maps each back to the parameters it may
/// hold, and drops the ones that reach none.
#[derive(Default)]
struct EscapeFacts {
    /// Names persisted where the caller cannot see them again: stored, or held by a persisted closure.
    direct: HashSet<Symbol>,
    /// Names the function returns. The caller gets these back, so they never leave its reach.
    returned: HashSet<Symbol>,
    /// Names mutated in place through an index or field write.
    mutates: HashSet<Symbol>,
    /// Names a call writes through its own effect: a native mutate or persist, or a conservative
    /// write by an opaque callee. A known callee's writes ride `forwards` instead.
    call_writes: HashSet<Symbol>,
    /// Forwarding edges to known callees, whose persist depends on the callee's own summary.
    forwards: Vec<EscapeForward>,
    /// `(local, source)` from `say` and assignment.
    aliases: Vec<(Symbol, Symbol)>,
}

/// One forwarding edge: the argument named `arg` escapes if `callee` persists it at its
/// `callee_param` position.
struct EscapeForward {
    callee: HirId<HirStmt>,
    callee_param: usize,
    arg: Symbol,
}

/// One function's body, resolved into what its summaries are computed from.
struct FnAnalysis {
    func: HirId<HirStmt>,
    /// The names the rows are indexed by.
    params: Vec<Symbol>,
    /// Which parameters each name in the body may hold.
    carriers: HashMap<Symbol, HashSet<Symbol>>,
    /// What the body does with each name it mentions.
    facts: EscapeFacts,
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

/// The position of `param` in the analyzed function's parameter list.
fn param_position(params: &[Symbol], param: Symbol) -> usize {
    // Every resolved escape fact names a parameter of that function, so it's always present.
    params.iter().position(|p| *p == param).expect("escape fact names a parameter")
}

/// Maps each name to the parameters whose value it may hold. Every parameter holds itself, and a
/// `say`/assignment alias makes its target hold whatever its source holds. Grown until stable so a
/// chain of aliases carries the parameter all the way through.
fn param_carriers(params: &[Symbol], aliases: &[(Symbol, Symbol)]) -> HashMap<Symbol, HashSet<Symbol>> {
    let mut carriers: HashMap<Symbol, HashSet<Symbol>> = params.iter().map(|p| (*p, HashSet::from([*p]))).collect();
    loop {
        let mut changed = false;
        for (local, source) in aliases {
            let Some(source_params) = carriers.get(source).cloned() else { continue };
            let held = carriers.entry(*local).or_default();
            for p in source_params {
                changed |= held.insert(p);
            }
        }
        if !changed { break; }
    }
    carriers
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

    /// Records escape facts for every lambda.
    pub(super) fn collect_lambda_captures(&mut self) {
        for id in self.hir.lambda_ids() {
            let HirExpr::Literal(HirLiteral::Lambda(decl)) = self.hir.get(&id) else { continue };
            let mut facts = EscapeFacts::default();
            self.walk_escapes(&decl.body, &mut facts, EscapeCollectMode::Capture, None);
            self.lambda_captures.insert(id, facts.direct.into_iter().collect());
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
        let carriers = param_carriers(&params, &facts.aliases);
        FnAnalysis { func, params, carriers, facts }
    }

    /// Folds one body's facts into a row of one entry per parameter. A forwarded argument undergoes
    /// whatever its callee does to it, which the visit order has already settled.
    fn fold_facts(&self, params: &[Symbol], carriers: &HashMap<Symbol, HashSet<Symbol>>, facts: &EscapeFacts) -> Vec<ParamFact> {
        let mut row = vec![ParamFact::default(); params.len()];
        // A directly persisted or mutated argument is kept whatever any callee does.
        for name in &facts.direct {
            for p in carriers.get(name).into_iter().flatten() {
                let fact = &mut row[param_position(params, *p)];
                fact.escapes = true;
                fact.beyond_return = true;
            }
        }
        for name in &facts.mutates {
            for p in carriers.get(name).into_iter().flatten() { row[param_position(params, *p)].mutates = true; }
        }
        for forward in &facts.forwards {
            let callee_fact = self.sigs.param_fact(&forward.callee, forward.callee_param);
            for p in carriers.get(&forward.arg).into_iter().flatten() {
                let fact = &mut row[param_position(params, *p)];
                fact.escapes |= callee_fact.escapes;
                fact.beyond_return |= callee_fact.beyond_return;
                fact.mutates |= callee_fact.mutates;
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
        for name in &a.facts.returned {
            for p in a.carriers.get(name).into_iter().flatten() {
                row[param_position(&a.params, *p)].escapes = true;
            }
        }

        for ret in self.returns.get(&a.func).into_iter().flatten() {
            // A function hands back an argument when its result keeps that argument reachable.
            // `return x`, `return [x]` and `return () => x.n` all count, as does returning a call
            // that itself hands the argument back.
            for name in self.reachable_names(ret) {
                for p in a.carriers.get(&name).into_iter().flatten() {
                    row[param_position(&a.params, *p)].hands_back = true;
                }
            }

            // The narrower fact: the result may be the argument itself, not merely something
            // holding it. `return x` counts and `return [x]` does not.
            for name in self.returned_identity(ret) {
                match a.carriers.get(&name) {
                    Some(carried) => for p in carried {
                        row[param_position(&a.params, *p)].hands_back_itself = true;
                    },
                    // A name no parameter carries comes from an outer scope, so the result is a
                    // second name for a binding the caller may hold too.
                    None => free.push(name),
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
                    if let Some(func) = self.resolved_callee(callee, owner) { out.push(func); }
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
        if self.sigs.type_named(self.hir, self.bindings, callee).is_some() {
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

        for (l, s) in &facts.aliases {
            seeds.insert(*l);
            seeds.insert(*s);
        }

        for f in &facts.forwards {
            seeds.insert(f.arg);
        }

        seeds.extend(&facts.direct);
        seeds.extend(&facts.mutates);
        seeds.extend(&facts.call_writes);

        let seeds: Vec<Symbol> = seeds.into_iter().collect();
        let carriers = param_carriers(&seeds, &facts.aliases);

        let mut writes = HashSet::new();
        for n in facts.direct.iter().chain(&facts.mutates).chain(&facts.call_writes) {
            if let Some(ps) = carriers.get(n) { writes.extend(ps.iter().copied()); }
        }
        for f in &facts.forwards {
            if self.sigs.param_escapes_at(&f.callee, f.callee_param) || self.sigs.param_mutates_at(&f.callee, f.callee_param) {
                if let Some(ps) = carriers.get(&f.arg) { writes.extend(ps.iter().copied()); }
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

            let carriers = param_carriers(&params, &facts.aliases);
            // A lambda publishes only whether it keeps an argument. Returning one hands it back, and
            // a lambda is never resolved at a call site, so the return does not enter this row.
            let row = self.fold_facts(&params, &carriers, &facts);
            self.sigs.lambda_param_escapes.insert(id, row.iter().map(|f| f.escapes).collect());
            let writes = self.body_writes(&facts);
            self.sigs.lambda_writes.insert(id, writes);
        }
    }

    fn param_sym(&self, id: &HirId<HirExpr>) -> Symbol {
        match self.hir.get(id) {
            HirExpr::Identifier(sym) => *sym,
            _ => unreachable!("parameter is an identifier"),
        }
    }

    /// The names a value keeps reachable. An identifier names itself; every other form follows the
    /// shared ownership children, so a call or read reaches nothing while `[x]` reaches `x`.
    fn reachable_names(&self, value: &HirId<HirExpr>) -> Vec<Symbol> {
        if let Some(names) = self.denoted_names(value) {
            return names;
        }
        // A closure holds the names its body reads, so persisting it persists them.
        if let Some(captured) = self.lambda_captures.get(value) {
            return captured.clone();
        }
        self.hir.ownership_children(value).iter().flat_map(|c| self.reachable_names(c)).collect()
    }

    /// The names a value denotes rather than holds, when it denotes any. A call answers through its
    /// own summary, since its result is whatever it handed back.
    fn denoted_names(&self, value: &HirId<HirExpr>) -> Option<Vec<Symbol>> {
        match self.hir.get(value) {
            HirExpr::Identifier(s) => Some(vec![*s]),
            HirExpr::This => Some(vec![self.this]),
            HirExpr::Call(callee, args) => Some(self.call_result_names(callee, args)),
            _ => None,
        }
    }

    /// The names a returned value may itself be, as opposed to hold. Handing one of these back gives
    /// the caller the value it supplied. Anything else wraps the value in a new one that outlives the
    /// lend, so a closure or a container holding a borrow is a persist.
    fn returned_identity(&self, value: &HirId<HirExpr>) -> Vec<Symbol> {
        if let Some(names) = self.denoted_names(value) {
            return names;
        }
        match self.hir.get(value) {
            // Either side of a fallback is a candidate for the one value handed back.
            HirExpr::Coalesce(left, right) | HirExpr::Handle(left, _, right) => {
                let mut names = self.returned_identity(left);
                names.extend(self.returned_identity(right));
                names
            },
            _ => Vec::new(),
        }
    }

    /// Whether a function's result keeps the argument at `param` reachable.
    fn returns_arg(&self, func: &HirId<HirStmt>, param: usize) -> bool {
        self.sigs.param_fact(func, param).hands_back
    }

    /// The names a call's result keeps reachable, for a callee the pass can resolve. An opaque
    /// callee answers nothing, leaving its result to the runtime borrow check.
    fn call_result_names(&self, callee: &HirId<HirExpr>, args: &[HirId<HirExpr>]) -> Vec<Symbol> {
        let Some(func) = self.resolved_callee(callee, None) else { return Vec::new() };
        args.iter().enumerate()
            .filter(|(i, _)| self.returns_arg(&func, *i))
            .flat_map(|(_, arg)| self.reachable_names(arg))
            .collect()
    }

    /// Records every name a persisted value keeps reachable as a direct escape.
    fn mark_persisted(&self, value: &HirId<HirExpr>, facts: &mut EscapeFacts) {
        facts.direct.extend(self.reachable_names(value));
    }

    /// Records how a call reaches its arguments. A known free function forwards each argument to its
    /// matching parameter, a constructor stores its arguments, and a `this.method` call forwards to
    /// the resolved method's parameters.
    fn escapes_at_call(&self, callee: &HirId<HirExpr>, args: &[HirId<HirExpr>], facts: &mut EscapeFacts, owner: Option<HirId<HirStmt>>) {
        // A constructor stores its arguments into the new object, so they persist.
        if self.sigs.type_named(self.hir, self.bindings, callee).is_some() {
            for arg in args { self.mark_persisted(arg, facts); }
            return;
        }
        if let Some(callee_fn) = self.resolved_callee(callee, owner) {
            self.forward_args(callee_fn, args, facts);
        }
    }

    /// Records a forwarding edge from each argument to the matching parameter of `callee`.
    fn forward_args(&self, callee: HirId<HirStmt>, args: &[HirId<HirExpr>], facts: &mut EscapeFacts) {
        for (callee_param, arg) in args.iter().enumerate() {
            for arg_name in self.reachable_names(arg) {
                facts.forwards.push(EscapeForward { callee, callee_param, arg: arg_name });
            }
        }
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
            HirExpr::Identifier(s) => if mode == EscapeCollectMode::Capture { facts.direct.insert(*s); },
            HirExpr::This => if mode == EscapeCollectMode::Capture { facts.direct.insert(self.this); },
            HirExpr::Assign(lhs, rhs) => if mode == EscapeCollectMode::Escape {
                // Writing through an index mutates the base value in place.
                if let HirExpr::Index(base, _, _) = self.hir.get(lhs) {
                    facts.mutates.extend(self.reachable_names(base));
                }
                if self.assign_persists(lhs) {
                    self.mark_persisted(rhs, facts);
                } else if let HirExpr::Identifier(local) = self.hir.get(lhs) {
                    // Rebinding a local aliases it to the value.
                    let local = *local;
                    for source in self.reachable_names(rhs) { facts.aliases.push((local, source)); }
                }
            },
            HirExpr::Call(callee, args) => if mode == EscapeCollectMode::Escape {
                self.escapes_at_call(callee, args, facts, owner);
                self.mark_call_writes(callee, args, owner, facts);
            },
            HirExpr::Construct(_, args, brace) => if mode == EscapeCollectMode::Escape {
                for arg in args { self.mark_persisted(arg, facts); }
                for (_, value) in brace { self.mark_persisted(value, facts); }
            },
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
                let handed: HashSet<Symbol> = self.returned_identity(e).into_iter().collect();
                for name in self.reachable_names(e) {
                    match handed.contains(&name) {
                        true => facts.returned.insert(name),
                        false => facts.direct.insert(name),
                    };
                }
            },
            HirStmt::Say(field) => if let Some(value) = field.value {
                // Binding a local to a value aliases it, so a parameter is tracked through the local.
                for source in self.reachable_names(&value) { facts.aliases.push((field.name, source)); }
            },
            // A nested function captures the names it references, so walk it in Capture mode. The
            // shared child walk treats it as a leaf.
            HirStmt::Fn(decl) => self.walk_escapes(&decl.body, facts, EscapeCollectMode::Capture, None),
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

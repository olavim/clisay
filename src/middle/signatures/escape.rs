//! Escape analysis: per parameter, whether a function persists its argument. A persist is a store,
//! return, capture, construct, or a forward to a callee that itself persists.

use std::collections::{HashMap, HashSet};

use crate::middle::bind::Place;
use crate::middle::hir::{HirExpr, HirId, HirLiteral, HirStmt, Symbol};
use crate::middle::native;

use super::Collector;
use super::walk::Child;

/// How the escape walk counts a parameter reference.
#[derive(Clone, Copy, PartialEq)]
enum EscapeCollectMode {
    /// Counts only references that escape the current function.
    Escape,
    /// Counts every reference, to mark a parameter as captured by a nested function or lambda.
    Capture,
}

/// The escape structure of one analyzed function.
#[derive(Default)]
struct EscapeFacts {
    /// Names persisted directly: stored, returned, or captured.
    direct: HashSet<Symbol>,
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

/// One function's resolved escape and mutation inputs.
struct EscapeJob {
    func: HirId<HirStmt>,
    params: Vec<Symbol>,
    forwards: Vec<(HirId<HirStmt>, usize, Symbol)>,
    facts: EscapeFacts,
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
    /// Infers, per parameter, whether a function persists its argument and whether it mutates it in place.
    /// Each body is walked once to collect its escape facts by name. Moves to dynamic boundary calls are
    /// deferred to runtime borrow checks.
    pub(super) fn infer_param_escapes(&mut self) {
        let funcs: Vec<HirId<HirStmt>> = self.sigs.fns.keys().copied().collect();
        let mut jobs: Vec<EscapeJob> = Vec::new();
        for func in funcs {
            let HirStmt::Fn(decl) = self.hir.get(&func) else { continue };
            let params: Vec<Symbol> = decl.params.iter().map(|p| self.param_sym(&p.name)).collect();
            let owner = self.sigs.method_owner.get(&func).copied();
            let mut facts = EscapeFacts::default();
            self.walk_escapes(&decl.body, &mut facts, EscapeCollectMode::Escape, owner);

            let carriers = param_carriers(&params, &facts.aliases);
            // A directly persisted or mutated parameter escapes or mutates no matter what any callee
            // does, so seed its bit now. Only forwarding depends on other jobs, so it waits.
            let mut escapes = vec![false; params.len()];
            let mut mutates = vec![false; params.len()];
            for n in &facts.direct {
                for p in carriers.get(n).into_iter().flatten() { escapes[param_position(&params, *p)] = true; }
            }
            for n in &facts.mutates {
                for p in carriers.get(n).into_iter().flatten() { mutates[param_position(&params, *p)] = true; }
            }
            let forwards: Vec<(HirId<HirStmt>, usize, Symbol)> = facts.forwards.iter()
                .filter_map(|f| carriers.get(&f.arg).map(|ps| (f, ps)))
                .flat_map(|(f, ps)| ps.iter().map(move |p| (f.callee, f.callee_param, *p)))
                .collect();
            self.sigs.param_escapes.insert(func, escapes);
            self.sigs.param_mutates.insert(func, mutates);
            jobs.push(EscapeJob { func, params, forwards, facts });
        }

        // A forwarded parameter escapes or mutates once its callee does at the matching position.
        // Propagate the edges until a full pass adds nothing, settling the mutually recursive cases.
        loop {
            let mut changed = false;
            for job in &jobs {
                for &(callee, callee_param, param) in &job.forwards {
                    let pos = param_position(&job.params, param);
                    if self.sigs.param_escapes_at(&callee, callee_param) {
                        let row = self.sigs.param_escapes.get_mut(&job.func).unwrap();
                        if !row[pos] {
                            row[pos] = true;
                            changed = true;
                        }
                    }
                    if self.sigs.param_mutates_at(&callee, callee_param) {
                        let row = self.sigs.param_mutates.get_mut(&job.func).unwrap();
                        if !row[pos] {
                            row[pos] = true;
                            changed = true;
                        }
                    }
                }
            }
            if !changed { break; }
        }

        // Once the masks settle, record the names each body writes so a read-only capture can be
        // told from a writing one.
        for job in &jobs {
            let writes = self.body_writes(&job.facts);
            self.sigs.writes.insert(job.func, writes);
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
            let mut row = vec![false; params.len()];
            for name in &facts.direct {
                if let Some(ps) = carriers.get(name) {
                    for p in ps { row[param_position(&params, *p)] = true; }
                }
            }
            for f in &facts.forwards {
                if self.sigs.param_escapes_at(&f.callee, f.callee_param) {
                    if let Some(ps) = carriers.get(&f.arg) {
                        for p in ps { row[param_position(&params, *p)] = true; }
                    }
                }
            }
            self.sigs.lambda_param_escapes.insert(id, row);
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
        if let HirExpr::Identifier(s) = self.hir.get(value) {
            return vec![*s];
        }
        self.hir.ownership_children(value).iter().flat_map(|c| self.reachable_names(c)).collect()
    }

    /// Records every name a persisted value keeps reachable as a direct escape.
    fn mark_persisted(&self, value: &HirId<HirExpr>, facts: &mut EscapeFacts) {
        facts.direct.extend(self.reachable_names(value));
    }

    /// Records how a call reaches its arguments. A known free function forwards each argument to its
    /// matching parameter, a constructor stores its arguments, and a `this.method` call forwards to
    /// the resolved method's parameters.
    fn escapes_at_call(&self, callee: &HirId<HirExpr>, args: &[HirId<HirExpr>], facts: &mut EscapeFacts, owner: Option<Symbol>) {
        match self.hir.get(callee) {
            // A constructor stores its arguments into the new object, so they persist.
            HirExpr::Identifier(name) if self.sigs.is_type(*name) => {
                for arg in args { self.mark_persisted(arg, facts); }
            },
            HirExpr::Identifier(name) => {
                if let Some(callee_fn) = self.sigs.fns_by_name.get(name) {
                    self.forward_args(*callee_fn, args, facts);
                }
            },
            // A `this.method` call resolves within the enclosing type, so it forwards like a
            // free-function call.
            HirExpr::Index(receiver, member, _) => {
                if let (Some(owner), HirExpr::This) = (owner, self.hir.get(receiver)) {
                    if let Some(method) = self.member_symbol(member) {
                        if let Some(callee_fn) = self.sigs.methods_by_type.get(&(owner, method)) {
                            self.forward_args(*callee_fn, args, facts);
                        }
                    }
                }
            },
            _ => {},
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
    fn mark_call_writes(&self, callee: &HirId<HirExpr>, args: &[HirId<HirExpr>], owner: Option<Symbol>, facts: &mut EscapeFacts) {
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
    fn walk_escapes(&self, expr: &HirId<HirExpr>, facts: &mut EscapeFacts, mode: EscapeCollectMode, owner: Option<Symbol>) {
        // Record what this node contributes, then recurse through the shared child structure.
        match self.hir.get(expr) {
            HirExpr::Identifier(s) => if mode == EscapeCollectMode::Capture { facts.direct.insert(*s); },
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
            // A lambda captures the names it references, so walk its body in Capture mode. The
            // shared child walk treats it as a leaf, so this does not double-descend.
            HirExpr::Literal(HirLiteral::Lambda(decl)) => self.walk_escapes(&decl.body, facts, EscapeCollectMode::Capture, None),
            _ => {},
        }
        for child in self.children_of_expr(expr) {
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

    fn walk_escapes_stmt(&self, stmt: &HirId<HirStmt>, facts: &mut EscapeFacts, mode: EscapeCollectMode, owner: Option<Symbol>) {
        match self.hir.get(stmt) {
            // Returning the value hands it out of the scope.
            HirStmt::Return(Some(e)) => if mode == EscapeCollectMode::Escape { self.mark_persisted(e, facts); },
            HirStmt::Say(field) => if let Some(value) = field.value {
                // Binding a local to a value aliases it, so a parameter is tracked through the local.
                for source in self.reachable_names(&value) { facts.aliases.push((field.name, source)); }
            },
            // A nested function captures the names it references, so walk it in Capture mode. The
            // shared child walk treats it as a leaf.
            HirStmt::Fn(decl) => self.walk_escapes(&decl.body, facts, EscapeCollectMode::Capture, None),
            _ => {},
        }
        for child in self.children_of_stmt(stmt) {
            match child {
                Child::Expr(e) => self.walk_escapes(&e, facts, mode, owner),
                Child::Stmt(s) => self.walk_escapes_stmt(&s, facts, mode, owner),
            }
        }
    }
}

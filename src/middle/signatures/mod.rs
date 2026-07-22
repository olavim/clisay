//! Builds a `Signatures` table. Records every function and type-member signature
//! and infers each function's return tag.

use std::collections::{HashMap, HashSet};

use crate::middle::hir::{Capability, Hir, HirExpr, HirFnDecl, HirId, HirLiteral, HirStmt, HirTypeDecl, ObligationRule, ReturnShape, Symbol};

/// A function's return: the obligations its result carries and whether any path returns a value.
#[derive(Clone, Default)]
pub struct RetSig {
    pub obligations: HashSet<Symbol>,
    pub void: bool,
}

/// A function's per-parameter obligation set and its return signature.
pub struct FnSig {
    pub param_clauses: Vec<HashSet<Symbol>>,
    pub param_markers: Vec<Capability>,
    pub ret: RetSig,
}

/// The value-mutability a value carries as it flows: the capability lattice the check pass tracks,
/// distinct from `Capability`, the syntactic `mut`/`*mut` marker a clause declares.
#[derive(Clone, Copy, PartialEq, Eq, Default)]
pub enum Mutability {
    /// A `mut` parameter or a `: mut` return: the value may be mutated.
    Mutable,
    /// Frozen, or an untagged return auto-frozen on the way out.
    Immutable,
    #[default]
    Unknown,
}

impl Mutability {
    /// The capability a parameter's clause marker grants its binding.
    pub fn param(capability: Capability) -> Mutability {
        if capability.is_mut() { Mutability::Mutable } else { Mutability::Unknown }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub enum TypeTag {
    Concrete(Symbol),
    SelfType,
    Unknown,
}

impl TypeTag {
    pub(crate) fn resolve(&self, receiver: &TypeTag) -> TypeTag {
        match self {
            TypeTag::SelfType => receiver.clone(),
            other => other.clone(),
        }
    }
}

/// How a running program tells that a slot still owes an obligation. `null` is the built-in value
/// witness; a type witness is tested by tag, a trait witness by trait-set membership.
#[derive(Clone)]
pub enum Witness {
    Null,
    Type(Symbol),
    Trait(Symbol),
}

pub struct Signatures {
    pub(crate) opt: Symbol,
    pub(crate) fails: Symbol,
    /// Each obligation's witness. Built-ins are seeded here; user obligations extend it.
    pub(crate) witnesses: HashMap<Symbol, Witness>,
    /// Each user obligation's rule.
    pub(crate) rules: HashMap<Symbol, ObligationRule>,

    // Per-function facts, keyed by the function's statement.
    pub(crate) fns: HashMap<HirId<HirStmt>, FnSig>,
    pub(crate) ret_tags: HashMap<HirId<HirStmt>, TypeTag>,
    pub(crate) ret_mut: HashMap<HirId<HirStmt>, Mutability>,

    // Name-to-declaration lookups.
    pub(crate) types_by_name: HashMap<Symbol, HirId<HirStmt>>,
    pub(crate) fns_by_name: HashMap<Symbol, HirId<HirStmt>>,
    pub(crate) methods_by_type: HashMap<(Symbol, Symbol), HirId<HirStmt>>,

    // Per-type field facts.
    /// Type name to the fields its `init` assigns directly.
    pub(crate) init_fields: HashMap<Symbol, HashSet<Symbol>>,
    /// Type name to the fields its methods assign, each mapped to the assigning node.
    pub(crate) method_field_assigns: HashMap<Symbol, HashMap<Symbol, HirId<HirExpr>>>,
}

impl Signatures {
    fn new(opt: Symbol, fails: Symbol) -> Signatures {
        Signatures {
            opt,
            fails,
            witnesses: HashMap::from([(opt, Witness::Null)]),
            rules: HashMap::new(),
            fns: HashMap::new(),
            ret_tags: HashMap::new(),
            ret_mut: HashMap::new(),
            types_by_name: HashMap::new(),
            fns_by_name: HashMap::new(),
            methods_by_type: HashMap::new(),
            init_fields: HashMap::new(),
            method_field_assigns: HashMap::new(),
        }
    }

    /// Whether `name` names a declared type.
    pub(crate) fn is_type(&self, name: Symbol) -> bool {
        self.types_by_name.contains_key(&name)
    }

    /// The witness of an obligation, when one is known.
    pub(crate) fn witness(&self, obligation: Symbol) -> Option<&Witness> {
        self.witnesses.get(&obligation)
    }

    /// Every registered object witness as `(obligation, witness type/trait name)`. The null
    /// witness of `opt` is excluded, since it is tested by the null op, not the `is` test.
    pub(crate) fn object_witnesses(&self) -> impl Iterator<Item = (Symbol, Symbol)> + '_ {
        self.witnesses.iter().filter_map(|(ob, w)| match w {
            Witness::Type(name) | Witness::Trait(name) => Some((*ob, *name)),
            Witness::Null => None,
        })
    }

    /// Whether an obligation's rule is `discharge to escape`: usable in place, but not persistable.
    pub(crate) fn is_to_escape(&self, obligation: Symbol) -> bool {
        matches!(self.rules.get(&obligation), Some(ObligationRule::ToEscape))
    }

    pub(crate) fn obligation_for_witness(&self, name: Symbol) -> Option<Symbol> {
        self.witnesses.iter().find_map(|(ob, w)| match w {
            Witness::Type(t) | Witness::Trait(t) if *t == name => Some(*ob),
            _ => None,
        })
    }

    /// Whether `name` is the type witness of some obligation.
    pub(crate) fn is_witness_type(&self, name: Symbol) -> bool {
        self.witnesses.values().any(|w| matches!(w, Witness::Type(t) if *t == name))
    }

    /// The type a callee names, when it is an identifier naming a declared type.
    pub(crate) fn type_named(&self, hir: &Hir, callee: &HirId<HirExpr>) -> Option<Symbol> {
        match hir.get(callee) {
            HirExpr::Identifier(name) if self.is_type(*name) => Some(*name),
            _ => None,
        }
    }
}

/// Collects the program's signatures and inferred return type tags.
pub fn collect(hir: &Hir) -> Signatures {
    let opt = hir.symbol_of("opt").expect("lowering interns the opt obligation");
    let fails = hir.symbol_of("fails").expect("lowering interns the fails obligation");
    let err = hir.symbol_of("Err");
    let mut sigs = Signatures::new(opt, fails);
    if let Some(err) = err {
        sigs.witnesses.insert(fails, Witness::Type(err));
    }
    let mut collector = Collector { hir, opt, fails, err, sigs, returns: HashMap::new() };
    collector.stmt(&hir.get_root());
    collector.register_obligations();
    collector.collect_all_returns();
    collector.infer_ret_tags();
    collector.infer_ret_mut();
    collector.infer_propagated();
    collector.sigs
}

struct Collector<'a> {
    hir: &'a Hir,
    opt: Symbol,
    fails: Symbol,
    err: Option<Symbol>,
    sigs: Signatures,
    returns: HashMap<HirId<HirStmt>, Vec<HirId<HirExpr>>>,
}

impl<'a> Collector<'a> {
    fn stmt(&mut self, stmt: &HirId<HirStmt>) {
        match self.hir.get(stmt) {
            HirStmt::Fn(decl) => {
                let sig = self.fn_sig(decl);
                self.sigs.fns.insert(*stmt, sig);
                self.sigs.fns_by_name.insert(decl.name, *stmt);
                self.expr(&decl.body);
            },
            HirStmt::Type(decl) => {
                self.sigs.types_by_name.insert(decl.name, *stmt);
                let init_fields = self.init_fields(decl);
                self.sigs.init_fields.insert(decl.name, init_fields);
                let method_field_assigns = self.method_field_assigns(decl);
                self.sigs.method_field_assigns.insert(decl.name, method_field_assigns);
                self.collect_sig(&decl.init);
                for method in &decl.methods {
                    if let HirStmt::Fn(m) = self.hir.get(method) {
                        self.sigs.methods_by_type.insert((decl.name, m.name), *method);
                    }
                    self.collect_sig(method);
                }
            },
            HirStmt::Trait(_) => {},
            HirStmt::Expression(e) | HirStmt::Throw(e) | HirStmt::Block(e) => self.expr(e),
            HirStmt::Return(opt) => if let Some(e) = opt { self.expr(e); },
            HirStmt::While(cond, body) => { self.expr(cond); self.expr(body); },
            HirStmt::If(cond, then, otherwise) => {
                self.expr(cond);
                self.expr(then);
                if let Some(otherwise) = otherwise { self.stmt(otherwise); }
            },
            HirStmt::Try(body, catch, finally) => {
                self.expr(body);
                if let Some(catch) = catch { self.expr(&catch.body); }
                if let Some(finally) = finally { self.expr(finally); }
            },
            HirStmt::Say(field) => if let Some(value) = field.value { self.expr(&value); },
            HirStmt::Match(scrutinee, arms) => {
                self.expr(scrutinee);
                for arm in arms {
                    if let Some(guard) = &arm.guard { self.expr(guard); }
                    self.expr(&arm.body);
                }
            },
            HirStmt::Nop => {},
        }
    }

    /// Registers each user obligation's witness and rule.
    fn register_obligations(&mut self) {
        for (name, decl) in self.hir.obligations() {
            self.sigs.rules.insert(name, decl.rule);
            if let Some(witness) = decl.witness {
                let w = if self.sigs.is_type(witness) {
                    Witness::Type(witness)
                } else {
                    Witness::Trait(witness)
                };
                self.sigs.witnesses.insert(name, w);
            }
        }
    }

    /// Records a method's or initializer's signature and recurses into its body.
    fn collect_sig(&mut self, stmt: &HirId<HirStmt>) {
        if let HirStmt::Fn(decl) = self.hir.get(stmt) {
            let sig = self.fn_sig(decl);
            self.sigs.fns.insert(*stmt, sig);
            self.expr(&decl.body);
        }
    }

    fn fn_sig(&self, decl: &HirFnDecl) -> FnSig {
        let mut ret = self.ret_sig(decl);
        if self.body_fails(&decl.body) {
            ret.obligations.insert(self.fails);
        }
        FnSig {
            param_clauses: decl.params.iter().map(|p| p.clause.names.iter().copied().collect()).collect(),
            param_markers: decl.params.iter().map(|p| p.clause.capability).collect(),
            ret,
        }
    }

    fn body_fails(&self, body: &HirId<HirExpr>) -> bool {
        let mut returns = Vec::new();
        self.collect_returns(body, &mut returns);
        returns.iter().any(|r| self.is_err_call(r))
    }

    fn is_err_call(&self, expr: &HirId<HirExpr>) -> bool {
        let HirExpr::Call(callee, _) = self.hir.get(expr) else { return false };
        matches!(self.hir.get(callee), HirExpr::Identifier(name) if Some(*name) == self.err)
    }

    /// Maps a function's declared return onto its obligation set and value presence. A marked return
    /// owes exactly its clause obligations. An unmarked return infers its presence from the body, and
    /// its obligations are filled by the propagation fixpoint.
    fn ret_sig(&self, decl: &HirFnDecl) -> RetSig {
        if decl.is_unmarked() {
            return RetSig { obligations: HashSet::new(), void: self.has_void_path(&decl.body) };
        }
        // A synthesized forwarder carries a `?` marker with no clause, so honor the marker too.
        let mut obligations: HashSet<Symbol> = decl.clause.names.iter().copied().collect();
        if decl.ret == ReturnShape::Nullable {
            obligations.insert(self.opt);
        }
        RetSig { obligations, void: decl.ret == ReturnShape::Void }
    }

    /// Whether a function body can finish without returning a value: it falls off the end, or it
    /// has a bare `return;`.
    fn has_void_path(&self, body: &HirId<HirExpr>) -> bool {
        !self.hir.definitely_returns(body) || self.has_bare_return(body)
    }

    /// Whether a body contains a bare `return;` outside any nested function.
    fn has_bare_return(&self, body: &HirId<HirExpr>) -> bool {
        match self.hir.get(body) {
            HirExpr::Block(stmts) => stmts.iter().any(|s| self.stmt_has_bare_return(s)),
            _ => false,
        }
    }

    fn stmt_has_bare_return(&self, stmt: &HirId<HirStmt>) -> bool {
        match self.hir.get(stmt) {
            HirStmt::Return(None) => true,
            HirStmt::Block(e) => self.has_bare_return(e),
            HirStmt::While(_, body) => self.has_bare_return(body),
            HirStmt::If(_, then, otherwise) => {
                self.has_bare_return(then) || otherwise.as_ref().is_some_and(|o| self.stmt_has_bare_return(o))
            },
            HirStmt::Try(body, catch, finally) => {
                self.has_bare_return(body)
                    || catch.as_ref().is_some_and(|c| self.has_bare_return(&c.body))
                    || finally.as_ref().is_some_and(|f| self.has_bare_return(f))
            },
            HirStmt::Match(_, arms) => arms.iter().any(|a| self.has_bare_return(&a.body)),
            _ => false,
        }
    }

    fn expr(&mut self, expr: &HirId<HirExpr>) {
        match self.hir.get(expr) {
            HirExpr::Block(stmts) => for s in stmts { self.stmt(s); },
            HirExpr::Unary(_, x) | HirExpr::Is(x, _) | HirExpr::Assert(x) | HirExpr::Propagate(x)
            | HirExpr::Has(x, _) | HirExpr::Match(x, _) | HirExpr::Mut(x) => self.expr(x),
            HirExpr::Binary(_, l, r) | HirExpr::Assign(l, r) | HirExpr::Coalesce(l, r)
            | HirExpr::Handle(l, _, r) | HirExpr::SafeAccess(l, r, _) | HirExpr::Index(l, r, _) => { self.expr(l); self.expr(r); },
            HirExpr::Call(callee, args) | HirExpr::SafeCall(callee, args) => {
                self.expr(callee);
                for a in args { self.expr(a); }
            },
            HirExpr::Construct(callee, args, brace) => {
                self.expr(callee);
                for a in args { self.expr(a); }
                for (_, v) in brace { self.expr(v); }
            },
            HirExpr::Literal(lit) => self.literal(lit),
            HirExpr::Identifier(_) | HirExpr::This => {},
        }
    }

    fn literal(&mut self, lit: &HirLiteral) {
        match lit {
            HirLiteral::Array(elems) => for e in elems { self.expr(e); },
            HirLiteral::Dict(pairs) => for (k, v) in pairs { self.expr(k); self.expr(v); },
            HirLiteral::Lambda(decl) => self.expr(&decl.body),
            _ => {},
        }
    }

    /// Infers every function's return type tag to a fixpoint, so a call to a factory or a
    /// function that calls one resolves its type.
    fn infer_ret_tags(&mut self) {
        let stmts: Vec<HirId<HirStmt>> = self.sigs.fns.keys().copied().collect();
        for stmt in &stmts {
            self.sigs.ret_tags.insert(*stmt, TypeTag::Unknown);
        }
        loop {
            let mut changed = false;
            for stmt in &stmts {
                let tag = self.infer_body_tag(&self.returns[stmt]);
                if self.sigs.ret_tags.get(stmt) != Some(&tag) {
                    self.sigs.ret_tags.insert(*stmt, tag);
                    changed = true;
                }
            }
            if !changed {
                break;
            }
        }
    }

    fn infer_ret_mut(&mut self) {
        let stmts: Vec<HirId<HirStmt>> = self.sigs.fns.keys().copied().collect();
        for stmt in &stmts {
            self.sigs.ret_mut.insert(*stmt, Mutability::Unknown);
        }
        let mut changed = true;
        while changed {
            changed = false;
            for stmt in &stmts {
                let HirStmt::Fn(decl) = self.hir.get(stmt) else { continue };
                let mutability = self.ret_mut_of(decl, &self.returns[stmt]);
                changed |= self.sigs.ret_mut.insert(*stmt, mutability) != Some(mutability);
            }
        }
    }

    /// Walks each function body once, so the tag and mutability passes share the return list.
    fn collect_all_returns(&mut self) {
        let stmts: Vec<HirId<HirStmt>> = self.sigs.fns.keys().copied().collect();
        for stmt in stmts {
            let HirStmt::Fn(decl) = self.hir.get(&stmt) else { continue };
            let mut returns = Vec::new();
            self.collect_returns(&decl.body, &mut returns);
            self.returns.insert(stmt, returns);
        }
    }

    /// A function's return mutability, inferred from its body.
    fn ret_mut_of(&self, decl: &HirFnDecl, returns: &[HirId<HirExpr>]) -> Mutability {
        if decl.clause.capability.is_mut() {
            return Mutability::Mutable;
        }
        if !returns.is_empty() && returns.iter().all(|r| self.returns_mutable(r)) {
            Mutability::Mutable
        } else {
            Mutability::Unknown
        }
    }

    /// Whether a return hands back a statically-mutable value: a `mut`-minted construction or a call
    /// to a function inferred to return a mutable.
    fn returns_mutable(&self, expr: &HirId<HirExpr>) -> bool {
        match self.hir.get(expr) {
            HirExpr::Mut(_) => true,
            HirExpr::Call(callee, _) => {
                let HirExpr::Identifier(name) = self.hir.get(callee) else { return false };
                let Some(stmt) = self.sigs.fns_by_name.get(name) else { return false };
                self.sigs.ret_mut.get(stmt) == Some(&Mutability::Mutable)
            },
            _ => false,
        }
    }

    /// The joined return type tag of a body: a single tag if every return agrees, else unknown.
    fn infer_body_tag(&self, returns: &[HirId<HirExpr>]) -> TypeTag {
        let mut joined: Option<TypeTag> = None;
        for ret in returns {
            let tag = self.classify_return(ret);
            joined = Some(match joined {
                None => tag,
                Some(prev) if prev == tag => prev,
                Some(_) => TypeTag::Unknown,
            });
        }
        joined.unwrap_or(TypeTag::Unknown)
    }

    fn classify_return(&self, expr: &HirId<HirExpr>) -> TypeTag {
        match self.hir.get(expr) {
            HirExpr::This => TypeTag::SelfType,
            // A `: mut` factory returns `mut Ctor()`, so classify the wrapped construction.
            HirExpr::Mut(inner) => self.classify_return(inner),
            HirExpr::Construct(callee, _, _) => {
                self.sigs.type_named(self.hir, callee).map_or(TypeTag::Unknown, TypeTag::Concrete)
            },
            HirExpr::Call(callee, _) => match self.hir.get(callee) {
                HirExpr::Identifier(name) if self.sigs.is_type(*name) => TypeTag::Concrete(*name),
                HirExpr::Identifier(name) => self.sigs.fns_by_name.get(name)
                    .and_then(|stmt| self.sigs.ret_tags.get(stmt).cloned())
                    .unwrap_or(TypeTag::Unknown),
                _ => TypeTag::Unknown,
            },
            _ => TypeTag::Unknown,
        }
    }

    fn collect_returns(&self, expr: &HirId<HirExpr>, out: &mut Vec<HirId<HirExpr>>) {
        if let HirExpr::Block(stmts) = self.hir.get(expr) {
            for stmt in stmts {
                self.collect_returns_stmt(stmt, out);
            }
        }
    }

    fn collect_returns_stmt(&self, stmt: &HirId<HirStmt>, out: &mut Vec<HirId<HirExpr>>) {
        match self.hir.get(stmt) {
            HirStmt::Return(Some(e)) => out.push(*e),
            HirStmt::Block(e) => self.collect_returns(e, out),
            HirStmt::While(_, body) => self.collect_returns(body, out),
            HirStmt::If(_, then, otherwise) => {
                self.collect_returns(then, out);
                if let Some(otherwise) = otherwise { self.collect_returns_stmt(otherwise, out); }
            },
            HirStmt::Try(body, catch, finally) => {
                self.collect_returns(body, out);
                if let Some(catch) = catch { self.collect_returns(&catch.body, out); }
                if let Some(finally) = finally { self.collect_returns(finally, out); }
            },
            // A nested function's returns belong to that function, not this one.
            _ => {},
        }
    }

    /// Adds each `?!` operand's obligations to the enclosing function's return set.
    fn infer_propagated(&mut self) {
        let stmts: Vec<HirId<HirStmt>> = self.sigs.fns.keys().copied().collect();
        loop {
            let mut changed = false;
            for stmt in &stmts {
                let HirStmt::Fn(decl) = self.hir.get(stmt) else { continue };
                let mut operands = Vec::new();
                self.collect_propagates(&decl.body, &mut operands);
                let mut add = HashSet::new();
                for operand in operands {
                    add.extend(self.operand_obligations(&operand, decl));
                }
                // An unmarked function also carries the obligations of each value it returns.
                if decl.is_unmarked() {
                    let mut returns = Vec::new();
                    self.collect_returns(&decl.body, &mut returns);
                    for ret in returns {
                        add.extend(self.operand_obligations(&ret, decl));
                    }
                }
                let sig = self.sigs.fns.get_mut(stmt).unwrap();
                for ob in add {
                    if sig.ret.obligations.insert(ob) { changed = true; }
                }
            }
            if !changed { break; }
        }
    }

    /// The obligations a `?!` operand carries. This mirrors the check pass's `chain_result`, so a
    /// chain does not launder an object witness out of the propagated set.
    fn operand_obligations(&self, operand: &HirId<HirExpr>, decl: &HirFnDecl) -> HashSet<Symbol> {
        match self.hir.get(operand) {
            HirExpr::Call(callee, _) => {
                if self.is_err_call(operand) {
                    return HashSet::from([self.fails]);
                }
                match self.hir.get(callee) {
                    HirExpr::Identifier(name) => self.sigs.fns_by_name.get(name)
                        .map(|s| self.sigs.fns[s].ret.obligations.clone())
                        .unwrap_or_default(),
                    _ => HashSet::new(),
                }
            },
            // A `?` chain carries its operand's obligations from the guarded access.
            HirExpr::SafeAccess(target, _, _) | HirExpr::SafeCall(target, _) => {
                let mut set = self.operand_obligations(target, decl);
                set.insert(self.opt);
                set
            },
            HirExpr::Literal(HirLiteral::Null) => HashSet::from([self.opt]),
            HirExpr::Identifier(name) => self.param_obligations(*name, decl),
            _ => HashSet::new(),
        }
    }

    /// The declared obligation set of `name` when it is a parameter of `decl`.
    fn param_obligations(&self, name: Symbol, decl: &HirFnDecl) -> HashSet<Symbol> {
        for p in &decl.params {
            if matches!(self.hir.get(&p.name), HirExpr::Identifier(pname) if *pname == name) {
                return p.clause.names.iter().copied().collect();
            }
        }
        HashSet::new()
    }

    /// Collects each `?!` operand in a body, skipping nested function and lambda bodies.
    fn collect_propagates(&self, expr: &HirId<HirExpr>, out: &mut Vec<HirId<HirExpr>>) {
        match self.hir.get(expr) {
            HirExpr::Propagate(operand) => { out.push(*operand); self.collect_propagates(operand, out); },
            HirExpr::Unary(_, x) | HirExpr::Is(x, _) | HirExpr::Assert(x) | HirExpr::Has(x, _) | HirExpr::Match(x, _) | HirExpr::Mut(x) => self.collect_propagates(x, out),
            HirExpr::Binary(_, l, r) | HirExpr::Assign(l, r) | HirExpr::Coalesce(l, r) | HirExpr::Handle(l, _, r)
            | HirExpr::SafeAccess(l, r, _) | HirExpr::Index(l, r, _) => { 
                self.collect_propagates(l, out); 
                self.collect_propagates(r, out); 
            },
            HirExpr::Call(callee, args) | HirExpr::SafeCall(callee, args) => {
                self.collect_propagates(callee, out);
                for a in args { self.collect_propagates(a, out); }
            },
            HirExpr::Construct(callee, args, brace) => {
                self.collect_propagates(callee, out);
                for a in args { self.collect_propagates(a, out); }
                for (_, v) in brace { self.collect_propagates(v, out); }
            },
            HirExpr::Block(stmts) => for s in stmts { self.collect_propagates_stmt(s, out); },
            HirExpr::Literal(HirLiteral::Array(elems)) => for e in elems { self.collect_propagates(e, out); },
            HirExpr::Literal(HirLiteral::Dict(pairs)) => for (k, v) in pairs { 
                self.collect_propagates(k, out); 
                self.collect_propagates(v, out); 
            },
            HirExpr::Literal(_) | HirExpr::Identifier(_) | HirExpr::This => {},
        }
    }

    fn collect_propagates_stmt(&self, stmt: &HirId<HirStmt>, out: &mut Vec<HirId<HirExpr>>) {
        match self.hir.get(stmt) {
            HirStmt::Expression(e) | HirStmt::Throw(e) | HirStmt::Block(e) => self.collect_propagates(e, out),
            HirStmt::Return(opt) => if let Some(e) = opt { self.collect_propagates(e, out); },
            HirStmt::While(cond, body) => { 
                self.collect_propagates(cond, out); 
                self.collect_propagates(body, out); 
            },
            HirStmt::If(cond, then, otherwise) => {
                self.collect_propagates(cond, out);
                self.collect_propagates(then, out);
                if let Some(otherwise) = otherwise { self.collect_propagates_stmt(otherwise, out); }
            },
            HirStmt::Try(body, catch, finally) => {
                self.collect_propagates(body, out);
                if let Some(catch) = catch { self.collect_propagates(&catch.body, out); }
                if let Some(finally) = finally { self.collect_propagates(finally, out); }
            },
            HirStmt::Say(field) => if let Some(value) = field.value { self.collect_propagates(&value, out); },
            HirStmt::Match(scrutinee, arms) => {
                self.collect_propagates(scrutinee, out);
                for arm in arms {
                    if let Some(guard) = &arm.guard { self.collect_propagates(guard, out); }
                    self.collect_propagates(&arm.body, out);
                }
            },
            // Nested declarations are separate functions.
            _ => {},
        }
    }

    /// The fields a type's `init` assigns directly: defaults, `this.f =`, and bare `f =`.
    /// Assignments inside a called helper do not count, since the helper is opaque to init.
    fn init_fields(&self, decl: &HirTypeDecl) -> HashSet<Symbol> {
        let mut assigns = Vec::new();
        if let HirStmt::Fn(init) = self.hir.get(&decl.init) {
            self.scan_field_assigns(&init.body, &decl.fields, &mut assigns);
        }
        assigns.into_iter().map(|(field, _)| field).collect()
    }

    /// The field assignments found in the type's methods, keyed by field (first one wins). These
    /// do not initialize the field, but they let a definition error point at the misplaced assign.
    fn method_field_assigns(&self, decl: &HirTypeDecl) -> HashMap<Symbol, HirId<HirExpr>> {
        let mut assigns = Vec::new();
        for method in &decl.methods {
            if let HirStmt::Fn(m) = self.hir.get(method) {
                self.scan_field_assigns(&m.body, &decl.fields, &mut assigns);
            }
        }
        let mut map = HashMap::new();
        for (field, node) in assigns {
            map.entry(field).or_insert(node);
        }
        map
    }

    /// Collects each direct field assignment as `(field, lhs node)`: `this.f =` and bare `f =`.
    fn scan_field_assigns(&self, expr: &HirId<HirExpr>, fields: &HashSet<Symbol>, out: &mut Vec<(Symbol, HirId<HirExpr>)>) {
        match self.hir.get(expr) {
            HirExpr::Assign(target, value) => {
                match self.hir.get(target) {
                    HirExpr::Index(obj, member, true) if matches!(self.hir.get(obj), HirExpr::This) => {
                        if let HirExpr::Literal(HirLiteral::String(name)) = self.hir.get(member) {
                            if let Some(sym) = self.hir.symbol_of(name) { out.push((sym, *target)); }
                        }
                    },
                    HirExpr::Identifier(name) if fields.contains(name) => { out.push((*name, *target)); },
                    _ => {},
                }
                self.scan_field_assigns(value, fields, out);
            },
            HirExpr::Block(stmts) => for s in stmts { self.scan_field_assigns_stmt(s, fields, out); },
            HirExpr::Unary(_, x) | HirExpr::Is(x, _) | HirExpr::Assert(x) | HirExpr::Propagate(x) => self.scan_field_assigns(x, fields, out),
            HirExpr::Binary(_, l, r) | HirExpr::Coalesce(l, r) | HirExpr::Handle(l, _, r)
            | HirExpr::SafeAccess(l, r, _) | HirExpr::Index(l, r, _) => { 
                self.scan_field_assigns(l, fields, out); 
                self.scan_field_assigns(r, fields, out); 
            },
            HirExpr::Call(callee, args) | HirExpr::SafeCall(callee, args) => {
                self.scan_field_assigns(callee, fields, out);
                for a in args { self.scan_field_assigns(a, fields, out); }
            },
            HirExpr::Construct(callee, args, brace) => {
                self.scan_field_assigns(callee, fields, out);
                for a in args { self.scan_field_assigns(a, fields, out); }
                for (_, v) in brace { self.scan_field_assigns(v, fields, out); }
            },
            // Literals, identifiers, and `this` carry no field assignment. A lambda body is opaque to it.
            _ => {},
        }
    }

    fn scan_field_assigns_stmt(&self, stmt: &HirId<HirStmt>, fields: &HashSet<Symbol>, out: &mut Vec<(Symbol, HirId<HirExpr>)>) {
        match self.hir.get(stmt) {
            HirStmt::Expression(e) | HirStmt::Throw(e) | HirStmt::Block(e) => self.scan_field_assigns(e, fields, out),
            HirStmt::Return(opt) => if let Some(e) = opt { self.scan_field_assigns(e, fields, out); },
            HirStmt::While(cond, body) => { self.scan_field_assigns(cond, fields, out); self.scan_field_assigns(body, fields, out); },
            HirStmt::If(cond, then, otherwise) => {
                self.scan_field_assigns(cond, fields, out);
                self.scan_field_assigns(then, fields, out);
                if let Some(otherwise) = otherwise { self.scan_field_assigns_stmt(otherwise, fields, out); }
            },
            HirStmt::Try(body, catch, finally) => {
                self.scan_field_assigns(body, fields, out);
                if let Some(catch) = catch { self.scan_field_assigns(&catch.body, fields, out); }
                if let Some(finally) = finally { self.scan_field_assigns(finally, fields, out); }
            },
            // Nested declarations do not assign fields directly.
            _ => {},
        }
    }
}

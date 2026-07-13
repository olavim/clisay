//! Builds a `Signatures` table. Records every function and type-member signature
//! and infers each function's return tag.

use std::collections::{HashMap, HashSet};

use crate::middle::hir::{Hir, HirExpr, HirFnDecl, HirId, HirLiteral, HirStmt, HirTypeDecl, ReturnShape, Symbol};

/// A function's return: the obligations its result carries and whether any path returns a value.
#[derive(Clone, Default)]
pub struct RetSig {
    pub obligations: HashSet<Symbol>,
    pub void: bool,
}

/// A function's per-parameter obligation set and its return signature.
pub struct FnSig {
    pub params: Vec<HashSet<Symbol>>,
    pub ret: RetSig,
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
    // Constructed once user obligations declare a trait witness.
    #[allow(dead_code)]
    Trait(Symbol),
}

pub struct Signatures {
    pub(crate) opt: Symbol,
    pub(crate) fails: Symbol,
    /// Each obligation's witness. Built-ins are seeded here; user obligations extend it.
    pub(crate) witnesses: HashMap<Symbol, Witness>,

    // Per-function facts, keyed by the function's statement.
    pub(crate) fns: HashMap<HirId<HirStmt>, FnSig>,
    pub(crate) ret_tags: HashMap<HirId<HirStmt>, TypeTag>,

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
            fns: HashMap::new(),
            ret_tags: HashMap::new(),
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
    let mut collector = Collector { hir, opt, fails, err, sigs };
    collector.stmt(&hir.get_root());
    collector.infer_ret_tags();
    collector.infer_propagated();
    collector.sigs
}

struct Collector<'a> {
    hir: &'a Hir,
    opt: Symbol,
    fails: Symbol,
    err: Option<Symbol>,
    sigs: Signatures,
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

    /// Records a method's or initializer's signature and recurses into its body.
    fn collect_sig(&mut self, stmt: &HirId<HirStmt>) {
        if let HirStmt::Fn(decl) = self.hir.get(stmt) {
            let sig = self.fn_sig(decl);
            self.sigs.fns.insert(*stmt, sig);
            self.expr(&decl.body);
        }
    }

    fn fn_sig(&self, decl: &HirFnDecl) -> FnSig {
        let mut ret = self.ret_sig(decl.ret);
        if self.body_fails(&decl.body) {
            ret.obligations.insert(self.fails);
        }
        FnSig {
            params: decl.params.iter().map(|p| self.obligation_set(p.nullable)).collect(),
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

    fn obligation_set(&self, nullable: bool) -> HashSet<Symbol> {
        let mut set = HashSet::new();
        if nullable {
            set.insert(self.opt);
        }
        set
    }

    /// Maps a declared return shape onto its obligation set and value presence.
    fn ret_sig(&self, ret: ReturnShape) -> RetSig {
        match ret {
            ReturnShape::Void => RetSig { obligations: HashSet::new(), void: true },
            ReturnShape::Nullable => RetSig { obligations: self.obligation_set(true), void: false },
            ReturnShape::NonNull | ReturnShape::Inferred => RetSig::default(),
        }
    }

    fn expr(&mut self, expr: &HirId<HirExpr>) {
        match self.hir.get(expr) {
            HirExpr::Block(stmts) => for s in stmts { self.stmt(s); },
            HirExpr::Unary(_, x) | HirExpr::Is(x, _) | HirExpr::Assert(x) | HirExpr::Propagate(x)
            | HirExpr::Has(x, _) | HirExpr::Match(x, _) => self.expr(x),
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
                let HirStmt::Fn(decl) = self.hir.get(stmt) else { continue };
                let tag = self.infer_body_tag(&decl.body);
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

    /// The joined return type tag of a body: a single tag if every return agrees, else unknown.
    fn infer_body_tag(&self, body: &HirId<HirExpr>) -> TypeTag {
        let mut returns = Vec::new();
        self.collect_returns(body, &mut returns);
        let mut joined: Option<TypeTag> = None;
        for ret in returns {
            let tag = self.classify_return(&ret);
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
            HirExpr::Unary(_, x) | HirExpr::Is(x, _) | HirExpr::Assert(x) | HirExpr::Has(x, _) | HirExpr::Match(x, _) => self.collect_propagates(x, out),
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

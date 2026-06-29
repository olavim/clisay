//! Signature collection. Builds a `Signatures` table consumed by the check pass.
//! Records every function and type-member signature and infers each function's return tag.

use std::collections::{HashMap, HashSet};

use crate::middle::hir::{Hir, HirExpr, HirFnDecl, HirId, HirLiteral, HirMatchBody, HirStmt, HirTypeDecl, ReturnShape, Symbol};

/// A function's per-parameter nullability and return shape.
pub struct FnSig {
    pub params: Vec<bool>,
    pub ret: ReturnShape,
}

impl FnSig {
    fn of(decl: &HirFnDecl) -> FnSig {
        FnSig { params: decl.params.iter().map(|p| p.nullable).collect(), ret: decl.ret }
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

#[derive(Default)]
pub struct Signatures {
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
    /// Whether `name` names a declared type.
    pub(crate) fn is_type(&self, name: Symbol) -> bool {
        self.types_by_name.contains_key(&name)
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
    let mut collector = Collector { hir, sigs: Signatures::default() };
    collector.stmt(&hir.get_root());
    collector.infer_ret_tags();
    collector.sigs
}

struct Collector<'a> {
    hir: &'a Hir,
    sigs: Signatures,
}

impl<'a> Collector<'a> {
    fn stmt(&mut self, stmt: &HirId<HirStmt>) {
        match self.hir.get(stmt) {
            HirStmt::Fn(decl) => {
                self.sigs.fns.insert(*stmt, FnSig::of(decl));
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
            // A trait emits no runtime type, so its methods are not callable on a concrete receiver.
            HirStmt::Trait(decl) => {
                self.collect_sig(&decl.init);
                for method in &decl.methods {
                    self.collect_sig(method);
                }
            },
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
            HirStmt::Match(scrutinee, body) => {
                self.expr(scrutinee);
                if let HirMatchBody::Arms(arms) = body {
                    for arm in arms {
                        if let Some(guard) = &arm.guard { self.expr(guard); }
                        self.expr(&arm.body);
                    }
                }
            },
        }
    }

    /// Records a method's or initializer's signature and recurses into its body.
    fn collect_sig(&mut self, stmt: &HirId<HirStmt>) {
        if let HirStmt::Fn(decl) = self.hir.get(stmt) {
            self.sigs.fns.insert(*stmt, FnSig::of(decl));
            self.expr(&decl.body);
        }
    }

    fn expr(&mut self, expr: &HirId<HirExpr>) {
        match self.hir.get(expr) {
            HirExpr::Block(stmts) => for s in stmts { self.stmt(s); },
            HirExpr::Unary(_, x) | HirExpr::Is(x, _) | HirExpr::Assert(x) | HirExpr::Has(x, _) => self.expr(x),
            HirExpr::Binary(_, l, r) | HirExpr::Assign(l, r) | HirExpr::Coalesce(l, r)
            | HirExpr::SafeAccess(l, r, _) | HirExpr::Index(l, r, _) => { self.expr(l); self.expr(r); },
            HirExpr::Call(callee, args) => {
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
            HirExpr::Unary(_, x) | HirExpr::Is(x, _) | HirExpr::Assert(x) => self.scan_field_assigns(x, fields, out),
            HirExpr::Binary(_, l, r) | HirExpr::Coalesce(l, r) | HirExpr::SafeAccess(l, r, _)
            | HirExpr::Index(l, r, _) => { self.scan_field_assigns(l, fields, out); self.scan_field_assigns(r, fields, out); },
            HirExpr::Call(callee, args) => {
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

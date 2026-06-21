//! Collects every function and type-member signature, and infers each function's return type tag.

use crate::middle::hir::{Hir, HirExpr, HirId, HirLiteral, HirStmt, Symbol};

use super::{FnSig, RetTag, Signatures};

/// Collects the program's signatures and inferred return type tags.
pub(super) fn collect(hir: &Hir) -> Signatures {
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
            HirExpr::Unary(_, x) | HirExpr::Is(x, _) | HirExpr::Assert(x) => self.expr(x),
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
            self.sigs.ret_tags.insert(*stmt, RetTag::Unknown);
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
    fn infer_body_tag(&self, body: &HirId<HirExpr>) -> RetTag {
        let mut returns = Vec::new();
        self.collect_returns(body, &mut returns);
        let mut joined: Option<RetTag> = None;
        for ret in returns {
            let tag = self.classify_return(&ret);
            joined = Some(match joined {
                None => tag,
                Some(prev) if prev == tag => prev,
                Some(_) => RetTag::Unknown,
            });
        }
        joined.unwrap_or(RetTag::Unknown)
    }

    fn classify_return(&self, expr: &HirId<HirExpr>) -> RetTag {
        match self.hir.get(expr) {
            HirExpr::This => RetTag::SelfType,
            HirExpr::Construct(callee, _, _) => {
                self.type_name(callee).map_or(RetTag::Unknown, RetTag::Concrete)
            },
            HirExpr::Call(callee, _) => match self.hir.get(callee) {
                HirExpr::Identifier(name) if self.sigs.types_by_name.contains_key(name) => RetTag::Concrete(*name),
                HirExpr::Identifier(name) => self.sigs.fns_by_name.get(name)
                    .and_then(|stmt| self.sigs.ret_tags.get(stmt).copied())
                    .unwrap_or(RetTag::Unknown),
                _ => RetTag::Unknown,
            },
            _ => RetTag::Unknown,
        }
    }

    fn type_name(&self, callee: &HirId<HirExpr>) -> Option<Symbol> {
        match self.hir.get(callee) {
            HirExpr::Identifier(name) if self.sigs.types_by_name.contains_key(name) => Some(*name),
            _ => None,
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
}

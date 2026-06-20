//! Static nullability checking.

mod native;

use std::collections::HashMap;

use crate::middle::bind::Bindings;
use crate::middle::hir::{Hir, HirExpr, HirFnDecl, HirId, HirLiteral, HirStmt, ReturnShape};

/// A function's declared nullability signature: per-parameter nullability and return shape.
#[allow(dead_code)]
pub struct FnSig {
    pub params: Vec<bool>,
    pub ret: ReturnShape,
}

impl FnSig {
    fn of(decl: &HirFnDecl) -> FnSig {
        FnSig { params: decl.params.iter().map(|p| p.nullable).collect(), ret: decl.ret }
    }
}

#[allow(dead_code)]
#[derive(Default)]
pub struct Signatures {
    pub fns: HashMap<HirId<HirStmt>, FnSig>,
}

pub fn check(hir: &Hir, _bindings: &Bindings) -> Result<(), anyhow::Error> {
    let _signatures = Collector::collect(hir);
    Ok(())
}

struct Collector<'a> {
    hir: &'a Hir,
    sigs: Signatures,
}

impl<'a> Collector<'a> {
    fn collect(hir: &Hir) -> Signatures {
        let mut collector = Collector { hir, sigs: Signatures::default() };
        collector.stmt(&hir.get_root());
        collector.sigs
    }

    fn stmt(&mut self, stmt: &HirId<HirStmt>) {
        match self.hir.get(stmt) {
            HirStmt::Fn(decl) => {
                self.sigs.fns.insert(*stmt, FnSig::of(decl));
                self.expr(&decl.body);
            },
            // A type's methods and initializer are themselves `Fn` statements.
            HirStmt::Type(decl) | HirStmt::Trait(decl) => {
                self.stmt(&decl.init);
                for method in &decl.methods {
                    self.stmt(method);
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
}

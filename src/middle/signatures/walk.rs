//! The one place the HIR child structure is spelled out for the signature fact-gatherers. Each
//! gatherer records what it cares about at a node, then recurses through `children_of`. A new node
//! variant is handled here once, instead of in every walker.

use crate::middle::hir::{HirExpr, HirId, HirLiteral, HirStmt};

use super::Collector;

/// A direct child of a node: either an expression or a statement.
pub(super) enum Child {
    Expr(HirId<HirExpr>),
    Stmt(HirId<HirStmt>),
}

impl<'a> Collector<'a> {
    /// The direct children of an expression, in source order. A nested declaration is a separate
    /// scope, so a lambda body is a leaf here.
    pub(super) fn children_of_expr(&self, node: &HirId<HirExpr>) -> Vec<Child> {
        let mut out = Vec::new();
        match self.hir.get(node) {
            HirExpr::Unary(_, x) | HirExpr::Is(x, _) | HirExpr::Assert(x) | HirExpr::Has(x, _)
            | HirExpr::Match(x, _) | HirExpr::Mut(x) | HirExpr::Propagate(x) => out.push(Child::Expr(*x)),
            HirExpr::Binary(_, l, r) | HirExpr::Assign(l, r) | HirExpr::Coalesce(l, r)
            | HirExpr::Handle(l, _, r) | HirExpr::SafeAccess(l, r, _) | HirExpr::Index(l, r, _) => {
                out.push(Child::Expr(*l));
                out.push(Child::Expr(*r));
            },
            HirExpr::Call(callee, args) | HirExpr::SafeCall(callee, args) => {
                out.push(Child::Expr(*callee));
                for a in args { out.push(Child::Expr(*a)); }
            },
            HirExpr::Construct(callee, args, brace) => {
                out.push(Child::Expr(*callee));
                for a in args { out.push(Child::Expr(*a)); }
                for (_, v) in brace { out.push(Child::Expr(*v)); }
            },
            HirExpr::Block(stmts) => for s in stmts { out.push(Child::Stmt(*s)); },
            HirExpr::Literal(HirLiteral::Array(elems)) => for e in elems { out.push(Child::Expr(*e)); },
            HirExpr::Literal(HirLiteral::Dict(pairs)) => for (k, v) in pairs {
                out.push(Child::Expr(*k));
                out.push(Child::Expr(*v));
            },
            HirExpr::Literal(_) | HirExpr::Identifier(_) | HirExpr::This => {},
        }
        out
    }

    /// The direct children of a statement, in source order. A nested declaration is a leaf.
    pub(super) fn children_of_stmt(&self, node: &HirId<HirStmt>) -> Vec<Child> {
        let mut out = Vec::new();
        match self.hir.get(node) {
            HirStmt::Expression(e) | HirStmt::Throw(e) | HirStmt::Block(e) => out.push(Child::Expr(*e)),
            HirStmt::Return(opt) => if let Some(e) = opt { out.push(Child::Expr(*e)); },
            HirStmt::While(cond, body) => {
                out.push(Child::Expr(*cond));
                out.push(Child::Expr(*body));
            },
            HirStmt::If(cond, then, otherwise) => {
                out.push(Child::Expr(*cond));
                out.push(Child::Expr(*then));
                if let Some(o) = otherwise { out.push(Child::Stmt(*o)); }
            },
            HirStmt::Try(body, catch, finally) => {
                out.push(Child::Expr(*body));
                if let Some(c) = catch { out.push(Child::Expr(c.body)); }
                if let Some(f) = finally { out.push(Child::Expr(*f)); }
            },
            HirStmt::Say(field) => if let Some(v) = field.value { out.push(Child::Expr(v)); },
            HirStmt::Match(scrutinee, arms) => {
                out.push(Child::Expr(*scrutinee));
                for arm in arms {
                    if let Some(g) = &arm.guard { out.push(Child::Expr(*g)); }
                    out.push(Child::Expr(arm.body));
                }
            },
            HirStmt::Fn(_) | HirStmt::Type(_) | HirStmt::Trait(_) | HirStmt::Nop => {},
        }
        out
    }
}

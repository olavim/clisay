//! The one place the HIR child structure is spelled out. A pass records what it cares about at a
//! node, then recurses through `children_of`. A new node variant is handled here once, instead of
//! in every walker.

use crate::middle::hir::{Hir, HirExpr, HirId, HirLiteral, HirStmt};

/// A direct child of a node: either an expression or a statement.
#[derive(Clone, Copy)]
pub enum Child {
    Expr(HirId<HirExpr>),
    Stmt(HirId<HirStmt>),
}

/// Visits every node of a body, itself included, in source order. A nested declaration is a leaf,
/// so a caller sees only the nodes its own function owns.
pub fn visit_body(hir: &Hir, root: &HirId<HirExpr>, visit: &mut impl FnMut(Child)) {
    visit_node(hir, Child::Expr(*root), visit);
}

fn visit_node(hir: &Hir, node: Child, visit: &mut impl FnMut(Child)) {
    visit(node);
    for child in children_of(hir, node) {
        visit_node(hir, child, visit);
    }
}

/// The direct children of a node, whichever kind it is.
pub fn children_of(hir: &Hir, node: Child) -> Vec<Child> {
    match node {
        Child::Expr(e) => children_of_expr(hir, &e),
        Child::Stmt(s) => children_of_stmt(hir, &s),
    }
}

/// The direct children of an expression, in source order. A nested declaration is a separate
/// scope, so a lambda body is a leaf here.
pub fn children_of_expr(hir: &Hir, node: &HirId<HirExpr>) -> Vec<Child> {
    let mut out = Vec::new();
    match hir.get(node) {
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
pub fn children_of_stmt(hir: &Hir, node: &HirId<HirStmt>) -> Vec<Child> {
    let mut out = Vec::new();
    match hir.get(node) {
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

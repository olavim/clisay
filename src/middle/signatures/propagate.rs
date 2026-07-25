//! Propagation inference: adds each `?!` operand's, and each returned value's, obligations to the
//! enclosing function's return set.

use std::collections::HashSet;

use crate::middle::hir::{HirExpr, HirFnDecl, HirId, HirLiteral, HirStmt, Symbol};

use super::Collector;
use super::walk::Child;

impl<'a> Collector<'a> {
    /// Adds each `?!` operand's obligations to the enclosing function's return set.
    pub(super) fn infer_propagated(&mut self) {
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
        if let HirExpr::Propagate(operand) = self.hir.get(expr) { out.push(*operand); }
        for child in self.children_of_expr(expr) {
            match child {
                Child::Expr(e) => self.collect_propagates(&e, out),
                Child::Stmt(s) => self.collect_propagates_stmt(&s, out),
            }
        }
    }

    fn collect_propagates_stmt(&self, stmt: &HirId<HirStmt>, out: &mut Vec<HirId<HirExpr>>) {
        for child in self.children_of_stmt(stmt) {
            match child {
                Child::Expr(e) => self.collect_propagates(&e, out),
                Child::Stmt(s) => self.collect_propagates_stmt(&s, out),
            }
        }
    }
}

//! Propagation inference: adds each `?!` operand's, and each returned value's, obligations to the
//! enclosing function's return set.

use std::collections::HashSet;

use crate::middle::hir::{HirExpr, HirFnDecl, HirId, HirLiteral, HirStmt, Symbol};
use crate::middle::obligations::Obligations;

use super::Collector;
use crate::middle::walk::Child;
use crate::middle::walk;

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

    fn operand_obligations(&self, operand: &HirId<HirExpr>, decl: &HirFnDecl) -> Obligations {
        match self.hir.get(operand) {
            HirExpr::Call(callee, _) => {
                if self.is_err_call(operand) {
                    return Obligations::from([self.fails]);
                }
                self.call_return_obligations(callee)
            },
            HirExpr::SafeAccess(target, _, _) => self.operand_obligations(target, decl),
            HirExpr::SafeCall(callee, _) => {
                let mut set = self.operand_obligations(callee, decl);
                set.extend(self.call_return_obligations(callee));
                set
            },
            HirExpr::Literal(HirLiteral::Null) => Obligations::from([self.opt]),
            HirExpr::Identifier(name) => self.param_obligations(*name, decl),
            _ => Obligations::new(),
        }
    }

    /// What calling this callee hands back, for a callee resolved by name.
    fn call_return_obligations(&self, callee: &HirId<HirExpr>) -> Obligations {
        match self.hir.get(callee) {
            HirExpr::Identifier(name) => self.sigs.fns_by_name.get(name)
                .map(|s| self.sigs.fns[s].ret.obligations.clone())
                .unwrap_or_default(),
            _ => Obligations::new(),
        }
    }

    /// The declared obligation set of `name` when it is a parameter of `decl`.
    fn param_obligations(&self, name: Symbol, decl: &HirFnDecl) -> Obligations {
        for p in &decl.params {
            if matches!(self.hir.get(&p.name), HirExpr::Identifier(pname) if *pname == name) {
                return p.clause.names.iter().copied().collect();
            }
        }
        Obligations::new()
    }

    /// Collects each `?!` operand in a body, skipping nested function and lambda bodies.
    fn collect_propagates(&self, expr: &HirId<HirExpr>, out: &mut Vec<HirId<HirExpr>>) {
        walk::visit_body(self.hir, expr, &mut |node| {
            if let Child::Expr(e) = node {
                if let HirExpr::Propagate(operand) = self.hir.get(&e) { out.push(*operand); }
            }
        });
    }
}

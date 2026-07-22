//! The signature-building walk: records every function and method signature, registers obligations,
//! and infers each function's declared return shape.

use std::collections::HashSet;

use crate::middle::hir::{HirExpr, HirFnDecl, HirId, HirLiteral, HirStmt, ReturnShape, Symbol};

use super::{Collector, FnSig, RetSig, Witness};
use super::walk::Child;

impl<'a> Collector<'a> {
    pub(super) fn stmt(&mut self, stmt: &HirId<HirStmt>) {
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
                    self.sigs.method_owner.insert(*method, decl.name);
                    self.collect_sig(method);
                }
            },
            HirStmt::Trait(_) | HirStmt::Nop => {},
            // A non-declaration statement holds no signatures of its own. Recurse into its children.
            _ => for child in self.children_of_stmt(stmt) {
                match child {
                    Child::Expr(e) => self.expr(&e),
                    Child::Stmt(s) => self.stmt(&s),
                }
            },
        }
    }

    fn expr(&mut self, expr: &HirId<HirExpr>) {
        // A lambda is a leaf to the shared child walk, but its body may still declare functions.
        if let HirExpr::Literal(HirLiteral::Lambda(decl)) = self.hir.get(expr) {
            self.expr(&decl.body);
            return;
        }
        for child in self.children_of_expr(expr) {
            match child {
                Child::Expr(e) => self.expr(&e),
                Child::Stmt(s) => self.stmt(&s),
            }
        }
    }

    /// Registers each user obligation's witness and rule.
    pub(super) fn register_obligations(&mut self) {
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

    pub(super) fn is_err_call(&self, expr: &HirId<HirExpr>) -> bool {
        let HirExpr::Call(callee, _) = self.hir.get(expr) else { return false };
        matches!(self.hir.get(callee), HirExpr::Identifier(name) if Some(*name) == self.err)
    }

    /// Maps a function's declared return onto its obligation set and value presence. A marked return
    /// owes exactly its clause obligations. An unmarked return infers its presence from the body, and
    /// its obligations are filled by the propagation pass.
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
}

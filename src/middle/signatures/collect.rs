//! The signature-building walk: records every function and method signature, registers obligations,
//! and infers each function's declared return shape.

use super::Resolved;
use crate::middle::hir::{HirExpr, HirFnDecl, HirId, HirLiteral, HirStmt};
use crate::middle::obligations::Obligations;

use super::{Collector, FnSig, RetSig, Witness};
use crate::middle::walk::Child;
use crate::middle::walk;
use super::CallableId;

impl<'a> Collector<'a> {
    pub(super) fn resolved(&self) -> Resolved<'_> {
        Resolved { hir: self.hir, bindings: self.bindings, sigs: &self.sigs }
    }

    pub(super) fn stmt(&mut self, stmt: &HirId<HirStmt>) {
        match self.hir.get(stmt) {
            HirStmt::Fn(decl) => {
                let sig = self.fn_sig(decl);
                self.sigs.fns.insert((*stmt).into(), sig);
                self.expr(&decl.body);
            },
            HirStmt::Type(decl) => {
                self.sigs.types_by_name.entry(decl.name).or_default().push(*stmt);
                if let Some(builtin) = decl.builtin {
                    self.sigs.builtin_decls[builtin.index()] = Some(*stmt);
                }
                self.sigs.decls_by_id.insert(decl.id, *stmt);
                self.collect_sig(&decl.init);
                self.collect_methods(stmt, &decl.methods);
            },
            HirStmt::Trait(decl) => {
                self.sigs.traits_by_name.entry(decl.name).or_default().push(*stmt);
                self.sigs.decls_by_id.insert(decl.id, *stmt);
                self.collect_methods(stmt, &decl.methods);
            },
            HirStmt::Nop => {},
            // A non-declaration statement holds no signatures of its own. Recurse into its children.
            _ => for child in walk::children_of_stmt(self.hir, stmt) {
                match child {
                    Child::Expr(e) => self.expr(&e),
                    Child::Stmt(s) => self.stmt(&s),
                }
            },
        }
    }

    fn expr(&mut self, expr: &HirId<HirExpr>) {
        if let HirExpr::Literal(HirLiteral::Lambda(decl)) = self.hir.get(expr) {
            let sig = self.fn_sig(decl);
            self.sigs.fns.insert((*expr).into(), sig);
            self.expr(&decl.body);
            return;
        }
        for child in walk::children_of_expr(self.hir, expr) {
            match child {
                Child::Expr(e) => self.expr(&e),
                Child::Stmt(s) => self.stmt(&s),
            }
        }
    }

    /// Registers each user obligation's witness and rule.
    pub(super) fn register_obligations(&mut self) {
        for (name, decl) in self.hir.obligations() {
            self.sigs.obligation_rules.insert(name, decl.rules);
            let w = match self.hir.is_trait(decl.witness) {
                true => Witness::Trait(decl.witness),
                false => Witness::Type(decl.witness),
            };
            self.sigs.witnesses.insert(name, w);
        }
    }

    /// Records a method's or factory's signature and recurses into its body.
    fn collect_sig(&mut self, stmt: &HirId<HirStmt>) {
        if let HirStmt::Fn(decl) = self.hir.get(stmt) {
            let sig = self.fn_sig(decl);
            self.sigs.fns.insert((*stmt).into(), sig);
            self.expr(&decl.body);
        }
    }

    /// Folds each parameter's witness alternatives into the obligations its signature advertises.
    /// A caller sees `x @ Node | null` exactly as it sees `x: opt`.
    pub(super) fn admit_pattern_obligations(&mut self) {
        let callables: Vec<CallableId> = self.sigs.fns.keys().copied().collect();
        for callable in callables {
            let decl = match callable {
                CallableId::Fn(stmt) => match self.hir.get(&stmt) {
                    HirStmt::Fn(decl) => decl,
                    _ => continue,
                },
                CallableId::Lambda(expr) => match self.hir.get(&expr) {
                    HirExpr::Literal(HirLiteral::Lambda(decl)) => decl,
                    _ => continue,
                },
            };
            let admitted: Vec<(usize, Obligations)> = decl.params.iter().enumerate()
                .filter_map(|(i, p)| Some((i, self.resolved().admitted_obligations(p.pattern.as_ref()?))))
                .filter(|(_, admits)| !admits.is_empty())
                .collect();
            let Some(sig) = self.sigs.fns.get_mut(&callable).filter(|_| !admitted.is_empty()) else { continue };
            for (i, admits) in admitted {
                sig.param_clauses[i].extend(admits);
            }
        }
    }

    fn fn_sig(&self, decl: &HirFnDecl) -> FnSig {
        let mut ret = self.ret_sig(decl);
        if self.body_fails(&decl.body) {
            ret.obligations.insert(self.fails);
        }
        FnSig {
            param_clauses: decl.params.iter().map(|p| p.clause.owed()).collect(),
            param_anchors: decl.params.iter().map(|p| p.anchor).collect(),
            wants_anchor_receiver: decl.receiver.as_ref().is_some_and(|r| r.anchor),
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

    fn collect_methods(&mut self, owner: &HirId<HirStmt>, methods: &[HirId<HirStmt>]) {
        for method in methods {
            if let HirStmt::Fn(m) = self.hir.get(method) {
                self.sigs.methods_by_type.insert((*owner, m.name), *method);
            }
            self.sigs.method_owner.insert(*method, *owner);
            self.collect_sig(method);
        }
    }

    fn ret_sig(&self, decl: &HirFnDecl) -> RetSig {
        if decl.has_no_return_clause() {
            return RetSig { obligations: Obligations::new(), void: self.has_void_path(&decl.body) };
        }
        RetSig { obligations: decl.clause.owed(), void: decl.clause.void }
    }

    /// Whether a function body can finish without returning a value.
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

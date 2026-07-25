//! Return inference: each function's return type tag and return mutability.

use crate::middle::hir::{HirExpr, HirFnDecl, HirId, HirStmt};

use super::{Collector, Mutability, TypeTag};
use super::walk::Child;

impl<'a> Collector<'a> {
    /// Infers every function's return type tag.
    pub(super) fn infer_ret_tags(&mut self) {
        let stmts: Vec<HirId<HirStmt>> = self.sigs.fns.keys().copied().collect();
        for stmt in &stmts {
            self.sigs.ret_tags.insert(*stmt, TypeTag::Unknown);
        }
        loop {
            let mut changed = false;
            for stmt in &stmts {
                let tag = self.infer_body_tag(&self.returns[stmt]);
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

    pub(super) fn infer_ret_mut(&mut self) {
        let stmts: Vec<HirId<HirStmt>> = self.sigs.fns.keys().copied().collect();
        for stmt in &stmts {
            self.sigs.ret_mut.insert(*stmt, Mutability::Unknown);
        }
        let mut changed = true;
        while changed {
            changed = false;
            for stmt in &stmts {
                let HirStmt::Fn(decl) = self.hir.get(stmt) else { continue };
                let mutability = self.ret_mut_of(decl, &self.returns[stmt]);
                changed |= self.sigs.ret_mut.insert(*stmt, mutability) != Some(mutability);
            }
        }
    }

    /// Walks each function body once, so the tag and mutability passes share the return list.
    pub(super) fn collect_all_returns(&mut self) {
        let stmts: Vec<HirId<HirStmt>> = self.sigs.fns.keys().copied().collect();
        for stmt in stmts {
            let HirStmt::Fn(decl) = self.hir.get(&stmt) else { continue };
            let mut returns = Vec::new();
            self.collect_returns(&decl.body, &mut returns);
            self.returns.insert(stmt, returns);
        }
    }

    /// A function's return mutability, inferred from its body.
    fn ret_mut_of(&self, decl: &HirFnDecl, returns: &[HirId<HirExpr>]) -> Mutability {
        if decl.clause.capability.is_mut() {
            return Mutability::Mutable;
        }
        if !returns.is_empty() && returns.iter().all(|r| self.returns_mutable(r)) {
            Mutability::Mutable
        } else {
            Mutability::Unknown
        }
    }

    /// Whether a return hands back a statically-mutable value: a `mut`-minted construction or a call
    /// to a function inferred to return a mutable.
    fn returns_mutable(&self, expr: &HirId<HirExpr>) -> bool {
        match self.hir.get(expr) {
            HirExpr::Mut(_) => true,
            HirExpr::Call(callee, _) => {
                let HirExpr::Identifier(name) = self.hir.get(callee) else { return false };
                let Some(stmt) = self.sigs.fns_by_name.get(name) else { return false };
                self.sigs.ret_mut.get(stmt) == Some(&Mutability::Mutable)
            },
            _ => false,
        }
    }

    /// The joined return type tag of a body: a single tag if every return agrees, else unknown.
    fn infer_body_tag(&self, returns: &[HirId<HirExpr>]) -> TypeTag {
        let mut joined: Option<TypeTag> = None;
        for ret in returns {
            let tag = self.classify_return(ret);
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
            // A `: mut` factory returns `mut Ctor()`, so classify the wrapped construction.
            HirExpr::Mut(inner) => self.classify_return(inner),
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

    pub(super) fn collect_returns(&self, expr: &HirId<HirExpr>, out: &mut Vec<HirId<HirExpr>>) {
        for child in self.children_of_expr(expr) {
            match child {
                Child::Expr(e) => self.collect_returns(&e, out),
                Child::Stmt(s) => self.collect_returns_stmt(&s, out),
            }
        }
    }

    fn collect_returns_stmt(&self, stmt: &HirId<HirStmt>, out: &mut Vec<HirId<HirExpr>>) {
        // A nested function's returns belong to that function, so the traversal treats it as a leaf.
        if let HirStmt::Return(Some(e)) = self.hir.get(stmt) { out.push(*e); }
        for child in self.children_of_stmt(stmt) {
            match child {
                Child::Expr(e) => self.collect_returns(&e, out),
                Child::Stmt(s) => self.collect_returns_stmt(&s, out),
            }
        }
    }
}

//! Return inference.

use crate::middle::hir::{HirExpr, HirId, HirStmt};

use super::CallableId;
use super::Signatures;
use super::{Collector, TypeTag};
use crate::middle::walk::Child;
use crate::middle::walk;

impl<'a> Collector<'a> {
    /// Infers every function's return type tag.
    pub(super) fn infer_ret_tags(&mut self) {
        let callables: Vec<CallableId> = self.sigs.fns.keys().copied().collect();
        for stmt in &callables {
            self.sigs.ret_tags.insert(*stmt, TypeTag::Unknown);
        }
        loop {
            let mut changed = false;
            for stmt in &callables {
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

    pub(super) fn collect_all_returns(&mut self) {
        let callables: Vec<CallableId> = self.sigs.fns.keys().copied().collect();
        for stmt in callables {
            let Some(decl) = Signatures::decl_of(self.hir, stmt) else { continue };
            let mut returns = Vec::new();
            self.collect_returns(&decl.body, &mut returns);
            self.returns.insert(stmt, returns);
        }
    }

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
            HirExpr::Construct(callee, _) => self.resolved().constructed_tag(callee),
            // A callee naming a type is a factory call, so it reports the type it builds.
            HirExpr::Call(callee, _) => match self.resolved().type_named(callee) {
                Some(decl) => TypeTag::Concrete(decl),
                None => match self.hir.get(callee) {
                    HirExpr::Identifier(name) => self.sigs.fns_by_name.get(name)
                        .and_then(|stmt| self.sigs.ret_tag_of(stmt).cloned())
                        .unwrap_or(TypeTag::Unknown),
                    _ => TypeTag::Unknown,
                },
            },
            _ => TypeTag::Unknown,
        }
    }

    /// A nested function's returns belong to that function, which the walk treats as a leaf.
    pub(super) fn collect_returns(&self, expr: &HirId<HirExpr>, out: &mut Vec<HirId<HirExpr>>) {
        if let Some(value) = self.hir.expression_body(expr) {
            out.push(value);
            return;
        }
        walk::visit_body(self.hir, expr, &mut |node| {
            if let Child::Stmt(s) = node {
                if let HirStmt::Return(Some(e)) = self.hir.get(&s) { out.push(*e); }
            }
        });
    }
}

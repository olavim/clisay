//! `@` ref access desugaring.

use crate::ast::{AstId, Expr};
use crate::frontend::lex::SourcePosition;
use crate::middle::hir::{BinOp, HirExpr, HirId};

use super::Lowerer;

/// An assignment target split at its rightmost `@`. In `w.@field.value`, `holder` is `w.field`,
/// `marker` is the `@` node, and `path` is the whole target.
pub(super) struct RefAccessTarget {
    holder: AstId<Expr>,
    marker: AstId<Expr>,
    path: AstId<Expr>,
}

impl<'a> Lowerer<'a> {
    pub(super) fn ref_access_target_of(&self, target: &AstId<Expr>) -> Option<RefAccessTarget> {
        let marker = self.rightmost_ref_access(target)?;
        let Expr::RefAccess(holder) = self.ast.get(&marker) else { return None };
        Some(RefAccessTarget { holder: *holder, marker, path: *target })
    }

    fn rightmost_ref_access(&self, target: &AstId<Expr>) -> Option<AstId<Expr>> {
        match self.ast.get(target) {
            Expr::RefAccess(_) => Some(*target),
            Expr::Index(base, ..) | Expr::SafeAccess(base, ..) => self.rightmost_ref_access(base),
            Expr::Call(callee, _) => self.rightmost_ref_access(callee),
            _ => None,
        }
    }

    /// `@t = v`, `@t += v` and `@t.pos = v`.
    pub(super) fn assign_through_ref_access(
        &mut self,
        target: RefAccessTarget,
        op: Option<BinOp>,
        rhs: &AstId<Expr>,
        pos: &SourcePosition,
    ) -> Result<HirId<HirExpr>, anyhow::Error> {
        let holder = self.expr(&target.holder)?;
        let reached = self.hir.add(HirExpr::RefValue { holder, safe: false }, pos.clone());
        let target = self.lower_with_expr_substitution(&target.path, target.marker, reached)?;
        let value = self.expr(rhs)?;
        let assign = match op {
            None => HirExpr::Assign(target, value),
            Some(binop) => HirExpr::CompoundAssign(target, binop, value),
        };
        Ok(self.hir.add(assign, pos.clone()))
    }

    pub(super) fn read_ref_access(&mut self, inner: &AstId<Expr>) -> Result<HirExpr, anyhow::Error> {
        let safe = matches!(self.ast.get(inner), Expr::SafeAccess(..));
        Ok(HirExpr::RefValue { holder: self.expr(inner)?, safe })
    }
}

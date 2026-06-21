//! Return-contract checks: shape conformance and definite return.

use crate::middle::hir::{HirExpr, HirId, HirStmt, ReturnShape};

use super::{Checker, Nullness, Violation};

impl<'a> Checker<'a> {
    /// Whether every path through a function body ends in a `return` or `throw`.
    pub(super) fn definitely_returns(&self, body: &HirId<HirExpr>) -> bool {
        match self.hir.get(body) {
            HirExpr::Block(stmts) => stmts.iter().any(|s| self.stmt_returns(s)),
            _ => false,
        }
    }

    fn stmt_returns(&self, stmt: &HirId<HirStmt>) -> bool {
        match self.hir.get(stmt) {
            HirStmt::Return(_) | HirStmt::Throw(_) => true,
            HirStmt::Block(body) => self.definitely_returns(body),
            HirStmt::If(_, then, Some(otherwise)) => self.definitely_returns(then) && self.stmt_returns(otherwise),
            // A `finally` that returns always runs. Otherwise the try returns when its body does
            // and any catch does too.
            HirStmt::Try(body, catch, finally) => {
                if finally.as_ref().is_some_and(|f| self.definitely_returns(f)) {
                    return true;
                }
                self.definitely_returns(body) && catch.as_ref().map_or(true, |c| self.definitely_returns(&c.body))
            },
            _ => false,
        }
    }

    /// Checks a `return <value>` against the declared return shape: a `!` rejects a possibly-null
    /// or void value, a `?` accepts any value, and a void function may not return a value at all.
    pub(super) fn check_return(&mut self, nullness: Nullness, shape: ReturnShape, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        match shape {
            ReturnShape::Void => Err(self.error("A void function cannot return a value".to_string(), node)),
            ReturnShape::NonNull => match self.non_null_violation(nullness, node) {
                None => Ok(()),
                Some(Violation::Void) => Err(self.error("Cannot return a void result from a '!' function".to_string(), node)),
                Some(Violation::Null | Violation::Nullable) => Err(self.error("A '!' function must return a non-null value".to_string(), node)),
            },
            ReturnShape::Nullable => match nullness {
                Nullness::Void => Err(self.error("Cannot return a void result".to_string(), node)),
                _ => Ok(()),
            },
            ReturnShape::Inferred => Ok(()),
        }
    }
}

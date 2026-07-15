//! Return-contract checks: shape conformance and definite return.

use crate::middle::hir::{HirExpr, HirId, ReturnShape};

use super::{Checker, Flow, Violation};

impl<'a> Checker<'a> {
    /// Checks a `return <value>` against the declared return shape: a `!` rejects a possibly-null
    /// or void value, a `?` accepts any value, and a void function may not return a value at all.
    pub(super) fn check_return(&mut self, flow: &Flow, shape: ReturnShape, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        // An unmarked function infers its obligations from what it returns. A bad value is a legal
        // return that names an obligation.
        if self.current_return_unmarked {
            return match flow {
                Flow::Bad { .. } => Ok(()),
                Flow::Void => Err(self.error("Cannot return a void result".to_string(), node)),
                _ => Err(self.error("A void function cannot return a value".to_string(), node)),
            };
        }
        match shape {
            ReturnShape::Void if !self.current_return_owes => {
                Err(self.error("A void function cannot return a value".to_string(), node))
            },
            ReturnShape::Void => if flow.is_void() {
                Err(self.error("Cannot return a void result".to_string(), node))
            } else {
                Ok(())
            },
            ReturnShape::NonNull => match self.non_null_violation(flow, node) {
                None => Ok(()),
                Some(Violation::Void) => Err(self.error("Cannot return a void result from a '!' function".to_string(), node)),
                Some(Violation::Null | Violation::Nullable) => Err(self.error("A '!' function must return a non-null value".to_string(), node)),
            },
            ReturnShape::Nullable => if flow.is_void() {
                Err(self.error("Cannot return a void result".to_string(), node))
            } else {
                Ok(())
            },
            ReturnShape::Inferred => Ok(()),
        }
    }
}

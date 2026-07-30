//! Return-contract checks: shape conformance and definite return.

use crate::middle::hir::{HirExpr, HirId, ReturnShape};
use crate::middle::obligations::Obligations;

use super::{Mutability, Checker, Flow, Typed, Violation};

impl<'a> Checker<'a> {
    /// A `: mut` function must hand back a mutable value.
    pub(super) fn check_return_mutability(&self, typed: &Typed, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        if self.fn_ctx.return_mut && typed.mutability == Mutability::Immutable {
            let label = format!("{} is immutable", self.arg_name(node));
            if let (Some(clause), Some(fname)) = (&self.fn_ctx.return_clause, self.fn_ctx.name) {
                let fname = self.hir.text(fname);
                return Err(self.error_ctx("invalid immutable return: expected mutable", self.hir.pos(node), label, clause, format!("`{fname}` expects a mutable return")));
            }
            return Err(self.error_labeled("invalid immutable return: expected mutable".to_string(), node, label));
        }
        Ok(())
    }

    /// Returning a field of a receiver would move a mutable value out of a receiver the caller only
    /// lends. A `: mut` function promises an owned mutable, so a bare field return is rejected.
    pub(super) fn check_return_field_move(&self, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        if !self.fn_ctx.return_mut {
            return Ok(());
        }
        let HirExpr::Index(target, member, true) = self.hir.get(node) else { return Ok(()) };
        let Some(field) = self.member_text(member) else { return Ok(()) };
        if !matches!(self.hir.get(target), HirExpr::This | HirExpr::Identifier(_)) {
            return Ok(());
        }
        Err(self.error_help(
            format!("Cannot return the mutable field '{field}'; it would move out of the receiver"),
            node,
            "freeze or copy it before returning"))
    }

    /// Checks a returned value against the obligations the return admits. An unmarked return infers
    /// its obligations from the body instead, so it admits whatever the body produces.
    fn check_return_obligations(&self, flow: &Flow, admits: &Obligations, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        if self.fn_ctx.return_unmarked {
            return Ok(());
        }

        let undeclared = self.undeclared_obligations(flow, admits);
        if undeclared.is_empty() {
            return Ok(());
        }

        let owed = self.quoted_obligation_list(&undeclared);
        let subject = self.quoted_subject(node);
        let help = "discharge it before returning, or declare it on the return";
        let Some(clause) = &self.fn_ctx.return_clause else {
            return Err(self.error_help(format!("cannot return a value owing {owed}"), node, help));
        };
        let fname = self.fn_ctx.name.map_or("this function".to_string(), |s| format!("`{}`", self.hir.text(s)));
        Err(self.error_ctx_help(
            format!("cannot return a value owing {owed}"),
            self.hir.pos(node), format!("{subject} owes {owed}"),
            clause, format!("{fname} does not declare {owed}"),
            help
        ))
    }

    /// Checks a `return <value>` against the declared return shape: a `!` rejects a possibly-null
    /// or void value, a `?` accepts any value, and a void function may not return a value at all.
    pub(super) fn check_return(&mut self, flow: &Flow, shape: ReturnShape, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        // A lambda and the program root have no signature, so the return infers what the body produces.
        let admits = match &self.fn_ctx.return_admits {
            Some(declared) => declared.clone(),
            None => self.owed_of(flow),
        };
        self.check_return_obligations(flow, &admits, node)?;
        self.reject_at(flow, super::Rule::NoReturn, super::Site::Return, node)?;
        self.mark_settled(node, &admits);

        // A lambda or the program root infers its shape from the body, so there is none to check.
        if shape == ReturnShape::Inferred {
            return Ok(());
        }
        
        // An unmarked function infers its obligations from what it returns. A bad value is a legal
        // return that names an obligation.
        if self.fn_ctx.return_unmarked {
            return match flow {
                Flow::Bad { .. } => Ok(()),
                Flow::Void => Err(self.error("Cannot return a void result".to_string(), node)),
                _ => Err(self.error("A void function cannot return a value".to_string(), node)),
            };
        }
        match shape {
            ReturnShape::Void if !self.fn_ctx.return_owes => {
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
            ReturnShape::Inferred => unreachable!("inferred returns are skipped above"),
        }
    }
}

//! What a body owes on the way out: return shape, definite return, and results left undischarged.


use crate::middle::diagnose::Diagnose;
use crate::middle::obligations::{quoted_obligation_list, obligation_atoms, ObligationRule, Site};
use crate::middle::hir::{HirFnDecl, HirExpr, HirId, ReturnShape};
use crate::middle::signatures::CallableId;
use crate::middle::obligations::Obligations;

use super::{Checker, Ctx, Mutability, Debt, ValueState, Violation};

impl<'a> Ctx<'a> {
    pub(super) fn pending_must_use(&self, obligations: &Obligations) -> Obligations {
        obligations.iter().copied().filter(|o| self.sigs.obligation_rules_of(*o).must_use).collect()
    }

    pub(super) fn check_unused_must_use(&self, debt: &Debt, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let Debt::Owed { obligations, .. } = debt else { return Ok(()) };
        let pending = self.pending_must_use(obligations);
        if pending.is_empty() {
            return Ok(());
        }

        let owed = quoted_obligation_list(self.hir, &pending);
        let help = self.obligation_rule_prevents_help(&pending, ObligationRule::MustUse, Site::Drop);
        Err(self.error_help(Site::Drop.refusal(&owed), node, help))
    }

    pub(super) fn callable_subject(&self, callable: CallableId, decl: &HirFnDecl) -> String {
        match callable {
            CallableId::Fn(_) => format!("'{}'", self.hir.text(decl.name)),
            CallableId::Lambda(_) => "this lambda".to_string(),
        }
    }

    pub(super) fn mixed_void_error(&self, callable: CallableId, decl: &HirFnDecl, obligations: &Obligations) -> anyhow::Error {
        let subject = self.callable_subject(callable, decl);
        let list = quoted_obligation_list(self.hir, obligations);
        // The annotation spells the obligations as clause atoms: `: void opt fails`.
        let annotation = format!(": void {}", obligation_atoms(self.hir, obligations));
        self.error_help(
            format!("{subject} returns a value owing {list} on some paths and no value on others"),
            &decl.body,
            format!("annotate its return '{annotation}', or return a value on every path"),
        )
    }
}

impl<'a> Checker<'a> {
    /// A `: mut` function must hand back a mutable value.
    pub(super) fn check_return_mutability(&self, state: &ValueState, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        if self.fn_ctx.return_mut && state.mutability == Mutability::Immutable {
            let label = format!("{} is immutable", self.ctx.arg_display_name(node));
            if let (Some(clause), Some(fname)) = (&self.fn_ctx.return_clause, self.fn_ctx.name) {
                let fname = self.ctx.hir.text(fname);
                return Err(self.error_ctx("invalid immutable return: expected mutable", self.ctx.hir.pos(node), label, clause, format!("`{fname}` expects a mutable return")));
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
        let HirExpr::Index(target, member, true) = self.ctx.hir.get(node) else { return Ok(()) };
        let Some(field) = self.ctx.member_display_name(member) else { return Ok(()) };
        if !matches!(self.ctx.hir.get(target), HirExpr::This | HirExpr::Identifier(_)) {
            return Ok(());
        }
        Err(self.error_help(
            format!("Cannot return the mutable field '{field}'; it would move out of the receiver"),
            node,
            "copy it before returning"))
    }

    /// Checks a returned value against the obligations the return declares.
    pub(super) fn check_return_obligations(&self, debt: &Debt, admits: &Obligations, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        if self.fn_ctx.return_unmarked {
            return Ok(());
        }

        let undeclared = self.ctx.unadmitted_obligations(debt, admits);
        if undeclared.is_empty() {
            return Ok(());
        }

        let owed = quoted_obligation_list(self.ctx.hir, &undeclared);
        let subject = self.ctx.quoted_subject(node);
        let help = "discharge it before returning, or declare it on the return";
        let Some(clause) = &self.fn_ctx.return_clause else {
            return Err(self.error_help(format!("cannot return a value owing {owed}"), node, help));
        };
        let fname = self.fn_ctx.name.map_or("this function".to_string(), |s| format!("`{}`", self.ctx.hir.text(s)));
        Err(self.error_ctx_help(
            format!("cannot return a value owing {owed}"),
            self.ctx.hir.pos(node), format!("{subject} owes {owed}"),
            clause, format!("{fname} does not declare {owed}"),
            help
        ))
    }

    /// Checks a `return <value>` against the declared return shape.
    pub(super) fn check_return(&mut self, debt: &Debt, shape: ReturnShape, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        // A lambda and the program root have no signature, so the return infers what the body produces.
        let admits = match &self.fn_ctx.return_admits {
            Some(declared) => declared.clone(),
            None => self.ctx.obligations_of(debt),
        };
        self.check_return_obligations(debt, &admits, node)?;
        self.ctx.obligation_rule_reject_at(debt, super::ObligationRule::NoReturn, super::Site::Return, node)?;

        if shape == ReturnShape::Inferred || self.fn_ctx.return_unmarked {
            return match debt {
                Debt::Void if self.fn_ctx.returns_void => Ok(()),
                Debt::Void => Err(self.error("Cannot return a void result".to_string(), node)),
                _ => Ok(()),
            };
        }
        match shape {
            ReturnShape::Void if debt.is_void() => Ok(()),
            ReturnShape::Void if !self.fn_ctx.return_owes => {
                Err(self.error("A void function cannot return a value".to_string(), node))
            },
            ReturnShape::Void => if debt.is_void() {
                Err(self.error("Cannot return a void result".to_string(), node))
            } else {
                Ok(())
            },
            ReturnShape::NonNull => match self.non_null_violation(debt, node) {
                None => Ok(()),
                Some(Violation::Void) => Err(self.error("Cannot return a void result from a '!' function".to_string(), node)),
                Some(Violation::Null | Violation::Nullable) => Err(self.error("A '!' function must return a non-null value".to_string(), node)),
            },
            ReturnShape::Nullable => if debt.is_void() {
                Err(self.error("Cannot return a void result".to_string(), node))
            } else {
                Ok(())
            },
            ReturnShape::Inferred => unreachable!("inferred returns are skipped above"),
        }
    }

    pub(super) fn check_dropped(&self, mark: usize, at: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        for local in &self.locals[mark..] {
            if local.fn_decl || local.used || local.owed.is_empty() {
                continue;
            }
            let pending = self.ctx.pending_must_use(&local.owed);
            if pending.is_empty() {
                continue;
            }
            let owed = quoted_obligation_list(self.ctx.hir, &pending);
            let text = self.ctx.binding_display_name(local.name);
            let help = self.ctx.obligation_rule_prevents_help(&pending, ObligationRule::MustUse, Site::ScopeEnd);
            return Err(self.ctx.error_labeled_help(format!("'{text}' owes {owed} and is never used"),
                local.site.as_ref().unwrap_or(at), format!("owes {owed} from here"), help));
        }
        Ok(())
    }

}


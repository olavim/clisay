//! What a body owes on the way out.

use crate::middle::diagnose::Diagnose;
use crate::middle::obligations::{quoted_obligation_list, ObligationRule, Site};
use crate::middle::hir::{HirFnDecl, HirExpr, HirId};
use crate::middle::signatures::CallableId;
use crate::middle::obligations::Obligations;

use super::{Checker, Ctx, Debt, Local};

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

    /// A function whose returns disagree about whether it hands anything back.
    pub(super) fn mixed_return_error(&self, callable: CallableId, decl: &HirFnDecl) -> anyhow::Error {
        let subject = self.callable_subject(callable, decl);
        self.error_help(
            format!("{subject} returns a value on some paths and no value on others"),
            &decl.body,
            "a function must return a value on every path or on none".to_string(),
        )
    }

    pub(super) fn mixed_void_error(&self, callable: CallableId, decl: &HirFnDecl, obligations: &Obligations) -> anyhow::Error {
        let subject = self.callable_subject(callable, decl);
        let list = quoted_obligation_list(self.hir, obligations);
        self.error_help(
            format!("{subject} returns a value owing {list} on some paths and no value on others"),
            &decl.body,
            "a function must return a value on every path or on none"
        )
    }
}

impl<'a> Checker<'a> {
    /// Checks a returned value against the obligations the return declares.
    pub(super) fn check_return_obligations(&self, debt: &Debt, admits: &Obligations, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        if self.fn_ctx.return_undeclared {
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

    pub(super) fn note_return(&mut self, debt: &Debt, value: &HirId<HirExpr>) {
        if let Debt::Void = debt {
            if let Some(callable) = self.fn_ctx.callable {
                self.out.void_returns.insert(callable);
            }
            self.out.void_return_sites.insert(*value);
            return;
        }
        if let Some(callable) = self.fn_ctx.callable {
            self.out.valued_returns.insert(callable);
        }
        // A value the return already admits needs no test on its way out. An unknown one is the
        // case the test is for, so it proves nothing however empty its obligations read.
        if matches!(debt, Debt::Unknown) {
            return;
        }
        let Some(admits) = &self.fn_ctx.return_admits else { return };
        if self.ctx.unadmitted_obligations(debt, admits).is_empty() {
            self.out.proven_returns.insert(*value);
        }
    }

    /// Checks a `return <value>` against what may leave the function.
    pub(super) fn check_return(&mut self, debt: &Debt, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        self.note_return(debt, node);
        // A lambda and the program root have no signature, so the return infers what the body produces.
        let admits = match &self.fn_ctx.return_admits {
            Some(declared) => declared.clone(),
            None => self.ctx.obligations_of(debt),
        };
        self.check_return_obligations(debt, &admits, node)?;
        self.ctx.obligation_rule_reject_at(debt, super::ObligationRule::NoReturn, super::Site::Return, node)?;

        // A `: void` return with no obligations takes no value at all.
        if self.fn_ctx.declares_void && !self.fn_ctx.return_owes && !debt.is_void() {
            return Err(self.error("A void function cannot return a value".to_string(), node));
        }
        match debt {
            // An unmarked function is whatever its returns make it, and one return cannot say.
            // `function` settles it once every return has been seen.
            Debt::Void if self.fn_ctx.returns_void || self.fn_ctx.return_undeclared => Ok(()),
            Debt::Void => Err(self.error("Cannot return a void result".to_string(), node)),
            _ => Ok(()),
        }
    }

    pub(super) fn bare_return_refused(&self) -> bool {
        if self.fn_ctx.return_undeclared || self.fn_ctx.declares_void {
            return false;
        }
        self.fn_ctx.return_admits.as_ref().is_some_and(|a| !a.contains(&self.ctx.sigs.opt))
    }

    pub(super) fn check_dropped(&self, mark: usize, at: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        for local in &self.locals[mark..] {
            if let Some(e) = self.check_unused_must_use(local, at) {
                return Err(e);
            }
        }
        Ok(())
    }

    fn check_unused_must_use(&self, local: &Local, at: &HirId<HirExpr>) -> Option<anyhow::Error> {
        if local.fn_decl || local.used || local.clause_owed().is_empty() {
            return None;
        }
        let pending = self.ctx.pending_must_use(local.clause_owed());
        if pending.is_empty() {
            return None;
        }
        let owed = quoted_obligation_list(self.ctx.hir, &pending);
        let text = self.ctx.binding_display_name(local.name);
        let help = self.ctx.obligation_rule_prevents_help(&pending, ObligationRule::MustUse, Site::ScopeEnd);
        Some(self.ctx.error_labeled_help(format!("'{text}' owes {owed} and is never used"),
            local.site.as_ref().unwrap_or(at), format!("owes {owed} from here"), help))
    }

}


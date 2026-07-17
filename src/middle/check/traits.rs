//! Trait-contract checks: surface use, override return conformance, and `req fn` variance.

use std::collections::HashSet;

use crate::middle::hir::{HirExpr, HirId, HirStmt, HirTypeDecl, Symbol};
use crate::middle::signatures::RetSig;

use super::{Checker, Typed};

impl<'a> Checker<'a> {
    /// A member overriding a trait method may return non-null where the traitmethod is nullable,
    /// but not the reverse.
    pub(super) fn check_method_overrides(&self, decl: &HirTypeDecl) -> Result<(), anyhow::Error> {
        for method in &decl.methods {
            let HirStmt::Fn(folded) = self.hir.get(method) else { continue };
            // A trait method the host overrides is folded under a `"Trait.method"` alias.
            let Some((trait_name, base)) = split_trait_alias(self.hir.text(folded.name)) else { continue };
            let Some(base_sym) = self.hir.symbol_of(base) else { continue };
            // A renamed private slot is also dotted. Only an exposed override is a contract.
            if !decl.pub_members.contains(&base_sym) {
                continue;
            }
            let host = decl.methods.iter().copied().find(|s| matches!(self.hir.get(s), HirStmt::Fn(h) if h.name == base_sym));
            let (Some(host), Some(trait_ret)) = (host, self.sigs.fns.get(method).map(|f| &f.ret)) else { continue };
            let Some(host_ret) = self.sigs.fns.get(&host).map(|f| &f.ret) else { continue };
            if !self.ret_conforms(host_ret, trait_ret) {
                return Err(self.error(format!("Method '{}' overrides trait '{}' but its return is more nullable than the trait declares", base, trait_name), &host));
            }
        }
        Ok(())
    }

    /// Checks obligation variance where a `req fn` hole is satisfied. A satisfier's return may
    /// promise fewer obligations than the hole. Its parameters must accept at least what the hole
    /// passes.
    pub(super) fn check_req_conformance(&self, decl: &HirTypeDecl) -> Result<(), anyhow::Error> {
        for req in &decl.req_fns {
            let Some(method) = self.satisfying_method(decl, req.name) else { continue };
            let (Some(sig), HirStmt::Fn(sat)) = (self.sigs.fns.get(&method), self.hir.get(&method)) else { continue };
            let name = self.hir.text(req.name);

            // The `[obl]` container shape is invariant: a bare value and a container are not
            // interchangeable, since the trait body reads one by index and the other directly.
            if req.ret.container != sat.clause.container {
                return Err(self.error(self.container_mismatch(name, "its return", req.ret.container), &method));
            }

            // A satisfier may not owe a return obligation the requirement does not permit.
            let diff = self.sorted_difference(&sig.ret.obligations, &self.clause_owed(&req.ret));
            if !diff.is_empty() {
                return Err(self.error(format!("Method '{name}' satisfies 'req fn {name}' but its return owes {}, which the requirement does not permit", quote_list(&diff)), &method));
            }

            // A satisfier's parameter must accept at least the obligations the hole passes it.
            for (i, hole) in req.param_clauses.iter().enumerate() {
                let Some(sat_param) = sat.params.get(i) else { continue };
                if hole.container != sat_param.clause.container {
                    return Err(self.error(self.container_mismatch(name, &format!("parameter {}", i + 1), hole.container), &method));
                }
                let Some(accepted) = sig.param_clauses.get(i) else { continue };
                let missing = self.sorted_difference(&self.clause_owed(hole), accepted);
                if !missing.is_empty() {
                    return Err(self.error(format!("Method '{name}' satisfies 'req fn {name}' but parameter {} does not accept {}, which the requirement passes", i + 1, quote_list(&missing)), &method));
                }
            }
        }
        Ok(())
    }

    /// The message for a slot whose `[obl]` container shape does not match the requirement.
    fn container_mismatch(&self, name: &str, slot: &str, hole_is_container: bool) -> String {
        if hole_is_container {
            format!("Method '{name}' satisfies 'req fn {name}' but {slot} is not a container, which the requirement declares")
        } else {
            format!("Method '{name}' satisfies 'req fn {name}' but {slot} is a container, which the requirement does not declare")
        }
    }

    /// The exposed method that fills a `req fn` hole, matched by its plain name.
    fn satisfying_method(&self, decl: &HirTypeDecl, name: Symbol) -> Option<HirId<HirStmt>> {
        decl.methods.iter().copied().find(|m| matches!(self.hir.get(m), HirStmt::Fn(h) if h.name == name))
    }

    fn sorted_difference(&self, set: &HashSet<Symbol>, other: &HashSet<Symbol>) -> Vec<String> {
        let mut names: Vec<String> = set.difference(other).map(|o| self.hir.text(*o).to_string()).collect();
        names.sort();
        names
    }

    pub(super) fn trait_member(&self, name: &str, node: &HirId<HirExpr>) -> Result<Typed, anyhow::Error> {
        let in_surface = self.current_trait_surface.as_ref().is_some_and(|surface| surface.iter().any(|m| self.hir.text(*m) == name));
        if !in_surface {
            return Err(self.error_help(format!("'{}' is not declared or required by this trait", name), node, "declare it or add a 'req'"));
        }
        Ok(Typed::unknown())
    }

    /// Whether a member's return conforms to a trait method's.
    fn ret_conforms(&self, host: &RetSig, trait_ret: &RetSig) -> bool {
        if trait_ret.void {
            return true;
        }
        if trait_ret.obligations.contains(&self.sigs.opt) {
            // The trait is nullable: the host may be non-null or nullable, but must return a value.
            return !host.void;
        }
        // The trait is non-null: the host must return a non-null value.
        !host.void && !host.obligations.contains(&self.sigs.opt)
    }
}

/// Splits a folded `"Trait.method"` alias into its trait and base method names. Lowering folds
/// an overridden trait method under this dotted name.
fn split_trait_alias(name: &str) -> Option<(&str, &str)> {
    name.split_once('.')
}

/// Quotes each name and joins them, like `'fails', 'opt'`.
fn quote_list(names: &[String]) -> String {
    names.iter().map(|n| format!("'{n}'")).collect::<Vec<_>>().join(", ")
}

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
                return Err(self.error_labeled("override is more nullable than the trait allows".to_string(),
                    &host, format!("`{base}` may return null where trait '{trait_name}' declares non-null")));
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
            let type_name = self.hir.text(decl.name);
            let trait_name = self.hir.text(req.trait_name);

            // The `[obl]` container shape is invariant: a bare value and a container are not
            // interchangeable, since the trait body reads one by index and the other directly.
            if req.ret.container != sat.clause.container {
                let (mine, theirs) = shape_words(req.ret.container);
                return Err(self.error_ctx("return shape does not match the trait",
                    self.hir.pos(&method), format!("`{type_name}.{name}` returns {mine}"),
                    &req.pos, format!("`{trait_name}.{name}` declares {theirs} return")));
            }

            // The capability axis is invariant.
            if req.ret.capability.is_mut() && !sat.clause.capability.is_mut() {
                return Err(self.error_ctx_help("return is immutable but trait requires mutable",
                    &sat.sig_pos, format!("`{type_name}.{name}` returns an immutable value"),
                    &req.pos, format!("`{trait_name}.{name}` requires a mutable return"),
                    format!("add `mut` to the return clause of `{type_name}.{name}`")));
            }
            if !req.ret.capability.is_mut() && sat.clause.capability.is_mut() {
                return Err(self.error_ctx_help("return is mutable but trait requires immutable",
                    &sat.sig_pos, format!("`{type_name}.{name}` returns a mutable value"),
                    &req.pos, format!("`{trait_name}.{name}` requires an immutable return"),
                    format!("drop `mut` from the return clause of `{type_name}.{name}`")));
            }

            // A satisfier may not owe a return obligation the requirement does not permit.
            let diff = self.sorted_difference(&sig.ret.obligations, &self.clause_owed(&req.ret));
            if !diff.is_empty() {
                return Err(self.error_ctx("return owes an obligation the trait forbids",
                    self.hir.pos(&method), format!("`{type_name}.{name}` returns a value owing {}", quote_list(&diff)),
                    &req.pos, format!("`{trait_name}.{name}` forbids {}", quote_list(&diff))));
            }

            // A satisfier's parameter must accept at least the obligations the hole passes it.
            for (i, hole) in req.params.iter().enumerate() {
                let Some(sat_param) = sat.params.get(i) else { continue };
                if hole.clause.container != sat_param.clause.container {
                    let (mine, theirs) = shape_words(hole.clause.container);
                    return Err(self.error_ctx("parameter shape does not match the trait",
                        self.hir.pos(&sat_param.name), format!("`{type_name}.{name}` takes {mine}"),
                        &req.pos, format!("`{trait_name}.{name}` declares {theirs} parameter")));
                }

                // A `*mut` hole only accepts a `*mut` satisfier. A borrow hole accepts either.
                if hole.clause.capability.is_move() && !sat_param.clause.capability.is_move() {
                    let param = self.hir.text(self.ident_sym(&sat_param.name));
                    return Err(self.error_ctx_help("parameter is less permissive than the trait requires",
                        &sat_param.pos, format!("`{type_name}.{name}` only borrows `{param}` here (`mut`)"),
                        &hole.pos, format!("`{trait_name}.{name}` requires ownership of `{param}` (`*mut`)"),
                        format!("take ownership of `{param}` to match the trait: `{param}: *mut`")));
                }

                let Some(accepted) = sig.param_clauses.get(i) else { continue };
                let missing = self.sorted_difference(&self.clause_owed(&hole.clause), accepted);
                if !missing.is_empty() {
                    let param = self.hir.text(self.ident_sym(&sat_param.name));
                    return Err(self.error_ctx("parameter rejects an obligation the trait passes",
                        self.hir.pos(&sat_param.name), format!("`{type_name}.{name}` does not accept {} for `{param}`", quote_list(&missing)),
                        &req.pos, format!("`{trait_name}.{name}` passes {}", quote_list(&missing))));
                }
            }
        }
        Ok(())
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

fn shape_words(container: bool) -> (&'static str, &'static str) {
    if container { ("a bare value", "a container") } else { ("a container", "a bare") }
}

/// Quotes each name and joins them, like `'fails', 'opt'`.
fn quote_list(names: &[String]) -> String {
    names.iter().map(|n| format!("'{n}'")).collect::<Vec<_>>().join(", ")
}

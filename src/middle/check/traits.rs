//! Trait-contract checks: surface use and override return conformance.

use crate::middle::hir::{HirExpr, HirId, HirStmt, HirTypeDecl, ReturnShape};

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
            let (Some(host), Some(trait_ret)) = (host, self.sigs.fns.get(method).map(|f| f.ret)) else { continue };
            let Some(host_ret) = self.sigs.fns.get(&host).map(|f| f.ret) else { continue };
            if !ret_conforms(host_ret, trait_ret) {
                return Err(self.error(format!("Method '{}' overrides trait '{}' but its return is more nullable than the trait declares", base, trait_name), &host));
            }
        }
        Ok(())
    }

    pub(super) fn trait_member(&self, name: &str, node: &HirId<HirExpr>) -> Result<Typed, anyhow::Error> {
        let in_surface = self.current_trait_surface.as_ref().is_some_and(|surface| surface.iter().any(|m| self.hir.text(*m) == name));
        if !in_surface {
            return Err(self.error(format!("'{}' is not declared or required by this trait; declare it or add a 'req'", name), node));
        }
        Ok(Typed::unknown())
    }
}

/// Splits a folded `"Trait.method"` alias into its trait and base method names. Lowering folds
/// an overridden trait method under this dotted name.
fn split_trait_alias(name: &str) -> Option<(&str, &str)> {
    name.split_once('.')
}

/// Whether a member's return shape conforms to a trait method's. An override may be non-null
/// where the trait is nullable, but not the reverse. A void trait method accepts any return,
/// since its result is ignored.
fn ret_conforms(host_ret: ReturnShape, trait_ret: ReturnShape) -> bool {
    match trait_ret {
        ReturnShape::Void | ReturnShape::Inferred => true,
        ReturnShape::NonNull => host_ret == ReturnShape::NonNull,
        ReturnShape::Nullable => matches!(host_ret, ReturnShape::NonNull | ReturnShape::Nullable),
    }
}

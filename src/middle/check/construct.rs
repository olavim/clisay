//! Construction checks: non-null field completeness at a brace.

use std::collections::HashSet;

use crate::core::objects::TypeMember;
use crate::middle::hir::{HirExpr, HirFnDecl, HirId, Symbol};

use super::{Checker, FieldInfo, TypeTag};

impl<'a> Checker<'a> {
    /// Iterates a type's fields with the facts the construction check needs.
    fn fields(&self, type_name: Symbol) -> impl Iterator<Item = FieldInfo> + 'a {
        let layout = self.layout_of(type_name);
        layout.into_iter().flat_map(move |layout| {
            layout.members.iter().filter_map(move |(name, member)| {
                if !matches!(member, TypeMember::Field(_)) {
                    return None;
                }
                Some(FieldInfo {
                    name: *name,
                    non_null: !layout.is_nullable(*name),
                    public: layout.is_public(*name),
                })
            })
        })
    }

    /// Checks a lambda body.
    pub(super) fn lambda(&mut self, decl: &HirFnDecl, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        self.function(None, self.sigs.lambda_writes.get(node), decl)
    }

    /// A brace must supply every non-null public field.
    pub(super) fn check_construction(&self, type_name: Symbol, braced: &HashSet<Symbol>, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let mut missing: Vec<Symbol> = self.fields(type_name)
            .filter(|field| field.non_null && field.public && !braced.contains(&field.name))
            .map(|field| field.name)
            .collect();
        // The field set iterates in a nondeterministic hash order, so report a stable one.
        missing.sort_by_key(|field| self.hir.text(*field));
        if let Some(field) = missing.first() {
            return Err(self.error(format!("Construction of '{}' is missing non-null field '{}'", self.hir.text(type_name), self.hir.text(*field)), node));
        }
        Ok(())
    }

    pub(super) fn construct_tag(&self, callee: &HirId<HirExpr>) -> TypeTag {
        self.sigs.type_named(self.hir, callee).map_or(TypeTag::Unknown, TypeTag::Concrete)
    }
}

//! Construction shape: a brace supplies every non-null public field.

use crate::core::objects::TypeMember;
use crate::middle::diagnose::Diagnose;
use crate::middle::hir::{HirExpr, HirId, HirStmt, Symbol};
use crate::middle::obligations::Obligations;

use super::Shape;

/// One field's declared facts, as the completeness check reads them.
struct FieldInfo {
    name: Symbol,
    non_null: bool,
    public: bool,
}

impl<'a> Shape<'a> {
    /// The fields a brace supplies, against the fields the type declares.
    pub(super) fn construct(&self, callee: &HirId<HirExpr>, brace: &[(Symbol, HirId<HirExpr>)]) -> Result<(), anyhow::Error> {
        let Some(decl) = self.resolved().type_named(callee) else { return Ok(()) };
        let braced: Obligations = brace.iter().map(|(name, _)| *name).collect();
        self.check_construction(&decl, &braced, callee)
    }

    /// Iterates a type's fields with the facts the construction check needs.
    fn fields(&self, decl: &HirId<HirStmt>) -> impl Iterator<Item = FieldInfo> + 'a {
        let layout = self.layout_of(decl);
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

    /// A brace must supply every non-null public field.
    fn check_construction(&self, decl: &HirId<HirStmt>, braced: &Obligations, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        // A non-null field a brace cannot set would be left null, breaking the non-null guarantee.
        let mut unreachable: Vec<Symbol> = self.fields(decl)
            .filter(|field| field.non_null && !field.public)
            .map(|field| field.name)
            .collect();
        // The field set iterates in a nondeterministic hash order, so report a stable one.
        unreachable.sort_by_key(|field| self.hir.text(*field));
        if let Some(field) = unreachable.first() {
            return Err(self.error_help(
                format!("'{}' cannot be brace-constructed: non-null field '{}' is not public, so a brace cannot set it", self.type_text(decl), self.hir.text(*field)),
                node,
                format!("make '{}' public, give it the `opt` obligation (`{}: opt`), or construct with a factory `{}(..)`", self.hir.text(*field), self.hir.text(*field), self.type_text(decl))));
        }

        let mut missing: Vec<Symbol> = self.fields(decl)
            .filter(|field| field.non_null && field.public && !braced.contains(&field.name))
            .map(|field| field.name)
            .collect();
        missing.sort_by_key(|field| self.hir.text(*field));
        if let Some(field) = missing.first() {
            return Err(self.error(format!("Construction of '{}' is missing non-null field '{}'", self.type_text(decl), self.hir.text(*field)), node));
        }
        Ok(())
    }
}

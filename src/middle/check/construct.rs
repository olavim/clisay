//! Construction checks: the init-body seal and non-null field completeness.

use std::collections::HashSet;

use crate::core::objects::TypeMember;
use crate::middle::hir::{HirExpr, HirFnDecl, HirId, HirStmt, Symbol};

use super::{Checker, FieldInfo, TypeTag};

/// State of the construction seal. Inside an `init` body the partially-built `this` may assign
/// and read its own fields. It may not otherwise escape.
#[derive(Default)]
pub(super) struct Seal {
    assigned: Option<HashSet<Symbol>>,
    /// Whether `this` was used since the flag was last reset. Catches a closure in `init`
    /// capturing the partially-built `this`.
    this_seen: bool,
}

impl Seal {
    /// Whether checking is currently inside an init body.
    pub(super) fn in_init(&self) -> bool {
        self.assigned.is_some()
    }

    /// Enters an init body seeded with the fields already provided. Returns the previous state
    /// to hand back to `restore` on exit.
    pub(super) fn enter(&mut self, seed: HashSet<Symbol>) -> Option<HashSet<Symbol>> {
        self.assigned.replace(seed)
    }

    /// Suspends the seal for a nested function body so it is not seen as inside the init.
    pub(super) fn suspend(&mut self) -> Option<HashSet<Symbol>> {
        self.assigned.take()
    }

    /// Restores the assigned-set state saved by `enter` or `suspend`.
    pub(super) fn restore(&mut self, saved: Option<HashSet<Symbol>>) {
        self.assigned = saved;
    }

    pub(super) fn mark_assigned(&mut self, field: Symbol) {
        if let Some(assigned) = self.assigned.as_mut() {
            assigned.insert(field);
        }
    }

    pub(super) fn is_assigned(&self, field: Symbol) -> bool {
        self.assigned.as_ref().is_some_and(|a| a.contains(&field))
    }

    /// Whether `field` is read in the current init before it is assigned.
    pub(super) fn reads_before_assign(&self, field: Symbol) -> bool {
        self.assigned.as_ref().is_some_and(|a| !a.contains(&field))
    }

    /// Resets the `this`-seen flag for a nested body, returning the previous value.
    pub(super) fn take_this_seen(&mut self) -> bool {
        std::mem::replace(&mut self.this_seen, false)
    }

    pub(super) fn this_seen(&self) -> bool {
        self.this_seen
    }

    pub(super) fn set_this_seen(&mut self, seen: bool) {
        self.this_seen = seen;
    }
}

impl<'a> Checker<'a> {
    /// During init, binding or assigning `this` would let a half-built object outlive the
    /// init. Rejecting it here gives a clearer message than the generic value-use check.
    pub(super) fn reject_this_store(&self, value: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        if self.seal.in_init() && matches!(self.hir.get(value), HirExpr::This) {
            return Err(self.this_seal_error(value, "cannot be stored"));
        }
        Ok(())
    }

    /// Construction-seal error for a disallowed use of `this` in init.
    pub(super) fn this_seal_error(&self, node: &HirId<HirExpr>, action: &str) -> anyhow::Error {
        self.error(format!("the partially-constructed 'this' {action}; inside init it may only assign or read its own fields"), node)
    }

    /// Iterates a type's fields with the facts the definition and construction checks need.
    fn fields(&self, type_name: Symbol) -> impl Iterator<Item = FieldInfo> + 'a {
        let layout = self.layout_of(type_name);
        let assigned = self.sigs.init_fields.get(&type_name);
        layout.into_iter().flat_map(move |layout| {
            layout.members.iter().filter_map(move |(name, member)| {
                if !matches!(member, TypeMember::Field(_)) {
                    return None;
                }
                Some(FieldInfo {
                    name: *name,
                    non_null: !layout.is_nullable(*name),
                    public: layout.is_public(*name),
                    init_assigned: assigned.is_some_and(|a| a.contains(name)),
                })
            })
        })
    }

    /// Every non-null private field must be initialized by the type's `init` or a default.
    pub(super) fn check_field_definitions(&self, type_name: Symbol, node: &HirId<HirStmt>) -> Result<(), anyhow::Error> {
        for field in self.fields(type_name) {
            if field.non_null && !field.public && !field.init_assigned {
                if let Some(assign) = self.sigs.method_field_assigns.get(&type_name).and_then(|m| m.get(&field.name)) {
                    return Err(self.error(format!("Field '{}' must be initialized directly in init or by a default", self.hir.text(field.name)), assign));
                }
                return Err(self.error(format!("Non-null field '{}' is never initialized; give it a default or assign it in init", self.hir.text(field.name)), node));
            }
        }
        Ok(())
    }

    /// Checks a type's `init` under the construction seal: the bare `this` may assign its own
    /// fields, read assigned fields, and call helpers, but may not escape.
    pub(super) fn check_init(&mut self, stmt: &HirId<HirStmt>, type_name: Symbol) -> Result<(), anyhow::Error> {
        let HirStmt::Fn(decl) = self.hir.get(stmt) else { return Ok(()) };
        // An init yields the instance, not a declared value, so its returns are not checked.
        let saved_return = self.current_return.take();
        let result = self.with_frame(&decl.params, |c| {
            // Brace-provided fields are assigned before the init body runs.
            let saved_seal = c.seal.enter(c.brace_provided_fields(type_name));
            c.expr(&decl.body)?;
            c.seal.restore(saved_seal);
            Ok(())
        });
        self.current_return = saved_return;
        result
    }

    /// Returns the public fields of a type that its init does not assign.
    fn brace_provided_fields(&self, type_name: Symbol) -> HashSet<Symbol> {
        self.fields(type_name)
            .filter(|f| f.public && !f.init_assigned)
            .map(|f| f.name)
            .collect()
    }

    /// Checks a lambda body. A closure defined in `init` may not capture the partially-built `this`.
    pub(super) fn lambda(&mut self, decl: &HirFnDecl, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let in_init = self.seal.in_init();
        let outer_seen = self.seal.take_this_seen();
        self.function(decl)?;
        let captured_this = self.seal.this_seen();
        self.seal.set_this_seen(outer_seen);
        if in_init && captured_this {
            return Err(self.error("a closure in init cannot capture the partially-constructed 'this'".to_string(), node));
        }
        Ok(())
    }

    /// A construction must supply every non-null public field the `init` does not assign.
    pub(super) fn check_construction(&self, type_name: Symbol, braced: &HashSet<Symbol>, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        for field in self.fields(type_name) {
            if field.non_null && field.public && !field.init_assigned && !braced.contains(&field.name) {
                return Err(self.error(format!("Construction of '{}' is missing non-null field '{}'", self.hir.text(type_name), self.hir.text(field.name)), node));
            }
        }
        Ok(())
    }

    pub(super) fn construct_tag(&self, callee: &HirId<HirExpr>) -> TypeTag {
        self.sigs.type_named(self.hir, callee).map_or(TypeTag::Unknown, TypeTag::Concrete)
    }
}

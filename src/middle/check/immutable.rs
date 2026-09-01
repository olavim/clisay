//! Immutability: what a value is, and who is allowed to write it.

use crate::middle::diagnose::Diagnose;
use crate::middle::hir::{Capability, HirExpr, HirId, HirStmt, Symbol};

use super::{Checker, Ctx, Mutability, ValueState};

const IMMUTABLE_MUTATION: &str = "cannot mutate an immutable value";

impl<'a> Ctx<'a> {
    pub(super) fn non_var_field_error(&self, decl: &HirId<HirStmt>, field: Symbol, lhs: &HirId<HirExpr>) -> anyhow::Error {
        let name = self.qualified_field_display_name(decl, field);
        self.error_help(format!("Cannot reassign field `{name}`"), lhs,
            format!("you can make `{name}` reassignable by declaring it as `{};`", self.var_decl_error_hint(decl, field)))
    }

    pub(super) fn method_assign_error(&self, field: Symbol, lhs: &HirId<HirExpr>) -> anyhow::Error {
        self.error(format!("Cannot assign to method '{}'", self.hir.text(field)), lhs)
    }

    fn param_needs_mut_error(&self, callee: &HirId<HirExpr>, arg: &HirId<HirExpr>) -> anyhow::Error {
        let (a, c) = (self.arg_display_name(arg), self.callee_display_name(callee));
        self.error_ctx("expected mutable argument".to_string(), self.hir.pos(arg), format!("{a} is immutable"),
            self.hir.pos(callee), format!("{c} expects {a} to be mutable"))
    }

    fn var_decl_error_hint(&self, decl: &HirId<HirStmt>, field: Symbol) -> String {
        let visibility = self.layout_of(decl).map_or("", |layout| {
            if layout.is_public(field) { "pub " } else if layout.is_inner(field) { "inner " } else { "" }
        });
        format!("{visibility}var {}", self.hir.text(field))
    }

    pub(super) fn is_this(&self, target: &HirId<HirExpr>) -> bool {
        match self.hir.get(target) {
            HirExpr::This => true,
            HirExpr::Index(inner, _, _) | HirExpr::SafeAccess(inner, _, _)
            | HirExpr::Assert(inner) | HirExpr::Propagate(inner) | HirExpr::Mut(inner) => self.is_this(inner),
            _ => false,
        }
    }
}

impl<'a> Checker<'a> {
    pub(super) fn binding_is_writable(&self, i: usize) -> bool {
        self.locals[i].writable && self.locals[i].mutability != Mutability::Immutable
    }

    pub(super) fn may_write_through(&self, expr: &HirId<HirExpr>, state: &ValueState) -> bool {
        if state.mutability == Mutability::Immutable {
            return false;
        }
        if self.ctx.is_this(expr) {
            return self.this_is_writable();
        }
        self.local_of(expr).is_none_or(|i| self.binding_is_writable(i))
    }

    pub(super) fn check_arg_mutability(&self, callee: &HirId<HirExpr>, markers: &[Capability], arg_types: &[ValueState], args: &[HirId<HirExpr>]) -> Result<(), anyhow::Error> {
        for (i, &marker) in markers.iter().enumerate() {
            let Some(state) = arg_types.get(i) else { break };

            if marker.is_mut() && !self.may_write_through(&args[i], state) {
                return Err(self.ctx.param_needs_mut_error(callee, &args[i]));
            }
        }
        Ok(())
    }

    pub(super) fn immutable_mutation_error(&self, target: &HirId<HirExpr>, i: usize) -> anyhow::Error {
        let name = self.ctx.hir.text(self.locals[i].name);
        if self.locals[i].param {
            return self.error_help(
                format!("cannot mutate `{name}`, a read-only parameter"), target,
                format!("declare the parameter `mut` to let `{name}` be mutated"));
        }
        self.error_labeled(IMMUTABLE_MUTATION.to_string(), target, format!("`{name}` is immutable"))
    }

    pub(super) fn this_is_writable(&self) -> bool {
        self.checking_factory || self.fn_ctx.receiver.writable
    }

    pub(super) fn is_readonly(&self, target: &HirId<HirExpr>) -> bool {
        if self.ctx.is_this(target) {
            return !self.this_is_writable();
        }
        self.root_local_of(target).is_some_and(|i| !self.locals[i].writable)
    }

    fn mut_this_help(&self) -> String {
        let method = self.fn_ctx.name.map_or("this method".to_string(), |s| format!("`{}`", self.ctx.hir.text(s)));
        format!("declare {method}'s receiver `mut this` to let it mutate the instance")
    }

    pub(super) fn readonly_write_error(&self, target: &HirId<HirExpr>) -> anyhow::Error {
        if !self.ctx.is_this(target) {
            return self.error_labeled(IMMUTABLE_MUTATION.to_string(), target,
                "reached through an immutable value");
        }
        self.error_help("cannot mutate through a read-only receiver".to_string(), target, self.mut_this_help())
    }

    pub(super) fn readonly_receiver_error(&self, decl: &HirId<HirStmt>, field: Symbol, lhs: &HirId<HirExpr>) -> anyhow::Error {
        let name = self.ctx.qualified_field_display_name(decl, field);
        self.error_help(format!("cannot assign `{name}` through a read-only receiver"), lhs, self.mut_this_help())
    }
}

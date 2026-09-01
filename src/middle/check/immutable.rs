//! Immutability: what a value is, and who is allowed to write it.

use crate::middle::diagnose::Diagnose;

use crate::core::objects;
use crate::middle::hir::{Capability, HirExpr, HirId, HirStmt, Symbol};

use super::{PatternBinderSource, Checker, Ctx, Guard, Mutability, ValueState};

impl<'a> Ctx<'a> {
    pub(super) fn non_var_field_error(&self, decl: &HirId<HirStmt>, field: Symbol, lhs: &HirId<HirExpr>) -> anyhow::Error {
        let name = self.qualified_field_display_name(decl, field);
        self.error_help(format!("Cannot reassign field `{name}`"), lhs,
            format!("you can make `{name}` reassignable by declaring it as `{};`", self.var_decl_error_hint(decl, field)))
    }

    pub(super) fn keeps_receiver_error(&self, callee: &HirId<HirExpr>, receiver: &HirId<HirExpr>) -> anyhow::Error {
        let subject = self.quoted_subject(receiver);
        let c = self.callee_display_name(callee);
        self.error_ctx(
            format!("{subject} is mutable and this method stores its receiver; freeze or copy it, or take the receiver by '*mut'"),
            self.hir.pos(receiver), format!("{subject} is mutable"),
            self.hir.pos(callee), format!("{c} stores its receiver"))
    }

    pub(super) fn method_assign_error(&self, field: Symbol, lhs: &HirId<HirExpr>) -> anyhow::Error {
        self.error(format!("Cannot assign to method '{}'", self.hir.text(field)), lhs)
    }

    fn mutable_in_immutable_error(&self, node: &HirId<HirExpr>) -> anyhow::Error {
        self.error_labeled_help(objects::MUTABLE_IN_IMMUTABLE.to_string(), node,
            "this value is mutable", "freeze the value, or mark the container `mut`")
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

    fn is_this(&self, target: &HirId<HirExpr>) -> bool {
        match self.hir.get(target) {
            HirExpr::This => true,
            HirExpr::Index(inner, _, _) | HirExpr::SafeAccess(inner, _, _)
            | HirExpr::Assert(inner) | HirExpr::Propagate(inner) | HirExpr::Mut(inner) => self.is_this(inner),
            _ => false,
        }
    }
}

impl<'a> Checker<'a> {
    pub(super) fn check_container_element_mutability(&mut self, immutable: bool, elem: &ValueState, elem_node: &HirId<HirExpr>, container: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        if !immutable {
            return Ok(());
        }
        match elem.mutability {
            Mutability::Mutable => Err(self.ctx.mutable_in_immutable_error(elem_node)),
            Mutability::Unknown => { self.record_seal_check(container); Ok(()) },
            Mutability::Immutable => Ok(()),
        }
    }

    pub(super) fn check_construct_field_mutability(&mut self, immutable: bool, value: &ValueState, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        if !immutable {
            return Ok(());
        }
        match value.mutability {
            Mutability::Mutable => Err(self.ctx.mutable_in_immutable_error(node)),
            Mutability::Unknown => { self.record_guard(node, Guard::Immutable); Ok(()) },
            Mutability::Immutable => { self.record_elision(node, Guard::Immutable); Ok(()) },
        }
    }

    fn names_immutable_param(&self, value: &HirId<HirExpr>) -> bool {
        self.local_of(value).is_some_and(|i| self.locals[i].param && !self.locals[i].writable)
    }

    fn names_immutable_param_binder(&self, value: &HirId<HirExpr>) -> bool {
        self.local_of(value).is_some_and(|i| {
            self.locals[i].pattern_binder_source == Some(PatternBinderSource::Param) && !self.locals[i].writable
        })
    }

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

    pub(super) fn immutable_receiver_error(&self, callee: &HirId<HirExpr>, receiver: &HirId<HirExpr>, reason: &str) -> anyhow::Error {
        let subject = self.ctx.receiver_subject(receiver);
        let method = self.ctx.callee_display_name(callee);
        let help = if self.names_immutable_param_binder(receiver) {
            format!("{method} {reason}; mark the parameter `mut (..)` to destructure a mutable value, or work through the value it came out of")
        } else if self.names_immutable_param(receiver) {
            format!("{method} {reason}; declare the parameter `mut` to let {subject} be mutated")
        } else {
            format!("{method} {reason}; construct the value with `mut` to call it")
        };
        self.error_labeled_help("expected mutable receiver".to_string(), receiver,
            format!("{subject} is immutable"), help)
    }

    pub(super) fn immutable_mutation_error(&self, target: &HirId<HirExpr>, i: usize) -> anyhow::Error {
        let name = self.ctx.hir.text(self.locals[i].name);
        if self.locals[i].param {
            return self.error_help(
                format!("cannot mutate `{name}`, a read-only parameter"), target,
                format!("declare the parameter `mut` to let `{name}` be mutated"));
        }
        self.error_labeled(objects::IMMUTABLE_MUTATION.to_string(), target, format!("`{name}` is immutable"))
    }

    pub(super) fn discharge_freeze(&mut self, arg: &HirId<HirExpr>) {
        let HirExpr::Identifier(name) = self.ctx.hir.get(arg) else { return };
        if let Some(i) = self.frame_index_of(*name) {
            self.locals[i].mutability = Mutability::Immutable;
        }
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
            return self.error_labeled(objects::IMMUTABLE_MUTATION.to_string(), target,
                "reached through an immutable value");
        }
        self.error_help("cannot mutate through a read-only receiver".to_string(), target, self.mut_this_help())
    }

    pub(super) fn readonly_receiver_error(&self, decl: &HirId<HirStmt>, field: Symbol, lhs: &HirId<HirExpr>) -> anyhow::Error {
        let name = self.ctx.qualified_field_display_name(decl, field);
        self.error_help(format!("cannot assign `{name}` through a read-only receiver"), lhs, self.mut_this_help())
    }
}

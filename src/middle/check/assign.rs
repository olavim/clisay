//! Assignment checks: write conformance and mutability.

use std::collections::HashSet;

use crate::core::objects::TypeMember;
use crate::middle::hir::{HirExpr, HirId, Symbol};

use super::{Checker, Flow, TypeTag, Typed, Violation};

impl<'a> Checker<'a> {
    pub(super) fn assign(&mut self, lhs: &HirId<HirExpr>, rhs: &HirId<HirExpr>) -> Result<Typed, anyhow::Error> {
        self.reject_this_store(rhs)?;
        let typed = self.expr(rhs)?;
        match self.hir.get(lhs) {
            HirExpr::Identifier(name) => {
                let name = *name;
                if let Some(i) = self.frame_index_of(name) {
                    // A function binding names a declaration, not a reassignable slot.
                    if self.locals[i].func.is_some() {
                        return Err(self.error(format!("Cannot reassign `{}`; it names a function", self.hir.text(name)), lhs));
                    }
                    let (mutable, assigned) = (self.locals[i].mutable, self.locals[i].assigned);
                    let owed = self.locals[i].owed.clone();
                    if !mutable && assigned {
                        let text = self.hir.text(name);
                        if self.locals[i].binder {
                            return Err(self.error_help(format!("Cannot reassign matcher binder `{text}`"), lhs,
                                format!("copy it into a `say mut {text}` first to change it")));
                        }
                        return Err(self.error_help(format!("Cannot reassign immutable binding `{text}`"), lhs,
                            format!("you can make `{text}` mutable by declaring it as `say mut {text}`")));
                    }
                    self.check_into_slot(&typed.flow, &owed, name, lhs)?;
                    self.locals[i].assigned = true;
                    self.locals[i].tag = typed.tag.clone();
                    // The immutability fact follows the value, so a rebind drops it.
                    self.locals[i].immutable = false;
                    self.reset_narrowing(i, matches!(typed.flow, Flow::Clean));
                } else if self.sigs.types_by_name.contains_key(&name) {
                    // A type binding names a declaration, not a reassignable slot.
                    return Err(self.error(format!("Cannot reassign `{}`; it names a type", self.hir.text(name)), lhs));
                } else {
                    // `field = ...` is implicitly `this.field = ...`
                    self.assign_field_this(name, &typed.flow, lhs, rhs)?;
                }
            },
            HirExpr::Index(target, member, is_dot) => self.assign_index(target, member, *is_dot, &typed.flow, lhs, rhs)?,
            _ => {},
        }
        Ok(typed)
    }

    /// Checks an assignment `target.member = value`.
    fn assign_index(&mut self, target: &HirId<HirExpr>, member: &HirId<HirExpr>, is_dot: bool, value: &Flow, lhs: &HirId<HirExpr>, rhs: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        // `this.field = ...` and `this["field"] = ...` both assign a field of the enclosing type.
        if matches!(self.hir.get(target), HirExpr::This) {
            if let Some(field) = self.string_member(member) {
                self.assign_field_this(field, value, lhs, rhs)?;
            }
            return Ok(());
        }

        // A frozen value rejects mutation at compile time.
        if let Some(name) = self.immutable_target(target) {
            return Err(self.error(format!("'{}' is immutable and cannot be mutated", self.hir.text(name)), lhs));
        }

        // A bracket index `obj[expr] = ...` is the dynamic data path. It bypasses the field rules.
        if !is_dot {
            return Ok(());
        }

        let receiver = self.receiver(target)?;
        let Some(field) = self.string_member(member) else { return Ok(()) };
        if let TypeTag::Concrete(type_name) = &receiver.tag {
            self.assign_field_external(*type_name, field, value, lhs, rhs)?;
        }
        Ok(())
    }

    /// The name of a mutation target that is a provably immutable local, or `None`.
    fn immutable_target(&self, target: &HirId<HirExpr>) -> Option<Symbol> {
        let HirExpr::Identifier(name) = self.hir.get(target) else { return None };
        let i = self.frame_index_of(*name)?;
        self.locals[i].immutable.then_some(*name)
    }

    /// Checks an assignment `this.field = value`.
    fn assign_field_this(&mut self, field: Symbol, flow: &Flow, lhs: &HirId<HirExpr>, rhs: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let Some(type_name) = self.current_type else { return Ok(()) };
        let (is_field, nullable, mutable) = match self.layout_of(type_name) {
            Some(layout) => (matches!(layout.members.get(&field), Some(TypeMember::Field(_))), layout.is_nullable(field), layout.is_mutable(field)),
            None => return Ok(()),
        };

        if !is_field {
            return Ok(());
        }

        if !mutable {
            if !self.seal.in_init() {
                return Err(self.immutable_field_error(type_name, field, lhs));
            }
            if self.seal.is_assigned(field) {
                return Err(match self.seal.first_assign(field) {
                    Some(first) => self.double_init_error(type_name, field, first, *lhs),
                    None => self.immutable_field_error(type_name, field, lhs),
                });
            }
        }

        self.check_into_field(flow, nullable, field, rhs)?;
        self.seal.mark_assigned(field, *lhs);
        Ok(())
    }

    /// Checks an external write `obj.field = value` on a known type.
    fn assign_field_external(&mut self, type_name: Symbol, field: Symbol, flow: &Flow, lhs: &HirId<HirExpr>, rhs: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let field_info = match self.layout_of(type_name) {
            Some(layout) => match layout.members.get(&field) {
                Some(TypeMember::Field(_)) => Some((layout.is_public(field), layout.is_nullable(field), layout.is_mutable(field))),
                _ => None,
            },
            None => None,
        };
        // A non-field or non-public member is invisible to external code.
        let Some((public, nullable, mutable)) = field_info else { return Ok(()) };
        if !public {
            return Ok(());
        }
        if !mutable {
            return Err(self.immutable_field_error(type_name, field, lhs));
        }
        self.check_into_field(flow, nullable, field, rhs)
    }

    fn immutable_field_error(&self, type_name: Symbol, field: Symbol, lhs: &HirId<HirExpr>) -> anyhow::Error {
        let name = self.qualified_field(type_name, field);
        self.error_help(format!("Cannot assign immutable field `{name}`"), lhs,
            format!("you can make `{name}` mutable by declaring it as `{};`", self.mut_decl_hint(type_name, field)))
    }

    fn double_init_error(&self, type_name: Symbol, field: Symbol, first: HirId<HirExpr>, second: HirId<HirExpr>) -> anyhow::Error {
        let name = self.qualified_field(type_name, field);
        self.error_two_spans(
            format!("Immutable field `{name}` is initialized more than once"),
            &second, "and a second time here", &first, "first initialized here",
            format!("keep only one, or make `{name}` mutable by declaring it as `{};`", self.mut_decl_hint(type_name, field)))
    }

    /// The `Type.field` name shown in field diagnostics.
    fn qualified_field(&self, type_name: Symbol, field: Symbol) -> String {
        format!("{}.{}", self.hir.text(type_name), self.hir.text(field))
    }

    /// The declaration a field needs to become mutable, e.g. `pub mut value` or `mut value`,
    /// keeping the field's current visibility.
    fn mut_decl_hint(&self, type_name: Symbol, field: Symbol) -> String {
        let visibility = self.layout_of(type_name).map_or("", |layout| {
            if layout.is_public(field) { "pub " } else if layout.is_inner(field) { "inner " } else { "" }
        });
        format!("{visibility}mut {}", self.hir.text(field))
    }

    /// Checks a value moving into a field per the field's nullability.
    fn check_into_field(&mut self, flow: &Flow, field_nullable: bool, field: Symbol, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        // Storing into a field persists the value, which a `discharge to escape` value forbids.
        self.reject_escape(flow, node)?;
        let text = self.hir.text(field);
        let void = || format!("Cannot assign a void result to field '{text}'; the call returns no value");
        if field_nullable {
            // A nullable field still rejects a void result, which is not a value.
            if flow.is_void() {
                return Err(self.error(void(), node));
            }
            // An unknown value into an `opt` field is guarded against every object witness it may be.
            if matches!(flow, Flow::Unknown) {
                self.record_boundary_barrier(node, &HashSet::from([self.sigs.opt]));
            }
            return Ok(());
        }
        match self.non_null_violation(flow, node) {
            None => Ok(()),
            Some(Violation::Void) => Err(self.error(void(), node)),
            Some(Violation::Null) => Err(self.error(format!("Cannot assign null to non-null field '{text}'"), node)),
            Some(Violation::Nullable) => Err(self.error(format!("Cannot assign a nullable value to non-null field '{text}'"), node)),
        }
    }

    /// Checks a brace-construction value against its field's declared nullability.
    pub(super) fn check_brace_field(&mut self, type_name: Symbol, field: Symbol, flow: &Flow, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let nullable = match self.layout_of(type_name) {
            Some(layout) => layout.is_nullable(field),
            None => return Ok(()),
        };
        self.check_into_field(flow, nullable, field, node)
    }
}

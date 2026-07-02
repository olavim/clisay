//! Assignment checks: write conformance and mutability.

use crate::core::objects::TypeMember;
use crate::middle::hir::{HirExpr, HirId, Symbol};

use super::{Checker, Nullness, TypeTag, Typed, Violation};

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
                        return Err(self.error(format!("Cannot reassign '{}'; it names a function", self.hir.text(name)), lhs));
                    }
                    let (mutable, assigned, declared_nullable) =
                        (self.locals[i].mutable, self.locals[i].assigned, self.locals[i].declared_nullable);
                    if !mutable && assigned {
                        if self.locals[i].binder {
                            return Err(self.error(format!("Cannot reassign matcher binder '{}'; copy it into a `say mut` to change it", self.hir.text(name)), lhs));
                        }
                        return Err(self.error(format!("Cannot reassign immutable binding '{}'; declare it 'mut'", self.hir.text(name)), lhs));
                    }
                    self.check_into_slot(typed.nullness, declared_nullable, name, lhs)?;
                    self.locals[i].assigned = true;
                    self.locals[i].tag = typed.tag.clone();
                    self.reset_narrowing(i, matches!(typed.nullness, Nullness::NonNull));
                } else if self.sigs.types_by_name.contains_key(&name) {
                    // A type binding names a declaration, not a reassignable slot.
                    return Err(self.error(format!("Cannot reassign '{}'; it names a type", self.hir.text(name)), lhs));
                } else {
                    // `field = ...` is implicitly `this.field = ...`
                    self.assign_field_this(name, typed.nullness, lhs, rhs)?;
                }
            },
            HirExpr::Index(target, member, is_dot) => self.assign_index(target, member, *is_dot, typed.nullness, lhs, rhs)?,
            _ => {},
        }
        Ok(typed)
    }

    /// Checks an assignment `target.member = value`.
    fn assign_index(&mut self, target: &HirId<HirExpr>, member: &HirId<HirExpr>, is_dot: bool, value: Nullness, lhs: &HirId<HirExpr>, rhs: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        // `this.field = ...` and `this["field"] = ...` both assign a field of the enclosing type.
        if matches!(self.hir.get(target), HirExpr::This) {
            if let Some(field) = self.string_member(member) {
                self.assign_field_this(field, value, lhs, rhs)?;
            }
            return Ok(());
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

    /// Checks an assignment `this.field = value`.
    fn assign_field_this(&mut self, field: Symbol, value: Nullness, lhs: &HirId<HirExpr>, rhs: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let (is_field, nullable, mutable) = match self.current_type.and_then(|t| self.layout_of(t)) {
            Some(layout) => (matches!(layout.members.get(&field), Some(TypeMember::Field(_))), layout.is_nullable(field), layout.is_mutable(field)),
            None => return Ok(()),
        };
        if !is_field {
            return Ok(());
        }
        if !mutable {
            if !self.seal.in_init() {
                return Err(self.error(format!("Cannot assign immutable field '{}' in a method; declare it 'mut'", self.hir.text(field)), lhs));
            }
            if self.seal.is_assigned(field) {
                return Err(self.error(format!("Immutable field '{}' is assigned more than once; declare it 'mut'", self.hir.text(field)), lhs));
            }
        }
        self.check_into_field(value, nullable, field, rhs)?;
        self.seal.mark_assigned(field);
        Ok(())
    }

    /// Checks an external write `obj.field = value` on a known type.
    fn assign_field_external(&mut self, type_name: Symbol, field: Symbol, value: Nullness, lhs: &HirId<HirExpr>, rhs: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
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
            return Err(self.error(format!("Cannot assign immutable field '{}' from outside its type; declare it 'mut'", self.hir.text(field)), lhs));
        }
        self.check_into_field(value, nullable, field, rhs)
    }

    /// Checks a value moving into a field per the field's nullability.
    fn check_into_field(&mut self, nullness: Nullness, field_nullable: bool, field: Symbol, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let text = self.hir.text(field);
        let void = || format!("Cannot assign a void result to field '{text}'; the call returns no value");
        if field_nullable {
            // A nullable field still rejects a void result, which is not a value.
            return if nullness.is_void() { Err(self.error(void(), node)) } else { Ok(()) };
        }
        match self.non_null_violation(nullness, node) {
            None => Ok(()),
            Some(Violation::Void) => Err(self.error(void(), node)),
            Some(Violation::Null) => Err(self.error(format!("Cannot assign null to non-null field '{text}'"), node)),
            Some(Violation::Nullable) => Err(self.error(format!("Cannot assign a nullable value to non-null field '{text}'"), node)),
        }
    }

    /// Checks a brace-construction value against its field's declared nullability.
    pub(super) fn check_brace_field(&mut self, type_name: Symbol, field: Symbol, value: Nullness, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let nullable = match self.layout_of(type_name) {
            Some(layout) => layout.is_nullable(field),
            None => return Ok(()),
        };
        self.check_into_field(value, nullable, field, node)
    }
}

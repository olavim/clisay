//! Type and trait layout: building each type's runtime member layout, resolving `this`-member
//! accesses, and validating brace construction.

use indexmap::IndexSet;
use std::collections::HashSet;

use anyhow::bail;

use crate::compiler_error;
use crate::core::objects::TypeMember;
use crate::middle::hir::{
    HirExpr, HirFnDecl, HirId, HirLiteral, HirStmt, HirTypeDecl, ReturnShape, Symbol,
};

use super::{FnKind, MemberClause, Resolver, TypeFrame, TypeLayout};

impl<'a> Resolver<'a> {
    /// The per-trait renamed slot for `name`.
    fn private_member(&self, name: Symbol) -> Option<Symbol> {
        let trait_sym = self.current_trait?;
        let frame = self.type_frames.last()?;
        frame.trait_privates.get(&trait_sym).and_then(|m| m.get(&name)).copied()
    }

    /// The member id `name` resolves to as an implicit-`this` field of the enclosing type.
    pub(super) fn this_field_id(&self, name: Symbol) -> Option<u8> {
        let layout = &self.type_frames.last()?.layout;
        layout.resolve_id(self.private_member(name).unwrap_or(name))
    }

    pub(super) fn deny_private_member<T: 'static>(&self, name: Symbol, node: &HirId<T>) -> Result<(), anyhow::Error> {
        if self.type_frames.last().is_some_and(|f| f.private_names.contains(&name)) {
            compiler_error!(self, node, "Member '{}' is private", self.hir.text(name));
        }
        Ok(())
    }

    fn is_builtin_type(&self, decl: &HirId<HirStmt>) -> bool {
        matches!(self.hir.get(decl), HirStmt::Type(decl) if decl.builtin.is_some())
    }

    /// Resolves and validates a brace construction `C { field: value, ... }`.
    pub(super) fn construct(&mut self, expr: &HirId<HirExpr>, callee: &HirId<HirExpr>, brace: &[(Symbol, HirId<HirExpr>)]) -> Result<(), anyhow::Error> {
        self.expression(callee)?;
        for (_, v) in brace { self.expression(v)?; }

        let HirExpr::Identifier(type_name) = self.hir.get(callee) else {
            compiler_error!(self, callee, "Brace construction requires a type name");
        };
        let type_name = *type_name;
        let decl = self.resolve_type_decl(type_name);

        if decl.is_some_and(|decl| self.is_builtin_type(&decl)) {
            compiler_error!(self, callee, "'{}' cannot be built with a brace", self.hir.text(type_name));
        }

        // A trait resolves to a declaration but has no layout, so a brace cannot build one.
        let Some(layout) = decl.and_then(|decl| self.bindings.layout_of_decl(&decl)).cloned() else {
            compiler_error!(self, callee, "'{}' is not a type", self.hir.text(type_name));
        };

        // A brace does not run the factory, so the factory's arity does not gate it.
        let mut seen: HashSet<Symbol> = HashSet::new();
        let mut ids = Vec::with_capacity(brace.len());
        for (field, _) in brace {
            if !seen.insert(*field) {
                compiler_error!(self, expr, "Duplicate field '{}' in construction of '{}'", self.hir.text(*field), self.hir.text(type_name));
            }
            match layout.resolve(*field) {
                Some(TypeMember::Field(id)) => {
                    if layout.non_public.contains(&id) {
                        compiler_error!(self, expr, "Field '{}' of '{}' is not public", self.hir.text(*field), self.hir.text(type_name));
                    }
                    ids.push(id);
                },
                Some(TypeMember::Method(_)) => compiler_error!(self, expr, "'{}' is a method of '{}', not a field", self.hir.text(*field), self.hir.text(type_name)),
                None => compiler_error!(self, expr, "'{}' has no field '{}'", self.hir.text(type_name), self.hir.text(*field)),
            }
        }
        self.bindings.construct_fields.insert(*expr, ids);
        Ok(())
    }

    pub(super) fn this_member_access(&mut self, target: &HirId<HirExpr>, member: &HirId<HirExpr>, _is_store: bool) -> Result<(), anyhow::Error> {
        self.resolve_this(target)?;
        let target_type = self.current_type().clone();

        let member_name = match self.hir.get(member) {
            HirExpr::Literal(HirLiteral::String(name)) => self.hir.symbol_of(name),
            HirExpr::Literal(_) => compiler_error!(self, target, "Invalid index: only member names index an instance"),
            _ => compiler_error!(self, target, "Invalid index: 'this' has no computed member; member names are statically known"),
        };

        let private_member_name = member_name.map(|name| self.private_member(name).unwrap_or(name));
        if let Some(private_member_id) = private_member_name.and_then(|name| target_type.resolve_id(name)) {
            self.bindings.members.insert(*target, private_member_id);
            return Ok(());
        }

        if self.validating_trait {
            if let HirExpr::Literal(HirLiteral::String(name)) = self.hir.get(member) {
                compiler_error!(self, target, "Trait '{}' accesses undeclared member 'this.{}'; use `with`, `req`, or `req fn` to provide it",
                    self.hir.text(target_type.name), name);
            }
        }

        if let Some(name) = member_name {
            self.deny_private_member(name, target)?;
        }

        let type_name = self.hir.text(target_type.name);
        if let Some(member_name) = member_name {
            let member_name = self.hir.text(member_name);
            compiler_error!(self, target, "Invalid index: {type_name} doesn't have member {member_name}")
        } else {
            compiler_error!(self, target, "Invalid index: {type_name} doesn't have that member")
        }
    }

    fn current_type(&self) -> &TypeLayout {
        &self.type_frames.last().unwrap().layout
    }

    pub(super) fn build_type_layout(&self, decl: &HirTypeDecl) -> Result<TypeLayout, anyhow::Error> {
        if decl.fields.len() + decl.methods.len() >= u8::MAX as usize {
            bail!("Too many members in type '{}'", self.hir.text(decl.name));
        }

        let mut layout = TypeLayout::empty(decl.name);

        let mut next_member_id: u8 = 0;
        for field in &decl.fields {
            layout.members.insert(*field, TypeMember::Field(next_member_id));
            layout.fields.push(next_member_id);
            if !decl.pub_members.contains(field) {
                layout.non_public.insert(next_member_id);
            }
            if decl.inner_members.contains(field) {
                layout.inner.insert(next_member_id);
            }
            if decl.nullable_fields.contains(field) {
                layout.nullable.insert(next_member_id);
            }
            if decl.var_fields.contains(field) {
                layout.reassignable.insert(next_member_id);
            }
            if let Some(clause) = decl.field_clauses.get(field) {
                let owed = clause.owed();
                if !owed.is_empty() {
                    layout.clauses.insert(next_member_id, MemberClause { owed, container: clause.container });
                }
            }
            next_member_id += 1;
        }

        for stmt_id in &decl.methods {
            let method = self.fn_decl(stmt_id);
            layout.members.insert(method.name, TypeMember::Method(next_member_id));
            if !decl.pub_members.contains(&method.name) {
                layout.non_public.insert(next_member_id);
            }
            if method.ret == ReturnShape::Nullable {
                layout.nullable.insert(next_member_id);
            }
            next_member_id += 1;
        }

        // A member id is reserved for the factory whether or not the type declares one.
        layout.factory_id = next_member_id;
        next_member_id += 1;
        layout.member_count = next_member_id;

        Ok(layout)
    }

    fn push_type_frame(&mut self, layout: TypeLayout, decl: &HirTypeDecl) {
        let private_names = decl.trait_privates.values().flat_map(|m| m.keys().copied()).collect();
        self.type_frames.push(TypeFrame {
            layout,
            trait_privates: decl.trait_privates.clone(),
            private_names,
        });
    }

    pub(super) fn type_declaration(&mut self, stmt: &HirId<HirStmt>, decl: &HirTypeDecl) -> Result<(), anyhow::Error> {
        if decl.builtin.is_none() {
            let slot = self.resolve_local(decl.name).expect("type declarations are reserved by hoisting");
            self.bindings.slots.insert(*stmt, slot);
        }

        self.enter_scope();

        let layout = self.bindings.layout_of_decl(stmt).expect("hoisting builds every type's layout").clone();
        self.push_type_frame(layout, decl);

        // Method bodies resolve under the declaring trait's private scope.
        let outer_trait = self.current_trait.take();

        if let HirStmt::Fn(init) = self.hir.get(&decl.init) {
            self.function(init, FnKind::Factory)?;
        }

        for (stmt_id, trait_sym) in decl.methods.iter().zip(&decl.method_traits) {
            self.current_trait = *trait_sym;
            let method = self.fn_decl(stmt_id);
            self.function(method, FnKind::Method)?;
        }

        self.current_trait = outer_trait;
        self.type_frames.pop();
        self.exit_scope(stmt);
        Ok(())
    }

    pub(super) fn record_public_members(&mut self, stmt: &HirId<HirStmt>, members: &IndexSet<Symbol>) {
        self.bindings.surfaces.insert(*stmt, members.iter().copied().collect());
    }

    /// Validates a standalone `trait`.
    pub(super) fn trait_declaration(&mut self, stmt: &HirId<HirStmt>, decl: &HirTypeDecl) -> Result<(), anyhow::Error> {
        self.enter_scope();

        let mut layout = TypeLayout::empty(decl.name);
        let mut id: u8 = 0;
        for name in &decl.surface {
            layout.members.entry(*name).or_insert(TypeMember::Method(id));
            id = id.wrapping_add(1);
        }
        for renamed in decl.trait_privates.values().flat_map(|m| m.values()) {
            layout.members.entry(*renamed).or_insert(TypeMember::Method(id));
            id = id.wrapping_add(1);
        }
        layout.member_count = id;

        self.push_type_frame(layout, decl);

        let outer_trait = std::mem::replace(&mut self.current_trait, Some(decl.name));
        let was_validating = std::mem::replace(&mut self.validating_trait, true);
        for stmt_id in &decl.methods {
            let method = self.fn_decl(stmt_id);
            self.function(method, FnKind::Method)?;
        }
        self.validating_trait = was_validating;
        self.current_trait = outer_trait;

        self.type_frames.pop();
        self.exit_scope(stmt);
        self.record_public_members(stmt, &decl.surface);
        Ok(())
    }

    fn fn_decl(&self, stmt: &HirId<HirStmt>) -> &'a HirFnDecl {
        let HirStmt::Fn(decl) = self.hir.get(stmt) else {
            unreachable!("expected a function statement");
        };
        decl
    }
}

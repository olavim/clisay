//! Type and trait layout: building each type's runtime member layout, resolving `this`-member
//! accesses, and validating brace construction.

use std::collections::HashSet;

use crate::compiler_error;
use crate::core::objects::TypeMember;
use crate::middle::hir::{
    HirExpr, HirFnDecl, HirId, HirLiteral, HirStmt, HirTypeDecl, ReturnShape, Symbol,
};

use super::{FnKind, Resolver, TypeFrame, TypeLayout};

impl<'a> Resolver<'a> {
    /// The per-trait renamed slot for `name` if it's a private member of the trait whose body is
    /// currently being resolved. Handles implicit-`this` private lookup.
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

    /// Reports a member that exists only inside some trait as private rather than missing.
    pub(super) fn deny_private<T: 'static>(&self, name: Symbol, node: &HirId<T>) -> Result<(), anyhow::Error> {
        if self.type_frames.last().is_some_and(|f| f.private_names.contains(&name)) {
            compiler_error!(self, node, "Member '{}' is private", self.hir.text(name));
        }
        Ok(())
    }

    /// Resolves and validates a brace construction `C(args) { field: value, ... }`: the type must
    /// be known, the paren args must match its `init` arity, and each brace field must be a `pub`
    /// field the `init` does not assign, with no duplicates.
    pub(super) fn construct(&mut self, expr: &HirId<HirExpr>, callee: &HirId<HirExpr>, args: &[HirId<HirExpr>], brace: &[(Symbol, HirId<HirExpr>)]) -> Result<(), anyhow::Error> {
        self.expression(callee)?;
        for a in args { self.expression(a)?; }
        for (_, v) in brace { self.expression(v)?; }

        let HirExpr::Identifier(type_name) = self.hir.get(callee) else {
            compiler_error!(self, callee, "Brace construction requires a type name");
        };
        let type_name = *type_name;
        let Some(layout) = self.types.get(&type_name).cloned() else {
            compiler_error!(self, callee, "'{}' is not a type", self.hir.text(type_name));
        };

        if args.len() != layout.init_arity as usize {
            compiler_error!(self, expr, "'{}' is constructed with {} argument(s), but its init takes {}",
                self.hir.text(type_name), args.len(), layout.init_arity);
        }

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
                    if layout.init_assigned.contains(field) {
                        compiler_error!(self, expr, "Field '{}' of '{}' is set by its init and cannot be brace-provided", self.hir.text(*field), self.hir.text(type_name));
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

    /// Resolves a `this` member access (`this.x`, `this["x"]`) to a member id.
    pub(super) fn this_member_access(&mut self, target: &HirId<HirExpr>, member: &HirId<HirExpr>, _is_store: bool) -> Result<(), anyhow::Error> {
        self.require_type(target)?;
        let target_type = self.current_type().clone();

        // Members on `this` are statically known. A string-literal key names a member; a
        // computed `this[expr]` has no statically-known member and is rejected.
        let member_name = match self.hir.get(member) {
            HirExpr::Literal(HirLiteral::String(name)) => self.hir.symbol_of(name),
            HirExpr::Literal(_) => compiler_error!(self, target, "Invalid index: only member names index an instance"),
            _ => compiler_error!(self, target, "Invalid index: 'this' has no computed member; member names are statically known"),
        };

        // Inside a trait body, `this.x` for the trait's own private member resolves to its
        // per-trait slot, otherwise the plain name.
        let lookup = member_name.map(|name| self.private_member(name).unwrap_or(name));
        if let Some(member_id) = lookup.and_then(|name| target_type.resolve_id(name)) {
            self.bindings.members.insert(*target, member_id);
            return Ok(());
        }

        // While validating a standalone trait, an unresolved member is a self-containment
        // violation: the trait reached something it does not declare.
        if self.validating_trait {
            if let HirExpr::Literal(HirLiteral::String(name)) = self.hir.get(member) {
                compiler_error!(self, target, "Trait '{}' accesses undeclared member 'this.{}'; use `with`, `req`, or `req fn` to provide it",
                    self.hir.text(target_type.name), name);
            }
        }

        // A name that resolved to nothing but is some trait's private member is reported as
        // private (it exists, but only inside its declaring trait), not as missing.
        if let Some(name) = member_name {
            self.deny_private(name, target)?;
        }

        let type_name = self.hir.text(target_type.name);
        if let Some(member_name) = member_name {
            let member_name = self.hir.text(member_name);
            compiler_error!(self, target, "Invalid index: {type_name} doesn't have member {member_name}")
        } else {
            compiler_error!(self, target, "Invalid index: {type_name} doesn't have that member")
        }
    }

    pub(super) fn require_type(&self, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        if self.type_frames.is_empty() {
            compiler_error!(self, node, "Cannot use 'this' outside of a type method");
        }
        Ok(())
    }

    fn current_type(&self) -> &TypeLayout {
        &self.type_frames.last().unwrap().layout
    }

    /// Builds a type's runtime [`TypeLayout`], assigning each member its id: own fields,
    /// methods, then the initializer.
    fn build_layout(&self, decl: &HirTypeDecl) -> TypeLayout {
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
            if decl.mut_fields.contains(field) {
                layout.mutable.insert(next_member_id);
            }
            next_member_id += 1;
        }
        for stmt_id in &decl.methods {
            let method = self.fn_decl(stmt_id);
            layout.members.insert(method.name, TypeMember::Method(next_member_id));
            if !decl.pub_members.contains(&method.name) {
                layout.non_public.insert(next_member_id);
            }
            // A method member's nullability is its return nullability.
            if method.ret == ReturnShape::Nullable {
                layout.nullable.insert(next_member_id);
            }
            next_member_id += 1;
        }
        // Every type has its own initializer (declared or virtual).
        layout.init_id = next_member_id;
        next_member_id += 1;
        layout.member_count = next_member_id;

        // Construction facts: the init's arity, and the fields it assigns (defaults + body), which
        // a brace construction may not also provide. A factory-less type has neither.
        if let HirStmt::Fn(init) = self.hir.get(&decl.init) {
            layout.init_arity = init.params.len() as u8;
            let mut assigned = HashSet::new();
            self.collect_assigned_fields(&init.body, &mut assigned);
            layout.init_assigned = assigned;
        }

        layout
    }

    /// Collects the field names a body assigns through `this.<field> = ...`, walking control flow
    /// but not nested functions/lambdas (whose execution isn't guaranteed).
    fn collect_assigned_fields(&self, expr: &HirId<HirExpr>, out: &mut HashSet<Symbol>) {
        match self.hir.get(expr) {
            HirExpr::Assign(target, value) => {
                if let HirExpr::Index(obj, member, true) = self.hir.get(target) {
                    if matches!(self.hir.get(obj), HirExpr::This) {
                        if let HirExpr::Literal(HirLiteral::String(name)) = self.hir.get(member) {
                            if let Some(sym) = self.hir.symbol_of(name) { out.insert(sym); }
                        }
                    }
                }
                self.collect_assigned_fields(value, out);
            },
            HirExpr::Block(stmts) => for s in stmts { self.collect_assigned_fields_stmt(s, out); },
            HirExpr::Unary(_, x) | HirExpr::Is(x, _) | HirExpr::Has(x, _) | HirExpr::Match(x, _) => self.collect_assigned_fields(x, out),
            HirExpr::Binary(_, l, r) => { self.collect_assigned_fields(l, out); self.collect_assigned_fields(r, out); },
            HirExpr::Call(c, args) | HirExpr::SafeCall(c, args) => {
                self.collect_assigned_fields(c, out);
                for a in args { self.collect_assigned_fields(a, out); }
            },
            HirExpr::Index(t, m, _) => { self.collect_assigned_fields(t, out); self.collect_assigned_fields(m, out); },
            HirExpr::Construct(c, args, brace) => {
                self.collect_assigned_fields(c, out);
                for a in args { self.collect_assigned_fields(a, out); }
                for (_, v) in brace { self.collect_assigned_fields(v, out); }
            },
            HirExpr::Coalesce(l, r) | HirExpr::Handle(l, _, r) | HirExpr::SafeAccess(l, r, _) => {
                self.collect_assigned_fields(l, out);
                self.collect_assigned_fields(r, out);
            },
            HirExpr::Assert(x) | HirExpr::Propagate(x) => self.collect_assigned_fields(x, out),
            _ => {},
        }
    }

    fn collect_assigned_fields_stmt(&self, stmt: &HirId<HirStmt>, out: &mut HashSet<Symbol>) {
        match self.hir.get(stmt) {
            HirStmt::Expression(e) | HirStmt::Throw(e) | HirStmt::Block(e) => self.collect_assigned_fields(e, out),
            HirStmt::Return(opt) => if let Some(e) = opt { self.collect_assigned_fields(e, out); },
            HirStmt::While(c, b) => { self.collect_assigned_fields(c, out); self.collect_assigned_fields(b, out); },
            HirStmt::If(c, then, otherwise) => {
                self.collect_assigned_fields(c, out);
                self.collect_assigned_fields(then, out);
                if let Some(s) = otherwise { self.collect_assigned_fields_stmt(s, out); }
            },
            HirStmt::Try(body, catch, finally) => {
                self.collect_assigned_fields(body, out);
                if let Some(c) = catch { self.collect_assigned_fields(&c.body, out); }
                if let Some(f) = finally { self.collect_assigned_fields(f, out); }
            },
            HirStmt::Say(field) => if let Some(v) = &field.value { self.collect_assigned_fields(v, out); },
            HirStmt::Match(scrutinee, arms) => {
                self.collect_assigned_fields(scrutinee, out);
                for arm in arms {
                    if let Some(guard) = &arm.guard { self.collect_assigned_fields(guard, out); }
                    self.collect_assigned_fields(&arm.body, out);
                }
            },
            // Nested functions/types do not establish init assignment in this body.
            HirStmt::Fn(_) | HirStmt::Type(_) | HirStmt::Trait(_) | HirStmt::Nop => {},
        }
    }

    /// Pushes the type frame that method bodies resolve against, deriving the private-name set
    /// (plain names of every trait's private members) from the declaration's per-trait map.
    fn push_type_frame(&mut self, layout: TypeLayout, decl: &HirTypeDecl) {
        let private_names = decl.trait_privates.values().flat_map(|m| m.keys().copied()).collect();
        self.type_frames.push(TypeFrame {
            layout,
            trait_privates: decl.trait_privates.clone(),
            private_names,
        });
    }

    pub(super) fn type_declaration(&mut self, stmt: &HirId<HirStmt>, decl: &HirTypeDecl) -> Result<(), anyhow::Error> {
        let slot = self.resolve_local(decl.name).expect("type declarations are reserved by hoisting");
        self.bindings.slots.insert(*stmt, slot);
        self.enter_scope();

        let layout = self.build_layout(decl);
        self.push_type_frame(layout.clone(), decl);

        // Method bodies resolve under the declaring trait's private scope (host members: none).
        // Save/restore around the whole type so a nested type declaration doesn't leak its scope.
        let outer_trait = self.current_trait.take();

        // A factory-less type has no factory body to resolve.
        if let HirStmt::Fn(init) = self.hir.get(&decl.init) {
            self.function(init, FnKind::Initializer)?;
        }

        for (stmt_id, trait_sym) in decl.methods.iter().zip(&decl.method_traits) {
            self.current_trait = *trait_sym;
            let method = self.fn_decl(stmt_id);
            self.function(method, FnKind::Method)?;
        }

        self.current_trait = outer_trait;
        self.type_frames.pop();
        self.exit_scope(stmt);

        self.types.insert(decl.name, layout.clone());
        self.bindings.types.insert(*stmt, layout);
        self.record_surface(decl.name, &decl.pub_members);
        Ok(())
    }

    /// Records a type/trait's public member names for the `x has T` surface form, in a stable
    /// order so codegen emits the membership checks deterministically.
    fn record_surface(&mut self, name: Symbol, members: &HashSet<Symbol>) {
        let mut members: Vec<Symbol> = members.iter().copied().collect();
        members.sort_by_key(|s| s.index());
        self.bindings.surfaces.insert(name, members);
    }

    /// Validates a standalone `trait`: resolves its method bodies against a layout built from its
    /// declared `surface`, so a `this.x` outside the surface is a self-containment error. If a
    /// trait fails self-containment validation, it will emit compile errors even if the trait is
    /// never used.
    pub(super) fn trait_declaration(&mut self, stmt: &HirId<HirStmt>, decl: &HirTypeDecl) -> Result<(), anyhow::Error> {
        self.enter_scope();

        // A validation layout: every surface name (and every renamed private slot) resolves to a throwaway id.
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
        self.record_surface(decl.name, &decl.surface);
        Ok(())
    }

    fn fn_decl(&self, stmt: &HirId<HirStmt>) -> &'a HirFnDecl {
        let HirStmt::Fn(decl) = self.hir.get(stmt) else {
            unreachable!("expected a function statement");
        };
        decl
    }
}

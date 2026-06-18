//! Binding and layout. A single lexical walk of the HIR that assigns each name its runtime location.
//! Produces a [`Bindings`] table that `codegen` consumes.
//!
//! Name-level questions (what is in scope, name collisions, trait references) are settled earlier in
//! `middle::names`; this pass assumes a well-formed namespace and only decides *where* things live.

use anyhow::anyhow;
use anyhow::bail;
use fnv::FnvHashMap;
use nohash_hasher::IntSet;

use crate::compiler_error;
use crate::core::objects::{TypeMember, UpvalueLocation};
use crate::middle::hir::{
    Hir, HirTypeDecl, HirExpr, HirFnDecl, HirId, HirLiteral, HirStmt, Symbol,
};

/// Where a bare identifier binds.
#[derive(Clone, Copy)]
pub enum Place {
    Local(u8),
    Upvalue(u8),
    /// An implicit-`this` type field, by member id.
    Field(u8),
    /// A global, by symbol (codegen interns its text into the constant pool).
    Global(Symbol),
}

/// How a `this`/`super` member access (`this.x`, `super.m`, `super(...)`) resolves.
#[derive(Clone, Copy)]
pub enum Member {
    /// A direct member slot (`GET`/`SET_PROPERTY_ID`).
    ById(u8),
    /// A getter/setter call: the accessor's member id.
    ByAccessor(u8),
    /// A `super(...)` call: the supertype initializer's member id.
    SuperInit(u8),
}

/// A local cleanup emitted when a scope exits, top of stack first.
#[derive(Clone, Copy)]
pub enum Cleanup {
    Pop,
    CloseUpvalue(u8),
}

#[derive(Clone, Copy)]
pub enum FnKind {
    Function,
    Method,
    Initializer,
}

/// The member layout of a type, for codegen to build its `ObjType`.
#[derive(Clone)]
pub struct TypeLayout {
    pub name: Symbol,
    /// Supertype name
    pub supertype: Option<Symbol>,
    /// Field and regular-method names to id. Includes inherited members.
    pub members: FnvHashMap<Symbol, TypeMember>,
    /// Field member ids. Includes inherited members.
    pub fields: Vec<u8>,
    /// Member ids that are **not** externally accessible (private or `inner`).
    /// Includes inherited non-public members. Consumed by codegen to omit these
    /// from the runtime name map.
    pub non_public: IntSet<u8>,
    pub member_count: u8,

    /// Member id of the getter function.
    pub getter_id: Option<u8>,
    /// Member id of the setter function.
    pub setter_id: Option<u8>,
    /// Member id of the initializer function.
    pub init_id: u8,
}

impl TypeLayout {
    /// A layout with no members yet, ready to be filled by a type/trait declaration.
    fn empty(name: Symbol, supertype: Option<Symbol>) -> TypeLayout {
        TypeLayout {
            name,
            supertype,
            members: FnvHashMap::default(),
            fields: Vec::new(),
            non_public: IntSet::default(),
            getter_id: None,
            setter_id: None,
            init_id: 0,
            member_count: 0,
        }
    }

    fn resolve(&self, name: Symbol) -> Option<TypeMember> {
        self.members.get(&name).copied()
    }

    fn resolve_id(&self, name: Symbol) -> Option<u8> {
        self.resolve(name).map(|m| match m {
            TypeMember::Field(id) | TypeMember::Method(id) => id,
        })
    }
}

/// The output of resolution: per-node binding decisions consumed by codegen.
#[derive(Default)]
pub struct Bindings {
    /// Identifier uses and assignment targets => their binding.
    places: FnvHashMap<HirId<HirExpr>, Place>,
    /// `this`/`super` member accesses => their resolution.
    members: FnvHashMap<HirId<HirExpr>, Member>,
    /// `say`/`fn`/`type` statements => the local slot they occupy.
    slots: FnvHashMap<HirId<HirStmt>, u8>,
    /// Function bodies => the captured upvalues of that function.
    upvalues: FnvHashMap<HirId<HirExpr>, Vec<UpvalueLocation>>,
    /// Type declarations => their member layout.
    types: FnvHashMap<HirId<HirStmt>, TypeLayout>,
    /// Scope nodes (by HIR node index) => locals to clean up on exit.
    cleanups: FnvHashMap<usize, Vec<Cleanup>>,
}

impl Bindings {
    pub fn place(&self, id: &HirId<HirExpr>) -> Place {
        self.places[id]
    }

    pub fn member(&self, id: &HirId<HirExpr>) -> Member {
        self.members[id]
    }

    pub fn slot(&self, id: &HirId<HirStmt>) -> u8 {
        self.slots[id]
    }

    pub fn upvalues(&self, body: &HirId<HirExpr>) -> &[UpvalueLocation] {
        &self.upvalues[body]
    }

    pub fn type_layout(&self, id: &HirId<HirStmt>) -> &TypeLayout {
        &self.types[id]
    }

    pub fn cleanup<T>(&self, scope: &HirId<T>) -> &[Cleanup] {
        self.cleanups.get(&scope.index()).map_or(&[], Vec::as_slice)
    }
}

struct Local {
    /// `None` for the callee/`this` slot of a method or initializer: it's
    /// addressed positionally (slot 0), never resolved by name.
    name: Option<Symbol>,
    depth: u8,
    is_mutable: bool,
    is_captured: bool,
}

struct FnFrame {
    upvalues: Vec<UpvalueLocation>,
    local_offset: u8,
    type_frame: Option<u8>,
    body: HirId<HirExpr>,
}

struct TypeFrame {
    layout: TypeLayout,
    supertype: Option<TypeLayout>,
    /// Per trait, its private members' plain name -> renamed slot name (from the HIR). The
    /// resolver scopes a trait body's accesses to its own trait's entry. See `lower::traits`.
    trait_privates: std::collections::HashMap<Symbol, std::collections::HashMap<Symbol, Symbol>>,
    /// The plain names of every trait private member (any trait) for diagnostics: an access
    /// that misses but names one of these is reported as private rather than missing.
    private_names: std::collections::HashSet<Symbol>,
}

pub struct Resolver<'a> {
    hir: &'a Hir,
    bindings: Bindings,
    locals: Vec<Local>,
    scope_depth: u8,
    fn_frames: Vec<FnFrame>,
    type_frames: Vec<TypeFrame>,
    types: FnvHashMap<Symbol, TypeLayout>,
    /// The trait whose method body is currently being resolved.
    current_trait: Option<Symbol>,
    /// `true` while validating a standalone `trait` against its declared surface.
    validating_trait: bool,
}

pub fn resolve(hir: &Hir) -> Result<Bindings, anyhow::Error> {
    let mut resolver = Resolver {
        hir,
        bindings: Bindings::default(),
        locals: Vec::new(),
        scope_depth: 0,
        fn_frames: Vec::new(),
        type_frames: Vec::new(),
        types: FnvHashMap::default(),
        current_trait: None,
        validating_trait: false,
    };

    let root = resolver.hir.get_root();
    resolver.statement(&root)?;
    Ok(resolver.bindings)
}

impl<'a> Resolver<'a> {
    fn error<T: 'static>(&self, msg: impl Into<String>, node_id: &HirId<T>) -> anyhow::Error {
        let pos = self.hir.pos(node_id);
        anyhow!("{}\n\tat {}", msg.into(), pos)
    }

    fn enter_scope(&mut self) {
        self.scope_depth += 1;
    }

    fn exit_scope<T: 'static>(&mut self, node_id: &HirId<T>) {
        self.scope_depth -= 1;
        let mut cleanups = Vec::new();
        while !self.locals.is_empty() && self.locals.last().unwrap().depth > self.scope_depth {
            if self.locals.last().unwrap().is_captured {
                cleanups.push(Cleanup::CloseUpvalue(self.locals.len() as u8 - 1));
            } else {
                cleanups.push(Cleanup::Pop);
            }
            self.locals.pop();
        }
        if !cleanups.is_empty() {
            self.bindings.cleanups.insert(node_id.index(), cleanups);
        }
    }

    fn declare_local(&mut self, name: Symbol, is_mutable: bool) -> Result<u8, anyhow::Error> {
        if self.locals.len() >= u8::MAX as usize {
            bail!("Too many variables in scope");
        }

        // Duplicate-name collisions across the whole namespace are caught earlier, in `middle::names`.
        self.locals.push(Local { name: Some(name), depth: self.scope_depth, is_mutable, is_captured: false });

        let local_offset = self.fn_frames.last().map_or(0, |frame| frame.local_offset);
        Ok((self.locals.len() - 1) as u8 - local_offset)
    }

    fn resolve_local(&self, name: Symbol) -> Option<u8> {
        let local_offset = self.fn_frames.last().map_or(0, |frame| frame.local_offset);
        self.resolve_local_in_range(name, local_offset, self.locals.len() as u8)
    }

    fn resolve_local_in_range(&self, name: Symbol, start: u8, end: u8) -> Option<u8> {
        for i in (start..end).rev() {
            if self.locals[i as usize].name == Some(name) {
                return Some(i - start);
            }
        }
        None
    }

    fn resolve_upvalue(&mut self, name: Symbol) -> Result<Option<u8>, anyhow::Error> {
        if self.fn_frames.is_empty() {
            return Ok(None);
        }
        let max_type_frame = self.resolve_member_type(name);
        self.resolve_frame_upvalue(name, self.fn_frames.len() - 1, max_type_frame)
    }

    fn resolve_frame_upvalue(&mut self, name: Symbol, frame_idx: usize, max_type_frame: Option<u8>) -> Result<Option<u8>, anyhow::Error> {
        let type_frame = self.fn_frames[frame_idx].type_frame;

        // A member-resolvable name must not capture past the type frame that owns it: stop if this
        // frame is outside that type (no type frame, or one nested shallower than the owner).
        if let Some(max) = max_type_frame {
            if type_frame.map_or(true, |cf| cf < max) {
                return Ok(None);
            }
        }

        let range_start = if frame_idx == 0 { 0 } else { self.fn_frames[frame_idx - 1].local_offset };
        let range_end = self.fn_frames[frame_idx].local_offset;

        if let Some(idx) = self.resolve_local_in_range(name, range_start, range_end) {
            self.locals[(range_start + idx) as usize].is_captured = true;
            return Ok(Some(self.add_upvalue(idx, true, frame_idx)?));
        }

        if frame_idx == 0 {
            return Ok(None);
        }

        if let Some(idx) = self.resolve_frame_upvalue(name, frame_idx - 1, max_type_frame)? {
            return Ok(Some(self.add_upvalue(idx, false, frame_idx)?));
        }

        Ok(None)
    }

    fn add_upvalue(&mut self, location: u8, is_local: bool, frame_idx: usize) -> Result<u8, anyhow::Error> {
        let upvalues = &mut self.fn_frames[frame_idx].upvalues;
        if let Some(i) = upvalues.iter().position(|u| u.location == location && u.is_local == is_local) {
            return Ok(i as u8);
        }
        if upvalues.len() >= u8::MAX as usize {
            bail!("Too many upvalues");
        }
        upvalues.push(UpvalueLocation { location, is_local });
        Ok((upvalues.len() - 1) as u8)
    }

    fn resolve_member_type(&self, name: Symbol) -> Option<u8> {
        for i in (0..self.type_frames.len()).rev() {
            if self.type_frames[i].layout.resolve(name).is_some() {
                return Some(i as u8);
            }
        }
        None
    }

    /// The per-trait renamed slot for `name` if it's a private member of the trait whose body is
    /// currently being resolved. Handles implicit-`this` private lookup.
    fn private_member(&self, name: Symbol) -> Option<Symbol> {
        let trait_sym = self.current_trait?;
        let frame = self.type_frames.last()?;
        frame.trait_privates.get(&trait_sym).and_then(|m| m.get(&name)).copied()
    }

    /// The member id `name` resolves to as an implicit-`this` field of the enclosing type: a trait
    /// body's own private member resolves to its per-trait slot, otherwise the plain name.
    fn this_field_id(&self, name: Symbol) -> Option<u8> {
        let layout = &self.type_frames.last()?.layout;
        layout.resolve_id(self.private_member(name).unwrap_or(name))
    }

    /// Reports a member that exists only inside some trait as private rather than missing. An
    /// implicit-`this` bare name and an explicit `this.x` resolve alike, so both route here.
    fn deny_private<T: 'static>(&self, name: Symbol, node: &HirId<T>) -> Result<(), anyhow::Error> {
        if self.type_frames.last().is_some_and(|f| f.private_names.contains(&name)) {
            compiler_error!(self, node, "Member '{}' is private", self.hir.text(name));
        }
        Ok(())
    }

    fn resolve_place(&mut self, name: Symbol, node: &HirId<HirExpr>) -> Result<Place, anyhow::Error> {
        let place = if let Some(slot) = self.resolve_local(name) {
            Place::Local(slot)
        } else if let Some(idx) = self.resolve_upvalue(name)? {
            Place::Upvalue(idx)
        } else if let Some(id) = self.this_field_id(name) {
            Place::Field(id)
        } else {
            self.deny_private(name, node)?;
            Place::Global(name)
        };
        Ok(place)
    }

    fn statement(&mut self, stmt_id: &HirId<HirStmt>) -> Result<(), anyhow::Error> {
        match self.hir.get(stmt_id) {
            HirStmt::Return(expr) => {
                if let Some(expr) = expr {
                    self.expression(expr)?;
                }
            },
            HirStmt::Throw(expr) => self.expression(expr)?,
            HirStmt::Try(try_body, catch, finally) => {
                self.expression(try_body)?;
                if let Some(catch) = catch {
                    self.enter_scope();
                    if let Some(param) = &catch.param {
                        let HirExpr::Identifier(name) = self.hir.get(param) else { unreachable!() };
                        self.declare_local(*name, false)?;
                    }
                    // catch body is a HirExpr::Block compiled inline (no extra scope).
                    let HirExpr::Block(stmts) = self.hir.get(&catch.body) else { unreachable!() };
                    self.statement_body(stmts)?;
                    self.exit_scope(&catch.body);
                }
                if let Some(finally) = finally {
                    self.expression(finally)?;
                }
            },
            HirStmt::Fn(decl) => {
                let name = decl.name;
                let slot = self.resolve_local(name)
                    .expect("fn declarations are reserved by hoisting");
                self.bindings.slots.insert(*stmt_id, slot);
                self.function(decl, FnKind::Function)?;
            },
            HirStmt::Type(decl) => self.type_declaration(stmt_id, decl)?,
            HirStmt::Trait(decl) => self.trait_declaration(stmt_id, decl)?,
            HirStmt::Say(field) => {
                let slot = self.declare_local(field.name, true)?;
                self.bindings.slots.insert(*stmt_id, slot);
                if let Some(expr) = &field.value {
                    self.expression(expr)?;
                }
            },
            HirStmt::Expression(expr) => self.expression(expr)?,
            HirStmt::While(cond, body) => {
                self.expression(cond)?;
                self.expression(body)?;
            },
            HirStmt::If(cond, then, otherwise) => {
                self.expression(cond)?;
                self.expression(then)?;
                if let Some(otherwise) = otherwise {
                    self.statement(otherwise)?;
                }
            },
            HirStmt::Block(body) => self.expression(body)?,
        };
        Ok(())
    }

    fn statement_body(&mut self, body: &[HirId<HirStmt>]) -> Result<(), anyhow::Error> {
        self.hoist_declarations(body)?;
        for stmt_id in body {
            self.statement(stmt_id)?;
        }
        Ok(())
    }

    fn scoped_body<T: 'static>(&mut self, body: &[HirId<HirStmt>], node_id: &HirId<T>) -> Result<(), anyhow::Error> {
        self.enter_scope();
        self.statement_body(body)?;
        self.exit_scope(node_id);
        Ok(())
    }

    fn hoist_declarations(&mut self, body: &[HirId<HirStmt>]) -> Result<(), anyhow::Error> {
        for stmt_id in body {
            let name = match self.hir.get(stmt_id) {
                HirStmt::Fn(decl) => decl.name,
                HirStmt::Type(decl) => decl.name,
                _ => continue,
            };
            self.declare_local(name, false)?;
        }
        Ok(())
    }

    fn expression(&mut self, expr: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        match self.hir.get(expr) {
            HirExpr::Block(stmts) => self.scoped_body(stmts, expr)?,
            HirExpr::Unary(_, operand) => self.expression(operand)?,
            HirExpr::Binary(_, left, right) => {
                self.expression(left)?;
                self.expression(right)?;
            },
            HirExpr::Assign(left, right) => self.assign(left, right)?,
            HirExpr::Call(callee, args) => self.call_expression(callee, args)?,
            HirExpr::Index(target, member, _) => self.index(target, member)?,
            HirExpr::Literal(lit) => self.literal(lit)?,
            HirExpr::Identifier(name) => {
                let place = self.resolve_place(*name, expr)?;
                self.bindings.places.insert(*expr, place);
            },
            // `x is T`: bind the receiver; `T` is a static name resolved at codegen.
            HirExpr::Is(target, _) => self.expression(target)?,
            HirExpr::This => self.require_type(expr)?,
            HirExpr::Super => { self.require_supertype(expr)?; },
        };
        Ok(())
    }

    fn assign(&mut self, lhs: &HirId<HirExpr>, rhs: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        match self.hir.get(lhs) {
            HirExpr::Identifier(name) => {
                let name = *name;
                if let Some(local) = self.locals.iter().rev().find(|local| local.name == Some(name)) {
                    if !local.is_mutable {
                        compiler_error!(self, lhs, "Invalid assignment: '{}' is immutable", self.hir.text(name));
                    }
                }
                let place = self.resolve_place(name, lhs)?;
                if let Place::Global(_) = place {
                    compiler_error!(self, lhs, "Cannot assign to undefined variable '{}'", self.hir.text(name));
                }
                self.bindings.places.insert(*lhs, place);
                self.expression(rhs)?;
                Ok(())
            },
            HirExpr::Index(obj, member, _) => {
                let (obj, member) = (*obj, *member);
                if matches!(self.hir.get(&obj), HirExpr::This | HirExpr::Super) {
                    self.this_member_access(&obj, &member, true)?;
                    self.expression(rhs)?;
                } else {
                    self.expression(rhs)?;
                    self.expression(&obj)?;
                    self.expression(&member)?;
                }
                Ok(())
            },
            _ => compiler_error!(self, lhs, "Invalid assignment"),
        }
    }

    /// Resolves an index load (`a[b]`, `a.b`).
    fn index(&mut self, target: &HirId<HirExpr>, member: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        if matches!(self.hir.get(target), HirExpr::This | HirExpr::Super) {
            return self.this_member_access(target, member, false);
        }

        self.expression(target)?;
        self.expression(member)?;
        Ok(())
    }

    /// Resolves a `this`/`super` member access, plus the member expression that becomes
    /// the getter/setter call's argument.
    fn this_member_access(&mut self, target: &HirId<HirExpr>, member: &HirId<HirExpr>, is_store: bool) -> Result<(), anyhow::Error> {
        self.type_member(target, member, is_store)?;
        if let Member::ByAccessor(_) = self.bindings.members[target] {
            self.expression(member)?;
        }
        Ok(())
    }

    /// Resolves a `this`/`super` member access.
    fn type_member(&mut self, target: &HirId<HirExpr>, member: &HirId<HirExpr>, is_store: bool) -> Result<(), anyhow::Error> {
        let is_super = matches!(self.hir.get(target), HirExpr::Super);
        let member_name = match self.hir.get(member) {
            HirExpr::Literal(HirLiteral::String(name)) => self.hir.symbol_of(name),
            _ => None,
        };

        let target_type = if is_super {
            self.require_supertype(target)?
        } else {
            self.require_type(target)?;
            self.current_type().clone()
        };

        // Inside a trait body, `this.x` for the trait's own private member resolves to its
        // per-trait slot, otherwise the plain name.
        let lookup = member_name.map(|name| match is_super {
            false => self.private_member(name).unwrap_or(name),
            true => name,
        });
        if let Some(member_id) = lookup.and_then(|name| target_type.resolve_id(name)) {
            self.bindings.members.insert(*target, Member::ById(member_id));
            return Ok(());
        }

        let accessor = if is_store { target_type.setter_id } else { target_type.getter_id };

        if let Some(accessor_id) = accessor {
            self.bindings.members.insert(*target, Member::ByAccessor(accessor_id));
            return Ok(());
        }

        // While validating a standalone trait, an unresolved member is a self-containment
        // violation: the trait reached something it does not declare.
        if self.validating_trait {
            if let HirExpr::Literal(HirLiteral::String(name)) = self.hir.get(member) {
                compiler_error!(self, target, "Trait '{}' accesses 'this.{}', which it does not declare (provide it via `with`, `req`, or `req fn`)",
                    self.hir.text(target_type.name), name);
            }
        }

        // A name that resolved to nothing but is some trait's private member is reported as
        // private (it exists, but only inside its declaring trait), not as missing.
        if let Some(name) = member_name {
            if !is_super { self.deny_private(name, target)?; }
        }

        let type_name = self.hir.text(target_type.name);
        let accessor_name = if is_store { "setter" } else { "getter" };
        if let Some(member_name) = member_name {
            let member_name = self.hir.text(member_name);
            compiler_error!(self, target, "Invalid index: {type_name} doesn't have member {member_name} and doesn't have a {accessor_name}")
        } else {
            compiler_error!(self, target, "Invalid index: {type_name} doesn't have a {accessor_name}")
        }
    }

    fn call_expression(&mut self, callee: &HirId<HirExpr>, args: &[HirId<HirExpr>]) -> Result<(), anyhow::Error> {
        if matches!(self.hir.get(callee), HirExpr::Super) {
            let supertype = self.require_supertype(callee)?;
            self.bindings.members.insert(*callee, Member::SuperInit(supertype.init_id));
        } else {
            self.expression(callee)?;
        }
        for arg in args {
            self.expression(arg)?;
        }
        Ok(())
    }

    fn literal(&mut self, literal: &HirLiteral) -> Result<(), anyhow::Error> {
        match literal {
            HirLiteral::Array(elements) => {
                for element in elements {
                    self.expression(element)?;
                }
            },
            HirLiteral::Dict(pairs) => {
                for (key, value) in pairs {
                    self.expression(key)?;
                    self.expression(value)?;
                }
            },
            HirLiteral::Lambda(decl) => self.lambda(decl)?,
            _ => {},
        }
        Ok(())
    }

    fn lambda(&mut self, decl: &HirFnDecl) -> Result<(), anyhow::Error> {
        self.function(decl, FnKind::Function)
    }

    fn require_type(&self, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        if self.type_frames.is_empty() {
            compiler_error!(self, node, "Cannot use 'this' outside of a type method");
        }
        Ok(())
    }

    fn require_supertype(&self, node: &HirId<HirExpr>) -> Result<TypeLayout, anyhow::Error> {
        let Some(frame) = self.type_frames.last() else {
            compiler_error!(self, node, "Cannot use 'super' outside of a type method");
        };
        let Some(supertype) = &frame.supertype else {
            compiler_error!(self, node, "Cannot use 'super' outside of a child type method");
        };
        Ok(supertype.clone())
    }

    fn current_type(&self) -> &TypeLayout {
        &self.type_frames.last().unwrap().layout
    }

    fn function(&mut self, decl: &HirFnDecl, kind: FnKind) -> Result<(), anyhow::Error> {
        // A function's callee slot is named for recursion; a method/initializer's
        // slot 0 is `this`, addressed positionally and never resolved by name.
        let self_name = match kind {
            FnKind::Function => Some(decl.name),
            _ => None,
        };

        self.scope_depth += 1;
        let local_offset = self.locals.len() as u8;
        self.locals.push(Local { name: self_name, depth: self.scope_depth, is_mutable: false, is_captured: false });
        self.fn_frames.push(FnFrame {
            upvalues: Vec::new(),
            local_offset,
            type_frame: self.type_frames.last().map(|_| self.type_frames.len() as u8 - 1),
            body: decl.body,
        });

        for param in &decl.params {
            let HirExpr::Identifier(param_name) = self.hir.get(param) else {
                unreachable!("parser guarantees parameters are identifiers");
            };
            self.declare_local(*param_name, true)?;
        }

        self.expression(&decl.body)?;

        let frame = self.fn_frames.pop().unwrap();
        self.scope_depth -= 1;
        // The frame's callee slot and params (the body's own locals are popped by its block scope).
        self.locals.truncate(frame.local_offset as usize);

        self.bindings.upvalues.insert(frame.body, frame.upvalues);
        Ok(())
    }

    /// Builds a type's runtime [`TypeLayout`], assigning each member its id: inherited members
    /// first (ids preserved), then own fields, methods, accessors, and the initializer.
    fn build_layout(&self, decl: &HirTypeDecl, supertype: Option<&TypeLayout>) -> TypeLayout {
        let mut layout = TypeLayout::empty(decl.name, decl.supertype);

        let mut next_member_id: u8 = 0;
        if let Some(supertype) = supertype {
            layout.members = supertype.members.clone();
            layout.fields = supertype.fields.clone();
            layout.non_public = supertype.non_public.clone();
            // Accessors are inherited unless overridden below; the initializer is not.
            layout.getter_id = supertype.getter_id;
            layout.setter_id = supertype.setter_id;
            next_member_id = supertype.member_count;
        }

        for field in &decl.fields {
            layout.members.insert(*field, TypeMember::Field(next_member_id));
            layout.fields.push(next_member_id);
            if !decl.pub_members.contains(field) {
                layout.non_public.insert(next_member_id);
            }
            next_member_id += 1;
        }
        for stmt_id in &decl.methods {
            let method = self.fn_decl(stmt_id);
            layout.members.insert(method.name, TypeMember::Method(next_member_id));
            if !decl.pub_members.contains(&method.name) {
                layout.non_public.insert(next_member_id);
            }
            next_member_id += 1;
        }
        if decl.getter.is_some() {
            layout.getter_id = Some(next_member_id);
            next_member_id += 1;
        }
        if decl.setter.is_some() {
            layout.setter_id = Some(next_member_id);
            next_member_id += 1;
        }
        // Every type has its own initializer (declared or virtual).
        layout.init_id = next_member_id;
        next_member_id += 1;
        layout.member_count = next_member_id;
        layout
    }

    /// Pushes the type frame that method bodies resolve against, deriving the private-name set
    /// (plain names of every trait's private members) from the declaration's per-trait map.
    fn push_type_frame(&mut self, layout: TypeLayout, supertype: Option<TypeLayout>, decl: &HirTypeDecl) {
        let private_names = decl.trait_privates.values().flat_map(|m| m.keys().copied()).collect();
        self.type_frames.push(TypeFrame {
            layout,
            supertype,
            trait_privates: decl.trait_privates.clone(),
            private_names,
        });
    }

    fn type_declaration(&mut self, stmt: &HirId<HirStmt>, decl: &HirTypeDecl) -> Result<(), anyhow::Error> {
        let slot = self.resolve_local(decl.name).expect("type declarations are reserved by hoisting");
        self.bindings.slots.insert(*stmt, slot);
        self.enter_scope();

        let supertype = match &decl.supertype {
            Some(super_name) => Some(self.types.get(super_name)
                .ok_or(anyhow!("Type '{}' not declared", self.hir.text(*super_name)))?
                .clone()),
            None => None,
        };

        let layout = self.build_layout(decl, supertype.as_ref());
        self.push_type_frame(layout.clone(), supertype, decl);

        // Method bodies resolve under the declaring trait's private scope (host members: none).
        // Save/restore around the whole type so a nested type declaration doesn't leak its scope.
        let outer_trait = self.current_trait.take();

        if let Some(stmt_id) = &decl.getter {
            let getter = self.fn_decl(stmt_id);
            self.function(getter, FnKind::Method)?;
        }
        if let Some(stmt_id) = &decl.setter {
            let setter = self.fn_decl(stmt_id);
            self.function(setter, FnKind::Method)?;
        }

        let init = self.fn_decl(&decl.init);
        self.function(init, FnKind::Initializer)?;

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
        Ok(())
    }

    /// Validates a standalone `trait`: resolves its method bodies against a layout built from its
    /// declared `surface`, so a `this.x` outside the surface is a self-containment error. If a
    /// trait fails self-containment validation, it will emit compile errors even if the trait is
    /// never used.
    fn trait_declaration(&mut self, stmt: &HirId<HirStmt>, decl: &HirTypeDecl) -> Result<(), anyhow::Error> {
        self.enter_scope();

        // A validation layout: every surface name (and every renamed private slot) resolves to a throwaway id.
        let mut surface = TypeLayout::empty(decl.name, None);
        let mut id: u8 = 0;
        for name in &decl.surface {
            surface.members.entry(*name).or_insert(TypeMember::Method(id));
            id = id.wrapping_add(1);
        }
        for renamed in decl.trait_privates.values().flat_map(|m| m.values()) {
            surface.members.entry(*renamed).or_insert(TypeMember::Method(id));
            id = id.wrapping_add(1);
        }
        surface.member_count = id;

        self.push_type_frame(surface, None, decl);

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
        Ok(())
    }

    fn fn_decl(&self, stmt: &HirId<HirStmt>) -> &'a HirFnDecl {
        let HirStmt::Fn(decl) = self.hir.get(stmt) else {
            unreachable!("expected a function statement");
        };
        decl
    }
}

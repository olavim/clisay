//! Name resolution. A single lexical walk of the HIR that decides what every
//! identifier binds to. Produces a [`Bindings`] table that `codegen` consumes.

use anyhow::anyhow;
use anyhow::bail;
use fnv::FnvHashMap;

use crate::compiler_error;
use crate::core::objects::{ClassMember, UpvalueLocation};
use crate::middle::hir::{
    Hir, HirTypeDecl, HirExpr, HirFnDecl, HirId, HirLiteral, HirStmt, Symbol,
};

/// Where a bare identifier binds.
#[derive(Clone, Copy)]
pub enum Place {
    Local(u8),
    Upvalue(u8),
    /// An implicit-`this` class field, by member id.
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

/// The member layout of a class, for codegen to build its `ObjType`.
#[derive(Clone)]
pub struct TypeLayout {
    pub name: Symbol,
    /// Superclass name
    pub superclass: Option<Symbol>,
    /// Field and regular-method names to id. Includes inherited members.
    pub members: FnvHashMap<Symbol, ClassMember>,
    /// Field member ids. Includes inherited members.
    pub fields: Vec<u8>,
    pub member_count: u8,

    /// Member id of the getter function.
    pub getter_id: Option<u8>,
    /// Member id of the setter function.
    pub setter_id: Option<u8>,
    /// Member id of the initializer function.
    pub init_id: u8,
}

impl TypeLayout {
    fn resolve(&self, name: Symbol) -> Option<ClassMember> {
        self.members.get(&name).copied()
    }

    fn resolve_id(&self, name: Symbol) -> Option<u8> {
        self.resolve(name).map(|m| match m {
            ClassMember::Field(id) | ClassMember::Method(id) => id,
        })
    }
}

/// The output of resolution: per-node binding decisions consumed by codegen.
pub struct Bindings {
    /// Identifier uses and assignment targets => their binding.
    places: FnvHashMap<HirId<HirExpr>, Place>,
    /// `this`/`super` member accesses => their resolution.
    members: FnvHashMap<HirId<HirExpr>, Member>,
    /// `say`/`fn`/`class` statements => the local slot they occupy.
    slots: FnvHashMap<HirId<HirStmt>, u8>,
    /// Function bodies => the captured upvalues of that function.
    upvalues: FnvHashMap<HirId<HirExpr>, Vec<UpvalueLocation>>,
    /// Class declarations => their member layout.
    classes: FnvHashMap<HirId<HirStmt>, TypeLayout>,
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

    pub fn class_layout(&self, id: &HirId<HirStmt>) -> &TypeLayout {
        &self.classes[id]
    }

    pub fn cleanup<T>(&self, scope: &HirId<T>) -> &[Cleanup] {
        self.cleanups.get(&scope.index()).map_or(&[], Vec::as_slice)
    }
}

struct Local {
    /// `None` for the callee/`this` slot of a method or initializer — it's
    /// addressed positionally (slot 0), never resolved by name.
    name: Option<Symbol>,
    depth: u8,
    is_mutable: bool,
    is_captured: bool,
}

struct FnFrame {
    upvalues: Vec<UpvalueLocation>,
    local_offset: u8,
    class_frame: Option<u8>,
    body: HirId<HirExpr>,
}

struct ClassFrame {
    layout: TypeLayout,
    superclass: Option<TypeLayout>,
}

pub struct Resolver<'a> {
    hir: &'a Hir,
    bindings: Bindings,
    locals: Vec<Local>,
    scope_depth: u8,
    fn_frames: Vec<FnFrame>,
    class_frames: Vec<ClassFrame>,
    classes: FnvHashMap<Symbol, TypeLayout>,
}

pub fn resolve(hir: &Hir) -> Result<Bindings, anyhow::Error> {
    let mut resolver = Resolver {
        hir,
        bindings: Bindings {
            places: FnvHashMap::default(),
            members: FnvHashMap::default(),
            slots: FnvHashMap::default(),
            upvalues: FnvHashMap::default(),
            classes: FnvHashMap::default(),
            cleanups: FnvHashMap::default(),
        },
        locals: Vec::new(),
        scope_depth: 0,
        fn_frames: Vec::new(),
        class_frames: Vec::new(),
        classes: FnvHashMap::default(),
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

    fn declare_local<T: 'static>(&mut self, name: Symbol, is_mutable: bool, node_id: &HirId<T>) -> Result<u8, anyhow::Error> {
        if self.locals.len() >= u8::MAX as usize {
            bail!("Too many variables in scope");
        }

        if self.locals.iter().rev().any(|local| local.depth == self.scope_depth && local.name == Some(name)) {
            compiler_error!(self, node_id, "Variable '{}' already declared in this scope", self.hir.text(name));
        }

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
        let max_class_frame = self.resolve_member_class(name);
        self.resolve_frame_upvalue(name, self.fn_frames.len() - 1, max_class_frame)
    }

    fn resolve_frame_upvalue(&mut self, name: Symbol, frame_idx: usize, max_class_frame: Option<u8>) -> Result<Option<u8>, anyhow::Error> {
        let class_frame = self.fn_frames[frame_idx].class_frame;

        if max_class_frame.is_some() && class_frame.is_some() && class_frame.unwrap() < max_class_frame.unwrap() {
            return Ok(None);
        }
        if max_class_frame.is_some() && class_frame.is_none() {
            return Ok(None);
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

        if let Some(idx) = self.resolve_frame_upvalue(name, frame_idx - 1, max_class_frame)? {
            return Ok(Some(self.add_upvalue(idx, false, frame_idx)?));
        }

        Ok(None)
    }

    fn add_upvalue(&mut self, location: u8, is_local: bool, frame_idx: usize) -> Result<u8, anyhow::Error> {
        for i in 0..self.fn_frames[frame_idx].upvalues.len() {
            let upvalue = &self.fn_frames[frame_idx].upvalues[i];
            if upvalue.location == location && upvalue.is_local == is_local {
                return Ok(i as u8);
            }
        }
        if self.fn_frames[frame_idx].upvalues.len() >= u8::MAX as usize {
            bail!("Too many upvalues");
        }
        self.fn_frames[frame_idx].upvalues.push(UpvalueLocation { location, is_local });
        Ok((self.fn_frames[frame_idx].upvalues.len() - 1) as u8)
    }

    fn resolve_member_class(&self, name: Symbol) -> Option<u8> {
        for i in (0..self.class_frames.len()).rev() {
            if self.class_frames[i].layout.resolve(name).is_some() {
                return Some(i as u8);
            }
        }
        None
    }

    fn resolve_place(&mut self, name: Symbol) -> Result<Place, anyhow::Error> {
        let place = if let Some(slot) = self.resolve_local(name) {
            Place::Local(slot)
        } else if let Some(idx) = self.resolve_upvalue(name)? {
            Place::Upvalue(idx)
        } else if let Some(id) = self.class_frames.last().and_then(|f| f.layout.resolve_id(name)) {
            Place::Field(id)
        } else {
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
                        self.declare_local(*name, false, param)?;
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
            HirStmt::Type(decl) => self.class_declaration(stmt_id, decl)?,
            HirStmt::Say(field) => {
                let slot = self.declare_local(field.name, true, stmt_id)?;
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

    fn statement_body(&mut self, body: &Vec<HirId<HirStmt>>) -> Result<(), anyhow::Error> {
        self.hoist_declarations(body)?;
        for stmt_id in body {
            self.statement(stmt_id)?;
        }
        Ok(())
    }

    fn scoped_body<T: 'static>(&mut self, body: &Vec<HirId<HirStmt>>, node_id: &HirId<T>) -> Result<(), anyhow::Error> {
        self.enter_scope();
        self.statement_body(body)?;
        self.exit_scope(node_id);
        Ok(())
    }

    fn hoist_declarations(&mut self, body: &Vec<HirId<HirStmt>>) -> Result<(), anyhow::Error> {
        for stmt_id in body {
            let name = match self.hir.get(stmt_id) {
                HirStmt::Fn(decl) => decl.name,
                HirStmt::Type(decl) => decl.name,
                _ => continue,
            };
            self.declare_local(name, false, stmt_id)?;
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
                let place = self.resolve_place(*name)?;
                self.bindings.places.insert(*expr, place);
            },
            HirExpr::This => self.require_class(expr)?,
            HirExpr::Super => { self.require_superclass(expr)?; },
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
                let place = self.resolve_place(name)?;
                // Script code can't create or reassign globals (only native fns live
                // there); an assignment target that resolves to a global is undefined.
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
                    self.class_member(&obj, &member, true)?;
                    // An accessor store evaluates the member expression as a call arg.
                    if let Member::ByAccessor(_) = self.bindings.members[&obj] {
                        self.expression(&member)?;
                    }
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
            self.class_member(target, member, false)?;
            // An accessor load evaluates the member expression as a call arg.
            if let Member::ByAccessor(_) = self.bindings.members[target] {
                self.expression(member)?;
            }
            return Ok(());
        }

        self.expression(target)?;
        self.expression(member)?;
        Ok(())
    }

    /// Resolves a `this`/`super` member access.
    fn class_member(&mut self, target: &HirId<HirExpr>, member: &HirId<HirExpr>, is_store: bool) -> Result<(), anyhow::Error> {
        let is_super = matches!(self.hir.get(target), HirExpr::Super);
        let member_name = match self.hir.get(member) {
            HirExpr::Literal(HirLiteral::String(name)) => self.hir.symbol_of(name),
            _ => None,
        };

        let class = if is_super {
            self.require_superclass(target)?
        } else {
            self.require_class(target)?;
            self.current_class().clone()
        };

        if let Some(member_id) = member_name.and_then(|name| class.resolve_id(name)) {
            self.bindings.members.insert(*target, Member::ById(member_id));
            return Ok(());
        }

        let accessor = if is_store { class.setter_id } else { class.getter_id };

        if let Some(accessor_id) = accessor {
            self.bindings.members.insert(*target, Member::ByAccessor(accessor_id));
            return Ok(());
        }

        let class_name = self.hir.text(class.name);
        let accessor_name = if is_store { "setter" } else { "getter" };
        if let Some(member_name) = member_name {
            let member_name = self.hir.text(member_name);
            compiler_error!(self, target, "Invalid index: {class_name} doesn't have member {member_name} and doesn't have a {accessor_name}")
        } else {
            compiler_error!(self, target, "Invalid index: {class_name} doesn't have a {accessor_name}")
        }
    }

    fn call_expression(&mut self, callee: &HirId<HirExpr>, args: &Vec<HirId<HirExpr>>) -> Result<(), anyhow::Error> {
        if matches!(self.hir.get(callee), HirExpr::Super) {
            let superclass = self.require_superclass(callee)?;
            self.bindings.members.insert(*callee, Member::SuperInit(superclass.init_id));
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

    fn require_class(&self, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        if self.class_frames.is_empty() {
            compiler_error!(self, node, "Cannot use 'this' outside of a type method");
        }
        Ok(())
    }

    fn require_superclass(&self, node: &HirId<HirExpr>) -> Result<TypeLayout, anyhow::Error> {
        let Some(frame) = self.class_frames.last() else {
            compiler_error!(self, node, "Cannot use 'super' outside of a type method");
        };
        let Some(superclass) = &frame.superclass else {
            compiler_error!(self, node, "Cannot use 'super' outside of a child type method");
        };
        Ok(superclass.clone())
    }

    fn current_class(&self) -> &TypeLayout {
        &self.class_frames.last().unwrap().layout
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
            class_frame: self.class_frames.last().map(|_| self.class_frames.len() as u8 - 1),
            body: decl.body,
        });

        for param in &decl.params {
            let HirExpr::Identifier(param_name) = self.hir.get(param) else {
                unreachable!("parser guarantees parameters are identifiers");
            };
            self.declare_local(*param_name, true, param)?;
        }

        self.expression(&decl.body)?;

        let frame = self.fn_frames.pop().unwrap();
        self.scope_depth -= 1;
        while !self.locals.is_empty() && self.locals.last().unwrap().depth > self.scope_depth {
            self.locals.pop();
        }

        self.bindings.upvalues.insert(frame.body, frame.upvalues);
        Ok(())
    }

    fn class_declaration(&mut self, stmt: &HirId<HirStmt>, decl: &Box<HirTypeDecl>) -> Result<(), anyhow::Error> {
        let name = decl.name;
        let slot = self.resolve_local(name).expect("class declarations are reserved by hoisting");
        self.bindings.slots.insert(*stmt, slot);
        self.enter_scope();

        let superclass = match &decl.superclass {
            Some(super_name) => {
                let layout = self.classes.get(super_name)
                    .ok_or(anyhow!("Type '{}' not declared", self.hir.text(*super_name)))?;
                Some(layout.clone())
            },
            None => None,
        };

        let mut layout = TypeLayout {
            name: decl.name,
            superclass: decl.superclass,
            members: FnvHashMap::default(),
            fields: Vec::new(),
            getter_id: None,
            setter_id: None,
            init_id: 0,
            member_count: 0,
        };

        let mut next_member_id: u8 = 0;
        if let Some(superclass) = &superclass {
            layout.members = superclass.members.clone();
            layout.fields = superclass.fields.clone();
            // Accessors are inherited unless overridden below; the initializer is not.
            layout.getter_id = superclass.getter_id;
            layout.setter_id = superclass.setter_id;
            next_member_id = superclass.member_count;
        }

        for field in &decl.fields {
            layout.members.insert(*field, ClassMember::Field(next_member_id));
            layout.fields.push(next_member_id);
            next_member_id += 1;
        }
        for stmt_id in &decl.methods {
            let method = self.fn_decl(stmt_id);
            layout.members.insert(method.name, ClassMember::Method(next_member_id));
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
        // Every class has its own initializer (declared or virtual).
        layout.init_id = next_member_id;
        next_member_id += 1;
        layout.member_count = next_member_id;

        self.class_frames.push(ClassFrame { layout: layout.clone(), superclass });

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

        for stmt_id in &decl.methods {
            let method = self.fn_decl(stmt_id);
            self.function(method, FnKind::Method)?;
        }

        self.class_frames.pop();
        self.exit_scope(stmt);

        self.classes.insert(decl.name, layout.clone());
        self.bindings.classes.insert(*stmt, layout);
        Ok(())
    }

    fn fn_decl(&self, stmt: &HirId<HirStmt>) -> &'a HirFnDecl {
        let HirStmt::Fn(decl) = self.hir.get(stmt) else {
            unreachable!("expected a function statement");
        };
        decl
    }
}

//! Binding and layout. A single lexical walk of the HIR that assigns each name its runtime location.

mod scope;
mod types;

use std::collections::HashMap;
use std::collections::HashSet;

use anyhow::anyhow;

use crate::frontend::lex::Diagnostic;
use fnv::FnvHashMap;
use nohash_hasher::IntSet;

use crate::compiler_error;
use crate::core::objects::{TypeMember, UpvalueLocation};
use crate::middle::hir::{
    BinOp, Hir, HirExpr, HirFnDecl, HirId, HirLiteral, HirStmt, Symbol,
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

#[derive(Clone)]
pub struct TypeLayout {
    pub name: Symbol,
    /// Field and regular-method names to id.
    pub members: FnvHashMap<Symbol, TypeMember>,
    /// Field member ids.
    pub fields: Vec<u8>,
    /// Member ids that are not externally accessible (private or `inner`).
    /// Consumed by codegen to omit these from the runtime name map.
    pub non_public: IntSet<u8>,
    /// Nullable fields and nullable-returning methods.
    pub nullable: IntSet<u8>,
    pub mutable: IntSet<u8>,
    pub inner: IntSet<u8>,
    pub member_count: u8,
    /// Member id of the initializer function.
    pub init_id: u8,
    /// The initializer's arity.
    pub init_arity: u8,
    /// Field names the initializer assigns.
    pub init_assigned: HashSet<Symbol>,
}

impl TypeLayout {
    fn empty(name: Symbol) -> TypeLayout {
        TypeLayout {
            name,
            members: FnvHashMap::default(),
            fields: Vec::new(),
            non_public: IntSet::default(),
            nullable: IntSet::default(),
            mutable: IntSet::default(),
            inner: IntSet::default(),
            init_id: 0,
            member_count: 0,
            init_arity: 0,
            init_assigned: HashSet::new(),
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

    pub fn is_nullable(&self, name: Symbol) -> bool {
        self.resolve_id(name).is_some_and(|id| self.nullable.contains(&id))
    }

    pub fn is_mutable(&self, name: Symbol) -> bool {
        self.resolve_id(name).is_some_and(|id| self.mutable.contains(&id))
    }

    pub fn is_public(&self, name: Symbol) -> bool {
        self.resolve_id(name).is_some_and(|id| !self.non_public.contains(&id))
    }

    pub fn is_inner(&self, name: Symbol) -> bool {
        self.resolve_id(name).is_some_and(|id| self.inner.contains(&id))
    }
}

/// The output of resolution: per-node binding decisions consumed by codegen.
#[derive(Default)]
pub struct Bindings {
    /// Identifier uses and assignment targets => their binding.
    places: FnvHashMap<HirId<HirExpr>, Place>,
    /// `this`/`super` member accesses => their resolution.
    members: FnvHashMap<HirId<HirExpr>, u8>,
    /// `say`/`fn`/`type` statements => the local slot they occupy.
    slots: FnvHashMap<HirId<HirStmt>, u8>,
    /// Function bodies => the captured upvalues of that function.
    upvalues: FnvHashMap<HirId<HirExpr>, Vec<UpvalueLocation>>,
    /// Type declarations => their member layout.
    types: FnvHashMap<HirId<HirStmt>, TypeLayout>,
    /// Type/trait name => its public member names, for the `x has T` surface form. A type
    /// contributes its public members; a trait its declared surface.
    surfaces: FnvHashMap<Symbol, Vec<Symbol>>,
    /// Scope nodes (by HIR node index) => locals to clean up on exit.
    cleanups: FnvHashMap<usize, Vec<Cleanup>>,
    /// Brace-construction expressions => the resolved member ids of their brace fields.
    construct_fields: FnvHashMap<HirId<HirExpr>, Vec<u8>>,
    /// Binding `match` nodes => the local slot of each binder.
    match_binders: FnvHashMap<HirId<HirExpr>, Vec<u8>>,
    /// `match` statements => their scrutinee temp and per-arm binder slots.
    match_info: FnvHashMap<HirId<HirStmt>, MatchInfo>,
    /// `e ?? p => h` handler nodes => the local slot binding the bad value.
    handle_binders: FnvHashMap<HirId<HirExpr>, u8>,
}

/// The slot layout codegen needs for a `match` statement. The scrutinee lives in `scrut_slot`
/// for the whole match. `arm_binders` gives each arm's binder name-to-slot, reusing the same
/// slot block across arms since only one arm runs.
pub struct MatchInfo {
    pub scrut_slot: u8,
    pub arm_binders: Vec<Vec<(Symbol, u8)>>,
    /// Slots reserved for the binder block, the max binder count over all arms.
    pub binder_slots: u8,
}

impl Bindings {
    pub fn place(&self, id: &HirId<HirExpr>) -> Place {
        self.places[id]
    }

    /// The binding of an identifier node.
    pub fn place_of(&self, id: &HirId<HirExpr>) -> Option<Place> {
        self.places.get(id).copied()
    }

    pub fn member(&self, id: &HirId<HirExpr>) -> u8 {
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

    pub fn surface(&self, name: Symbol) -> Option<&[Symbol]> {
        self.surfaces.get(&name).map(Vec::as_slice)
    }

    pub fn cleanup<T>(&self, scope: &HirId<T>) -> &[Cleanup] {
        self.cleanups.get(&scope.index()).map_or(&[], Vec::as_slice)
    }

    pub fn construct_fields(&self, id: &HirId<HirExpr>) -> &[u8] {
        &self.construct_fields[id]
    }

    pub fn match_binders(&self, id: &HirId<HirExpr>) -> Option<&[u8]> {
        self.match_binders.get(id).map(Vec::as_slice)
    }

    pub fn match_info(&self, id: &HirId<HirStmt>) -> &MatchInfo {
        &self.match_info[id]
    }

    pub fn handle_binder(&self, id: &HirId<HirExpr>) -> u8 {
        self.handle_binders[id]
    }
}

struct Local {
    /// `None` for the callee/`this` slot of a method or initializer: it's
    /// addressed positionally (slot 0), never resolved by name.
    name: Option<Symbol>,
    depth: u8,
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
    /// Per trait, its private members' plain name -> renamed slot name (from the HIR). The
    /// resolver scopes a trait body's accesses to its own trait's entry. See `lower::traits`.
    trait_privates: HashMap<Symbol, HashMap<Symbol, Symbol>>,
    /// The plain names of every trait private member (any trait) for diagnostics: an access
    /// that misses but names one of these is reported as private rather than missing.
    private_names: HashSet<Symbol>,
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
        anyhow!("{}", Diagnostic::new(msg, self.hir.pos(node_id).clone()))
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
                        self.declare_local(*name)?;
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
                // Resolve the initializer before declaring the binding, so a name inside it
                // refers to the prior (shadowed) binding, not the one being introduced.
                if let Some(expr) = &field.value {
                    self.expression(expr)?;
                }
                let slot = self.declare_local(field.name)?;
                self.bindings.slots.insert(*stmt_id, slot);
            },
            HirStmt::Expression(expr) => self.expression(expr)?,
            HirStmt::While(cond, body) => self.conditioned(cond, body)?,
            HirStmt::If(cond, then, otherwise) => {
                self.conditioned(cond, then)?;
                if let Some(otherwise) = otherwise {
                    self.statement(otherwise)?;
                }
            },
            HirStmt::Block(body) => self.expression(body)?,
            HirStmt::Nop => {},
            HirStmt::Match(scrutinee, arms) => {
                self.expression(scrutinee)?;
                self.enter_scope();

                // Store the scrutinee in a temp so each arm can access it without re-evaluating.
                // If the scrutinee is a local, we could reuse its slot, but that would require
                // complex analysis across all arms to ensure it's not e.g. mutated or shadowed.
                let scrut_slot = self.declare_temp()?;

                // One binder block sized to the widest arm, counting a binding guard's binders too.
                // Each arm declares its binders from the block base. Truncating after the arm lets
                // the next arm reuse the same slots.
                let block_base = self.locals.len();
                let binder_slots = arms.iter()
                    .map(|a| a.matcher.binders().len() + a.guard.as_ref().map_or(0, |g| self.hir.condition_binders(g).len()))
                    .max().unwrap_or(0);

                let mut arm_binders = Vec::with_capacity(arms.len());
                for arm in arms {
                    let names = arm.matcher.binders();
                    let mut slots = Vec::with_capacity(names.len());
                    for name in &names {
                        slots.push((*name, self.declare_local(*name)?));
                    }

                    // A binding guard publishes into the slots right after the matcher's, so its
                    // stores land inside the reserved block that codegen pushes.
                    if let Some(guard) = &arm.guard {
                        self.resolve_condition(guard, true)?;
                    }

                    self.expression(&arm.body)?;
                    self.locals.truncate(block_base);
                    arm_binders.push(slots);
                }

                // Reserve the block so the scope cleanup pops the scrutinee and every binder slot.
                for _ in 0..binder_slots {
                    self.declare_temp()?;
                }

                self.bindings.match_info.insert(*stmt_id, MatchInfo {
                    scrut_slot,
                    arm_binders,
                    binder_slots: binder_slots as u8,
                });
                self.exit_scope(stmt_id);
            },
        };
        Ok(())
    }

    /// Resolves a condition and its dominated body in one scope, so the condition's binders are
    /// live in the body and dropped after.
    fn conditioned(&mut self, cond: &HirId<HirExpr>, body: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        self.enter_scope();
        self.resolve_condition(cond, true)?;
        self.expression(body)?;
        self.exit_scope(cond);
        Ok(())
    }

    /// Resolves a condition, declaring into the current scope the binders a true result makes
    /// live. An `&&` keeps both sides' binders. An `||` resolves each side but keeps only the
    /// names both sides bind, since either side may have matched.
    fn resolve_condition(&mut self, cond: &HirId<HirExpr>, record: bool) -> Result<(), anyhow::Error> {
        match self.hir.get(cond) {
            HirExpr::Binary(BinOp::And, left, right) => {
                self.resolve_condition(left, record)?;
                self.resolve_condition(right, record)?;
            },
            HirExpr::Binary(BinOp::Or, left, right) => {
                // Both sides bind the identical set only when the union is non-empty. Then each
                // side stores into the shared slots, so record its match binders. Otherwise the
                // sides bind nothing usable, so resolve them only for their scrutinees.
                let union = self.hir.condition_binders(cond);
                let record_sides = record && !union.is_empty();
                let mark = self.locals.len();
                self.resolve_condition(left, record_sides)?;
                self.locals.truncate(mark);
                self.resolve_condition(right, record_sides)?;
                self.locals.truncate(mark);
                for name in union {
                    self.declare_local(name)?;
                }
            },
            HirExpr::Match(scrutinee, matcher) => {
                self.expression(scrutinee)?;
                let mut slots = Vec::new();
                for name in matcher.binders() {
                    slots.push(self.declare_local(name)?);
                }
                if record && !slots.is_empty() {
                    self.bindings.match_binders.insert(*cond, slots);
                }
            },
            _ => self.expression(cond)?,
        }
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
            self.declare_local(name)?;
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
                // The only valid global is a predefined built-in; every other name reaching here is
                // neither a local/upvalue/field nor a declaration, so it is undefined.
                if let Place::Global(g) = place {
                    if !crate::core::builtins::is_builtin(self.hir.text(g)) {
                        compiler_error!(self, expr, "Undefined variable '{}'", self.hir.text(g));
                    }
                }
                self.bindings.places.insert(*expr, place);
            },
            // `x is T`: bind the receiver; `T` is a static name resolved at codegen.
            HirExpr::Is(target, _) => self.expression(target)?,
            // `x has spec`: bind the left value; the spec is a static shape with no bindings.
            HirExpr::Has(left, _) => self.expression(left)?,
            HirExpr::Match(scrutinee, _) => self.expression(scrutinee)?,
            HirExpr::Construct(callee, args, brace) => {
                let callee = *callee;
                let args = args.clone();
                let brace = brace.clone();
                self.construct(expr, &callee, &args, &brace)?;
            },
            HirExpr::Mut(inner) => self.expression(inner)?,
            HirExpr::This => self.require_type(expr)?,
            HirExpr::Coalesce(left, right) => {
                self.expression(left)?;
                self.expression(right)?;
            },
            HirExpr::SafeAccess(target, member, _) => self.index(target, member)?,
            HirExpr::SafeCall(callee, args) => self.call_expression(callee, args)?,
            HirExpr::Propagate(operand) => self.expression(operand)?,
            HirExpr::Handle(left, binder, handler) => {
                self.expression(left)?;
                // The binder is live only while resolving the handler. Its slot is where the bad
                // value already sits, so it is dropped without a cleanup. Slot reused for the result.
                let mark = self.locals.len();
                let slot = self.declare_local(*binder)?;
                self.bindings.handle_binders.insert(*expr, slot);
                self.expression(handler)?;
                self.locals.truncate(mark);
            },
            HirExpr::Assert(operand) => self.expression(operand)?,
        };
        Ok(())
    }

    fn assign(&mut self, lhs: &HirId<HirExpr>, rhs: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        match self.hir.get(lhs) {
            HirExpr::Identifier(name) => {
                let name = *name;
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
                if matches!(self.hir.get(&obj), HirExpr::This) {
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
        if matches!(self.hir.get(target), HirExpr::This) {
            return self.this_member_access(target, member, false);
        }

        self.expression(target)?;
        self.expression(member)?;
        Ok(())
    }

    fn call_expression(&mut self, callee: &HirId<HirExpr>, args: &[HirId<HirExpr>]) -> Result<(), anyhow::Error> {
        self.expression(callee)?;
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
}

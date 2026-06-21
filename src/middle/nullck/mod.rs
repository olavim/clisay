//! Static nullability checking.

mod collect;
mod narrow;
mod native;

use std::collections::{HashMap, HashSet};

use anyhow::anyhow;

use crate::core::objects::TypeMember;
use crate::middle::bind::{Bindings, TypeLayout};
use crate::middle::hir::{BinOp, Hir, HirExpr, HirFnDecl, HirId, HirLiteral, HirStmt, HirTypeDecl, ReturnShape, Symbol, UnOp};

/// A function's per-parameter nullability and return shape.
pub struct FnSig {
    #[allow(dead_code)]
    pub params: Vec<bool>,
    pub ret: ReturnShape,
}

impl FnSig {
    fn of(decl: &HirFnDecl) -> FnSig {
        FnSig { params: decl.params.iter().map(|p| p.nullable).collect(), ret: decl.ret }
    }
}

/// A function's inferred return type tag.
#[derive(Clone, Copy, PartialEq, Eq)]
enum RetTag {
    /// Every return yields the same concrete type.
    Concrete(Symbol),
    /// Every return yields `this`, so the result takes the receiver's type at the call site.
    SelfType,
    Unknown,
}

#[derive(Default)]
pub struct Signatures {
    fns: HashMap<HirId<HirStmt>, FnSig>,
    ret_tags: HashMap<HirId<HirStmt>, RetTag>,
    types_by_name: HashMap<Symbol, HirId<HirStmt>>,
    fns_by_name: HashMap<Symbol, HirId<HirStmt>>,
    methods_by_type: HashMap<(Symbol, Symbol), HirId<HirStmt>>,
    /// Type name to the fields its `init` assigns directly.
    init_fields: HashMap<Symbol, HashSet<Symbol>>,
    /// Type name to fields assigned in its methods.
    method_field_assigns: HashMap<Symbol, HashMap<Symbol, HirId<HirExpr>>>,
}

/// The expression nodes where an `unknown` value crosses into a non-null slot and needs a
/// runtime barrier.
#[derive(Default)]
pub struct Barriers {
    crossings: HashSet<HirId<HirExpr>>,
}

impl Barriers {
    /// Whether a barrier must guard the value produced at this node.
    pub fn has(&self, node: &HirId<HirExpr>) -> bool {
        self.crossings.contains(node)
    }

    pub fn len(&self) -> usize {
        self.crossings.len()
    }
}

pub fn check(hir: &Hir, bindings: &Bindings) -> Result<Barriers, anyhow::Error> {
    let sigs = collect::collect(hir);
    let mut checker = Checker {
        hir,
        bindings,
        sigs: &sigs,
        locals: Vec::new(),
        frame_start: 0,
        current_type: None,
        narrowed: HashSet::new(),
        init_assigned: None,
        this_seen: false,
        current_trait_surface: None,
        current_return: None,
        barriers: HashSet::new(),
    };
    checker.stmt(&hir.get_root())?;
    Ok(Barriers { crossings: checker.barriers })
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Nullness {
    NonNull,
    Nullable,
    Null,
    Unknown,
    Void,
}

#[derive(Clone, PartialEq, Eq)]
enum TypeTag {
    Concrete(Symbol),
    TraitSelf,
    Unknown,
}

#[derive(Clone)]
struct Typed {
    nullness: Nullness,
    tag: TypeTag,
}

impl Typed {
    fn unknown() -> Typed { Typed { nullness: Nullness::Unknown, tag: TypeTag::Unknown } }
    fn nonnull() -> Typed { Typed { nullness: Nullness::NonNull, tag: TypeTag::Unknown } }
    fn null() -> Typed { Typed { nullness: Nullness::Null, tag: TypeTag::Unknown } }
    fn of(nullness: Nullness, tag: TypeTag) -> Typed { Typed { nullness, tag } }
}

/// A tracked binding in the current function frame.
struct Local {
    name: Symbol,
    declared_nullable: bool,
    mutable: bool,
    /// Whether the binding is provably assigned on the current path.
    assigned: bool,
    tag: TypeTag,
    func: Option<HirId<HirStmt>>,
}

/// A narrowable place: a local, a `this` field, or a field of an immutable local receiver.
#[derive(Clone, PartialEq, Eq, Hash)]
enum NarrowKey {
    Local(usize),
    ThisField(Symbol),
    LocalField(usize, Symbol),
}

/// A flow fact a check establishes for a branch.
enum NarrowFact {
    /// The place is non-null.
    NonNull(NarrowKey),
    /// The local has the given concrete type.
    Tag(usize, TypeTag),
}

/// A snapshot of flow facts that branches widen back at a join.
struct FlowSnapshot {
    assigned: Vec<bool>,
    tags: Vec<TypeTag>,
    narrowed: HashSet<NarrowKey>,
}

struct Checker<'a> {
    hir: &'a Hir,
    bindings: &'a Bindings,
    sigs: &'a Signatures,
    locals: Vec<Local>,
    /// The start index in `locals` of the current function frame. Value reads only see
    /// bindings at or above this, so a closure does not read an enclosing local's flow state.
    frame_start: usize,
    /// The enclosing type's name while checking its methods, for `this` typing and field layout.
    current_type: Option<Symbol>,
    /// Places narrowed to non-null on the current path.
    narrowed: HashSet<NarrowKey>,
    /// While checking a type's `init`, the fields assigned so far. The construction seal uses
    /// this for read-before-assignment and rejects any other use of a bare `this`.
    init_assigned: Option<HashSet<Symbol>>,
    /// Whether `this` was used since this flag was last reset, used to catch a closure in `init`
    /// capturing the partially-constructed `this`.
    this_seen: bool,
    current_trait_surface: Option<HashSet<Symbol>>,
    current_return: Option<ReturnShape>,
    /// Nodes where an `unknown` value enters a non-null slot and needs a runtime barrier.
    barriers: HashSet<HirId<HirExpr>>,
}

impl<'a> Checker<'a> {
    fn error<T>(&self, msg: String, node: &HirId<T>) -> anyhow::Error {
        anyhow!("{}\n\tat {}", msg, self.hir.pos(node))
    }

    fn ident_sym(&self, id: &HirId<HirExpr>) -> Symbol {
        match self.hir.get(id) {
            HirExpr::Identifier(sym) => *sym,
            _ => unreachable!("parameter is an identifier"),
        }
    }

    fn frame_index_of(&self, name: Symbol) -> Option<usize> {
        self.locals[self.frame_start..].iter().rposition(|l| l.name == name)
            .map(|i| self.frame_start + i)
    }

    /// The nearest binding of `name` across all frames. Functions resolve across frames so a
    /// nested body can call an enclosing function.
    fn func_of(&self, name: Symbol) -> Option<HirId<HirStmt>> {
        self.locals.iter().rev().find(|l| l.name == name).and_then(|l| l.func)
    }

    /// The layout of a tracked concrete type.
    fn layout_of(&self, name: Symbol) -> Option<&'a TypeLayout> {
        self.sigs.types_by_name.get(&name).map(|stmt| self.bindings.type_layout(stmt))
    }

    fn this_tag(&self) -> TypeTag {
        if self.current_trait_surface.is_some() {
            TypeTag::TraitSelf
        } else {
            self.current_type.map_or(TypeTag::Unknown, TypeTag::Concrete)
        }
    }

    fn this_typed(&self) -> Typed {
        Typed::of(Nullness::NonNull, self.this_tag())
    }

    fn stmt(&mut self, stmt: &HirId<HirStmt>) -> Result<(), anyhow::Error> {
        match self.hir.get(stmt) {
            HirStmt::Fn(decl) => {
                // Register the name first so the body may call itself.
                self.locals.push(Local { name: decl.name, declared_nullable: false, mutable: false, assigned: true, tag: TypeTag::Unknown, func: Some(*stmt) });
                self.function(decl)?;
            },
            HirStmt::Type(decl) => self.type_decl(stmt, Some(decl.name), decl)?,
            HirStmt::Trait(decl) => self.type_decl(stmt, None, decl)?,
            HirStmt::Say(field) => self.say(field.name, field.nullable, field.mutable, &field.value)?,
            HirStmt::Expression(e) | HirStmt::Block(e) => { self.expr(e)?; },
            HirStmt::Return(opt) => match (opt, self.current_return) {
                (Some(e), Some(shape)) => {
                    let typed = self.expr(e)?;
                    self.check_return(typed.nullness, shape, e)?;
                },
                // A `!` function falls back to null on a bare return, which it may not.
                (None, Some(ReturnShape::NonNull)) => {
                    return Err(self.error("A '!' function must return a value, but this 'return' yields null".to_string(), stmt));
                },
                (Some(e), None) => { self.expr(e)?; },
                (None, _) => {},
            },
            HirStmt::Throw(e) => { self.expr(e)?; },
            HirStmt::While(cond, body) => {
                self.expr(cond)?;
                let body_narrow = self.narrowings(cond, true);
                // Loop bodies may run zero times, so their flow facts don't survive the loop.
                let pre = self.snapshot();
                self.apply_narrowings(&body_narrow);
                self.expr(body)?;
                self.restore(&pre);
            },
            HirStmt::If(cond, then, otherwise) => {
                self.expr(cond)?;
                let then_narrow = self.narrowings(cond, true);
                let else_narrow = self.narrowings(cond, false);
                let pre = self.snapshot();
                self.apply_narrowings(&then_narrow);
                self.expr(then)?;
                let then_snap = self.snapshot();
                self.restore(&pre);
                self.apply_narrowings(&else_narrow);
                if let Some(otherwise) = otherwise {
                    self.stmt(otherwise)?;
                }
                let else_snap = self.snapshot();
                self.restore(&pre);
                self.join(&then_snap, &else_snap);
            },
            HirStmt::Try(body, catch, finally) => {
                self.expr(body)?;
                if let Some(catch) = catch {
                    let mark = self.locals.len();
                    if let Some(param) = catch.param {
                        // The thrown value is arbitrary, so the catch parameter is nullable.
                        let name = self.ident_sym(&param);
                        self.locals.push(Local { name, declared_nullable: true, mutable: false, assigned: true, tag: TypeTag::Unknown, func: None });
                    }
                    self.expr(&catch.body)?;
                    self.locals.truncate(mark);
                }
                if let Some(finally) = finally { self.expr(finally)?; }
            },
        }
        Ok(())
    }

    fn say(&mut self, name: Symbol, declared_nullable: bool, mutable: bool, value: &Option<HirId<HirExpr>>) -> Result<(), anyhow::Error> {
        let (assigned, tag) = if let Some(value) = value {
            self.reject_this_store(value)?;
            let typed = self.expr(value)?;
            self.check_into_slot(typed.nullness, declared_nullable, name, value)?;
            (true, typed.tag)
        } else {
            (false, TypeTag::Unknown)
        };
        self.locals.push(Local { name, declared_nullable, mutable, assigned, tag, func: None });
        Ok(())
    }

    /// During init, binding or assigning `this` would let a half-built object outlive the
    /// init. Rejecting it here gives a clearer message than the generic value-use check.
    fn reject_this_store(&self, value: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        if self.init_assigned.is_some() && matches!(self.hir.get(value), HirExpr::This) {
            return Err(self.this_seal_error(value, "cannot be stored"));
        }
        Ok(())
    }

    /// Construction-seal error for a disallowed use of `this` in init.
    fn this_seal_error(&self, node: &HirId<HirExpr>, action: &str) -> anyhow::Error {
        self.error(format!("the partially-constructed 'this' {action}; inside init it may only assign or read its own fields"), node)
    }

    fn function(&mut self, decl: &HirFnDecl) -> Result<(), anyhow::Error> {
        let saved_seal = self.init_assigned.take();
        let saved_frame = self.frame_start;
        // A lambda's shape is inferred, so it is not checked against a declaration.
        let saved_return = std::mem::replace(&mut self.current_return, match decl.ret {
            ReturnShape::Inferred => None,
            ret => Some(ret),
        });
        let mark = self.locals.len();
        self.frame_start = mark;
        for param in &decl.params {
            let name = self.ident_sym(&param.name);
            self.locals.push(Local { name, declared_nullable: param.nullable, mutable: param.mutable, assigned: true, tag: TypeTag::Unknown, func: None });
        }
        self.expr(&decl.body)?;
        // A non-null return must be produced on every path.
        if self.current_return == Some(ReturnShape::NonNull) && !self.definitely_returns(&decl.body) {
            return Err(self.error("This function can finish without returning a value; a '!' return must produce one on every path".to_string(), &decl.body));
        }
        self.locals.truncate(mark);
        self.frame_start = saved_frame;
        self.current_return = saved_return;
        self.init_assigned = saved_seal;
        Ok(())
    }

    /// Whether every path through a function body ends in a `return` or `throw`.
    fn definitely_returns(&self, body: &HirId<HirExpr>) -> bool {
        match self.hir.get(body) {
            HirExpr::Block(stmts) => stmts.iter().any(|s| self.stmt_returns(s)),
            _ => false,
        }
    }

    fn stmt_returns(&self, stmt: &HirId<HirStmt>) -> bool {
        match self.hir.get(stmt) {
            HirStmt::Return(_) | HirStmt::Throw(_) => true,
            HirStmt::Block(body) => self.definitely_returns(body),
            HirStmt::If(_, then, Some(otherwise)) => self.definitely_returns(then) && self.stmt_returns(otherwise),
            _ => false,
        }
    }

    /// Checks a `return <value>` against the declared return shape: a `!` rejects a possibly-null
    /// or void value, a `?` accepts any value, and a void function may not return a value at all.
    fn check_return(&mut self, nullness: Nullness, shape: ReturnShape, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        match shape {
            ReturnShape::Void => Err(self.error("A void function cannot return a value".to_string(), node)),
            ReturnShape::NonNull => match nullness {
                Nullness::Void => Err(self.error("Cannot return a void result from a '!' function".to_string(), node)),
                Nullness::Null | Nullness::Nullable => Err(self.error("A '!' function must return a non-null value".to_string(), node)),
                Nullness::NonNull => Ok(()),
                // An `unknown` returned as non-null is guarded by a runtime barrier.
                Nullness::Unknown => { self.barriers.insert(*node); Ok(()) },
            },
            ReturnShape::Nullable => match nullness {
                Nullness::Void => Err(self.error("Cannot return a void result".to_string(), node)),
                _ => Ok(()),
            },
            ReturnShape::Inferred => Ok(()),
        }
    }

    fn type_decl(&mut self, node: &HirId<HirStmt>, type_name: Option<Symbol>, decl: &HirTypeDecl) -> Result<(), anyhow::Error> {
        let saved_type = self.current_type;
        let saved_surface = self.current_trait_surface.take();
        self.current_type = type_name;
        if let Some(type_name) = type_name {
            self.check_field_definitions(type_name, node)?;
            self.check_method_overrides(decl)?;
            self.check_init(&decl.init, type_name)?;
        } else {
            // A trait method reaches only the trait's declared surface through `this`.
            self.current_trait_surface = Some(decl.surface.clone());
        }
        for method in &decl.methods {
            self.function_stmt(method)?;
        }
        self.current_type = saved_type;
        self.current_trait_surface = saved_surface;
        Ok(())
    }

    /// A member overriding a trait method may return non-null where the traitmethod is nullable,
    /// but not the reverse.
    fn check_method_overrides(&self, decl: &HirTypeDecl) -> Result<(), anyhow::Error> {
        for method in &decl.methods {
            let HirStmt::Fn(folded) = self.hir.get(method) else { continue };
            // A trait method the host overrides is folded under a `"Trait.method"` alias.
            let name = self.hir.text(folded.name);
            let Some(dot) = name.find('.') else { continue };
            let (trait_name, base) = (name[..dot].to_string(), name[dot + 1..].to_string());
            let Some(base_sym) = self.hir.symbol_of(&base) else { continue };
            // A renamed private slot is also dotted. Only an exposed override is a contract.
            if !decl.pub_members.contains(&base_sym) {
                continue;
            }
            let host = decl.methods.iter().copied().find(|s| matches!(self.hir.get(s), HirStmt::Fn(h) if h.name == base_sym));
            let (Some(host), Some(trait_ret)) = (host, self.sigs.fns.get(method).map(|f| f.ret)) else { continue };
            let Some(host_ret) = self.sigs.fns.get(&host).map(|f| f.ret) else { continue };
            if !ret_conforms(host_ret, trait_ret) {
                return Err(self.error(format!("Method '{}' overrides trait '{}' but its return is more nullable than the trait declares", base, trait_name), &host));
            }
        }
        Ok(())
    }

    fn function_stmt(&mut self, stmt: &HirId<HirStmt>) -> Result<(), anyhow::Error> {
        if let HirStmt::Fn(decl) = self.hir.get(stmt) {
            self.function(decl)?;
        }
        Ok(())
    }

    /// Every non-null private field must be initialized by the type's `init` or a default.
    fn check_field_definitions(&self, type_name: Symbol, node: &HirId<HirStmt>) -> Result<(), anyhow::Error> {
        let Some(layout) = self.layout_of(type_name) else { return Ok(()) };
        let assigned = self.sigs.init_fields.get(&type_name);
        for (name, member) in &layout.members {
            let TypeMember::Field(id) = member else { continue };
            let non_null = !layout.nullable.contains(id);
            let private = layout.non_public.contains(id);
            let init_assigned = assigned.is_some_and(|a| a.contains(name));
            if non_null && private && !init_assigned {
                if let Some(assign) = self.sigs.method_field_assigns.get(&type_name).and_then(|m| m.get(name)) {
                    return Err(self.error(format!("Field '{}' must be initialized directly in init or by a default", self.hir.text(*name)), assign));
                }
                return Err(self.error(format!("Non-null field '{}' is never initialized; give it a default or assign it in init", self.hir.text(*name)), node));
            }
        }
        Ok(())
    }

    /// Checks a type's `init` under the construction seal: the bare `this` may assign its own
    /// fields, read assigned fields, and call helpers, but may not escape.
    fn check_init(&mut self, stmt: &HirId<HirStmt>, type_name: Symbol) -> Result<(), anyhow::Error> {
        let HirStmt::Fn(decl) = self.hir.get(stmt) else { return Ok(()) };
        let saved_frame = self.frame_start;
        // An init yields the instance, not a declared value, so its returns are not checked.
        let saved_return = self.current_return.take();
        let mark = self.locals.len();
        self.frame_start = mark;
        for param in &decl.params {
            let name = self.ident_sym(&param.name);
            self.locals.push(Local { name, declared_nullable: param.nullable, mutable: param.mutable, assigned: true, tag: TypeTag::Unknown, func: None });
        }
        // Brace-provided fields are assigned before the init body runs.
        self.init_assigned = Some(self.brace_provided_fields(type_name));
        self.expr(&decl.body)?;
        self.init_assigned = None;
        self.current_return = saved_return;
        self.locals.truncate(mark);
        self.frame_start = saved_frame;
        Ok(())
    }

    /// Returns the public fields of a type that its init does not assign.
    fn brace_provided_fields(&self, type_name: Symbol) -> HashSet<Symbol> {
        let mut seed = HashSet::new();
        let Some(layout) = self.layout_of(type_name) else { return seed };
        let assigned = self.sigs.init_fields.get(&type_name);
        for (name, member) in &layout.members {
            let TypeMember::Field(id) = member else { continue };
            if !layout.non_public.contains(id) && !assigned.is_some_and(|a| a.contains(name)) {
                seed.insert(*name);
            }
        }
        seed
    }

    fn expr(&mut self, expr: &HirId<HirExpr>) -> Result<Typed, anyhow::Error> {
        Ok(match self.hir.get(expr) {
            HirExpr::Literal(HirLiteral::Null) => Typed::null(),
            HirExpr::Literal(lit) => { self.literal_children(lit, expr)?; Typed::nonnull() },
            HirExpr::Identifier(name) => self.identifier(*name, expr)?,
            HirExpr::This => {
                self.this_seen = true;
                if self.init_assigned.is_some() {
                    return Err(self.this_seal_error(expr, "cannot be used as a value here"));
                }
                self.this_typed()
            },
            HirExpr::Assign(lhs, rhs) => self.assign(lhs, rhs)?,
            HirExpr::Call(callee, args) => self.call(callee, args)?,
            HirExpr::Construct(callee, args, brace) => {
                let tag = self.construct_tag(callee);
                for a in args { self.expr(a)?; }
                for (_, v) in brace { self.expr(v)?; }
                if let TypeTag::Concrete(type_name) = &tag {
                    let braced: HashSet<Symbol> = brace.iter().map(|(name, _)| *name).collect();
                    self.check_construction(*type_name, &braced, callee)?;
                }
                Typed::of(Nullness::NonNull, tag)
            },
            HirExpr::Index(target, member, _) => self.index(target, member)?,
            HirExpr::Binary(op, l, r) => self.binary(*op, l, r)?,
            HirExpr::Unary(op, x) => self.unary(*op, x)?,
            HirExpr::Is(x, _) => { self.expr(x)?; Typed::nonnull() },
            HirExpr::Block(stmts) => {
                let mark = self.locals.len();
                for s in stmts { self.stmt(s)?; }
                self.locals.truncate(mark);
                Typed::unknown()
            },
            // `a ?? b` is itself the null check, so it consumes a possibly-null left with no
            // barrier. The result is `a` when non-null, else `b`.
            HirExpr::Coalesce(l, r) => {
                let left = self.expr(l)?;
                let right = self.expr(r)?;
                let nullness = if left.nullness == Nullness::NonNull { Nullness::NonNull } else { right.nullness };
                let tag = if left.tag == right.tag { left.tag } else { TypeTag::Unknown };
                Typed::of(nullness, tag)
            },
            // `a?.b` / `a?[i]` short-circuits to null, so the result is nullable.
            HirExpr::SafeAccess(target, member, _) => { self.expr(target)?; self.expr(member)?; Typed::of(Nullness::Nullable, TypeTag::Unknown) },
            // `a!` asserts non-null, keeping the operand's type tag. A barrier guards it unless
            // the operand is already proven non-null.
            HirExpr::Assert(x) => {
                let typed = self.expr(x)?;
                if typed.nullness != Nullness::NonNull {
                    self.barriers.insert(*expr);
                }
                Typed::of(Nullness::NonNull, typed.tag)
            },
        })
    }

    fn literal_children(&mut self, lit: &HirLiteral, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        match lit {
            HirLiteral::Array(elems) => for e in elems { self.expr(e)?; },
            HirLiteral::Dict(pairs) => for (k, v) in pairs { self.expr(k)?; self.expr(v)?; },
            HirLiteral::Lambda(decl) => self.lambda(decl, node)?,
            _ => {},
        }
        Ok(())
    }

    /// Checks a lambda body. A closure defined in `init` may not capture the partially-built `this`.
    fn lambda(&mut self, decl: &HirFnDecl, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let in_init = self.init_assigned.is_some();
        let outer_seen = self.this_seen;
        self.this_seen = false;
        self.function(decl)?;
        let captured_this = self.this_seen;
        self.this_seen = outer_seen;
        if in_init && captured_this {
            return Err(self.error("a closure in init cannot capture the partially-constructed 'this'".to_string(), node));
        }
        Ok(())
    }

    fn identifier(&mut self, name: Symbol, expr: &HirId<HirExpr>) -> Result<Typed, anyhow::Error> {
        let Some(i) = self.frame_index_of(name) else {
            return Ok(Typed::unknown());
        };
        if self.locals[i].func.is_some() {
            return Ok(Typed::unknown());
        }
        if !self.locals[i].assigned && !self.locals[i].declared_nullable {
            return Err(self.error(format!("'{}' is used before it is assigned", self.hir.text(name)), expr));
        }
        let narrowed = self.narrowed.contains(&NarrowKey::Local(i));
        let nullness = if self.locals[i].declared_nullable && !narrowed { Nullness::Nullable } else { Nullness::NonNull };
        Ok(Typed::of(nullness, self.locals[i].tag.clone()))
    }

    fn assign(&mut self, lhs: &HirId<HirExpr>, rhs: &HirId<HirExpr>) -> Result<Typed, anyhow::Error> {
        self.reject_this_store(rhs)?;
        let typed = self.expr(rhs)?;
        match self.hir.get(lhs) {
            HirExpr::Identifier(name) => {
                let name = *name;
                if let Some(i) = self.frame_index_of(name) {
                    if self.locals[i].func.is_none() {
                        let (mutable, assigned, declared_nullable) =
                            (self.locals[i].mutable, self.locals[i].assigned, self.locals[i].declared_nullable);
                        if !mutable && assigned {
                            return Err(self.error(format!("Cannot reassign immutable binding '{}'; declare it 'mut'", self.hir.text(name)), lhs));
                        }
                        self.check_into_slot(typed.nullness, declared_nullable, name, lhs)?;
                        self.locals[i].assigned = true;
                        self.locals[i].tag = typed.tag.clone();
                        self.reset_narrowing(i, matches!(typed.nullness, Nullness::NonNull));
                    }
                } else {
                    // `field = ...` is implicitly `this.field = ...`
                    self.field_assign(name, lhs)?;
                }
            },
            // `this.field = ...`
            HirExpr::Index(target, member, _) if matches!(self.hir.get(target), HirExpr::This) => {
                if let Some(field) = self.string_member(member) {
                    self.field_assign(field, lhs)?;
                }
            },
            _ => {},
        }
        Ok(typed)
    }

    /// Records and checks an assignment to a field of the enclosing type.
    fn field_assign(&mut self, field: Symbol, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let (is_field, mutable) = match self.current_type.and_then(|t| self.layout_of(t)) {
            Some(layout) => (matches!(layout.members.get(&field), Some(TypeMember::Field(_))), layout.is_mutable(field)),
            None => return Ok(()),
        };
        if !is_field {
            return Ok(());
        }
        if !mutable {
            match &self.init_assigned {
                Some(assigned) if assigned.contains(&field) =>
                    return Err(self.error(format!("Immutable field '{}' is assigned more than once; declare it 'mut'", self.hir.text(field)), node)),
                Some(_) => {},
                None =>
                    return Err(self.error(format!("Cannot assign immutable field '{}' in a method; declare it 'mut'", self.hir.text(field)), node)),
            }
        }
        self.mark_field_assigned(field);
        Ok(())
    }

    fn mark_field_assigned(&mut self, field: Symbol) {
        if let Some(assigned) = self.init_assigned.as_mut() {
            assigned.insert(field);
        }
    }

    /// A construction must supply every non-null public field the `init` does not assign.
    fn check_construction(&self, type_name: Symbol, braced: &HashSet<Symbol>, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let Some(layout) = self.layout_of(type_name) else { return Ok(()) };
        let assigned = self.sigs.init_fields.get(&type_name);
        for (name, member) in &layout.members {
            let TypeMember::Field(id) = member else { continue };
            let non_null = !layout.nullable.contains(id);
            let public = !layout.non_public.contains(id);
            let init_assigned = assigned.is_some_and(|a| a.contains(name));
            if non_null && public && !init_assigned && !braced.contains(name) {
                return Err(self.error(format!("Construction of '{}' is missing non-null field '{}'", self.hir.text(type_name), self.hir.text(*name)), node));
            }
        }
        Ok(())
    }

    fn call(&mut self, callee: &HirId<HirExpr>, args: &[HirId<HirExpr>]) -> Result<Typed, anyhow::Error> {
        for a in args { self.expr(a)?; }
        match self.hir.get(callee) {
            HirExpr::Identifier(name) => {
                let name = *name;
                if self.sigs.types_by_name.contains_key(&name) {
                    self.check_construction(name, &HashSet::new(), callee)?;
                    return Ok(Typed::of(Nullness::NonNull, TypeTag::Concrete(name)));
                }
                match self.func_of(name) {
                    Some(stmt) => Ok(self.call_result(stmt, &TypeTag::Unknown)),
                    None => self.indirect_call(callee),
                }
            },
            // A method call resolves against the receiver's type when it is known.
            HirExpr::Index(receiver, member, _) => {
                let Some(name) = self.member_text(member) else { return self.indirect_call(callee) };
                let receiver_typed = self.receiver(receiver)?;
                if matches!(receiver_typed.tag, TypeTag::TraitSelf) {
                    return self.trait_member(name, member);
                }
                if let (TypeTag::Concrete(type_name), Some(method)) = (&receiver_typed.tag, self.hir.symbol_of(name)) {
                    if let Some(stmt) = self.sigs.methods_by_type.get(&(*type_name, method)).copied() {
                        return Ok(self.call_result(stmt, &receiver_typed.tag));
                    }
                }
                Ok(Typed::unknown())
            },
            _ => self.indirect_call(callee),
        }
    }

    /// A call through a value: the callee must be non-null and its result is a dynamic boundary.
    fn indirect_call(&mut self, callee: &HirId<HirExpr>) -> Result<Typed, anyhow::Error> {
        let callee_typed = self.expr(callee)?;
        self.require_value(callee_typed.nullness, callee)?;
        Ok(Typed::unknown())
    }

    /// The nullability and type of a call result, given the callee and receiver tag.
    fn call_result(&self, stmt: HirId<HirStmt>, receiver_tag: &TypeTag) -> Typed {
        let nullness = match self.sigs.fns.get(&stmt).map(|s| s.ret) {
            Some(ReturnShape::NonNull) => Nullness::NonNull,
            Some(ReturnShape::Nullable) => Nullness::Nullable,
            Some(ReturnShape::Void) => Nullness::Void,
            _ => Nullness::Unknown,
        };
        let tag = match self.sigs.ret_tags.get(&stmt) {
            Some(RetTag::Concrete(name)) => TypeTag::Concrete(*name),
            Some(RetTag::SelfType) => receiver_tag.clone(),
            _ => TypeTag::Unknown,
        };
        Typed::of(nullness, tag)
    }

    fn construct_tag(&self, callee: &HirId<HirExpr>) -> TypeTag {
        match self.hir.get(callee) {
            HirExpr::Identifier(name) if self.sigs.types_by_name.contains_key(name) => TypeTag::Concrete(*name),
            _ => TypeTag::Unknown,
        }
    }

    /// Checks if moving a value into a slot is allowed per its nullability. A nullable or null value into a
    /// non-null slot is an error. `Unknown` is always allowed and gets a runtime barrier.
    fn check_into_slot(&mut self, nullness: Nullness, slot_nullable: bool, name: Symbol, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        if nullness == Nullness::Void {
            return Err(self.error(format!("Cannot assign a void result to '{}'; the call returns no value", self.hir.text(name)), node));
        }
        if slot_nullable {
            return Ok(());
        }
        match nullness {
            Nullness::Null => Err(self.error(format!("Cannot assign null to non-null binding '{}'", self.hir.text(name)), node)),
            Nullness::Nullable => Err(self.error(format!("Cannot assign a nullable value to non-null binding '{}'", self.hir.text(name)), node)),
            // An `unknown` value entering a non-null slot is guarded by a runtime barrier.
            Nullness::Unknown => { self.barriers.insert(*node); Ok(()) },
            Nullness::NonNull | Nullness::Void => Ok(()),
        }
    }

    /// Member or data access `target.member` / `target[member]`. Resolves a field on a known-type
    /// receiver to its declared nullability; any other access is a dynamic-boundary read.
    fn index(&mut self, target: &HirId<HirExpr>, member: &HirId<HirExpr>) -> Result<Typed, anyhow::Error> {
        let receiver = self.receiver(target)?;
        let Some(name) = self.member_text(member) else {
            // A computed `[expr]` access is a dynamic-boundary read.
            self.expr(member)?;
            return Ok(Typed::unknown());
        };
        if matches!(receiver.tag, TypeTag::TraitSelf) {
            return self.trait_member(name, member);
        }
        // A member name never interned as an identifier names no declared member.
        let Some(field) = self.hir.symbol_of(name) else { return Ok(Typed::unknown()) };
        let on_this = matches!(self.hir.get(target), HirExpr::This);
        let key = self.narrowable_field_key(target, field);
        if let TypeTag::Concrete(type_name) = &receiver.tag {
            if let Some(layout) = self.layout_of(*type_name) {
                if let Some(member_kind) = layout.members.get(&field).copied() {
                    let nullness = match member_kind {
                        TypeMember::Field(id) => {
                            // Inside init, a non-null field read before its assignment is unsound.
                            if on_this && !layout.nullable.contains(&id)
                                && self.init_assigned.as_ref().is_some_and(|a| !a.contains(&field)) {
                                return Err(self.error(format!("Field '{}' is read before it is assigned in init", self.hir.text(field)), member));
                            }
                            let narrowed = key.is_some_and(|k| self.narrowed.contains(&k));
                            if layout.is_nullable(field) && !narrowed { Nullness::Nullable } else { Nullness::NonNull }
                        },
                        // A method reference is a non-null value.
                        TypeMember::Method(_) => Nullness::NonNull,
                    };
                    return Ok(Typed::of(nullness, TypeTag::Unknown));
                }
            }
        }
        Ok(Typed::unknown())
    }

    fn trait_member(&self, name: &str, node: &HirId<HirExpr>) -> Result<Typed, anyhow::Error> {
        let in_surface = self.current_trait_surface.as_ref().is_some_and(|surface| surface.iter().any(|m| self.hir.text(*m) == name));
        if !in_surface {
            return Err(self.error(format!("'{}' is not declared or required by this trait; declare it or add a 'req'", name), node));
        }
        Ok(Typed::unknown())
    }

    /// Evaluates a receiver, requiring it to be non-null.
    fn receiver(&mut self, receiver: &HirId<HirExpr>) -> Result<Typed, anyhow::Error> {
        if matches!(self.hir.get(receiver), HirExpr::This) {
            self.this_seen = true;
            return Ok(self.this_typed());
        }
        let typed = self.expr(receiver)?;
        self.require_value(typed.nullness, receiver)?;
        Ok(typed)
    }

    fn binary(&mut self, op: BinOp, l: &HirId<HirExpr>, r: &HirId<HirExpr>) -> Result<Typed, anyhow::Error> {
        match op {
            // Short-circuit operators narrow their left operand into the right operand.
            BinOp::And => {
                self.expr(l)?;
                let narrow = self.narrowings(l, true);
                let pre = self.snapshot();
                self.apply_narrowings(&narrow);
                self.expr(r)?;
                self.restore(&pre);
                Ok(Typed::nonnull())
            },
            BinOp::Or => {
                self.expr(l)?;
                let narrow = self.narrowings(l, false);
                let pre = self.snapshot();
                self.apply_narrowings(&narrow);
                self.expr(r)?;
                self.restore(&pre);
                Ok(Typed::nonnull())
            },
            // Equality is a boolean context; a possibly-null operand is fine.
            BinOp::Equal | BinOp::NotEqual => {
                self.expr(l)?;
                self.expr(r)?;
                Ok(Typed::nonnull())
            },
            _ => {
                let ln = self.expr(l)?;
                self.require_value(ln.nullness, l)?;
                let rn = self.expr(r)?;
                self.require_value(rn.nullness, r)?;
                Ok(Typed::nonnull())
            },
        }
    }

    fn unary(&mut self, op: UnOp, x: &HirId<HirExpr>) -> Result<Typed, anyhow::Error> {
        let typed = self.expr(x)?;
        // `!` is a boolean context; negation and bitwise-not require a value.
        if matches!(op, UnOp::Negate | UnOp::BitNot) {
            self.require_value(typed.nullness, x)?;
        }
        Ok(Typed::nonnull())
    }

    /// Rejects an operation that requires a value when the operand may be null or is void.
    fn require_value(&self, nullness: Nullness, operand: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        if nullness == Nullness::Void {
            return Err(self.error("This call returns no value, so its result cannot be used here".to_string(), operand));
        }
        if !matches!(nullness, Nullness::Nullable | Nullness::Null) {
            return Ok(());
        }
        let msg = match self.hir.get(operand) {
            HirExpr::Identifier(name) => format!("'{}' may be null; narrow it before use", self.hir.text(*name)),
            _ => "Value may be null; narrow it before use".to_string(),
        };
        Err(self.error(msg, operand))
    }

    fn member_text(&self, member: &HirId<HirExpr>) -> Option<&'a str> {
        match self.hir.get(member) {
            HirExpr::Literal(HirLiteral::String(name)) => Some(name),
            _ => None,
        }
    }

    fn string_member(&self, member: &HirId<HirExpr>) -> Option<Symbol> {
        self.member_text(member).and_then(|name| self.hir.symbol_of(name))
    }

}

/// Whether a member's return shape conforms to a trait method's. An override may be non-null
/// where the trait is nullable, but not the reverse. A void trait method accepts any return,
/// since its result is ignored.
fn ret_conforms(host_ret: ReturnShape, trait_ret: ReturnShape) -> bool {
    match trait_ret {
        ReturnShape::Void | ReturnShape::Inferred => true,
        ReturnShape::NonNull => host_ret == ReturnShape::NonNull,
        ReturnShape::Nullable => matches!(host_ret, ReturnShape::NonNull | ReturnShape::Nullable),
    }
}

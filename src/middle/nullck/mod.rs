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
}

pub fn check(hir: &Hir, bindings: &Bindings) -> Result<(), anyhow::Error> {
    let sigs = collect::collect(hir);
    let mut checker = Checker {
        hir,
        bindings,
        sigs: &sigs,
        locals: Vec::new(),
        frame_start: 0,
        current_type: None,
        narrowed: HashSet::new(),
    };
    checker.stmt(&hir.get_root())
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Nullness {
    NonNull,
    Nullable,
    Null,
    Unknown,
}

#[derive(Clone, PartialEq, Eq)]
enum TypeTag {
    Concrete(Symbol),
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

    /// The layout of a tracked concrete type, else `None`.
    fn layout_of(&self, name: Symbol) -> Option<&'a TypeLayout> {
        self.sigs.types_by_name.get(&name).map(|stmt| self.bindings.type_layout(stmt))
    }

    /// The type tag of `this` in the current method.
    fn this_tag(&self) -> TypeTag {
        self.current_type.map_or(TypeTag::Unknown, TypeTag::Concrete)
    }

    /// The typed value of `this`: always non-null, carrying the current type's tag.
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
            HirStmt::Type(decl) => self.type_decl(Some(decl.name), decl)?,
            // A trait has no runtime layout, so its `this` fields are not resolved here.
            HirStmt::Trait(decl) => self.type_decl(None, decl)?,
            HirStmt::Say(field) => self.say(field.name, field.nullable, field.mutable, &field.value)?,
            HirStmt::Expression(e) | HirStmt::Block(e) => { self.expr(e)?; },
            HirStmt::Return(opt) => if let Some(e) = opt { self.expr(e)?; },
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
            let typed = self.expr(value)?;
            self.check_into_slot(typed.nullness, declared_nullable, name, value)?;
            (true, typed.tag)
        } else {
            (false, TypeTag::Unknown)
        };
        self.locals.push(Local { name, declared_nullable, mutable, assigned, tag, func: None });
        Ok(())
    }

    /// Checks a function/method/lambda body in a fresh frame holding its parameters.
    fn function(&mut self, decl: &HirFnDecl) -> Result<(), anyhow::Error> {
        let saved_frame = self.frame_start;
        let mark = self.locals.len();
        self.frame_start = mark;
        for param in &decl.params {
            let name = self.ident_sym(&param.name);
            self.locals.push(Local { name, declared_nullable: param.nullable, mutable: param.mutable, assigned: true, tag: TypeTag::Unknown, func: None });
        }
        self.expr(&decl.body)?;
        self.locals.truncate(mark);
        self.frame_start = saved_frame;
        Ok(())
    }

    fn type_decl(&mut self, type_name: Option<Symbol>, decl: &HirTypeDecl) -> Result<(), anyhow::Error> {
        let saved = self.current_type;
        self.current_type = type_name;
        self.function_stmt(&decl.init)?;
        for method in &decl.methods {
            self.function_stmt(method)?;
        }
        self.current_type = saved;
        Ok(())
    }

    fn function_stmt(&mut self, stmt: &HirId<HirStmt>) -> Result<(), anyhow::Error> {
        if let HirStmt::Fn(decl) = self.hir.get(stmt) {
            self.function(decl)?;
        }
        Ok(())
    }

    /// Evaluates an expression, emitting diagnostics, and returns its static nullability and type.
    fn expr(&mut self, expr: &HirId<HirExpr>) -> Result<Typed, anyhow::Error> {
        Ok(match self.hir.get(expr) {
            HirExpr::Literal(HirLiteral::Null) => Typed::null(),
            HirExpr::Literal(lit) => { self.literal_children(lit)?; Typed::nonnull() },
            HirExpr::Identifier(name) => self.identifier(*name, expr)?,
            HirExpr::This => self.this_typed(),
            HirExpr::Assign(lhs, rhs) => self.assign(lhs, rhs)?,
            HirExpr::Call(callee, args) => self.call(callee, args)?,
            HirExpr::Construct(callee, args, brace) => {
                let tag = self.construct_tag(callee);
                for a in args { self.expr(a)?; }
                for (_, v) in brace { self.expr(v)?; }
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
            HirExpr::Coalesce(l, r) => { self.expr(l)?; self.expr(r)?; Typed::unknown() },
            HirExpr::SafeAccess(target, member, _) => { self.expr(target)?; self.expr(member)?; Typed::unknown() },
            HirExpr::Assert(x) => { self.expr(x)?; Typed::nonnull() },
        })
    }

    fn literal_children(&mut self, lit: &HirLiteral) -> Result<(), anyhow::Error> {
        match lit {
            HirLiteral::Array(elems) => for e in elems { self.expr(e)?; },
            HirLiteral::Dict(pairs) => for (k, v) in pairs { self.expr(k)?; self.expr(v)?; },
            HirLiteral::Lambda(decl) => self.function(decl)?,
            _ => {},
        }
        Ok(())
    }

    fn identifier(&mut self, name: Symbol, expr: &HirId<HirExpr>) -> Result<Typed, anyhow::Error> {
        let Some(i) = self.frame_index_of(name) else {
            // Captures, globals, types, and fields are not tracked as locals here.
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
        let typed = self.expr(rhs)?;
        if let HirExpr::Identifier(name) = self.hir.get(lhs) {
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
            }
        }
        Ok(typed)
    }

    fn call(&mut self, callee: &HirId<HirExpr>, args: &[HirId<HirExpr>]) -> Result<Typed, anyhow::Error> {
        for a in args { self.expr(a)?; }
        match self.hir.get(callee) {
            HirExpr::Identifier(name) => {
                let name = *name;
                // A type name called yields a non-null instance of that type.
                if self.sigs.types_by_name.contains_key(&name) {
                    return Ok(Typed::of(Nullness::NonNull, TypeTag::Concrete(name)));
                }
                match self.func_of(name) {
                    Some(stmt) => Ok(self.call_result(stmt, &TypeTag::Unknown)),
                    None => self.indirect_call(callee),
                }
            },
            // A method call resolves against the receiver's type when it is known.
            HirExpr::Index(receiver, member, _) => {
                let Some(method) = self.string_member(member) else { return self.indirect_call(callee) };
                let receiver_typed = self.receiver(receiver)?;
                if let TypeTag::Concrete(type_name) = receiver_typed.tag {
                    if let Some(stmt) = self.sigs.methods_by_type.get(&(type_name, method)).copied() {
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

    /// The nullability and type of a call result, given the resolved callee and receiver tag.
    /// A self-typed return takes the receiver's tag.
    fn call_result(&self, stmt: HirId<HirStmt>, receiver_tag: &TypeTag) -> Typed {
        let nullness = match self.sigs.fns.get(&stmt).map(|s| s.ret) {
            Some(ReturnShape::NonNull) => Nullness::NonNull,
            Some(ReturnShape::Nullable) => Nullness::Nullable,
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
    fn check_into_slot(&self, nullness: Nullness, slot_nullable: bool, name: Symbol, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        if slot_nullable {
            return Ok(());
        }
        match nullness {
            Nullness::Null => Err(self.error(format!("Cannot assign null to non-null binding '{}'", self.hir.text(name)), node)),
            Nullness::Nullable => Err(self.error(format!("Cannot assign a nullable value to non-null binding '{}'", self.hir.text(name)), node)),
            Nullness::NonNull | Nullness::Unknown => Ok(()),
        }
    }

    /// Member or data access `target.member` / `target[member]`. Resolves a field on a known-type
    /// receiver to its declared nullability; any other access is a dynamic-boundary read.
    fn index(&mut self, target: &HirId<HirExpr>, member: &HirId<HirExpr>) -> Result<Typed, anyhow::Error> {
        let receiver = self.receiver(target)?;
        let Some(field) = self.string_member(member) else {
            // A computed `[expr]` access is a dynamic-boundary read.
            self.expr(member)?;
            return Ok(Typed::unknown());
        };
        let key = self.narrowable_field_key(target, field);
        if let TypeTag::Concrete(type_name) = &receiver.tag {
            if let Some(layout) = self.layout_of(*type_name) {
                if let Some(member_kind) = layout.members.get(&field).copied() {
                    let nullness = match member_kind {
                        TypeMember::Field(_) => {
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

    /// Evaluates a receiver, requiring it to be non-null. `this` is always non-null.
    fn receiver(&mut self, receiver: &HirId<HirExpr>) -> Result<Typed, anyhow::Error> {
        if matches!(self.hir.get(receiver), HirExpr::This) {
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

    /// Rejects an operation that requires a value when the operand may be null.
    fn require_value(&self, nullness: Nullness, operand: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        if !matches!(nullness, Nullness::Nullable | Nullness::Null) {
            return Ok(());
        }
        let msg = match self.hir.get(operand) {
            HirExpr::Identifier(name) => format!("'{}' may be null; narrow it before use", self.hir.text(*name)),
            _ => "Value may be null; narrow it before use".to_string(),
        };
        Err(self.error(msg, operand))
    }

    /// The interned field name of a `.name` access, or `None` for a computed `[expr]` access.
    fn string_member(&self, member: &HirId<HirExpr>) -> Option<Symbol> {
        match self.hir.get(member) {
            HirExpr::Literal(HirLiteral::String(name)) => self.hir.symbol_of(name),
            _ => None,
        }
    }

}

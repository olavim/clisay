//! Flow-sensitive semantic checks.

mod assign;
mod call;
mod construct;
mod narrow;
mod native;
mod returns;
mod traits;

use std::collections::{HashMap, HashSet};

use anyhow::anyhow;

use crate::frontend::lex::Diagnostic;

use crate::core::objects::TypeMember;
use crate::middle::bind::{Bindings, TypeLayout};
use crate::middle::signatures::{Signatures, TypeTag, Witness};
use self::construct::Seal;
use crate::middle::hir::{BinOp, Hir, HirExpr, HirFnDecl, HirId, HirLiteral, HirMatchArm, HirMatcher, HirParam, HirStmt, HirTypeDecl, ReturnShape, Symbol, UnOp};

/// The runtime checks codegen emits: null-barriers where an `unknown` value crosses into an
/// unobligated slot, and object-witness tests at discharge nodes.
#[derive(Default)]
pub struct Barriers {
    /// Nodes where an `unknown` value crosses into a clean slot and needs a null-barrier.
    null_barriers: HashSet<HirId<HirExpr>>,
    /// Discharge nodes (`??`, `?`) whose operand owes an object witness.
    witness_tests: HashSet<HirId<HirExpr>>,
}

impl Barriers {
    /// Whether a null-barrier must guard the value produced at this node.
    pub fn has(&self, node: &HirId<HirExpr>) -> bool {
        self.null_barriers.contains(node)
    }

    /// Whether a discharge node must run the general object-witness test, not just the null fast-path.
    pub fn tests_object_witness(&self, node: &HirId<HirExpr>) -> bool {
        self.witness_tests.contains(node)
    }

    pub fn len(&self) -> usize {
        self.null_barriers.len()
    }

    pub fn is_empty(&self) -> bool {
        self.null_barriers.is_empty()
    }
}

pub fn check(hir: &Hir, bindings: &Bindings, sigs: &Signatures) -> Result<Barriers, anyhow::Error> {
    let mut checker = Checker::new(hir, bindings, sigs);
    checker.stmt(&hir.get_root())?;
    Ok(Barriers { null_barriers: checker.barriers, witness_tests: checker.witness_tests })
}

/// The obligation state of a value as it flows.
#[derive(Clone)]
enum Flow {
    /// A present value owing no obligations.
    Clean,
    /// A void result: no value at all.
    Void,
    /// A dynamic-boundary value whose obligations are unknown.
    Unknown,
    /// A value owing obligations. `definite` marks a value known to be in the bad state, as
    /// opposed to one that only may be.
    Bad { obligations: HashSet<Symbol>, definite: bool },
}

impl Flow {
    fn is_void(&self) -> bool {
        matches!(self, Flow::Void)
    }
}

/// Why a value fails to satisfy a non-null target.
enum Violation {
    Void,
    Null,
    Nullable,
}

#[derive(Clone)]
struct Typed {
    flow: Flow,
    tag: TypeTag,
}

impl Typed {
    fn unknown() -> Typed { Typed { flow: Flow::Unknown, tag: TypeTag::Unknown } }
    fn nonnull() -> Typed { Typed { flow: Flow::Clean, tag: TypeTag::Unknown } }
    fn of(flow: Flow, tag: TypeTag) -> Typed { Typed { flow, tag } }
}

/// A type's field with the facts the definition and construction checks need.
struct FieldInfo {
    name: Symbol,
    non_null: bool,
    public: bool,
    /// Whether the type's `init` assigns this field directly.
    init_assigned: bool,
}

/// A tracked binding in the current function frame.
struct Local {
    name: Symbol,
    /// The obligations this binding owes. Reading it yields those obligations until it is narrowed.
    owed: HashSet<Symbol>,
    mutable: bool,
    /// Whether the binding is provably assigned on the current path.
    assigned: bool,
    tag: TypeTag,
    func: Option<HirId<HirStmt>>,
    binder: bool,
}

impl Local {
    fn param(name: Symbol, owed: HashSet<Symbol>, mutable: bool) -> Local {
        Local { name, owed, mutable, assigned: true, tag: TypeTag::Unknown, func: None, binder: false }
    }

    fn catch(name: Symbol, owed: HashSet<Symbol>, mutable: bool) -> Local {
        Local { name, owed, mutable, assigned: true, tag: TypeTag::Unknown, func: None, binder: false }
    }

    fn binder(name: Symbol) -> Local {
        Local { name, owed: HashSet::new(), mutable: false, assigned: true, tag: TypeTag::Unknown, func: None, binder: true }
    }

    fn binder_owing(name: Symbol, owed: HashSet<Symbol>) -> Local {
        Local { name, owed, mutable: false, assigned: true, tag: TypeTag::Unknown, func: None, binder: true }
    }

    fn func(name: Symbol, stmt: HirId<HirStmt>) -> Local {
        Local { name, owed: HashSet::new(), mutable: false, assigned: true, tag: TypeTag::Unknown, func: Some(stmt), binder: false }
    }

    fn value(name: Symbol, owed: HashSet<Symbol>, mutable: bool, assigned: bool, tag: TypeTag) -> Local {
        Local { name, owed, mutable, assigned, tag, func: None, binder: false }
    }
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
    /// The place no longer owes this obligation on the branch.
    Discharge(NarrowKey, Symbol),
    /// The local has the given concrete type.
    Tag(usize, TypeTag),
}

/// The flow state of one local.
#[derive(Clone)]
struct LocalFlow {
    assigned: bool,
    tag: TypeTag,
}

/// A snapshot of flow facts that branches widen back at a join.
struct FlowSnapshot {
    locals: Vec<LocalFlow>,
    narrowed: HashMap<NarrowKey, HashSet<Symbol>>,
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
    /// The obligations discharged per place on the current path.
    narrowed: HashMap<NarrowKey, HashSet<Symbol>>,
    /// The construction seal while checking a type's `init`.
    seal: Seal,
    current_trait_surface: Option<HashSet<Symbol>>,
    current_return: Option<ReturnShape>,
    /// Nodes where an `unknown` value enters a non-null slot and needs a runtime barrier.
    barriers: HashSet<HirId<HirExpr>>,
    /// Discharge nodes whose operand owes an object witness.
    witness_tests: HashSet<HirId<HirExpr>>,
}

impl<'a> Checker<'a> {
    fn new(hir: &'a Hir, bindings: &'a Bindings, sigs: &'a Signatures) -> Checker<'a> {
        Checker {
            hir,
            bindings,
            sigs,
            locals: Vec::new(),
            frame_start: 0,
            current_type: None,
            narrowed: HashMap::new(),
            seal: Seal::default(),
            current_trait_surface: None,
            current_return: None,
            barriers: HashSet::new(),
            witness_tests: HashSet::new(),
        }
    }

    fn error<T>(&self, msg: String, node: &HirId<T>) -> anyhow::Error {
        anyhow!("{}", Diagnostic::new(msg, self.hir.pos(node).clone()))
    }

    /// An error carrying a `help:` note on how to fix it.
    fn error_help<T>(&self, msg: String, node: &HirId<T>, help: impl Into<String>) -> anyhow::Error {
        anyhow!("{}", Diagnostic::new(msg, self.hir.pos(node).clone()).with_help(help))
    }

    /// An error caretting two nodes, each with its own label.
    fn error_two_spans<T, U>(&self, msg: String, primary: &HirId<T>, primary_label: &str, other: &HirId<U>, other_label: &str, help: String) -> anyhow::Error {
        anyhow!("{}", Diagnostic::new(msg, self.hir.pos(primary).clone())
            .with_label(primary_label)
            .with_span(self.hir.pos(other).clone(), other_label)
            .with_help(help))
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
            TypeTag::SelfType
        } else {
            self.current_type.map_or(TypeTag::Unknown, TypeTag::Concrete)
        }
    }

    fn this_typed(&self) -> Typed {
        Typed::of(Flow::Clean, self.this_tag())
    }

    /// A value owing `opt`. `definite` marks a known-null value versus a possibly-null one.
    fn opt_flow(&self, definite: bool) -> Flow {
        Flow::Bad { obligations: HashSet::from([self.sigs.opt]), definite }
    }

    fn opt_set(&self, nullable: bool) -> HashSet<Symbol> {
        if nullable { HashSet::from([self.sigs.opt]) } else { HashSet::new() }
    }

    /// A value owing `fails`: an `Err` witness.
    fn fails_flow(&self) -> Flow {
        Flow::Bad { obligations: HashSet::from([self.sigs.fails]), definite: false }
    }

    fn owes_object_witness(&self, flow: &Flow) -> bool {
        matches!(flow, Flow::Bad { obligations, .. } if obligations.contains(&self.sigs.fails))
    }

    /// The witness type name when the value is confirmed to be a witness it owes.
    fn confirmed_witness_name(&self, typed: &Typed) -> Option<&'a str> {
        let TypeTag::Concrete(tag) = &typed.tag else { return None };
        let Flow::Bad { obligations, .. } = &typed.flow else { return None };
        if !obligations.iter().all(|o| matches!(self.sigs.witness(*o), Some(Witness::Type(_) | Witness::Trait(_)))) {
            return None;
        }
        obligations.iter().find_map(|o| match self.sigs.witness(*o) {
            Some(Witness::Type(w)) if w == tag => Some(self.hir.text(*w)),
            _ => None,
        })
    }

    /// Whether the value is confirmed to be one of the witnesses it owes.
    fn confirmed_witness(&self, typed: &Typed) -> bool {
        self.confirmed_witness_name(typed).is_some()
    }

    /// The tag a caught value narrows to. A single type witness confirms the value's type. A set,
    /// a trait witness, or `opt` leaves it unknown.
    fn single_object_witness_tag(&self, caught: &HashSet<Symbol>) -> TypeTag {
        let mut it = caught.iter();
        match (it.next(), it.next()) {
            (Some(o), None) => match self.sigs.witness(*o) {
                Some(Witness::Type(name)) => TypeTag::Concrete(*name),
                _ => TypeTag::Unknown,
            },
            _ => TypeTag::Unknown,
        }
    }

    /// The result of a `?` chain. It carries the operand's obligations, since a bad operand
    /// short-circuits to that value. `opt` is always added because the chain can also yield null on
    /// the clean path: a null short-circuit when the operand owes `opt`, or a nullable/dynamic
    /// access. The exact member flow is discarded, so this is conservative but never unsound.
    fn chain_result(&mut self, operand: &Flow, node: &HirId<HirExpr>) -> Typed {
        if self.owes_object_witness(operand) {
            self.witness_tests.insert(*node);
        }
        let mut obligations = match operand {
            Flow::Bad { obligations, .. } => obligations.clone(),
            _ => HashSet::new(),
        };
        obligations.insert(self.sigs.opt);
        Typed::of(Flow::Bad { obligations, definite: false }, TypeTag::Unknown)
    }

    fn stmt(&mut self, stmt: &HirId<HirStmt>) -> Result<(), anyhow::Error> {
        match self.hir.get(stmt) {
            HirStmt::Nop => {},
            HirStmt::Fn(decl) => {
                // Register the name first so the body may call itself.
                self.locals.push(Local::func(decl.name, *stmt));
                self.function(decl)?;
            },
            HirStmt::Type(decl) => self.type_decl(stmt, Some(decl.name), decl)?,
            HirStmt::Trait(decl) => self.type_decl(stmt, None, decl)?,
            HirStmt::Say(field) => self.say(field.name, field.nullable, field.mutable, &field.value)?,
            HirStmt::Expression(e) | HirStmt::Block(e) => { self.expr(e)?; },
            HirStmt::Return(opt) => match (opt, self.current_return) {
                (Some(e), Some(shape)) => {
                    let typed = self.expr(e)?;
                    self.check_return(&typed.flow, shape, e)?;
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
                let binders = self.hir.condition_binders(cond);
                // Loop bodies may run zero times, so their flow facts don't survive the loop.
                self.narrow_branch(&body_narrow, |c| -> Result<(), anyhow::Error> {
                    c.with_binders(&binders, |c| c.expr(body))?;
                    Ok(())
                })?;
            },
            HirStmt::If(cond, then, otherwise) => {
                self.expr(cond)?;
                let then_narrow = self.narrowings(cond, true);
                let else_narrow = self.narrowings(cond, false);
                let binders = self.hir.condition_binders(cond);
                let then_snap = self.narrow_branch(&then_narrow, |c| -> Result<FlowSnapshot, anyhow::Error> {
                    c.with_binders(&binders, |c| c.expr(then))?;
                    Ok(c.snapshot())
                })?;
                let else_snap = self.narrow_branch(&else_narrow, |c| -> Result<FlowSnapshot, anyhow::Error> {
                    if let Some(otherwise) = otherwise {
                        c.stmt(otherwise)?;
                    }
                    Ok(c.snapshot())
                })?;
                self.join(&then_snap, &else_snap);
            },
            HirStmt::Try(body, catch, finally) => {
                self.expr(body)?;
                if let Some(catch) = catch {
                    let mark = self.locals.len();
                    if let Some(param) = catch.param {
                        let name = self.ident_sym(&param);
                        self.locals.push(Local::catch(name, self.opt_set(true), catch.mutable));
                    }
                    self.expr(&catch.body)?;
                    self.locals.truncate(mark);
                }
                if let Some(finally) = finally { self.expr(finally)?; }
            },
            HirStmt::Match(scrutinee, arms) => {
                let typed = self.expr(scrutinee)?;
                if typed.flow.is_void() {
                    return Err(self.error("This call returns no value, so its result cannot be matched here".to_string(), scrutinee));
                }
                // A match discharges by ruling out witnesses. A guard-free arm total over a witness
                // clears it for the arms below.
                let mut remaining = match &typed.flow {
                    Flow::Bad { obligations, .. } => obligations.clone(),
                    _ => HashSet::new(),
                };
                for arm in arms {
                    let whole_binders = whole_value_binders(&arm.matcher);
                    let mut binders = arm.matcher.binders();
                    if let Some(guard) = &arm.guard {
                        binders.extend(self.hir.condition_binders(guard));
                    }
                    self.with_arm_binders(&binders, &whole_binders, &remaining, |c| -> Result<(), anyhow::Error> {
                        if let Some(guard) = &arm.guard { c.expr(guard)?; }
                        c.expr(&arm.body)?;
                        Ok(())
                    })?;
                    let ruled = self.arm_rules_out(arm, &remaining);
                    remaining.retain(|w| !ruled.contains(w));
                }
            },
        }
        Ok(())
    }

    /// Declares `binders` as immutable locals for the duration of `f`, then drops them.
    fn with_binders<R>(&mut self, binders: &[Symbol], f: impl FnOnce(&mut Self) -> R) -> R {
        let mark = self.locals.len();
        for &name in binders {
            self.locals.push(Local::binder(name));
        }
        let r = f(self);
        self.locals.truncate(mark);
        r
    }

    /// Declares a match arm's binders for the duration of `f`.
    fn with_arm_binders<R>(&mut self, binders: &[Symbol], whole_binders: &[Symbol], remaining_obligations: &HashSet<Symbol>, f: impl FnOnce(&mut Self) -> R) -> R {
        let mark = self.locals.len();
        for &name in binders {
            let owed = if whole_binders.contains(&name) { remaining_obligations.clone() } else { HashSet::new() };
            self.locals.push(Local::binder_owing(name, owed));
        }
        let r = f(self);
        self.locals.truncate(mark);
        r
    }

    fn expr(&mut self, expr: &HirId<HirExpr>) -> Result<Typed, anyhow::Error> {
        Ok(match self.hir.get(expr) {
            HirExpr::Literal(HirLiteral::Null) => Typed::of(self.opt_flow(true), TypeTag::Unknown),
            HirExpr::Literal(lit) => { self.literal_children(lit, expr)?; Typed::nonnull() },
            HirExpr::Identifier(name) => self.identifier(*name, expr)?,
            HirExpr::This => {
                self.seal.set_this_seen(true);
                if self.seal.in_init() {
                    return Err(self.this_seal_error(expr, "cannot be used as a value here"));
                }
                self.this_typed()
            },
            HirExpr::Assign(lhs, rhs) => self.assign(lhs, rhs)?,
            HirExpr::Call(callee, args) => self.call(callee, args)?,
            HirExpr::Construct(callee, args, brace) => {
                let tag = self.construct_tag(callee);
                for a in args { self.expr(a)?; }
                for (name, v) in brace {
                    let typed = self.expr(v)?;
                    if let TypeTag::Concrete(type_name) = &tag {
                        self.check_brace_field(*type_name, *name, &typed.flow, v)?;
                    }
                }
                if let TypeTag::Concrete(type_name) = &tag {
                    let braced: HashSet<Symbol> = brace.iter().map(|(name, _)| *name).collect();
                    self.check_construction(*type_name, &braced, callee)?;
                }
                Typed::of(Flow::Clean, tag)
            },
            HirExpr::Index(target, member, _) => self.member_access(target, member)?,
            HirExpr::Binary(op, l, r) => self.binary(*op, l, r)?,
            HirExpr::Unary(op, x) => self.unary(*op, x)?,
            HirExpr::Is(x, _) => { self.expr(x)?; Typed::nonnull() },
            HirExpr::Has(left, _) => {
                let typed = self.expr(left)?;
                if typed.flow.is_void() {
                    return Err(self.error("This call returns no value, so its result cannot be used here".to_string(), left));
                }
                Typed::nonnull()
            },
            HirExpr::Match(scrutinee, _) => {
                let typed = self.expr(scrutinee)?;
                if typed.flow.is_void() {
                    return Err(self.error("This call returns no value, so its result cannot be matched here".to_string(), scrutinee));
                }
                Typed::nonnull()
            },
            HirExpr::Block(stmts) => {
                let mark = self.locals.len();
                for s in stmts { self.stmt(s)?; }
                self.locals.truncate(mark);
                Typed::unknown()
            },
            // `a ?? b` discharges the whole obligation set: the fallback runs on any bad value, so
            // a possibly-bad left crosses with no barrier. The result is `a` when clean, else `b`.
            HirExpr::Coalesce(l, r) => {
                let left = self.expr(l)?;
                if self.owes_object_witness(&left.flow) { self.witness_tests.insert(*expr); }
                let right = self.expr(r)?;
                let tag = if left.tag == right.tag { left.tag.clone() } else { TypeTag::Unknown };
                let flow = if matches!(left.flow, Flow::Clean) { Flow::Clean } else { right.flow };
                Typed::of(flow, tag)
            },
            // `a?.b` / `a?[i]` short-circuits on a bad operand, so the result carries the operand's
            // obligations.
            HirExpr::SafeAccess(target, member, _) => {
                let target = self.expr(target)?;
                self.expr(member)?;
                self.chain_result(&target.flow, expr)
            },
            // `cb?(args)` short-circuits on a bad callee, carrying its obligations.
            HirExpr::SafeCall(callee, args) => {
                let callee = self.expr(callee)?;
                for a in args { self.expr(a)?; }
                self.chain_result(&callee.flow, expr)
            },
            // `a?!` discharges the operand on its fall-through path. The enclosing function carries
            // the obligation instead, recorded in signatures. The yielded value is clean.
            HirExpr::Propagate(operand) => {
                let typed = self.expr(operand)?;
                Typed::of(Flow::Clean, typed.tag)
            },
            // `a ?? p => h` binds the caught bad value to `p`, which still owes what `a` owed. A
            // single type witness narrows `p`'s tag, so a caught `Err` is usable as one.
            HirExpr::Handle(left, binder, handler) => {
                let left = self.expr(left)?;
                let caught: HashSet<Symbol> = match &left.flow {
                    Flow::Bad { obligations, .. } => obligations.clone(),
                    _ => HashSet::new(),
                };
                let tag = self.single_object_witness_tag(&caught);
                let mut binder_local = Local::binder_owing(*binder, caught);
                binder_local.tag = tag;
                let mark = self.locals.len();
                self.locals.push(binder_local);
                let h = self.expr(handler)?;
                self.locals.truncate(mark);
                let tag = if left.tag == h.tag { left.tag.clone() } else { TypeTag::Unknown };
                let flow = if matches!(left.flow, Flow::Clean) { Flow::Clean } else { h.flow };
                Typed::of(flow, tag)
            },
            // `a!` asserts the value is clean, keeping its type tag. A barrier guards it unless
            // the operand is already proven clean.
            HirExpr::Assert(x) => {
                let typed = self.expr(x)?;
                if !matches!(typed.flow, Flow::Clean) {
                    self.add_barrier(expr);
                }
                Typed::of(Flow::Clean, typed.tag)
            },
        })
    }

    fn say(&mut self, name: Symbol, declared_nullable: bool, mutable: bool, value: &Option<HirId<HirExpr>>) -> Result<(), anyhow::Error> {
        let (assigned, tag) = if let Some(value) = value {
            self.reject_this_store(value)?;
            let typed = self.expr(value)?;
            self.check_into_slot(&typed.flow, declared_nullable, name, value)?;
            (true, typed.tag)
        } else {
            (false, TypeTag::Unknown)
        };
        self.locals.push(Local::value(name, self.opt_set(declared_nullable), mutable, assigned, tag));
        Ok(())
    }

    /// Runs `body` in a fresh function frame whose locals are the given params. `frame_start` is
    /// moved past the enclosing locals so value reads do not cross into them, then restored.
    fn with_frame<R>(&mut self, params: &[HirParam], body: impl FnOnce(&mut Self) -> Result<R, anyhow::Error>) -> Result<R, anyhow::Error> {
        let saved_frame = self.frame_start;
        let mark = self.locals.len();
        self.frame_start = mark;
        for param in params {
            let name = self.ident_sym(&param.name);
            self.locals.push(Local::param(name, self.opt_set(param.nullable), param.mutable));
        }
        let result = body(self);
        self.locals.truncate(mark);
        self.frame_start = saved_frame;
        result
    }

    fn function(&mut self, decl: &HirFnDecl) -> Result<(), anyhow::Error> {
        let saved_seal = self.seal.suspend();
        // A lambda's shape is inferred, so it is not checked against a declaration.
        let saved_return = std::mem::replace(&mut self.current_return, match decl.ret {
            ReturnShape::Inferred => None,
            ret => Some(ret),
        });
        let result = self.with_frame(&decl.params, |c| {
            c.expr(&decl.body)?;
            // A non-null return must be produced on every path.
            if c.current_return == Some(ReturnShape::NonNull) && !c.definitely_returns(&decl.body) {
                return Err(c.error("This function can finish without returning a value; a '!' return must produce one on every path".to_string(), &decl.body));
            }
            Ok(())
        });
        self.current_return = saved_return;
        self.seal.restore(saved_seal);
        result
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

    fn function_stmt(&mut self, stmt: &HirId<HirStmt>) -> Result<(), anyhow::Error> {
        if let HirStmt::Fn(decl) = self.hir.get(stmt) {
            self.function(decl)?;
        }
        Ok(())
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

    fn identifier(&mut self, name: Symbol, expr: &HirId<HirExpr>) -> Result<Typed, anyhow::Error> {
        let Some(i) = self.frame_index_of(name) else {
            return Ok(Typed::unknown());
        };

        if self.locals[i].func.is_some() {
            return Ok(Typed::unknown());
        }

        if !self.locals[i].assigned && !self.locals[i].owed.contains(&self.sigs.opt) {
            return Err(self.error(format!("'{}' is used before it is assigned", self.hir.text(name)), expr));
        }

        let owed: HashSet<Symbol> = match self.narrowed.get(&NarrowKey::Local(i)) {
            Some(discharged) => self.locals[i].owed.difference(discharged).copied().collect(),
            None => self.locals[i].owed.clone(),
        };
        let flow = if owed.is_empty() {
            Flow::Clean
        } else {
            Flow::Bad { obligations: owed, definite: false }
        };

        Ok(Typed::of(flow, self.locals[i].tag.clone()))
    }

    /// Marks a node whose `unknown` value crosses into a non-null slot, so codegen guards it
    /// with a runtime null-check.
    fn add_barrier(&mut self, node: &HirId<HirExpr>) {
        self.barriers.insert(*node);
    }

    /// Classifies a value entering a non-null target. A non-null slot forbids `opt`, so only a
    /// value owing `opt` violates it.
    fn non_null_violation(&mut self, value: &Flow, target: &HirId<HirExpr>) -> Option<Violation> {
        match value {
            Flow::Clean => None,
            Flow::Unknown => { self.add_barrier(target); None },
            Flow::Void => Some(Violation::Void),
            Flow::Bad { obligations, definite } if obligations.contains(&self.sigs.opt) => {
                Some(if *definite { Violation::Null } else { Violation::Nullable })
            },
            Flow::Bad { .. } => None,
        }
    }

    /// Checks if moving a value into a slot is allowed per its nullability.
    fn check_into_slot(&mut self, flow: &Flow, slot_nullable: bool, name: Symbol, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let text = self.hir.text(name);
        let void = || format!("Cannot assign a void result to '{text}'; the call returns no value");
        if slot_nullable {
            return if flow.is_void() { Err(self.error(void(), node)) } else { Ok(()) };
        }
        match self.non_null_violation(flow, node) {
            None => Ok(()),
            Some(Violation::Void) => Err(self.error(void(), node)),
            Some(Violation::Null) => Err(self.error(format!("Cannot assign null to non-null binding '{text}'"), node)),
            Some(Violation::Nullable) => Err(self.error(format!("Cannot assign a nullable value to non-null binding '{text}'"), node)),
        }
    }

    /// Member or data access `target.member` / `target[member]`. Resolves a field on a known-type
    /// receiver to its declared nullability; any other access is a dynamic-boundary read.
    fn member_access(&mut self, target: &HirId<HirExpr>, member: &HirId<HirExpr>) -> Result<Typed, anyhow::Error> {
        let receiver = self.receiver(target)?;
        let Some(name) = self.member_text(member) else {
            // A computed `[expr]` access is a dynamic-boundary read.
            self.expr(member)?;
            return Ok(Typed::unknown());
        };
        if matches!(receiver.tag, TypeTag::SelfType) {
            return self.trait_member(name, member);
        }
        // A member name never interned as an identifier names no declared member.
        let Some(field) = self.hir.symbol_of(name) else { return Ok(Typed::unknown()) };
        let on_this = matches!(self.hir.get(target), HirExpr::This);
        let key = self.narrowable_field_key(target, field);
        if let TypeTag::Concrete(type_name) = &receiver.tag {
            if let Some(layout) = self.layout_of(*type_name) {
                if let Some(member_kind) = layout.members.get(&field).copied() {
                    let flow = match member_kind {
                        TypeMember::Field(_) => {
                            // Inside init, a non-null field read before its assignment is unsound.
                            if on_this && !layout.is_nullable(field) && self.seal.reads_before_assign(field) {
                                return Err(self.error(format!("Field '{}' is read before it is assigned in init", self.hir.text(field)), member));
                            }
                            let narrowed = key.is_some_and(|k| self.discharged(&k, self.sigs.opt));
                            if layout.is_nullable(field) && !narrowed { self.opt_flow(false) } else { Flow::Clean }
                        },
                        // A method reference is a non-null value.
                        TypeMember::Method(_) => Flow::Clean,
                    };
                    return Ok(Typed::of(flow, TypeTag::Unknown));
                }
            }
        }
        Ok(Typed::unknown())
    }

    /// Evaluates a receiver, requiring it to be non-null.
    fn receiver(&mut self, receiver: &HirId<HirExpr>) -> Result<Typed, anyhow::Error> {
        if matches!(self.hir.get(receiver), HirExpr::This) {
            self.seal.set_this_seen(true);
            return Ok(self.this_typed());
        }
        let typed = self.expr(receiver)?;
        // A value confirmed to be a witness it owes is usable by that type, even while it owes.
        if self.confirmed_witness(&typed) {
            return Ok(typed);
        }
        self.require_usable_value(&typed, receiver)?;
        Ok(typed)
    }

    fn binary(&mut self, op: BinOp, l: &HirId<HirExpr>, r: &HirId<HirExpr>) -> Result<Typed, anyhow::Error> {
        match op {
            // Short-circuit operators narrow their left operand into the right operand. `and`
            // narrows where the left holds (true), `or` where it fails (false).
            BinOp::And | BinOp::Or => {
                self.expr(l)?;
                let narrow = self.narrowings(l, matches!(op, BinOp::And));
                self.narrow_branch(&narrow, |c| c.expr(r))?;
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
                let rn = self.expr(r)?;
                // A confirmed-witness operand makes the operation invalid whatever the other side
                // is, so name both operand types like the runtime's operand error does.
                if self.confirmed_witness(&ln) || self.confirmed_witness(&rn) {
                    return Err(self.invalid_operands(op, l, &ln, r, &rn));
                }
                self.require_usable_value(&ln, l)?;
                self.require_usable_value(&rn, r)?;
                Ok(Typed::nonnull())
            },
        }
    }

    fn unary(&mut self, op: UnOp, x: &HirId<HirExpr>) -> Result<Typed, anyhow::Error> {
        let typed = self.expr(x)?;
        // `!` is a boolean context; negation and bitwise-not require a value.
        if matches!(op, UnOp::Negate | UnOp::BitNot) {
            if let Some(witness) = self.confirmed_witness_name(&typed) {
                return Err(self.confirmed_use_error(format!("invalid operand of `{op}`: {witness}"), x, witness));
            }
            self.require_usable_value(&typed, x)?;
        }
        Ok(Typed::nonnull())
    }

    /// A single-caret error for a confirmed witness used where its type is not allowed. Each
    /// operation site names itself, since a confirmed witness is not fixed by narrowing.
    fn confirmed_use_error(&self, header: String, operand: &HirId<HirExpr>, witness: &str) -> anyhow::Error {
        anyhow!("{}", Diagnostic::new(header, self.hir.pos(operand).clone()).with_label(witness.to_string()))
    }

    /// Rejects an operand that cannot be used as a value here: a void result, or one still owing an
    /// obligation. A confirmed witness is caught by its operation site first, so this only advises
    /// narrowing a value that merely may be a witness.
    fn require_usable_value(&self, typed: &Typed, operand: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        debug_assert!(!self.confirmed_witness(typed), "a confirmed witness must be handled at its operation site, not advised to narrow");
        if typed.flow.is_void() {
            return Err(self.error("This call returns no value, so its result cannot be used here".to_string(), operand));
        }

        let Flow::Bad { obligations, .. } = &typed.flow else { return Ok(()) };

        let name = match self.hir.get(operand) {
            HirExpr::Identifier(name) => Some(self.hir.text(*name)),
            _ => None,
        };

        let mut owed: Vec<&str> = obligations.iter().map(|o| self.hir.text(*o)).collect();
        owed.sort();
        let owed = owed.iter().map(|o| format!("'{o}'")).collect::<Vec<_>>().join(", ");

        // The header stays generic so it is easy to search for. The caret and help carry the name.
        let mut diagnostic = Diagnostic::new(format!("unchecked value owes {owed}"), self.hir.pos(operand).clone());

        // Name the witness so the reader knows what to rule out, and how.
        let mut witnesses: Vec<&str> = obligations.iter().filter_map(|o| self.witness_name(*o)).collect();
        witnesses.sort();
        witnesses.dedup();

        if witnesses.is_empty() {
            diagnostic = diagnostic.with_help("discharge it before use");
        } else {
            let witness = witnesses.join(" or ");
            diagnostic = diagnostic.with_label(format!("might be {witness}"));
            diagnostic = match name {
                Some(name) => diagnostic.with_help(format!("make sure `{name}` is not {witness} before using it")),
                None => diagnostic.with_help(format!("make sure the value is not {witness} before using it")),
            };
        }

        Err(anyhow!("{}", diagnostic))
    }

    fn invalid_operands(&self, op: BinOp, l: &HirId<HirExpr>, ln: &Typed, r: &HirId<HirExpr>, rn: &Typed) -> anyhow::Error {
        let (lt, rt) = (self.operand_type_name(ln, l), self.operand_type_name(rn, r));
        let header = format!("invalid operands of `{op}`: {lt} and {rt}");
        // Point the primary caret at the confirmed operand; the other side is a labeled span.
        let (primary, primary_ty, other, other_ty) = if self.confirmed_witness(ln) {
            (l, &lt, r, &rt)
        } else {
            (r, &rt, l, &lt)
        };
        anyhow!("{}", Diagnostic::new(header, self.hir.pos(primary).clone())
            .with_label(primary_ty.to_string())
            .with_span(self.hir.pos(other).clone(), other_ty.to_string()))
    }

    fn operand_type_name(&self, typed: &Typed, node: &HirId<HirExpr>) -> String {
        if let Some(witness) = self.confirmed_witness_name(typed) {
            return witness.to_string();
        }
        match self.hir.get(node) {
            HirExpr::Literal(HirLiteral::Number(_)) => "number".to_string(),
            HirExpr::Literal(HirLiteral::String(_)) => "string".to_string(),
            HirExpr::Literal(HirLiteral::Boolean(_)) => "boolean".to_string(),
            HirExpr::Literal(HirLiteral::Null) => "null".to_string(),
            _ => match &typed.tag {
                TypeTag::Concrete(name) => self.hir.text(*name).to_string(),
                _ => "value".to_string(),
            },
        }
    }

    /// The type or trait that witnesses an obligation at runtime, if it has one. A pure-static
    /// obligation has no object witness to name.
    fn witness_name(&self, obligation: Symbol) -> Option<&'a str> {
        match self.sigs.witness(obligation)? {
            Witness::Type(name) | Witness::Trait(name) => Some(self.hir.text(*name)),
            Witness::Null => Some("null"),
        }
    }

    /// The witnesses a match arm rules out for the arms below it.
    fn arm_rules_out(&self, arm: &HirMatchArm, remaining: &HashSet<Symbol>) -> HashSet<Symbol> {
        if let Some(guard) = &arm.guard {
            // For now just require a literal `true` guard to rule out witnesses. A more general analysis could
            // evaluate the guard's flow facts and see if it is total over a witness.
            if !self.is_literal_true(guard) {
                return HashSet::new();
            }
        }
        remaining.iter().copied().filter(|w| self.matcher_total_over(&arm.matcher, *w)).collect()
    }

    /// Whether a matcher matches every value in a witness's bad state.
    fn matcher_total_over(&self, matcher: &HirMatcher, witness: Symbol) -> bool {
        self.sigs.witness(witness).is_some_and(|w| self.total_over(matcher, w))
    }

    /// Whether a matcher matches every value the witness names. The `|`/`&`/`as` combinators recurse
    /// the same way for any witness; only the leaf test differs. A bare `is W` is always total. A
    /// destructure is total only for a type witness, and only when every named field is public and
    /// binds irrefutably. A trait destructure tests a provider's public surface, which a real
    /// witness can lack, so it is never total.
    fn total_over(&self, matcher: &HirMatcher, witness: &Witness) -> bool {
        match matcher {
            HirMatcher::As(_, inner) => self.total_over(inner, witness),
            HirMatcher::Or(alternatives) => alternatives.iter().any(|m| self.total_over(m, witness)),
            HirMatcher::And(parts) => parts.iter().all(|m| self.total_over(m, witness)),
            HirMatcher::Literal(HirLiteral::Null) => matches!(witness, Witness::Null),
            HirMatcher::Type { name: tested, shape, .. } => match witness {
                Witness::Type(name) if tested == name => shape.as_ref().is_none_or(|s| self.destructure_total(*name, s)),
                Witness::Trait(name) => tested == name && shape.is_none(),
                _ => false,
            },
            _ => false,
        }
    }

    /// Whether an `is Type { ... }` destructure matches every value of the type: every named field
    /// is public and binds irrefutably.
    fn destructure_total(&self, type_name: Symbol, shape: &HirMatcher) -> bool {
        let HirMatcher::Shape(fields) = shape else { return false };
        fields.iter().all(|field| {
            self.is_public_field(type_name, &field.key)
                && matches!(field.value, HirMatcher::Binder(_) | HirMatcher::Wildcard)
        })
    }

    /// Whether `key` names a public field of the type. A built-in witness type carries no layout,
    /// so its public surface is answered directly.
    fn is_public_field(&self, type_name: Symbol, key: &HirLiteral) -> bool {
        let HirLiteral::String(field) = key else { return false };
        match self.layout_of(type_name) {
            Some(layout) => match self.hir.symbol_of(field) {
                Some(field) => matches!(layout.members.get(&field), Some(TypeMember::Field(_))) && layout.is_public(field),
                None => false,
            },
            None => builtin_public_field(self.hir.text(type_name), field),
        }
    }

    fn is_literal_true(&self, guard: &HirId<HirExpr>) -> bool {
        matches!(self.hir.get(guard), HirExpr::Literal(HirLiteral::Boolean(true)))
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

/// The public fields of a built-in witness type, which carries no layout. `Err` exposes `value`.
fn builtin_public_field(type_name: &str, field: &str) -> bool {
    matches!((type_name, field), ("Err", "value"))
}

/// The names a matcher binds to the whole matched value: a top-level binder or an `as` name. A
/// shape, array, or type destructure binds sub-values, which are clean payloads.
fn whole_value_binders(matcher: &HirMatcher) -> Vec<Symbol> {
    let mut out = Vec::new();
    collect_whole_value_binders(matcher, &mut out);
    out
}

fn collect_whole_value_binders(matcher: &HirMatcher, out: &mut Vec<Symbol>) {
    match matcher {
        HirMatcher::Binder(name) => out.push(*name),
        HirMatcher::As(name, inner) => { out.push(*name); collect_whole_value_binders(inner, out); },
        HirMatcher::And(parts) => for part in parts { collect_whole_value_binders(part, out); },
        // Alternatives bind the same names, so the first one stands for all.
        HirMatcher::Or(alternatives) => if let Some(first) = alternatives.first() { collect_whole_value_binders(first, out); },
        _ => {},
    }
}


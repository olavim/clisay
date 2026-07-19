//! Flow-sensitive semantic checks.

mod assign;
mod barriers;
mod call;
mod construct;
mod matching;
mod narrow;
mod native;
mod returns;
mod traits;
mod values;
mod witness;

use std::collections::{HashMap, HashSet};

use anyhow::anyhow;

use crate::frontend::lex::Diagnostic;

use crate::core::objects::TypeMember;
use crate::middle::bind::{Bindings, TypeLayout};
use crate::middle::signatures::{Mutability, Signatures, TypeTag};
use self::construct::Seal;
use self::matching::whole_value_binders;
use crate::middle::hir::{BinOp, Hir, HirExpr, HirFnDecl, HirId, HirLiteral, HirParam, HirSlotClause, HirStmt, HirTypeDecl, ReturnShape, Symbol, UnOp};

pub use barriers::{Barrier, Barriers, WitnessSet};

pub fn check(hir: &Hir, bindings: &Bindings, sigs: &Signatures) -> Result<Barriers, anyhow::Error> {
    let mut checker = Checker::new(hir, bindings, sigs);
    checker.stmt(&hir.get_root())?;
    let witness_names = sigs.object_witnesses().map(|(_, name)| name).collect();
    Ok(Barriers {
        null_barriers: checker.barriers,
        boundary_barriers: checker.boundary_barriers,
        witness_tests: checker.witness_tests,
        witness_names,
    })
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
    /// opposed to one that only may be. `container` marks an array or dict whose elements owe
    /// the obligations, so a read of it yields a pending element.
    Bad { obligations: HashSet<Symbol>, definite: bool, container: bool },
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
    mutability: Mutability,
}

impl Typed {
    fn unknown() -> Typed { Typed { flow: Flow::Unknown, tag: TypeTag::Unknown, mutability: Mutability::Unknown } }
    fn nonnull() -> Typed { Typed { flow: Flow::Clean, tag: TypeTag::Unknown, mutability: Mutability::Unknown } }
    fn of(flow: Flow, tag: TypeTag) -> Typed { Typed { flow, tag, mutability: Mutability::Unknown } }
    fn with_mutability(mut self, mutability: Mutability) -> Typed { self.mutability = mutability; self }
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
    /// Whether the binding holds a container whose elements owe `owed`.
    container: bool,
    /// The value-mutability of the value in the slot.
    mutability: Mutability,
}

impl Local {
    fn param(name: Symbol, owed: HashSet<Symbol>, mutable: bool) -> Local {
        Local { name, owed, mutable, assigned: true, tag: TypeTag::Unknown, func: None, binder: false, container: false, mutability: Mutability::Unknown }
    }

    fn catch(name: Symbol, owed: HashSet<Symbol>, mutable: bool) -> Local {
        Local { name, owed, mutable, assigned: true, tag: TypeTag::Unknown, func: None, binder: false, container: false, mutability: Mutability::Unknown }
    }

    fn binder(name: Symbol) -> Local {
        Local { name, owed: HashSet::new(), mutable: false, assigned: true, tag: TypeTag::Unknown, func: None, binder: true, container: false, mutability: Mutability::Unknown }
    }

    fn binder_owing(name: Symbol, owed: HashSet<Symbol>) -> Local {
        Local { name, owed, mutable: false, assigned: true, tag: TypeTag::Unknown, func: None, binder: true, container: false, mutability: Mutability::Unknown }
    }

    fn func(name: Symbol, stmt: HirId<HirStmt>) -> Local {
        Local { name, owed: HashSet::new(), mutable: false, assigned: true, tag: TypeTag::Unknown, func: Some(stmt), binder: false, container: false, mutability: Mutability::Unknown }
    }

    fn value(name: Symbol, owed: HashSet<Symbol>, mutable: bool, assigned: bool, tag: TypeTag) -> Local {
        Local { name, owed, mutable, assigned, tag, func: None, binder: false, container: false, mutability: Mutability::Unknown }
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
    mutability: Mutability,
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
    /// Whether the current function's return declares any obligation.
    current_return_owes: bool,
    /// Whether the current function has no return marker.
    current_return_unmarked: bool,
    /// Whether the current function's return is declared `: mut`.
    current_return_mut: bool,
    /// The null fast path: a `!` operand owing only `opt`, needing just the null assertion.
    barriers: HashSet<HirId<HirExpr>>,
    /// Nodes where an unknown value needs the boundary guard: a slot crossing or a `!` operand.
    boundary_barriers: HashMap<HirId<HirExpr>, Barrier>,
    /// Discharge nodes whose operand owes an object witness.
    witness_tests: HashMap<HirId<HirExpr>, WitnessSet>,
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
            current_return_owes: false,
            current_return_unmarked: false,
            current_return_mut: false,
            barriers: HashSet::new(),
            boundary_barriers: HashMap::new(),
            witness_tests: HashMap::new(),
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
        Flow::Bad { obligations: HashSet::from([self.sigs.opt]), definite, container: false }
    }

    fn opt_set(&self, nullable: bool) -> HashSet<Symbol> {
        if nullable { HashSet::from([self.sigs.opt]) } else { HashSet::new() }
    }

    /// The obligations a slot's `:` clause declares.
    fn clause_owed(&self, clause: &HirSlotClause) -> HashSet<Symbol> {
        clause.names.iter().copied().collect()
    }

    fn stmt(&mut self, stmt: &HirId<HirStmt>) -> Result<(), anyhow::Error> {
        match self.hir.get(stmt) {
            HirStmt::Nop => {},
            HirStmt::Fn(decl) => {
                // Register the name first so the body may call itself.
                self.locals.push(Local::func(decl.name, *stmt));
                self.function(Some(*stmt), decl)?;
            },
            HirStmt::Type(decl) => self.type_decl(stmt, Some(decl.name), decl)?,
            HirStmt::Trait(decl) => self.type_decl(stmt, None, decl)?,
            HirStmt::Say(field) => self.say(field.name, &field.clause, field.mutable, &field.value)?,
            HirStmt::Expression(e) | HirStmt::Block(e) => { self.expr(e)?; },
            HirStmt::Return(opt) => match opt {
                Some(e) => {
                    let typed = self.expr(e)?;
                    self.check_return_mutability(&typed, e)?;
                    if let Some(shape) = self.current_return {
                        self.check_return(&typed.flow, shape, e)?;
                    }
                },
                // A `!` function falls back to null on a bare return, which it may not.
                None if self.current_return == Some(ReturnShape::NonNull) => {
                    return Err(self.error("A '!' function must return a value, but this 'return' yields null".to_string(), stmt));
                },
                None => {},
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
            HirExpr::Mut(inner) => self.expr(inner)?.with_mutability(Mutability::Mutable),
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
                if self.owes_object_witness(&left.flow) { self.record_witness_test(expr, &left.flow); }
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
                if self.owes_object_witness(&typed.flow) { self.record_witness_test(expr, &typed.flow); }
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
                if self.owes_object_witness(&typed.flow) {
                    self.record_witness_test(expr, &typed.flow);
                } else if matches!(typed.flow, Flow::Unknown) {
                    // An unknown value could be any witness, so `!` must assert against them all.
                    self.record_boundary_barrier(expr, &HashSet::new());
                } else if !matches!(typed.flow, Flow::Clean) {
                    self.add_barrier(expr);
                }
                Typed::of(Flow::Clean, typed.tag)
            },
        })
    }

    fn say(&mut self, name: Symbol, clause: &HirSlotClause, mutable: bool, value: &Option<HirId<HirExpr>>) -> Result<(), anyhow::Error> {
        let owed = self.clause_owed(clause);
        let (assigned, tag, mutability) = if let Some(value) = value {
            self.reject_this_store(value)?;
            let typed = self.expr(value)?;
            self.check_into_slot(&typed.flow, &owed, name, value)?;
            (true, typed.tag, typed.mutability)
        } else {
            (false, TypeTag::Unknown, Mutability::Unknown)
        };
        let mut local = Local::value(name, owed, mutable, assigned, tag);
        local.container = clause.container;
        local.mutability = mutability;
        self.locals.push(local);
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
            let mut local = Local::param(name, self.clause_owed(&param.clause), param.mutable);
            local.container = param.clause.container;
            local.mutability = Mutability::param(param.clause.capability);
            self.locals.push(local);
        }
        let result = body(self);
        self.locals.truncate(mark);
        self.frame_start = saved_frame;
        result
    }

    fn function(&mut self, stmt: Option<HirId<HirStmt>>, decl: &HirFnDecl) -> Result<(), anyhow::Error> {
        // An unmarked return is inferred whole from the body. When it can both finish with no value
        // and return a bad value, the mixed shape must be named, not inferred.
        let unmarked = decl.is_unmarked();
        if unmarked {
            if let Some(ret) = stmt.and_then(|s| self.sigs.fns.get(&s))
                .map(|s| &s.ret).filter(|r| r.void && !r.obligations.is_empty())
            {
                return Err(self.mixed_void_error(decl, &ret.obligations));
            }
        }
        let saved_seal = self.seal.suspend();
        // A lambda's shape is inferred, so it is not checked against a declaration.
        let saved_return = std::mem::replace(&mut self.current_return, match decl.ret {
            ReturnShape::Inferred => None,
            ret => Some(ret),
        });
        let prev_owes = std::mem::replace(&mut self.current_return_owes, !decl.clause.names.is_empty());
        let prev_unmarked = std::mem::replace(&mut self.current_return_unmarked, unmarked);
        let prev_mut = std::mem::replace(&mut self.current_return_mut, decl.clause.capability.is_mut());
        let result = self.with_frame(&decl.params, |c| {
            c.expr(&decl.body)?;
            // A non-null return must be produced on every path.
            if c.current_return == Some(ReturnShape::NonNull) && !c.hir.definitely_returns(&decl.body) {
                return Err(c.error("This function can finish without returning a value; a '!' return must produce one on every path".to_string(), &decl.body));
            }
            Ok(())
        });
        self.current_return = saved_return;
        self.current_return_owes = prev_owes;
        self.current_return_unmarked = prev_unmarked;
        self.current_return_mut = prev_mut;
        self.seal.restore(saved_seal);
        result
    }

    /// An unmarked function that returns a bad value on one path and nothing on another owes a shape
    /// the compiler will not infer silently. The message names the annotation that makes it explicit.
    fn mixed_void_error(&self, decl: &HirFnDecl, obligations: &HashSet<Symbol>) -> anyhow::Error {
        let name = self.hir.text(decl.name);
        let list = self.quoted_obligation_list(obligations);
        // The annotation spells the obligations as clause atoms: `: void opt fails`.
        let mut names: Vec<&str> = obligations.iter().map(|o| self.hir.text(*o)).collect();
        names.sort();
        let annotation = format!(": void {}", names.join(" "));
        self.error_help(
            format!("'{name}' returns a value owing {list} on some paths and no value on others"),
            &decl.body,
            format!("annotate its return '{annotation}', or return a value on every path"),
        )
    }

    /// The obligations sorted and quoted for a diagnostic, like `'fails', 'opt'`.
    fn quoted_obligation_list(&self, obligations: &HashSet<Symbol>) -> String {
        let mut names: Vec<&str> = obligations.iter().map(|o| self.hir.text(*o)).collect();
        names.sort();
        names.iter().map(|o| format!("'{o}'")).collect::<Vec<_>>().join(", ")
    }

    fn type_decl(&mut self, node: &HirId<HirStmt>, type_name: Option<Symbol>, decl: &HirTypeDecl) -> Result<(), anyhow::Error> {
        let saved_type = self.current_type;
        let saved_surface = self.current_trait_surface.take();
        self.current_type = type_name;
        if let Some(type_name) = type_name {
            self.check_field_definitions(type_name, node)?;
            self.check_method_overrides(decl)?;
            self.check_req_conformance(decl)?;
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
            self.function(Some(*stmt), decl)?;
        }
        Ok(())
    }

    fn literal_children(&mut self, lit: &HirLiteral, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        match lit {
            // Putting a value into a container persists it, which a `discharge to escape` value forbids.
            HirLiteral::Array(elems) => for e in elems {
                let t = self.expr(e)?;
                self.reject_escape(&t.flow, e)?;
            },
            HirLiteral::Dict(pairs) => for (k, v) in pairs {
                self.expr(k)?;
                let t = self.expr(v)?;
                self.reject_escape(&t.flow, v)?;
            },
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
            Flow::Bad { obligations: owed, definite: false, container: self.locals[i].container }
        };

        Ok(Typed::of(flow, self.locals[i].tag.clone()).with_mutability(self.locals[i].mutability))
    }

    /// Member or data access `target.member` / `target[member]`. Resolves a field on a known-type
    /// receiver to its declared nullability; any other access is a dynamic-boundary read.
    fn member_access(&mut self, target: &HirId<HirExpr>, member: &HirId<HirExpr>) -> Result<Typed, anyhow::Error> {
        let receiver = self.receiver(target)?;
        let Some(name) = self.member_text(member) else {
            self.expr(member)?;
            // Reading a container yields a pending element. Presence is tracked, not depth, so the
            // read stays a container.
            if let Flow::Bad { obligations, container: true, .. } = &receiver.flow {
                return Ok(Typed::of(Flow::Bad { obligations: obligations.clone(), definite: false, container: true }, TypeTag::Unknown));
            }
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
        // A container is indexable and its methods callable even while it owes element obligations.
        if Self::is_container(&typed.flow) {
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


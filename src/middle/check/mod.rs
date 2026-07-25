//! Flow-sensitive semantic checks.

mod assign;
mod barriers;
mod call;
mod construct;
mod matching;
mod narrow;
mod returns;
mod traits;
mod values;
mod witness;

use std::collections::{HashMap, HashSet};

use anyhow::anyhow;

use crate::frontend::lex::{Diagnostic, SourcePosition};

use crate::core::objects::TypeMember;
use crate::middle::bind::{Bindings, TypeLayout};
use crate::middle::signatures::{Mutability, Signatures, TypeTag};
use self::construct::Seal;
use self::matching::whole_value_binders;
use crate::middle::hir::{BinOp, Capability, Hir, HirExpr, HirFnDecl, HirId, HirLiteral, HirParam, HirSlotClause, HirStmt, HirTypeDecl, ReturnShape, Symbol, UnOp};

pub use barriers::{Barrier, Barriers, WitnessSet};

pub fn check(hir: &Hir, bindings: &Bindings, sigs: &Signatures) -> Result<Barriers, anyhow::Error> {
    let mut checker = Checker::new(hir, bindings, sigs);
    checker.stmt(&hir.get_root())?;
    let witness_names = sigs.object_witnesses().map(|(_, name)| name).collect();
    Ok(Barriers {
        null_barriers: checker.barriers,
        boundary_barriers: checker.boundary_barriers,
        witness_tests: checker.witness_tests,
        survive_barriers: checker.survive_barriers,
        borrow_marks: checker.borrow_marks,
        witness_names,
        seal_checks: checker.seal_checks,
        deep_seals: checker.deep_seals,
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

/// Why a mutable binding was moved.
#[derive(Clone, Copy)]
enum MoveCause {
    /// Bound, stored, returned, or passed to a consuming parameter.
    Value,
    /// Captured by a closure that writes the value. Carries the closure's name.
    Capture(Symbol),
}

/// Where and why a mutable binding was moved out.
#[derive(Clone, Copy)]
struct MovedAt {
    node: HirId<HirExpr>,
    cause: MoveCause,
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
    /// Whether the binding is a function parameter.
    param: bool,
    borrowed: bool,
    /// Where the mutable value was moved out, or `None` while the binding is live.
    move_site: Option<MovedAt>,
    /// The prior slots this binding took its mutable value from. They stay dead while it holds the
    /// value. The nearest survivor becomes the live holder again if it dies still holding it. A
    /// binding with sources is escape-tracked.
    provenance: Vec<usize>,
}

impl Local {
    /// A binding with every fact at its neutral default. Each named constructor overrides only the
    /// fields that distinguish it, so a new field is added here once.
    fn base(name: Symbol) -> Local {
        Local { name, owed: HashSet::new(), mutable: false, assigned: true, tag: TypeTag::Unknown, func: None, binder: false, container: false, mutability: Mutability::Unknown, param: false, borrowed: false, move_site: None, provenance: Vec::new() }
    }

    fn param(name: Symbol, owed: HashSet<Symbol>, mutable: bool) -> Local {
        Local { owed, mutable, ..Local::base(name) }
    }

    fn catch(name: Symbol, owed: HashSet<Symbol>, mutable: bool) -> Local {
        Local::param(name, owed, mutable)
    }

    fn binder(name: Symbol) -> Local {
        Local { binder: true, ..Local::base(name) }
    }

    fn binder_owing(name: Symbol, owed: HashSet<Symbol>) -> Local {
        Local { owed, binder: true, ..Local::base(name) }
    }

    fn func(name: Symbol, stmt: HirId<HirStmt>) -> Local {
        Local { func: Some(stmt), ..Local::base(name) }
    }

    fn value(name: Symbol, owed: HashSet<Symbol>, mutable: bool, assigned: bool, tag: TypeTag) -> Local {
        Local { owed, mutable, assigned, tag, ..Local::base(name) }
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
    move_site: Option<MovedAt>,
    provenance: Vec<usize>,
}

/// A snapshot of flow facts that branches widen back at a join.
#[derive(Clone)]
struct FlowSnapshot {
    locals: Vec<LocalFlow>,
    narrowed: HashMap<NarrowKey, HashSet<Symbol>>,
}

/// The declared facts of the function currently being checked.
#[derive(Default)]
struct FnContext<'a> {
    /// The declared return shape. `Inferred` marks a lambda or the program root.
    return_shape: ReturnShape,
    /// Whether the return declares any obligation.
    return_owes: bool,
    /// Whether the function has no return marker.
    return_unmarked: bool,
    /// Whether the return is declared `: mut`.
    return_mut: bool,
    /// The function's name.
    name: Option<Symbol>,
    /// The return-clause span.
    return_clause: Option<SourcePosition>,
    /// The parameters as `(name, span)`.
    params: Vec<(Symbol, SourcePosition)>,
    /// The names this body writes, so a read-only capture is told from a writing one.
    writes: Option<&'a HashSet<Symbol>>,
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
    /// The function currently being checked.
    fn_ctx: FnContext<'a>,
    /// The null fast path: a `!` operand owing only `opt`, needing just the null assertion.
    barriers: HashSet<HirId<HirExpr>>,
    /// Nodes where an unknown value needs the boundary guard: a slot crossing or a `!` operand.
    boundary_barriers: HashMap<HirId<HirExpr>, Barrier>,
    /// Discharge nodes whose operand owes an object witness.
    witness_tests: HashMap<HirId<HirExpr>, WitnessSet>,
    /// Opaque calls whose argument must survive, keyed by callee node to the argument positions
    /// that need the callee to borrow rather than consume.
    survive_barriers: HashMap<HirId<HirExpr>, Vec<u8>>,
    /// Calls that lend a mutable argument, keyed by callee node to the param positions to mark as borrowed.
    borrow_marks: HashMap<HirId<HirExpr>, Vec<u8>>,
    /// Immutable container literals with an unknown-capability element, needing a runtime seal-check.
    seal_checks: HashSet<HirId<HirExpr>>,
    /// Plain constructions whose fields deep-freeze once sealed.
    deep_seals: HashSet<HirId<HirExpr>>,
    /// Set while checking the construction directly under a `mut`, so it is not recorded as a plain
    /// (deep-sealing) construction.
    mut_construction: bool,
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
            fn_ctx: FnContext::default(),
            barriers: HashSet::new(),
            boundary_barriers: HashMap::new(),
            witness_tests: HashMap::new(),
            survive_barriers: HashMap::new(),
            borrow_marks: HashMap::new(),
            seal_checks: HashSet::new(),
            deep_seals: HashSet::new(),
            mut_construction: false,
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

    /// An error with a label on its own caret.
    fn error_labeled<T>(&self, msg: String, node: &HirId<T>, label: impl Into<String>) -> anyhow::Error {
        anyhow!("{}", Diagnostic::new(msg, self.hir.pos(node).clone()).with_label(label))
    }

    /// An error with a primary caret at `primary` and a context caret at `site`.
    fn error_ctx(&self, msg: impl Into<String>, primary: &SourcePosition, label: impl Into<String>, site: &SourcePosition, site_label: impl Into<String>) -> anyhow::Error {
        anyhow!("{}", Diagnostic::new(msg, primary.clone())
            .with_label(label)
            .with_context_span(site.clone(), site_label))
    }

    /// A context-caret error that also carries a `help:` note.
    fn error_ctx_help(&self, msg: impl Into<String>, primary: &SourcePosition, label: impl Into<String>, site: &SourcePosition, site_label: impl Into<String>, help: impl Into<String>) -> anyhow::Error {
        anyhow!("{}", Diagnostic::new(msg, primary.clone())
            .with_label(label)
            .with_context_span(site.clone(), site_label)
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

    /// Moves an enclosing-frame mutable binding when a nested function writes it. A read-only
    /// capture borrows the value, so the enclosing binding stays live.
    fn capture_enclosing(&mut self, name: Symbol, node: &HirId<HirExpr>) {
        let Some(i) = self.locals[..self.frame_start].iter().rposition(|l| l.name == name) else { return };
        if !self.fn_ctx.writes.is_some_and(|w| w.contains(&name)) {
            return;
        }
        let cause = self.fn_ctx.name.map_or(MoveCause::Value, MoveCause::Capture);
        let local = &mut self.locals[i];
        if local.func.is_none() && local.mutability == Mutability::Mutable && local.move_site.is_none() {
            local.move_site = Some(MovedAt { node: *node, cause });
        }
    }

    fn use_after_move_error(&self, name: Symbol, use_site: &HirId<HirExpr>, moved: MovedAt) -> anyhow::Error {
        let text = self.hir.text(name);
        match moved.cause {
            MoveCause::Value => {
                // A loop's re-check reads the value at the very node that moved it, so one caret is
                // clearer than two on the same spot.
                if *use_site == moved.node {
                    return self.loop_move_error(name, moved);
                }
                // Caret both the use and the move so the reader sees where the value went.
                anyhow!("{}", Diagnostic::new("value used after it was moved".to_string(), self.hir.pos(use_site).clone())
                    .with_label(format!("`{text}` used here"))
                    .with_span(self.hir.pos(&moved.node).clone(), format!("`{text}` moved here")))
            },
            MoveCause::Capture(captor) => {
                let who = if self.hir.text(captor) == "lambda" { "a closure".to_string() } else { format!("closure `{}`", self.hir.text(captor)) };
                anyhow!("{}", Diagnostic::new("value used after it was moved into a closure".to_string(), self.hir.pos(use_site).clone())
                    .with_label(format!("`{text}` used here"))
                    .with_span(self.hir.pos(&moved.node).clone(), format!("`{text}` is written here, which moves it into {who}"))
                    .with_help(format!("a closure that writes a captured value takes ownership of it; to keep using `{text}`, pass it to a `mut` function parameter instead of capturing it, or `copy` it into the closure")))
            },
        }
    }

    /// Moving a value reads it, so a loop body that moves one reads a moved value on the next pass.
    /// The read and the move are the same spot, so a single caret marks it.
    fn loop_move_error(&self, name: Symbol, moved: MovedAt) -> anyhow::Error {
        let text = self.hir.text(name);
        anyhow!("{}", Diagnostic::new("value used after it was moved".to_string(), self.hir.pos(&moved.node).clone())
            .with_label(format!("`{text}` is moved here, then read again on the next loop iteration")))
    }

    /// When a block-local dies still holding its moved value, hands the value back to a surviving
    /// source. A local moved out on some path holds nothing, so it hands back nothing. The nearest
    /// source that outlives the block becomes the live holder again, along every path, past cycles.
    fn revive_scoped_sources(&mut self, mark: usize) {
        for i in (mark..self.locals.len()).rev() {
            if self.locals[i].move_site.is_some() {
                continue;
            }
            let mut stack = self.locals[i].provenance.clone();
            let mut seen: HashSet<usize> = HashSet::new();
            while let Some(s) = stack.pop() {
                if !seen.insert(s) {
                    continue;
                }
                if s < mark {
                    self.locals[s].move_site = None;
                } else {
                    stack.extend(self.locals[s].provenance.iter().copied());
                }
            }
        }
    }

    /// Whether local `i` holds a mutable value: it owns or borrows one directly, or took one from
    /// other sources. These are the slots move tracking follows.
    fn holds_mutable(&self, i: usize) -> bool {
        self.locals[i].mutability == Mutability::Mutable || !self.locals[i].provenance.is_empty()
    }

    /// The mutable-value holders a value expression reaches, each with the node to blame.
    fn reachable_sources(&self, node: &HirId<HirExpr>, out: &mut Vec<(usize, HirId<HirExpr>)>) {
        match self.hir.get(node) {
            HirExpr::Identifier(name) => {
                if let Some(i) = self.frame_index_of(*name) {
                    if self.holds_mutable(i) {
                        out.push((i, *node));
                    }
                }
            },
            // A brace also persists its field values into the new instance, so each escapes.
            HirExpr::Construct(callee, args, brace) => {
                self.reachable_init_args(callee, args, out);
                for (_, v) in brace { self.reachable_sources(v, out); }
            },
            HirExpr::Call(callee, args) => self.reachable_init_args(callee, args, out),
            _ => for c in self.hir.ownership_children(node) { self.reachable_sources(&c, out); },
        }
    }

    /// The sources a call's arguments keep reachable, when the callee is a constructor. Only the
    /// arguments its init persists escape into the instance; the rest are borrowed. A non-constructor
    /// call yields a fresh value, so it reaches none.
    fn reachable_init_args(&self, callee: &HirId<HirExpr>, args: &[HirId<HirExpr>], out: &mut Vec<(usize, HirId<HirExpr>)>) {
        let Some(init) = self.constructor_init(callee) else { return };
        for (i, a) in args.iter().enumerate() {
            if self.sigs.param_escapes_at(&init, i) {
                self.reachable_sources(a, out);
            }
        }
    }

    /// The init of the type a callee names.
    fn constructor_init(&self, callee: &HirId<HirExpr>) -> Option<HirId<HirStmt>> {
        let type_name = self.sigs.type_named(self.hir, callee)?;
        let type_stmt = self.sigs.types_by_name.get(&type_name)?;
        let HirStmt::Type(decl) = self.hir.get(type_stmt) else { return None };
        Some(decl.init)
    }

    /// Whether a type has a factory. A factory-less type (not all-defaulted, no `init`) is built
    /// only by brace, so `T(..)` cannot construct it.
    fn type_has_factory(&self, name: Symbol) -> bool {
        self.sigs.types_by_name.get(&name).is_some_and(|stmt| {
            matches!(self.hir.get(stmt), HirStmt::Type(decl) if matches!(self.hir.get(&decl.init), HirStmt::Fn(_)))
        })
    }

    /// Marks every mutable-value holder a moved value reaches as moved out, so a later read is
    /// use-after-move. Returns those holders, so a caller that also records provenance reuses the walk.
    fn move_source(&mut self, node: &HirId<HirExpr>) -> Vec<usize> {
        let mut sources = Vec::new();
        self.reachable_sources(node, &mut sources);
        let moved: Vec<usize> = sources.iter().map(|(i, _)| *i).collect();
        for (i, blame) in sources {
            self.locals[i].move_site = Some(MovedAt { node: blame, cause: MoveCause::Value });
        }
        moved
    }

    /// The mutable-value holders a value reaches.
    fn source_indices(&self, value: &HirId<HirExpr>) -> Vec<usize> {
        let mut sources = Vec::new();
        self.reachable_sources(value, &mut sources);
        sources.into_iter().map(|(i, _)| i).collect()
    }

    /// The source slots feeding a binding's value: the mutable-value holders it reaches, plus the
    /// enclosing sources a captured writing closure moves.
    fn provenance_of(&self, value: &HirId<HirExpr>) -> Vec<usize> {
        let mut out = self.source_indices(value);
        out.extend(self.captured_sources(value));
        out
    }

    /// The enclosing locals a closure moves by writing to them. Only mutable-value holders count.
    fn captured_sources(&self, value: &HirId<HirExpr>) -> Vec<usize> {
        let HirExpr::Literal(HirLiteral::Lambda(_)) = self.hir.get(value) else { return Vec::new() };
        let Some(writes) = self.sigs.lambda_writes.get(value) else { return Vec::new() };
        writes.iter().filter_map(|name| {
            let i = self.frame_index_of(*name)?;
            self.holds_mutable(i).then_some(i)
        }).collect()
    }

    /// Stores a value into a container. The value persists there, so a `no persist` value
    /// is rejected, a borrowed value is rejected since it cannot outlive its lender, and a mutable
    /// value moves in as the container becomes its owner.
    fn store_into_container(&mut self, flow: &Flow, expr: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        self.reject_escape(flow, expr)?;
        if self.arg_is_borrowed(expr) {
            return Err(self.error_help("cannot persist a borrowed value".to_string(), expr,
                "take it by `*mut` to own it, then it may be persisted"));
        }
        self.move_source(expr);
        Ok(())
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
                self.function(Some(*stmt), self.sigs.writes.get(stmt), decl)?;
            },
            HirStmt::Type(decl) => self.type_decl(stmt, Some(decl.name), decl)?,
            HirStmt::Trait(decl) => self.type_decl(stmt, None, decl)?,
            HirStmt::Say(field) => self.say(field.name, &field.clause, field.mutable, &field.value)?,
            HirStmt::Expression(e) | HirStmt::Block(e) => { self.expr(e)?; },
            HirStmt::Return(opt) => match opt {
                Some(e) => {
                    let typed = self.expr(e)?;
                    self.check_return_mutability(&typed, e)?;
                    self.check_return_field_move(e)?;
                    self.check_return(&typed.flow, self.fn_ctx.return_shape, e)?;
                    // Returning a mutable value moves it out to the caller.
                    self.move_source(e);
                },
                // A `!` function falls back to null on a bare return, which it may not.
                None if self.fn_ctx.return_shape == ReturnShape::NonNull => {
                    return Err(self.error("A '!' function must return a value, but this 'return' yields null".to_string(), stmt));
                },
                None => {},
            },
            HirStmt::Throw(e) => { self.expr(e)?; },
            HirStmt::While(cond, body) => {
                self.expr(cond)?;
                let body_narrow = self.narrowings(cond, true);
                let binders = self.hir.condition_binders(cond);
                let pre = self.snapshot();
                // Check the body twice. The second pass sees the first pass's moves, so a value the
                // body reads after moving it is caught as a cross-iteration use. A rebind before the
                // move clears it first, so rebind-then-move is still accepted.
                for _ in 0..2 {
                    self.apply_narrowings(&body_narrow);
                    self.with_binders(&binders, |c| c.expr(body))?;
                    self.restore_keeping_moves(&pre);
                }
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

                // A branch that returns or throws never reaches the code after the if, so its end
                // state is not merged.
                let then_diverges = self.hir.definitely_returns(then);
                let else_diverges = otherwise.as_ref().is_some_and(|o| self.hir.stmt_returns(o));
                match (then_diverges, else_diverges) {
                    (false, false) => self.join(&then_snap, &else_snap),
                    (true, false) => self.restore(&else_snap),
                    (false, true) => self.restore(&then_snap),
                    (true, true) => {},
                }
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

                // Arms are mutually exclusive, so each runs from the same pre-match state and only
                // the arms that fall through decide the state after the match.
                let baseline = self.snapshot();
                let mut fallthrough: Vec<FlowSnapshot> = Vec::new();
                let mut exhaustive = false;
                for arm in arms {
                    self.restore(&baseline);
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

                    // A returning or throwing arm never reaches the code after the match.
                    if !self.hir.definitely_returns(&arm.body) {
                        fallthrough.push(self.snapshot());
                    }

                    // An irrefutable guardless arm always matches, so no value slips past unmatched.
                    exhaustive |= arm.guard.is_none() && arm.matcher.is_irrefutable();
                    let ruled = self.arm_rules_out(arm, &remaining);
                    remaining.retain(|w| !ruled.contains(w));
                }

                // A non-exhaustive match can fall through with no arm matching, keeping the pre-match
                // state. Narrowing does not cross a match, so the pre-match narrowings stay.
                let mut outcomes = fallthrough;
                if !exhaustive {
                    outcomes.push(baseline.clone());
                }

                match outcomes.split_first() {
                    Some((first, rest)) => {
                        self.restore(first);
                        for snap in rest { self.join_in(snap); }
                        self.narrowed = baseline.narrowed;
                    },
                    None => self.restore(&baseline),
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
            HirExpr::Literal(HirLiteral::Null) => Typed::of(self.opt_flow(true), TypeTag::Unknown).with_mutability(Mutability::Immutable),
            HirExpr::Literal(lit) => {
                // A plain container literal is immutable by default, so its children are checked
                // for deep immutability. Every literal is an immutable value.
                self.literal_children(lit, expr, true)?;
                Typed::nonnull().with_mutability(Mutability::Immutable)
            },
            HirExpr::Identifier(name) => self.identifier(*name, expr)?,
            HirExpr::This => {
                self.seal.set_this_seen(true);
                if self.seal.in_init() {
                    return Err(self.this_seal_error(expr, "cannot be used as a value here"));
                }
                self.this_typed()
            },
            HirExpr::Assign(lhs, rhs) => self.assign(lhs, rhs)?,
            HirExpr::Call(callee, args) => self.call(expr, callee, args)?,
            HirExpr::Construct(callee, args, brace) => {
                // A construction is a factory call or a brace, never both.
                if !args.is_empty() && !brace.is_empty() {
                    return Err(self.error("cannot mix constructor arguments and brace fields; use `K(..)` or `K { .. }`".to_string(), expr));
                }
                // A plain construction seals into an immutable object; a `mut` one does not. Capture
                // it before checking children, which reset the flag for their own constructions.
                let immutable = !std::mem::take(&mut self.mut_construction);
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
                    self.check_construction(*type_name, &braced, true, callee)?;
                }
                // A plain construction is immutable by default. A `mut Ctor()` overrides it above.
                if immutable { self.record_deep_seal(expr); }
                Typed::of(Flow::Clean, tag).with_mutability(Mutability::Immutable)
            },
            HirExpr::Mut(inner) => {
                // A mutable container may hold mutable elements, so its children skip the
                // immutable-container check that a plain literal applies.
                match self.hir.get(inner) {
                    HirExpr::Literal(lit @ (HirLiteral::Array(_) | HirLiteral::Dict(_))) => {
                        self.literal_children(lit, inner, false)?;
                        Typed::nonnull().with_mutability(Mutability::Mutable)
                    },
                    _ => {
                        // The inner construction is mutable, so it does not deep-seal its fields.
                        self.mut_construction = true;
                        let typed = self.expr(inner)?;
                        self.mut_construction = false;
                        typed.with_mutability(Mutability::Mutable)
                    },
                }
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
                self.revive_scoped_sources(mark);
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
                Typed::of(self.discharged_flow(&typed.flow), typed.tag)
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
                Typed::of(self.discharged_flow(&typed.flow), typed.tag)
            },
        })
    }

    fn say(&mut self, name: Symbol, clause: &HirSlotClause, mutable: bool, value: &Option<HirId<HirExpr>>) -> Result<(), anyhow::Error> {
        let owed = self.clause_owed(clause);
        let (assigned, tag, mutability, provenance) = if let Some(value) = value {
            self.reject_this_store(value)?;
            let typed = self.expr(value)?;
            self.check_into_slot(&typed.flow, &owed, name, value)?;
            // The move records the sources feeding the value, so the slot reuses that walk for its
            // provenance and adds the closure captures a bare walk would miss.
            let mut provenance = self.move_source(value);
            provenance.extend(self.captured_sources(value));
            (true, typed.tag, typed.mutability, provenance)
        } else {
            (false, TypeTag::Unknown, Mutability::Unknown, Vec::new())
        };
        let mut local = Local::value(name, owed, mutable, assigned, tag);
        local.container = clause.container;
        local.mutability = mutability;
        local.provenance = provenance;
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
            local.param = true;
            local.mutability = Mutability::param(param.clause.capability);
            // A plain `mut` parameter borrows its argument; `*mut` owns it.
            local.borrowed = param.clause.capability == Capability::Mut;
            self.locals.push(local);
        }
        let result = body(self);
        self.locals.truncate(mark);
        self.frame_start = saved_frame;
        result
    }

    fn function(&mut self, stmt: Option<HirId<HirStmt>>, writes: Option<&'a HashSet<Symbol>>, decl: &HirFnDecl) -> Result<(), anyhow::Error> {
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
        // A `mut` parameter borrows its argument, so it may not persist it. `*mut` owns the
        // argument and may persist it. A `no persist` parameter is left to `reject_escape`.
        if let Some(stmt) = stmt {
            for (i, param) in decl.params.iter().enumerate() {
                let cap = param.clause.capability;
                let owes_no_persist = self.owes_no_persist(param.clause.names.iter().copied());
                if cap.is_mut() && !cap.is_move() && !owes_no_persist && self.sigs.param_escapes_at(&stmt, i) {
                    return Err(self.error_help(
                        "a `mut` parameter borrows its argument and cannot let it escape".to_string(),
                        &param.name,
                        "take it by `*mut` to own it, or freeze or copy it before persisting"));
                }
            }
        }
        let saved_seal = self.seal.suspend();
        // A lambda's shape is inferred, so it is not checked against a declaration.
        let ctx = FnContext {
            return_shape: decl.ret,
            return_owes: !decl.clause.names.is_empty(),
            return_unmarked: unmarked,
            return_mut: decl.clause.capability.is_mut(),
            name: Some(decl.name),
            return_clause: decl.clause.pos.clone(),
            params: decl.params.iter().map(|p| (self.ident_sym(&p.name), p.pos.clone())).collect(),
            writes,
        };
        let saved = std::mem::replace(&mut self.fn_ctx, ctx);
        let result = self.with_frame(&decl.params, |c| {
            c.expr(&decl.body)?;
            // A non-null return must be produced on every path.
            if c.fn_ctx.return_shape == ReturnShape::NonNull && !c.hir.definitely_returns(&decl.body) {
                return Err(c.error("This function can finish without returning a value; a '!' return must produce one on every path".to_string(), &decl.body));
            }
            Ok(())
        });
        self.fn_ctx = saved;
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
            self.function(Some(*stmt), self.sigs.writes.get(stmt), decl)?;
        }
        Ok(())
    }

    /// Checks a literal's children. `immutable` is set for a plain container literal: an immutable
    /// container is immutable all the way down, so a mutable element is rejected, and an element of
    /// unknown capability records a runtime seal-check.
    fn literal_children(&mut self, lit: &HirLiteral, node: &HirId<HirExpr>, immutable: bool) -> Result<(), anyhow::Error> {
        match lit {
            HirLiteral::Array(elems) => for e in elems {
                let t = self.expr(e)?;
                self.check_container_element(immutable, &t, e, node)?;
                self.store_into_container(&t.flow, e)?;
            },
            HirLiteral::Dict(pairs) => for (k, v) in pairs {
                self.expr(k)?;
                let t = self.expr(v)?;
                self.check_container_element(immutable, &t, v, node)?;
                self.store_into_container(&t.flow, v)?;
            },
            HirLiteral::Lambda(decl) => self.lambda(decl, node)?,
            _ => {},
        }
        Ok(())
    }

    /// Enforces deep immutability at a container element. A known-mutable element in an immutable
    /// container is a compile error; an unknown-capability one defers to a runtime seal-check.
    fn check_container_element(&mut self, immutable: bool, elem: &Typed, elem_node: &HirId<HirExpr>, container: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        if !immutable {
            return Ok(());
        }
        match elem.mutability {
            Mutability::Mutable => Err(self.mutable_in_immutable_error(elem_node)),
            Mutability::Unknown => { self.record_seal_check(container); Ok(()) },
            Mutability::Immutable => Ok(()),
        }
    }

    fn mutable_in_immutable_error(&self, node: &HirId<HirExpr>) -> anyhow::Error {
        anyhow!("{}", Diagnostic::new(
            "cannot store a mutable value in an immutable container".to_string(),
            self.hir.pos(node).clone())
            .with_label("this value is mutable")
            .with_help("freeze the value, or mark the container `mut`"))
    }

    fn identifier(&mut self, name: Symbol, expr: &HirId<HirExpr>) -> Result<Typed, anyhow::Error> {
        let Some(i) = self.frame_index_of(name) else {
            // A read that resolves to an enclosing frame is a closure capture.
            self.capture_enclosing(name, expr);
            return Ok(Typed::unknown());
        };

        if self.locals[i].func.is_some() {
            return Ok(Typed::unknown());
        }

        if let Some(site) = self.locals[i].move_site {
            return Err(self.use_after_move_error(name, expr, site));
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


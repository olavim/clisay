//! Flow-sensitive semantic checks: what a program does on the way to each point.

mod barriers;
mod conform;
mod narrow;
mod returns;
pub(crate) mod scope;
pub(crate) mod paths;
mod values;
use values::{Route, UnroutedValueState, ValueState};
pub use paths::PathMap;
use scope::Callables;
mod walk;
mod write_order;
pub use write_order::AssignStrategy;

use crate::ast::BuiltinType;
use crate::core::objects;
use indexmap::IndexSet;
use std::collections::{BTreeSet, HashMap};

use crate::frontend::lex::SourcePosition;
use crate::RunConfig;
use crate::middle::bind::{Bindings, TypeLayout};
use crate::middle::diagnose::Diagnose;
use crate::middle::hir::{Hir, HirExpr, HirId, HirStmt, Symbol};
use crate::middle::obligations::{Obligations, ObligationRule, Site};
use crate::middle::signatures::Resolved;
use crate::middle::signatures::{CallableId, Signatures, TypeTag};


pub use barriers::{Barrier, Barriers, Guard, CheckedSlot};

pub fn check(hir: &Hir, bindings: &Bindings, sigs: &Signatures, config: RunConfig) -> Result<Barriers, anyhow::Error> {
    let mut checker = Checker::new(hir, bindings, sigs, config);
    checker.stmt(&hir.get_root())?;
    checker.out.witness_decls = sigs.object_witnesses().map(|(_, id)| id).collect();
    Ok(checker.out)
}

/// What a callee expression turns out to be, before anything is walked.
enum Callee {
    /// A type built by a paren call.
    Construct(HirId<HirStmt>),
    /// A function or lambda this pass can name.
    Callable(CallableId),
    /// A built-in global.
    Builtin(crate::middle::native::NativeSig),
    /// A member call on a receiver. `safe_receiver` is the `?` in `a?.m()`, which is a different
    /// question from the `?` in `a.m?()`.
    Method { receiver: HirId<HirExpr>, member: HirId<HirExpr>, safe_receiver: bool },
    /// A value whose declaration is not visible here.
    Value,
}

/// How a call form treats a callee that owes something.
#[derive(Clone, Copy)]
enum BadCallee {
    /// `cb()` has nothing to call, so it refuses.
    Refuse,
    /// `cb?()` skips the call and carries what the callee owed into the result.
    ShortCircuit,
}

/// The obligation state of a value as it flows.
#[derive(Clone)]
enum Debt {
    Clean,
    Void,
    Unknown,
    Owed { obligations: Obligations, definite: bool },
}

impl Debt {
    fn is_void(&self) -> bool {
        matches!(self, Debt::Void)
    }

    /// Whether the value is known to be in a bad state.
    fn is_definite(&self) -> bool {
        matches!(self, Debt::Owed { definite: true, .. })
    }

}

/// The kind of operand a value is, which decides which debts still block it.
pub(super) enum OperandKind {
    /// Used whole: an operator's operand, or a call's callee. Every blocking debt blocks.
    Whole,
    /// Reached into: the base of a `.` or `[]` access.
    Base,
}

struct Local {
    name: Symbol,
    reassignable: bool,
    assigned: bool,
    fn_decl: bool,
    resolved_callable: Option<CallableId>,
    pattern_binder_source: Option<PatternBinderSource>,
    used: bool,
    clause: Option<Obligations>,
    proven: PathMap<ProvenFacts>,
    possible: PathMap<PossibleFacts>,
    /// Where the binding was introduced.
    site: Option<HirId<HirExpr>>,
    is_anchor: bool,
    is_param: bool,
    is_catch: bool,
    /// The node that declared the binding.
    decl: Option<usize>,
}

#[derive(Clone, Copy, PartialEq, Default)]
pub(super) enum PatternBinderSource {
    #[default]
    Param,
    Arm,
    Condition,
    Handler,
    Say,
}

impl Local {
    /// What the local owes. With nothing recorded, it owes at most what its clause admits.
    fn owed(&self) -> &Obligations {
        self.proven.get(&[][..]).and_then(|f| f.owed.as_ref()).unwrap_or(self.clause_owed())
    }

    fn set_element_owed(&mut self, owed: Obligations) {
        self.proven.update(ELEMENTS.to_vec(), |facts| facts.owed = Some(owed));
    }

    fn set_owed(&mut self, owed: Obligations) {
        self.proven.update(FlowPath::new(), |facts| facts.owed = Some(owed));
    }

    fn set_value(&mut self, value: &ValueState) {
        self.proven = value.proven.clone();
        self.possible = value.possible.clone();
        self.set_owed(match &value.debt {
            Debt::Owed { obligations, .. } => obligations.clone(),
            _ => Obligations::new(),
        });
        self.set_tag(value.tag.clone());
    }

    fn tag(&self) -> &TypeTag {
        self.proven.get(&FlowPath::new()).map_or(&TypeTag::Unknown, |f| &f.tag)
    }

    fn set_tag(&mut self, tag: TypeTag) {
        self.proven.update(FlowPath::new(), |facts| facts.tag = tag);
    }

    fn clause_owed(&self) -> &Obligations {
        self.clause.as_ref().unwrap_or(&NO_OBLIGATIONS)
    }

    fn read_debt(&self, owed: Obligations) -> Debt {
        match (owed.is_empty(), self.clause.is_none()) {
            (false, _) => Debt::Owed { obligations: owed, definite: false },
            (true, true) => Debt::Unknown,
            (true, false) => Debt::Clean,
        }
    }

    fn base(name: Symbol) -> Local {
        Local {
            name,
            reassignable: false,
            assigned: true,
            fn_decl: false,
            resolved_callable: None,
            pattern_binder_source: None,
            used: false,
            clause: Some(Obligations::new()),
            proven: PathMap::new(),
            possible: PathMap::new(),
            site: None,
            decl: None,
            is_anchor: false,
            is_param: false,
            is_catch: false,
        }
    }

    fn param(name: Symbol, clause: Obligations, reassignable: bool) -> Local {
        let mut local = Local { reassignable, clause: Some(clause.clone()), ..Local::base(name) };
        local.set_owed(clause);
        local
    }

    fn catch(name: Symbol) -> Local {
        let mut local = Local { clause: None, is_catch: true, ..Local::base(name) };
        local.set_value(&ValueState::unknown());
        local
    }

    fn as_used(mut self) -> Local {
        self.used = true;
        self
    }

    fn pattern_binder(name: Symbol, value: &ValueState, source: PatternBinderSource) -> Local {
        let owed = match &value.debt {
            Debt::Owed { obligations, .. } => obligations.clone(),
            _ => Obligations::new(),
        };
        let mut local = Local {
            pattern_binder_source: Some(source),
            clause: (!matches!(value.debt, Debt::Unknown)).then_some(owed),
            ..Local::base(name)
        };
        local.set_value(value);
        local
    }

    fn func(name: Symbol, stmt: HirId<HirStmt>) -> Local {
        Local { fn_decl: true, resolved_callable: Some(stmt.into()), decl: Some(stmt.index()), ..Local::base(name) }
    }

    fn say(name: Symbol, clause: Obligations, value: &ValueState, reassignable: bool, assigned: bool) -> Local {
        let mut local = Local {
            reassignable, assigned,
            clause: Some(clause),
            ..Local::base(name)
        };
        local.set_value(value);
        local
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum PathStep {
    Field(Symbol),
    Index { offset: u8, from_back: bool },
    EveryElement,
}

/// A path from a root to the value the facts are about. Empty is the root itself.
pub type FlowPath = Vec<PathStep>;

#[derive(Clone, Default, PartialEq)]
pub struct PossibleFacts {
    pub callables: Callables,
}

#[derive(Clone, Default, PartialEq)]
pub struct ProvenFacts {
    pub owed: Option<Obligations>,
    pub tag: TypeTag,
}

const ELEMENTS: [PathStep; 1] = [PathStep::EveryElement];
static NO_OBLIGATIONS: Obligations = Obligations::new();

impl ProvenFacts {
    /// What a value read from here owes. Nothing recorded is nothing known.
    fn debt(&self) -> Debt {
        match &self.owed {
            Some(owed) if !owed.is_empty() => Debt::Owed { obligations: owed.clone(), definite: false },
            Some(_) => Debt::Clean,
            None => Debt::Unknown,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.owed.is_none() && self.tag == TypeTag::Unknown
    }

    pub fn clear(&mut self) {
        self.owed = None;
        self.tag = TypeTag::Unknown;
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum NarrowRoot {
    Local(usize),
    This,
}

struct AnchorArgument {
    /// The call the anchor was passed to.
    at: HirId<HirExpr>,
    /// What the anchor owed before the call.
    owed: Obligations,
}

#[derive(Clone, PartialEq, Eq, Hash)]
struct NarrowTarget {
    root: NarrowRoot,
    path: FlowPath,
}

impl NarrowTarget {
    fn local(i: usize) -> NarrowTarget {
        NarrowTarget { root: NarrowRoot::Local(i), path: FlowPath::new() }
    }

    /// The receiver itself, which is the empty path.
    fn this_root() -> NarrowTarget {
        NarrowTarget { root: NarrowRoot::This, path: FlowPath::new() }
    }

    /// The target one step further in.
    fn child(&self, step: PathStep) -> NarrowTarget {
        NarrowTarget { root: self.root, path: self.path.iter().copied().chain([step]).collect() }
    }

    fn parent(&self) -> Option<NarrowTarget> {
        let (_, rest) = self.path.split_last()?;
        Some(NarrowTarget { root: self.root, path: rest.to_vec() })
    }
}

#[derive(PartialEq)]
enum NarrowFact {
    /// The anchor no longer owes this obligation on the branch.
    Discharge(NarrowTarget, Symbol),
    /// The anchor has the given concrete type.
    Tag(NarrowTarget, TypeTag),
    Owes(NarrowTarget, Obligations),
}

#[derive(Default)]
struct FnContext {
    callable: Option<CallableId>,
    declares_void: bool,
    return_owes: bool,
    return_undeclared: bool,
    return_admits: Option<Obligations>,
    receiver: Obligations,
    receiver_reassignable: bool,
    /// Whether the receiver is `&var this`, which names storage the caller chose.
    wants_anchor_receiver: bool,
    name: Option<Symbol>,
    return_clause: Option<SourcePosition>,
    returns_void: bool,
    in_defer: bool,
}

#[derive(Clone, Copy)]
pub(super) enum WriteRoot {
    Local(usize),
    Anchor(usize),
    Receiver,
    Capture { node: HirId<HirExpr>, local: Option<usize> },
    Ref,
    Global,
    Temporary,
    /// A path that starts at a value no binding holds, as `f()` in `f()[0]`.
    Value,
}

impl WriteRoot {
    fn untracked_store_route(self) -> Route {
        match self {
            WriteRoot::Anchor(_) => Route::WrittenThroughAnchor,
            WriteRoot::Receiver => Route::StoredInThis,
            WriteRoot::Ref => Route::StoredInRef,
            WriteRoot::Global => Route::StoredInGlobal,
            WriteRoot::Temporary | WriteRoot::Value => Route::StoredInTemporary,
            WriteRoot::Local(_) => unreachable!("a write into a followed local reached untracked routing"),
            WriteRoot::Capture { .. } => unreachable!("a write through a capture reached routing without being refused"),
        }
    }

    fn narrow_root(self) -> Option<NarrowRoot> {
        match self {
            WriteRoot::Local(i) | WriteRoot::Anchor(i) => Some(NarrowRoot::Local(i)),
            WriteRoot::Receiver => Some(NarrowRoot::This),
            _ => None,
        }
    }
}

#[derive(Clone, Copy)]
pub(super) struct Ctx<'a> {
    hir: &'a Hir,
    bindings: &'a Bindings,
    sigs: &'a Signatures,
    force_checks: bool,
}

impl<'a> Diagnose for Ctx<'a> {
    fn hir(&self) -> &Hir { self.hir }
}

impl<'a> Ctx<'a> {
    pub(super) fn ref_admits(&self) -> Obligations {
        let Some(decl) = self.sigs.builtin_decl(BuiltinType::Ref) else { return Obligations::new() };
        let Some(layout) = self.layout_of(&decl) else { return Obligations::new() };
        layout.owed_at(objects::REF_VALUE_FIELD)
    }

    pub(in crate::middle::check) fn ref_debt(&self) -> Debt {
        let owed = self.ref_admits();
        match owed.is_empty() {
            true => Debt::Clean,
            false => Debt::Owed { obligations: owed, definite: false },
        }
    }

    fn resolved(&self) -> Resolved<'a> {
        Resolved { hir: self.hir, bindings: self.bindings, sigs: self.sigs }
    }

    /// The layout of a tracked concrete type.
    pub(super) fn layout_of(&self, decl: &HirId<HirStmt>) -> Option<&'a TypeLayout> {
        self.bindings.layout_of_decl(decl)
    }

    fn binding_display_name(&self, name: Symbol) -> String {
        let text = self.hir.text(name);
        text.strip_prefix('$').unwrap_or(text).to_string()
    }

    fn constructor_init(&self, callee: &HirId<HirExpr>) -> Option<HirId<HirStmt>> {
        let type_stmt = self.resolved().type_named(callee)?;
        let HirStmt::Type(decl) = self.hir.get(&type_stmt) else { return None };
        Some(decl.init)
    }

    fn is_factory_field(&self, name: Symbol) -> bool {
        self.hir.text(name).starts_with('$')
    }

    fn opt_debt(&self, definite: bool) -> Debt {
        Debt::Owed { obligations: Obligations::from([self.sigs.opt]), definite }
    }

    /// Whether a type has a factory. A factory-less type (not all-defaulted, no `init`) is built
    /// only by brace, so `T(..)` cannot construct it.
    fn type_has_factory(&self, decl: &HirId<HirStmt>) -> bool {
        match self.hir.get(decl) {
            HirStmt::Type(decl) if decl.builtin.is_some() => true,
            HirStmt::Type(decl) => matches!(self.hir.get(&decl.init), HirStmt::Fn(_)),
            _ => false,
        }
    }

    fn type_name_of(&self, decl: &HirId<HirStmt>) -> Option<Symbol> {
        match self.hir.get(decl) {
            HirStmt::Type(decl) | HirStmt::Trait(decl) => Some(decl.name),
            _ => None,
        }
    }

}

/// What is known so far about a lambda, function or type.
struct CallableState {
    unreached: Option<Symbol>,
    frame: usize,
    uses: Callables,
}

struct Checker<'a> {
    ctx: Ctx<'a>,
    effects: scope::SubtreeEffects,
    defer_reads: Vec<BTreeSet<usize>>,
    callables: HashMap<usize, CallableState>,
    out: Barriers,
    locals: Vec<Local>,
    frame_start: usize,
    current_frame_id: usize,
    frames_opened: usize,
    current_type: Option<HirId<HirStmt>>,
    /// What the type or trait being checked witnesses, which its methods' `this` owes.
    current_type_witnesses: Obligations,
    checking_factory: bool,
    resolved_callees: HashMap<HirId<HirExpr>, CallableId>,
    this_narrowed: PathMap<ProvenFacts>,
    current_trait_surface: Option<IndexSet<Symbol>>,
    current_trait: Option<HirId<HirStmt>>,
    fn_ctx: FnContext,
    anchor_arguments: HashMap<NarrowTarget, AnchorArgument>,
}

impl<'a> Diagnose for Checker<'a> {
    fn hir(&self) -> &Hir { self.ctx.hir }
}

impl<'a> Checker<'a> {
    fn new(hir: &'a Hir, bindings: &'a Bindings, sigs: &'a Signatures, config: RunConfig) -> Checker<'a> {
        Checker {
            ctx: Ctx { hir, bindings, sigs, force_checks: config.force_checks },
            resolved_callees: HashMap::new(),
            anchor_arguments: HashMap::new(),
            locals: Vec::new(),
            frame_start: 0,
            current_frame_id: 0,
            frames_opened: 0,
            current_type: None,
            current_type_witnesses: Obligations::new(),
            checking_factory: false,
            this_narrowed: PathMap::new(),
            current_trait_surface: None,
            current_trait: None,
            fn_ctx: FnContext::default(),
            effects: scope::SubtreeEffects::default(),
            defer_reads: Vec::new(),
            callables: HashMap::new(),
            out: Barriers { force_return_tests: config.force_checks, ..Barriers::default() },
        }
    }

    fn this_tag(&self) -> TypeTag {
        if self.current_trait_surface.is_some() {
            TypeTag::SelfType
        } else {
            self.current_type.map_or(TypeTag::Unknown, TypeTag::Concrete)
        }
    }

    fn this_valuestate(&self) -> ValueState {
        let receiver = &self.fn_ctx.receiver;
        let debt = if receiver.is_empty() {
            Debt::Clean
        } else {
            Debt::Owed { obligations: receiver.clone(), definite: false }
        };
        ValueState::of(debt, self.this_tag())
    }

    fn callable_of(&self, name: Symbol) -> Option<CallableId> {
        self.locals.iter().rev().find(|l| l.name == name).and_then(|l| l.resolved_callable)
    }

    fn trait_member(&self, name: &str, node: &HirId<HirExpr>) -> Result<ValueState, anyhow::Error> {
        let in_surface = self.current_trait_surface.as_ref().is_some_and(|surface| surface.iter().any(|m| self.ctx.hir.text(*m) == name));
        if !in_surface {
            return Err(self.error_help(format!("'{}' is not declared or required by this trait", name), node, "declare it or add a 'req'"));
        }
        Ok(ValueState::unknown())
    }
}

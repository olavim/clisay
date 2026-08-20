//! Flow-sensitive semantic checks: what a program does on the way to each point.

pub(crate) mod alias;
mod barriers;
mod conform;
mod narrow;
mod returns;
pub(crate) mod scope;
mod walk;

use indexmap::IndexSet;
use std::collections::{HashMap, HashSet};

use crate::frontend::lex::SourcePosition;
use crate::middle::bind::{Bindings, TypeLayout};
use crate::middle::diagnose::Diagnose;
use crate::middle::hir::{Hir, HirExpr, HirId, HirStmt, ReturnShape, Symbol};
use crate::middle::obligations::{Obligations, ObligationRule, Site};
use crate::middle::signatures::Resolved;
use crate::middle::signatures::{Mutability, Signatures, TypeTag};

use alias::{AliasLocal, ElementKey, TransferSite};

pub use barriers::{Barrier, Barriers, Guard, WitnessSet};

pub fn check(hir: &Hir, bindings: &Bindings, sigs: &Signatures, force_checks: bool) -> Result<Barriers, anyhow::Error> {
    let mut checker = Checker::new(hir, bindings, sigs, force_checks);
    checker.stmt(&hir.get_root())?;
    checker.out.witness_decls = sigs.object_witnesses().map(|(_, id)| id).collect();
    Ok(checker.out)
}

/// The obligation state of a value as it flows.
#[derive(Clone)]
enum Debt {
    /// A present value owing no obligations.
    Clean,
    /// A void result: no value at all.
    Void,
    /// A dynamic-boundary value whose obligations are unknown.
    Unknown,
    /// A value owing obligations. `definite` marks a value known to be in the bad state, as
    /// opposed to one that only may be. `container` marks an array or dict whose elements owe
    /// the obligations.
    Owed { obligations: Obligations, definite: bool, container: bool },
}

impl Debt {
    fn is_void(&self) -> bool {
        matches!(self, Debt::Void)
    }
}

/// Why a value fails to satisfy a non-null target.
enum Violation {
    Void,
    Null,
    Nullable,
}

/// What the pass knows about a value at a point: what it owes, what it is, and who may write it.
#[derive(Clone)]
struct ValueState {
    debt: Debt,
    tag: TypeTag,
    /// What the value is: whether anything may mutate it at all.
    mutability: Mutability,
    /// Whether this name may write it. A binding that a closure took write-permission from may not.
    writable: Mutability,
}

impl ValueState {
    fn unknown() -> ValueState { ValueState { debt: Debt::Unknown, tag: TypeTag::Unknown, mutability: Mutability::Unknown, writable: Mutability::Unknown } }
    fn nonnull() -> ValueState { ValueState { debt: Debt::Clean, tag: TypeTag::Unknown, mutability: Mutability::Unknown, writable: Mutability::Unknown } }
    fn of(debt: Debt, tag: TypeTag) -> ValueState { ValueState { debt, tag, mutability: Mutability::Unknown, writable: Mutability::Unknown } }
    fn with_mutability(mut self, mutability: Mutability) -> ValueState {
        self.mutability = mutability;
        self.writable = mutability;
        self
    }
    fn with_writable(mut self, writable: Mutability) -> ValueState { self.writable = writable; self }
}

/// A tracked binding in the current function frame.
struct Local {
    name: Symbol,
    /// The obligations this binding owes. Reading it yields those obligations until it is narrowed.
    owed: Obligations,
    reassignable: bool,
    /// Whether the binding is provably assigned on the current path.
    assigned: bool,
    tag: TypeTag,
    func: Option<HirId<HirStmt>>,
    /// The form this binding was bound by, or `None` for an ordinary binding.
    binder: Option<BinderSource>,
    /// Whether the binding holds a container whose elements owe `owed`.
    container: bool,
    /// Whether the binding is a function parameter.
    param: bool,
    /// The obligations settled on this binding: discharged here, or handed to a slot that declares them.
    handled: Obligations,
    /// The obligations discharged on this binding on the current path.
    discharged: Obligations,
    /// The obligations discharged per field of this binding, where it holds an immutable value of a known type.
    field_discharged: HashMap<Symbol, Obligations>,
    /// Where the binding was introduced.
    site: Option<HirId<HirExpr>>,
    /// The node that declared the binding.
    decl: Option<usize>,
    /// Everything the one-writer rule tracks about this binding, which `alias` owns.
    alias: AliasLocal,
    /// Bound where no test proved what the member holds, so a read is a dynamic-boundary value.
    unknown: bool,
}

#[derive(Clone, Copy, PartialEq, Default)]
pub(super) enum BinderSource {
    #[default]
    Param,
    Arm,
    Condition,
    Handler,
}

impl Local {
    /// What a read of this binding owes, given what the binding still owes.
    fn read_debt(&self, owed: Obligations) -> Debt {
        match (owed.is_empty(), self.unknown) {
            (false, _) => Debt::Owed { obligations: owed, definite: false, container: self.container },
            (true, true) => Debt::Unknown,
            (true, false) => Debt::Clean,
        }
    }

    /// A binding with every fact at its neutral default. Each named constructor overrides only the
    /// fields that distinguish it, so a new field is added here once.
    fn base(name: Symbol) -> Local {
        Local { name, owed: Obligations::new(), reassignable: false, assigned: true, tag: TypeTag::Unknown, func: None, binder: None, container: false, param: false, handled: Obligations::new(), discharged: Obligations::new(), field_discharged: HashMap::new(), site: None, decl: None, alias: AliasLocal { may_be_shared: true, ..AliasLocal::default() }, unknown: false }
    }

    fn param(name: Symbol, owed: Obligations, reassignable: bool) -> Local {
        Local { owed, reassignable, ..Local::base(name) }
    }

    /// A caught value, bound for the handler only. Nothing reassigns it.
    fn catch(name: Symbol, owed: Obligations) -> Local {
        Local::param(name, owed, false)
    }

    fn binder_owing(name: Symbol, owed: Obligations, source: BinderSource) -> Local {
        Local { owed, binder: Some(source), ..Local::base(name) }
    }

    fn func(name: Symbol, stmt: HirId<HirStmt>) -> Local {
        Local { func: Some(stmt), ..Local::base(name) }
    }

    fn value(name: Symbol, owed: Obligations, reassignable: bool, assigned: bool, tag: TypeTag) -> Local {
        Local { owed, reassignable, assigned, tag, ..Local::base(name) }
    }
}

/// Where a narrowing applies: a local, a `this` field, or a field of an immutable local receiver.
#[derive(Clone, Copy, PartialEq)]
enum NarrowTarget {
    Local(usize),
    ThisField(Symbol),
    LocalField(usize, Symbol),
}

/// A flow fact a check establishes for a branch.
#[derive(PartialEq)]
enum NarrowFact {
    /// The place no longer owes this obligation on the branch.
    Discharge(NarrowTarget, Symbol),
    /// The local has the given concrete type.
    Tag(usize, TypeTag),
}

/// What a method's declared `this` says about the receiver.
#[derive(Default, Clone)]
struct ReceiverFacts {
    mutability: Mutability,
    /// The obligations the receiver clause declares.
    owed: Obligations,
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
    /// The obligations the return admits, from the function's signature. A returned value may owe
    /// no more than these. `None` where there is no signature to conform to, as in a lambda.
    return_admits: Option<Obligations>,
    /// What the declared `this` says about the receiver.
    receiver: ReceiverFacts,
    /// The function's name.
    name: Option<Symbol>,
    /// The return-clause span.
    return_clause: Option<SourcePosition>,
    /// The parameters as `(name, span)`.
    params: Vec<(Symbol, SourcePosition)>,
    /// Per parameter, whether the escape summary clears it of ever leaving the call.
    param_confined: Vec<bool>,
    /// The names this body writes, so a read-only capture is told from a writing one.
    writes: Option<&'a HashSet<Symbol>>,
}

/// What the pass reads and never writes.
#[derive(Clone, Copy)]
pub(super) struct Ctx<'a> {
    hir: &'a Hir,
    bindings: &'a Bindings,
    sigs: &'a Signatures,
    /// Whether to record the runtime checks the pass proves unnecessary, so codegen can emit them anyway.
    force_checks: bool,
}

impl<'a> Diagnose for Ctx<'a> {
    fn hir(&self) -> &Hir { self.hir }
}

impl<'a> Ctx<'a> {
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
        Debt::Owed { obligations: Obligations::from([self.sigs.opt]), definite, container: false }
    }

    fn nullable_to_obligations(&self, nullable: bool) -> Obligations {
        if nullable { Obligations::from([self.sigs.opt]) } else { Obligations::new() }
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

struct Checker<'a> {
    ctx: Ctx<'a>,
    out: Barriers,
    /// The identifier a member access is about to read as its path base.
    path_base: Option<HirId<HirExpr>>,
    locals: Vec<Local>,
    frame_start: usize,
    current_type: Option<HirId<HirStmt>>,
    checking_factory: bool,
    resolved_callees: HashMap<HirId<HirExpr>, HirId<HirStmt>>,
    this_narrowed: HashMap<Symbol, Obligations>,
    current_trait_surface: Option<IndexSet<Symbol>>,
    fn_ctx: FnContext<'a>,
    pub(super) mut_construction: bool,
    /// Locals a call in the expression being walked may have rebound.
    rebound_in_expr: HashSet<usize>,
    /// How many times an element's write-ownership has been handed to a container.
    element_write_ownerships_transferred: usize,
}

impl<'a> Diagnose for Checker<'a> {
    fn hir(&self) -> &Hir { self.ctx.hir }
}

impl<'a> Checker<'a> {
    fn new(hir: &'a Hir, bindings: &'a Bindings, sigs: &'a Signatures, force_checks: bool) -> Checker<'a> {
        Checker {
            ctx: Ctx { hir, bindings, sigs, force_checks },
            resolved_callees: HashMap::new(),
            rebound_in_expr: HashSet::new(),
            element_write_ownerships_transferred: 0,
            locals: Vec::new(),
            frame_start: 0,
            current_type: None,
            checking_factory: false,
            this_narrowed: HashMap::new(),
            current_trait_surface: None,
            fn_ctx: FnContext::default(),
            out: Barriers::default(),
            path_base: None,
            mut_construction: false,
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
        let debt = if receiver.owed.is_empty() {
            Debt::Clean
        } else {
            Debt::Owed { obligations: receiver.owed.clone(), definite: false, container: false }
        };
        ValueState::of(debt, self.this_tag()).with_mutability(receiver.mutability)
    }

    fn func_of(&self, name: Symbol) -> Option<HirId<HirStmt>> {
        self.locals.iter().rev().find(|l| l.name == name).and_then(|l| l.func)
    }

    fn trait_member(&self, name: &str, node: &HirId<HirExpr>) -> Result<ValueState, anyhow::Error> {
        let in_surface = self.current_trait_surface.as_ref().is_some_and(|surface| surface.iter().any(|m| self.ctx.hir.text(*m) == name));
        if !in_surface {
            return Err(self.error_help(format!("'{}' is not declared or required by this trait", name), node, "declare it or add a 'req'"));
        }
        Ok(ValueState::unknown())
    }

}

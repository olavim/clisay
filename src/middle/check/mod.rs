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
use crate::RunConfig;
use crate::middle::bind::{Bindings, TypeLayout};
use crate::middle::diagnose::Diagnose;
use crate::middle::hir::{Hir, HirExpr, HirId, HirStmt, ReturnShape, Symbol};
use crate::middle::obligations::{Obligations, ObligationRule, Site};
use crate::middle::signatures::Resolved;
use crate::middle::signatures::{CallableId, Mutability, Signatures, TypeTag};

use alias::{AliasLocal, ElementKey, TransferSite};

pub use barriers::{Barrier, Barriers, Guard, WitnessSet};

pub fn check(hir: &Hir, bindings: &Bindings, sigs: &Signatures, config: RunConfig) -> Result<Barriers, anyhow::Error> {
    let mut checker = Checker::new(hir, bindings, sigs, config);
    checker.stmt(&hir.get_root())?;
    checker.out.witness_decls = sigs.object_witnesses().map(|(_, id)| id).collect();
    Ok(checker.out)
}

/// The obligation state of a value as it flows.
#[derive(Clone)]
enum Debt {
    Clean,
    Void,
    Unknown,
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

struct Local {
    name: Symbol,
    owed: Obligations,
    reassignable: bool,
    assigned: bool,
    tag: TypeTag,
    fn_decl: bool,
    resolved_callable: Option<CallableId>,
    pattern_binder_source: Option<PatternBinderSource>,
    /// Whether the binding holds a container whose elements owe `owed`.
    container: bool,
    param: bool,
    /// The obligations settled on this binding.
    handled: Obligations,
    /// The obligations discharged on this binding.
    discharged: Obligations,
    /// The obligations discharged per field of this binding.
    field_discharged: HashMap<Symbol, Obligations>,
    /// Where the binding was introduced.
    site: Option<HirId<HirExpr>>,
    /// The node that declared the binding.
    decl: Option<usize>,
    alias: AliasLocal,
    unknown: bool,
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

impl PatternBinderSource {
    pub(super) fn can_be_var(self) -> bool {
        matches!(self, PatternBinderSource::Say)
    }
}

impl Local {
    fn read_debt(&self, owed: Obligations) -> Debt {
        match (owed.is_empty(), self.unknown) {
            (false, _) => Debt::Owed { obligations: owed, definite: false, container: self.container },
            (true, true) => Debt::Unknown,
            (true, false) => Debt::Clean,
        }
    }

    fn base(name: Symbol) -> Local {
        Local { name, owed: Obligations::new(), reassignable: false, assigned: true, tag: TypeTag::Unknown, fn_decl: false, resolved_callable: None, pattern_binder_source: None, container: false, param: false, handled: Obligations::new(), discharged: Obligations::new(), field_discharged: HashMap::new(), site: None, decl: None, alias: AliasLocal { may_be_shared: true, ..AliasLocal::default() }, unknown: false }
    }

    fn param(name: Symbol, owed: Obligations, reassignable: bool) -> Local {
        Local { owed, reassignable, ..Local::base(name) }
    }

    /// A caught value.
    fn catch(name: Symbol, owed: Obligations) -> Local {
        Local::param(name, owed, false)
    }

    fn binder_owing(name: Symbol, owed: Obligations, source: PatternBinderSource) -> Local {
        Local { owed, pattern_binder_source: Some(source), ..Local::base(name) }
    }

    fn func(name: Symbol, stmt: HirId<HirStmt>) -> Local {
        Local { fn_decl: true, resolved_callable: Some(stmt.into()), ..Local::base(name) }
    }

    fn value(name: Symbol, owed: Obligations, reassignable: bool, assigned: bool, tag: TypeTag) -> Local {
        Local { owed, reassignable, assigned, tag, ..Local::base(name) }
    }
}

/// Where a narrowing applies.
#[derive(Clone, Copy, PartialEq)]
enum NarrowTarget {
    Local(usize),
    ThisField(Symbol),
    LocalField(usize, Symbol),
}

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
    owed: Obligations,
}

#[derive(Default)]
struct FnContext<'a> {
    return_shape: ReturnShape,
    return_owes: bool,
    return_unmarked: bool,
    return_mut: bool,
    return_admits: Option<Obligations>,
    /// What the declared `this` says about the receiver.
    receiver: ReceiverFacts,
    /// The function's name.
    name: Option<Symbol>,
    return_clause: Option<SourcePosition>,
    params: Vec<(Symbol, SourcePosition)>,
    /// Per parameter, whether the escape summary clears it of ever leaving the call.
    param_confined: Vec<bool>,
    /// The names this body writes.
    writes: Option<&'a HashSet<Symbol>>,
    returns_void: bool,
    in_defer: bool,
}

#[derive(Clone, Copy)]
pub(super) struct Ctx<'a> {
    hir: &'a Hir,
    bindings: &'a Bindings,
    sigs: &'a Signatures,
    force_checks: bool,
    drop_escape_refusal: bool,
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
    resolved_callees: HashMap<HirId<HirExpr>, CallableId>,
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
    fn new(hir: &'a Hir, bindings: &'a Bindings, sigs: &'a Signatures, config: RunConfig) -> Checker<'a> {
        Checker {
            ctx: Ctx { hir, bindings, sigs, force_checks: config.force_checks, drop_escape_refusal: config.drop_escape_refusal },
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

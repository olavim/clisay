//! Flow-sensitive semantic checks: what a program does on the way to each point.

mod alias;
mod barriers;
mod conform;
mod narrow;
mod returns;
mod scope;
mod walk;

use std::collections::{HashMap, HashSet};

use crate::frontend::lex::SourcePosition;
use crate::middle::bind::{Bindings, TypeLayout};
use crate::middle::diagnose::Diagnose;
use crate::middle::hir::{Hir, HirExpr, HirId, HirStmt, ReturnShape, Symbol};
use crate::middle::obligations::{Obligations, Rule, Site};
use crate::middle::signatures::{Mutability, Signatures, TypeTag};

use alias::{AliasLocal, ElementKey, MovedAt};

pub use barriers::{Barrier, Barriers, Guard, WitnessSet};

pub fn check(hir: &Hir, bindings: &Bindings, sigs: &Signatures) -> Result<Barriers, anyhow::Error> {
    let mut checker = Checker::new(hir, bindings, sigs);
    checker.stmt(&hir.get_root())?;
    checker.out.witness_decls = sigs.object_witnesses().map(|(_, id)| id).collect();
    Ok(checker.out)
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
    Bad { obligations: Obligations, definite: bool, container: bool },
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

/// A tracked binding in the current function frame.
struct Local {
    name: Symbol,
    /// The obligations this binding owes. Reading it yields those obligations until it is narrowed.
    owed: Obligations,
    mutable: bool,
    /// Whether the binding is provably assigned on the current path.
    assigned: bool,
    tag: TypeTag,
    func: Option<HirId<HirStmt>>,
    binder: bool,
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
    /// Everything the one-writer rule tracks about this binding, which `alias` owns.
    alias: AliasLocal,
}

impl Local {
    /// A binding with every fact at its neutral default. Each named constructor overrides only the
    /// fields that distinguish it, so a new field is added here once.
    fn base(name: Symbol) -> Local {
        Local { name, owed: Obligations::new(), mutable: false, assigned: true, tag: TypeTag::Unknown, func: None, binder: false, container: false, param: false, handled: Obligations::new(), discharged: Obligations::new(), field_discharged: HashMap::new(), site: None, alias: AliasLocal::default() }
    }

    fn param(name: Symbol, owed: Obligations, mutable: bool) -> Local {
        Local { owed, mutable, ..Local::base(name) }
    }

    fn catch(name: Symbol, owed: Obligations, mutable: bool) -> Local {
        Local::param(name, owed, mutable)
    }

    fn binder_owing(name: Symbol, owed: Obligations) -> Local {
        Local { owed, binder: true, ..Local::base(name) }
    }

    fn func(name: Symbol, stmt: HirId<HirStmt>) -> Local {
        Local { func: Some(stmt), ..Local::base(name) }
    }

    fn value(name: Symbol, owed: Obligations, mutable: bool, assigned: bool, tag: TypeTag) -> Local {
        Local { owed, mutable, assigned, tag, ..Local::base(name) }
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
    /// The names this body writes, so a read-only capture is told from a writing one.
    writes: Option<&'a HashSet<Symbol>>,
}

struct Checker<'a> {
    /// What the pass hands to codegen.
    out: Barriers,
    hir: &'a Hir,
    bindings: &'a Bindings,
    sigs: &'a Signatures,
    locals: Vec<Local>,
    /// The start index in `locals` of the current function frame. Value reads only see
    /// bindings at or above this, so a closure does not read an enclosing local's flow state.
    frame_start: usize,
    /// The enclosing type's name while checking its methods, for `this` typing and field layout.
    current_type: Option<HirId<HirStmt>>,
    /// While checking a factory body, where writing an immutable field is its initialization, not
    /// a mutation.
    checking_factory: bool,
    /// Each resolved call site's callee, keyed by the callee node. A later walk of the same value
    /// reads it rather than resolving a receiver's type again.
    resolved_callees: HashMap<HirId<HirExpr>, HirId<HirStmt>>,
    /// The obligations discharged per `this` field on the current path. Keyed by name rather than
    /// by slot, so it is scoped to the frame instead of to a local.
    this_narrowed: HashMap<Symbol, Obligations>,
    current_trait_surface: Option<HashSet<Symbol>>,
    /// The function currently being checked.
    fn_ctx: FnContext<'a>,
    /// Set while descending into a `mut` construction.
    pub(super) mut_construction: bool,
}

impl<'a> Diagnose for Checker<'a> {
    fn hir(&self) -> &Hir { self.hir }
}

impl<'a> Checker<'a> {
    fn new(hir: &'a Hir, bindings: &'a Bindings, sigs: &'a Signatures) -> Checker<'a> {
        Checker {
            hir,
            bindings,
            sigs,
            resolved_callees: HashMap::new(),
            locals: Vec::new(),
            frame_start: 0,
            current_type: None,
            checking_factory: false,
            this_narrowed: HashMap::new(),
            current_trait_surface: None,
            fn_ctx: FnContext::default(),
            out: Barriers::default(),
            mut_construction: false,
        }
    }

    /// The init of the type a callee names.
    fn this_tag(&self) -> TypeTag {
        if self.current_trait_surface.is_some() {
            TypeTag::SelfType
        } else {
            self.current_type.map_or(TypeTag::Unknown, TypeTag::Concrete)
        }
    }

    fn this_typed(&self) -> Typed {
        let receiver = &self.fn_ctx.receiver;
        let flow = if receiver.owed.is_empty() {
            Flow::Clean
        } else {
            Flow::Bad { obligations: receiver.owed.clone(), definite: false, container: false }
        };
        Typed::of(flow, self.this_tag()).with_mutability(receiver.mutability)
    }

    /// A value owing `opt`. `definite` marks a known-null value versus a possibly-null one.
    fn opt_flow(&self, definite: bool) -> Flow {
        Flow::Bad { obligations: Obligations::from([self.sigs.opt]), definite, container: false }
    }

    fn opt_set(&self, nullable: bool) -> Obligations {
        if nullable { Obligations::from([self.sigs.opt]) } else { Obligations::new() }
    }

    fn constructor_init(&self, callee: &HirId<HirExpr>) -> Option<HirId<HirStmt>> {
        let type_stmt = self.sigs.type_named(self.hir, self.bindings, callee)?;
        let HirStmt::Type(decl) = self.hir.get(&type_stmt) else { return None };
        Some(decl.init)
    }

    /// Whether a type has a factory. A factory-less type (not all-defaulted, no `init`) is built
    /// only by brace, so `T(..)` cannot construct it.
    fn type_has_factory(&self, decl: &HirId<HirStmt>) -> bool {
        match self.hir.get(decl) {
            // A built-in's factory is the native one the VM installed, which no declaration shows.
            HirStmt::Type(decl) if decl.builtin.is_some() => true,
            HirStmt::Type(decl) => matches!(self.hir.get(&decl.init), HirStmt::Fn(_)),
            _ => false,
        }
    }

    /// The nearest binding of `name` across all frames. Functions resolve across frames so a
    /// nested body can call an enclosing function.
    fn func_of(&self, name: Symbol) -> Option<HirId<HirStmt>> {
        self.locals.iter().rev().find(|l| l.name == name).and_then(|l| l.func)
    }

    /// A binding's display name, hiding the `$` prefix of a synthetic field-local. A source name
    /// cannot start with `$`, so only a field-local is affected.
    fn binding_text(&self, name: Symbol) -> String {
        let text = self.hir.text(name);
        text.strip_prefix('$').unwrap_or(text).to_string()
    }

    /// Whether a binding is a factory's synthetic field-local. Its `$` prefix cannot occur in a
    /// source name, so a diagnostic can present it as the field it stands for.
    fn is_field_local(&self, name: Symbol) -> bool {
        self.hir.text(name).starts_with('$')
    }

    /// The layout of a tracked concrete type.
    fn layout_of(&self, decl: &HirId<HirStmt>) -> Option<&'a TypeLayout> {
        self.bindings.layout_of_decl(decl)
    }

    /// A declaration's name.
    fn type_name_of(&self, decl: &HirId<HirStmt>) -> Option<Symbol> {
        match self.hir.get(decl) {
            HirStmt::Type(decl) | HirStmt::Trait(decl) => Some(decl.name),
            _ => None,
        }
    }

    /// Inside a trait body, `this` reaches only the surface the trait declares or requires.
    fn trait_member(&self, name: &str, node: &HirId<HirExpr>) -> Result<Typed, anyhow::Error> {
        let in_surface = self.current_trait_surface.as_ref().is_some_and(|surface| surface.iter().any(|m| self.hir.text(*m) == name));
        if !in_surface {
            return Err(self.error_help(format!("'{}' is not declared or required by this trait", name), node, "declare it or add a 'req'"));
        }
        Ok(Typed::unknown())
    }

}

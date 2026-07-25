//! Builds a `Signatures` table. Records every function and type-member signature
//! and infers each function's return tag.

mod collect;
mod escape;
mod fields;
mod propagate;
mod returns;
mod walk;

use std::collections::{HashMap, HashSet};

use crate::middle::bind::Bindings;
use crate::middle::hir::{Capability, Hir, HirExpr, HirId, HirStmt, ObligationRule, Symbol};

/// A function's return: the obligations its result carries and whether any path returns a value.
#[derive(Clone, Default)]
pub struct RetSig {
    pub obligations: HashSet<Symbol>,
    pub void: bool,
}

/// A function's per-parameter obligation set and its return signature.
pub struct FnSig {
    pub param_clauses: Vec<HashSet<Symbol>>,
    pub param_markers: Vec<Capability>,
    pub ret: RetSig,
}

/// The value-mutability a value carries as it flows: the capability lattice the check pass tracks,
/// distinct from `Capability`, the syntactic `mut`/`*mut` marker a clause declares.
#[derive(Clone, Copy, PartialEq, Eq, Default)]
pub enum Mutability {
    /// A `mut` parameter or a `: mut` return: the value may be mutated.
    Mutable,
    /// Frozen, or an untagged return auto-frozen on the way out.
    Immutable,
    #[default]
    Unknown,
}

impl Mutability {
    /// The mutability a parameter's clause marker grants its binding.
    pub fn param(capability: Capability) -> Mutability {
        if capability.is_mut() { Mutability::Mutable } else { Mutability::Immutable }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub enum TypeTag {
    Concrete(Symbol),
    SelfType,
    Unknown,
}

impl TypeTag {
    pub(crate) fn resolve(&self, receiver: &TypeTag) -> TypeTag {
        match self {
            TypeTag::SelfType => receiver.clone(),
            other => other.clone(),
        }
    }
}

/// How a running program tells that a slot still owes an obligation. `null` is the built-in value
/// witness; a type witness is tested by tag, a trait witness by trait-set membership.
#[derive(Clone)]
pub enum Witness {
    Null,
    Type(Symbol),
    Trait(Symbol),
}

pub struct Signatures {
    pub(crate) opt: Symbol,
    pub(crate) fails: Symbol,
    /// Each obligation's witness. Built-ins are seeded here; user obligations extend it.
    pub(crate) witnesses: HashMap<Symbol, Witness>,
    /// Each user obligation's rule.
    pub(crate) rules: HashMap<Symbol, ObligationRule>,

    // Per-function facts, keyed by the function's statement.
    pub(crate) fns: HashMap<HirId<HirStmt>, FnSig>,
    pub(crate) ret_tags: HashMap<HirId<HirStmt>, TypeTag>,
    pub(crate) ret_mut: HashMap<HirId<HirStmt>, Mutability>,
    /// Per parameter, whether the body persists its argument. A free function, constructor, or
    /// `this.method` call is resolved. An opaque call is deferred to the runtime borrow check.
    pub(crate) param_escapes: HashMap<HirId<HirStmt>, Vec<bool>>,
    /// Per parameter, whether the body mutates its argument in place. A read-only borrow leaves
    /// its argument untouched, so a mutable value is admitted only where this is false.
    pub(crate) param_mutates: HashMap<HirId<HirStmt>, Vec<bool>>,
    /// Per lambda parameter, whether the body persists its argument.
    pub(crate) lambda_param_escapes: HashMap<HirId<HirExpr>, Vec<bool>>,
    /// Names each function's body writes, either persisting or mutating them. A closure that only
    /// reads a captured name borrows it, so its enclosing binding stays live.
    pub(crate) writes: HashMap<HirId<HirStmt>, HashSet<Symbol>>,
    /// The same write set for each lambda, keyed by its expression id.
    pub(crate) lambda_writes: HashMap<HirId<HirExpr>, HashSet<Symbol>>,

    // Name-to-declaration lookups.
    pub(crate) types_by_name: HashMap<Symbol, HirId<HirStmt>>,
    pub(crate) fns_by_name: HashMap<Symbol, HirId<HirStmt>>,
    pub(crate) methods_by_type: HashMap<(Symbol, Symbol), HirId<HirStmt>>,
    /// The type each method belongs to.
    pub(crate) method_owner: HashMap<HirId<HirStmt>, Symbol>,

    // Per-type field facts.
    /// Type name to the fields its `init` assigns directly.
    pub(crate) init_fields: HashMap<Symbol, HashSet<Symbol>>,
    /// Type name to the fields its methods assign, each mapped to the assigning node.
    pub(crate) method_field_assigns: HashMap<Symbol, HashMap<Symbol, HirId<HirExpr>>>,
}

impl Signatures {
    fn new(opt: Symbol, fails: Symbol) -> Signatures {
        Signatures {
            opt,
            fails,
            witnesses: HashMap::from([(opt, Witness::Null)]),
            rules: HashMap::new(),
            fns: HashMap::new(),
            ret_tags: HashMap::new(),
            ret_mut: HashMap::new(),
            param_escapes: HashMap::new(),
            param_mutates: HashMap::new(),
            lambda_param_escapes: HashMap::new(),
            writes: HashMap::new(),
            lambda_writes: HashMap::new(),
            types_by_name: HashMap::new(),
            fns_by_name: HashMap::new(),
            methods_by_type: HashMap::new(),
            method_owner: HashMap::new(),
            init_fields: HashMap::new(),
            method_field_assigns: HashMap::new(),
        }
    }

    /// Whether `name` names a declared type.
    pub(crate) fn is_type(&self, name: Symbol) -> bool {
        self.types_by_name.contains_key(&name)
    }

    /// The witness of an obligation, when one is known.
    pub(crate) fn witness(&self, obligation: Symbol) -> Option<&Witness> {
        self.witnesses.get(&obligation)
    }

    /// Every registered object witness as `(obligation, witness type/trait name)`. The null
    /// witness of `opt` is excluded, since it is tested by the null op, not the `is` test.
    pub(crate) fn object_witnesses(&self) -> impl Iterator<Item = (Symbol, Symbol)> + '_ {
        self.witnesses.iter().filter_map(|(ob, w)| match w {
            Witness::Type(name) | Witness::Trait(name) => Some((*ob, *name)),
            Witness::Null => None,
        })
    }

    /// Whether `func` persists the argument to its parameter at position `param`.
    /// An unresolved function is assumed not to; the runtime borrow check covers it instead.
    pub(crate) fn param_escapes_at(&self, func: &HirId<HirStmt>, param: usize) -> bool {
        self.param_escapes.get(func).and_then(|v| v.get(param)).copied().unwrap_or(false)
    }

    /// Whether `func` mutates the argument to its parameter at position `param` in place.
    /// An unresolved function is assumed not to; the runtime borrow check covers it instead.
    pub(crate) fn param_mutates_at(&self, func: &HirId<HirStmt>, param: usize) -> bool {
        self.param_mutates.get(func).and_then(|v| v.get(param)).copied().unwrap_or(false)
    }

    /// Whether an obligation's rule is `no persist`: usable in place, but not persistable.
    pub(crate) fn is_no_persist(&self, obligation: Symbol) -> bool {
        matches!(self.rules.get(&obligation), Some(ObligationRule::NoPersist))
    }

    pub(crate) fn obligation_for_witness(&self, name: Symbol) -> Option<Symbol> {
        self.witnesses.iter().find_map(|(ob, w)| match w {
            Witness::Type(t) | Witness::Trait(t) if *t == name => Some(*ob),
            _ => None,
        })
    }

    /// Whether `name` is the type witness of some obligation.
    pub(crate) fn is_witness_type(&self, name: Symbol) -> bool {
        self.witnesses.values().any(|w| matches!(w, Witness::Type(t) if *t == name))
    }

    /// The type a callee names, when it is an identifier naming a declared type.
    pub(crate) fn type_named(&self, hir: &Hir, callee: &HirId<HirExpr>) -> Option<Symbol> {
        match hir.get(callee) {
            HirExpr::Identifier(name) if self.is_type(*name) => Some(*name),
            _ => None,
        }
    }
}

/// Collects the program's signatures and inferred return type tags.
pub fn collect(hir: &Hir, bindings: &Bindings) -> Signatures {
    let opt = hir.symbol_of("opt").expect("lowering interns the opt obligation");
    let fails = hir.symbol_of("fails").expect("lowering interns the fails obligation");
    let err = hir.symbol_of("Err");
    let mut sigs = Signatures::new(opt, fails);
    if let Some(err) = err {
        sigs.witnesses.insert(fails, Witness::Type(err));
    }
    let mut collector = Collector { hir, bindings, opt, fails, err, sigs, returns: HashMap::new() };
    collector.stmt(&hir.get_root());
    collector.register_obligations();
    collector.collect_all_returns();
    collector.infer_ret_tags();
    collector.infer_ret_mut();
    collector.infer_propagated();
    collector.infer_param_escapes();
    collector.infer_lambda_escapes();
    collector.sigs
}

struct Collector<'a> {
    hir: &'a Hir,
    bindings: &'a Bindings,
    opt: Symbol,
    fails: Symbol,
    err: Option<Symbol>,
    sigs: Signatures,
    returns: HashMap<HirId<HirStmt>, Vec<HirId<HirExpr>>>,
}

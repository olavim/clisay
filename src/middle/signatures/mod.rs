//! Builds a `Signatures` table. Records every function and type-member signature
//! and infers each function's return tag.

mod collect;
mod escape;
mod propagate;
mod returns;

use std::collections::{HashMap, HashSet};

use crate::middle::bind::Bindings;
use crate::middle::hir::{builtin_obligation_rules, Capability, Hir, HirExpr, HirFnDecl, HirId, HirLiteral, HirMatcher, HirStmt, HirTypeDecl, ObligationRules, Symbol, TypeId};
use crate::middle::obligations::Obligations;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum CallableId {
    Fn(HirId<HirStmt>),
    Lambda(HirId<HirExpr>),
}

impl CallableId {
    fn sort_key(&self) -> (u8, usize) {
        match self {
            CallableId::Fn(s) => (0, s.index()),
            CallableId::Lambda(e) => (1, e.index()),
        }
    }
}

impl From<HirId<HirStmt>> for CallableId {
    fn from(id: HirId<HirStmt>) -> CallableId { CallableId::Fn(id) }
}

impl From<HirId<HirExpr>> for CallableId {
    fn from(id: HirId<HirExpr>) -> CallableId { CallableId::Lambda(id) }
}

impl From<&HirId<HirStmt>> for CallableId {
    fn from(id: &HirId<HirStmt>) -> CallableId { CallableId::Fn(*id) }
}

impl From<&HirId<HirExpr>> for CallableId {
    fn from(id: &HirId<HirExpr>) -> CallableId { CallableId::Lambda(*id) }
}

/// What one parameter's argument undergoes in the body it is passed to.
#[derive(Clone, Copy, Default, PartialEq)]
pub(crate) struct ParamFact {
    /// The body keeps the argument past the call.
    pub escapes: bool,
    /// The body puts it where the caller cannot reach it again. A return is excluded, since that
    /// hands the value back to the caller that supplied it.
    pub escapes_beyond_return: bool,
    /// The body mutates it in place. A read-only borrow leaves its argument untouched, so a mutable
    /// value is admitted only where this is false.
    pub mutates: bool,
    /// The body's result keeps the argument reachable, so a caller that persists the result persists
    /// the argument. Read while the rows are built, to see a borrow through a call.
    pub hands_back: bool,
    /// The body's result may be the argument itself rather than something holding it. Binding the
    /// result then names that argument a second time.
    pub hands_back_itself: bool,
    /// The body stores it where a second name can write it.
    pub stored_away: bool,
    /// The param is borrowed and the body might hand it to a call that retains it.
    pub needs_borrow_mark: bool,
    pub escape_site: Option<HirId<HirExpr>>,
}

/// A function's return.
#[derive(Clone, Default)]
pub struct RetSig {
    pub obligations: Obligations,
    pub void: bool,
}

pub struct FnSig {
    pub receiver_marker: Option<Capability>,
    pub param_clauses: Vec<Obligations>,
    pub param_markers: Vec<Capability>,
    pub ret: RetSig,
}

#[derive(Clone, Copy, PartialEq, Eq, Default)]
pub enum Mutability {
    Mutable,
    Immutable,
    #[default]
    Unknown,
}

impl Mutability {
    pub fn of(capability: Capability) -> Mutability {
        if capability.is_mut() { Mutability::Mutable } else { Mutability::Unknown }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub enum TypeTag {
    /// The declaration of the value's type.
    Concrete(HirId<HirStmt>),
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
    Type(TypeId),
    Trait(TypeId),
}

pub struct Signatures {
    pub(crate) opt: Symbol,
    pub(crate) fails: Symbol,
    /// Each obligation's witness. Built-ins are seeded here; user obligations extend it.
    pub(crate) witnesses: HashMap<Symbol, Witness>,
    /// Each user obligation's rule.
    pub(crate) rules: HashMap<Symbol, ObligationRules>,
    pub(crate) fns: HashMap<CallableId, FnSig>,
    pub(crate) ret_tags: HashMap<CallableId, TypeTag>,
    pub(crate) ret_mut: HashMap<CallableId, Mutability>,
    pub(crate) params: HashMap<CallableId, Vec<ParamFact>>,
    pub(crate) returns_upvalues: HashMap<CallableId, Vec<Symbol>>,
    /// Names each callable's body writes.
    pub(crate) writes: HashMap<CallableId, HashSet<Symbol>>,
    /// Every name some body rebinds through a capture or a global.
    pub(crate) any_rebind: HashSet<Symbol>,

    // Name-to-declaration lookups.
    /// Every declaration of each type name.
    pub(crate) types_by_name: HashMap<Symbol, Vec<HirId<HirStmt>>>,
    /// The trait declarations each name stands for.
    pub(crate) traits_by_name: HashMap<Symbol, Vec<HirId<HirStmt>>>,
    /// The declaration each identity stands for.
    pub(crate) decls_by_id: HashMap<TypeId, HirId<HirStmt>>,
    pub(crate) fns_by_name: HashMap<Symbol, HirId<HirStmt>>,
    pub(crate) methods_by_type: HashMap<(HirId<HirStmt>, Symbol), HirId<HirStmt>>,
    /// The type declaration each method belongs to.
    pub(crate) method_owner: HashMap<HirId<HirStmt>, HirId<HirStmt>>,
}

impl Signatures {
    fn new(opt: Symbol, fails: Symbol) -> Signatures {
        Signatures {
            returns_upvalues: HashMap::new(),
            opt,
            fails,
            witnesses: HashMap::from([(opt, Witness::Null)]),
            rules: HashMap::new(),
            fns: HashMap::new(),
            ret_tags: HashMap::new(),
            ret_mut: HashMap::new(),
            params: HashMap::new(),
            writes: HashMap::new(),
            any_rebind: HashSet::new(),
            types_by_name: HashMap::new(),
            traits_by_name: HashMap::new(),
            decls_by_id: HashMap::new(),
            fns_by_name: HashMap::new(),
            methods_by_type: HashMap::new(),
            method_owner: HashMap::new(),
        }
    }

    pub(crate) fn ret_tag_of(&self, callable: impl Into<CallableId>) -> Option<&TypeTag> {
        self.ret_tags.get(&callable.into())
    }

    pub(crate) fn ret_mut_of_callable(&self, callable: impl Into<CallableId>) -> Mutability {
        self.ret_mut.get(&callable.into()).copied().unwrap_or(Mutability::Unknown)
    }

    /// The declaration a callable id stands for, whichever spelling wrote it.
    pub(crate) fn decl_of<'h>(hir: &'h Hir, callable: CallableId) -> Option<&'h HirFnDecl> {
        match callable {
            CallableId::Fn(stmt) => match hir.get(&stmt) {
                HirStmt::Fn(decl) => Some(decl),
                _ => None,
            },
            CallableId::Lambda(expr) => match hir.get(&expr) {
                HirExpr::Literal(HirLiteral::Lambda(decl)) => Some(decl),
                _ => None,
            },
        }
    }

    pub(crate) fn fn_sig_of(&self, callable: impl Into<CallableId>) -> Option<&FnSig> {
        self.fns.get(&callable.into())
    }

    pub(crate) fn is_type(&self, name: Symbol) -> bool {
        self.types_by_name.contains_key(&name)
    }

    pub(crate) fn type_decl(&self, name: Symbol) -> Option<HirId<HirStmt>> {
        match self.types_by_name.get(&name) {
            Some(decls) if decls.len() == 1 => decls.first().copied(),
            _ => None,
        }
    }

    pub(crate) fn trait_decl(&self, name: Symbol) -> Option<HirId<HirStmt>> {
        match self.traits_by_name.get(&name) {
            Some(decls) if decls.len() == 1 => decls.first().copied(),
            _ => None,
        }
    }

    pub(crate) fn type_decl_of_id(&self, id: TypeId) -> Option<HirId<HirStmt>> {
        self.decls_by_id.get(&id).copied()
    }

    pub(crate) fn obligation_for_witness_id(&self, id: TypeId) -> Option<Symbol> {
        self.witnesses.iter().find_map(|(obligation, witness)| match witness {
            Witness::Type(w) | Witness::Trait(w) => (*w == id).then_some(*obligation),
            Witness::Null => None,
        })
    }

    pub(crate) fn obligations_witnessed_by_decl(&self, decl: &HirTypeDecl) -> Obligations {
        let mut out: Obligations = self.obligation_for_witness_id(decl.id).into_iter().collect();
        for (_, id) in &decl.provides {
            out.extend(self.obligation_for_witness_id(*id));
        }
        out
    }

    pub(crate) fn witness_of(&self, obligation: Symbol) -> Option<&Witness> {
        self.witnesses.get(&obligation)
    }

    /// Every obligation witnessed by an object, in a stable order.
    pub(crate) fn object_witnesses(&self) -> impl Iterator<Item = (Symbol, TypeId)> + '_ {
        let mut out: Vec<(Symbol, TypeId)> = self.witnesses.iter().filter_map(|(ob, w)| match w {
            Witness::Type(id) | Witness::Trait(id) => Some((*ob, *id)),
            Witness::Null => None,
        }).collect();
        out.sort_unstable();
        out.into_iter()
    }

    fn param_fact(&self, func: impl Into<CallableId>, param: usize) -> ParamFact {
        self.params.get(&func.into()).and_then(|row| row.get(param)).copied().unwrap_or_default()
    }

    pub(crate) fn param_escapes_at(&self, func: impl Into<CallableId>, param: usize) -> bool {
        self.param_fact(func, param).escapes
    }

    pub(crate) fn param_stored_at(&self, func: impl Into<CallableId>, param: usize) -> bool {
        self.param_fact(func, param).stored_away
    }

    pub(crate) fn param_needs_borrow_mark_at(&self, func: impl Into<CallableId>, param: usize) -> bool {
        self.param_fact(func, param).needs_borrow_mark
    }

    pub(crate) fn escapes_beyond_return_at(&self, func: impl Into<CallableId>, param: usize) -> bool {
        self.param_fact(func, param).escapes_beyond_return
    }

    pub(crate) fn escape_site_at(&self, func: impl Into<CallableId>, param: usize) -> Option<HirId<HirExpr>> {
        self.param_fact(func, param).escape_site
    }

    pub(crate) fn param_mutates_at(&self, func: impl Into<CallableId>, param: usize) -> bool {
        self.param_fact(func, param).mutates
    }

    pub(crate) fn obligation_rules_of(&self, obligation: Symbol) -> ObligationRules {
        self.rules.get(&obligation).copied().unwrap_or_default()
    }

}

#[derive(Clone, Copy)]
pub(crate) struct Resolved<'a> {
    pub(crate) hir: &'a Hir,
    pub(crate) bindings: &'a Bindings,
    pub(crate) sigs: &'a Signatures,
}

impl<'a> Resolved<'a> {
    /// The obligations a matcher admits on the value it matches: one per bindingless witness among
    /// its alternatives. `Node | null` admits `opt`, so a name bound to that value owes `opt`.
    pub(crate) fn admitted_obligations(&self, matcher: &HirId<HirMatcher>) -> Obligations {
        match self.hir.get(matcher) {
            HirMatcher::Or(alternatives) => alternatives.iter()
                .flat_map(|alt| self.bindingless_witness_obligations(alt))
                .collect(),
            HirMatcher::Type { .. } => self.obligations_witnessed_by_test(matcher),
            HirMatcher::As(_, inner) => self.admitted_obligations(inner),
            _ => Obligations::new(),
        }
    }

    /// The obligations a bindingless alternative witnesses. `null` witnesses `opt`. A bare witness
    /// type witnesses its own. A non-witness alternative yields nothing.
    pub(crate) fn bindingless_witness_obligations(&self, alt: &HirId<HirMatcher>) -> Obligations {
        match self.hir.get(alt) {
            HirMatcher::Literal(HirLiteral::Null) => Obligations::from([self.sigs.opt]),
            HirMatcher::Type { shape: None, .. } => self.obligations_witnessed_by_test(alt),
            _ => Obligations::new(),
        }
    }

    /// The obligations the declaration a type test names witnesses.
    fn obligations_witnessed_by_test(&self, matcher: &HirId<HirMatcher>) -> Obligations {
        let Some(stmt) = self.bindings.type_ref(matcher) else { return Obligations::new() };
        match self.hir.get(&stmt) {
            HirStmt::Type(decl) | HirStmt::Trait(decl) => self.sigs.obligations_witnessed_by_decl(decl),
            _ => Obligations::new(),
        }
    }

    /// The declaration a callee names, when it is an identifier naming a declared type.
    pub(crate) fn type_named(&self, callee: &HirId<HirExpr>) -> Option<HirId<HirStmt>> {
        let decl = self.bindings.expr_type(callee)?;
        matches!(self.hir.get(&decl), HirStmt::Type(_)).then_some(decl)
    }

    /// The tag a construction on this callee produces. A callee naming no type says nothing.
    pub(crate) fn constructed_tag(&self, callee: &HirId<HirExpr>) -> TypeTag {
        self.type_named(callee).map_or(TypeTag::Unknown, TypeTag::Concrete)
    }
}

/// Collects the program's signatures and inferred return type tags.
pub fn collect(hir: &Hir, bindings: &Bindings) -> Signatures {
    let opt = hir.symbol_of("opt").expect("lowering interns the opt obligation");
    let fails = hir.symbol_of("fails").expect("lowering interns the fails obligation");
    let err = hir.symbol_of("Err");
    let this = hir.symbol_of("this").expect("lowering interns the receiver name");
    let mut sigs = Signatures::new(opt, fails);
    for (name, sym) in [("opt", opt), ("fails", fails)] {
        if let Some(rules) = builtin_obligation_rules(name) {
            sigs.rules.insert(sym, rules);
        }
    }
    let mut collector = Collector { hir, bindings, opt, fails, err, this, sigs, returns: HashMap::new(), lambda_captures: HashMap::new() };
    collector.stmt(&hir.get_root());

    if let Some(id) = err.and_then(|err| collector.sigs.type_decl(err)).map(|decl| match hir.get(&decl) {
        HirStmt::Type(decl) => decl.id,
        _ => unreachable!("Err names a type declaration"),
    }) {
        collector.sigs.witnesses.insert(fails, Witness::Type(id));
    }

    collector.register_obligations();
    collector.admit_pattern_obligations();
    collector.collect_all_returns();
    collector.infer_ret_tags();
    collector.infer_ret_mut();
    collector.infer_propagated();
    collector.collect_lambda_captures();
    collector.infer_escape_summaries();
    collector.sigs
}

struct Collector<'a> {
    hir: &'a Hir,
    bindings: &'a Bindings,
    opt: Symbol,
    fails: Symbol,
    err: Option<Symbol>,
    /// The receiver's reserved name, which the escape rows track it under.
    this: Symbol,
    sigs: Signatures,
    returns: HashMap<CallableId, Vec<HirId<HirExpr>>>,
    /// Each lambda's captured names, resolved once so a closure mentioned many times is walked once.
    lambda_captures: HashMap<HirId<HirExpr>, Vec<Symbol>>,
}

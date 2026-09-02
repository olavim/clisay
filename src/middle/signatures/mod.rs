//! Builds a `Signatures` table. Records every function and type-member signature
//! and infers each function's return tag.

mod collect;
mod propagate;
mod returns;

use std::collections::HashMap;

use crate::middle::bind::Bindings;
use crate::middle::hir::{builtin_obligation_rules, Hir, HirExpr, HirFnDecl, HirId, HirLiteral, HirMatcher, HirStmt, HirTypeDecl, ObligationRules, Symbol, TypeId};
use crate::middle::obligations::Obligations;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum CallableId {
    Fn(HirId<HirStmt>),
    Lambda(HirId<HirExpr>),
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

#[derive(Clone, Default)]
pub struct RetSig {
    pub obligations: Obligations,
    pub void: bool,
}

pub struct FnSig {
    pub param_clauses: Vec<Obligations>,
    pub ret: RetSig,
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

#[derive(Clone)]
pub enum Witness {
    Null,
    Type(TypeId),
    Trait(TypeId),
}

pub struct Signatures {
    pub(crate) opt: Symbol,
    pub(crate) fails: Symbol,
    pub(crate) witnesses: HashMap<Symbol, Witness>,
    pub(crate) obligation_rules: HashMap<Symbol, ObligationRules>,
    pub(crate) fns: HashMap<CallableId, FnSig>,
    pub(crate) ret_tags: HashMap<CallableId, TypeTag>,
    pub(crate) types_by_name: HashMap<Symbol, Vec<HirId<HirStmt>>>,
    pub(crate) traits_by_name: HashMap<Symbol, Vec<HirId<HirStmt>>>,
    pub(crate) decls_by_id: HashMap<TypeId, HirId<HirStmt>>,
    pub(crate) fns_by_name: HashMap<Symbol, HirId<HirStmt>>,
    pub(crate) methods_by_type: HashMap<(HirId<HirStmt>, Symbol), HirId<HirStmt>>,
    pub(crate) method_owner: HashMap<HirId<HirStmt>, HirId<HirStmt>>,
}

impl Signatures {
    fn new(opt: Symbol, fails: Symbol) -> Signatures {
        Signatures {
            opt,
            fails,
            witnesses: HashMap::from([(opt, Witness::Null)]),
            obligation_rules: HashMap::new(),
            fns: HashMap::new(),
            ret_tags: HashMap::new(),
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

    pub(crate) fn obligation_rules_of(&self, obligation: Symbol) -> ObligationRules {
        self.obligation_rules.get(&obligation).copied().unwrap_or_default()
    }

}

#[derive(Clone, Copy)]
pub(crate) struct Resolved<'a> {
    pub(crate) hir: &'a Hir,
    pub(crate) bindings: &'a Bindings,
    pub(crate) sigs: &'a Signatures,
}

impl<'a> Resolved<'a> {
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

    pub(crate) fn bindingless_witness_obligations(&self, alt: &HirId<HirMatcher>) -> Obligations {
        match self.hir.get(alt) {
            HirMatcher::Literal(HirLiteral::Null) => Obligations::from([self.sigs.opt]),
            HirMatcher::Type { shape: None, .. } => self.obligations_witnessed_by_test(alt),
            _ => Obligations::new(),
        }
    }

    fn obligations_witnessed_by_test(&self, matcher: &HirId<HirMatcher>) -> Obligations {
        let Some(stmt) = self.bindings.type_ref(matcher) else { return Obligations::new() };
        match self.hir.get(&stmt) {
            HirStmt::Type(decl) | HirStmt::Trait(decl) => self.sigs.obligations_witnessed_by_decl(decl),
            _ => Obligations::new(),
        }
    }

    pub(crate) fn type_named(&self, callee: &HirId<HirExpr>) -> Option<HirId<HirStmt>> {
        let decl = self.bindings.expr_type(callee)?;
        matches!(self.hir.get(&decl), HirStmt::Type(_)).then_some(decl)
    }

    pub(crate) fn constructed_tag(&self, callee: &HirId<HirExpr>) -> TypeTag {
        self.type_named(callee).map_or(TypeTag::Unknown, TypeTag::Concrete)
    }
}

/// Collects the program's signatures and inferred return type tags.
pub fn collect(hir: &Hir, bindings: &Bindings) -> Signatures {
    let opt = hir.symbol_of("opt").expect("lowering interns the opt obligation");
    let fails = hir.symbol_of("fails").expect("lowering interns the fails obligation");
    let err = hir.symbol_of("Err");
    let mut sigs = Signatures::new(opt, fails);
    for (name, sym) in [("opt", opt), ("fails", fails)] {
        if let Some(rules) = builtin_obligation_rules(name) {
            sigs.obligation_rules.insert(sym, rules);
        }
    }
    let mut collector = Collector { hir, bindings, opt, fails, err, sigs, returns: HashMap::new() };
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
    collector.infer_propagated();
    collector.sigs
}

struct Collector<'a> {
    hir: &'a Hir,
    bindings: &'a Bindings,
    opt: Symbol,
    fails: Symbol,
    err: Option<Symbol>,
    sigs: Signatures,
    returns: HashMap<CallableId, Vec<HirId<HirExpr>>>,
}

//! The high-level IR (HIR): a post-lowering node hierarchy in which surface-only
//! constructs are unrepresentable.

use indexmap::IndexSet;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt;
use std::marker::PhantomData;

pub use crate::frontend::ast::{builtin_obligation_rules, Capability, ObligationRules, ReturnShape, Symbol};
use crate::frontend::lex::{SourcePosition, TokenType};

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    LeftShift,
    RightShift,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
    Equal,
    NotEqual,
    And,
    Or,
    BitAnd,
    BitOr,
    BitXor,
}

/// A runtime unary operator.
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum UnOp {
    Negate,
    Not,
    BitNot,
}

/// The source glyph of each operator lives in `TokenType`, so both `Display` impls route through
/// it rather than repeating the strings.
impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            BinOp::Add => TokenType::Plus,
            BinOp::Subtract => TokenType::Minus,
            BinOp::Multiply => TokenType::Multiply,
            BinOp::Divide => TokenType::Divide,
            BinOp::LeftShift => TokenType::LessLess,
            BinOp::RightShift => TokenType::GreaterGreater,
            BinOp::LessThan => TokenType::LessThan,
            BinOp::LessThanEqual => TokenType::LessEqual,
            BinOp::GreaterThan => TokenType::GreaterThan,
            BinOp::GreaterThanEqual => TokenType::GreaterEqual,
            BinOp::Equal => TokenType::EqualEqual,
            BinOp::NotEqual => TokenType::NotEqual,
            BinOp::And => TokenType::AmpAmp,
            BinOp::Or => TokenType::PipePipe,
            BinOp::BitAnd => TokenType::Amp,
            BinOp::BitOr => TokenType::Pipe,
            BinOp::BitXor => TokenType::Hat,
        })
    }
}

impl fmt::Display for UnOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            UnOp::Negate => TokenType::Minus,
            UnOp::Not => TokenType::Exclamation,
            UnOp::BitNot => TokenType::Tilde,
        })
    }
}

pub enum HirLiteral {
    Null,
    Boolean(bool),
    Number(f64),
    String(String),
    Array(Vec<HirId<HirExpr>>),
    Dict(Vec<(HirId<HirExpr>, HirId<HirExpr>)>),
    Lambda(HirFnDecl),
}

/// Where a value came from.
pub enum ValueSource<'a> {
    Name(Symbol),
    Receiver,
    Call(&'a HirId<HirExpr>, &'a [HirId<HirExpr>]),
    Element,
    Closure,
    Yields(Vec<HirId<HirExpr>>),
    Holds(Vec<HirId<HirExpr>>),
    Fresh,
}

pub enum HirExpr {
    Block(Vec<HirId<HirStmt>>),
    Unary(UnOp, HirId<HirExpr>),
    Binary(BinOp, HirId<HirExpr>, HirId<HirExpr>),
    Assign(HirId<HirExpr>, HirId<HirExpr>),
    Call(HirId<HirExpr>, Vec<HirId<HirExpr>>),
    /// `Index(target, member, is_dot)`: `is_dot` distinguishes `.name` (member)
    /// from `[expr]` (data). See `ast::Expr::Index`.
    Index(HirId<HirExpr>, HirId<HirExpr>, bool),
    Literal(HirLiteral),
    Identifier(Symbol),
    /// Brace construction `C { field: value, ... }`: the callee type expression, an unused args
    /// slot (the combined form is retired), then the brace fields.
    Construct(HirId<HirExpr>, Vec<(Symbol, HirId<HirExpr>)>),
    /// A `mut`-minted construction (`mut {}`, `mut []`, `mut Ctor()`).
    Mut(HirId<HirExpr>),
    This,
    /// Coalesce `a ?? b`: discharges `a`'s obligation set, yielding `a` when it is clean, else `b`.
    /// Short-circuit lowering is deferred to codegen.
    Coalesce(HirId<HirExpr>, HirId<HirExpr>),
    /// The `?` access-guard `a?.b` / `a?[i]`: on a bad operand the chain short-circuits to it,
    /// carrying its obligation; otherwise the access runs. `is_dot` distinguishes `.name` from
    /// `[expr]` (see `HirExpr::Index`).
    SafeAccess(HirId<HirExpr>, HirId<HirExpr>, bool),
    /// The `?` access-guard on a call `cb?(args)`: short-circuits to the callee on a bad operand,
    /// carrying its obligation; otherwise the call runs.
    SafeCall(HirId<HirExpr>, Vec<HirId<HirExpr>>),
    /// The propagate operator `a?!`: on a bad value the enclosing function returns it.
    Propagate(HirId<HirExpr>),
    /// The handler `e ?? p => h`: on a bad value binds it to `p` and yields `h`, else yields `e`.
    Handle(HirId<HirExpr>, Symbol, HirId<HirExpr>),
    /// The non-null assertion `a!`: yields the value, checking against null at runtime.
    Assert(HirId<HirExpr>),
    Match(HirId<HirExpr>, HirId<HirMatcher>),
}

/// A lowered matcher: it tests a value and binds sub-values out into names.
pub enum HirMatcher {
    /// `_`: matches anything, binds nothing.
    Wildcard,
    /// A scalar literal compared with `==`.
    Literal(HirLiteral),
    /// A bare name that binds the whole value.
    Binder(Symbol),
    /// `is T shape?` or `has T shape?`.
    Type { nominal: bool, name: Symbol, shape: Option<HirId<HirMatcher>> },
    /// A structural shape `{ k: m, ... }`.
    Shape(Vec<HirMatchField>),
    /// An array shape `[ ... ]` with at most one rest element.
    Array(Vec<HirMatchElem>),
    /// `name @ m`: binds the whole value and also matches `m`.
    As(Symbol, HirId<HirMatcher>),
    /// `a | b | ...`: alternatives tried left to right.
    Or(Vec<HirId<HirMatcher>>),
    /// `a & b & ...`: all must match.
    And(Vec<HirId<HirMatcher>>),
}

/// A field of a shape matcher `{ key: value }`.
pub struct HirMatchField {
    pub key: HirLiteral,
    pub value: HirId<HirMatcher>,
}

/// An element of an array matcher. `Rest` is `..` or `..name`, at most one per array.
pub enum HirMatchElem {
    Elem(HirId<HirMatcher>),
    Rest(Option<Symbol>),
}

impl HirMatcher {
    /// The names this matcher binds, in the left-to-right order codegen stores them.
    pub fn binders(&self, hir: &Hir) -> Vec<Symbol> {
        let mut out = Vec::new();
        self.collect_binders(hir, &mut out);
        out
    }

    /// Whether this matcher binds any name, without allocating the binder list.
    pub fn binds_anything(&self, hir: &Hir) -> bool {
        match self {
            HirMatcher::Wildcard | HirMatcher::Literal(_) => false,
            HirMatcher::Binder(_) | HirMatcher::As(..) => true,
            HirMatcher::Type { shape, .. } => shape.is_some_and(|s| hir.get(&s).binds_anything(hir)),
            HirMatcher::Shape(fields) => fields.iter().any(|f| hir.get(&f.value).binds_anything(hir)),
            HirMatcher::Array(elements) => elements.iter().any(|e| match e {
                HirMatchElem::Elem(m) => hir.get(m).binds_anything(hir),
                HirMatchElem::Rest(name) => name.is_some(),
            }),
            HirMatcher::And(parts) => parts.iter().any(|p| hir.get(p).binds_anything(hir)),
            HirMatcher::Or(alternatives) => alternatives.iter().any(|a| hir.get(a).binds_anything(hir)),
        }
    }

    /// Whether this matcher accepts every value, so a guardless arm using it is a catch-all.
    pub fn is_irrefutable(&self, hir: &Hir) -> bool {
        match self {
            HirMatcher::Wildcard | HirMatcher::Binder(_) => true,
            HirMatcher::As(_, inner) => hir.get(inner).is_irrefutable(hir),
            HirMatcher::And(parts) => parts.iter().all(|p| hir.get(p).is_irrefutable(hir)),
            HirMatcher::Or(alternatives) => alternatives.iter().any(|a| hir.get(a).is_irrefutable(hir)),
            _ => false,
        }
    }

    /// Whether matching this proves the value is not null. A bare binder, a wildcard, and a `null`
    /// literal each admit null, so they prove nothing.
    pub fn rejects_null(&self, hir: &Hir) -> bool {
        match self {
            HirMatcher::Wildcard | HirMatcher::Binder(_) => false,
            HirMatcher::Literal(HirLiteral::Null) => false,
            HirMatcher::Literal(_) => true,
            HirMatcher::Type { .. } | HirMatcher::Shape(_) | HirMatcher::Array(_) => true,
            HirMatcher::As(_, inner) => hir.get(inner).rejects_null(hir),
            HirMatcher::And(parts) => parts.iter().any(|p| hir.get(p).rejects_null(hir)),
            HirMatcher::Or(alternatives) => alternatives.iter().all(|a| hir.get(a).rejects_null(hir)),
        }
    }

    fn collect_binders(&self, hir: &Hir, out: &mut Vec<Symbol>) {
        match self {
            HirMatcher::Wildcard | HirMatcher::Literal(_) => {},
            HirMatcher::Binder(name) => out.push(*name),
            HirMatcher::Type { shape, .. } => if let Some(shape) = shape { hir.get(shape).collect_binders(hir, out) },
            HirMatcher::Shape(fields) => for field in fields { hir.get(&field.value).collect_binders(hir, out) },
            HirMatcher::Array(elements) => for element in elements {
                match element {
                    HirMatchElem::Elem(matcher) => hir.get(matcher).collect_binders(hir, out),
                    HirMatchElem::Rest(Some(name)) => out.push(*name),
                    HirMatchElem::Rest(None) => {},
                }
            },
            HirMatcher::As(name, inner) => { out.push(*name); hir.get(inner).collect_binders(hir, out); },
            HirMatcher::And(parts) => for part in parts { hir.get(part).collect_binders(hir, out) },
            // Binding alternatives agree on their names, so the first that binds stands for all. A
            // bindingless witness alternative such as `| null` contributes none.
            HirMatcher::Or(alternatives) => {
                if let Some(binding) = alternatives.iter().find(|a| hir.get(*a).binds_anything(hir)) {
                    hir.get(binding).collect_binders(hir, out);
                }
            },
        }
    }
}

/// A slot's lowered `:` clause.
#[derive(Default, Clone)]
pub struct HirSlotClause {
    pub capability: Capability,
    pub names: Vec<Symbol>,
    pub container: bool,
    pub void: bool,
    pub pos: Option<SourcePosition>,
}

impl HirSlotClause {
    /// The obligations the clause declares.
    pub fn owed(&self) -> crate::middle::obligations::Obligations {
        self.names.iter().copied().collect()
    }
}

pub struct HirFieldInit {
    pub name: Symbol,
    pub value: Option<HirId<HirExpr>>,
    /// Declared nullable with a `?` marker (`say x?`). Non-null otherwise.
    pub nullable: bool,
    /// Declared reassignable with a `var` modifier (`say var x`). Fixed otherwise.
    pub reassignable: bool,
    pub clause: HirSlotClause,
}

/// A function/method/lambda parameter: its bound identifier plus the declared nullability marker
/// and the reassignability slot reserves.
pub struct HirParam {
    pub name: HirId<HirExpr>,
    /// The parameter's pattern, when it does more than name its slot.
    pub pattern: Option<HirId<HirMatcher>>,
    /// The `name[: clause]` span.
    pub pos: SourcePosition,
    pub nullable: bool,
    pub reassignable: bool,
    pub clause: HirSlotClause,
}

pub struct HirFnDecl {
    pub name: Symbol,
    /// The `name(params): clause` signature span.
    pub sig_pos: SourcePosition,
    /// The declared `this` clause, present on an instance method and absent on a plain function.
    pub receiver: Option<HirSlotClause>,
    pub params: Vec<HirParam>,
    pub body: HirId<HirExpr>,
    /// The declared return shape (the postfix marker after the parameter list).
    pub ret: ReturnShape,
    pub clause: HirSlotClause,
}

impl HirFnDecl {
    /// Whether the return carries no annotation.
    pub(crate) fn is_unmarked(&self) -> bool {
        if self.clause.void {
            return false;
        }
        // Lowering maps a declared return clause to `Inferred` too, so the shape alone does not
        // say whether anything was annotated. An empty clause beside it is what does.
        self.ret == ReturnShape::Void
            || (self.ret == ReturnShape::Inferred
                && self.clause.names.is_empty()
                && self.clause.capability == Capability::None)
    }
}

/// A `req fn` hole's obligation signature: the contract a composer's satisfying method must meet.
pub struct HirReqFn {
    pub name: Symbol,
    /// The trait that declares this hole.
    pub trait_name: Symbol,
    /// The `name(params): clause` span in the trait.
    pub pos: SourcePosition,
    /// What the hole asks of `this`. A satisfier may ask less and not more.
    pub receiver: Option<HirSlotClause>,
    /// Each parameter's clause and `name: clause` span.
    pub params: Vec<HirReqParam>,
    /// What the return may carry. A satisfier may promise fewer obligations.
    pub ret: HirSlotClause,
}

/// A `req "var"? name (":" clause)?;` state hole a composer must fill.
pub struct HirReqMember {
    pub name: Symbol,
    /// The trait that declares this hole.
    pub trait_name: Symbol,
    /// The `var name: clause` span in the trait.
    pub pos: SourcePosition,
    /// Required to be reassignable, with the `var` marker.
    pub reassignable: bool,
    /// What the member must owe.
    pub clause: HirSlotClause,
}

/// What lowering names a parameter whose pattern binds no name for the whole value.
pub const SYNTHETIC_PARAM: &str = "$p";

/// A `req fn` parameter hole.
pub struct HirReqParam {
    pub pos: SourcePosition,
    pub clause: HirSlotClause,
    pub pattern: Option<HirId<HirMatcher>>,
}

/// A `catch (param) { … }` clause of a try statement.
pub struct HirCatchClause {
    pub param: Option<HirId<HirExpr>>,
    pub body: HirId<HirExpr>,
}

pub use crate::core::objects::TypeId;
pub use crate::frontend::ast::BuiltinType;

/// What a declaration id stands for.
pub struct TypeInfo {
    pub name: Symbol,
    pub is_trait: bool,
}

pub struct HirTypeDecl {
    pub name: Symbol,
    pub id: TypeId,
    pub init: HirId<HirStmt>,
    pub fields: IndexSet<Symbol>,
    /// Fields declared nullable with a `?` marker (`next?;`).
    pub nullable_fields: HashSet<Symbol>,
    /// Fields declared reassignable with a `var` modifier (`var count;`).
    pub var_fields: HashSet<Symbol>,
    /// Each field's declared `:` clause, for the fields that have one.
    pub field_clauses: HashMap<Symbol, HirSlotClause>,
    /// Where each field is declared.
    pub field_positions: HashMap<Symbol, SourcePosition>,
    pub methods: Vec<HirId<HirStmt>>,
    /// The `req fn` holes this composer must satisfy: its own and those of its `with` traits.
    pub req_fns: Vec<HirReqFn>,
    /// The `req <member>` holes this composer must satisfy: its own and those of its `with` traits.
    pub req_members: Vec<HirReqMember>,
    /// The declaring trait of each method in `methods`, one entry per method. `None` where the
    /// host type declares the member itself.
    pub method_traits: Vec<Option<Symbol>>,
    /// Members declared `pub`, which external code can reach. Ordered, since a surface test emits
    /// one check per name in this order.
    pub pub_members: IndexSet<Symbol>,
    /// Members declared `inner`, which a composing type can reach and external code cannot.
    pub inner_members: IndexSet<Symbol>,
    /// Per trait, that trait's **private** members mapped from their plain name to the
    /// per-trait renamed slot name (`"<Trait>.<name>"`).
    pub trait_privates: HashMap<Symbol, HashMap<Symbol, Symbol>>,
    /// Which built-in this declares, if any.
    pub builtin: Option<BuiltinType>,
    /// A trait's declared surface. Ordered, since a surface test emits one check per name in this order.
    pub surface: IndexSet<Symbol>,
    /// What this type **provides** for `x is T`: its own declaration plus every transitively
    /// `with`-mixed trait, each as its name and its declaration id.
    pub provides: Vec<(Symbol, TypeId)>,
    /// The `gives` delegations, `(field, trait name, trait declaration)`. A construction verifies
    /// each field provides its trait.
    pub gives: Vec<(Symbol, Symbol, TypeId)>,
}

/// One arm of a `match`.
pub struct HirMatchArm {
    pub matcher: HirId<HirMatcher>,
    pub guard: Option<HirId<HirExpr>>,
    pub body: HirId<HirExpr>,
}

pub enum HirStmt {
    Expression(HirId<HirExpr>),
    Return(Option<HirId<HirExpr>>),
    Throw(HirId<HirExpr>),
    Try(HirId<HirExpr>, Option<HirCatchClause>, Option<HirId<HirExpr>>),
    While(HirId<HirExpr>, HirId<HirExpr>),
    If(HirId<HirExpr>, HirId<HirExpr>, Option<HirId<HirStmt>>),
    Block(HirId<HirExpr>),
    Say(HirFieldInit),
    Fn(HirFnDecl),
    Type(Box<HirTypeDecl>),
    Trait(Box<HirTypeDecl>),
    Match(HirId<HirExpr>, Vec<HirMatchArm>),
    Nop,
}

pub enum HirNodeKind {
    Expr(HirExpr),
    Stmt(HirStmt),
    Matcher(HirMatcher),
}

pub trait HirNode: Sized {
    fn wrap(self) -> HirNodeKind;
    fn unwrap(node: &HirNodeKind) -> &Self;
}

impl HirNode for HirExpr {
    fn wrap(self) -> HirNodeKind { HirNodeKind::Expr(self) }
    fn unwrap(node: &HirNodeKind) -> &HirExpr {
        match node { HirNodeKind::Expr(expr) => expr, _ => unreachable!() }
    }
}

impl HirNode for HirStmt {
    fn wrap(self) -> HirNodeKind { HirNodeKind::Stmt(self) }
    fn unwrap(node: &HirNodeKind) -> &HirStmt {
        match node { HirNodeKind::Stmt(stmt) => stmt, _ => unreachable!() }
    }
}

impl HirNode for HirMatcher {
    fn wrap(self) -> HirNodeKind { HirNodeKind::Matcher(self) }
    fn unwrap(node: &HirNodeKind) -> &HirMatcher {
        match node { HirNodeKind::Matcher(matcher) => matcher, _ => unreachable!() }
    }
}

struct HirArenaNode {
    pos: SourcePosition,
    kind: HirNodeKind,
}

pub struct HirId<T> {
    id: usize,
    _marker: PhantomData<T>,
}

impl<T> HirId<T> {
    /// The node's index in the arena. A stable key for side-tables (e.g. resolver bindings).
    pub fn index(&self) -> usize {
        self.id
    }

    /// Rebuilds a handle from an index produced by `index`.
    pub fn from_index(id: usize) -> HirId<T> {
        HirId { id, _marker: PhantomData }
    }
}

impl<T> Copy for HirId<T> {}
impl<T> Clone for HirId<T> {
    fn clone(&self) -> HirId<T> {
        *self
    }
}

impl<T> PartialEq for HirId<T> {
    fn eq(&self, other: &HirId<T>) -> bool {
        self.id == other.id
    }
}
impl<T> Eq for HirId<T> {}
impl<T> std::hash::Hash for HirId<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

/// The type or trait an obligation names as its bad state.
pub struct ObligationWitness {
    pub name: Symbol,
    pub id: TypeId,
}

/// A user `obligation` declaration's witness and rule, kept for signatures and the check pass.
/// The declaration itself lowers to a `Nop`, so its facts live here instead.
pub struct ObligationDecl {
    /// The bad state this obligation is about. A witnessless obligation names none.
    pub witness: Option<ObligationWitness>,
    pub rules: ObligationRules,
}

/// The lowered compilation unit: a flat arena of HIR nodes plus the identifier
/// interning tables (moved out of the `Ast` during lowering).
pub struct Hir {
    nodes: Vec<HirArenaNode>,
    ident_ids: HashMap<String, u32>,
    ident_texts: Vec<String>,
    obligations: HashMap<Symbol, ObligationDecl>,
    type_info: HashMap<TypeId, TypeInfo>,
}

impl Hir {
    pub(crate) fn new(ident_ids: HashMap<String, u32>, ident_texts: Vec<String>) -> Hir {
        Hir { nodes: Vec::new(), ident_ids, ident_texts, obligations: HashMap::new(), type_info: HashMap::new() }
    }

    pub(crate) fn declare_type(&mut self, id: TypeId, name: Symbol, is_trait: bool) {
        self.type_info.insert(id, TypeInfo { name, is_trait });
    }

    pub fn type_info(&self, id: TypeId) -> Option<&TypeInfo> {
        self.type_info.get(&id)
    }

    pub fn is_trait(&self, id: TypeId) -> bool {
        self.type_info(id).is_some_and(|info| info.is_trait)
    }

    pub(crate) fn declare_obligation(&mut self, name: Symbol, witness: Option<ObligationWitness>, rules: ObligationRules) {
        self.obligations.insert(name, ObligationDecl { witness, rules });
    }

    pub fn obligations(&self) -> impl Iterator<Item = (Symbol, &ObligationDecl)> {
        self.obligations.iter().map(|(name, decl)| (*name, decl))
    }

    pub fn text(&self, symbol: Symbol) -> &str {
        &self.ident_texts[symbol.index()]
    }

    pub fn symbol_of(&self, text: &str) -> Option<Symbol> {
        self.ident_ids.get(text).copied().map(Symbol::from_raw)
    }

    pub(crate) fn intern(&mut self, text: &str) -> Symbol {
        if let Some(&id) = self.ident_ids.get(text) {
            return Symbol::from_raw(id);
        }
        let id = self.ident_texts.len() as u32;
        self.ident_texts.push(text.to_string());
        self.ident_ids.insert(text.to_string(), id);
        Symbol::from_raw(id)
    }

    pub fn get<T: HirNode>(&self, id: &HirId<T>) -> &T {
        T::unwrap(&self.nodes[id.id].kind)
    }

    pub fn ident_sym(&self, id: &HirId<HirExpr>) -> Symbol {
        match self.get(id) {
            HirExpr::Identifier(sym) => *sym,
            _ => unreachable!("node is an identifier"),
        }
    }

    pub fn pos<T>(&self, id: &HirId<T>) -> &SourcePosition {
        &self.nodes[id.id].pos
    }

    pub fn get_root(&self) -> HirId<HirStmt> {
        HirId { id: self.nodes.len() - 1, _marker: PhantomData }
    }

    pub(crate) fn stmt_at(&self, index: usize) -> Option<HirId<HirStmt>> {
        match self.nodes.get(index).map(|n| &n.kind) {
            Some(HirNodeKind::Stmt(_)) => Some(HirId { id: index, _marker: PhantomData }),
            _ => None,
        }
    }

    pub(crate) fn lambda_ids(&self) -> Vec<HirId<HirExpr>> {
        self.nodes.iter().enumerate()
            .filter(|(_, n)| matches!(&n.kind, HirNodeKind::Expr(HirExpr::Literal(HirLiteral::Lambda(_)))))
            .map(|(i, _)| HirId { id: i, _marker: PhantomData })
            .collect()
    }

    pub fn condition_pattern_binder_sources(&self, cond: &HirId<HirExpr>) -> Vec<(Symbol, HirId<HirExpr>)> {
        match self.get(cond) {
            HirExpr::Match(scrutinee, matcher) => self.get(matcher).binders(self).into_iter().map(|n| (n, *scrutinee)).collect(),
            HirExpr::Binary(BinOp::And, left, right) => {
                let mut out = self.condition_pattern_binder_sources(left);
                out.extend(self.condition_pattern_binder_sources(right));
                out
            },
            // An `or` binds the same names on both sides, so either side names their sources.
            HirExpr::Binary(BinOp::Or, left, _) => match self.condition_pattern_binders(cond).is_empty() {
                true => Vec::new(),
                false => self.condition_pattern_binder_sources(left),
            },
            _ => Vec::new(),
        }
    }

    pub fn condition_pattern_binders(&self, cond: &HirId<HirExpr>) -> Vec<Symbol> {
        match self.get(cond) {
            HirExpr::Match(_, matcher) => self.get(matcher).binders(self),
            HirExpr::Binary(BinOp::And, left, right) => {
                let mut out = self.condition_pattern_binders(left);
                out.extend(self.condition_pattern_binders(right));
                out
            },
            HirExpr::Binary(BinOp::Or, left, right) => {
                let left = self.condition_pattern_binders(left);
                let right = self.condition_pattern_binders(right);
                let same = left.len() == right.len() && left.iter().all(|name| right.contains(name));
                if same { left } else { Vec::new() }
            },
            _ => Vec::new(),
        }
    }

    pub(crate) fn value_source(&self, value: &HirId<HirExpr>) -> ValueSource<'_> {
        match self.get(value) {
            HirExpr::Identifier(name) => ValueSource::Name(*name),
            HirExpr::This => ValueSource::Receiver,
            HirExpr::Call(callee, args) | HirExpr::SafeCall(callee, args) => ValueSource::Call(callee, args),
            HirExpr::Index(..) | HirExpr::SafeAccess(..) => ValueSource::Element,
            HirExpr::Literal(HirLiteral::Lambda(_)) => ValueSource::Closure,

            HirExpr::Mut(x) | HirExpr::Assert(x) | HirExpr::Propagate(x) => ValueSource::Yields(vec![*x]),
            HirExpr::Coalesce(l, r) | HirExpr::Handle(l, _, r) => ValueSource::Yields(vec![*l, *r]),
            HirExpr::Binary(BinOp::And | BinOp::Or, l, r) => ValueSource::Yields(vec![*l, *r]),
            HirExpr::Assign(_, rhs) => ValueSource::Yields(vec![*rhs]),

            HirExpr::Construct(_, brace) => ValueSource::Holds(brace.iter().map(|(_, v)| *v).collect()),
            HirExpr::Literal(HirLiteral::Array(elems)) => ValueSource::Holds(elems.clone()),
            HirExpr::Literal(HirLiteral::Dict(pairs)) => ValueSource::Holds(pairs.iter().flat_map(|(k, v)| [*k, *v]).collect()),

            HirExpr::Unary(..) | HirExpr::Binary(..) | HirExpr::Match(..) | HirExpr::Block(_)
            | HirExpr::Literal(HirLiteral::Null | HirLiteral::Boolean(_)
                | HirLiteral::Number(_) | HirLiteral::String(_)) => ValueSource::Fresh,
        }
    }

    pub(crate) fn expression_body(&self, body: &HirId<HirExpr>) -> Option<HirId<HirExpr>> {
        match self.get(body) {
            HirExpr::Block(_) => None,
            _ => Some(*body),
        }
    }

    pub(crate) fn body_returns_a_value(&self, body: &HirId<HirExpr>) -> bool {
        self.expression_body(body).is_some() || self.definitely_returns(body)
    }

    pub(crate) fn definitely_returns(&self, body: &HirId<HirExpr>) -> bool {
        match self.get(body) {
            HirExpr::Block(stmts) => stmts.iter().any(|s| self.stmt_returns(s)),
            _ => false,
        }
    }

    pub(crate) fn stmt_returns(&self, stmt: &HirId<HirStmt>) -> bool {
        match self.get(stmt) {
            HirStmt::Return(_) | HirStmt::Throw(_) => true,
            HirStmt::Block(body) => self.definitely_returns(body),
            HirStmt::If(_, then, Some(otherwise)) => self.definitely_returns(then) && self.stmt_returns(otherwise),
            // A `finally` that returns always runs. Otherwise the try returns when its body does
            // and any catch does too.
            HirStmt::Try(body, catch, finally) => {
                if finally.as_ref().is_some_and(|f| self.definitely_returns(f)) {
                    return true;
                }
                self.definitely_returns(body) && catch.as_ref().map_or(true, |c| self.definitely_returns(&c.body))
            },
            // A match returns on every path when it cannot fall through: some guardless arm is a
            // catch-all, and every arm returns.
            HirStmt::Match(_, arms) => {
                arms.iter().any(|a| a.guard.is_none() && self.get(&a.matcher).is_irrefutable(self))
                    && arms.iter().all(|a| self.definitely_returns(&a.body))
            },
            _ => false,
        }
    }

    pub(crate) fn add<T: HirNode>(&mut self, kind: T, pos: SourcePosition) -> HirId<T> {
        self.nodes.push(HirArenaNode { kind: kind.wrap(), pos });
        HirId { id: self.nodes.len() - 1, _marker: PhantomData }
    }
}

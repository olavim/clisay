//! The high-level IR (HIR): a post-lowering node hierarchy in which surface-only
//! constructs are unrepresentable.

use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt;
use std::marker::PhantomData;

pub use crate::frontend::ast::{Capability, ObligationRule, ReturnShape, Symbol};
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
    Is(HirId<HirExpr>, Symbol),
    /// Brace construction `C(args) { field: value, ... }`: the callee type expression, the
    /// `init` args, then the brace field initializers.
    Construct(HirId<HirExpr>, Vec<HirId<HirExpr>>, Vec<(Symbol, HirId<HirExpr>)>),
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
    Has(HirId<HirExpr>, Box<HirMatcher>),
    Match(HirId<HirExpr>, Box<HirMatcher>),
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
    Type { nominal: bool, name: Symbol, shape: Option<Box<HirMatcher>> },
    /// A structural shape `{ k: m, ... }`.
    Shape(Vec<HirMatchField>),
    /// An array shape `[ ... ]` with at most one rest element.
    Array(Vec<HirMatchElem>),
    /// `name @ m`: binds the whole value and also matches `m`.
    As(Symbol, Box<HirMatcher>),
    /// `a | b | ...`: alternatives tried left to right.
    Or(Vec<HirMatcher>),
    /// `a & b & ...`: all must match.
    And(Vec<HirMatcher>),
}

/// A field of a shape matcher `{ key: value }`.
pub struct HirMatchField {
    pub key: HirLiteral,
    pub value: HirMatcher,
}

/// An element of an array matcher. `Rest` is `..` or `..name`, at most one per array.
pub enum HirMatchElem {
    Elem(HirMatcher),
    Rest(Option<Symbol>),
}

impl HirMatcher {
    /// The names this matcher binds, in the left-to-right order codegen stores them.
    pub fn binders(&self) -> Vec<Symbol> {
        let mut out = Vec::new();
        self.collect_binders(&mut out);
        out
    }

    fn collect_binders(&self, out: &mut Vec<Symbol>) {
        match self {
            HirMatcher::Wildcard | HirMatcher::Literal(_) => {},
            HirMatcher::Binder(name) => out.push(*name),
            HirMatcher::Type { shape, .. } => if let Some(shape) = shape { shape.collect_binders(out) },
            HirMatcher::Shape(fields) => for field in fields { field.value.collect_binders(out) },
            HirMatcher::Array(elements) => for element in elements {
                match element {
                    HirMatchElem::Elem(matcher) => matcher.collect_binders(out),
                    HirMatchElem::Rest(Some(name)) => out.push(*name),
                    HirMatchElem::Rest(None) => {},
                }
            },
            HirMatcher::As(name, inner) => { out.push(*name); inner.collect_binders(out); },
            HirMatcher::And(parts) => for part in parts { part.collect_binders(out) },
            // Alternatives bind the same set, so the first one's binders stand for all.
            HirMatcher::Or(alternatives) => if let Some(first) = alternatives.first() { first.collect_binders(out) },
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
}

pub struct HirFieldInit {
    pub name: Symbol,
    pub value: Option<HirId<HirExpr>>,
    /// Declared nullable with a `?` marker (`say x?`). Non-null otherwise.
    pub nullable: bool,
    /// Declared reassignable with a `mut` modifier (`say mut x`). Immutable otherwise.
    pub mutable: bool,
    pub clause: HirSlotClause,
}

/// A function/method/lambda parameter: its bound identifier plus the declared
/// nullability and mutability markers (`fn f(mut x?)`).
pub struct HirParam {
    pub name: HirId<HirExpr>,
    pub nullable: bool,
    pub mutable: bool,
    pub clause: HirSlotClause,
}

pub struct HirFnDecl {
    pub name: Symbol,
    pub params: Vec<HirParam>,
    pub body: HirId<HirExpr>,
    /// The declared return shape (the postfix marker after the parameter list).
    pub ret: ReturnShape,
    pub clause: HirSlotClause,
}

impl HirFnDecl {
    /// Whether the return carries no annotation.
    pub(crate) fn is_unmarked(&self) -> bool {
        self.ret == ReturnShape::Void && !self.clause.void
    }
}

/// A `req fn` hole's obligation signature: the contract a composer's satisfying method must meet.
pub struct HirReqFn {
    pub name: Symbol,
    /// What each parameter passes in. A satisfier must accept at least these obligations.
    pub param_clauses: Vec<HirSlotClause>,
    /// What the return may carry. A satisfier may promise fewer obligations.
    pub ret: HirSlotClause,
}

/// A `catch (param) { … }` clause of a try statement.
pub struct HirCatchClause {
    pub param: Option<HirId<HirExpr>>,
    pub mutable: bool,
    pub body: HirId<HirExpr>,
}

pub struct HirTypeDecl {
    pub name: Symbol,
    pub init: HirId<HirStmt>,
    pub fields: HashSet<Symbol>,
    /// Fields declared nullable with a `?` marker (`next?;`).
    pub nullable_fields: HashSet<Symbol>,
    /// Fields declared reassignable with a `mut` modifier (`mut count;`).
    pub mut_fields: HashSet<Symbol>,
    pub methods: Vec<HirId<HirStmt>>,
    /// The `req fn` holes this composer must satisfy: its own and those of its `with` traits.
    pub req_fns: Vec<HirReqFn>,
    /// The declaring trait of each method in `methods` (parallel), or `None` for a member
    /// the host type declares itself.
    pub method_traits: Vec<Option<Symbol>>,
    /// Members declared `pub` (externally accessible). See `ast::TypeDecl`.
    pub pub_members: HashSet<Symbol>,
    /// Members declared `inner` (visible to composing types, not external code).
    pub inner_members: HashSet<Symbol>,
    /// Per trait, that trait's **private** members mapped from their plain name to the
    /// per-trait renamed slot name (`"<Trait>.<name>"`).
    pub trait_privates: HashMap<Symbol, HashMap<Symbol, Symbol>>,
    /// For a standalone trait (`HirStmt::Trait`): its **declared surface**.
    pub surface: HashSet<Symbol>,
    /// The trait/type names this type **provides** for `x is T`: its own name plus every
    /// transitively `with`-mixed trait.
    pub provides: Vec<Symbol>,
}

/// One arm of a `match`.
pub struct HirMatchArm {
    pub matcher: HirMatcher,
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

/// A user `obligation` declaration's witness and rule, kept for signatures and the check pass.
/// The declaration itself lowers to a `Nop`, so its facts live here instead.
pub struct ObligationDecl {
    pub witness: Option<Symbol>,
    pub rule: ObligationRule,
}

/// The lowered compilation unit: a flat arena of HIR nodes plus the identifier
/// interning tables (moved out of the `Ast` during lowering).
pub struct Hir {
    nodes: Vec<HirArenaNode>,
    ident_ids: HashMap<String, u32>,
    ident_texts: Vec<String>,
    obligations: HashMap<Symbol, ObligationDecl>,
}

impl Hir {
    pub(crate) fn new(ident_ids: HashMap<String, u32>, ident_texts: Vec<String>) -> Hir {
        Hir { nodes: Vec::new(), ident_ids, ident_texts, obligations: HashMap::new() }
    }

    pub(crate) fn declare_obligation(&mut self, name: Symbol, witness: Option<Symbol>, rule: ObligationRule) {
        self.obligations.insert(name, ObligationDecl { witness, rule });
    }

    /// Every user-declared obligation, keyed by name.
    pub fn obligations(&self) -> impl Iterator<Item = (Symbol, &ObligationDecl)> {
        self.obligations.iter().map(|(name, decl)| (*name, decl))
    }

    /// The text of an interned symbol.
    pub fn text(&self, symbol: Symbol) -> &str {
        &self.ident_texts[symbol.index()]
    }

    /// The symbol for `text` if it was ever interned, else `None`.
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

    pub fn pos<T>(&self, id: &HirId<T>) -> &SourcePosition {
        &self.nodes[id.id].pos
    }

    pub fn get_root(&self) -> HirId<HirStmt> {
        HirId { id: self.nodes.len() - 1, _marker: PhantomData }
    }

    /// The binder names a condition makes live in its true branch, in store order. `&&` unions
    /// both sides. An `||` contributes a name only when both sides bind the identical set.
    pub fn condition_binders(&self, cond: &HirId<HirExpr>) -> Vec<Symbol> {
        match self.get(cond) {
            HirExpr::Match(_, matcher) => matcher.binders(),
            HirExpr::Binary(BinOp::And, left, right) => {
                let mut out = self.condition_binders(left);
                out.extend(self.condition_binders(right));
                out
            },
            HirExpr::Binary(BinOp::Or, left, right) => {
                let left = self.condition_binders(left);
                let right = self.condition_binders(right);
                let same = left.len() == right.len() && left.iter().all(|name| right.contains(name));
                if same { left } else { Vec::new() }
            },
            _ => Vec::new(),
        }
    }

    /// Whether every path through a function body ends in a `return` or `throw`.
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
            _ => false,
        }
    }

    pub(crate) fn add<T: HirNode>(&mut self, kind: T, pos: SourcePosition) -> HirId<T> {
        self.nodes.push(HirArenaNode { kind: kind.wrap(), pos });
        HirId { id: self.nodes.len() - 1, _marker: PhantomData }
    }
}

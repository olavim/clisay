//! AST vocabulary.

mod operator;

use indexmap::IndexSet;
use core::fmt;
use std::collections::{HashMap, HashSet};
use std::marker::PhantomData;

pub use operator::Operator;

use crate::frontend::lex::SourcePosition;

/// An interned identifier.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Symbol(u32);

impl Symbol {
    pub(crate) fn from_raw(id: u32) -> Symbol {
        Symbol(id)
    }

    pub fn index(&self) -> usize {
        self.0 as usize
    }
}

pub enum Literal {
    Null,
    Boolean(bool),
    Number(f64),
    String(String),
    Array(Vec<AstId<Expr>>),
    Dict(Vec<(AstId<Expr>, AstId<Expr>)>),
    Lambda(FnDecl)
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Literal::Null => write!(f, "null"),
            Literal::Boolean(b) => write!(f, "{}", b),
            Literal::Number(n) => write!(f, "{}", n),
            Literal::String(s) => write!(f, "\"{}\"", s),
            Literal::Array(_) => write!(f, "[]"),
            Literal::Dict(_) => write!(f, "{{}}"),
            Literal::Lambda(_) => write!(f, "<lambda>")
        }
    }
}

pub enum Expr {
    Block(Vec<AstId<Stmt>>),
    Unary(Operator, AstId<Expr>),
    Binary(Operator, AstId<Expr>, AstId<Expr>),
    Call(AstId<Expr>, Vec<AstId<Expr>>),
    Index(AstId<Expr>, AstId<Expr>, bool),
    Literal(Literal),
    Identifier(Symbol),
    Is(AstId<Expr>, Symbol),
    Has(AstId<Expr>, AstId<Matcher>),
    /// Brace construction `C(args) { field: value, ... }`.
    Construct(AstId<Expr>, Vec<(Symbol, AstId<Expr>)>),
    This,
    /// `a?.b` / `a?[i]`
    SafeAccess(AstId<Expr>, AstId<Expr>, bool),
    /// `cb?(args)`
    SafeCall(AstId<Expr>, Vec<AstId<Expr>>),
    /// `a?!`
    Propagate(AstId<Expr>),
    /// `e ?? p => h`
    Handle(AstId<Expr>, Symbol, AstId<Expr>),
    /// `a!`
    Assert(AstId<Expr>),
    Mut(AstId<Expr>),
    /// `scrutinee ~ matcher`
    Match(AstId<Expr>, AstId<Matcher>),
}

#[derive(Clone, PartialEq, Debug)]
pub enum MatchScalar {
    Null,
    Boolean(bool),
    Number(f64),
    String(String),
}

/// A field of a shape matcher `{ key: value }`.
pub struct MatchField {
    pub key: MatchScalar,
    pub value: AstId<Matcher>,
}

/// An element of an array matcher.
#[derive(Clone, Copy)]
pub enum MatchElem {
    Elem(AstId<Matcher>),
    Rest(Option<AstId<Matcher>>),
}

pub enum Matcher {
    /// `_`
    Wildcard,
    Literal(MatchScalar),
    Binder(Symbol),
    Type { nominal: bool, name: Symbol, shape: Option<AstId<Matcher>> },
    Shape(Vec<MatchField>),
    Array(Vec<MatchElem>),
    /// `name @ m`
    As(Symbol, AstId<Matcher>),
    /// `a | b | ...`
    Or(Vec<AstId<Matcher>>),
    /// `a & b & ...`
    And(Vec<AstId<Matcher>>),
}

#[derive(Default, Clone, Copy, PartialEq, Eq, Debug)]
pub enum Capability {
    #[default]
    None,
    Mut,
    Move,
    MoveMut,
}

impl Capability {
    pub fn is_mut(self) -> bool {
        matches!(self, Capability::Mut | Capability::MoveMut)
    }

    pub fn is_retain(self) -> bool {
        matches!(self, Capability::Move | Capability::MoveMut)
    }
}

#[derive(Default)]
pub struct SlotClause {
    pub capability: Capability,
    pub names: Vec<Symbol>,
    pub container: bool,
    pub void: bool,
    /// The span of each of the clause's atoms.
    pub pos: Option<SourcePosition>,
}

/// The name a destructuring binding gives the whole value its binders are read out of. The space
/// keeps it out of the identifier grammar, so no source name can be it.
pub const SYNTHETIC_BINDING: &str = "the binding";

pub struct SayDecl {
    pub name: Symbol,
    pub otherwise: Option<AstId<Expr>>,
    pub pattern: Option<AstId<Matcher>>,
    pub value: Option<AstId<Expr>>,
    pub nullable: bool,
    pub reassignable: bool,
    pub clause: SlotClause,
}

/// A function/method/lambda parameter.
pub struct Param {
    pub pattern: AstId<Matcher>,
    /// The `pattern[: clause]` span.
    pub pos: SourcePosition,
    pub nullable: bool,
    pub reassignable: bool,
    pub clause: SlotClause,
}

impl Param {
    pub fn binder(&self, ast: &Ast) -> Option<Symbol> {
        match ast.get(&self.pattern) {
            Matcher::Binder(name) | Matcher::As(name, _) => Some(*name),
            _ => None,
        }
    }
}

/// The `this` parameter of an instance method.
pub struct Receiver {
    pub pos: SourcePosition,
    pub clause: SlotClause,
}

// TODO: fold into SlotClause
#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub enum ReturnShape {
    /// `fn f()!`
    NonNull,
    /// `fn f()?`
    Nullable,
    /// `fn f()`
    Void,
    #[default]
    Inferred,
}

pub struct FnDecl {
    pub name: Symbol,
    /// The `name(params): clause` signature span.
    pub sig_pos: SourcePosition,
    /// The declared `this`.
    pub receiver: Option<Receiver>,
    pub params: Vec<Param>,
    pub body: AstId<Expr>,
    pub ret: ReturnShape,
    pub clause: SlotClause,
}

/// `req fn f(this, params): clause;`
pub struct ReqFn {
    pub name: Symbol,
    /// The `name(params): clause` span.
    pub pos: SourcePosition,
    pub receiver: Option<Receiver>,
    pub params: Vec<Param>,
    pub ret: ReturnShape,
    pub clause: SlotClause,
}

/// `req "var"? name (":" clause)?;`
pub struct ReqMember {
    pub name: Symbol,
    /// The `var name: clause` span.
    pub pos: SourcePosition,
    pub reassignable: bool,
    pub clause: SlotClause,
}

/// `catch (param) { ... }`
pub struct CatchClause {
    pub param: Option<AstId<Expr>>,
    pub body: AstId<Expr>
}

#[derive(Clone, Copy, PartialEq)]
pub enum TraitClause { With, Req, Gives }

pub struct TraitRef {
    pub clause: TraitClause,
    pub trait_sym: Symbol,
    pub pos: SourcePosition,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum BuiltinType {
    Err,
}

impl BuiltinType {
    pub const COUNT: usize = std::mem::variant_count::<BuiltinType>();

    pub fn index(self) -> usize {
        self as usize
    }
}

pub struct TypeDecl {
    pub name: Symbol,
    pub is_trait: bool,
    pub builtin: Option<BuiltinType>,
    pub with_traits: Vec<Symbol>,
    /// Source spans of every `with`/`req`/`gives` trait mention.
    pub trait_refs: Vec<TraitRef>,
    pub req_traits: Vec<Symbol>,
    pub req_fns: Vec<ReqFn>,
    pub req_members: Vec<ReqMember>,
    pub gives: Vec<(Symbol, Symbol)>,
    pub init_name: Symbol,
    pub init: Option<AstId<Stmt>>,
    pub fields: IndexSet<Symbol>,
    pub nullable_fields: HashSet<Symbol>,
    pub var_fields: HashSet<Symbol>,
    pub field_clauses: Vec<(Symbol, SlotClause)>,
    /// Where each field is declared.
    pub field_positions: Vec<(Symbol, SourcePosition)>,
    /// Field defaults (`field = value`).
    pub field_inits: Vec<(Symbol, AstId<Expr>)>,
    pub methods: Vec<AstId<Stmt>>,
    pub pub_members: IndexSet<Symbol>,
    pub inner_members: IndexSet<Symbol>,
}

pub struct MatchArm {
    pub matcher: AstId<Matcher>,
    pub guard: Option<AstId<Expr>>,
    pub body: AstId<Expr>,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub struct ObligationRules {
    /// The value cannot be read until the obligation is discharged.
    pub to_use: bool,
    /// The value may not be stored where it would outlive its binding: a field, a container, or a
    /// closure. It may still be handed along a call chain.
    pub no_persist: bool,
    /// The value may not leave its frame by `return`, so its lifetime is the call.
    pub no_return: bool,
    /// The binding must be discharged before its scope ends.
    pub before_drop: bool,
    /// Reserved for typestate.
    pub no_drop: bool,
}

pub fn builtin_obligation_rules(name: &str) -> Option<ObligationRules> {
    Some(match name {
        "opt" => ObligationRules { to_use: true, ..Default::default() },
        "fails" => ObligationRules { to_use: true, no_persist: true, before_drop: true, ..Default::default() },
        _ => return None,
    })
}

pub enum Stmt {
    Expression(AstId<Expr>),
    Return(Option<AstId<Expr>>),
    Throw(AstId<Expr>),
    /// A try statement: Try(body, optional catch, optional finally).
    Try(AstId<Expr>, Option<CatchClause>, Option<AstId<Expr>>),
    While(AstId<Expr>, AstId<Expr>),
    /// An if statement: If(condition, then block, else body).
    If(AstId<Expr>, AstId<Expr>, Option<AstId<Stmt>>),
    /// A bare `{ ... }` statement block (wraps an `Expr::Block`).
    Block(AstId<Expr>),
    Defer(AstId<Expr>),
    Say(SayDecl),
    /// `say _ = expr;`
    Discard(AstId<Expr>),
    Fn(FnDecl),
    Type(Box<TypeDecl>),
    Obligation { name: Symbol, witness: Option<Symbol>, rules: ObligationRules },
    /// A match statement dispatching the scrutinee over arms.
    Match(AstId<Expr>, Vec<MatchArm>)
}

pub enum NodeKind {
    Expr(Expr),
    Stmt(Stmt),
    Matcher(Matcher)
}

pub trait AstNode: Sized {
    fn wrap(self) -> NodeKind;
    fn unwrap(node: &NodeKind) -> &Self;
}

impl AstNode for Expr {
    fn wrap(self) -> NodeKind { NodeKind::Expr(self) }
    fn unwrap(node: &NodeKind) -> &Expr {
        match node { NodeKind::Expr(expr) => expr, _ => unreachable!() }
    }
}

impl AstNode for Stmt {
    fn wrap(self) -> NodeKind { NodeKind::Stmt(self) }
    fn unwrap(node: &NodeKind) -> &Stmt {
        match node { NodeKind::Stmt(stmt) => stmt, _ => unreachable!() }
    }
}

impl AstNode for Matcher {
    fn wrap(self) -> NodeKind { NodeKind::Matcher(self) }
    fn unwrap(node: &NodeKind) -> &Matcher {
        match node { NodeKind::Matcher(matcher) => matcher, _ => unreachable!() }
    }
}

pub struct Node {
    pub pos: SourcePosition,
    pub kind: NodeKind
}

pub struct AstId<T> {
    id: usize,
    _marker: PhantomData<T>
}

impl<T> Copy for AstId<T> {}
impl<T> Clone for AstId<T> {
    fn clone(&self) -> AstId<T> {
        *self
    }
}

impl<T> PartialEq for AstId<T> {
    fn eq(&self, other: &AstId<T>) -> bool {
        self.id == other.id
    }
}
impl<T> Eq for AstId<T> {}
impl<T> std::hash::Hash for AstId<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl AstId<Expr> {
    pub fn as_comma_separated(&self, ast: &Ast) -> Vec<AstId<Expr>> {
        let mut vec = Vec::new();
        let mut q = vec![*self];

        while !q.is_empty() {
            let id = q.pop().unwrap();
            match ast.get(&id) {
                Expr::Binary(Operator::Comma, left, right) => {
                    q.push(*right);
                    q.push(*left);
                },
                _ => vec.push(id)
            }
        }

        vec
    }
}

pub struct Ast {
    nodes: Vec<Node>,
    /// Identifier interning
    ident_ids: HashMap<String, u32>,
    ident_texts: Vec<String>,
}

impl Ast {
    pub(crate) fn new() -> Ast {
        Ast {
            nodes: Vec::new(),
            ident_ids: HashMap::new(),
            ident_texts: Vec::new(),
        }
    }

    /// Interns (deduplicates) an identifier, returning its symbol.
    pub fn intern(&mut self, text: &str) -> Symbol {
        if let Some(&id) = self.ident_ids.get(text) {
            return Symbol(id);
        }
        let id = self.ident_texts.len() as u32;
        self.ident_texts.push(text.to_string());
        self.ident_ids.insert(text.to_string(), id);
        Symbol(id)
    }

    /// Removes the identifier interning tables, leaving them empty. Used by lowering
    /// to move name identity into the `Hir` (the `Ast` is discarded afterward).
    pub(crate) fn take_idents(&mut self) -> (HashMap<String, u32>, Vec<String>) {
        (std::mem::take(&mut self.ident_ids), std::mem::take(&mut self.ident_texts))
    }

    pub fn get<T: AstNode>(&self, id: &AstId<T>) -> &T {
        T::unwrap(&self.nodes[id.id].kind)
    }

    pub fn pos<T>(&self, id: &AstId<T>) -> &SourcePosition {
        &self.nodes[id.id].pos
    }

    pub fn text(&self, sym: Symbol) -> &str {
        &self.ident_texts[sym.index()]
    }

    pub fn symbol(&self, text: &str) -> Option<Symbol> {
        self.ident_ids.get(text).copied().map(Symbol::from_raw)
    }

    pub fn get_root(&self) -> AstId<Stmt> {
        AstId { id: self.nodes.len() - 1, _marker: PhantomData }
    }

    fn add<T: AstNode>(&mut self, kind: T, pos: SourcePosition) -> AstId<T> {
        self.nodes.push(Node { kind: kind.wrap(), pos });
        AstId { id: self.nodes.len() - 1, _marker: PhantomData }
    }

    pub(crate) fn add_stmt(&mut self, kind: Stmt, pos: SourcePosition) -> AstId<Stmt> {
        self.add(kind, pos)
    }

    pub(crate) fn add_expr(&mut self, kind: Expr, pos: SourcePosition) -> AstId<Expr> {
        self.add(kind, pos)
    }

    pub(crate) fn add_matcher(&mut self, kind: Matcher, pos: SourcePosition) -> AstId<Matcher> {
        self.add(kind, pos)
    }
}

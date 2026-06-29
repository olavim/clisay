//! AST vocabulary.

mod operator;

use core::fmt;
use std::collections::{HashMap, HashSet};
use std::marker::PhantomData;

pub use operator::Operator;

use crate::frontend::lex::SourcePosition;

/// An interned identifier, a cheap `Copy` identity. Resolved back to text via
/// [`Ast::text`]. Identifiers are interned by the parser as nodes are built.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Symbol(u32);

impl Symbol {
    pub(crate) fn from_raw(id: u32) -> Symbol {
        Symbol(id)
    }

    /// The symbol's raw index, used to look its text up in the interning table.
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
    /// A `dict` literal: `{ key: value, ... }`. Each key is a string-literal expr
    /// (identifier keys intern to strings); duplicate keys are rejected at parse.
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
    /// A block body (function/method/lambda/program)
    Block(Vec<AstId<Stmt>>),
    /// A unary expression: Unary(operator, operand)
    Unary(Operator, AstId<Expr>),
    /// A binary expression: Binary(operator, left, right)
    Binary(Operator, AstId<Expr>, AstId<Expr>),
    /// A function call: Call(callee, arguments)
    Call(AstId<Expr>, Vec<AstId<Expr>>),
    /// An access expression: `Index(target, index, is_dot)`. `is_dot` marks `.name`
    /// (member access via `.`) versus `[expr]` (data access via `[]`).
    Index(AstId<Expr>, AstId<Expr>, bool),
    Literal(Literal),
    Identifier(Symbol),
    /// `expr is T`: a nominal capability test against a static type/trait *name*
    /// resolved at runtime.
    Is(AstId<Expr>, Symbol),
    /// Brace construction `C(args) { field: value, ... }`. The first expr is the
    /// constructed callee (`C` or `C(args)`); the list is the brace field initializers.
    Construct(AstId<Expr>, Vec<(Symbol, AstId<Expr>)>),
    This,
    /// Safe navigation `a?.b` / `a?[i]`: short-circuits to null when the target is null.
    /// `SafeAccess(target, index, is_dot)`.
    SafeAccess(AstId<Expr>, AstId<Expr>, bool),
    /// The non-null assertion `a!`: yields the value, checking against null at runtime.
    Assert(AstId<Expr>),
    /// `expr is MATCHER` / `expr has MATCHER`: a bindingless matcher test yielding a boolean. The
    /// matcher is the bindingless subset of the `match` grammar. A bare nominal `is T` uses `Is`
    /// instead; everything richer (shapes, `&`/`|`) lands here.
    Has(AstId<Expr>, AstId<Matcher>),
}

/// A scalar literal in a matcher: an equality value (`v == s`) or a shape key.
#[derive(Clone, PartialEq, Debug)]
pub enum MatchScalar {
    Null,
    Boolean(bool),
    Number(f64),
    String(String),
}

/// A field of a shape matcher `{ key: value }`. The `{ x }` shorthand parses to key `x`
/// with a binder value.
pub struct MatchField {
    pub key: MatchScalar,
    pub value: AstId<Matcher>,
}

/// An element of an array matcher. `Rest` is `..` or `..name`, at most one per array.
pub enum MatchElem {
    Elem(AstId<Matcher>),
    Rest(Option<Symbol>),
}

pub enum Matcher {
    /// `_`: matches anything, binds nothing.
    Wildcard,
    /// A scalar literal compared with `==`.
    Literal(MatchScalar),
    /// A bare identifier that binds the whole value.
    Binder(Symbol),
    /// `is T shape?` (nominal) or `has T shape?` (structural).
    Type { nominal: bool, name: Symbol, shape: Option<AstId<Matcher>> },
    /// A structural shape `{ k: m, ... }`.
    Shape(Vec<MatchField>),
    /// An array shape `[ ... ]` with at most one rest element.
    Array(Vec<MatchElem>),
    /// `name @ m`: binds the whole value and also matches `m`.
    As(Symbol, AstId<Matcher>),
    /// `a | b | ...`: alternatives tried left to right.
    Or(Vec<AstId<Matcher>>),
    /// `a & b & ...`: all must match.
    And(Vec<AstId<Matcher>>),
}

pub struct FieldInit {
    pub name: Symbol,
    pub value: Option<AstId<Expr>>,
    /// Declared nullable with a `?` marker (`say x?`).
    pub nullable: bool,
    /// Declared reassignable with a `mut` modifier (`say mut x`).
    pub mutable: bool,
}

/// A function/method/lambda parameter: the bound identifier plus its declared
/// nullability and mutability markers (`fn f(mut x?)`).
pub struct Param {
    pub name: AstId<Expr>,
    pub nullable: bool,
    pub mutable: bool,
}

/// A function/method/lambda's declared return shape, the postfix marker after the
/// parameter list. Lambdas carry `Inferred` since their shape comes from the body.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum ReturnShape {
    /// `fn f()!` returns a non-null value.
    NonNull,
    /// `fn f()?` returns a nullable value.
    Nullable,
    /// `fn f()` returns no value.
    Void,
    /// A lambda, whose return shape is inferred from its body.
    Inferred,
}

pub struct FnDecl {
    pub name: Symbol,
    pub params: Vec<Param>,
    pub body: AstId<Expr>,
    pub ret: ReturnShape,
}

/// A `catch (param) { ... }` clause of a try statement.
pub struct CatchClause {
    pub param: Option<AstId<Expr>>,
    /// Declared reassignable with a `mut` modifier (`catch (mut e)`).
    pub mutable: bool,
    pub body: AstId<Expr>
}

pub struct TypeDecl {
    pub name: Symbol,
    /// `true` for a `trait` declaration, `false` for a `type`.
    pub is_trait: bool,
    /// Traits mixed in via `with T1, T2, ...`.
    pub with_traits: Vec<Symbol>,
    /// Traits depended on via `req T1, T2, ...`.
    pub req_traits: Vec<Symbol>,
    /// Method holes declared via `req fn f(params)`.
    pub req_fns: Vec<(Symbol, usize, ReturnShape)>,
    /// Member holes declared via `req name`.
    pub req_members: Vec<Symbol>,
    /// Delegation fields declared via `field gives Trait`.
    pub gives: Vec<(Symbol, Symbol)>,
    pub init_name: Symbol,
    pub init: Option<AstId<Stmt>>,
    pub fields: HashSet<Symbol>,
    pub nullable_fields: HashSet<Symbol>,
    pub mut_fields: HashSet<Symbol>,
    /// Field initializers (`field = value`), spliced into the init during lowering.
    pub field_inits: Vec<(Symbol, AstId<Expr>)>,
    pub methods: Vec<AstId<Stmt>>,
    pub pub_members: HashSet<Symbol>,
    pub inner_members: HashSet<Symbol>,
}

pub struct MatchArm {
    pub matcher: AstId<Matcher>,
    pub guard: Option<AstId<Expr>>,
    pub body: AstId<Expr>,
}

/// A `match` block. Either a dispatch over arms or a single matcher tested as a boolean.
pub enum MatchBody {
    Arms(Vec<MatchArm>),
    Matcher(AstId<Matcher>),
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
    Say(FieldInit),
    Fn(FnDecl),
    Type(Box<TypeDecl>),
    /// A match statement: Match(scrutinee, body).
    Match(AstId<Expr>, MatchBody)
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

    /// The interned text of a symbol. Valid until [`Ast::take_idents`] moves the table out.
    pub fn text(&self, sym: Symbol) -> &str {
        &self.ident_texts[sym.index()]
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

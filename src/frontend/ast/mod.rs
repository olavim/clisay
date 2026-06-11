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

    /// An index expression: Index(target, index)
    Index(AstId<Expr>, AstId<Expr>),

    Literal(Literal),
    Identifier(Symbol),
    This,
    Super
}

pub struct FieldInit {
    pub name: Symbol,
    pub value: Option<AstId<Expr>>
}

pub struct FnDecl {
    pub name: Symbol,
    pub params: Vec<AstId<Expr>>,
    pub body: AstId<Expr>
}

/// A `catch (param) { … }` clause of a try statement.
pub struct CatchClause {
    pub param: Option<AstId<Expr>>,
    pub body: AstId<Expr>
}

pub struct ClassDecl {
    pub name: Symbol,
    pub superclass: Option<Symbol>,
    /// The initializer's runtime name (`"{class}.init"`), used whether the init is
    /// declared or synthesised during lowering.
    pub init_name: Symbol,
    /// The declared initializer (`Stmt::Fn`), or `None` when the class has none —
    /// lowering synthesises a virtual init in that case.
    pub init: Option<AstId<Stmt>>,
    pub getter: Option<AstId<Stmt>>,
    pub setter: Option<AstId<Stmt>>,
    pub fields: HashSet<Symbol>,
    /// Field initializers (`field = value`), spliced into the init during lowering.
    pub field_inits: Vec<(Symbol, AstId<Expr>)>,
    pub methods: Vec<AstId<Stmt>>
}

pub enum Stmt {
    Expression(AstId<Expr>),
    Return(Option<AstId<Expr>>),
    Throw(AstId<Expr>),
    /// A try statement: Try(body block, optional catch clause, optional finally block).
    Try(AstId<Expr>, Option<CatchClause>, Option<AstId<Expr>>),
    While(AstId<Expr>, AstId<Expr>),
    /// An if statement: If(condition, then block, else body). The bodies are
    /// `Expr::Block`s; the else branch is a `Stmt::If` (else-if) or `Stmt::Block`.
    If(AstId<Expr>, AstId<Expr>, Option<AstId<Stmt>>),
    /// A bare `{ … }` statement block (wraps an `Expr::Block`).
    Block(AstId<Expr>),
    Say(FieldInit),
    Fn(FnDecl),
    Class(Box<ClassDecl>)
}

pub enum NodeKind {
    Expr(Expr),
    Stmt(Stmt)
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
}

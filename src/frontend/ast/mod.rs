//! AST vocabulary.

mod operator;

use core::fmt;
use std::collections::HashSet;
use std::marker::PhantomData;

pub use operator::Operator;

use crate::frontend::lex::SourcePosition;

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
    Identifier(String),
    This,
    Super
}

pub struct FieldInit {
    pub name: String,
    pub value: Option<AstId<Expr>>
}

pub struct FnDecl {
    pub name: String,
    pub params: Vec<AstId<Expr>>,
    pub body: AstId<Expr>
}

/// A `catch (param) { … }` clause of a try statement.
pub struct CatchClause {
    pub param: Option<AstId<Expr>>,
    pub body: AstId<Expr>
}

pub struct ClassDecl {
    pub name: String,
    pub superclass: Option<String>,
    pub init: AstId<Stmt>,
    pub getter: Option<AstId<Stmt>>,
    pub setter: Option<AstId<Stmt>>,
    pub fields: HashSet<String>,
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
    fn unwrap_mut(node: &mut NodeKind) -> &mut Self;
}

impl AstNode for Expr {
    fn wrap(self) -> NodeKind { NodeKind::Expr(self) }
    fn unwrap(node: &NodeKind) -> &Expr {
        match node { NodeKind::Expr(expr) => expr, _ => unreachable!() }
    }
    fn unwrap_mut(node: &mut NodeKind) -> &mut Expr {
        match node { NodeKind::Expr(expr) => expr, _ => unreachable!() }
    }
}

impl AstNode for Stmt {
    fn wrap(self) -> NodeKind { NodeKind::Stmt(self) }
    fn unwrap(node: &NodeKind) -> &Stmt {
        match node { NodeKind::Stmt(stmt) => stmt, _ => unreachable!() }
    }
    fn unwrap_mut(node: &mut NodeKind) -> &mut Stmt {
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
    nodes: Vec<Node>
}

impl Ast {
    pub(crate) fn new() -> Ast {
        Ast { nodes: Vec::new() }
    }

    pub fn get<T: AstNode>(&self, id: &AstId<T>) -> &T {
        T::unwrap(&self.nodes[id.id].kind)
    }

    pub fn get_mut<T: AstNode>(&mut self, id: &AstId<T>) -> &mut T {
        T::unwrap_mut(&mut self.nodes[id.id].kind)
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

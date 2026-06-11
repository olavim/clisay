//! The high-level IR (HIR): a post-lowering node hierarchy in which surface-only
//! constructs are unrepresentable. Produced by [`crate::middle::lower`] and consumed
//! by `resolve` and `codegen`.

use std::collections::HashMap;
use std::collections::HashSet;
use std::marker::PhantomData;

pub use crate::frontend::ast::Symbol;
use crate::frontend::lex::SourcePosition;

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

pub enum HirLiteral {
    Null,
    Boolean(bool),
    Number(f64),
    String(String),
    Array(Vec<HirId<HirExpr>>),
    Lambda(HirFnDecl),
}

pub enum HirExpr {
    Block(Vec<HirId<HirStmt>>),
    Unary(UnOp, HirId<HirExpr>),
    Binary(BinOp, HirId<HirExpr>, HirId<HirExpr>),
    Assign(HirId<HirExpr>, HirId<HirExpr>),
    Call(HirId<HirExpr>, Vec<HirId<HirExpr>>),
    Index(HirId<HirExpr>, HirId<HirExpr>),
    Literal(HirLiteral),
    Identifier(Symbol),
    This,
    Super,
}

pub struct HirFieldInit {
    pub name: Symbol,
    pub value: Option<HirId<HirExpr>>,
}

pub struct HirFnDecl {
    pub name: Symbol,
    pub params: Vec<HirId<HirExpr>>,
    pub body: HirId<HirExpr>,
}

/// A `catch (param) { … }` clause of a try statement.
pub struct HirCatchClause {
    pub param: Option<HirId<HirExpr>>,
    pub body: HirId<HirExpr>,
}

pub struct HirClassDecl {
    pub name: Symbol,
    pub superclass: Option<Symbol>,
    pub init: HirId<HirStmt>,
    pub getter: Option<HirId<HirStmt>>,
    pub setter: Option<HirId<HirStmt>>,
    pub fields: HashSet<Symbol>,
    pub methods: Vec<HirId<HirStmt>>,
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
    Class(Box<HirClassDecl>),
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

/// The lowered compilation unit: a flat arena of HIR nodes plus the identifier
/// interning tables (moved out of the `Ast` during lowering).
pub struct Hir {
    nodes: Vec<HirArenaNode>,
    ident_ids: HashMap<String, u32>,
    ident_texts: Vec<String>,
}

impl Hir {
    pub(crate) fn new(ident_ids: HashMap<String, u32>, ident_texts: Vec<String>) -> Hir {
        Hir { nodes: Vec::new(), ident_ids, ident_texts }
    }

    /// The text of an interned symbol.
    pub fn text(&self, symbol: Symbol) -> &str {
        &self.ident_texts[symbol.index()]
    }

    /// The symbol for `text` if it was ever interned, else `None`.
    pub fn symbol_of(&self, text: &str) -> Option<Symbol> {
        self.ident_ids.get(text).copied().map(Symbol::from_raw)
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

    pub(crate) fn add<T: HirNode>(&mut self, kind: T, pos: SourcePosition) -> HirId<T> {
        self.nodes.push(HirArenaNode { kind: kind.wrap(), pos });
        HirId { id: self.nodes.len() - 1, _marker: PhantomData }
    }
}

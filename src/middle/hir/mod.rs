//! The high-level IR (HIR): a post-lowering node hierarchy in which surface-only
//! constructs are unrepresentable.

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
    Dict(Vec<(HirId<HirExpr>, HirId<HirExpr>)>),
    Lambda(HirFnDecl),
}

pub enum HirExpr {
    Block(Vec<HirId<HirStmt>>),
    Unary(UnOp, HirId<HirExpr>),
    Binary(BinOp, HirId<HirExpr>, HirId<HirExpr>),
    Assign(HirId<HirExpr>, HirId<HirExpr>),
    Call(HirId<HirExpr>, Vec<HirId<HirExpr>>),
    Index(HirId<HirExpr>, HirId<HirExpr>, bool),
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

pub struct HirCatchClause {
    pub param: Option<HirId<HirExpr>>,
    pub body: HirId<HirExpr>,
}

pub struct HirTypeDecl {
    pub name: Symbol,
    pub superclass: Option<Symbol>,
    pub init: HirId<HirStmt>,
    pub getter: Option<HirId<HirStmt>>,
    pub setter: Option<HirId<HirStmt>>,
    pub fields: HashSet<Symbol>,
    pub methods: Vec<HirId<HirStmt>>,
    pub method_traits: Vec<Option<Symbol>>,
    pub pub_members: HashSet<Symbol>,
    pub trait_privates: HashMap<Symbol, HashMap<Symbol, Symbol>>,
    /// For a standalone trait (`HirStmt::Trait`): its **declared surface**.
    pub surface: HashSet<Symbol>,
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

pub struct Hir {
    nodes: Vec<HirArenaNode>,
    ident_ids: HashMap<String, u32>,
    ident_texts: Vec<String>,
}

impl Hir {
    pub(crate) fn new(ident_ids: HashMap<String, u32>, ident_texts: Vec<String>) -> Hir {
        Hir { nodes: Vec::new(), ident_ids, ident_texts }
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

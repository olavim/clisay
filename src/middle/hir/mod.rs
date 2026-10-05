//! The high-level IR (HIR): a post-lowering node hierarchy in which surface-only
//! constructs are unrepresentable.

use indexmap::IndexSet;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt;
use std::marker::PhantomData;

pub use crate::frontend::ast::{builtin_obligation_rules, ObligationRules, Receiver, SlotClause, Symbol};
use crate::frontend::lex::{SourcePosition, TokenType};

impl SlotClause {
    pub fn owed(&self) -> crate::middle::obligations::Obligations {
        self.names.iter().copied().collect()
    }
}

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

impl BinOp {
    pub fn yields_an_operand(self) -> bool {
        matches!(self, BinOp::And | BinOp::Or)
    }
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
    CompoundAssign(HirId<HirExpr>, BinOp, HirId<HirExpr>),
    Call(HirId<HirExpr>, Vec<HirId<HirExpr>>),
    /// `a.b` and `a[b]`. `safe` is the `?.` and `?[` form, which short-circuits on a bad operand.
    Index { base: HirId<HirExpr>, member: HirId<HirExpr>, is_dot: bool, safe: bool },
    Literal(HirLiteral),
    Identifier(Symbol),
    /// Brace construction `C { field: value, ... }`
    Construct(HirId<HirExpr>, Vec<(Symbol, HirId<HirExpr>)>),
    This,
    /// `a ?? b`
    Coalesce(HirId<HirExpr>, HirId<HirExpr>),
    /// `cb?(args)`
    SafeCall(HirId<HirExpr>, Vec<HirId<HirExpr>>),
    /// `a?!`
    Propagate(HirId<HirExpr>),
    /// `e ?? p => h`
    Handle(HirId<HirExpr>, Symbol, HirId<HirExpr>),
    /// `a!`
    Assert(HirId<HirExpr>),
    /// `&x`
    Anchor(HirId<HirExpr>),
    RefValue { holder: HirId<HirExpr>, safe: bool },
    Match(HirId<HirExpr>, HirId<HirMatcher>),
}

#[derive(Clone, PartialEq)]
pub struct HirMatchRest {
    pub binder: Option<Symbol>,
    pub every: Option<HirId<HirMatcher>>,
}

impl HirMatchRest {
    pub fn tests_something(&self, hir: &Hir) -> bool {
        self.every.is_some_and(|e| !hir.get(&e).is_irrefutable(hir))
    }
}

pub enum HirMatcher {
    /// `_`
    Wildcard,
    Literal(HirLiteral),
    Binder(Symbol),
    /// `is T shape?`
    Type { nominal: bool, name: Symbol, shape: Option<HirId<HirMatcher>> },
    /// `{ k: m, ..x @ q }`
    Shape { fields: Vec<HirMatchField>, rest: Option<HirMatchRest> },
    Dict(HirId<HirMatcher>),
    /// `[ ... ]`
    Array(Vec<HirMatchElem>),
    /// `name @ m`
    As(Symbol, HirId<HirMatcher>),
    /// `a | b | ...`
    Or(Vec<HirId<HirMatcher>>),
    /// `a & b & ...`
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
    Rest(HirMatchRest),
}

impl HirMatcher {
    pub fn binders(&self, hir: &Hir) -> Vec<Symbol> {
        let mut out = Vec::new();
        self.collect_binders(hir, &mut out);
        out
    }

    pub fn binds_anything(&self, hir: &Hir) -> bool {
        match self {
            HirMatcher::Wildcard | HirMatcher::Literal(_) => false,
            HirMatcher::Binder(_) | HirMatcher::As(..) => true,
            HirMatcher::Type { shape, .. } => shape.is_some_and(|s| hir.get(&s).binds_anything(hir)),
            HirMatcher::Shape { fields, rest } => fields.iter().any(|f| hir.get(&f.value).binds_anything(hir))
                || rest.as_ref().is_some_and(|r| r.binder.is_some() || r.every.is_some_and(|e| hir.get(&e).binds_anything(hir))),
            HirMatcher::Dict(shape) => hir.get(shape).binds_anything(hir),
            HirMatcher::Array(elements) => elements.iter().any(|e| match e {
                HirMatchElem::Elem(m) => hir.get(m).binds_anything(hir),
                HirMatchElem::Rest(rest) => rest.binder.is_some()
                    || rest.every.is_some_and(|e| hir.get(&e).binds_anything(hir)),
            }),
            HirMatcher::And(parts) => parts.iter().any(|p| hir.get(p).binds_anything(hir)),
            HirMatcher::Or(alternatives) => alternatives.iter().any(|a| hir.get(a).binds_anything(hir)),
        }
    }

    pub fn is_irrefutable(&self, hir: &Hir) -> bool {
        match self {
            HirMatcher::Wildcard | HirMatcher::Binder(_) => true,
            HirMatcher::As(_, inner) => hir.get(inner).is_irrefutable(hir),
            HirMatcher::And(parts) => parts.iter().all(|p| hir.get(p).is_irrefutable(hir)),
            HirMatcher::Or(alternatives) => alternatives.iter().any(|a| hir.get(a).is_irrefutable(hir)),
            _ => false,
        }
    }

    pub fn rejects_null(&self, hir: &Hir) -> bool {
        match self {
            HirMatcher::Wildcard | HirMatcher::Binder(_) => false,
            HirMatcher::Literal(HirLiteral::Null) => false,
            HirMatcher::Literal(_) => true,
            HirMatcher::Type { .. } | HirMatcher::Shape { .. } | HirMatcher::Dict(_) | HirMatcher::Array(_) => true,
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
            HirMatcher::Shape { fields, rest } => {
                for field in fields { hir.get(&field.value).collect_binders(hir, out) }
                if let Some(rest) = rest {
                    out.extend(rest.binder);
                    if let Some(every) = rest.every { hir.get(&every).collect_binders(hir, out) }
                }
            },
            HirMatcher::Dict(shape) => hir.get(shape).collect_binders(hir, out),
            HirMatcher::Array(elements) => for element in elements {
                match element {
                    HirMatchElem::Elem(matcher) => hir.get(matcher).collect_binders(hir, out),
                    HirMatchElem::Rest(rest) => {
                        out.extend(rest.binder);
                        if let Some(every) = &rest.every { hir.get(every).collect_binders(hir, out) }
                    },
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

pub struct HirSayDecl {
    pub name: Symbol,
    pub otherwise: Option<HirId<HirExpr>>,
    pub pattern: Option<HirId<HirMatcher>>,
    pub value: Option<HirId<HirExpr>>,
    pub reassignable: bool,
    pub clause: SlotClause,
}

pub struct HirParam {
    pub anchor: bool,
    pub name: HirId<HirExpr>,
    /// The parameter's pattern, when it does more than name its slot.
    pub pattern: Option<HirId<HirMatcher>>,
    /// The `name[: clause]` span.
    pub pos: SourcePosition,
    pub reassignable: bool,
    pub clause: SlotClause,
}

pub enum WritePlace {
    /// `x`
    Name(Symbol),
    /// `this`
    Receiver,
    /// `base.f` and `base[i]`
    Path { base: HirId<HirExpr>, member: HirId<HirExpr>, is_dot: bool, safe: bool },
    /// `a!` and `a?!`
    Wrapped(HirId<HirExpr>),
    /// `@t`
    Ref(HirId<HirExpr>),
    /// Names no slot, so a write cannot target it.
    Value,
}

/// One step of an `a.b[0].c` path.
#[derive(Clone, Copy)]
pub struct AccessStep {
    pub key: HirId<HirExpr>,
    pub is_dot: bool,
    /// The access node when this step is a safe access, like `?.name` or `?[key]`.
    pub safe: Option<HirId<HirExpr>>,
}

pub fn access_path_steps(hir: &Hir, path: &HirId<HirExpr>) -> (HirId<HirExpr>, Vec<AccessStep>) {
    let mut steps = Vec::new();
    let mut at = *path;
    loop {
        match hir.write_place_of(&at) {
            WritePlace::Path { base, member, is_dot, safe } => {
                steps.push(AccessStep { key: member, is_dot, safe: safe.then_some(at) });
                at = base;
            },
            WritePlace::Wrapped(inner) => at = inner,
            _ => break,
        }
    }
    steps.reverse();
    (at, steps)
}

pub fn access_path_root(hir: &Hir, path: &HirId<HirExpr>) -> HirId<HirExpr> {
    let mut at = *path;
    loop {
        match hir.write_place_of(&at) {
            WritePlace::Path { base, .. } => at = base,
            WritePlace::Wrapped(inner) => at = inner,
            _ => return at,
        }
    }
}

pub fn same_scalar(a: &HirLiteral, b: &HirLiteral) -> bool {
    match (a, b) {
        (HirLiteral::Null, HirLiteral::Null) => true,
        (HirLiteral::Boolean(a), HirLiteral::Boolean(b)) => a == b,
        (HirLiteral::Number(a), HirLiteral::Number(b)) => a == b,
        (HirLiteral::String(a), HirLiteral::String(b)) => a == b,
        _ => false,
    }
}

pub struct HirFnDecl {
    pub name: Symbol,
    /// The `name(params): clause` signature span.
    pub sig_pos: SourcePosition,
    pub receiver: Option<Receiver>,
    pub params: Vec<HirParam>,
    pub body: HirId<HirExpr>,
    pub clause: SlotClause,
}

impl Hir {
    pub fn path_has_safe_access(&self, expr: &HirId<HirExpr>) -> bool {
        match self.get(expr) {
            HirExpr::Index { base: inner, safe, .. } | HirExpr::RefValue { holder: inner, safe } =>
                *safe || self.path_has_safe_access(inner),
            HirExpr::Anchor(path) => self.path_has_safe_access(path),
            _ => false,
        }
    }

    /// The step each `?` on a path checks, rightmost `?` first.
    pub fn safe_access_checked_steps(&self, path: &HirId<HirExpr>) -> Vec<HirId<HirExpr>> {
        let mut bases = Vec::new();
        let mut at = *path;
        loop {
            match self.get(&at) {
                HirExpr::Index { base: inner, safe, .. } | HirExpr::RefValue { holder: inner, safe } => {
                    if *safe {
                        bases.push(*inner);
                    }
                    at = *inner;
                },
                HirExpr::Assert(inner) | HirExpr::Propagate(inner) => at = *inner,
                _ => return bases,
            }
        }
    }

    /// The keys a path evaluates on its way to its value.
    pub fn path_keys(&self, path: &HirId<HirExpr>) -> Vec<HirId<HirExpr>> {
        let mut keys = Vec::new();
        let mut at = *path;
        loop {
            match self.get(&at) {
                HirExpr::Index { base, member, .. } => {
                    keys.push(*member);
                    at = *base;
                },
                HirExpr::RefValue { holder, .. } => {
                    keys.push(*holder);
                    at = *holder;
                },
                HirExpr::Assert(inner) | HirExpr::Propagate(inner) => at = *inner,
                _ => return keys,
            }
        }
    }

    /// The reads of a name or a path, as `a` or `a.b[0]`, whose value `expr` can hand back
    /// unchanged. `a && b` hands back what `a` or `b` reads, and `x!` what `x` reads. A literal,
    /// a construction, a call and a `~` test hand back no stored value.
    pub fn reads_handed_back(&self, expr: &HirId<HirExpr>) -> Vec<HirId<HirExpr>> {
        match self.get(expr) {
            HirExpr::Identifier(_)
                | HirExpr::This 
                | HirExpr::Index { .. } 
                | HirExpr::RefValue { .. } => vec![*expr],
            HirExpr::Binary(op, left, right) | HirExpr::CompoundAssign(left, op, right) if op.yields_an_operand() => {
                [self.reads_handed_back(left), self.reads_handed_back(right)].concat()
            },
            HirExpr::Coalesce(left, right) | HirExpr::Handle(left, _, right) => {
                [self.reads_handed_back(left), self.reads_handed_back(right)].concat()
            },
            HirExpr::Assign(_, value)
                | HirExpr::Assert(value)
                | HirExpr::Propagate(value) => self.reads_handed_back(value),
            HirExpr::Literal(_)
                | HirExpr::Unary(..)
                | HirExpr::Binary(..)
                | HirExpr::CompoundAssign(..)
                | HirExpr::Construct(..)
                | HirExpr::Call(..)
                | HirExpr::SafeCall(..)
                | HirExpr::Anchor(_)
                | HirExpr::Match(..)
                | HirExpr::Block(_) => Vec::new(),
        }
    }

    pub fn passes_anchor_receiver(&self, callee: &HirId<HirExpr>) -> bool {
        matches!(self.get(callee), HirExpr::Index { base, .. } if matches!(self.get(base), HirExpr::Anchor(_)))
    }

    /// A call's receiver when written `&x.m()`, then its arguments.
    pub fn call_anchors(&self, callee: &HirId<HirExpr>, args: &[HirId<HirExpr>]) -> Vec<HirId<HirExpr>> {
        let receiver = match self.get(callee) {
            HirExpr::Index { base, .. } if matches!(self.get(base), HirExpr::Anchor(_)) => Some(*base),
            _ => None,
        };
        receiver.into_iter().chain(args.iter().copied()).collect()
    }

    /// The names a node may rebind.
    pub fn rebound_names(&self, node: &HirId<HirExpr>) -> Vec<HirId<HirExpr>> {
        match self.get(node) {
            HirExpr::Assign(lhs, _) | HirExpr::CompoundAssign(lhs, _, _) => vec![*lhs],
            HirExpr::Call(callee, args) | HirExpr::SafeCall(callee, args) => self.call_anchors(callee, args).iter()
                .filter_map(|arg| match self.get(arg) {
                    HirExpr::Anchor(path) => Some(*path),
                    _ => None,
                })
                .filter(|path| matches!(self.get(path), HirExpr::Identifier(_) | HirExpr::This))
                .collect(),
            _ => Vec::new(),
        }
    }

    /// The `this` node when `expr` is `this` or `&this`.
    pub fn as_this(&self, expr: &HirId<HirExpr>) -> Option<HirId<HirExpr>> {
        match self.get(expr) {
            HirExpr::This => Some(*expr),
            HirExpr::Anchor(inner) if matches!(self.get(inner), HirExpr::This) => Some(*inner),
            _ => None,
        }
    }

    pub fn write_place_of(&self, node: &HirId<HirExpr>) -> WritePlace {
        match self.get(node) {
            HirExpr::Identifier(name) => WritePlace::Name(*name),
            HirExpr::This => WritePlace::Receiver,
            HirExpr::Index { base, member, is_dot, safe } => WritePlace::Path { base: *base, member: *member, is_dot: *is_dot, safe: *safe },
            HirExpr::RefValue { holder, .. } => WritePlace::Ref(*holder),
            HirExpr::Assert(inner) | HirExpr::Propagate(inner) | HirExpr::Anchor(inner) => WritePlace::Wrapped(*inner),
            HirExpr::Block(..) | HirExpr::Unary(..) | HirExpr::Binary(..) | HirExpr::Assign(..)
            | HirExpr::CompoundAssign(..) | HirExpr::Call(..) | HirExpr::Literal(..)
            | HirExpr::Construct(..) | HirExpr::Coalesce(..) | HirExpr::SafeCall(..)
            | HirExpr::Handle(..) | HirExpr::Match(..) => WritePlace::Value,
        }
    }
}

impl HirFnDecl {
    pub(crate) fn has_no_return_clause(&self) -> bool {
        !self.clause.void && self.clause.is_empty()
    }
}

pub struct HirReqFn {
    pub name: Symbol,
    /// The trait that declares this hole.
    pub trait_name: Symbol,
    /// The `name(params): clause` span in the trait.
    pub pos: SourcePosition,
    /// Each parameter's clause and `name: clause` span.
    pub params: Vec<HirReqParam>,
    /// What the return may carry. A satisfier may promise fewer obligations.
    pub ret: SlotClause,
}

pub struct HirReqMember {
    pub name: Symbol,
    /// The trait that declares this hole.
    pub trait_name: Symbol,
    /// The `var name: clause` span in the trait.
    pub pos: SourcePosition,
    /// Required to be reassignable, with the `var` marker.
    pub reassignable: bool,
    /// What the member must owe.
    pub clause: SlotClause,
}

/// What lowering names a parameter whose pattern binds no name for the whole value.
pub const SYNTHETIC_PARAM: &str = "$p";

pub struct HirReqParam {
    pub pos: SourcePosition,
    pub clause: SlotClause,
    pub pattern: Option<HirId<HirMatcher>>,
}

/// `catch (param) { .. }`
pub struct HirCatchClause {
    pub param: Option<HirId<HirExpr>>,
    pub body: HirId<HirExpr>,
}

pub use crate::core::objects::TypeId;
pub use crate::frontend::ast::BuiltinType;

pub struct TypeInfo {
    pub name: Symbol,
    pub is_trait: bool,
}

pub struct HirTypeDecl {
    pub name: Symbol,
    pub id: TypeId,
    pub init: HirId<HirStmt>,
    pub fields: IndexSet<Symbol>,
    pub var_fields: HashSet<Symbol>,
    pub field_clauses: HashMap<Symbol, SlotClause>,
    /// Where each field is declared.
    pub field_positions: HashMap<Symbol, SourcePosition>,
    pub methods: Vec<HirId<HirStmt>>,
    pub req_fns: Vec<HirReqFn>,
    pub req_members: Vec<HirReqMember>,
    pub method_traits: Vec<Option<Symbol>>,
    pub pub_members: IndexSet<Symbol>,
    pub inner_members: IndexSet<Symbol>,
    pub trait_privates: HashMap<Symbol, HashMap<Symbol, Symbol>>,
    /// Which built-in this declares, if any.
    pub builtin: Option<BuiltinType>,
    /// A trait's declared surface.
    pub surface: IndexSet<Symbol>,
    /// What this type provides for `x is T`.
    pub provides: Vec<(Symbol, TypeId)>,
    pub gives: Vec<(Symbol, Symbol, TypeId)>,
}

impl HirTypeDecl {
    pub fn field_owes(&self, field: Symbol, obligation: Symbol) -> bool {
        self.field_clauses.get(&field).is_some_and(|c| c.owes(obligation))
    }
}

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
    Defer(HirId<HirExpr>),
    Say(HirSayDecl),
    Discard(HirId<HirExpr>),
    Fn(HirFnDecl),
    Type(Box<HirTypeDecl>),
    Trait(Box<HirTypeDecl>),
    Match(HirId<HirExpr>, Vec<HirMatchArm>),
    Nop,
}

impl HirStmt {
    pub fn declares_slot(&self) -> bool {
        match self {
            HirStmt::Fn(_) => true,
            HirStmt::Type(decl) => decl.builtin.is_none(),
            _ => false,
        }
    }
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

pub struct ObligationWitness {
    pub name: Symbol,
    pub id: TypeId,
}

pub struct ObligationDecl {
    pub witness: ObligationWitness,
    pub rules: ObligationRules,
}

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

    pub(crate) fn declare_obligation(&mut self, name: Symbol, witness: ObligationWitness, rules: ObligationRules) {
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

    pub(crate) fn member_symbol(&self, member: &HirId<HirExpr>) -> Option<Symbol> {
        let HirExpr::Literal(HirLiteral::String(text)) = self.get(member) else { return None };
        self.symbol_of(text)
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

    pub fn expr_at(&self, index: usize) -> Option<&HirExpr> {
        match &self.nodes[index].kind {
            HirNodeKind::Expr(expr) => Some(expr),
            _ => None,
        }
    }

    pub fn stmt_at(&self, index: usize) -> Option<&HirStmt> {
        match &self.nodes[index].kind {
            HirNodeKind::Stmt(stmt) => Some(stmt),
            _ => None,
        }
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

    pub fn script_body(&self) -> Option<HirId<HirExpr>> {
        match self.get(&self.get_root()) {
            HirStmt::Expression(body) | HirStmt::Block(body) => Some(*body),
            _ => None,
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
            HirStmt::Try(body, catch, finally) => {
                if finally.as_ref().is_some_and(|f| self.definitely_returns(f)) {
                    return true;
                }
                self.definitely_returns(body) && catch.as_ref().map_or(true, |c| self.definitely_returns(&c.body))
            },
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

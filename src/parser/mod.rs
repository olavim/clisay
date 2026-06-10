mod operator;

use core::fmt;
use std::collections::HashSet;
use std::marker::PhantomData;

use anyhow::anyhow;
pub use operator::Operator;

use crate::lexer::{SourcePosition, TokenStream, TokenType};

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
    pub fn as_comma_separated(&self, ast: &AstArena) -> Vec<AstId<Expr>> {
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

pub struct AstArena {
    nodes: Vec<Node>
}

impl AstArena {
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

    fn add_stmt(&mut self, kind: Stmt, pos: SourcePosition) -> AstId<Stmt> {
        self.add(kind, pos)
    }

    fn add_expr(&mut self, kind: Expr, pos: SourcePosition) -> AstId<Expr> {
        self.add(kind, pos)
    }
}

macro_rules! parse_error {
    ($self:ident, $pos:expr, $($arg:tt)*) => { return Err($self.error(format!($($arg)*), $pos)) };
}

pub struct Parser<'parser, 'vm> {
    tokens: &'vm mut TokenStream<'vm>,
    arena: &'parser mut AstArena,
    current_class: Option<String>
}

impl<'parser, 'vm> Parser<'parser, 'vm> {
    pub fn parse(tokens: &'vm mut TokenStream<'vm>) -> Result<AstArena, anyhow::Error> {
        let mut arena = AstArena {
            nodes: Vec::new()
        };

        let mut parser = Parser {
            tokens,
            arena: &mut arena,
            current_class: None
        };

        let pos = parser.tokens.peek(0).pos.clone();
        let mut stmts: Vec<AstId<Stmt>> = Vec::new();
        while parser.tokens.has_next() {
            stmts.push(parser.parse_stmt()?);
        }
        let block = parser.arena.add_expr(Expr::Block(stmts), pos.clone());
        arena.add_stmt(Stmt::Expression(block), pos);

        Ok(arena)
    }

    fn error(&self, message: impl Into<String>, pos: &SourcePosition) -> anyhow::Error {
        anyhow!(format!("{}\nat {}", message.into(), pos))
    }

    fn parse_identifier(&mut self) -> Result<String, anyhow::Error> {
        let token = self.tokens.expect(TokenType::Identifier)?;
        Ok(token.lexeme.clone())
    }

    fn parse_identifier_expr(&mut self) -> Result<AstId<Expr>, anyhow::Error> {
        let token = self.tokens.expect(TokenType::Identifier)?;
        let id = self.arena.add_expr(Expr::Identifier(token.lexeme.clone()), token.pos.clone());
        Ok(id)
    }

    fn parse_stmt(&mut self) -> Result<AstId<Stmt>, anyhow::Error> {
        match self.tokens.peek(0).kind {
            TokenType::Say => self.parse_say(),
            TokenType::While => self.parse_while(),
            TokenType::Fn => self.parse_fn(),
            TokenType::Class => self.parse_class(),
            TokenType::Return => self.parse_return(),
            TokenType::Throw => self.parse_throw(),
            TokenType::Try => self.parse_trycatch(),
            TokenType::If => self.parse_if_stmt(),
            TokenType::LeftBrace => self.parse_block_stmt(),
            _ => self.parse_expr_stmt()
        }
    }

    fn parse_if_stmt(&mut self) -> Result<AstId<Stmt>, anyhow::Error> {
        let pos = self.tokens.expect(TokenType::If)?.pos.clone();
        let condition = self.parse_expr()?;
        let then = self.parse_block_or_stmt()?;
        let otherwise = match self.tokens.next_if(TokenType::Else) {
            Some(_) => match self.tokens.peek(0).kind {
                TokenType::If => Some(self.parse_if_stmt()?),
                _ => Some(self.parse_block_stmt()?)
            },
            None => None
        };
        Ok(self.arena.add_stmt(Stmt::If(condition, then, otherwise), pos))
    }

    fn parse_block_stmt(&mut self) -> Result<AstId<Stmt>, anyhow::Error> {
        let pos = self.tokens.peek(0).pos.clone();
        let body = self.parse_block_or_stmt()?;
        Ok(self.arena.add_stmt(Stmt::Block(body), pos))
    }

    fn parse_say(&mut self) -> Result<AstId<Stmt>, anyhow::Error> {
        let pos = self.tokens.expect(TokenType::Say)?.pos.clone();
        let name = self.parse_identifier()?;

        let expr = if let Some(_) = self.tokens.next_if(TokenType::Equal) {
            Some(self.parse_expr()?)
        } else {
            None
        };

        self.tokens.expect(TokenType::Semicolon)?;
        let field_init = FieldInit { name, value: expr };
        Ok(self.arena.add_stmt(Stmt::Say(field_init), pos))
    }

    fn parse_fn(&mut self) -> Result<AstId<Stmt>, anyhow::Error> {
        let pos = self.tokens.expect(TokenType::Fn)?.pos.clone();
        let name = self.parse_identifier()?;
        let fn_decl = self.parse_fn_decl(name)?;
        Ok(self.arena.add_stmt(Stmt::Fn(fn_decl), pos))
    }

    fn parse_fn_decl(&mut self, name: String) -> Result<FnDecl, anyhow::Error> {
        self.tokens.expect(TokenType::LeftParen)?;
        let params = self.parse_params(TokenType::RightParen)?;
        let body = self.parse_block()?;
        Ok(FnDecl {
            name,
            params,
            body
        })
    }

    fn parse_accessor(&mut self, name: String, arity: usize, arity_error: &str) -> Result<AstId<Stmt>, anyhow::Error> {
        let token = self.tokens.peek(0).clone();
        let fn_decl = self.parse_fn_decl(name)?;
        if fn_decl.params.len() != arity {
            parse_error!(self, &token.pos, "{}", arity_error)
        }
        Ok(self.arena.add_stmt(Stmt::Fn(fn_decl), token.pos))
    }

    fn parse_init(&mut self, has_superclass: bool) -> Result<AstId<Stmt>, anyhow::Error> {
        let pos = self.tokens.peek(0).pos.clone();
        let Some(name) = &self.current_class else { unreachable!() };
        let name = format!("{}.init", name);
        self.tokens.expect(TokenType::LeftParen)?;
        let params = self.parse_params(TokenType::RightParen)?;
        self.tokens.expect(TokenType::LeftBrace)?;

        // Ensure initializer calls super() if there is a superclass
        let mut stmts = if has_superclass {
            // Parse super() call if it exists, or add a virtual zero-arity super() call
            match (self.tokens.peek(0).kind, self.tokens.peek(1).kind) {
                (TokenType::Super, TokenType::LeftParen) => vec![self.parse_super_call()?],
                _ => vec![self.virtual_super_call(pos.clone())?]
            }
        } else {
            Vec::new()
        };

        stmts.extend(self.parse_stmts()?);
        self.tokens.expect(TokenType::RightBrace)?;

        let body = self.arena.add_expr(Expr::Block(stmts), pos.clone());
        let fn_decl = FnDecl { name, params, body };
        Ok(self.arena.add_stmt(Stmt::Fn(fn_decl), pos))
    }

    fn parse_params(&mut self, end_token: TokenType) -> Result<Vec<AstId<Expr>>, anyhow::Error> {
        let params = match self.tokens.next_if(end_token) {
            Some(_) => Vec::new(),
            None => {
                let mut params = Vec::new();
                while !self.tokens.match_next(end_token) {
                    if params.len() > 0 {
                        self.tokens.expect(TokenType::Comma)?;
                    }
                    params.push(self.parse_identifier_expr()?);
                }
                self.tokens.expect(end_token)?;
                params
            }
        };

        Ok(params)
    }

    fn virtual_super_call(&mut self, pos: SourcePosition) -> Result<AstId<Stmt>, anyhow::Error> {
        let super_expr = self.arena.add_expr(Expr::Super, pos.clone());
        let expr = self.arena.add_expr(Expr::Call(super_expr, Vec::new()), pos.clone());
        Ok(self.arena.add_stmt(Stmt::Expression(expr), pos.clone()))
    }

    fn parse_class(&mut self) -> Result<AstId<Stmt>, anyhow::Error> {
        let pos = self.tokens.expect(TokenType::Class)?.pos.clone();
        let name = self.parse_identifier()?;

        let prev_class = std::mem::replace(&mut self.current_class, Some(name.clone()));

        let superclass = match self.tokens.next_if(TokenType::Colon) {
            Some(_) => Some(self.parse_identifier()?),
            None => None
        };

        self.tokens.expect(TokenType::LeftBrace)?;

        let mut fields: HashSet<String> = HashSet::default();
        let mut field_stmts: Vec<AstId<Stmt>> = Vec::new();
        let mut method_stmts: Vec<AstId<Stmt>> = Vec::new();
        let mut init = None;
        let mut getter = None;
        let mut setter = None;

        while !self.tokens.match_next(TokenType::RightBrace) {
            let kind = self.tokens.peek(0).kind;
            match kind {
                TokenType::Fn => {
                    method_stmts.push(self.parse_fn()?);
                },
                TokenType::Identifier => {
                    let name = self.parse_identifier()?;

                    // Some identifiers have special meaning in classes but are not outright keywords
                    match name.as_str() {
                        "init" => {
                            init = Some(self.parse_init(superclass.is_some())?);
                        },
                        "get" => {
                            getter = Some(self.parse_accessor(name, 1, "Getter must have exactly one parameter")?);
                        },
                        "set" => {
                            setter = Some(self.parse_accessor(name, 2, "Setter must have exactly two parameters")?);
                        },
                        _ => {
                            // Field declaration
                            let value = if let Some(_) = self.tokens.next_if(TokenType::Equal) {
                                Some(self.parse_expr()?)
                            } else {
                                None
                            };

                            self.tokens.expect(TokenType::Semicolon)?;
                            fields.insert(name.clone());

                            if let Some(value) = value {
                                let id = self.arena.add_expr(Expr::Identifier(name), pos.clone());
                                let assign = self.arena.add_expr(Expr::Binary(Operator::Assign(None), id, value), pos.clone());
                                field_stmts.push(self.arena.add_stmt(Stmt::Expression(assign), pos.clone()));
                            }
                        }
                    }
                },
                kind => {
                    parse_error!(self, &self.tokens.peek(0).pos, "Unexpected token {kind}")
                }
            }
        }
        self.tokens.expect(TokenType::RightBrace)?;

        let init = match init {
            Some(stmt_id) => stmt_id,
            None => {
                // Virtual initializer
                let stmts = if superclass.is_some() {
                    vec![self.virtual_super_call(pos.clone())?]
                } else {
                    Vec::new()
                };

                let body = self.arena.add_expr(Expr::Block(stmts), pos.clone());
                let fn_decl = FnDecl {
                    name: format!("{}.init", name),
                    params: Vec::new(),
                    body
                };
                self.arena.add_stmt(Stmt::Fn(fn_decl), pos.clone())
            }
        };

        let Stmt::Fn(fn_decl) = self.arena.get(&init) else { unreachable!() };
        let Expr::Block(body) = self.arena.get_mut(&fn_decl.body.clone()) else { unreachable!() };
        body.splice(0..0, field_stmts.iter().cloned());

        let class_decl = Box::new(ClassDecl {
            name,
            superclass,
            init,
            getter,
            setter,
            fields,
            methods: method_stmts
        });

        self.current_class = prev_class;
        Ok(self.arena.add_stmt(Stmt::Class(class_decl), pos))
    }

    fn parse_while(&mut self) -> Result<AstId<Stmt>, anyhow::Error> {
        let pos = self.tokens.expect(TokenType::While)?.pos.clone();
        let condition = self.parse_expr()?;
        let body = self.parse_block_or_stmt()?;
        Ok(self.arena.add_stmt(Stmt::While(condition, body), pos))
    }

    fn parse_return(&mut self) -> Result<AstId<Stmt>, anyhow::Error> {
        let pos = self.tokens.expect(TokenType::Return)?.pos.clone();
        let expr = match self.tokens.match_next(TokenType::Semicolon) {
            true => None,
            false => Some(self.parse_expr()?)
        };
        self.tokens.expect(TokenType::Semicolon)?;
        Ok(self.arena.add_stmt(Stmt::Return(expr), pos))
    }

    fn parse_throw(&mut self) -> Result<AstId<Stmt>, anyhow::Error> {
        let pos = self.tokens.expect(TokenType::Throw)?.pos.clone();
        let expr = self.parse_expr_semi()?;
        Ok(self.arena.add_stmt(Stmt::Throw(expr), pos))
    }

    /// Parses an expression terminated by a required semicolon.
    fn parse_expr_semi(&mut self) -> Result<AstId<Expr>, anyhow::Error> {
        let expr = self.parse_expr()?;
        self.tokens.expect(TokenType::Semicolon)?;
        Ok(expr)
    }

    fn parse_trycatch(&mut self) -> Result<AstId<Stmt>, anyhow::Error> {
        let pos = self.tokens.expect(TokenType::Try)?.pos.clone();
        let try_body = self.parse_block_or_stmt()?;

        let catch = if let Some(catch_tok) = self.tokens.next_if(TokenType::Catch) {
            let catch_pos = catch_tok.pos.clone();
            let param = match self.tokens.peek(0).kind {
                TokenType::Identifier => Some(self.parse_identifier_expr()?),
                TokenType::LeftParen => {
                    self.tokens.expect(TokenType::LeftParen)?;
                    let params = self.parse_params(TokenType::RightParen)?;
                    if params.len() != 1 {
                        parse_error!(self, &catch_pos, "Expected one parameter in catch block")
                    }
                    Some(params[0])
                },
                _ => None
            };
            let body = self.parse_block_or_stmt()?;
            Some(CatchClause { param, body })
        } else {
            None
        };

        let finally = if self.tokens.next_if(TokenType::Finally).is_some() {
            Some(self.parse_block_or_stmt()?)
        } else {
            None
        };

        if catch.is_none() && finally.is_none() {
            parse_error!(self, &pos, "Expected catch or finally block")
        }

        Ok(self.arena.add_stmt(Stmt::Try(try_body, catch, finally), pos))
    }

    fn parse_block(&mut self) -> Result<AstId<Expr>, anyhow::Error> {
        let pos = self.tokens.peek(0).pos.clone();
        self.tokens.expect(TokenType::LeftBrace)?;
        let stmts = self.parse_stmts()?;
        self.tokens.expect(TokenType::RightBrace)?;
        Ok(self.arena.add_expr(Expr::Block(stmts), pos))
    }

    fn parse_block_or_stmt(&mut self) -> Result<AstId<Expr>, anyhow::Error> {
        if self.tokens.match_next(TokenType::LeftBrace) {
            self.parse_block()
        } else {
            let pos = self.tokens.peek(0).pos.clone();
            let stmt = self.parse_stmt()?;
            Ok(self.arena.add_expr(Expr::Block(vec![stmt]), pos))
        }
    }

    fn parse_block_or_expr(&mut self, prec: u8) -> Result<AstId<Expr>, anyhow::Error> {
        if self.tokens.match_next(TokenType::LeftBrace) {
            self.parse_block()
        } else {
            self.parse_expr_precedence(prec)
        }
    }

    /// Parses statements up to (but not consuming) the closing `}`.
    fn parse_stmts(&mut self) -> Result<Vec<AstId<Stmt>>, anyhow::Error> {
        let mut stmts: Vec<AstId<Stmt>> = Vec::new();
        while !self.tokens.match_next(TokenType::RightBrace) {
            stmts.push(self.parse_stmt()?);
        }
        Ok(stmts)
    }

    fn make_lambda(&self, params: Vec<AstId<Expr>>, body: AstId<Expr>) -> Expr {
        Expr::Literal(Literal::Lambda(FnDecl { name: String::from("lambda"), params, body }))
    }

    fn parse_expr_stmt(&mut self) -> Result<AstId<Stmt>, anyhow::Error> {
        let pos = self.tokens.peek(0).pos.clone();
        let expr = self.parse_expr_semi()?;
        Ok(self.arena.add_stmt(Stmt::Expression(expr), pos))
    }

    fn parse_expr(&mut self) -> Result<AstId<Expr>, anyhow::Error> {
        self.parse_expr_precedence(0)
    }

    fn parse_expr_precedence(&mut self, min_precedence: u8) -> Result<AstId<Expr>, anyhow::Error> {
        let mut left = match Operator::parse_prefix(self.tokens, 0) {
            Some(op) => self.parse_expr_prefix(op)?,
            _ => self.parse_expr_atom()?
        };

        loop {
            if let Some(op) = Operator::parse_postfix(self.tokens, min_precedence) {
                left = self.parse_expr_postfix(op, left)?;
            } else if let Some(op) = Operator::parse_infix(self.tokens, min_precedence) {
                left = self.parse_expr_infix(op, left)?;
            } else {
                break;
            }
        }

        Ok(left)
    }

    fn parse_expr_infix(&mut self, op: Operator, expr: AstId<Expr>) -> Result<AstId<Expr>, anyhow::Error> {
        let pos = self.arena.pos(&expr).clone();

        let kind = match &op {
            Operator::Assign(Some(assign_op)) => {
                // Normalize compound assignment, for example convert (a += 1) to (a = a + 1)
                let mut right = self.parse_expr_precedence(op.infix_precedence().unwrap())?;
                let kind = Expr::Binary(assign_op.as_ref().clone(), expr, right);
                right = self.arena.add_expr(kind, pos.clone());
                Expr::Binary(op, expr, right)
            },
            Operator::MemberAccess => {
                let id = self.parse_identifier()?;
                let id = self.arena.add_expr(Expr::Literal(Literal::String(id)), pos.clone());
                Expr::Index(expr, id)
            },
            Operator::Arrow => {
                let right = self.parse_block_or_expr(op.infix_precedence().unwrap())?;
                let params = expr.as_comma_separated(self.arena).iter()
                    .map(|id| match self.arena.get(id) {
                        Expr::Identifier(_) => Ok(*id),
                        _ => parse_error!(self, &pos, "Invalid lambda parameter")
                    })
                    .collect::<Result<Vec<_>, anyhow::Error>>()?;
                self.make_lambda(params, right)
            },
            _ => {
                let right = self.parse_expr_precedence(op.infix_precedence().unwrap())?;
                Expr::Binary(op, expr, right)
            }
        };

        Ok(self.arena.add_expr(kind, pos.clone()))
    }

    fn parse_expr_prefix(&mut self, op: Operator) -> Result<AstId<Expr>, anyhow::Error> {
        let pos = self.tokens.peek(0).pos.clone();
        let kind = match &op {
            Operator::Group => {
                match self.tokens.next_if(TokenType::RightParen) {
                    Some(_) => {
                        let Some(Operator::Arrow) = Operator::parse_infix(self.tokens, 0) else {
                            parse_error!(self, &pos, "Unexpected token: Expected '=>'")
                        };
                        let right = self.parse_block_or_expr(Operator::Arrow.infix_precedence().unwrap())?;
                        self.make_lambda(Vec::new(), right)
                    },
                    None => {
                        let expr = self.parse_expr()?;
                        self.tokens.expect(TokenType::RightParen)?;
                        return Ok(expr);
                    }
                }
            },
            Operator::Array => {
                let elements = match self.tokens.next_if(TokenType::RightBracket) {
                    Some(_) => Vec::new(),
                    None => {
                        let expr = self.parse_expr()?;
                        self.tokens.expect(TokenType::RightBracket)?;
                        expr.as_comma_separated(self.arena)
                    }
                };

                return Ok(self.arena.add_expr(Expr::Literal(Literal::Array(elements)), pos));
            },
            _ => {
                let right = self.parse_expr_precedence(op.prefix_precedence().unwrap())?;
                Expr::Unary(op, right)
            }
        };
        Ok(self.arena.add_expr(kind, pos))
    }

    fn parse_expr_postfix(&mut self, op: Operator, expr: AstId<Expr>) -> Result<AstId<Expr>, anyhow::Error> {
        let pos = self.arena.pos(&expr).clone();
        match op {
            Operator::Call => {
                let args = self.parse_call_arguments()?;
                Ok(self.arena.add_expr(Expr::Call(expr, args), pos))
            },
            Operator::Index => {
                let index = self.parse_expr()?;
                self.tokens.expect(TokenType::RightBracket)?;
                Ok(self.arena.add_expr(Expr::Index(expr, index), pos))
            },
            _ => unreachable!()
        }
    }

    fn parse_expr_atom(&mut self) -> Result<AstId<Expr>, anyhow::Error> {
        let token = self.tokens.next().clone();
        let pos = token.pos.clone();

        let kind = match token.kind {
            TokenType::StringLiteral => {
                let val = token.lexeme.clone();
                let val = &val[1..val.len() - 1]; // Strip quotes
                Expr::Literal(Literal::String(String::from(val)))
            },
            TokenType::NumericLiteral => Expr::Literal(Literal::Number(token.lexeme.parse().unwrap())),
            TokenType::Null => Expr::Literal(Literal::Null),
            TokenType::True => Expr::Literal(Literal::Boolean(true)),
            TokenType::False => Expr::Literal(Literal::Boolean(false)),
            TokenType::This => Expr::This,
            /* The super keyword is not a valid expression on its own,
             * so it makes sense to consider something like `super.x` as an "atom".
             */
            TokenType::Super => return self.parse_super(),
            TokenType::Identifier => Expr::Identifier(token.lexeme.clone()),
            _ => parse_error!(self, &pos, "Unexpected token {token}")
        };

        Ok(self.arena.add_expr(kind, pos))
    }

    fn parse_super(&mut self) -> Result<AstId<Expr>, anyhow::Error> {
        let token = self.tokens.next();
        let pos = token.pos.clone();

        match token.kind {
            TokenType::Dot => {
                let super_expr = self.arena.add_expr(Expr::Super, pos.clone());
                let id = self.parse_identifier()?;
                let id = self.arena.add_expr(Expr::Literal(Literal::String(id)), pos.clone());
                let expr = self.arena.add_expr(Expr::Index(super_expr, id), pos.clone());
                Ok(expr)
            },
            TokenType::LeftBracket => {
                let super_expr = self.arena.add_expr(Expr::Super, pos.clone());
                let expr = self.parse_expr()?;
                self.tokens.expect(TokenType::RightBracket)?;
                let expr = self.arena.add_expr(Expr::Index(super_expr, expr), pos.clone());
                Ok(expr)
            },
            TokenType::LeftParen => parse_error!(self, &pos, "Super call must be the first statement in an init block"),
            _ => parse_error!(self, &pos, "Unexpected token {token}"),
        }
    }

    fn parse_super_call(&mut self) -> Result<AstId<Stmt>, anyhow::Error> {
        let pos = self.tokens.expect(TokenType::Super)?.pos.clone();
        self.tokens.expect(TokenType::LeftParen)?;
        let args = self.parse_call_arguments()?;
        self.tokens.expect(TokenType::Semicolon)?;
        let super_expr = self.arena.add_expr(Expr::Super, pos.clone());
        let expr = self.arena.add_expr(Expr::Call(super_expr, args), pos.clone());
        Ok(self.arena.add_stmt(Stmt::Expression(expr), pos))
    }

    fn parse_call_arguments(&mut self) -> Result<Vec<AstId<Expr>>, anyhow::Error> {
        match self.tokens.next_if(TokenType::RightParen) {
            Some(_) => Ok(Vec::new()),
            None => {
                let args = self.parse_expr()?;
                self.tokens.expect(TokenType::RightParen)?;
                Ok(args.as_comma_separated(self.arena))
            }
        }
    }
}
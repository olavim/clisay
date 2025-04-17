mod operator;

use core::fmt;
use std::collections::HashSet;
use std::{any::Any, marker::PhantomData};

use anyhow::anyhow;
pub use operator::Operator;

use crate::lexer::{SourcePosition, TokenStream, TokenType};

pub enum Literal {
    Null,
    Boolean(bool),
    Number(f64),
    String(String),
    Array(Option<ASTId<Expr>>),
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

pub trait ASTKind {
    fn as_any(&self) -> &dyn Any;
    fn as_any_mut(&mut self) -> &mut dyn Any;
}

pub enum Expr {
    /// A block of statements, with an optional expression at the end.
    Block(Vec<ASTId<Stmt>>, Option<ASTId<Expr>>),

    /// An if statement: If(condition, then, Option(else))
    If(ASTId<Expr>, ASTId<Expr>, Option<ASTId<Expr>>),

    /// A unary expression: Unary(operator, operand)
    Unary(Operator, ASTId<Expr>),

    /// A binary expression: Binary(operator, left, right)
    Binary(Operator, ASTId<Expr>, ASTId<Expr>),

    /// A function call: Call(callee, List(...arguments))
    Call(ASTId<Expr>, Option<ASTId<Expr>>),

    /// An index expression: Index(target, index)
    Index(ASTId<Expr>, ASTId<Expr>),

    Literal(Literal),
    Identifier(String),
    This,
    Super
}

impl ASTKind for Expr {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

pub struct FieldInit {
    pub name: String,
    pub value: Option<ASTId<Expr>>
}

pub struct FnDecl {
    pub name: String,
    pub params: Vec<ASTId<Expr>>,
    pub body: ASTId<Expr>
}

pub struct ClassDecl {
    pub name: String,
    pub superclass: Option<String>,
    pub init: ASTId<Stmt>,
    pub getter: Option<ASTId<Stmt>>,
    pub setter: Option<ASTId<Stmt>>,
    pub fields: HashSet<String>,
    pub methods: Vec<ASTId<Stmt>>
}

pub enum Stmt {
    Expression(ASTId<Expr>),
    Return(Option<ASTId<Expr>>),
    Throw(ASTId<Expr>),
    /// (try body, opt(opt(catch param), catch body), finally body)
    Try(Vec<ASTId<Stmt>>, Option<ASTId<Stmt>>, Option<ASTId<Stmt>>),
    Catch(Option<ASTId<Expr>>, Vec<ASTId<Stmt>>),
    Finally(Vec<ASTId<Stmt>>),
    While(ASTId<Expr>, Vec<ASTId<Stmt>>),
    Say(FieldInit),
    Fn(FnDecl),
    Class(Box<ClassDecl>)
}

impl ASTKind for Stmt {
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

pub struct ASTNode {
    pub pos: SourcePosition,
    pub value: Box<dyn ASTKind>
}

pub struct ASTId<T> {
    id: usize,
    _marker: PhantomData<T>
}

impl Copy for ASTId<Stmt> {}
impl Clone for ASTId<Stmt> {
    fn clone(&self) -> ASTId<Stmt> {
        *self
    }
}

impl ASTId<Expr> {
    pub fn as_comma_separated(&self, ast: &AST) -> Vec<ASTId<Expr>> {
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

impl Copy for ASTId<Expr> {}
impl Clone for ASTId<Expr> {
    fn clone(&self) -> ASTId<Expr> {
        *self
    }
}

pub struct AST {
    nodes: Vec<ASTNode>
}

impl AST {
    pub fn get<T: 'static>(&self, id: &ASTId<T>) -> &T {
        self.nodes[id.id].value.as_any().downcast_ref::<T>().unwrap()
    }

    pub fn get_mut<T: 'static>(&mut self, id: &ASTId<T>) -> &mut T {
        self.nodes[id.id].value.as_any_mut().downcast_mut::<T>().unwrap()
    }

    pub fn pos<T: 'static>(&self, id: &ASTId<T>) -> &SourcePosition {
        &self.nodes[id.id].pos
    }

    pub fn get_root(&self) -> ASTId<Stmt> {
        ASTId { id: self.nodes.len() - 1, _marker: PhantomData }
    }

    fn add_stmt(&mut self, kind: Stmt, pos: SourcePosition) -> ASTId<Stmt> {
        let node = ASTNode { value: Box::new(kind), pos };
        self.nodes.push(node);
        return ASTId { id: self.nodes.len() - 1, _marker: PhantomData };
    }

    fn add_expr(&mut self, kind: Expr, pos: SourcePosition) -> ASTId<Expr> {
        let node = ASTNode { value: Box::new(kind), pos };
        self.nodes.push(node);
        return ASTId { id: self.nodes.len() - 1, _marker: PhantomData };
    }
}

macro_rules! parse_error {
    ($self:ident, $pos:expr, $($arg:tt)*) => { return Err($self.error(format!($($arg)*), $pos)) };
}

enum ParseContext {
    Other,
    Class(String),
    Call
}

pub struct Parser<'parser, 'vm> {
    tokens: &'vm mut TokenStream<'vm>,
    ast: &'parser mut AST,
    ctx: ParseContext
}

impl<'parser, 'vm> Parser<'parser, 'vm> {
    pub fn parse(tokens: &'vm mut TokenStream<'vm>) -> Result<AST, anyhow::Error> {
        let mut ast = AST {
            nodes: Vec::new()
        };

        let mut parser = Parser {
            tokens,
            ast: &mut ast,
            ctx: ParseContext::Other
        };

        let pos = parser.tokens.peek(0).pos.clone();
        let mut stmts: Vec<ASTId<Stmt>> = Vec::new();
        while parser.tokens.has_next() {
            stmts.push(parser.parse_stmt()?);
        }
        let block = parser.ast.add_expr(Expr::Block(stmts, None), pos.clone());
        ast.add_stmt(Stmt::Expression(block), pos);

        Ok(ast)
    }

    fn error(&self, message: impl Into<String>, pos: &SourcePosition) -> anyhow::Error {
        anyhow!(format!("{}\nat {}", message.into(), pos))
    }

    fn parse_identifier(&mut self) -> Result<String, anyhow::Error> {
        let token = self.tokens.expect(TokenType::Identifier)?;
        Ok(token.lexeme.clone())
    }

    fn parse_identifier_expr(&mut self) -> Result<ASTId<Expr>, anyhow::Error> {
        let token = self.tokens.expect(TokenType::Identifier)?;
        let id = self.ast.add_expr(Expr::Identifier(token.lexeme.clone()), token.pos.clone());
        Ok(id)
    }

    fn parse_stmt(&mut self) -> Result<ASTId<Stmt>, anyhow::Error> {
        match self.tokens.peek(0).kind {
            TokenType::Say => self.parse_say(),
            TokenType::While => self.parse_while(),
            TokenType::Fn => self.parse_fn(),
            TokenType::Class => self.parse_class(),
            TokenType::Return => self.parse_return(),
            TokenType::Throw => self.parse_throw(),
            TokenType::Try => self.parse_trycatch(),
            _ => self.parse_expr_stmt()
        }
    }

    fn parse_say(&mut self) -> Result<ASTId<Stmt>, anyhow::Error> {
        let pos = self.tokens.expect(TokenType::Say)?.pos.clone();
        let name = self.parse_identifier()?;

        let expr = if let Some(_) = self.tokens.next_if(TokenType::Equal) {
            Some(self.parse_expr()?)
        } else {
            None
        };

        self.tokens.expect(TokenType::Semicolon)?;
        let field_init = FieldInit { name, value: expr };
        Ok(self.ast.add_stmt(Stmt::Say(field_init), pos))
    }

    /// Parses an fn statement, which includes the `fn` keyword and a function declaration.
    fn parse_fn(&mut self) -> Result<ASTId<Stmt>, anyhow::Error> {
        let pos = self.tokens.expect(TokenType::Fn)?.pos.clone();
        let name = self.parse_identifier()?;
        let fn_decl = self.parse_fn_decl(name)?;
        Ok(self.ast.add_stmt(Stmt::Fn(fn_decl), pos))
    }

    /// Parses a function declaration, including the function name, parameters, and body.
    fn parse_fn_decl(&mut self, name: String) -> Result<FnDecl, anyhow::Error> {
        self.tokens.expect(TokenType::LeftParen)?;
        let params = self.parse_params(TokenType::RightParen)?;
        self.tokens.expect(TokenType::LeftBrace)?;
        let body = self.parse_block()?;
        Ok(FnDecl {
            name,
            params,
            body
        })
    }

    /// Parses a class initializer method.
    /// The init method may contain a super() call in the body as the first statement.
    fn parse_init(&mut self, has_superclass: bool) -> Result<ASTId<Stmt>, anyhow::Error> {
        let pos = self.tokens.peek(0).pos.clone();
        let ParseContext::Class(name) = &self.ctx else { unreachable!() };
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

        let body = self.ast.add_expr(Expr::Block(stmts, None), pos.clone());
        let fn_decl = FnDecl { name, params, body };
        Ok(self.ast.add_stmt(Stmt::Fn(fn_decl), pos))
    }

    fn parse_params(&mut self, end_token: TokenType) -> Result<Vec<ASTId<Expr>>, anyhow::Error> {
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

    fn virtual_super_call(&mut self, pos: SourcePosition) -> Result<ASTId<Stmt>, anyhow::Error> {
        let super_expr = self.ast.add_expr(Expr::Super, pos.clone());
        let expr = self.ast.add_expr(Expr::Call(super_expr, None), pos.clone());
        Ok(self.ast.add_stmt(Stmt::Expression(expr), pos.clone()))
    }

    fn parse_class(&mut self) -> Result<ASTId<Stmt>, anyhow::Error> {
        let pos = self.tokens.expect(TokenType::Class)?.pos.clone();
        let name = self.parse_identifier()?;

        let prev_ctx = std::mem::replace(&mut self.ctx, ParseContext::Class(name.clone()));

        let superclass = match self.tokens.next_if(TokenType::Colon) {
            Some(_) => Some(self.parse_identifier()?),
            None => None
        };

        self.tokens.expect(TokenType::LeftBrace)?;

        let mut fields: HashSet<String> = HashSet::default();
        let mut field_stmts: Vec<ASTId<Stmt>> = Vec::new();
        let mut method_stmts: Vec<ASTId<Stmt>> = Vec::new();
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
                            let token = self.tokens.peek(0).clone();
                            let fn_decl = self.parse_fn_decl(name)?;
                            if fn_decl.params.len() != 1 {
                                parse_error!(self, &token.pos, "Getter must have exactly one parameter")
                            }
                            let stmt = Some(self.ast.add_stmt(Stmt::Fn(fn_decl), token.pos));
                            getter = stmt;
                        },
                        "set" => {
                            let token = self.tokens.peek(0).clone();
                            let fn_decl = self.parse_fn_decl(name)?;
                            if fn_decl.params.len() != 2 {
                                parse_error!(self, &token.pos, "Setter must have exactly two parameters")
                            }
                            let stmt = Some(self.ast.add_stmt(Stmt::Fn(fn_decl), token.pos));
                            setter = stmt;
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
                                let id = self.ast.add_expr(Expr::Identifier(name), pos.clone());
                                let assign = self.ast.add_expr(Expr::Binary(Operator::Assign(None), id, value), pos.clone());
                                field_stmts.push(self.ast.add_stmt(Stmt::Expression(assign), pos.clone()));
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

                let body = self.ast.add_expr(Expr::Block(stmts, None), pos.clone());
                let fn_decl = FnDecl {
                    name: name.clone(),
                    params: Vec::new(),
                    body
                };
                self.ast.add_stmt(Stmt::Fn(fn_decl), pos.clone())
            }
        };

        let Stmt::Fn(fn_decl) = self.ast.get(&init) else { unreachable!() };
        let Expr::Block(body, _) = self.ast.get_mut(&fn_decl.body.clone()) else { unreachable!() };
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

        self.ctx = prev_ctx;
        Ok(self.ast.add_stmt(Stmt::Class(class_decl), pos))
    }

    fn parse_while(&mut self) -> Result<ASTId<Stmt>, anyhow::Error> {
        let pos = self.tokens.expect(TokenType::While)?.pos.clone();
        let condition = self.parse_expr()?;
        let body = self.parse_stmt_body()?;
        Ok(self.ast.add_stmt(Stmt::While(condition, body), pos))
    }

    fn parse_return(&mut self) -> Result<ASTId<Stmt>, anyhow::Error> {
        let pos = self.tokens.expect(TokenType::Return)?.pos.clone();
        let expr = match self.tokens.match_next(TokenType::Semicolon) {
            true => None,
            false => Some(self.parse_expr()?)
        };
        self.tokens.expect(TokenType::Semicolon)?;
        Ok(self.ast.add_stmt(Stmt::Return(expr), pos))
    }

    fn parse_throw(&mut self) -> Result<ASTId<Stmt>, anyhow::Error> {
        let pos = self.tokens.expect(TokenType::Throw)?.pos.clone();
        let expr = self.parse_expr()?;
        self.tokens.expect(TokenType::Semicolon)?;
        Ok(self.ast.add_stmt(Stmt::Throw(expr), pos))
    }

    fn parse_trycatch(&mut self) -> Result<ASTId<Stmt>, anyhow::Error> {
        let pos = self.tokens.expect(TokenType::Try)?.pos.clone();
        let try_body = self.parse_stmt_body()?;

        let catch = if let Some(token) = self.tokens.next_if(TokenType::Catch) {
            let pos = token.pos.clone();
            let param = match self.tokens.peek(0).kind {
                TokenType::Identifier => Some(self.parse_identifier_expr()?),
                TokenType::LeftParen => {
                    self.tokens.expect(TokenType::LeftParen)?;
                    let params = self.parse_params(TokenType::RightParen)?;
                    if params.len() != 1 {
                        parse_error!(self, &pos, "Expected one parameter in catch block")
                    }
                    Some(params[0])
                },
                _ => None
            };
            let body = self.parse_stmt_body()?;
            Some(self.ast.add_stmt(Stmt::Catch(param, body), pos))
        } else {
            None
        };

        let finally = if let Some(token) = self.tokens.next_if(TokenType::Finally) {
            let body = self.parse_stmt_body()?;
            Some(self.ast.add_stmt(Stmt::Finally(body), token.pos.clone()))
        } else {
            None
        };

        if catch.is_none() && finally.is_none() {
            parse_error!(self, &pos, "Expected catch or finally block")
        }

        Ok(self.ast.add_stmt(Stmt::Try(try_body, catch, finally), pos))
    }

    fn parse_if(&mut self) -> Result<ASTId<Expr>, anyhow::Error> {
        let pos = self.tokens.peek(0).pos.clone();
        let condition = self.parse_expr()?;
        let then = self.parse_expr_body()?;
        let otherwise = match self.tokens.next_if(TokenType::Else) {
            Some(_) => Some(self.parse_expr()?),
            None => None
        };

        Ok(self.ast.add_expr(Expr::If(condition, then, otherwise), pos))
    }

    /// Parses a statement or an expression, and converts it to an expression if necessary.
    fn parse_expr_body(&mut self) -> Result<ASTId<Expr>, anyhow::Error> {
        let token = self.tokens.peek(0);
        match token.kind {
            kind if kind.starts_statement() => {
                let stmt = self.parse_stmt()?;
                Ok(self.ast.add_expr(Expr::Block(vec![stmt], None), token.pos.clone()))
            },
            _ => self.parse_expr()
        }
    }

    /// Parses a statement or a block, and converts it to a list of statements.
    fn parse_stmt_body(&mut self) -> Result<Vec<ASTId<Stmt>>, anyhow::Error> {
        let token = self.tokens.peek(0);
        match token.kind {
            TokenType::LeftBrace => {
                self.tokens.expect(TokenType::LeftBrace)?;
                let stmts = self.parse_stmts()?;
                self.tokens.expect(TokenType::RightBrace)?;
                Ok(stmts)
            },
            _ => Ok(vec![self.parse_stmt()?])
        }
    }

    fn parse_block(&mut self) -> Result<ASTId<Expr>, anyhow::Error> {
        let pos = self.tokens.peek(0).pos.clone();

        let mut stmts: Vec<ASTId<Stmt>> = Vec::new();
        let mut result_expr: Option<ASTId<Expr>> = None;

        while !self.tokens.match_next(TokenType::RightBrace) {
            match self.tokens.peek(0).kind {
                kind if kind.starts_statement() => stmts.push(self.parse_stmt()?),
                _ => {
                    let expr = self.parse_expr()?;
                    if self.tokens.match_next(TokenType::RightBrace) {
                        result_expr = Some(expr);
                        break;
                    } else {
                        if !matches!(self.ast.get(&expr), Expr::Block(_, _) | Expr::If(_, _, _)) {
                            self.tokens.expect(TokenType::Semicolon)?;
                        }
                        let stmt = self.ast.add_stmt(Stmt::Expression(expr), pos.clone());
                        stmts.push(stmt);
                    }
                }
            }
        }

        self.tokens.expect(TokenType::RightBrace)?;
        Ok(self.ast.add_expr(Expr::Block(stmts, result_expr), pos))
    }

    fn parse_stmts(&mut self) -> Result<Vec<ASTId<Stmt>>, anyhow::Error> {
        let mut stmts: Vec<ASTId<Stmt>> = Vec::new();
        while !self.tokens.match_next(TokenType::RightBrace) {
            stmts.push(self.parse_stmt()?);
        }
        Ok(stmts)
    }

    fn parse_expr_stmt(&mut self) -> Result<ASTId<Stmt>, anyhow::Error> {
        let token = self.tokens.peek(0);
        let expr = self.parse_expr()?;
        let expr_type = self.ast.get(&expr);
        if !matches!(expr_type, Expr::Block(_, _) | Expr::If(_, _, _)) {
            self.tokens.expect(TokenType::Semicolon)?;
        }
        Ok(self.ast.add_stmt(Stmt::Expression(expr), token.pos.clone()))
    }

    fn parse_expr(&mut self) -> Result<ASTId<Expr>, anyhow::Error> {
        self.parse_expr_precedence(0)
    }

    fn parse_expr_precedence(&mut self, min_precedence: u8) -> Result<ASTId<Expr>, anyhow::Error> {
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

    fn parse_expr_infix(&mut self, op: Operator, expr: ASTId<Expr>) -> Result<ASTId<Expr>, anyhow::Error> {
        let pos = self.ast.pos(&expr).clone();

        let kind = match &op {
            Operator::Assign(Some(assign_op)) => {
                // Normalize compound assignment, for example convert (a += 1) to (a = a + 1)
                let mut right = self.parse_expr_precedence(op.infix_precedence().unwrap())?;
                let kind = Expr::Binary(assign_op.as_ref().clone(), expr, right);
                right = self.ast.add_expr(kind, pos.clone());
                Expr::Binary(op, expr, right)
            },
            Operator::MemberAccess => {
                let id = self.parse_identifier()?;
                let id = self.ast.add_expr(Expr::Literal(Literal::String(id)), pos.clone());
                Expr::Index(expr, id)
            },
            Operator::Arrow => {
                let right = self.parse_expr_precedence(op.infix_precedence().unwrap())?;
                let params = expr.as_comma_separated(self.ast).iter()
                    .map(|id| match self.ast.get(id) {
                        Expr::Identifier(_) => Ok(*id),
                        _ => parse_error!(self, &pos, "Invalid lambda parameter")
                    })
                    .collect::<Result<Vec<_>, anyhow::Error>>()?;
                Expr::Literal(Literal::Lambda(FnDecl {
                    name: String::from("lambda"),
                    params,
                    body: right
                }))
            },
            _ => {
                let right = self.parse_expr_precedence(op.infix_precedence().unwrap())?;
                Expr::Binary(op, expr, right)
            }
        };

        Ok(self.ast.add_expr(kind, pos.clone()))
    }

    fn parse_expr_prefix(&mut self, op: Operator) -> Result<ASTId<Expr>, anyhow::Error> {
        let pos = self.tokens.peek(0).pos.clone();
        let kind = match &op {
            Operator::Group => {
                match self.tokens.next_if(TokenType::RightParen) {
                    Some(_) => {
                        let Some(Operator::Arrow) = Operator::parse_infix(self.tokens, 0) else {
                            parse_error!(self, &pos, "Unexpected token: Expected '=>'")
                        };
                        let right = self.parse_expr_precedence(Operator::Arrow.infix_precedence().unwrap())?;
                        Expr::Literal(Literal::Lambda(FnDecl {
                            name: String::from("lambda"),
                            params: Vec::new(),
                            body: right
                        }))
                    },
                    None => {
                        let expr = self.parse_expr()?;
                        self.tokens.expect(TokenType::RightParen)?;
                        return Ok(expr);
                    }
                }
            },
            Operator::Array => {
                let expr = match self.tokens.next_if(TokenType::RightBracket) {
                    Some(_) => None,
                    None => {
                        let expr = self.parse_expr()?;
                        self.tokens.expect(TokenType::RightBracket)?;
                        Some(expr)
                    }
                };

                return Ok(self.ast.add_expr(Expr::Literal(Literal::Array(expr)), pos));
            },
            _ => {
                let right = self.parse_expr_precedence(op.prefix_precedence().unwrap())?;
                Expr::Unary(op, right)
            }
        };
        Ok(self.ast.add_expr(kind, pos))
    }

    fn parse_expr_postfix(&mut self, op: Operator, expr: ASTId<Expr>) -> Result<ASTId<Expr>, anyhow::Error> {
        let pos = self.ast.pos(&expr).clone();
        match op {
            Operator::Call => {
                let args = self.parse_call_arguments()?;
                Ok(self.ast.add_expr(Expr::Call(expr, args), pos))
            },
            Operator::Index => {
                let index = self.parse_expr()?;
                self.tokens.expect(TokenType::RightBracket)?;
                Ok(self.ast.add_expr(Expr::Index(expr, index), pos))
            },
            _ => unreachable!()
        }
    }

    fn parse_expr_atom(&mut self) -> Result<ASTId<Expr>, anyhow::Error> {
        let token = self.tokens.next().clone();
        let pos = token.pos.clone();

        let kind = match token.kind {
            TokenType::StringLiteral => {
                let val = token.lexeme.clone();
                let val = &val[1..val.len() - 1]; // Strip quotes
                Expr::Literal(Literal::String(String::from(val)))
            },
            TokenType::NumericLiteral => Expr::Literal(Literal::Number(token.lexeme.parse().unwrap())),
            TokenType::If => return self.parse_if(),
            TokenType::LeftBrace => return self.parse_block(),
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

        Ok(self.ast.add_expr(kind, pos))
    }

    fn parse_super(&mut self) -> Result<ASTId<Expr>, anyhow::Error> {
        let token = self.tokens.next();
        let pos = token.pos.clone();

        match token.kind {
            TokenType::Dot => {
                let super_expr = self.ast.add_expr(Expr::Super, pos.clone());
                let id = self.parse_identifier()?;
                let id = self.ast.add_expr(Expr::Literal(Literal::String(id)), pos.clone());
                let expr = self.ast.add_expr(Expr::Index(super_expr, id), pos.clone());
                Ok(expr)
            },
            TokenType::LeftBracket => {
                let super_expr = self.ast.add_expr(Expr::Super, pos.clone());
                let expr = self.parse_expr()?;
                self.tokens.expect(TokenType::RightBracket)?;
                let expr = self.ast.add_expr(Expr::Index(super_expr, expr), pos.clone());
                Ok(expr)
            },
            TokenType::LeftParen => parse_error!(self, &pos, "Super call must be the first statement in an init block"),
            _ => parse_error!(self, &pos, "Unexpected token {token}"),
        }
    }

    fn parse_super_call(&mut self) -> Result<ASTId<Stmt>, anyhow::Error> {
        let pos = self.tokens.expect(TokenType::Super)?.pos.clone();
        self.tokens.expect(TokenType::LeftParen)?;
        let args = self.parse_call_arguments()?;
        self.tokens.expect(TokenType::Semicolon)?;
        let super_expr = self.ast.add_expr(Expr::Super, pos.clone());
        let expr = self.ast.add_expr(Expr::Call(super_expr, args), pos.clone());
        Ok(self.ast.add_stmt(Stmt::Expression(expr), pos))
    }

    fn parse_call_arguments(&mut self) -> Result<Option<ASTId<Expr>>, anyhow::Error> {
        let prev_ctx = std::mem::replace(&mut self.ctx, ParseContext::Call);

        let args = match self.tokens.next_if(TokenType::RightParen) {
            Some(_) => None,
            None => {
                let args = self.parse_expr()?;
                self.tokens.expect(TokenType::RightParen)?;
                Some(args)
            }
        };

        self.ctx = prev_ctx;

        Ok(args)
    }
}
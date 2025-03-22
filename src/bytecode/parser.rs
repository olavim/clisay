use std::{any::Any, marker::PhantomData};

use anyhow::bail;

use crate::lexer::{SourcePosition, TokenStream, TokenType};

use super::{gc::GcRef, operator::Operator, Gc};

pub enum Literal {
    Null,
    Boolean(bool),
    Number(f64),
    String(GcRef<String>)
}

pub trait ASTKind {
    fn as_any(&self) -> &dyn Any;
}

pub enum Expr {
    Literal(Literal),
    Identifier(GcRef<String>),
    Unary(Operator, ASTId<Expr>),
    Binary(Operator, ASTId<Expr>, ASTId<Expr>),
    Ternary(ASTId<Expr>, ASTId<Expr>, ASTId<Expr>),
    Call(ASTId<Expr>, Vec<ASTId<Expr>>)
}

impl ASTKind for Expr {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

pub enum Stmt {
    Block(Vec<ASTId<Stmt>>),
    Expression(ASTId<Expr>),
    Return(Option<ASTId<Expr>>),
    Say(GcRef<String>, Option<ASTId<Expr>>),
    Fn(GcRef<String>, Vec<GcRef<String>>, ASTId<Stmt>),
    If(ASTId<Expr>, ASTId<Stmt>, Option<ASTId<Stmt>>),
    While(ASTId<Expr>, ASTId<Stmt>)
}

impl ASTKind for Stmt {
    fn as_any(&self) -> &dyn Any {
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

pub struct Parser<'parser, 'vm> {
    gc: &'vm mut Gc,
    tokens: &'vm mut TokenStream<'vm>,
    ast: &'parser mut AST
}

impl<'parser, 'vm> Parser<'parser, 'vm> {
    pub fn parse(gc: &'vm mut Gc, tokens: &'vm mut TokenStream<'vm>) -> Result<AST, anyhow::Error> {
        let mut ast = AST {
            nodes: Vec::new()
        };

        let mut parser = Parser {
            gc,
            tokens,
            ast: &mut ast
        };

        let pos = parser.tokens.peek(0).pos.clone();
        let mut stmts: Vec<ASTId<Stmt>> = Vec::new();
        while parser.tokens.has_next() {
            stmts.push(parser.parse_stmt()?);
        }
        ast.add_stmt(Stmt::Block(stmts), pos);

        return Ok(ast);
    }

    fn parse_identifier(&mut self) -> Result<GcRef<String>, anyhow::Error> {
        let token = self.tokens.expect(TokenType::Identifier)?;
        return Ok(self.gc.intern(token.lexeme.clone()));
    }

    fn parse_stmt(&mut self) -> Result<ASTId<Stmt>, anyhow::Error> {
        return match self.tokens.peek(0).kind {
            TokenType::Say => self.parse_say(),
            TokenType::Fn => self.parse_fn(),
            TokenType::If => self.parse_if(),
            TokenType::While => self.parse_while(),
            TokenType::Return => self.parse_return(),
            TokenType::LeftBrace => self.parse_block(),
            _ => self.parse_expr_stmt()
        };
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
        return Ok(self.ast.add_stmt(Stmt::Say(name, expr), pos));
    }

    fn parse_fn(&mut self) -> Result<ASTId<Stmt>, anyhow::Error> {
        let pos = self.tokens.expect(TokenType::Fn)?.pos.clone();
        let name = self.parse_identifier()?;

        self.tokens.expect(TokenType::LeftParen)?;

        let mut params: Vec<GcRef<String>> = Vec::new();
        while !self.tokens.match_next(TokenType::RightParen) {
            if params.len() > 0 {
                self.tokens.expect(TokenType::Comma)?;
            }
            params.push(self.parse_identifier()?);
        }

        self.tokens.expect(TokenType::RightParen)?;

        let body = self.parse_block()?;

        return Ok(self.ast.add_stmt(Stmt::Fn(name, params, body), pos));
    }

    fn parse_if(&mut self) -> Result<ASTId<Stmt>, anyhow::Error> {
        let pos = self.tokens.expect(TokenType::If)?.pos.clone();
        let condition = self.parse_expr()?;
        let then = match self.tokens.match_next(TokenType::LeftBrace) {
            true => self.parse_block()?,
            false => self.parse_expr_stmt()?
        };
        let otherwise = match self.tokens.next_if(TokenType::Else) {
            Some(_) => match self.tokens.peek(0).kind {
                TokenType::LeftBrace => Some(self.parse_block()?),
                TokenType::If => Some(self.parse_if()?),
                _ => Some(self.parse_expr_stmt()?)
            },
            None => None
        };

        return Ok(self.ast.add_stmt(Stmt::If(condition, then, otherwise), pos));
    }

    fn parse_while(&mut self) -> Result<ASTId<Stmt>, anyhow::Error> {
        let pos = self.tokens.expect(TokenType::While)?.pos.clone();
        let condition = self.parse_expr()?;
        let body = match self.tokens.match_next(TokenType::LeftBrace) {
            true => self.parse_block()?,
            false => self.parse_expr_stmt()?
        };
        return Ok(self.ast.add_stmt(Stmt::While(condition, body), pos));
    }

    fn parse_return(&mut self) -> Result<ASTId<Stmt>, anyhow::Error> {
        let pos = self.tokens.expect(TokenType::Return)?.pos.clone();
        let expr = match self.tokens.match_next(TokenType::Semicolon) {
            true => None, 
            false => Some(self.parse_expr()?)
        };
        self.tokens.expect(TokenType::Semicolon)?;
        return Ok(self.ast.add_stmt(Stmt::Return(expr), pos));
    }

    fn parse_block(&mut self) -> Result<ASTId<Stmt>, anyhow::Error> {
        let pos = self.tokens.expect(TokenType::LeftBrace)?.pos.clone();
        
        let mut stmts: Vec<ASTId<Stmt>> = Vec::new();
        while !self.tokens.match_next(TokenType::RightBrace) {
            stmts.push(self.parse_stmt()?);
        }

        self.tokens.expect(TokenType::RightBrace)?;
        return Ok(self.ast.add_stmt(Stmt::Block(stmts), pos));
    }

    fn parse_expr_stmt(&mut self) -> Result<ASTId<Stmt>, anyhow::Error> {
        let pos = self.tokens.peek(0).pos.clone();
        let expr = self.parse_expr()?;
        self.tokens.expect(TokenType::Semicolon)?;
        return Ok(self.ast.add_stmt(Stmt::Expression(expr), pos));
    }

    fn parse_expr(&mut self) -> Result<ASTId<Expr>, anyhow::Error> {
        return self.parse_expr_precedence(0);
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
    
        return Ok(left);
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
            // Operator::MemberAccess => {
            //     let member = stream.expect(TokenType::Identifier)?.lexeme.clone();
            //     ExprKind::MemberAccess(Box::new(expr), member)
            // },
            Operator::Ternary => {
                let left = self.parse_expr_precedence(0)?;
                self.tokens.expect(TokenType::Colon)?;
                let right = self.parse_expr_precedence(0)?;
                Expr::Ternary(expr, left, right)
            },
            _ => {
                let right = self.parse_expr_precedence(op.infix_precedence().unwrap())?;
                Expr::Binary(op, expr, right)
            }
        };
    
        return Ok(self.ast.add_expr(kind, pos.clone()));
    }

    fn parse_expr_prefix(&mut self, op: Operator) -> Result<ASTId<Expr>, anyhow::Error> {
        let pos = self.tokens.peek(0).pos.clone();
        let kind = match &op {
            Operator::Group => {
                let expr = self.parse_expr_precedence(0)?;
                self.tokens.expect(TokenType::RightParen)?;
                return Ok(expr);
            },
            _ => {
                let right = self.parse_expr_precedence(op.prefix_precedence().unwrap())?;
                Expr::Unary(op, right)
            }
        };
        return Ok(self.ast.add_expr(kind, pos));
    }

    fn parse_expr_postfix(&mut self, op: Operator, expr: ASTId<Expr>) -> Result<ASTId<Expr>, anyhow::Error> {
        let pos = self.ast.pos(&expr).clone();
        match op {
            Operator::Call => {
                let mut args: Vec<ASTId<Expr>> = Vec::new();
                while !self.tokens.match_next(TokenType::RightParen) {
                    if args.len() > 0 {
                        self.tokens.expect(TokenType::Comma)?;
                    }
                    args.push(self.parse_expr()?);
                }
        
                self.tokens.expect(TokenType::RightParen)?;
                Ok(self.ast.add_expr(Expr::Call(expr, args), pos))
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
                Expr::Literal(Literal::String(self.gc.intern(val)))
            },
            TokenType::NumericLiteral => Expr::Literal(Literal::Number(token.lexeme.parse().unwrap())),
            TokenType::Null => Expr::Literal(Literal::Null),
            TokenType::True => Expr::Literal(Literal::Boolean(true)),
            TokenType::False => Expr::Literal(Literal::Boolean(false)),
            // TokenType::This => {
            //     ExprKind::This
            // },
            // TokenType::Super => {
            //     ExprKind::Super
            // },
            TokenType::Identifier => Expr::Identifier(self.gc.intern(token.lexeme.clone())),
            _ => bail!("Unexpected token {} at {}", token, token.pos)
        };
    
        return Ok(self.ast.add_expr(kind, pos));
    }
    
}
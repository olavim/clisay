//! Recursive-descent parser. Turns tokens into an AST.

mod precedence;

use anyhow::anyhow;

use crate::ast::{Ast, AstId, CatchClause, TypeDecl, Expr, FieldInit, FnDecl, Literal, Operator, Stmt, Symbol};
use crate::frontend::lex::{SourcePosition, TokenStream, TokenType};

macro_rules! parse_error {
    ($self:ident, $pos:expr, $($arg:tt)*) => { return Err($self.error(format!($($arg)*), $pos)) };
}

pub struct Parser<'parser, 'vm> {
    tokens: &'vm mut TokenStream<'vm>,
    ast: &'parser mut Ast,
    current_class: Option<String>
}

impl<'parser, 'vm> Parser<'parser, 'vm> {
    pub fn parse(tokens: &'vm mut TokenStream<'vm>) -> Result<Ast, anyhow::Error> {
        let mut ast = Ast::new();

        let mut parser = Parser {
            tokens,
            ast: &mut ast,
            current_class: None
        };

        let pos = parser.tokens.peek(0).pos.clone();
        let mut stmts: Vec<AstId<Stmt>> = Vec::new();
        while parser.tokens.has_next() {
            stmts.push(parser.parse_stmt()?);
        }
        let block = parser.ast.add_expr(Expr::Block(stmts), pos.clone());
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

    fn parse_identifier_expr(&mut self) -> Result<AstId<Expr>, anyhow::Error> {
        let token = self.tokens.expect(TokenType::Identifier)?;
        let pos = token.pos.clone();
        let name = self.ast.intern(&token.lexeme);
        Ok(self.ast.add_expr(Expr::Identifier(name), pos))
    }

    fn parse_stmt(&mut self) -> Result<AstId<Stmt>, anyhow::Error> {
        match self.tokens.peek(0).kind {
            TokenType::Say => self.parse_say(),
            TokenType::While => self.parse_while(),
            TokenType::Fn => self.parse_fn(),
            TokenType::Type => self.parse_class(),
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
        Ok(self.ast.add_stmt(Stmt::If(condition, then, otherwise), pos))
    }

    fn parse_block_stmt(&mut self) -> Result<AstId<Stmt>, anyhow::Error> {
        let pos = self.tokens.peek(0).pos.clone();
        let body = self.parse_block_or_stmt()?;
        Ok(self.ast.add_stmt(Stmt::Block(body), pos))
    }

    fn parse_say(&mut self) -> Result<AstId<Stmt>, anyhow::Error> {
        let pos = self.tokens.expect(TokenType::Say)?.pos.clone();
        let name = self.parse_identifier()?;
        let name = self.ast.intern(&name);

        let expr = if let Some(_) = self.tokens.next_if(TokenType::Equal) {
            Some(self.parse_expr()?)
        } else {
            None
        };

        self.tokens.expect(TokenType::Semicolon)?;
        let field_init = FieldInit { name, value: expr };
        Ok(self.ast.add_stmt(Stmt::Say(field_init), pos))
    }

    fn parse_fn(&mut self) -> Result<AstId<Stmt>, anyhow::Error> {
        let pos = self.tokens.expect(TokenType::Fn)?.pos.clone();
        let name = self.parse_identifier()?;
        let name = self.ast.intern(&name);
        let fn_decl = self.parse_fn_decl(name)?;
        Ok(self.ast.add_stmt(Stmt::Fn(fn_decl), pos))
    }

    fn parse_fn_decl(&mut self, name: Symbol) -> Result<FnDecl, anyhow::Error> {
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
        let name = self.ast.intern(&name);
        let fn_decl = self.parse_fn_decl(name)?;
        if fn_decl.params.len() != arity {
            parse_error!(self, &token.pos, "{}", arity_error)
        }
        Ok(self.ast.add_stmt(Stmt::Fn(fn_decl), token.pos))
    }

    fn init_name(&mut self) -> Symbol {
        let class = self.current_class.clone().expect("init parsed outside a class");
        self.ast.intern(&format!("{}.init", class))
    }

    fn parse_init(&mut self, has_superclass: bool) -> Result<AstId<Stmt>, anyhow::Error> {
        let pos = self.tokens.peek(0).pos.clone();
        let name = self.init_name();
        self.tokens.expect(TokenType::LeftParen)?;
        let params = self.parse_params(TokenType::RightParen)?;
        self.tokens.expect(TokenType::LeftBrace)?;

        let mut stmts = Vec::new();
        if has_superclass && matches!(
            (self.tokens.peek(0).kind, self.tokens.peek(1).kind),
            (TokenType::Super, TokenType::LeftParen)
        ) {
            stmts.push(self.parse_super_call()?);
        }

        stmts.extend(self.parse_stmts()?);
        self.tokens.expect(TokenType::RightBrace)?;

        let body = self.ast.add_expr(Expr::Block(stmts), pos.clone());
        let fn_decl = FnDecl { name, params, body };
        Ok(self.ast.add_stmt(Stmt::Fn(fn_decl), pos))
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

    fn parse_class(&mut self) -> Result<AstId<Stmt>, anyhow::Error> {
        let pos = self.tokens.expect(TokenType::Type)?.pos.clone();
        let class_name = self.parse_identifier()?;
        let class_sym = self.ast.intern(&class_name);

        let prev_class = std::mem::replace(&mut self.current_class, Some(class_name.clone()));

        let superclass = match self.tokens.next_if(TokenType::Colon) {
            Some(_) => {
                let super_name = self.parse_identifier()?;
                Some(self.ast.intern(&super_name))
            },
            None => None
        };

        self.tokens.expect(TokenType::LeftBrace)?;

        let mut fields: std::collections::HashSet<Symbol> = std::collections::HashSet::default();
        let mut field_inits: Vec<(Symbol, AstId<Expr>)> = Vec::new();
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
                            let value = self.tokens.next_if(TokenType::Equal)
                                .map(|_| self.parse_expr())
                                .transpose()?;

                            self.tokens.expect(TokenType::Semicolon)?;
                            let field = self.ast.intern(&name);
                            fields.insert(field);

                            if let Some(value) = value {
                                field_inits.push((field, value));
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

        let init_name = self.ast.intern(&format!("{}.init", class_name));

        let class_decl = Box::new(TypeDecl {
            name: class_sym,
            superclass,
            init_name,
            init,
            getter,
            setter,
            fields,
            field_inits,
            methods: method_stmts
        });

        self.current_class = prev_class;
        Ok(self.ast.add_stmt(Stmt::Type(class_decl), pos))
    }

    fn parse_while(&mut self) -> Result<AstId<Stmt>, anyhow::Error> {
        let pos = self.tokens.expect(TokenType::While)?.pos.clone();
        let condition = self.parse_expr()?;
        let body = self.parse_block_or_stmt()?;
        Ok(self.ast.add_stmt(Stmt::While(condition, body), pos))
    }

    fn parse_return(&mut self) -> Result<AstId<Stmt>, anyhow::Error> {
        let pos = self.tokens.expect(TokenType::Return)?.pos.clone();
        let expr = match self.tokens.match_next(TokenType::Semicolon) {
            true => None,
            false => Some(self.parse_expr()?)
        };
        self.tokens.expect(TokenType::Semicolon)?;
        Ok(self.ast.add_stmt(Stmt::Return(expr), pos))
    }

    fn parse_throw(&mut self) -> Result<AstId<Stmt>, anyhow::Error> {
        let pos = self.tokens.expect(TokenType::Throw)?.pos.clone();
        let expr = self.parse_expr_semi()?;
        Ok(self.ast.add_stmt(Stmt::Throw(expr), pos))
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

        Ok(self.ast.add_stmt(Stmt::Try(try_body, catch, finally), pos))
    }

    fn parse_block(&mut self) -> Result<AstId<Expr>, anyhow::Error> {
        let pos = self.tokens.peek(0).pos.clone();
        self.tokens.expect(TokenType::LeftBrace)?;
        let stmts = self.parse_stmts()?;
        self.tokens.expect(TokenType::RightBrace)?;
        Ok(self.ast.add_expr(Expr::Block(stmts), pos))
    }

    fn parse_block_or_stmt(&mut self) -> Result<AstId<Expr>, anyhow::Error> {
        if self.tokens.match_next(TokenType::LeftBrace) {
            self.parse_block()
        } else {
            let pos = self.tokens.peek(0).pos.clone();
            let stmt = self.parse_stmt()?;
            Ok(self.ast.add_expr(Expr::Block(vec![stmt]), pos))
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

    fn make_lambda(&mut self, params: Vec<AstId<Expr>>, body: AstId<Expr>) -> Expr {
        let name = self.ast.intern("lambda");
        Expr::Literal(Literal::Lambda(FnDecl { name, params, body }))
    }

    fn parse_expr_stmt(&mut self) -> Result<AstId<Stmt>, anyhow::Error> {
        let pos = self.tokens.peek(0).pos.clone();
        let expr = self.parse_expr_semi()?;
        Ok(self.ast.add_stmt(Stmt::Expression(expr), pos))
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
        let pos = self.ast.pos(&expr).clone();

        let kind = match &op {
            Operator::MemberAccess => {
                let id = self.parse_identifier()?;
                let id = self.ast.add_expr(Expr::Literal(Literal::String(id)), pos.clone());
                Expr::Index(expr, id)
            },
            Operator::Arrow => {
                let right = self.parse_block_or_expr(op.infix_precedence().unwrap())?;
                let params = expr.as_comma_separated(self.ast).iter()
                    .map(|id| match self.ast.get(id) {
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

        Ok(self.ast.add_expr(kind, pos.clone()))
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
                        expr.as_comma_separated(self.ast)
                    }
                };

                return Ok(self.ast.add_expr(Expr::Literal(Literal::Array(elements)), pos));
            },
            _ => {
                let right = self.parse_expr_precedence(op.prefix_precedence().unwrap())?;
                Expr::Unary(op, right)
            }
        };
        Ok(self.ast.add_expr(kind, pos))
    }

    fn parse_expr_postfix(&mut self, op: Operator, expr: AstId<Expr>) -> Result<AstId<Expr>, anyhow::Error> {
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
            // The super keyword is not a valid expression on its own,
            // so it makes sense to consider something like `super.x` as an "atom".
            TokenType::Super => return self.parse_super(),
            TokenType::Identifier => Expr::Identifier(self.ast.intern(&token.lexeme)),
            _ => parse_error!(self, &pos, "Unexpected token {token}")
        };

        Ok(self.ast.add_expr(kind, pos))
    }

    fn parse_super(&mut self) -> Result<AstId<Expr>, anyhow::Error> {
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

    fn parse_super_call(&mut self) -> Result<AstId<Stmt>, anyhow::Error> {
        let pos = self.tokens.expect(TokenType::Super)?.pos.clone();
        self.tokens.expect(TokenType::LeftParen)?;
        let args = self.parse_call_arguments()?;
        self.tokens.expect(TokenType::Semicolon)?;
        let super_expr = self.ast.add_expr(Expr::Super, pos.clone());
        let expr = self.ast.add_expr(Expr::Call(super_expr, args), pos.clone());
        Ok(self.ast.add_stmt(Stmt::Expression(expr), pos))
    }

    fn parse_call_arguments(&mut self) -> Result<Vec<AstId<Expr>>, anyhow::Error> {
        match self.tokens.next_if(TokenType::RightParen) {
            Some(_) => Ok(Vec::new()),
            None => {
                let args = self.parse_expr()?;
                self.tokens.expect(TokenType::RightParen)?;
                Ok(args.as_comma_separated(self.ast))
            }
        }
    }
}

//! Recursive-descent parser. Turns tokens into an AST.

mod precedence;

use anyhow::anyhow;

use crate::ast::{Ast, AstId, CatchClause, TypeDecl, Expr, FieldInit, FnDecl, Literal, Operator, Stmt, Symbol};
use crate::frontend::lex::{ContextualKeyword, SourcePosition, TokenStream, TokenType};

macro_rules! parse_error {
    ($self:ident, $pos:expr, $($arg:tt)*) => { return Err($self.error(format!($($arg)*), $pos)) };
}

/// A member's declared visibility (from a `pub`/`inner` modifier, or private by default).
#[derive(Clone, Copy, PartialEq, Eq)]
enum Visibility { Pub, Inner, Private }

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
            TokenType::Type => self.parse_type_decl(false),
            TokenType::Trait => self.parse_type_decl(true),
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

    /// Parses the composition header: an optional `with T1, T2, ...` clause then an optional
    /// `req T1, T2, ...` clause (each at most once, in that order).
    fn parse_composition_header(&mut self) -> Result<(Vec<Symbol>, Vec<Symbol>), anyhow::Error> {
        let with_traits = self.parse_trait_clause(ContextualKeyword::With)?;
        let req_traits = self.parse_trait_clause(ContextualKeyword::Req)?;

        // A header is `with ... req ...`; any further `with`/`req` here is a duplicate or misordered clause.
        let tok = self.tokens.peek(0);
        match tok.contextual() {
            Some(kw @ (ContextualKeyword::With | ContextualKeyword::Req)) =>
                parse_error!(self, &tok.pos, "Unexpected '{kw}' clause: a type/trait header allows at most one `with` clause followed by at most one `req` clause"),
            _ => Ok((with_traits, req_traits)),
        }
    }

    /// Parses a single `<keyword> T1, T2, ...` trait-list clause.
    fn parse_trait_clause(&mut self, keyword: ContextualKeyword) -> Result<Vec<Symbol>, anyhow::Error> {
        let present = self.tokens.peek(0).contextual() == Some(keyword);
        let mut traits = Vec::new();
        if present {
            self.tokens.next();
            loop {
                let name = self.parse_identifier()?;
                traits.push(self.ast.intern(&name));
                if self.tokens.next_if(TokenType::Comma).is_none() { break; }
            }
        }
        Ok(traits)
    }

    /// Reads an optional leading member-visibility modifier (`pub`/`inner`), consuming it if
    /// present; absent means private.
    fn parse_visibility(&mut self) -> Visibility {
        match self.tokens.peek(0).contextual() {
            Some(ContextualKeyword::Pub) => { self.tokens.next(); Visibility::Pub },
            Some(ContextualKeyword::Inner) => { self.tokens.next(); Visibility::Inner },
            _ => Visibility::Private,
        }
    }

    /// Parses a `req fn f(params);` method hole, returning its name and arity.
    fn parse_req_fn(&mut self) -> Result<(Symbol, usize), anyhow::Error> {
        self.tokens.expect(TokenType::Fn)?;
        let name = self.parse_identifier()?;
        let name = self.ast.intern(&name);
        self.tokens.expect(TokenType::LeftParen)?;
        let params = self.parse_params(TokenType::RightParen)?;
        self.tokens.expect(TokenType::Semicolon)?;
        Ok((name, params.len()))
    }

    fn parse_type_decl(&mut self, is_trait: bool) -> Result<AstId<Stmt>, anyhow::Error> {
        let keyword = if is_trait { TokenType::Trait } else { TokenType::Type };
        let pos = self.tokens.expect(keyword)?.pos.clone();
        let class_name = self.parse_identifier()?;
        let class_sym = self.ast.intern(&class_name);

        let prev_class = std::mem::replace(&mut self.current_class, Some(class_name.clone()));

        let (with_traits, req_traits) = self.parse_composition_header()?;

        // `: Super` inheritance is types-only (and is being removed in a later step).
        let superclass = if !is_trait {
            match self.tokens.next_if(TokenType::Colon) {
                Some(_) => {
                    let super_name = self.parse_identifier()?;
                    Some(self.ast.intern(&super_name))
                },
                None => None
            }
        } else {
            None
        };

        self.tokens.expect(TokenType::LeftBrace)?;

        let mut fields: std::collections::HashSet<Symbol> = std::collections::HashSet::default();
        let mut field_inits: Vec<(Symbol, AstId<Expr>)> = Vec::new();
        let mut method_stmts: Vec<AstId<Stmt>> = Vec::new();
        let mut pub_members: std::collections::HashSet<Symbol> = std::collections::HashSet::default();
        let mut inner_members: std::collections::HashSet<Symbol> = std::collections::HashSet::default();
        let mut req_fns: Vec<(Symbol, usize)> = Vec::new();
        let mut gives: Vec<(Symbol, Symbol)> = Vec::new();
        let mut init = None;
        let mut getter = None;
        let mut setter = None;

        while !self.tokens.match_next(TokenType::RightBrace) {
            let member_pos = self.tokens.peek(0).pos.clone();
            let visibility = self.parse_visibility();

            // `req fn f(params);`
            if self.tokens.peek(0).contextual() == Some(ContextualKeyword::Req)
                && self.tokens.peek(1).kind == TokenType::Fn
            {
                if visibility != Visibility::Private { parse_error!(self, &member_pos, "A `req fn` cannot have a visibility modifier"); }
                self.tokens.next(); // consume `req`
                req_fns.push(self.parse_req_fn()?);
                continue;
            }

            let kind = self.tokens.peek(0).kind;
            match kind {
                TokenType::Fn => {
                    let stmt = self.parse_fn()?;
                    if let Stmt::Fn(decl) = self.ast.get(&stmt) {
                        match visibility {
                            Visibility::Pub => { pub_members.insert(decl.name); },
                            Visibility::Inner => { inner_members.insert(decl.name); },
                            Visibility::Private => {},
                        }
                    }
                    method_stmts.push(stmt);
                },
                TokenType::Identifier => {
                    let name = self.parse_identifier()?;

                    // `init`/`get`/`set` are specially-named methods (normal method syntax); they
                    // take no visibility modifier.
                    match name.as_str() {
                        "init" => {
                            if visibility != Visibility::Private { parse_error!(self, &member_pos, "An initializer cannot have a visibility modifier"); }
                            init = Some(self.parse_init(superclass.is_some())?);
                        },
                        "get" => {
                            if visibility != Visibility::Private { parse_error!(self, &member_pos, "An accessor cannot have a visibility modifier"); }
                            getter = Some(self.parse_accessor(name, 1, "Getter must have exactly one parameter")?);
                        },
                        "set" => {
                            if visibility != Visibility::Private { parse_error!(self, &member_pos, "An accessor cannot have a visibility modifier"); }
                            setter = Some(self.parse_accessor(name, 2, "Setter must have exactly two parameters")?);
                        },
                        _ => {
                            // Field declaration, optionally with a `gives Trait` delegation suffix.
                            let field = self.ast.intern(&name);
                            let give = if self.tokens.peek(0).contextual() == Some(ContextualKeyword::Gives) {
                                self.tokens.next();
                                let trait_name = self.parse_identifier()?;
                                Some(self.ast.intern(&trait_name))
                            } else {
                                None
                            };

                            let value = self.tokens.next_if(TokenType::Equal)
                                .map(|_| self.parse_expr())
                                .transpose()?;

                            self.tokens.expect(TokenType::Semicolon)?;
                            fields.insert(field);
                            match visibility {
                                Visibility::Pub => { pub_members.insert(field); },
                                Visibility::Inner => { inner_members.insert(field); },
                                Visibility::Private => {},
                            }

                            if let Some(trait_sym) = give {
                                gives.push((field, trait_sym));
                            }
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
            is_trait,
            with_traits,
            req_traits,
            req_fns,
            gives,
            superclass,
            init_name,
            init,
            getter,
            setter,
            fields,
            field_inits,
            methods: method_stmts,
            pub_members,
            inner_members,
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
                Expr::Index(expr, id, true) // `.name` member access
            },
            Operator::Is => {
                // The right operand is a static type/trait name, not an expression.
                let name = self.parse_identifier()?;
                Expr::Is(expr, self.ast.intern(&name))
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
            Operator::Dict => {
                let mut pairs: Vec<(AstId<Expr>, AstId<Expr>)> = Vec::new();
                // Statically-known keys, tagged by value type so a value-equal pair
                // collides (`{ a, "a" }`, `{ 1, 1.0 }`) but a cross-type pair does not
                // (`{ 1, "1" }`). Computed `[expr]` keys are not tracked here.
                let mut seen: std::collections::HashSet<String> = std::collections::HashSet::new();
                // Parse the value tighter than `,` so the comma separates pairs.
                let value_precedence = Operator::Comma.infix_precedence().unwrap() + 1;

                if self.tokens.next_if(TokenType::RightBrace).is_none() {
                    loop {
                        let tok = self.tokens.peek(0).clone();
                        let key_pos = tok.pos.clone();

                        // Each arm yields (key expr, value expr, optional static dedup key).
                        let (key_expr, value_expr, dup_key): (AstId<Expr>, AstId<Expr>, Option<String>) = match tok.kind {
                            // Computed key: the key is the runtime VALUE of the expression.
                            TokenType::LeftBracket => {
                                self.tokens.next();
                                let key = self.parse_expr()?;
                                self.tokens.expect(TokenType::RightBracket)?;
                                self.tokens.expect(TokenType::Colon)?;
                                let value = self.parse_expr_precedence(value_precedence)?;
                                (key, value, None)
                            },
                            // Identifier: a name → string key. `:` gives an explicit value,
                            // otherwise it's shorthand `{ x }` ≡ `{ x: x }`.
                            TokenType::Identifier => {
                                self.tokens.next();
                                let name = tok.lexeme.clone();
                                let key = self.ast.add_expr(Expr::Literal(Literal::String(name.clone())), key_pos.clone());
                                let value = if self.tokens.next_if(TokenType::Colon).is_some() {
                                    self.parse_expr_precedence(value_precedence)?
                                } else {
                                    let sym = self.ast.intern(&name);
                                    self.ast.add_expr(Expr::Identifier(sym), key_pos.clone())
                                };
                                (key, value, Some(format!("s:{name}")))
                            },
                            // String literal → string key.
                            TokenType::StringLiteral => {
                                self.tokens.next();
                                let raw = tok.lexeme.clone();
                                let s = String::from(&raw[1..raw.len() - 1]); // strip quotes
                                let key = self.ast.add_expr(Expr::Literal(Literal::String(s.clone())), key_pos.clone());
                                self.tokens.expect(TokenType::Colon)?;
                                let value = self.parse_expr_precedence(value_precedence)?;
                                (key, value, Some(format!("s:{s}")))
                            },
                            // Number literal → number key (no coercion; `{ 1: v }` is read by `d[1]`).
                            TokenType::NumericLiteral => {
                                self.tokens.next();
                                let n: f64 = tok.lexeme.parse().unwrap();
                                let key = self.ast.add_expr(Expr::Literal(Literal::Number(n)), key_pos.clone());
                                self.tokens.expect(TokenType::Colon)?;
                                let value = self.parse_expr_precedence(value_precedence)?;
                                (key, value, Some(format!("n:{n}")))
                            },
                            // Keyword literals → their bool/null value key (quote for the string).
                            TokenType::True | TokenType::False => {
                                self.tokens.next();
                                let b = matches!(tok.kind, TokenType::True);
                                let key = self.ast.add_expr(Expr::Literal(Literal::Boolean(b)), key_pos.clone());
                                self.tokens.expect(TokenType::Colon)?;
                                let value = self.parse_expr_precedence(value_precedence)?;
                                (key, value, Some(format!("b:{b}")))
                            },
                            TokenType::Null => {
                                self.tokens.next();
                                let key = self.ast.add_expr(Expr::Literal(Literal::Null), key_pos.clone());
                                self.tokens.expect(TokenType::Colon)?;
                                let value = self.parse_expr_precedence(value_precedence)?;
                                (key, value, Some(String::from("null")))
                            },
                            _ => parse_error!(self, &key_pos, "Unexpected token {}: expected a dict key", tok)
                        };

                        if let Some(dup) = dup_key {
                            if !seen.insert(dup) {
                                parse_error!(self, &key_pos, "Duplicate dict key '{}'", tok.lexeme);
                            }
                        }
                        pairs.push((key_expr, value_expr));

                        if self.tokens.next_if(TokenType::Comma).is_some() {
                            if self.tokens.match_next(TokenType::RightBrace) { break; } // trailing comma
                        } else {
                            break;
                        }
                    }
                    self.tokens.expect(TokenType::RightBrace)?;
                }

                return Ok(self.ast.add_expr(Expr::Literal(Literal::Dict(pairs)), pos));
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
                Ok(self.ast.add_expr(Expr::Index(expr, index, false), pos)) // `[expr]` data access
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
                let expr = self.ast.add_expr(Expr::Index(super_expr, id, true), pos.clone()); // `super.name`
                Ok(expr)
            },
            TokenType::LeftBracket => {
                let super_expr = self.ast.add_expr(Expr::Super, pos.clone());
                let expr = self.parse_expr()?;
                self.tokens.expect(TokenType::RightBracket)?;
                let expr = self.ast.add_expr(Expr::Index(super_expr, expr, false), pos.clone()); // `super[expr]`
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

//! Recursive-descent parser. Turns tokens into an AST.

use std::collections::HashSet;

use anyhow::anyhow;

use crate::ast::{Arm, Ast, AstId, CatchClause, TypeDecl, Expr, FieldInit, FnDecl, Literal, MatchElem, MatchField, MatchScalar, Matcher, Operator, Param, ReturnShape, Stmt, Symbol};
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
    current_type: Option<String>,
    /// True while parsing a condition (`if`/`while`), where a trailing `{` is the body
    /// block, not a brace construction.
    prevent_construct: bool,
    /// True while parsing a match-arm guard, where a `=>` delimits the body rather than
    /// opening a lambda. Reset inside a sub-expression by `parse_expr`.
    stop_at_arrow: bool,
}

impl<'parser, 'vm> Parser<'parser, 'vm> {
    pub fn parse(tokens: &'vm mut TokenStream<'vm>) -> Result<Ast, anyhow::Error> {
        let mut ast = Ast::new();

        let mut parser = Parser {
            tokens,
            ast: &mut ast,
            current_type: None,
            prevent_construct: false,
            stop_at_arrow: false,
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

    /// Consumes a leading `mut` modifier if present, reporting whether it was there.
    fn parse_mut(&mut self) -> bool {
        if self.tokens.peek(0).contextual() == Some(ContextualKeyword::Mut) {
            self.tokens.next();
            return true;
        }
        false
    }

    /// Consumes an optional trailing `?` nullability marker.
    fn parse_nullable(&mut self) -> bool {
        self.tokens.next_if(TokenType::Question).is_some()
    }

    /// Parses a return shape marker after a parameter list: `!` non-null, `?` nullable,
    /// or nothing for void.
    fn parse_return_shape(&mut self) -> ReturnShape {
        if self.tokens.next_if(TokenType::Exclamation).is_some() {
            ReturnShape::NonNull
        } else if self.tokens.next_if(TokenType::Question).is_some() {
            ReturnShape::Nullable
        } else {
            ReturnShape::Void
        }
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
            TokenType::Match => self.parse_match(),
            TokenType::LeftBrace => self.parse_block_stmt(),
            _ => self.parse_expr_stmt()
        }
    }

    fn parse_if_stmt(&mut self) -> Result<AstId<Stmt>, anyhow::Error> {
        let pos = self.tokens.expect(TokenType::If)?.pos.clone();
        let condition = self.parse_expr_prevent_construct()?;
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
        let mutable = self.parse_mut();
        let name = self.parse_identifier()?;
        let name = self.ast.intern(&name);
        let nullable = self.parse_nullable();

        let expr = if let Some(_) = self.tokens.next_if(TokenType::Equal) {
            Some(self.parse_expr()?)
        } else {
            None
        };

        self.tokens.expect(TokenType::Semicolon)?;
        let field_init = FieldInit { name, value: expr, nullable, mutable };
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
        let ret = self.parse_return_shape();
        let body = self.parse_block()?;
        Ok(FnDecl {
            name,
            params,
            body,
            ret,
        })
    }

    fn init_name(&mut self) -> Symbol {
        let ty = self.current_type.clone().expect("init parsed outside a type");
        self.ast.intern(&format!("{}.init", ty))
    }

    fn parse_init(&mut self) -> Result<AstId<Stmt>, anyhow::Error> {
        let pos = self.tokens.peek(0).pos.clone();
        let name = self.init_name();
        self.tokens.expect(TokenType::LeftParen)?;
        let params = self.parse_params(TokenType::RightParen)?;
        self.tokens.expect(TokenType::LeftBrace)?;

        let stmts = self.parse_stmts()?;
        self.tokens.expect(TokenType::RightBrace)?;

        let body = self.ast.add_expr(Expr::Block(stmts), pos.clone());
        // An `init` takes no return marker. It produces no value.
        let fn_decl = FnDecl { name, params, body, ret: ReturnShape::Void };
        Ok(self.ast.add_stmt(Stmt::Fn(fn_decl), pos))
    }

    /// Parses a parameter list up to `end_token`. Each parameter is `[mut] name [?]`.
    fn parse_params(&mut self, end_token: TokenType) -> Result<Vec<Param>, anyhow::Error> {
        let params = match self.tokens.next_if(end_token) {
            Some(_) => Vec::new(),
            None => {
                let mut params = Vec::new();
                while !self.tokens.matches(end_token) {
                    if params.len() > 0 {
                        self.tokens.expect(TokenType::Comma)?;
                    }
                    let mutable = self.parse_mut();
                    let name = self.parse_identifier_expr()?;
                    let nullable = self.parse_nullable();
                    params.push(Param { name, nullable, mutable });
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

    /// Parses a `req fn f(params)<marker>;` method hole, returning its name, arity, and
    /// declared return shape.
    fn parse_req_fn(&mut self) -> Result<(Symbol, usize, ReturnShape), anyhow::Error> {
        self.tokens.expect(TokenType::Fn)?;
        let name = self.parse_identifier()?;
        let name = self.ast.intern(&name);
        self.tokens.expect(TokenType::LeftParen)?;
        let params = self.parse_params(TokenType::RightParen)?;
        let ret = self.parse_return_shape();
        self.tokens.expect(TokenType::Semicolon)?;
        Ok((name, params.len(), ret))
    }

    fn parse_type_decl(&mut self, is_trait: bool) -> Result<AstId<Stmt>, anyhow::Error> {
        let keyword = if is_trait { TokenType::Trait } else { TokenType::Type };
        let pos = self.tokens.expect(keyword)?.pos.clone();
        let type_name = self.parse_identifier()?;
        let type_sym = self.ast.intern(&type_name);

        let prev_type = std::mem::replace(&mut self.current_type, Some(type_name.clone()));

        let (with_traits, req_traits) = self.parse_composition_header()?;

        self.tokens.expect(TokenType::LeftBrace)?;

        let mut fields: HashSet<Symbol> = HashSet::default();
        let mut nullable_fields: HashSet<Symbol> = HashSet::default();
        let mut mut_fields: HashSet<Symbol> = HashSet::default();
        let mut field_inits: Vec<(Symbol, AstId<Expr>)> = Vec::new();
        let mut method_stmts: Vec<AstId<Stmt>> = Vec::new();
        let mut pub_members: HashSet<Symbol> = HashSet::default();
        let mut inner_members: HashSet<Symbol> = HashSet::default();
        let mut req_fns: Vec<(Symbol, usize, ReturnShape)> = Vec::new();
        let mut req_members: Vec<Symbol> = Vec::new();
        let mut gives: Vec<(Symbol, Symbol)> = Vec::new();
        let mut init = None;

        while !self.tokens.matches(TokenType::RightBrace) {
            let member_pos = self.tokens.peek(0).pos.clone();
            let visibility = self.parse_visibility();

            // `req fn f(params);` (method hole) or `req name;` (member/state hole)
            if self.tokens.peek(0).contextual() == Some(ContextualKeyword::Req) {
                if visibility != Visibility::Private { parse_error!(self, &member_pos, "A `req` declaration cannot have a visibility modifier"); }
                self.tokens.next(); // consume `req`
                if self.tokens.matches(TokenType::Fn) {
                    req_fns.push(self.parse_req_fn()?);
                } else {
                    let name = self.parse_identifier()?;
                    self.tokens.expect(TokenType::Semicolon)?;
                    req_members.push(self.ast.intern(&name));
                }
                continue;
            }

            // `mut` is a field modifier. Methods and `init` reject it.
            let mutable = self.parse_mut();

            let kind = self.tokens.peek(0).kind;
            match kind {
                TokenType::Fn => {
                    if mutable { parse_error!(self, &member_pos, "Only fields can be `mut`"); }
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

                    // `init` is a specially-named method (normal method syntax); it
                    // takes no visibility modifier.
                    match name.as_str() {
                        "init" => {
                            if is_trait { parse_error!(self, &member_pos, "A trait cannot declare an `init`; put initialization on the host type"); }
                            if visibility != Visibility::Private { parse_error!(self, &member_pos, "An initializer cannot have a visibility modifier"); }
                            if mutable { parse_error!(self, &member_pos, "Only fields can be `mut`"); }
                            init = Some(self.parse_init()?);
                        },
                        _ => {
                            // Field declaration, optionally with a `gives Trait` delegation suffix.
                            if is_trait { parse_error!(self, &member_pos, "A trait cannot declare fields; `req` the state it needs and let the host type hold it"); }
                            let field = self.ast.intern(&name);
                            let nullable = self.parse_nullable();
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
                            if nullable { nullable_fields.insert(field); }
                            if mutable { mut_fields.insert(field); }
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

        let init_name = self.ast.intern(&format!("{}.init", type_name));

        let type_decl = Box::new(TypeDecl {
            name: type_sym,
            is_trait,
            with_traits,
            req_traits,
            req_fns,
            req_members,
            gives,
            init_name,
            init,
            fields,
            nullable_fields,
            mut_fields,
            field_inits,
            methods: method_stmts,
            pub_members,
            inner_members,
        });

        self.current_type = prev_type;
        Ok(self.ast.add_stmt(Stmt::Type(type_decl), pos))
    }

    fn parse_while(&mut self) -> Result<AstId<Stmt>, anyhow::Error> {
        let pos = self.tokens.expect(TokenType::While)?.pos.clone();
        let condition = self.parse_expr_prevent_construct()?;
        let body = self.parse_block_or_stmt()?;
        Ok(self.ast.add_stmt(Stmt::While(condition, body), pos))
    }

    /// match_stmt := "match" expr "{" arm ("," arm)* ","? "}"
    fn parse_match(&mut self) -> Result<AstId<Stmt>, anyhow::Error> {
        let pos = self.tokens.expect(TokenType::Match)?.pos.clone();
        let scrutinee = self.parse_expr_prevent_construct()?;
        self.tokens.expect(TokenType::LeftBrace)?;

        let mut arms: Vec<Arm> = Vec::new();
        while !self.tokens.matches(TokenType::RightBrace) {
            arms.push(self.parse_match_arm()?);
            if self.tokens.next_if(TokenType::Comma).is_none() {
                break;
            }
        }

        if arms.is_empty() {
            parse_error!(self, &pos, "A `match` needs at least one arm");
        }

        self.tokens.expect(TokenType::RightBrace)?;
        Ok(self.ast.add_stmt(Stmt::Match(scrutinee, arms), pos))
    }

    /// arm := matcher ("if" guard)? "=>" body
    fn parse_match_arm(&mut self) -> Result<Arm, anyhow::Error> {
        let matcher = self.parse_matcher()?;
        let guard = match self.tokens.next_if(TokenType::If) {
            Some(_) => Some(self.parse_guard()?),
            None => None,
        };
        self.tokens.expect(TokenType::FatArrow)?;
        let body = self.parse_arm_body()?;
        Ok(Arm { matcher, guard, body })
    }

    /// An arm body: a `{ ... }` block, or a single expression run for effect.
    fn parse_arm_body(&mut self) -> Result<AstId<Expr>, anyhow::Error> {
        if self.tokens.matches(TokenType::LeftBrace) {
            return self.parse_block();
        }
        let pos = self.tokens.peek(0).pos.clone();
        let prev = std::mem::replace(&mut self.prevent_construct, false);
        let prev_arrow = std::mem::replace(&mut self.stop_at_arrow, false);
        let body_precedence = Operator::Comma.infix_precedence().unwrap() + 1;
        let result = self.parse_expr_precedence(body_precedence);
        self.prevent_construct = prev;
        self.stop_at_arrow = prev_arrow;
        let stmt = self.ast.add_stmt(Stmt::Expression(result?), pos.clone());
        Ok(self.ast.add_expr(Expr::Block(vec![stmt]), pos))
    }

    /// Parses a guard expression, stopping before the arm's `=>`.
    fn parse_guard(&mut self) -> Result<AstId<Expr>, anyhow::Error> {
        let prev = std::mem::replace(&mut self.stop_at_arrow, true);
        let result = self.parse_expr_precedence(0);
        self.stop_at_arrow = prev;
        result
    }

    fn parse_return(&mut self) -> Result<AstId<Stmt>, anyhow::Error> {
        let pos = self.tokens.expect(TokenType::Return)?.pos.clone();
        let expr = match self.tokens.matches(TokenType::Semicolon) {
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
            let (param, mutable) = match self.tokens.peek(0).kind {
                TokenType::Identifier => (Some(self.parse_identifier_expr()?), false),
                TokenType::LeftParen => {
                    self.tokens.expect(TokenType::LeftParen)?;
                    let params = self.parse_params(TokenType::RightParen)?;
                    if params.len() != 1 {
                        parse_error!(self, &catch_pos, "Expected one parameter in catch block")
                    }
                    (Some(params[0].name), params[0].mutable)
                },
                _ => (None, false)
            };
            let body = self.parse_block_or_stmt()?;
            Some(CatchClause { param, mutable, body })
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
        if self.tokens.matches(TokenType::LeftBrace) {
            self.parse_block()
        } else {
            let pos = self.tokens.peek(0).pos.clone();
            let stmt = self.parse_stmt()?;
            Ok(self.ast.add_expr(Expr::Block(vec![stmt]), pos))
        }
    }

    fn parse_block_or_expr(&mut self, prec: u8) -> Result<AstId<Expr>, anyhow::Error> {
        if self.tokens.matches(TokenType::LeftBrace) {
            self.parse_block()
        } else {
            self.parse_expr_precedence(prec)
        }
    }

    /// Parses statements up to (but not consuming) the closing `}`.
    fn parse_stmts(&mut self) -> Result<Vec<AstId<Stmt>>, anyhow::Error> {
        let mut stmts: Vec<AstId<Stmt>> = Vec::new();
        while !self.tokens.matches(TokenType::RightBrace) {
            stmts.push(self.parse_stmt()?);
        }
        Ok(stmts)
    }

    fn make_lambda(&mut self, params: Vec<AstId<Expr>>, body: AstId<Expr>) -> Expr {
        let name = self.ast.intern("lambda");
        // Lambda parameters take no markers. The return shape is inferred from the body.
        let params = params.into_iter()
            .map(|name| Param { name, nullable: false, mutable: false })
            .collect();
        Expr::Literal(Literal::Lambda(FnDecl { name, params, body, ret: ReturnShape::Inferred }))
    }

    fn parse_expr_stmt(&mut self) -> Result<AstId<Stmt>, anyhow::Error> {
        let pos = self.tokens.peek(0).pos.clone();
        let expr = self.parse_expr_semi()?;
        Ok(self.ast.add_stmt(Stmt::Expression(expr), pos))
    }

    fn parse_expr(&mut self) -> Result<AstId<Expr>, anyhow::Error> {
        let prev = std::mem::replace(&mut self.prevent_construct, false);
        let prev_arrow = std::mem::replace(&mut self.stop_at_arrow, false);
        let result = self.parse_expr_precedence(0);
        self.prevent_construct = prev;
        self.stop_at_arrow = prev_arrow;
        result
    }

    /// Parses an expression with a trailing `{`. Prevent construction to disambiguate.
    fn parse_expr_prevent_construct(&mut self) -> Result<AstId<Expr>, anyhow::Error> {
        let prev = std::mem::replace(&mut self.prevent_construct, true);
        let result = self.parse_expr_precedence(0);
        self.prevent_construct = prev;
        result
    }

    /// Whether `expr` can be brace-constructed: a bare type name `C` or a call `C(args)`.
    fn is_constructible(&self, expr: AstId<Expr>) -> bool {
        match self.ast.get(&expr) {
            Expr::Identifier(_) => true,
            Expr::Call(callee, _) => matches!(self.ast.get(callee), Expr::Identifier(_)),
            _ => false,
        }
    }

    /// Parses `{ field: value, ... }` after a constructible callee into an `Expr::Construct`.
    fn parse_construction(&mut self, callee: AstId<Expr>) -> Result<AstId<Expr>, anyhow::Error> {
        let pos = self.ast.pos(&callee).clone();
        self.tokens.expect(TokenType::LeftBrace)?;
        let prev = std::mem::replace(&mut self.prevent_construct, false);

        // Parse each value tighter than `,` so the comma separates fields, not the value expression.
        let value_precedence = Operator::Comma.infix_precedence().unwrap() + 1;
        let mut fields: Vec<(Symbol, AstId<Expr>)> = Vec::new();
        while !self.tokens.matches(TokenType::RightBrace) {
            let key_pos = self.tokens.peek(0).pos.clone();
            let name = self.parse_identifier()?;
            let field = self.ast.intern(&name);
            let value = if self.tokens.next_if(TokenType::Colon).is_some() {
                self.parse_expr_precedence(value_precedence)?
            } else {
                self.ast.add_expr(Expr::Identifier(field), key_pos) // shorthand `{ x }` == `{ x: x }`
            };
            fields.push((field, value));
            if self.tokens.next_if(TokenType::Comma).is_none() { break; }
        }

        self.prevent_construct = prev;
        self.tokens.expect(TokenType::RightBrace)?;
        Ok(self.ast.add_expr(Expr::Construct(callee, fields), pos))
    }

    fn parse_expr_precedence(&mut self, min_precedence: u8) -> Result<AstId<Expr>, anyhow::Error> {
        let mut left = match Operator::parse_prefix(self.tokens, 0) {
            Some(op) => self.parse_expr_prefix(op)?,
            _ => self.parse_expr_atom()?
        };

        loop {
            // In a match arm guard, a `=>` delimits the body. Stop before it so it is not read as a lambda.
            if self.stop_at_arrow && self.tokens.matches(TokenType::FatArrow) {
                break;
            }
            // A `{` after a type name or call is a brace construction, unless we are parsing a
            // condition where the `{` opens the body block.
            if !self.prevent_construct
                && self.tokens.matches(TokenType::LeftBrace)
                && self.is_constructible(left)
            {
                left = self.parse_construction(left)?;
            } else if let Some(op) = Operator::parse_postfix(self.tokens, min_precedence) {
                left = self.parse_expr_postfix(op, left)?;
            } else if let Some(op) = Operator::parse_infix(self.tokens, min_precedence) {
                left = self.parse_expr_infix(op, left)?;
            } else {
                break;
            }
        }

        Ok(left)
    }

    /// Parses a primary expression: a literal, an array or dict literal, or an identifier.
    fn parse_primary(&mut self) -> Result<AstId<Expr>, anyhow::Error> {
        match Operator::parse_prefix(self.tokens, 0) {
            Some(op) => self.parse_expr_prefix(op),
            _ => self.parse_expr_atom(),
        }
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
            Operator::Has => {
                // The right operand is a static spec read as a primary. Later passes validate it.
                let right = self.parse_primary()?;
                Expr::Binary(op, expr, right)
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
                let mut seen: HashSet<String> = HashSet::new();
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
                            // A reserved word is a plain string key. It needs an explicit value;
                            // a keyword cannot stand in as the shorthand `{ x }` value.
                            _ if tok.name_word().is_some() => {
                                self.tokens.next();
                                let name = tok.lexeme.clone();
                                let key = self.ast.add_expr(Expr::Literal(Literal::String(name.clone())), key_pos.clone());
                                self.tokens.expect(TokenType::Colon)?;
                                let value = self.parse_expr_precedence(value_precedence)?;
                                (key, value, Some(format!("s:{name}")))
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
                            if self.tokens.matches(TokenType::RightBrace) { break; } // trailing comma
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
            Operator::SafeMemberAccess => {
                let id = self.parse_identifier()?;
                let id = self.ast.add_expr(Expr::Literal(Literal::String(id)), pos.clone());
                Ok(self.ast.add_expr(Expr::SafeAccess(expr, id, true), pos)) // `?.name`
            },
            Operator::SafeIndex => {
                let index = self.parse_expr()?;
                self.tokens.expect(TokenType::RightBracket)?;
                Ok(self.ast.add_expr(Expr::SafeAccess(expr, index, false), pos)) // `?[expr]`
            },
            Operator::Assert => Ok(self.ast.add_expr(Expr::Assert(expr), pos)),
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
            TokenType::Identifier => Expr::Identifier(self.ast.intern(&token.lexeme)),
            _ => parse_error!(self, &pos, "Unexpected token {token}")
        };

        Ok(self.ast.add_expr(kind, pos))
    }

    /// Parses one matcher into a fresh `Ast` and returns its handle. Used by tests.
    pub fn parse_matcher_root(tokens: &'vm mut TokenStream<'vm>) -> Result<(Ast, AstId<Matcher>), anyhow::Error> {
        let mut ast = Ast::new();
        let matcher = {
            let mut parser = Parser { tokens, ast: &mut ast, current_type: None, prevent_construct: false, stop_at_arrow: false };
            let matcher = parser.parse_matcher()?;
            parser.tokens.expect(TokenType::EOF)?;
            matcher
        };
        Ok((ast, matcher))
    }

    /// matcher := IDENT "@" matcher | or_matcher
    fn parse_matcher(&mut self) -> Result<AstId<Matcher>, anyhow::Error> {
        // An as-binding is a bare name immediately followed by `@`. The right side is a full matcher.
        if self.tokens.peek(0).kind == TokenType::Identifier && self.tokens.peek(1).kind == TokenType::At {
            let token = self.tokens.next().clone();
            let pos = token.pos.clone();
            let name = self.ast.intern(&token.lexeme);
            self.tokens.next();
            let inner = self.parse_matcher()?;
            return Ok(self.ast.add_matcher(Matcher::As(name, inner), pos));
        }
        self.parse_or_matcher()
    }

    /// or_matcher := and_matcher ("|" and_matcher)*
    fn parse_or_matcher(&mut self) -> Result<AstId<Matcher>, anyhow::Error> {
        let pos = self.tokens.peek(0).pos.clone();
        let first = self.parse_and_matcher()?;
        if !self.tokens.matches_single(TokenType::Pipe) {
            return Ok(first);
        }
        let mut alternatives = vec![first];
        while self.tokens.next_if_single(TokenType::Pipe).is_some() {
            alternatives.push(self.parse_and_matcher()?);
        }
        Ok(self.ast.add_matcher(Matcher::Or(alternatives), pos))
    }

    /// and_matcher := primary ("&" primary)*
    fn parse_and_matcher(&mut self) -> Result<AstId<Matcher>, anyhow::Error> {
        let pos = self.tokens.peek(0).pos.clone();
        let first = self.parse_primary_matcher()?;
        if !self.tokens.matches_single(TokenType::Amp) {
            return Ok(first);
        }
        let mut parts = vec![first];
        while self.tokens.next_if_single(TokenType::Amp).is_some() {
            parts.push(self.parse_primary_matcher()?);
        }
        Ok(self.ast.add_matcher(Matcher::And(parts), pos))
    }

    fn parse_primary_matcher(&mut self) -> Result<AstId<Matcher>, anyhow::Error> {
        let token = self.tokens.peek(0).clone();
        let pos = token.pos.clone();
        match token.kind {
            TokenType::Identifier if token.lexeme == "_" => {
                self.tokens.next();
                Ok(self.ast.add_matcher(Matcher::Wildcard, pos))
            },
            TokenType::Identifier => {
                self.tokens.next();
                let name = self.ast.intern(&token.lexeme);
                Ok(self.ast.add_matcher(Matcher::Binder(name), pos))
            },
            TokenType::Is => self.parse_is_matcher(),
            TokenType::Has => self.parse_has_matcher(),
            TokenType::LeftBrace => self.parse_shape_matcher(),
            TokenType::LeftBracket => self.parse_array_matcher(),
            TokenType::LeftParen => {
                self.tokens.next();
                let inner = self.parse_matcher()?;
                self.tokens.expect(TokenType::RightParen)?;
                Ok(inner)
            },
            TokenType::NumericLiteral | TokenType::StringLiteral
            | TokenType::True | TokenType::False | TokenType::Null => {
                let scalar = self.parse_match_scalar()?;
                Ok(self.ast.add_matcher(Matcher::Literal(scalar), pos))
            },
            _ => parse_error!(self, &pos, "Expected a matcher but found '{token}'"),
        }
    }

    /// `is` type_ref shape?.
    fn parse_is_matcher(&mut self) -> Result<AstId<Matcher>, anyhow::Error> {
        let pos = self.tokens.next().pos.clone();
        if self.tokens.peek(0).kind != TokenType::Identifier {
            parse_error!(self, &pos, "`is` is nominal and needs a type name; use `{{ ... }}` for a structural shape");
        }
        self.parse_typed_matcher(true, pos)
    }

    /// `has` type_ref shape? or `has` shape.
    fn parse_has_matcher(&mut self) -> Result<AstId<Matcher>, anyhow::Error> {
        let pos = self.tokens.next().pos.clone();
        if self.tokens.peek(0).kind == TokenType::LeftBrace {
            return self.parse_shape_matcher();
        }
        if self.tokens.peek(0).kind != TokenType::Identifier {
            parse_error!(self, &pos, "`has` needs a type name or a shape");
        }
        self.parse_typed_matcher(false, pos)
    }

    /// A type test `T shape?` after its `is`/`has` keyword.
    fn parse_typed_matcher(&mut self, nominal: bool, pos: SourcePosition) -> Result<AstId<Matcher>, anyhow::Error> {
        let name = self.parse_identifier()?;
        let name = self.ast.intern(&name);
        let shape = if self.tokens.peek(0).kind == TokenType::LeftBrace {
            Some(self.parse_shape_matcher()?)
        } else {
            None
        };
        Ok(self.ast.add_matcher(Matcher::Type { nominal, name, shape }, pos))
    }

    /// shape := "{" ( field ("," field)* ","? )? "}"
    fn parse_shape_matcher(&mut self) -> Result<AstId<Matcher>, anyhow::Error> {
        let pos = self.tokens.expect(TokenType::LeftBrace)?.pos.clone();
        let mut fields: Vec<MatchField> = Vec::new();
        let mut seen: HashSet<String> = HashSet::new();

        while !self.tokens.matches(TokenType::RightBrace) {
            let field = self.parse_shape_field()?;
            let dedup = match &field.key {
                MatchScalar::String(s) => format!("s:{s}"),
                MatchScalar::Number(n) => format!("n:{n}"),
                MatchScalar::Boolean(b) => format!("b:{b}"),
                MatchScalar::Null => String::from("null"),
            };
            if !seen.insert(dedup) {
                parse_error!(self, &pos, "Duplicate key in a shape matcher");
            }
            fields.push(field);
            if self.tokens.next_if(TokenType::Comma).is_none() {
                break;
            }
        }
        
        self.tokens.expect(TokenType::RightBrace)?;
        Ok(self.ast.add_matcher(Matcher::Shape(fields), pos))
    }

    /// field := IDENT ":" matcher | scalar ":" matcher | IDENT (shorthand `{ x }` binds x)
    fn parse_shape_field(&mut self) -> Result<MatchField, anyhow::Error> {
        let token = self.tokens.peek(0).clone();
        let pos = token.pos.clone();
        match token.kind {
            TokenType::Identifier => {
                self.tokens.next();
                let key = MatchScalar::String(token.lexeme.clone());

                if self.tokens.next_if(TokenType::Colon).is_some() {
                    let value = self.parse_matcher()?;
                    return Ok(MatchField { key, value });
                }

                // `{ x }` binds x.
                let sym = self.ast.intern(&token.lexeme);
                let value = self.ast.add_matcher(Matcher::Binder(sym), pos);
                Ok(MatchField { key, value })
            },
            TokenType::StringLiteral | TokenType::NumericLiteral
            | TokenType::True | TokenType::False | TokenType::Null => {
                let key = self.parse_match_scalar()?;
                self.tokens.expect(TokenType::Colon)?;
                let value = self.parse_matcher()?;
                Ok(MatchField { key, value })
            },
            _ => parse_error!(self, &pos, "Expected a shape field"),
        }
    }

    /// array := "[" ( elem ("," elem)* ","? )? "]"
    /// elem := matcher | ".." IDENT?
    fn parse_array_matcher(&mut self) -> Result<AstId<Matcher>, anyhow::Error> {
        let pos = self.tokens.expect(TokenType::LeftBracket)?.pos.clone();
        let mut elements: Vec<MatchElem> = Vec::new();
        let mut seen_rest = false;

        while !self.tokens.matches(TokenType::RightBracket) {
            if self.tokens.peek(0).kind == TokenType::DotDot {
                let rest_pos = self.tokens.next().pos.clone();
                if seen_rest {
                    parse_error!(self, &rest_pos, "An array matcher allows at most one rest `..`");
                }
                seen_rest = true;
                let name = match self.tokens.next_if(TokenType::Identifier) {
                    Some(token) => Some(self.ast.intern(&token.lexeme)),
                    None => None
                };
                elements.push(MatchElem::Rest(name));
            } else {
                let matcher = self.parse_matcher()?;
                elements.push(MatchElem::Elem(matcher));
            }
            if self.tokens.next_if(TokenType::Comma).is_none() {
                break;
            }
        }

        self.tokens.expect(TokenType::RightBracket)?;
        Ok(self.ast.add_matcher(Matcher::Array(elements), pos))
    }

    /// A scalar literal: a matcher equality value or a shape key.
    fn parse_match_scalar(&mut self) -> Result<MatchScalar, anyhow::Error> {
        let token = self.tokens.next().clone();
        let pos = token.pos.clone();
        Ok(match token.kind {
            TokenType::NumericLiteral => MatchScalar::Number(token.lexeme.parse().unwrap()),
            TokenType::StringLiteral => MatchScalar::String(String::from(&token.lexeme[1..token.lexeme.len() - 1])),
            TokenType::True => MatchScalar::Boolean(true),
            TokenType::False => MatchScalar::Boolean(false),
            TokenType::Null => MatchScalar::Null,
            _ => parse_error!(self, &pos, "Expected a literal"),
        })
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

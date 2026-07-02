//! Statement parsing: control flow, bindings, blocks.

use super::*;

impl<'parser, 'vm> Parser<'parser, 'vm> {
    pub(super) fn parse_stmt(&mut self) -> Result<AstId<Stmt>, anyhow::Error> {
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

    pub(super) fn parse_if_stmt(&mut self) -> Result<AstId<Stmt>, anyhow::Error> {
        let pos = self.tokens.expect(TokenType::If)?.pos.clone();
        let condition = self.parse_condition()?;
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

    pub(super) fn parse_block_stmt(&mut self) -> Result<AstId<Stmt>, anyhow::Error> {
        let pos = self.tokens.peek(0).pos.clone();
        let body = self.parse_block_or_stmt()?;
        Ok(self.ast.add_stmt(Stmt::Block(body), pos))
    }

    pub(super) fn parse_say(&mut self) -> Result<AstId<Stmt>, anyhow::Error> {
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

    pub(super) fn parse_while(&mut self) -> Result<AstId<Stmt>, anyhow::Error> {
        let pos = self.tokens.expect(TokenType::While)?.pos.clone();
        let condition = self.parse_condition()?;
        let body = self.parse_block_or_stmt()?;
        Ok(self.ast.add_stmt(Stmt::While(condition, body), pos))
    }

    pub(super) fn parse_return(&mut self) -> Result<AstId<Stmt>, anyhow::Error> {
        let pos = self.tokens.expect(TokenType::Return)?.pos.clone();
        let expr = match self.tokens.matches(TokenType::Semicolon) {
            true => None,
            false => Some(self.parse_expr()?)
        };
        self.tokens.expect(TokenType::Semicolon)?;
        Ok(self.ast.add_stmt(Stmt::Return(expr), pos))
    }

    pub(super) fn parse_throw(&mut self) -> Result<AstId<Stmt>, anyhow::Error> {
        let pos = self.tokens.expect(TokenType::Throw)?.pos.clone();
        let expr = self.parse_expr_semi()?;
        Ok(self.ast.add_stmt(Stmt::Throw(expr), pos))
    }

    /// Parses an expression terminated by a required semicolon.
    pub(super) fn parse_expr_semi(&mut self) -> Result<AstId<Expr>, anyhow::Error> {
        let expr = self.parse_expr()?;
        self.tokens.expect(TokenType::Semicolon)?;
        Ok(expr)
    }

    pub(super) fn parse_trycatch(&mut self) -> Result<AstId<Stmt>, anyhow::Error> {
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

    pub(super) fn parse_block(&mut self) -> Result<AstId<Expr>, anyhow::Error> {
        let pos = self.tokens.peek(0).pos.clone();
        self.tokens.expect(TokenType::LeftBrace)?;
        let stmts = self.parse_stmts()?;
        self.tokens.expect(TokenType::RightBrace)?;
        Ok(self.ast.add_expr(Expr::Block(stmts), pos))
    }

    pub(super) fn parse_block_or_stmt(&mut self) -> Result<AstId<Expr>, anyhow::Error> {
        if self.tokens.matches(TokenType::LeftBrace) {
            self.parse_block()
        } else {
            let pos = self.tokens.peek(0).pos.clone();
            let stmt = self.parse_stmt()?;
            Ok(self.ast.add_expr(Expr::Block(vec![stmt]), pos))
        }
    }

    pub(super) fn parse_block_or_expr(&mut self, prec: u8) -> Result<AstId<Expr>, anyhow::Error> {
        if self.tokens.matches(TokenType::LeftBrace) {
            self.parse_block()
        } else {
            self.parse_expr_precedence(prec)
        }
    }

    /// Parses statements up to (but not consuming) the closing `}`.
    pub(super) fn parse_stmts(&mut self) -> Result<Vec<AstId<Stmt>>, anyhow::Error> {
        let mut stmts: Vec<AstId<Stmt>> = Vec::new();
        while !self.tokens.matches(TokenType::RightBrace) {
            stmts.push(self.parse_stmt()?);
        }
        Ok(stmts)
    }

    pub(super) fn parse_expr_stmt(&mut self) -> Result<AstId<Stmt>, anyhow::Error> {
        let pos = self.tokens.peek(0).pos.clone();
        let expr = self.parse_expr_semi()?;
        Ok(self.ast.add_stmt(Stmt::Expression(expr), pos))
    }
}

//! Statement parsing.

use super::*;

impl<'parser, 'vm> Parser<'parser, 'vm> {
    pub(super) fn parse_stmt(&mut self) -> Result<AstId<Stmt>, anyhow::Error> {
        match self.tokens.peek(0).kind {
            TokenType::Say => self.parse_say(),
            TokenType::While => self.parse_while(),
            TokenType::Fn => self.parse_fn(),
            TokenType::Type => self.parse_type_decl(false),
            TokenType::Trait => self.parse_type_decl(true),
            TokenType::Obligation => self.parse_obligation(),
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
        Ok(self.node_stmt(Stmt::If(condition, then, otherwise), pos))
    }

    pub(super) fn parse_block_stmt(&mut self) -> Result<AstId<Stmt>, anyhow::Error> {
        let pos = self.tokens.peek(0).pos.clone();
        let body = self.parse_block_or_stmt()?;
        Ok(self.node_stmt(Stmt::Block(body), pos))
    }

    pub(super) fn parse_say(&mut self) -> Result<AstId<Stmt>, anyhow::Error> {
        let pos = self.tokens.expect(TokenType::Say)?.pos.clone();
        let mutable = self.parse_mut();
        let name = self.parse_identifier()?;
        let name = self.ast.intern(&name);
        let nullable = self.parse_nullable();
        let clause = self.parse_slot_clause(SlotKind::Local)?;

        let expr = if let Some(_) = self.tokens.next_if(TokenType::Equal) {
            Some(self.parse_expr()?)
        } else {
            None
        };

        self.tokens.expect(TokenType::Semicolon)?;
        let field_init = FieldInit { name, value: expr, nullable, mutable, clause };
        Ok(self.node_stmt(Stmt::Say(field_init), pos))
    }

    pub(super) fn parse_obligation(&mut self) -> Result<AstId<Stmt>, anyhow::Error> {
        let pos = self.tokens.expect(TokenType::Obligation)?.pos.clone();
        let name = self.parse_identifier()?;
        let name = self.ast.intern(&name);

        let (rule, witness) = self.parse_obligation_rule()?;
        self.tokens.expect(TokenType::Semicolon)?;
        Ok(self.node_stmt(Stmt::Obligation { name, witness, rule }, pos))
    }

    fn parse_obligation_rule(&mut self) -> Result<(ObligationRule, Option<Symbol>), anyhow::Error> {
        if self.tokens.next_if(TokenType::Colon).is_none() {
            return Ok((ObligationRule::ToUse, None));
        }

        let pos = self.tokens.peek(0).pos.clone();
        if self.tokens.peek(0).contextual() != Some(ContextualKeyword::Discharge) {
            return Err(self.obligation_rule_error(&pos));
        }
        self.tokens.next();

        match self.parse_identifier()?.as_str() {
            "to" => match self.parse_identifier()?.as_str() {
                "use" => {
                    let witness = self.tokens.next_if(TokenType::Identifier).map(|tok| self.ast.intern(&tok.lexeme));
                    Ok((ObligationRule::ToUse, witness))
                },
                "escape" => Ok((ObligationRule::ToEscape, None)),
                _ => Err(self.obligation_rule_error(&pos)),
            },
            "before" => match self.parse_identifier()?.as_str() {
                "drop" => Ok((ObligationRule::BeforeDrop, None)),
                _ => Err(self.obligation_rule_error(&pos)),
            },
            _ => Err(self.obligation_rule_error(&pos)),
        }
    }

    fn obligation_rule_error(&self, pos: &SourcePosition) -> anyhow::Error {
        self.error_help("Invalid obligation rule", pos, "a rule is `discharge to use`, `discharge to escape`, or `discharge before drop`")
    }

    pub(super) fn parse_while(&mut self) -> Result<AstId<Stmt>, anyhow::Error> {
        let pos = self.tokens.expect(TokenType::While)?.pos.clone();
        let condition = self.parse_condition()?;
        let body = self.parse_block_or_stmt()?;
        Ok(self.node_stmt(Stmt::While(condition, body), pos))
    }

    pub(super) fn parse_return(&mut self) -> Result<AstId<Stmt>, anyhow::Error> {
        let pos = self.tokens.expect(TokenType::Return)?.pos.clone();
        let expr = match self.tokens.matches(TokenType::Semicolon) {
            true => None,
            false => Some(self.parse_expr()?)
        };
        self.tokens.expect(TokenType::Semicolon)?;
        Ok(self.node_stmt(Stmt::Return(expr), pos))
    }

    pub(super) fn parse_throw(&mut self) -> Result<AstId<Stmt>, anyhow::Error> {
        let pos = self.tokens.expect(TokenType::Throw)?.pos.clone();
        let expr = self.parse_expr_semi()?;
        Ok(self.node_stmt(Stmt::Throw(expr), pos))
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

        let catch = if self.tokens.next_if(TokenType::Catch).is_some() {
            let (param, mutable) = match self.tokens.peek(0).kind {
                TokenType::Identifier => (Some(self.parse_identifier_expr()?), false),
                TokenType::LeftParen => {
                    let open = self.tokens.expect(TokenType::LeftParen)?.pos.clone();
                    let mutable = self.parse_mut();
                    let param = self.parse_identifier_expr()?;
                    // A caught value is always nullable, so a marker or clause carries no meaning,
                    // but accept the parameter surface so a catch binding parses like any other.
                    self.parse_nullable();
                    self.parse_slot_clause(SlotKind::Param)?;
                    self.tokens.expect_close(TokenType::RightParen, &open)?;
                    (Some(param), mutable)
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

        Ok(self.node_stmt(Stmt::Try(try_body, catch, finally), pos))
    }

    pub(super) fn parse_block(&mut self) -> Result<AstId<Expr>, anyhow::Error> {
        let pos = self.tokens.peek(0).pos.clone();
        self.tokens.expect(TokenType::LeftBrace)?;
        let stmts = self.parse_stmts()?;
        self.tokens.expect_close(TokenType::RightBrace, &pos)?;
        Ok(self.node_expr(Expr::Block(stmts), pos))
    }

    pub(super) fn parse_block_or_stmt(&mut self) -> Result<AstId<Expr>, anyhow::Error> {
        if self.tokens.matches(TokenType::LeftBrace) {
            self.parse_block()
        } else {
            let pos = self.tokens.peek(0).pos.clone();
            let stmt = self.parse_stmt()?;
            Ok(self.node_expr(Expr::Block(vec![stmt]), pos))
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
        while !self.tokens.matches(TokenType::RightBrace) && self.tokens.has_next() {
            stmts.push(self.parse_stmt()?);
        }
        Ok(stmts)
    }

    pub(super) fn parse_expr_stmt(&mut self) -> Result<AstId<Stmt>, anyhow::Error> {
        let pos = self.tokens.peek(0).pos.clone();
        let expr = self.parse_expr_semi()?;
        Ok(self.node_stmt(Stmt::Expression(expr), pos))
    }
}

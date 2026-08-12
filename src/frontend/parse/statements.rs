//! Statement parsing.

use super::*;

impl<'parser, 'vm> Parser<'parser, 'vm> {
    pub(super) fn parse_stmt(&mut self) -> Result<AstId<Stmt>, anyhow::Error> {
        match self.tokens.peek(0).kind {
            TokenType::Say => self.parse_say(),
            TokenType::While => self.parse_while(),
            TokenType::Fn => self.parse_fn(false),
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
        if self.tokens.peek(0).contextual() == Some(ContextualKeyword::Mut) {
            let at = self.tokens.peek(0).pos.clone();
            parse_error!(self, &at, "A reassignable binding is declared with `var`, not `mut`");
        }
        let reassignable = self.take_modifier(ContextualKeyword::Var);
        let name_pos = self.tokens.peek(0).pos.clone();
        let name = self.parse_identifier()?;
        self.check_name_case(&name, NameKind::Variable, &name_pos)?;
        let name = self.ast.intern(&name);
        let nullable = self.parse_nullable();
        let clause = self.parse_slot_clause(SlotKind::Local)?;

        let expr = if let Some(_) = self.tokens.next_if(TokenType::Equal) {
            Some(self.parse_expr()?)
        } else {
            None
        };

        self.tokens.expect(TokenType::Semicolon)?;
        let field_init = FieldInit { name, value: expr, nullable, reassignable, clause };
        Ok(self.node_stmt(Stmt::Say(field_init), pos))
    }

    /// obligation := "obligation" Name obligation_body
    pub(super) fn parse_obligation(&mut self) -> Result<AstId<Stmt>, anyhow::Error> {
        let pos = self.tokens.expect(TokenType::Obligation)?.pos.clone();
        let name_pos = self.tokens.peek(0).pos.clone();
        let name = self.parse_identifier()?;
        self.check_name_case(&name, NameKind::Obligation, &name_pos)?;
        let (witness, rules) = self.parse_obligation_body(&name)?;
        let name = self.ast.intern(&name);
        Ok(self.node_stmt(Stmt::Obligation { name, witness, rules }, pos))
    }

    /// obligation_body := "{" entry* "}"
    fn parse_obligation_body(&mut self, name: &str) -> Result<(Option<Symbol>, ObligationRules), anyhow::Error> {
        let pos = self.tokens.peek(0).pos.clone();
        if self.tokens.matches(TokenType::Semicolon) {
            return Err(self.error_help(format!("Obligation '{name}' declares no rules"), &pos,
                format!("spell out what it forbids, as in `obligation {name} {{ discharge to use; }}`")));
        }

        let open = self.tokens.expect(TokenType::LeftBrace)?.pos.clone();
        let mut witness = None;
        let mut rules = ObligationRules::default();

        while !self.tokens.matches(TokenType::RightBrace) && self.tokens.has_next() {
            self.parse_obligation_entry(&mut witness, &mut rules)?;
        }

        self.tokens.expect_close(TokenType::RightBrace, &open)?;

        if rules == ObligationRules::default() {
            // A witness names a bad state without forbidding anything, so it needs a rule beside it.
            let help = match witness.is_some() {
                true => "a witness alone constrains nothing, so add a rule such as `discharge to use;`",
                false => "spell out what it forbids, as in `discharge to use;`, or drop the declaration",
            };
            return Err(self.error_help(format!("Obligation '{name}' declares no rules"), &open, help));
        }

        Ok((witness, rules))
    }

    /// entry := ("witness" Name | "discharge" ("to" "use" | "before" "drop") | "no" ("persist" | "return" | "drop")) ";"
    fn parse_obligation_entry(&mut self, witness: &mut Option<Symbol>, rules: &mut ObligationRules) -> Result<(), anyhow::Error> {
        let pos = self.tokens.peek(0).pos.clone();
        match self.parse_identifier()?.as_str() {
            "witness" => {
                if witness.is_some() { parse_error!(self, &pos, "Repeated 'witness'"); }
                let name = self.parse_identifier()?;
                *witness = Some(self.ast.intern(&name));
            },
            "discharge" => match self.parse_identifier()?.as_str() {
                "to" => {
                    self.expect_word("use", &pos)?;
                    self.set_rule(&mut rules.to_use, "discharge to use", &pos)?;
                },
                "before" => {
                    self.expect_word("drop", &pos)?;
                    self.set_rule(&mut rules.before_drop, "discharge before drop", &pos)?;
                },
                _ => return Err(self.obligation_rule_error(&pos)),
            },
            // `return` is a keyword, so it does not arrive as an identifier like the other rules.
            "no" if self.tokens.next_if(TokenType::Return).is_some() => {
                self.set_rule(&mut rules.no_return, "no return", &pos)?;
            },
            "no" => match self.parse_identifier()?.as_str() {
                "persist" => self.set_rule(&mut rules.no_persist, "no persist", &pos)?,
                "drop" => self.set_rule(&mut rules.no_drop, "no drop", &pos)?,
                _ => return Err(self.obligation_rule_error(&pos)),
            },
            _ => return Err(self.obligation_rule_error(&pos)),
        }
        self.tokens.expect(TokenType::Semicolon)?;
        Ok(())
    }

    /// Consumes the second word of a two-word rule name.
    fn expect_word(&mut self, word: &str, pos: &SourcePosition) -> Result<(), anyhow::Error> {
        match self.parse_identifier()?.as_str() {
            found if found == word => Ok(()),
            _ => Err(self.obligation_rule_error(pos)),
        }
    }

    /// Sets a rule flag, rejecting a repeat so each rule reads once per declaration.
    fn set_rule(&self, flag: &mut bool, name: &str, pos: &SourcePosition) -> Result<(), anyhow::Error> {
        if *flag {
            return Err(self.error(format!("Repeated '{name}'"), pos));
        }
        *flag = true;
        Ok(())
    }

    fn obligation_rule_error(&self, pos: &SourcePosition) -> anyhow::Error {
        self.error_help("Invalid obligation rule", pos,
            "a rule is `witness T`, `discharge to use`, `discharge before drop`, `no persist`, `no return`, or `no drop`")
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
            let param = match self.tokens.peek(0).kind {
                TokenType::Identifier => {
                    let (lex, at) = (self.tokens.peek(0).lexeme.clone(), self.tokens.peek(0).pos.clone());
                    let param = self.parse_identifier_expr()?;
                    self.check_name_case(&lex, NameKind::Parameter, &at)?;
                    Some(param)
                },
                TokenType::LeftParen => {
                    let open = self.tokens.expect(TokenType::LeftParen)?.pos.clone();
                    // A caught value is bound for the handler and nothing reassigns it, so neither
                    // `var` nor the `mut` capability has anything to say here.
                    if let Some(word @ (ContextualKeyword::Var | ContextualKeyword::Mut)) = self.tokens.peek(0).contextual() {
                        let at = self.tokens.peek(0).pos.clone();
                        parse_error!(self, &at, "A catch parameter cannot be `{word}`");
                    }
                    let (lex, at) = (self.tokens.peek(0).lexeme.clone(), self.tokens.peek(0).pos.clone());
                    let param = self.parse_identifier_expr()?;
                    self.check_name_case(&lex, NameKind::Parameter, &at)?;
                    // A caught value is always nullable, so a marker or clause carries no meaning,
                    // but accept the parameter surface so a catch binding parses like any other.
                    self.parse_nullable();
                    self.parse_slot_clause(SlotKind::Param)?;
                    self.tokens.expect_close(TokenType::RightParen, &open)?;
                    Some(param)
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

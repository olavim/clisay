//! Function, factory, and parameter-list parsing.

use super::*;

impl<'parser, 'vm> Parser<'parser, 'vm> {
    pub(super) fn parse_fn(&mut self, is_method: bool) -> Result<AstId<Stmt>, anyhow::Error> {
        let pos = self.tokens.expect(TokenType::Fn)?.pos.clone();
        let name_pos = self.tokens.peek(0).pos.clone();
        let name = self.parse_identifier()?;
        self.check_name_case(&name, NameKind::Fn, &name_pos)?;
        let name = self.ast.intern(&name);
        let fn_decl = self.parse_fn_decl(name, name_pos)?;
        self.check_receiver_presence(fn_decl.receiver.as_ref(), is_method, &fn_decl.sig_pos)?;
        Ok(self.node_stmt(Stmt::Fn(fn_decl), pos))
    }

    pub(super) fn check_receiver_presence(&self, receiver: Option<&Receiver>, is_method: bool, sig_pos: &SourcePosition) -> Result<(), anyhow::Error> {
        match (is_method, receiver) {
            (true, None) => Err(self.error_help("A method must declare its receiver", sig_pos, "name it first in the parameter list, as `fn name(this)`")),
            (false, Some(r)) => Err(self.error_help("Only a method declares 'this'", &r.pos, "a function outside a type body has no receiver")),
            _ => Ok(()),
        }
    }

    pub(super) fn parse_fn_decl(&mut self, name: Symbol, name_pos: SourcePosition) -> Result<FnDecl, anyhow::Error> {
        self.tokens.expect(TokenType::LeftParen)?;
        let (receiver, params) = self.parse_params(TokenType::RightParen)?;
        let ret = self.parse_return_shape();
        let clause = self.parse_slot_clause(SlotKind::Return)?;
        let sig_pos = name_pos.to(&self.tokens.previous().pos);
        let body = self.parse_block()?;
        Ok(FnDecl {
            name,
            sig_pos,
            receiver,
            params,
            body,
            ret,
            clause,
        })
    }

    pub(super) fn init_name(&mut self) -> Symbol {
        let ty = self.current_type.clone().expect("init parsed outside a type");
        self.ast.intern(&format!("{}.init", ty))
    }

    pub(super) fn parse_init(&mut self) -> Result<AstId<Stmt>, anyhow::Error> {
        let pos = self.tokens.peek(0).pos.clone();
        let name = self.init_name();
        self.tokens.expect(TokenType::LeftParen)?;
        let (receiver, params) = self.parse_params(TokenType::RightParen)?;
        let sig_pos = pos.to(&self.tokens.previous().pos);
        self.check_receiver_presence(receiver.as_ref(), true, &sig_pos)?;
        let open = self.tokens.expect(TokenType::LeftBrace)?.pos.clone();

        let stmts = self.parse_stmts()?;
        self.tokens.expect_close(TokenType::RightBrace, &open)?;

        let body = self.node_expr(Expr::Block(stmts), pos.clone());
        let fn_decl = FnDecl {
            name,
            sig_pos,
            receiver,
            params,
            body,
            ret: ReturnShape::Void,
            clause: SlotClause::default()
        };
        Ok(self.node_stmt(Stmt::Fn(fn_decl), pos))
    }

    /// Parses a parameter list up to `end_token`, taking a leading `this` as the receiver.
    pub(super) fn parse_params(&mut self, end_token: TokenType) -> Result<(Option<Receiver>, Vec<Param>), anyhow::Error> {
        if self.tokens.next_if(end_token).is_some() {
            return Ok((None, Vec::new()));
        }

        let receiver = self.parse_receiver()?;
        if receiver.is_some() && !self.tokens.matches(end_token) {
            self.tokens.expect(TokenType::Comma)?;
        }

        let mut params = Vec::new();
        while !self.tokens.matches(end_token) {
            // A `this` further along still spells the receiver, so point at where it belongs.
            if self.at_receiver() {
                let pos = self.tokens.peek(0).pos.clone();
                let msg = if receiver.is_some() { "Repeated 'this' parameter" } else { "'this' must be the first parameter" };
                return Err(self.error_help(msg, &pos, "a method declares its receiver once, ahead of the other parameters"));
            }
            params.push(self.parse_param()?);
            if self.tokens.next_if(TokenType::Comma).is_none() {
                break;
            }
        }
        self.tokens.expect(end_token)?;
        Ok((receiver, params))
    }

    /// receiver := "this" (":" clause)?
    fn parse_receiver(&mut self) -> Result<Option<Receiver>, anyhow::Error> {
        let start = self.tokens.peek(0).pos.clone();
        if !self.at_receiver() {
            return Ok(None);
        }
        let prefix = self.parse_capability_prefix();
        self.tokens.expect(TokenType::This)?;
        let mut clause = self.parse_slot_clause(SlotKind::Receiver)?;
        clause.capability = prefix;
        let pos = start.to(&self.tokens.previous().pos);
        Ok(Some(Receiver { pos, clause }))
    }

    fn at_receiver(&self) -> bool {
        let mut ahead = 0;
        if matches!(self.tokens.peek(ahead).kind, TokenType::StarMut | TokenType::Multiply) {
            ahead += 1;
        }
        if self.tokens.peek(ahead).contextual() == Some(ContextualKeyword::Mut) {
            ahead += 1;
        }
        self.tokens.peek(ahead).kind == TokenType::This
    }

    fn parse_capability_prefix(&mut self) -> Capability {
        if self.tokens.next_if(TokenType::StarMut).is_some() {
            return Capability::MoveMut;
        }
        let takes = self.tokens.next_if(TokenType::Multiply).is_some();
        let writes = self.take_modifier(ContextualKeyword::Mut);
        match (takes, writes) {
            (true, true) => Capability::MoveMut,
            (true, false) => Capability::Move,
            (false, true) => Capability::Mut,
            (false, false) => Capability::None,
        }
    }

    fn pattern_takes_prefix(&self, pattern: &AstId<Matcher>) -> bool {
        !matches!(self.ast.get(pattern), Matcher::As(..) | Matcher::Or(_) | Matcher::And(_))
    }

    /// param := pattern (":" clause)?
    ///
    /// The pattern is the whole parameter. A lone lowercase name binds the argument, `_` discards
    /// it, and any other pattern is a precondition the argument has to satisfy.
    fn parse_param(&mut self) -> Result<Param, anyhow::Error> {
        let start = self.tokens.peek(0).pos.clone();
        let prefix = self.parse_capability_prefix();
        // A group settles what the marker covers, so its contents need no further judging.
        let grouped = self.tokens.matches(TokenType::LeftParen);
        let pattern = self.with_ctx(ExprCtx::matcher(), |p| p.parse_matcher())?;
        if prefix != Capability::None && !grouped && !self.pattern_takes_prefix(&pattern) {
            return Err(self.error_help("A capability ahead of a combinator needs the pattern grouped", &start,
                "group it so the marker cannot read as part of the pattern, as in `*(p @ Node | null)`"));
        }
        let nullable = self.parse_nullable();
        let mut clause = self.parse_slot_clause(SlotKind::Param)?;
        clause.capability = prefix;
        let pos = start.to(&self.tokens.previous().pos);
        Ok(Param { pattern, pos, nullable, reassignable: false, clause })
    }
}

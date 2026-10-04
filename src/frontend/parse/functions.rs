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
        let clause = self.parse_slot_clause(SlotKind::Return)?;
        let sig_pos = name_pos.to(&self.tokens.previous().pos);
        let body = self.parse_block()?;
        Ok(FnDecl {
            name,
            sig_pos,
            receiver,
            params,
            body,
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
            clause: SlotClause::default()
        };
        Ok(self.node_stmt(Stmt::Fn(fn_decl), pos))
    }

    /// params := (receiver | param) ("," (receiver | param))* ","?
    pub(super) fn parse_params(&mut self, end_token: TokenType) -> Result<(Option<Receiver>, Vec<Param>), anyhow::Error> {
        let mut receiver = None;
        let mut params = Vec::new();
        while !self.tokens.matches(end_token) {
            let start = self.tokens.peek(0).pos.clone();
            let anchor = self.tokens.next_if(TokenType::Amp).is_some();
            let reassignable = self.take_modifier(ContextualKeyword::Var);
            match self.tokens.next_if(TokenType::This) {
                Some(_) => {
                    if receiver.is_some() || !params.is_empty() {
                        let msg = if receiver.is_some() { "Repeated 'this' parameter" } else { "'this' must be the first parameter" };
                        return Err(self.error_help(msg, &start, "a method declares its receiver once, ahead of the other parameters"));
                    }
                    let clause = self.parse_slot_clause(SlotKind::Receiver)?;
                    let pos = start.to(&self.tokens.previous().pos);
                    if anchor && !reassignable {
                        return Err(self.error_help("Invalid read-only anchor parameter", &pos,
                            "A parameter cannot be declared as a read-only anchor. Declare it `&var this` to mutate the original value, or `this` to take a read-only copy."));
                    }
                    receiver = Some(Receiver { pos, clause, reassignable, anchor });
                },
                None => params.push(self.finish_param(start, anchor, reassignable)?),
            }

            if !self.tokens.matches(end_token) {
                self.tokens.expect(TokenType::Comma)?;
            }
        }
        self.tokens.expect(end_token)?;
        Ok((receiver, params))
    }

    /// param := "var"? pattern (":" clause)?
    pub(super) fn parse_param(&mut self) -> Result<Param, anyhow::Error> {
        let start = self.tokens.peek(0).pos.clone();
        let anchor = self.tokens.next_if(TokenType::Amp).is_some();
        let reassignable = self.take_modifier(ContextualKeyword::Var);
        self.finish_param(start, anchor, reassignable)
    }

    fn finish_param(&mut self, start: SourcePosition, anchor: bool, reassignable: bool) -> Result<Param, anyhow::Error> {
        let pattern = self.with_ctx(ExprCtx::matcher(), |p| p.parse_matcher())?;
        let clause = self.parse_slot_clause(SlotKind::Param)?;
        let pos = start.to(&self.tokens.previous().pos);
        if anchor && !matches!(self.ast.get(&pattern), Matcher::Binder(_)) {
            return Err(self.error("An anchor parameter names one binding, so it takes no pattern", &pos));
        }
        if anchor && !reassignable {
            let Matcher::Binder(name) = self.ast.get(&pattern) else { unreachable!("an anchor parameter is a binder") };
            let name = self.ast.text(*name).to_string();
            return Err(self.error_help("Invalid read-only anchor parameter", &pos,
                format!("A parameter cannot be declared as a read-only anchor. Declare it `&var {name}` to mutate the original value, or `{name}` to take a read-only copy.")));
        }
        Ok(Param { anchor, pattern, pos, reassignable, clause })
    }
}

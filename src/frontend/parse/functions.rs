//! Function, factory, and parameter-list parsing.

use super::*;

impl<'parser, 'vm> Parser<'parser, 'vm> {
    pub(super) fn parse_fn(&mut self) -> Result<AstId<Stmt>, anyhow::Error> {
        let pos = self.tokens.expect(TokenType::Fn)?.pos.clone();
        let name_pos = self.tokens.peek(0).pos.clone();
        let name = self.parse_identifier()?;
        self.check_name_case(&name, NameKind::Fn, &name_pos)?;
        let name = self.ast.intern(&name);
        let fn_decl = self.parse_fn_decl(name, name_pos)?;
        Ok(self.node_stmt(Stmt::Fn(fn_decl), pos))
    }

    pub(super) fn parse_fn_decl(&mut self, name: Symbol, name_pos: SourcePosition) -> Result<FnDecl, anyhow::Error> {
        self.tokens.expect(TokenType::LeftParen)?;
        let params = self.parse_params(TokenType::RightParen)?;
        let ret = self.parse_return_shape();
        let clause = self.parse_slot_clause(SlotKind::Return)?;
        let sig_pos = name_pos.to(&self.tokens.previous().pos);
        let body = self.parse_block()?;
        Ok(FnDecl {
            name,
            sig_pos,
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
        let params = self.parse_params(TokenType::RightParen)?;
        let sig_pos = pos.to(&self.tokens.previous().pos);
        let open = self.tokens.expect(TokenType::LeftBrace)?.pos.clone();

        let stmts = self.parse_stmts()?;
        self.tokens.expect_close(TokenType::RightBrace, &open)?;

        let body = self.node_expr(Expr::Block(stmts), pos.clone());
        // An `init` takes no return marker. It produces no value.
        let fn_decl = FnDecl { name, sig_pos, params, body, ret: ReturnShape::Void, clause: SlotClause::default() };
        Ok(self.node_stmt(Stmt::Fn(fn_decl), pos))
    }

    /// Parses a parameter list up to `end_token`.
    pub(super) fn parse_params(&mut self, end_token: TokenType) -> Result<Vec<Param>, anyhow::Error> {
        if self.tokens.next_if(end_token).is_some() {
            return Ok(Vec::new());
        }

        let mut params = Vec::new();
        while !self.tokens.matches(end_token) {
            params.push(self.parse_param()?);
            if self.tokens.next_if(TokenType::Comma).is_none() {
                break;
            }
        }
        self.tokens.expect(end_token)?;
        Ok(params)
    }

    /// param := pattern (":" clause)?
    ///
    /// The pattern is the whole parameter. A lone lowercase name binds the argument, `_` discards
    /// it, and any other pattern is a precondition the argument has to satisfy.
    fn parse_param(&mut self) -> Result<Param, anyhow::Error> {
        let start = self.tokens.peek(0).pos.clone();
        let pattern = self.with_ctx(ExprCtx::matcher(), |p| p.parse_matcher())?;
        let nullable = self.parse_nullable();
        let clause = self.parse_slot_clause(SlotKind::Param)?;
        let pos = start.to(&self.tokens.previous().pos);
        Ok(Param { pattern, pos, nullable, mutable: false, clause })
    }
}

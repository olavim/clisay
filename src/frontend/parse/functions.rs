//! Function, initializer, and parameter-list parsing.

use super::*;

impl<'parser, 'vm> Parser<'parser, 'vm> {
    pub(super) fn parse_fn(&mut self) -> Result<AstId<Stmt>, anyhow::Error> {
        let pos = self.tokens.expect(TokenType::Fn)?.pos.clone();
        let name = self.parse_identifier()?;
        let name = self.ast.intern(&name);
        let fn_decl = self.parse_fn_decl(name)?;
        Ok(self.ast.add_stmt(Stmt::Fn(fn_decl), pos))
    }

    pub(super) fn parse_fn_decl(&mut self, name: Symbol) -> Result<FnDecl, anyhow::Error> {
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

    pub(super) fn init_name(&mut self) -> Symbol {
        let ty = self.current_type.clone().expect("init parsed outside a type");
        self.ast.intern(&format!("{}.init", ty))
    }

    pub(super) fn parse_init(&mut self) -> Result<AstId<Stmt>, anyhow::Error> {
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
    pub(super) fn parse_params(&mut self, end_token: TokenType) -> Result<Vec<Param>, anyhow::Error> {
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
}

//! `type`/`trait` declaration parsing.

use super::*;

impl<'parser, 'vm> Parser<'parser, 'vm> {
    /// Parses the composition header: an optional `with T1, T2, ...` clause then an optional
    /// `req T1, T2, ...` clause (each at most once, in that order).
    pub(super) fn parse_composition_header(&mut self) -> Result<(Vec<Symbol>, Vec<Symbol>), anyhow::Error> {
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
    pub(super) fn parse_trait_clause(&mut self, keyword: ContextualKeyword) -> Result<Vec<Symbol>, anyhow::Error> {
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
    pub(super) fn parse_visibility(&mut self) -> Visibility {
        match self.tokens.peek(0).contextual() {
            Some(ContextualKeyword::Pub) => { self.tokens.next(); Visibility::Pub },
            Some(ContextualKeyword::Inner) => { self.tokens.next(); Visibility::Inner },
            _ => Visibility::Private,
        }
    }

    /// Parses a `req fn f(params)<marker>;` method hole, returning its name, arity, and
    /// declared return shape.
    pub(super) fn parse_req_fn(&mut self) -> Result<(Symbol, usize, ReturnShape), anyhow::Error> {
        self.tokens.expect(TokenType::Fn)?;
        let name = self.parse_identifier()?;
        let name = self.ast.intern(&name);
        self.tokens.expect(TokenType::LeftParen)?;
        let params = self.parse_params(TokenType::RightParen)?;
        let ret = self.parse_return_shape();
        self.tokens.expect(TokenType::Semicolon)?;
        Ok((name, params.len(), ret))
    }

    pub(super) fn parse_type_decl(&mut self, is_trait: bool) -> Result<AstId<Stmt>, anyhow::Error> {
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
}

//! `type`/`trait` declaration parsing.

use std::collections::HashMap;

use indexmap::IndexSet;
use super::*;

fn builtin_of(name: &str, pos: &SourcePosition) -> Option<BuiltinType> {
    if !pos.is_vm_source() {
        return None;
    }
    match name {
        "Err" => Some(BuiltinType::Err),
        "Ref" => Some(BuiltinType::Ref),
        _ => None,
    }
}

impl<'parser, 'vm> Parser<'parser, 'vm> {
    /// Parses the composition header: an optional `with T1, T2, ...` clause then an optional
    /// `req T1, T2, ...` clause (each at most once, in that order).
    pub(super) fn parse_composition_header(&mut self, is_trait: bool, refs: &mut Vec<TraitRef>) -> Result<(Vec<Symbol>, Vec<Symbol>), anyhow::Error> {
        let with_traits = self.parse_trait_clause(ContextualKeyword::With, TraitClause::With, refs)?;
        let req_pos = self.tokens.peek(0).pos.clone();
        let req_traits = self.parse_trait_clause(ContextualKeyword::Req, TraitClause::Req, refs)?;

        if !is_trait {
            if let Some(first) = req_traits.first() {
                let name = self.ast.text(*first);
                return Err(self.error_help("'req' is a trait-only clause", &req_pos,
                    format!("a type provides what it uses: `with {name}`, or a field that `gives {name}`")));
            }
        }

        // A header is `with ... req ...`; any further `with`/`req` here is a duplicate or misordered clause.
        let tok = self.tokens.peek(0);
        match tok.contextual() {
            Some(kw @ (ContextualKeyword::With | ContextualKeyword::Req)) =>
                Err(self.error_help(format!("Unexpected '{kw}' clause"), &tok.pos, "a type/trait header allows at most one `with` clause followed by at most one `req` clause")),
            _ => Ok((with_traits, req_traits)),
        }
    }

    /// Parses a single `<keyword> T1, T2, ...` trait-list clause, recording each trait's span.
    pub(super) fn parse_trait_clause(&mut self, keyword: ContextualKeyword, clause: TraitClause, refs: &mut Vec<TraitRef>) -> Result<Vec<Symbol>, anyhow::Error> {
        let present = self.tokens.peek(0).contextual() == Some(keyword);
        let mut traits = Vec::new();
        if present {
            self.tokens.next();
            loop {
                let pos = self.tokens.peek(0).pos.clone();
                let name = self.parse_identifier()?;
                let trait_sym = self.ast.intern(&name);
                traits.push(trait_sym);
                refs.push(TraitRef { clause, trait_sym, pos });
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

    /// Parses a `req fn f(params)<marker>: clause;` method hole.
    pub(super) fn parse_req_fn(&mut self) -> Result<ReqFn, anyhow::Error> {
        self.tokens.expect(TokenType::Fn)?;
        let start = self.tokens.peek(0).pos.clone();
        let name = self.parse_identifier()?;
        self.check_name_case(&name, NameKind::Fn, &start)?;
        let name = self.ast.intern(&name);
        self.tokens.expect(TokenType::LeftParen)?;
        let (receiver, params) = self.parse_params(TokenType::RightParen)?;
        let clause = self.parse_slot_clause(SlotKind::Return)?;
        let pos = start.to(&self.tokens.previous().pos);
        self.check_receiver_presence(receiver.as_ref(), true, &pos)?;
        let anchored = receiver.as_ref().filter(|r| r.anchor).map(|r| &r.pos)
            .or_else(|| params.iter().find(|p| p.anchor).map(|p| &p.pos));
        if let Some(pos) = anchored {
            return Err(self.error_help("A `req fn` cannot require an anchor parameter", pos,
                "an anchor names a slot of the caller, which is a calling convention rather than a contract"));
        }
        let marked = receiver.as_ref().filter(|r| r.reassignable).map(|r| &r.pos)
            .or_else(|| params.iter().find(|p| p.reassignable).map(|p| &p.pos));
        if let Some(pos) = marked {
            return Err(self.error_help("A `req fn` cannot require a `var` parameter", pos,
                "`req fn` cannot require its params to be `var`; composing types can still mark params as `var`, but it cannot be part of the contract"));
        }
        self.tokens.expect(TokenType::Semicolon)?;
        Ok(ReqFn { name, pos, receiver, params, clause })
    }

    /// Parses a `req "var"? name (":" clause)?;` member hole.
    pub(super) fn parse_req_member(&mut self) -> Result<ReqMember, anyhow::Error> {
        let start = self.tokens.peek(0).pos.clone();
        let reassignable = self.take_modifier(ContextualKeyword::Var);
        let name_pos = self.tokens.peek(0).pos.clone();
        let name = self.parse_identifier()?;
        self.check_name_case(&name, NameKind::Member, &name_pos)?;
        let clause = self.parse_slot_clause(SlotKind::Member)?;
        let pos = start.to(&self.tokens.previous().pos);
        self.tokens.expect(TokenType::Semicolon)?;
        Ok(ReqMember { name: self.ast.intern(&name), pos, reassignable, clause })
    }

    fn declare_member(&self, declared: &mut HashMap<Symbol, (SourcePosition, &'static str)>, name: Symbol, pos: &SourcePosition, kind: &'static str) -> Result<(), anyhow::Error> {
        let Some((first_pos, first_kind)) = declared.insert(name, (pos.clone(), kind)) else { return Ok(()) };
        let text = self.ast.text(name);
        Err(anyhow!("{}", Diagnostic::new(format!("'{text}' is declared more than once"), pos.clone())
            .with_label(format!("declared again as a {kind}"))
            .with_context_span(first_pos, format!("already declared as a {first_kind}"))
            .with_help("give one of them another name, or drop it")))
    }

    pub(super) fn parse_type_decl(&mut self, is_trait: bool) -> Result<AstId<Stmt>, anyhow::Error> {
        let keyword = if is_trait { TokenType::Trait } else { TokenType::Type };
        let pos = self.tokens.expect(keyword)?.pos.clone();
        let name_pos = self.tokens.peek(0).pos.clone();
        let type_name = self.parse_identifier()?;
        self.check_name_case(&type_name, if is_trait { NameKind::Trait } else { NameKind::Type }, &name_pos)?;
        let type_sym = self.ast.intern(&type_name);

        let prev_type = std::mem::replace(&mut self.current_type, Some(type_name.clone()));

        let mut trait_refs: Vec<TraitRef> = Vec::new();
        let (with_traits, req_traits) = self.parse_composition_header(is_trait, &mut trait_refs)?;

        let body_open = self.tokens.expect(TokenType::LeftBrace)?.pos.clone();

        let mut fields: IndexSet<Symbol> = IndexSet::default();
        let mut var_fields: HashSet<Symbol> = HashSet::default();
        let mut field_clauses: Vec<(Symbol, SlotClause)> = Vec::new();
        let mut field_positions: Vec<(Symbol, SourcePosition)> = Vec::new();
        let mut field_inits: Vec<(Symbol, AstId<Expr>)> = Vec::new();
        let mut method_stmts: Vec<AstId<Stmt>> = Vec::new();
        let mut pub_members: IndexSet<Symbol> = IndexSet::default();
        let mut inner_members: IndexSet<Symbol> = IndexSet::default();
        let mut req_fns: Vec<ReqFn> = Vec::new();
        let mut req_members: Vec<ReqMember> = Vec::new();
        let mut gives: Vec<(Symbol, Symbol)> = Vec::new();
        let mut init = None;
        let mut declared: HashMap<Symbol, (SourcePosition, &'static str)> = HashMap::default();

        while !self.tokens.matches(TokenType::RightBrace) && self.tokens.has_next() {
            let member_pos = self.tokens.peek(0).pos.clone();
            let visibility = self.parse_visibility();

            // `req fn f(params);` (method hole) or `req name;` (member/state hole)
            if self.tokens.peek(0).contextual() == Some(ContextualKeyword::Req) {
                if visibility != Visibility::Private { parse_error!(self, &member_pos, "A `req` declaration cannot have a visibility modifier"); }
                self.tokens.next(); // consume `req`
                if self.tokens.matches(TokenType::Fn) {
                    let req = self.parse_req_fn()?;
                    self.declare_member(&mut declared, req.name, &req.pos, "required method")?;
                    req_fns.push(req);
                } else {
                    let req = self.parse_req_member()?;
                    self.declare_member(&mut declared, req.name, &req.pos, "required member")?;
                    req_members.push(req);
                }
                continue;
            }

            let reassignable = self.take_modifier(ContextualKeyword::Var);

            let kind = self.tokens.peek(0).kind;
            match kind {
                TokenType::Fn => {
                    if reassignable { parse_error!(self, &member_pos, "Only fields can be `var`"); }
                    let stmt = self.parse_fn(true)?;
                    if let Stmt::Fn(decl) = self.ast.get(&stmt) {
                        let (name, sig_pos) = (decl.name, decl.sig_pos.clone());
                        self.declare_member(&mut declared, name, &sig_pos, "method")?;
                        match visibility {
                            Visibility::Pub => { pub_members.insert(decl.name); },
                            Visibility::Inner => { inner_members.insert(decl.name); },
                            Visibility::Private => {},
                        }
                    }
                    method_stmts.push(stmt);
                },
                TokenType::Identifier => {
                    let name_pos = self.tokens.peek(0).pos.clone();
                    let name = self.parse_identifier()?;

                    // `init` is a specially-named method (normal method syntax); it
                    // takes no visibility modifier.
                    match name.as_str() {
                        "init" => {
                            if is_trait { return Err(self.error_help("A trait cannot declare an `init`", &member_pos, "put initialization on the host type")); }
                            if visibility != Visibility::Private { parse_error!(self, &member_pos, "A factory cannot have a visibility modifier"); }
                            if reassignable { parse_error!(self, &member_pos, "Only fields can be `var`"); }
                            init = Some(self.parse_init()?);
                        },
                        _ => {
                            // Field declaration, optionally with a `gives Trait` delegation suffix.
                            if is_trait { return Err(self.error_help("A trait cannot declare fields", &member_pos, "`req` the state it needs and let the host type hold it")); }
                            self.check_name_case(&name, NameKind::Field, &name_pos)?;
                            let field = self.ast.intern(&name);
                            let clause = self.parse_slot_clause(SlotKind::Field)?;
                            let give = if self.tokens.peek(0).contextual() == Some(ContextualKeyword::Gives) {
                                self.tokens.next();
                                let give_pos = self.tokens.peek(0).pos.clone();
                                let trait_name = self.parse_identifier()?;
                                let trait_sym = self.ast.intern(&trait_name);
                                trait_refs.push(TraitRef { clause: TraitClause::Gives, trait_sym, pos: give_pos });
                                Some(trait_sym)
                            } else {
                                None
                            };

                            let value = self.tokens.next_if(TokenType::Equal)
                                .map(|_| self.parse_expr())
                                .transpose()?;

                            self.tokens.expect(TokenType::Semicolon)?;
                            let field_pos = member_pos.to(&self.tokens.previous().pos);
                            self.declare_member(&mut declared, field, &field_pos, "field")?;
                            fields.insert(field);
                            field_positions.push((field, field_pos));

                            if reassignable { var_fields.insert(field); }

                            if !clause.is_empty() || clause.void {
                                field_clauses.push((field, clause));
                            }

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
        self.tokens.expect_close(TokenType::RightBrace, &body_open)?;

        let init_name = self.ast.intern(&format!("{}.init", type_name));

        let type_decl = Box::new(TypeDecl {
            name: type_sym,
            is_trait,
            builtin: builtin_of(&type_name, &pos),
            with_traits,
            trait_refs,
            req_traits,
            req_fns,
            req_members,
            gives,
            init_name,
            init,
            fields,
            var_fields,
            field_clauses,
            field_positions,
            field_inits,
            methods: method_stmts,
            pub_members,
            inner_members,
        });

        self.current_type = prev_type;
        Ok(self.node_stmt(Stmt::Type(type_decl), pos))
    }
}

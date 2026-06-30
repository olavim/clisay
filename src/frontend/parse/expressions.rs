//! Expression parsing: the precedence climber and its operand forms.

use super::*;

impl<'parser, 'vm> Parser<'parser, 'vm> {
    pub(super) fn make_lambda(&mut self, params: Vec<AstId<Expr>>, body: AstId<Expr>) -> Expr {
        let name = self.ast.intern("lambda");
        // Lambda parameters take no markers. The return shape is inferred from the body.
        let params = params.into_iter()
            .map(|name| Param { name, nullable: false, mutable: false })
            .collect();
        Expr::Literal(Literal::Lambda(FnDecl { name, params, body, ret: ReturnShape::Inferred }))
    }

    pub(super) fn parse_expr(&mut self) -> Result<AstId<Expr>, anyhow::Error> {
        self.with_ctx(ExprCtx::default(), |p| p.parse_expr_precedence(0))
    }

    /// Parses an `if`/`while` head: a condition where a trailing `{` is the body block.
    pub(super) fn parse_condition(&mut self) -> Result<AstId<Expr>, anyhow::Error> {
        self.with_ctx(ExprCtx::condition(), |p| p.parse_expr_precedence(0))
    }

    /// Whether `expr` can be brace-constructed: a bare type name `C` or a call `C(args)`.
    pub(super) fn is_constructible(&self, expr: AstId<Expr>) -> bool {
        match self.ast.get(&expr) {
            Expr::Identifier(_) => true,
            Expr::Call(callee, _) => matches!(self.ast.get(callee), Expr::Identifier(_)),
            _ => false,
        }
    }

    /// Parses `{ field: value, ... }` after a constructible callee into an `Expr::Construct`.
    pub(super) fn parse_construction(&mut self, callee: AstId<Expr>) -> Result<AstId<Expr>, anyhow::Error> {
        let pos = self.ast.pos(&callee).clone();
        self.tokens.expect(TokenType::LeftBrace)?;

        // Allow nested braces to construct inside this body, but keep the other modes as they are.
        let value_precedence = Operator::Comma.infix_precedence().unwrap() + 1;
        let fields = self.with_ctx(ExprCtx::construct(self.ctx), |p| -> Result<_, anyhow::Error> {
            // Parse each value tighter than `,` so the comma separates fields, not the value expression.
            let mut fields: Vec<(Symbol, AstId<Expr>)> = Vec::new();
            while !p.tokens.matches(TokenType::RightBrace) {
                let key_pos = p.tokens.peek(0).pos.clone();
                let name = p.parse_identifier()?;
                let field = p.ast.intern(&name);
                let value = if p.tokens.next_if(TokenType::Colon).is_some() {
                    p.parse_expr_precedence(value_precedence)?
                } else {
                    p.ast.add_expr(Expr::Identifier(field), key_pos) // shorthand `{ x }` == `{ x: x }`
                };
                fields.push((field, value));
                if p.tokens.next_if(TokenType::Comma).is_none() { break; }
            }
            Ok(fields)
        })?;

        self.tokens.expect(TokenType::RightBrace)?;
        Ok(self.ast.add_expr(Expr::Construct(callee, fields), pos))
    }

    pub(super) fn parse_expr_precedence(&mut self, min_precedence: u8) -> Result<AstId<Expr>, anyhow::Error> {
        let mut left = self.parse_primary()?;

        loop {
            // In a match arm guard, a `=>` delimits the body. Stop before it so it is not read as a lambda.
            if self.ctx.stops_at_arrow() && self.tokens.matches(TokenType::FatArrow) {
                break;
            }
            // A `{` after a type name or call is a brace construction, unless we are parsing a
            // condition where the `{` opens the body block.
            if self.ctx.can_construct()
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
    pub(super) fn parse_primary(&mut self) -> Result<AstId<Expr>, anyhow::Error> {
        match Operator::parse_prefix(self.tokens, 0) {
            Some(op) => self.parse_expr_prefix(op),
            _ => self.parse_expr_atom(),
        }
    }

    pub(super) fn parse_expr_infix(&mut self, op: Operator, expr: AstId<Expr>) -> Result<AstId<Expr>, anyhow::Error> {
        let pos = self.ast.pos(&expr).clone();

        let kind = match &op {
            Operator::MemberAccess => {
                let id = self.parse_identifier()?;
                let id = self.ast.add_expr(Expr::Literal(Literal::String(id)), pos.clone());
                Expr::Index(expr, id, true) // `.name` member access
            },
            Operator::Is => {
                let name_token = self.tokens.peek(0).clone();
                if name_token.kind != TokenType::Identifier {
                    parse_error!(self, &pos, "`is` needs a type name");
                }
                let name = self.parse_identifier()?;
                let name = self.ast.intern(&name);
                match self.parse_type_shape()? {
                    Some(shape) => {
                        let m = self.ast.add_matcher(Matcher::Type { nominal: true, name, shape: Some(shape) }, name_token.pos);
                        Expr::Has(expr, m)
                    },
                    None => Expr::Is(expr, name),
                }
            },
            Operator::Has => Expr::Has(expr, self.parse_has_spec()?),
            Operator::Match => {
                let matcher = self.with_ctx(ExprCtx::matcher(), |p| p.parse_matcher())?;
                if let Some(Operator::Match) = Operator::peek_infix(self.tokens, 0) {
                    parse_error!(self, &pos, "`~` does not chain. Combine tests with `&` and `|`, or join tests with `&&` and `||`");
                }
                Expr::Match(expr, matcher)
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

    pub(super) fn parse_expr_prefix(&mut self, op: Operator) -> Result<AstId<Expr>, anyhow::Error> {
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
                // (`{ 1, "1" }`).
                let mut seen: HashSet<String> = HashSet::new();
                // Parse the value tighter than `,` so the comma separates pairs.
                let value_precedence = Operator::Comma.infix_precedence().unwrap() + 1;

                if self.tokens.next_if(TokenType::RightBrace).is_none() {
                    loop {
                        let tok = self.tokens.peek(0).clone();
                        let key_pos = tok.pos.clone();

                        // Each arm yields (key expr, value expr, optional static dedup key).
                        let (key_expr, value_expr, dup_key): (AstId<Expr>, AstId<Expr>, Option<String>) = match tok.kind {
                            TokenType::LeftBracket => {
                                self.tokens.next();
                                let key = self.parse_expr()?;
                                self.tokens.expect(TokenType::RightBracket)?;
                                self.tokens.expect(TokenType::Colon)?;
                                let value = self.parse_expr_precedence(value_precedence)?;
                                (key, value, None)
                            },
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
                            TokenType::StringLiteral => {
                                self.tokens.next();
                                let raw = tok.lexeme.clone();
                                let s = String::from(&raw[1..raw.len() - 1]); // strip quotes
                                let key = self.ast.add_expr(Expr::Literal(Literal::String(s.clone())), key_pos.clone());
                                self.tokens.expect(TokenType::Colon)?;
                                let value = self.parse_expr_precedence(value_precedence)?;
                                (key, value, Some(format!("s:{s}")))
                            },
                            TokenType::NumericLiteral => {
                                self.tokens.next();
                                let n: f64 = tok.lexeme.parse().unwrap();
                                let key = self.ast.add_expr(Expr::Literal(Literal::Number(n)), key_pos.clone());
                                self.tokens.expect(TokenType::Colon)?;
                                let value = self.parse_expr_precedence(value_precedence)?;
                                (key, value, Some(format!("n:{n}")))
                            },
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

    pub(super) fn parse_expr_postfix(&mut self, op: Operator, expr: AstId<Expr>) -> Result<AstId<Expr>, anyhow::Error> {
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

    pub(super) fn parse_expr_atom(&mut self) -> Result<AstId<Expr>, anyhow::Error> {
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

    pub(super) fn parse_call_arguments(&mut self) -> Result<Vec<AstId<Expr>>, anyhow::Error> {
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

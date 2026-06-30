//! Pattern matching: the `match` statement and the matcher grammar.

use super::*;

impl<'parser, 'vm> Parser<'parser, 'vm> {
    /// match_stmt := "match" expr "{" arm ("," arm)* ","? "}"
    pub(super) fn parse_match(&mut self) -> Result<AstId<Stmt>, anyhow::Error> {
        let pos = self.tokens.expect(TokenType::Match)?.pos.clone();
        let scrutinee = self.with_ctx(ExprCtx::scrutinee(), |p| p.parse_expr_precedence(0))?;
        self.tokens.expect(TokenType::LeftBrace)?;

        let mut arms: Vec<MatchArm> = Vec::new();
        while !self.tokens.matches(TokenType::RightBrace) {
            arms.push(self.parse_arm()?);
            if self.tokens.next_if(TokenType::Comma).is_none() {
                break;
            }
        }
        self.tokens.expect(TokenType::RightBrace)?;

        if arms.is_empty() {
            parse_error!(self, &pos, "A `match` needs at least one arm");
        }
        Ok(self.ast.add_stmt(Stmt::Match(scrutinee, arms), pos))
    }

    /// arm := matcher ("if" guard)? "=>" body
    fn parse_arm(&mut self) -> Result<MatchArm, anyhow::Error> {
        let matcher = self.with_ctx(ExprCtx::matcher(), |p| p.parse_matcher())?;
        let guard = match self.tokens.next_if(TokenType::If) {
            Some(_) => Some(self.parse_guard()?),
            None => None,
        };
        self.tokens.expect(TokenType::FatArrow)?;
        let body = self.parse_arm_body()?;
        Ok(MatchArm { matcher, guard, body })
    }

    /// An arm body: a `{ ... }` block, or a single expression run for effect.
    pub(super) fn parse_arm_body(&mut self) -> Result<AstId<Expr>, anyhow::Error> {
        if self.tokens.matches(TokenType::LeftBrace) {
            return self.parse_block();
        }
        let pos = self.tokens.peek(0).pos.clone();
        let body_precedence = Operator::Comma.infix_precedence().unwrap() + 1;
        let result = self.with_ctx(ExprCtx::default(), |p| p.parse_expr_precedence(body_precedence));
        let stmt = self.ast.add_stmt(Stmt::Expression(result?), pos.clone());
        Ok(self.ast.add_expr(Expr::Block(vec![stmt]), pos))
    }

    /// Parses a guard expression, stopping before the arm's `=>`.
    pub(super) fn parse_guard(&mut self) -> Result<AstId<Expr>, anyhow::Error> {
        self.with_ctx(ExprCtx::guard(), |p| p.parse_expr_precedence(0))
    }

    /// Parses one matcher into a fresh `Ast` and returns its handle. Used by tests.
    pub fn parse_matcher_root(tokens: &'vm mut TokenStream<'vm>) -> Result<(Ast, AstId<Matcher>), anyhow::Error> {
        let mut ast = Ast::new();
        let matcher = {
            let mut parser = Parser { tokens, ast: &mut ast, current_type: None, ctx: ExprCtx::default() };
            let matcher = parser.parse_matcher()?;
            parser.tokens.expect(TokenType::EOF)?;
            matcher
        };
        Ok((ast, matcher))
    }

    /// matcher := IDENT "@" matcher | or_matcher
    pub(super) fn parse_matcher(&mut self) -> Result<AstId<Matcher>, anyhow::Error> {
        // An as-binding is a bare name immediately followed by `@`. The right side is a full matcher.
        if self.tokens.peek(0).kind == TokenType::Identifier && self.tokens.peek(1).kind == TokenType::At {
            let token = self.tokens.next().clone();
            let pos = token.pos.clone();
            let name = self.ast.intern(&token.lexeme);
            self.tokens.next();
            let inner = self.parse_matcher()?;
            return Ok(self.ast.add_matcher(Matcher::As(name, inner), pos));
        }
        self.parse_or_matcher()
    }

    /// or_matcher := and_matcher ("|" and_matcher)*
    pub(super) fn parse_or_matcher(&mut self) -> Result<AstId<Matcher>, anyhow::Error> {
        let pos = self.tokens.peek(0).pos.clone();
        let first = self.parse_and_matcher()?;
        if !self.tokens.matches(TokenType::Pipe) {
            return Ok(first);
        }
        let mut alternatives = vec![first];
        while self.tokens.next_if(TokenType::Pipe).is_some() {
            alternatives.push(self.parse_and_matcher()?);
        }
        Ok(self.ast.add_matcher(Matcher::Or(alternatives), pos))
    }

    /// and_matcher := primary ("&" primary)*
    pub(super) fn parse_and_matcher(&mut self) -> Result<AstId<Matcher>, anyhow::Error> {
        let pos = self.tokens.peek(0).pos.clone();
        let first = self.parse_primary_matcher()?;
        if !self.tokens.matches(TokenType::Amp) {
            return Ok(first);
        }
        let mut parts = vec![first];
        while self.tokens.next_if(TokenType::Amp).is_some() {
            parts.push(self.parse_primary_matcher()?);
        }
        Ok(self.ast.add_matcher(Matcher::And(parts), pos))
    }

    pub(super) fn parse_primary_matcher(&mut self) -> Result<AstId<Matcher>, anyhow::Error> {
        let token = self.tokens.peek(0).clone();
        let pos = token.pos.clone();
        match token.kind {
            TokenType::Identifier if token.lexeme == "_" => {
                self.tokens.next();
                Ok(self.ast.add_matcher(Matcher::Wildcard, pos))
            },
            TokenType::Identifier => {
                self.tokens.next();
                let name = self.ast.intern(&token.lexeme);
                Ok(self.ast.add_matcher(Matcher::Binder(name), pos))
            },
            TokenType::Is => self.parse_is_matcher(),
            TokenType::Has => self.parse_has_matcher(),
            TokenType::LeftBrace => self.parse_shape_matcher(),
            TokenType::LeftBracket => self.parse_array_matcher(),
            TokenType::LeftParen => {
                self.tokens.next();
                let inner = self.with_ctx(ExprCtx::matcher(), |p| p.parse_matcher())?;
                self.tokens.expect(TokenType::RightParen)?;
                Ok(inner)
            },
            TokenType::NumericLiteral | TokenType::StringLiteral
            | TokenType::True | TokenType::False | TokenType::Null => {
                let scalar = self.parse_match_scalar()?;
                Ok(self.ast.add_matcher(Matcher::Literal(scalar), pos))
            },
            _ => parse_error!(self, &pos, "Expected a matcher but found '{token}'"),
        }
    }

    /// `is` type_ref shape?.
    pub(super) fn parse_is_matcher(&mut self) -> Result<AstId<Matcher>, anyhow::Error> {
        let pos = self.tokens.next().pos.clone();
        if self.tokens.peek(0).kind != TokenType::Identifier {
            parse_error!(self, &pos, "`is` is nominal and needs a type name; use `{{ ... }}` for a structural shape");
        }
        self.parse_typed_matcher(true, pos)
    }

    /// `has` type_ref shape?, `has` shape, or `has` array.
    pub(super) fn parse_has_matcher(&mut self) -> Result<AstId<Matcher>, anyhow::Error> {
        let pos = self.tokens.next().pos.clone();
        match self.tokens.peek(0).kind {
            TokenType::LeftBrace => self.parse_shape_matcher(),
            TokenType::LeftBracket => self.parse_array_matcher(),
            TokenType::Identifier => self.parse_typed_matcher(false, pos),
            TokenType::NumericLiteral | TokenType::StringLiteral
            | TokenType::True | TokenType::False | TokenType::Null =>
                parse_error!(self, &pos, "`has` needs a type name, a shape, or an array; for key presence use `{{ k: _ }}`, for equality use `==`"),
            _ => parse_error!(self, &pos, "`has` needs a type name, a shape, or an array"),
        }
    }

    pub(super) fn parse_typed_matcher(&mut self, nominal: bool, pos: SourcePosition) -> Result<AstId<Matcher>, anyhow::Error> {
        let name = self.parse_identifier()?;
        let name = self.ast.intern(&name);
        let shape = if self.tokens.peek(0).kind == TokenType::LeftBrace {
            Some(self.parse_shape_matcher()?)
        } else {
            None
        };
        Ok(self.ast.add_matcher(Matcher::Type { nominal, name, shape }, pos))
    }

    pub(super) fn parse_has_spec(&mut self) -> Result<AstId<Matcher>, anyhow::Error> {
        let token = self.tokens.peek(0).clone();
        let pos = token.pos.clone();
        match token.kind {
            TokenType::Identifier => {
                let name = self.parse_identifier()?;
                let name = self.ast.intern(&name);
                let shape = self.parse_type_shape()?;
                Ok(self.ast.add_matcher(Matcher::Type { nominal: false, name, shape }, pos))
            },
            TokenType::LeftBrace => self.parse_shape_matcher(),
            TokenType::LeftBracket => self.parse_array_matcher(),
            TokenType::NumericLiteral | TokenType::StringLiteral
            | TokenType::True | TokenType::False | TokenType::Null =>
                parse_error!(self, &pos, "`has` needs a type name, a shape, or an array; for key presence use `{{ k: _ }}`, for equality use `==`"),
            _ => parse_error!(self, &pos, "`has` needs a type name, a shape, or an array"),
        }
    }

    /// An optional `{ ... }` destructuring shape after a `has T`/`is T` operator. A `{` opens a
    /// shape only when it reads as `{ key: ... }`.
    pub(super) fn parse_type_shape(&mut self) -> Result<Option<AstId<Matcher>>, anyhow::Error> {
        let opens_shape = self.tokens.peek(0).kind == TokenType::LeftBrace
            && matches!(self.tokens.peek(1).kind,
                TokenType::Identifier | TokenType::NumericLiteral | TokenType::StringLiteral
                | TokenType::True | TokenType::False | TokenType::Null)
            && self.tokens.peek(2).kind == TokenType::Colon;
        match opens_shape {
            true => Ok(Some(self.parse_shape_matcher()?)),
            false => Ok(None),
        }
    }

    /// shape := "{" ( field ("," field)* ","? )? "}"
    pub(super) fn parse_shape_matcher(&mut self) -> Result<AstId<Matcher>, anyhow::Error> {
        let pos = self.tokens.expect(TokenType::LeftBrace)?.pos.clone();
        let mut fields: Vec<MatchField> = Vec::new();
        let mut seen: HashSet<String> = HashSet::new();

        while !self.tokens.matches(TokenType::RightBrace) {
            let field = self.with_ctx(ExprCtx::matcher(), |p| p.parse_shape_field())?;
            if !seen.insert(scalar_dedup(&field.key)) {
                parse_error!(self, &pos, "Duplicate key in a shape matcher");
            }
            fields.push(field);
            if self.tokens.next_if(TokenType::Comma).is_none() {
                break;
            }
        }
        
        self.tokens.expect(TokenType::RightBrace)?;
        Ok(self.ast.add_matcher(Matcher::Shape(fields), pos))
    }

    /// field := IDENT ":" matcher | scalar ":" matcher | IDENT (shorthand `{ x }` binds x)
    pub(super) fn parse_shape_field(&mut self) -> Result<MatchField, anyhow::Error> {
        let token = self.tokens.peek(0).clone();
        let pos = token.pos.clone();
        match token.kind {
            TokenType::Identifier => {
                self.tokens.next();
                let key = MatchScalar::String(token.lexeme.clone());

                if self.tokens.next_if(TokenType::Colon).is_some() {
                    let value = self.parse_matcher()?;
                    return Ok(MatchField { key, value });
                }

                // `{ x }` binds x.
                let sym = self.ast.intern(&token.lexeme);
                let value = self.ast.add_matcher(Matcher::Binder(sym), pos);
                Ok(MatchField { key, value })
            },
            TokenType::StringLiteral | TokenType::NumericLiteral
            | TokenType::True | TokenType::False | TokenType::Null => {
                let key = self.parse_match_scalar()?;
                self.tokens.expect(TokenType::Colon)?;
                let value = self.parse_matcher()?;
                Ok(MatchField { key, value })
            },
            _ => parse_error!(self, &pos, "Expected a shape field"),
        }
    }

    /// array := "[" ( elem ("," elem)* ","? )? "]"
    /// elem := matcher | ".." IDENT?
    pub(super) fn parse_array_matcher(&mut self) -> Result<AstId<Matcher>, anyhow::Error> {
        let pos = self.tokens.expect(TokenType::LeftBracket)?.pos.clone();
        let mut elements: Vec<MatchElem> = Vec::new();
        let mut seen_rest = false;

        while !self.tokens.matches(TokenType::RightBracket) {
            if self.tokens.peek(0).kind == TokenType::DotDot {
                let rest_pos = self.tokens.next().pos.clone();
                if seen_rest {
                    parse_error!(self, &rest_pos, "An array matcher allows at most one rest `..`");
                }
                seen_rest = true;
                let name = match self.tokens.next_if(TokenType::Identifier) {
                    Some(token) => Some(self.ast.intern(&token.lexeme)),
                    None => None
                };
                elements.push(MatchElem::Rest(name));
            } else {
                let matcher = self.with_ctx(ExprCtx::matcher(), |p| p.parse_matcher())?;
                elements.push(MatchElem::Elem(matcher));
            }
            if self.tokens.next_if(TokenType::Comma).is_none() {
                break;
            }
        }

        self.tokens.expect(TokenType::RightBracket)?;
        Ok(self.ast.add_matcher(Matcher::Array(elements), pos))
    }

    /// A scalar literal: a matcher equality value or a shape key.
    pub(super) fn parse_match_scalar(&mut self) -> Result<MatchScalar, anyhow::Error> {
        let token = self.tokens.next().clone();
        let pos = token.pos.clone();
        Ok(match token.kind {
            TokenType::NumericLiteral => MatchScalar::Number(token.lexeme.parse().unwrap()),
            TokenType::StringLiteral => MatchScalar::String(String::from(&token.lexeme[1..token.lexeme.len() - 1])),
            TokenType::True => MatchScalar::Boolean(true),
            TokenType::False => MatchScalar::Boolean(false),
            TokenType::Null => MatchScalar::Null,
            _ => parse_error!(self, &pos, "Expected a literal"),
        })
    }
}

/// A type-tagged string that distinguishes shape keys for duplicate detection,
/// so `1` and `"1"` do not collide.
fn scalar_dedup(key: &MatchScalar) -> String {
    match key {
        MatchScalar::String(s) => format!("s:{s}"),
        MatchScalar::Number(n) => format!("n:{n}"),
        MatchScalar::Boolean(b) => format!("b:{b}"),
        MatchScalar::Null => String::from("null"),
    }
}

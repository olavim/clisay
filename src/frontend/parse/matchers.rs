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
        self.reject_combinator_binder(&first)?;
        let mut alternatives = vec![first];
        while self.tokens.next_if(TokenType::Pipe).is_some() {
            let alt = self.parse_and_matcher()?;
            self.reject_combinator_binder(&alt)?;
            alternatives.push(alt);
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
        self.reject_combinator_binder(&first)?;
        let mut parts = vec![first];
        while self.tokens.next_if(TokenType::Amp).is_some() {
            let part = self.parse_primary_matcher()?;
            self.reject_combinator_binder(&part)?;
            parts.push(part);
        }
        Ok(self.ast.add_matcher(Matcher::And(parts), pos))
    }

    /// A bare name binds the whole value, so as an `&`/`|` operand it is pointless and usually a
    /// mistaken type test.
    fn reject_combinator_binder(&self, id: &AstId<Matcher>) -> Result<(), anyhow::Error> {
        if let Matcher::Binder(sym) = self.ast.get(id) {
            let name = self.ast.text(*sym).to_string();
            let pos = self.ast.pos(id).clone();
            parse_error!(self, &pos, "a bare name cannot be an operand of `&` or `|`; write `is {name}` or `has {name}` to test its type, or `{name} @ ...` to bind it");
        }
        Ok(())
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
            TokenType::Minus | TokenType::NumericLiteral | TokenType::StringLiteral
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
            return Err(self.is_needs_type_error(&pos));
        }
        self.parse_typed_matcher(true, pos)
    }

    /// The error for an `is` with no type name. A literal operand reads as a value comparison; any
    /// other operand reads as a missed structural test.
    pub(super) fn is_needs_type_error(&self, pos: &SourcePosition) -> anyhow::Error {
        let msg = if matches!(self.tokens.peek(0).kind,
            TokenType::NumericLiteral | TokenType::StringLiteral
            | TokenType::True | TokenType::False | TokenType::Null) {
            "`is` needs a type name. Use `==` to compare values."
        } else {
            "`is` needs a type name. Did you mean `is T { ... }`? For a structural test, write `{ ... }` without `is`."
        };
        self.error(msg, pos)
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
        let shape = if self.opens_matcher_shape() {
            Some(self.parse_shape_matcher()?)
        } else {
            None
        };
        Ok(self.ast.add_matcher(Matcher::Type { nominal, name, shape }, pos))
    }

    /// Whether a `{` after `is T`/`has T` opens a destructuring shape rather than an `if`/`while` body.
    fn opens_matcher_shape(&self) -> bool {
        if self.tokens.peek(0).kind != TokenType::LeftBrace {
            return false;
        }
        match self.tokens.peek(1).kind {
            TokenType::Identifier => matches!(self.tokens.peek(2).kind,
                TokenType::Colon | TokenType::Comma | TokenType::RightBrace),
            TokenType::NumericLiteral | TokenType::StringLiteral
            | TokenType::True | TokenType::False | TokenType::Null => self.tokens.peek(2).kind == TokenType::Colon,
            _ => false,
        }
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
            TokenType::Minus | TokenType::StringLiteral | TokenType::NumericLiteral
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

    /// A `~` publishes its binders, so a binding that always matches is just an unconditional
    /// assignment wearing a test. Reject that. A non-binding matcher that always matches is only a
    /// constant boolean, no different from `if true`, so it is allowed.
    pub(super) fn validate_match_operator_operand(&self, id: &AstId<Matcher>) -> Result<(), anyhow::Error> {
        let matcher = self.ast.get(id);
        if self.binds_any(matcher) && self.always_matches(matcher) {
            let pos = self.ast.pos(id).clone();
            parse_error!(self, &pos, "`~` binding must be fallible; `{}` always matches.", self.matcher_source(id));
        }
        Ok(())
    }

    /// Renders a matcher back to its source form for diagnostics. A `{ x }` shorthand reads back as
    /// its `{ x: x }` desugaring.
    fn matcher_source(&self, id: &AstId<Matcher>) -> String {
        match self.ast.get(id) {
            Matcher::Wildcard => "_".to_string(),
            Matcher::Literal(scalar) => match_scalar_source(scalar),
            Matcher::Binder(name) => self.ast.text(*name).to_string(),
            Matcher::As(name, inner) => format!("{} @ {}", self.ast.text(*name), self.matcher_source(inner)),
            Matcher::Type { nominal, name, shape } => {
                let keyword = if *nominal { "is" } else { "has" };
                match shape {
                    Some(shape) => format!("{keyword} {} {}", self.ast.text(*name), self.matcher_source(shape)),
                    None => format!("{keyword} {}", self.ast.text(*name)),
                }
            },
            Matcher::Shape(fields) => {
                let parts: Vec<String> = fields.iter()
                    .map(|f| format!("{}: {}", match_scalar_source(&f.key), self.matcher_source(&f.value)))
                    .collect();
                format!("{{ {} }}", parts.join(", "))
            },
            Matcher::Array(elements) => {
                let parts: Vec<String> = elements.iter().map(|e| match e {
                    MatchElem::Elem(m) => self.matcher_source(m),
                    MatchElem::Rest(Some(name)) => format!("..{}", self.ast.text(*name)),
                    MatchElem::Rest(None) => "..".to_string(),
                }).collect();
                format!("[{}]", parts.join(", "))
            },
            Matcher::Or(alternatives) => self.join_matchers(alternatives, " | "),
            Matcher::And(parts) => self.join_matchers(parts, " & "),
        }
    }

    fn join_matchers(&self, ids: &[AstId<Matcher>], sep: &str) -> String {
        ids.iter().map(|id| self.matcher_source(id)).collect::<Vec<_>>().join(sep)
    }

    /// Whether a matcher matches every value. A bare binder and `_` always match. An `&` does when
    /// all parts do, an `|` when any alternative does, and an `@` when its inner does.
    fn always_matches(&self, matcher: &Matcher) -> bool {
        match matcher {
            Matcher::Wildcard | Matcher::Binder(_) => true,
            Matcher::Literal(_) | Matcher::Type { .. } | Matcher::Shape(_) | Matcher::Array(_) => false,
            Matcher::As(_, inner) => self.always_matches(self.ast.get(inner)),
            Matcher::And(parts) => parts.iter().all(|p| self.always_matches(self.ast.get(p))),
            Matcher::Or(alternatives) => alternatives.iter().any(|a| self.always_matches(self.ast.get(a))),
        }
    }

    /// Whether a matcher introduces any binder.
    fn binds_any(&self, matcher: &Matcher) -> bool {
        match matcher {
            Matcher::Wildcard | Matcher::Literal(_) => false,
            Matcher::Binder(_) | Matcher::As(..) => true,
            Matcher::Type { shape, .. } => shape.as_ref().is_some_and(|s| self.binds_any(self.ast.get(s))),
            Matcher::Shape(fields) => fields.iter().any(|f| self.binds_any(self.ast.get(&f.value))),
            Matcher::Array(elements) => elements.iter().any(|e| match e {
                MatchElem::Elem(m) => self.binds_any(self.ast.get(m)),
                MatchElem::Rest(name) => name.is_some(),
            }),
            Matcher::And(parts) => parts.iter().any(|p| self.binds_any(self.ast.get(p))),
            Matcher::Or(alternatives) => alternatives.iter().any(|a| self.binds_any(self.ast.get(a))),
        }
    }

    /// A scalar literal: a matcher equality value or a shape key. A leading `-` negates a number,
    /// since the lexer has no signed-number token.
    pub(super) fn parse_match_scalar(&mut self) -> Result<MatchScalar, anyhow::Error> {
        let negate = self.tokens.next_if(TokenType::Minus).is_some();
        let token = self.tokens.next().clone();
        let pos = token.pos.clone();
        if negate && token.kind != TokenType::NumericLiteral {
            parse_error!(self, &pos, "Expected a number after '-'");
        }
        Ok(match token.kind {
            TokenType::NumericLiteral => {
                let n: f64 = token.lexeme.parse().unwrap();
                MatchScalar::Number(if negate { -n } else { n })
            },
            TokenType::StringLiteral => MatchScalar::String(String::from(&token.lexeme[1..token.lexeme.len() - 1])),
            TokenType::True => MatchScalar::Boolean(true),
            TokenType::False => MatchScalar::Boolean(false),
            TokenType::Null => MatchScalar::Null,
            _ => parse_error!(self, &pos, "Expected a literal"),
        })
    }
}

/// Renders a scalar back to its source form for diagnostics.
fn match_scalar_source(scalar: &MatchScalar) -> String {
    match scalar {
        MatchScalar::String(s) => format!("\"{s}\""),
        MatchScalar::Number(n) => format!("{n}"),
        MatchScalar::Boolean(b) => format!("{b}"),
        MatchScalar::Null => "null".to_string(),
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

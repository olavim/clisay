//! Pattern matching: the `match` statement, the `<-` match-bind, and the matcher grammar.

use super::*;

impl<'parser, 'vm> Parser<'parser, 'vm> {
    /// match_stmt := "match" expr "{" arm ("," arm)* ","? "}"
    pub(super) fn parse_match(&mut self) -> Result<AstId<Stmt>, anyhow::Error> {
        let pos = self.tokens.expect(TokenType::Match)?.pos.clone();
        let scrutinee = self.with_ctx(ExprCtx::scrutinee(), |p| p.parse_expr_precedence(0))?;
        self.tokens.expect(TokenType::LeftBrace)?;

        let mut arms: Vec<Arm> = Vec::new();
        while !self.tokens.matches(TokenType::RightBrace) {
            arms.push(self.parse_match_arm()?);
            if self.tokens.next_if(TokenType::Comma).is_none() {
                break;
            }
        }

        if arms.is_empty() {
            parse_error!(self, &pos, "A `match` needs at least one arm");
        }

        self.tokens.expect(TokenType::RightBrace)?;
        Ok(self.ast.add_stmt(Stmt::Match(scrutinee, arms), pos))
    }

    /// arm := matcher ("if" guard)? "=>" body
    pub(super) fn parse_match_arm(&mut self) -> Result<Arm, anyhow::Error> {
        let matcher = self.parse_matcher()?;
        let guard = match self.tokens.next_if(TokenType::If) {
            Some(_) => Some(self.parse_guard()?),
            None => None,
        };
        self.tokens.expect(TokenType::FatArrow)?;
        let body = self.parse_arm_body()?;
        Ok(Arm { matcher, guard, body })
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

    /// Parses a guard expression, stopping before the arm's `=>`. A guard is a condition
    /// context, so a `<-` match-bind may appear in it.
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

    /// Whether the tokens at the cursor begin a match-bind: a depth-zero `<-` reached through
    /// only matcher tokens and balanced groups. A non-matcher token at depth zero ends the scan.
    pub(super) fn scan_is_match_bind(&self) -> bool {
        let mut i = 0;
        let mut depth = 0u32;
        loop {
            let kind = self.tokens.peek(i).kind;
            match kind {
                TokenType::EOF => return false,
                TokenType::LeftArrow if depth == 0 => return true,
                TokenType::LeftParen | TokenType::LeftBracket | TokenType::LeftBrace => depth += 1,
                TokenType::RightParen | TokenType::RightBracket | TokenType::RightBrace => {
                    if depth == 0 { return false; }
                    depth -= 1;
                },
                _ if depth == 0 && !Self::is_matcher_token(kind) => return false,
                _ => {},
            }
            i += 1;
        }
    }

    /// Whether a token can appear in a matcher at nesting depth zero, before its `<-`. The
    /// combinators are the single `|`/`&`. The doubled `||`/`&&` are boundaries.
    pub(super) fn is_matcher_token(kind: TokenType) -> bool {
        matches!(kind,
            TokenType::Identifier
            | TokenType::NumericLiteral | TokenType::StringLiteral
            | TokenType::True | TokenType::False | TokenType::Null
            | TokenType::Is | TokenType::Has
            | TokenType::At | TokenType::Pipe | TokenType::Amp | TokenType::DotDot)
    }

    /// MATCHER "<-" expr. The scrutinee parses tighter than the comparison and boolean operators.
    /// So `{ a } <- x ?? y` is `({ a } <- x) ?? y`, and `{} <- x && rest` is `({} <- x) && rest`.
    pub(super) fn parse_match_bind(&mut self) -> Result<AstId<Expr>, anyhow::Error> {
        let pos = self.tokens.peek(0).pos.clone();
        let matcher = self.parse_matcher()?;
        self.validate_top_operand(matcher)?;
        self.tokens.expect(TokenType::LeftArrow)?;
        let scrutinee_precedence = Operator::Is.infix_precedence().unwrap() + 1;
        let scrutinee = self.parse_expr_precedence(scrutinee_precedence)?;
        if self.tokens.matches(TokenType::LeftArrow) {
            let arrow_pos = self.tokens.peek(0).pos.clone();
            parse_error!(self, &arrow_pos, "`<-` is non-associative; parenthesize the scrutinee to nest a match-bind");
        }
        Ok(self.ast.add_expr(Expr::MatchBind(matcher, scrutinee), pos))
    }

    /// Rejects a top-level `<-` operand that tests nothing useful: a bare binder or a pure-value
    /// matcher. A structural/type/array test or any contained binder is allowed.
    pub(super) fn validate_top_operand(&self, matcher: AstId<Matcher>) -> Result<(), anyhow::Error> {
        let pos = self.ast.pos(&matcher).clone();
        match self.ast.get(&matcher) {
            Matcher::Binder(name) => {
                let name = self.ast.text(*name).to_string();
                parse_error!(self, &pos, "a bare name on the left of `<-` only binds it; write `is {name}` to test its type, or put a space between `<` and `-` for a comparison `{name} < -...`");
            },
            _ if self.is_pure_value(matcher) => {
                parse_error!(self, &pos, "the left of `<-` tests a value but binds nothing; use `==` for an equality test or a shape like `{{ k: _ }}`");
            },
            _ => Ok(()),
        }
    }

    /// Whether a matcher only compares a value or nothing.
    pub(super) fn is_pure_value(&self, matcher: AstId<Matcher>) -> bool {
        match self.ast.get(&matcher) {
            Matcher::Wildcard | Matcher::Literal(_) => true,
            Matcher::Or(alts) => alts.iter().all(|a| self.is_pure_value(*a)),
            _ => false,
        }
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
        if !self.tokens.matches_single(TokenType::Pipe) {
            return Ok(first);
        }
        let mut alternatives = vec![first];
        while self.tokens.next_if_single(TokenType::Pipe).is_some() {
            alternatives.push(self.parse_and_matcher()?);
        }
        Ok(self.ast.add_matcher(Matcher::Or(alternatives), pos))
    }

    /// and_matcher := primary ("&" primary)*
    pub(super) fn parse_and_matcher(&mut self) -> Result<AstId<Matcher>, anyhow::Error> {
        let pos = self.tokens.peek(0).pos.clone();
        let first = self.parse_primary_matcher()?;
        if !self.tokens.matches_single(TokenType::Amp) {
            return Ok(first);
        }
        let mut parts = vec![first];
        while self.tokens.next_if_single(TokenType::Amp).is_some() {
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
                let inner = self.parse_matcher()?;
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

    /// `has` type_ref shape? or `has` shape.
    pub(super) fn parse_has_matcher(&mut self) -> Result<AstId<Matcher>, anyhow::Error> {
        let pos = self.tokens.next().pos.clone();
        if self.tokens.peek(0).kind == TokenType::LeftBrace {
            return self.parse_shape_matcher();
        }
        if self.tokens.peek(0).kind != TokenType::Identifier {
            parse_error!(self, &pos, "`has` needs a type name or a shape");
        }
        self.parse_typed_matcher(false, pos)
    }

    /// A type test `T shape?` after its `is`/`has` keyword.
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

    /// shape := "{" ( field ("," field)* ","? )? "}"
    pub(super) fn parse_shape_matcher(&mut self) -> Result<AstId<Matcher>, anyhow::Error> {
        let pos = self.tokens.expect(TokenType::LeftBrace)?.pos.clone();
        let mut fields: Vec<MatchField> = Vec::new();
        let mut seen: HashSet<String> = HashSet::new();

        while !self.tokens.matches(TokenType::RightBrace) {
            let field = self.parse_shape_field()?;
            let dedup = match &field.key {
                MatchScalar::String(s) => format!("s:{s}"),
                MatchScalar::Number(n) => format!("n:{n}"),
                MatchScalar::Boolean(b) => format!("b:{b}"),
                MatchScalar::Null => String::from("null"),
            };
            if !seen.insert(dedup) {
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
                let matcher = self.parse_matcher()?;
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

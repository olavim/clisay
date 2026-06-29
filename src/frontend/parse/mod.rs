//! Recursive-descent parser. Turns tokens into an AST.

use std::collections::HashSet;

use anyhow::anyhow;

use crate::ast::{Arm, Ast, AstId, CatchClause, TypeDecl, Expr, FieldInit, FnDecl, Literal, MatchElem, MatchField, MatchScalar, Matcher, Operator, Param, ReturnShape, Stmt, Symbol};
use crate::frontend::lex::{ContextualKeyword, SourcePosition, TokenStream, TokenType};

macro_rules! parse_error {
    ($self:ident, $pos:expr, $($arg:tt)*) => { return Err($self.error(format!($($arg)*), $pos)) };
}

/// A member's declared visibility (from a `pub`/`inner` modifier, or private by default).
#[derive(Clone, Copy, PartialEq, Eq)]
enum Visibility { Pub, Inner, Private }

/// The modes that decide how the current expression position is parsed.
#[derive(Clone, Copy, Default)]
struct ExprCtx {
    prevent_construct: bool,
    stop_at_arrow: bool,
    /// A `<-` match-bind may begin here.
    allow_matchbind: bool,
}

impl ExprCtx {
    /// An `if`/`while` head: a trailing `{` is the body, and a `<-` match-bind may begin.
    fn condition() -> ExprCtx {
        ExprCtx { prevent_construct: true, allow_matchbind: true, ..Default::default() }
    }

    /// A `match` scrutinee: a trailing `{` opens the match body, not a construction.
    fn scrutinee() -> ExprCtx {
        ExprCtx { prevent_construct: true, ..Default::default() }
    }

    /// A `match` arm guard: a `=>` ends it, and a `<-` match-bind may begin.
    fn guard() -> ExprCtx {
        ExprCtx { stop_at_arrow: true, allow_matchbind: true, ..Default::default() }
    }

    /// A matcher interior, where a `{` after a type is a destructuring shape, not a body block.
    fn matcher() -> ExprCtx {
        ExprCtx { prevent_construct: false, ..Default::default() }
    }

    /// A construction body derived from `outer`: nested braces construct, other modes carry over.
    fn construct(outer: ExprCtx) -> ExprCtx {
        ExprCtx { prevent_construct: false, ..outer }
    }

    /// An `&&`/`||` operand derived from `outer`: a `<-` match-bind may begin, other modes carry over.
    fn cond_operand(outer: ExprCtx) -> ExprCtx {
        ExprCtx { allow_matchbind: true, ..outer }
    }

    /// Whether a `{` after a constructible expression starts a brace construction.
    fn can_construct(&self) -> bool { !self.prevent_construct }
    
    /// Whether a `=>` ends the current expression instead of starting a lambda.
    fn stops_at_arrow(&self) -> bool { self.stop_at_arrow }

    /// Takes the match-bind permission, clearing it. A match-bind is recognized only at a leading
    /// position, so the permission is consumed once per climb.
    fn take_matchbind(&mut self) -> bool {
        std::mem::replace(&mut self.allow_matchbind, false)
    }
}

pub struct Parser<'parser, 'vm> {
    tokens: &'vm mut TokenStream<'vm>,
    ast: &'parser mut Ast,
    /// The type whose body is being parsed, used to name its `init`.
    current_type: Option<String>,
    /// The modes governing how the current expression position is parsed.
    ctx: ExprCtx,
}

mod statements;
mod functions;
mod types;
mod expressions;
mod matchers;

impl<'parser, 'vm> Parser<'parser, 'vm> {
    pub fn parse(tokens: &'vm mut TokenStream<'vm>) -> Result<Ast, anyhow::Error> {
        let mut ast = Ast::new();

        let mut parser = Parser {
            tokens,
            ast: &mut ast,
            current_type: None,
            ctx: ExprCtx::default(),
        };

        let pos = parser.tokens.peek(0).pos.clone();
        let mut stmts: Vec<AstId<Stmt>> = Vec::new();
        while parser.tokens.has_next() {
            stmts.push(parser.parse_stmt()?);
        }
        let block = parser.ast.add_expr(Expr::Block(stmts), pos.clone());
        ast.add_stmt(Stmt::Expression(block), pos);

        Ok(ast)
    }

    fn error(&self, message: impl Into<String>, pos: &SourcePosition) -> anyhow::Error {
        anyhow!(format!("{}\nat {}", message.into(), pos))
    }

    /// Consumes a leading `mut` modifier if present, reporting whether it was there.
    fn parse_mut(&mut self) -> bool {
        if self.tokens.peek(0).contextual() == Some(ContextualKeyword::Mut) {
            self.tokens.next();
            return true;
        }
        false
    }

    /// Consumes an optional trailing `?` nullability marker.
    fn parse_nullable(&mut self) -> bool {
        self.tokens.next_if(TokenType::Question).is_some()
    }

    /// Parses a return shape marker after a parameter list: `!` non-null, `?` nullable,
    /// or nothing for void.
    fn parse_return_shape(&mut self) -> ReturnShape {
        if self.tokens.next_if(TokenType::Exclamation).is_some() {
            ReturnShape::NonNull
        } else if self.tokens.next_if(TokenType::Question).is_some() {
            ReturnShape::Nullable
        } else {
            ReturnShape::Void
        }
    }

    fn parse_identifier(&mut self) -> Result<String, anyhow::Error> {
        let token = self.tokens.expect(TokenType::Identifier)?;
        Ok(token.lexeme.clone())
    }

    fn parse_identifier_expr(&mut self) -> Result<AstId<Expr>, anyhow::Error> {
        let token = self.tokens.expect(TokenType::Identifier)?;
        let pos = token.pos.clone();
        let name = self.ast.intern(&token.lexeme);
        Ok(self.ast.add_expr(Expr::Identifier(name), pos))
    }

    /// Runs `f` with the expression context replaced, restoring the previous context after.
    /// A fresh nested parse starts from whatever modes `ctx` names and nothing else.
    fn with_ctx<R>(&mut self, ctx: ExprCtx, f: impl FnOnce(&mut Self) -> R) -> R {
        let prev = std::mem::replace(&mut self.ctx, ctx);
        let result = f(self);
        self.ctx = prev;
        result
    }
}

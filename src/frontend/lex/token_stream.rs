use anyhow::bail;

use crate::frontend::lex::{Diagnostic, SourcePosition, Token, TokenType};

type TokenResult<T> = Result<T, anyhow::Error>;

pub struct TokenStream<'a> {
    tokens: &'a Vec<Token>,
    pos: usize
}

impl<'a> TokenStream<'a> {
    pub fn new(tokens: &'a Vec<Token>) -> TokenStream<'a> {
        return TokenStream { tokens, pos: 0 };
    }

    pub fn peek(&self, look_ahead: usize) -> &'a Token {
        if self.pos + look_ahead >= self.tokens.len() {
            return self.tokens.last().unwrap();
        }

        return self.tokens.get(self.pos + look_ahead).unwrap();
    }

    pub fn matches(&self, token_type: TokenType) -> bool {
        return self.peek(0).kind == token_type;
    }

    pub fn next(&mut self) -> &'a Token {
        if self.pos >= self.tokens.len() {
            return self.tokens.last().unwrap();
        }

        let token = self.tokens.get(self.pos);
        self.pos += 1;
        return token.unwrap();
    }

    pub fn next_if(&mut self, token_type: TokenType) -> Option<&'a Token> {
        if self.matches(token_type) {
            return Some(self.next());
        }

        return None;
    }

    pub fn has_next(&self) -> bool {
        return self.pos < self.tokens.len() && self.peek(0).kind != TokenType::EOF;
    }

    pub fn expect(&mut self, token_type: TokenType) -> TokenResult<&'a Token> {
        let token = self.next();
        if token.kind == token_type {
            Ok(token)
        } else {
            // Name the found token by its source text. EOF has none, so fall back to its kind.
            let found = if token.kind == TokenType::EOF { token.kind.to_string() } else { token.pos.snippet().to_string() };
            let message = format!("Unexpected token: Expected '{token_type}' but found '{found}'");
            let diag = Diagnostic::new(message, token.pos.clone()).with_label(format!("expected '{token_type}'"));
            bail!("{diag}")
        }
    }

    pub fn expect_close(&mut self, close: TokenType, open_pos: &SourcePosition) -> TokenResult<&'a Token> {
        let token = self.next();
        if token.kind == close {
            return Ok(token)
        }
        
        let open_kind = match close {
            TokenType::RightBracket => TokenType::LeftBracket,
            TokenType::RightBrace => TokenType::LeftBrace,
            _ => TokenType::LeftParen,
        };
        let found = if token.kind == TokenType::EOF { token.kind.to_string() } else { token.pos.snippet().to_string() };
        let message = format!("Unexpected token: Expected '{close}' but found '{found}'");
        let diag = Diagnostic::new(message, token.pos.clone())
            .with_label(format!("expected '{close}'"))
            .with_opener(open_pos.clone(), format!("unclosed '{open_kind}'"));
        bail!("{diag}")
    }

    pub fn advance(&mut self, count: usize) {
        self.pos += count;
    }

    pub fn back(&mut self) {
        self.pos = self.pos.saturating_sub(1);
    }

    pub fn previous(&self) -> &'a Token {
        return self.tokens.get(self.pos.saturating_sub(1)).unwrap();
    }
}
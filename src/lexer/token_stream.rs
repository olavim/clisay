use anyhow::bail;

use crate::lexer::{Token, TokenType};

type TokenResult<T> = Result<T, anyhow::Error>;

pub struct TokenStream<'a> {
    tokens: &'a Vec<Token>,
    pos: usize
}

impl<'a> TokenStream<'a> {
    pub fn new(tokens: &Vec<Token>) -> TokenStream {
        return TokenStream { tokens, pos: 0 };
    }

    pub fn peek(&self, look_ahead: usize) -> &'a Token {
        if self.pos + look_ahead >= self.tokens.len() {
            return self.tokens.last().unwrap();
        }

        return self.tokens.get(self.pos + look_ahead).unwrap();
    }

    pub fn match_next(&self, token_type: TokenType) -> bool {
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
        if self.peek(0).kind == token_type {
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
            bail!("Unexpected token: Expected '{token_type}' but found '{token}'\nat {}", token.pos)
        }
    }

    pub fn advance(&mut self, count: usize) {
        self.pos += count;
    }
}
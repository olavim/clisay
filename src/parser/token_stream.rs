use anyhow::bail;

use crate::lexer::{Token, TokenType};

type TokenResult<T> = Result<T, anyhow::Error>;

pub struct TokenStream<'a> {
    tokens: &'a Vec<Token>,
    pos: usize,
    saved_pos: usize
}

impl TokenStream<'_> {
    pub fn new(tokens: &Vec<Token>) -> TokenStream {
        return TokenStream { tokens, pos: 0, saved_pos: 0 };
    }

    pub fn peek(&self, look_ahead: usize) -> TokenResult<&Token> {
        if self.pos + look_ahead >= self.tokens.len() {
            bail!("Unexpected end of file");
        }

        return Ok(self.tokens.get(self.pos + look_ahead).unwrap());
    }

    pub fn peek_type(&self, look_ahead: usize) -> TokenResult<&TokenType> {
        return Ok(&self.peek(look_ahead)?.kind);
    }

    pub fn next(&mut self) -> TokenResult<&Token> {
        if self.pos >= self.tokens.len() {
            bail!("Unexpected end of file");
        }

        let token = self.tokens.get(self.pos);
        self.pos += 1;
        return Ok(token.unwrap());
    }

    pub fn back(&mut self) {
        self.pos -= 1;
    }

    pub fn next_if(&mut self, token_type: TokenType) -> TokenResult<Option<&Token>> {
        if self.peek(0)?.kind == token_type {
            return Ok(Some(self.next()?));
        }

        return Ok(None);
    }

    pub fn expect(&mut self, token_type: TokenType) -> TokenResult<&Token> {
        let token = self.next()?;

        return if token.kind == token_type {
            Ok(token)
        } else {
            bail!("Unexpected token {} at {}: Expected {} but got {}", token, token.pos, token_type, token)
        }
    }

    pub fn save_position(&mut self) {
        self.saved_pos = self.pos;
    }

    pub fn restore_position(&mut self) {
        self.pos = self.saved_pos;
    }
}
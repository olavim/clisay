mod token;
mod token_stream;

use std::fmt;

use anyhow::bail;
use regex::Regex;
pub use token::{Token, TokenType};
pub use token_stream::TokenStream;

const REGEX_STRING: &str = r#""([^"\\]|\\.)*""#;
const REGEX_NUMERIC: &str = r"(0|[1-9][0-9]*)(\.[0-9]+)?([eE][+-]?[0-9]+)?";
const REGEX_ALPHANUMERIC: &str = r"[a-zA-Z_][a-zA-Z0-9_]*";
const REGEX_COMMENT: &str = r"\/\/[^\n\r]*";
const REGEX_NEWLINE: &str = r"(\r\n|\r|\n)";
const REGEX_WHITESPACE: &str = r"[^\S\r\n]+";

#[derive(Clone)]
pub struct SourcePosition {
    pub file: String,
    pub line: usize,
    pub column: usize
}

impl fmt::Display for SourcePosition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        return write!(f, "line {}, column {}", self.line, self.column);
    }
}

fn find_at<'a>(regex: &str, input: &'a str, pos: usize) -> Option<usize> {
    return match Regex::new(regex).unwrap().find(&input[pos..]) {
        Some(mat) if mat.start() == 0 => Option::from(pos + mat.len()),
        _ => None
    };
}

fn next_token(input: &str, input_index: usize) -> Result<Token, anyhow::Error> {
    if input_index >= input.len() {
        return Ok(Token::new(TokenType::EOF, ""));
    }
    
    if let Some(end) = find_at(REGEX_COMMENT, input, input_index) {
        return Ok(Token::new(TokenType::Comment, &input[input_index..end]));
    }
    
    if let Some(end) = find_at(REGEX_WHITESPACE, input, input_index) {
        return Ok(Token::new(TokenType::Whitespace, &input[input_index..end]));
    }
    
    if let Some(end) = find_at(REGEX_NEWLINE, input, input_index) {
        return Ok(Token::new(TokenType::Newline, &input[input_index..end]));
    }
    
    if let Some(end) = find_at(REGEX_ALPHANUMERIC, input, input_index) {
        return Ok(Token::from_alphanumeric(&input[input_index..end]));
    }
    
    if let Some(end) = find_at(REGEX_NUMERIC, input, input_index) {
        return Ok(Token::new(TokenType::NumericLiteral, &input[input_index..end]));
    }
    
    if let Some(end) = find_at(REGEX_STRING, input, input_index) {
        return Ok(Token::new(TokenType::StringLiteral, &input[input_index..end]));
    }

    let end = input_index + 1;
    let substr = &input[input_index..end];

    if let Some(token) = Token::from_punctuation(substr) {
        return Ok(token);
    }

    bail!("Unexpected character `{}` at pos {}", substr, input_index);
}

pub fn tokenize(file_name: String, input: String) -> Result<Vec<Token>, anyhow::Error> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut input_index = 0;
    let mut line = 1;
    let mut column = 1;

    while tokens.last().map_or(true, |t| t.kind != TokenType::EOF) {
        let mut token = next_token(&input, input_index)?;
        token.pos = SourcePosition { file: file_name.clone(), line, column };
        input_index += token.lexeme.len();
        column += token.lexeme.len();

        if token.kind == TokenType::Newline {
            line += 1;
            column = 1;
        }
        
        if !matches!(token.kind, TokenType::Whitespace | TokenType::Comment | TokenType::Newline) {
            tokens.push(token);
        }
    }

    return Ok(tokens);
}
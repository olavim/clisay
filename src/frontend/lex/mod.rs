mod token;
mod token_stream;

use std::fmt;
use std::rc::Rc;
use std::sync::LazyLock;

use anyhow::bail;
use regex::Regex;
pub use token::{ContextualKeyword, Token, TokenType};
pub use token_stream::TokenStream;

// Compile token patterns once on first use.
static REGEX_STRING: LazyLock<Regex> = LazyLock::new(|| Regex::new(r#""([^"\\]|\\.)*""#).unwrap());
static REGEX_NUMERIC: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"(0|[1-9][0-9]*)(\.[0-9]+)?([eE][+-]?[0-9]+)?").unwrap());
static REGEX_ALPHANUMERIC: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"[a-zA-Z_][a-zA-Z0-9_]*").unwrap());
static REGEX_COMMENT: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"\/\/[^\n\r]*").unwrap());
static REGEX_NEWLINE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"(\r\n|\r|\n)").unwrap());
static REGEX_WHITESPACE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"[^\S\r\n]+").unwrap());

pub struct SourceFile {
    pub name: String,
    pub content: Rc<str>
}

#[derive(Clone)]
pub struct SourcePosition {
    pub source: Rc<SourceFile>,
    pub start: usize,
    pub end: usize,
    pub line: usize
}

impl SourcePosition {
    pub fn snippet(&self) -> &str {
        return &self.source.content[self.start..self.end];
    }
}

impl fmt::Display for SourcePosition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        return write!(f, "{}:{}", self.source.name, self.line);
    }
}

fn find_at(regex: &Regex, input: &str, pos: usize) -> Option<usize> {
    return match regex.find(&input[pos..]) {
        Some(mat) if mat.start() == 0 => Option::from(pos + mat.len()),
        _ => None
    };
}

fn next_token(input: &str, input_index: usize, pos: &SourcePosition) -> Result<Token, anyhow::Error> {
    if input_index >= input.len() {
        return Ok(Token::new(TokenType::EOF, ""));
    }
    
    if let Some(end) = find_at(&REGEX_COMMENT, input, input_index) {
        return Ok(Token::new(TokenType::Comment, &input[input_index..end]));
    }
    
    if let Some(end) = find_at(&REGEX_WHITESPACE, input, input_index) {
        return Ok(Token::new(TokenType::Whitespace, &input[input_index..end]));
    }
    
    if let Some(end) = find_at(&REGEX_NEWLINE, input, input_index) {
        return Ok(Token::new(TokenType::Newline, &input[input_index..end]));
    }
    
    if let Some(end) = find_at(&REGEX_ALPHANUMERIC, input, input_index) {
        return Ok(Token::from_alphanumeric(&input[input_index..end]));
    }
    
    if let Some(end) = find_at(&REGEX_NUMERIC, input, input_index) {
        return Ok(Token::new(TokenType::NumericLiteral, &input[input_index..end]));
    }
    
    if let Some(end) = find_at(&REGEX_STRING, input, input_index) {
        return Ok(Token::new(TokenType::StringLiteral, &input[input_index..end]));
    }

    // Match the longest punctuation operator first, so `<<=` beats `<<` beats `<`.
    for width in (1..=3).rev() {
        if let Some(substr) = input.get(input_index..input_index + width) {
            if let Some(token) = Token::from_punctuation(substr) {
                return Ok(token);
            }
        }
    }

    let next = input[input_index..].chars().next().unwrap();
    bail!("Unexpected character `{}`\n\tat {}", next, pos);
}

pub fn tokenize(file_name: String, input: String) -> Result<Vec<Token>, anyhow::Error> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut input_index = 0;
    let mut line = 1;
    let source = Rc::new(SourceFile { name: file_name, content: Rc::from(input.as_str()) });

    while tokens.last().map_or(true, |t| t.kind != TokenType::EOF) {
        let mut pos = SourcePosition { source: source.clone(), start: input_index, end: input_index, line };
        let mut token = next_token(&source.content, input_index, &pos)?;
        pos.end = input_index + token.lexeme.len();
        input_index = pos.end;
        token.pos = pos;

        if token.kind == TokenType::Newline {
            line += 1;
        }
        
        if !matches!(token.kind, TokenType::Whitespace | TokenType::Comment | TokenType::Newline) {
            tokens.push(token);
        }
    }

    return Ok(tokens);
}
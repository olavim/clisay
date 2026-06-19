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

#[derive(Clone)]
pub struct SourcePosition {
    pub file: Rc<str>,
    pub line: usize
}

impl fmt::Display for SourcePosition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        return write!(f, "{}:{}", self.file, self.line);
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

    let end = input_index + 1;
    let substr = &input[input_index..end];

    if let Some(token) = Token::from_punctuation(substr) {
        return Ok(token);
    }

    bail!("Unexpected character `{}`\n\tat {}", substr, pos);
}

pub fn tokenize(file_name: String, input: String) -> Result<Vec<Token>, anyhow::Error> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut input_index = 0;
    let mut line = 1;
    let file: Rc<str> = Rc::from(file_name);

    while tokens.last().map_or(true, |t| t.kind != TokenType::EOF) {
        let pos = SourcePosition { file: file.clone(), line };
        let mut token = next_token(&input, input_index, &pos)?;
        token.pos = pos;
        input_index += token.lexeme.len();

        if token.kind == TokenType::Newline {
            line += 1;
        }
        
        if !matches!(token.kind, TokenType::Whitespace | TokenType::Comment | TokenType::Newline) {
            tokens.push(token);
        }
    }

    return Ok(tokens);
}

#[cfg(test)]
mod tests {
    use super::*;

    fn kinds(src: &str) -> Vec<TokenType> {
        tokenize(String::new(), src.to_string())
            .unwrap()
            .iter()
            .map(|t| t.kind)
            .filter(|k| *k != TokenType::EOF)
            .collect()
    }

    #[test]
    fn lexes_question_mark() {
        // `?` is a single-char token; the multi-char `??`/`?.`/`?[` forms are
        // assembled in the parser from adjacent tokens, like `&&` and `==`.
        assert_eq!(kinds("?"), vec![TokenType::Question]);
        assert_eq!(kinds("??"), vec![TokenType::Question, TokenType::Question]);
        assert_eq!(kinds("?."), vec![TokenType::Question, TokenType::Dot]);
        assert_eq!(kinds("?["), vec![TokenType::Question, TokenType::LeftBracket]);
    }

    #[test]
    fn lexes_exclamation() {
        assert_eq!(kinds("!"), vec![TokenType::Exclamation]);
    }

    #[test]
    fn mut_is_a_contextual_keyword() {
        // `mut` is a modifier in declaration position, like `pub`/`inner`, so it
        // lexes as an identifier and is recognized contextually.
        let toks = tokenize(String::new(), "mut".to_string()).unwrap();
        assert_eq!(toks[0].kind, TokenType::Identifier);
        assert_eq!(toks[0].contextual(), Some(ContextualKeyword::Mut));
    }
}
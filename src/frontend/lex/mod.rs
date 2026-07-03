mod diagnostic;
mod token;
mod token_stream;

use std::cell::Cell;
use std::fmt;
use std::rc::Rc;
use std::sync::LazyLock;

use anyhow::bail;
use regex::Regex;
pub use diagnostic::Diagnostic;
pub use token::{ContextualKeyword, Token, TokenType};
pub use token_stream::TokenStream;

thread_local! {
    /// Whether rendered diagnostics include ANSI color.
    static COLOR: Cell<bool> = const { Cell::new(false) };
}

/// Turns ANSI color in diagnostics on or off for the current thread.
pub fn enable_color(on: bool) {
    COLOR.with(|c| c.set(on));
}

/// Wraps `text` in an ANSI color code when color is on, otherwise returns it unchanged.
fn paint(text: &str, code: &str) -> String {
    if COLOR.with(Cell::get) {
        return format!("\x1b[{code}m{text}\x1b[0m");
    }
    return text.to_string();
}

/// A gutter cell: a right-aligned line number and its ` |` rail. A blank label makes an empty rail.
fn rail(label: &str, width: usize) -> String {
    return paint(&format!("{label:>width$} |"), COLOR_CYAN);
}

/// The `help:` notes shown under a frame, each on its own gutter-aligned line.
fn render_help(width: usize, help: &[String]) -> String {
    let prefix = format!("{} = help:", " ".repeat(width));
    let mut out = String::new();
    for note in help {
        out.push_str(&format!("\n{} {note}", paint(&prefix, COLOR_CYAN)));
    }
    return out;
}

// Compile token patterns once on first use.
static REGEX_STRING: LazyLock<Regex> = LazyLock::new(|| Regex::new(r#""([^"\\]|\\.)*""#).unwrap());
static REGEX_NUMERIC: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"(0|[1-9][0-9]*)(\.[0-9]+)?([eE][+-]?[0-9]+)?").unwrap());
static REGEX_ALPHANUMERIC: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"[a-zA-Z_][a-zA-Z0-9_]*").unwrap());
static REGEX_COMMENT: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"\/\/[^\n\r]*").unwrap());
static REGEX_NEWLINE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"(\r\n|\r|\n)").unwrap());
static REGEX_WHITESPACE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"[^\S\r\n]+").unwrap());

static COLOR_RED: &str = "31";
static COLOR_CYAN: &str = "36";

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

    pub fn column(&self) -> usize {
        return self.start - self.line_bounds(self.start).0 + 1;
    }

    /// The byte range of the line containing offset `at`, newlines excluded.
    fn line_bounds(&self, at: usize) -> (usize, usize) {
        let content = &self.source.content;
        let start = content[..at].rfind('\n').map_or(0, |i| i + 1);
        let end = content[at..].find('\n').map_or(content.len(), |i| at + i);
        return (start, end);
    }

    pub fn to(&self, end: &SourcePosition) -> SourcePosition {
        return SourcePosition { source: self.source.clone(), start: self.start, end: end.end, line: self.line };
    }

    pub fn render_snippet(&self, label: Option<&str>, help: &[String]) -> String {
        let content = &self.source.content;
        let line_start = content[..self.start].rfind('\n').map_or(0, |i| i + 1);
        let line_end = content[self.start..].find('\n').map_or(content.len(), |i| self.start + i);
        let line = &content[line_start..line_end];

        // Keep the run on this one line. Always show at least one caret.
        let caret_end = self.end.min(line_end);
        let pad = " ".repeat(self.start - line_start);
        let carets = paint(&"^".repeat(caret_end.saturating_sub(self.start).max(1)), COLOR_RED);
        let note = label.map_or(String::new(), |l| format!(" {}", paint(l, COLOR_RED)));

        // Show the line before for context, but a blank one is just noise, so skip it.
        let prev = (line_start > 0).then(|| {
            let prev_start = content[..line_start - 1].rfind('\n').map_or(0, |i| i + 1);
            (self.line - 1, &content[prev_start..line_start - 1])
        }).filter(|(_, text)| !text.trim().is_empty());

        let width = self.line.to_string().len();
        let bar = rail("", width);

        let mut out = format!("{bar}\n");
        if let Some((n, text)) = prev {
            out.push_str(&format!("{} {text}\n", rail(&n.to_string(), width)));
        }
        out.push_str(&format!("{} {line}\n{bar} {pad}{carets}{note}", rail(&self.line.to_string(), width)));
        out.push_str(&render_help(width, help));
        return out;
    }

    /// Renders the failure point plus the unclosed opener it belongs to.
    pub fn render_snippet_pair(&self, label: Option<&str>, opener: &SourcePosition, opener_label: &str, help: &[String]) -> String {
        let content = &self.source.content;
        let (p_start, p_end) = self.line_bounds(self.start);
        let p_col = self.start - p_start;
        let carets = "^".repeat(self.end.min(p_end).saturating_sub(self.start).max(1));
        let note = label.map_or(String::new(), |l| format!(" {}", paint(l, COLOR_RED)));
        let (o_start, o_end) = self.line_bounds(opener.start);
        let o_col = opener.start - o_start;
        let width = self.line.to_string().len();
        let bar = rail("", width);

        if self.line == opener.line {
            let line = &content[p_start..p_end];
            let lead = " ".repeat(o_col);
            let mid = " ".repeat(p_col.saturating_sub(o_col + 1));
            let ann = format!("{lead}{}{mid}{}", paint("^", COLOR_CYAN), paint(&carets, COLOR_RED));
            let stack = " ".repeat(o_col);
            let frame = format!("{bar}\n{num} {line}\n{bar} {ann}{note}\n{bar} {stack}{pipe}\n{bar} {stack}{olabel}",
                num = rail(&self.line.to_string(), width), pipe = paint("|", COLOR_CYAN), olabel = paint(opener_label, COLOR_CYAN));
            return frame + &render_help(width, help);
        }

        let o_line = &content[o_start..o_end];
        let p_line = &content[p_start..p_end];
        let frame = format!("{bar}\n{orail} {o_line}\n{bar} {o_pad}{ocaret} {olabel}\n{prail} {p_line}\n{bar} {p_pad}{pcarets}{note}",
            orail = rail(&opener.line.to_string(), width), prail = rail(&self.line.to_string(), width),
            o_pad = " ".repeat(o_col), p_pad = " ".repeat(p_col),
            ocaret = paint("^", COLOR_CYAN), olabel = paint(opener_label, COLOR_CYAN), pcarets = paint(&carets, COLOR_RED));
        return frame + &render_help(width, help);
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
    bail!("{}", Diagnostic::new(format!("Unexpected character `{next}`"), pos.clone()));
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
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

// Compile token patterns once on first use.
static REGEX_STRING: LazyLock<Regex> = LazyLock::new(|| Regex::new(r#""([^"\\]|\\.)*""#).unwrap());
static REGEX_NUMERIC: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"(0|[1-9][0-9]*)(\.[0-9]+)?([eE][+-]?[0-9]+)?").unwrap());
static REGEX_ALPHANUMERIC: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"[a-zA-Z_][a-zA-Z0-9_]*").unwrap());
static REGEX_STAR_MUT: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"\*mut\b").unwrap());
static REGEX_COMMENT: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"\/\/[^\n\r]*").unwrap());
static REGEX_NEWLINE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"(\r\n|\r|\n)").unwrap());
static REGEX_WHITESPACE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"[^\S\r\n]+").unwrap());

pub(crate) static COLOR_RED: &str = "31";
pub(crate) static COLOR_CYAN: &str = "36";
pub(crate) static COLOR_BRIGHT_CYAN: &str = "96";
pub(crate) static COLOR_BOLD_RED: &str = "1;31";
pub(crate) static BOLD: &str = "1";

thread_local! {
    /// Whether rendered diagnostics include ANSI color.
    static COLOR: Cell<bool> = const { Cell::new(false) };
}

/// Turns ANSI color in diagnostics on or off for the current thread.
pub fn enable_color(on: bool) {
    COLOR.with(|c| c.set(on));
}

/// A caretted span's role in a frame. The primary points at the error; a context span points at
/// a related site the message refers to.
#[derive(Clone, Copy, PartialEq)]
pub(crate) enum SpanKind {
    Primary,
    Context,
}

impl SpanKind {
    /// The underline glyph and color a span of this kind draws with.
    fn style(self) -> (char, &'static str) {
        match self {
            SpanKind::Primary => ('^', COLOR_RED),
            SpanKind::Context => ('-', COLOR_CYAN),
        }
    }
}

/// A caretted source range with its label and role.
#[derive(Clone)]
pub(crate) struct Span {
    pub pos: SourcePosition,
    pub label: String,
    pub kind: SpanKind,
}

/// Wraps `text` in an ANSI color code when color is on, otherwise returns it unchanged.
fn paint(text: &str, code: &str) -> String {
    if COLOR.with(Cell::get) {
        return format!("\x1b[{code}m{text}\x1b[0m");
    }
    return text.to_string();
}

/// A gutter cell: a right-aligned line number and its ` |` rail. A blank label makes an empty rail.
pub(crate) fn rail(label: &str, width: usize) -> String {
    return paint(&format!("{label:>width$} |"), COLOR_CYAN);
}

const TAB_WIDTH: usize = 4;

/// Renders a source line for a frame, expanding tabs so carets can line up under it.
fn expand_tabs(line: &str) -> String {
    return line.replace('\t', &" ".repeat(TAB_WIDTH));
}

/// Display columns `text` occupies once tabs are expanded. Uses char counting
fn display_width(text: &str) -> usize {
    return text.chars().map(|c| if c == '\t' { TAB_WIDTH } else { 1 }).sum();
}

/// Renders several labeled spans in one frame, in source order. Spans are assumed sorted by
/// position and to share one source. Spans on different lines each get their own caret row;
/// spans on the same line share a caret row and stack their labels below it. A jump over
/// unshown lines is marked with a `...` rail.
fn render_spans(spans: &[Span], help: &[String]) -> String {
    let content = &spans[0].pos.source.content;
    let width = spans.iter().map(|s| s.pos.line).max().unwrap().to_string().len();
    let bar = rail("", width);
    let mut lines = vec![bar.clone()];

    // Show the line before the first span for context, but only when that span is the primary.
    // A leading context span already introduces its own site, so the extra line is just noise.
    let first = &spans[0];
    let (first_start, _) = first.pos.line_bounds(first.pos.start);
    if first.kind == SpanKind::Primary && first_start > 0 {
        let prev_start = content[..first_start - 1].rfind('\n').map_or(0, |i| i + 1);
        let prev = expand_tabs(&content[prev_start..first_start - 1]);
        if !prev.trim().is_empty() {
            lines.push(format!("{} {prev}", rail(&(first.pos.line - 1).to_string(), width)));
        }
    }

    // Draw one source line at a time, gathering all the spans that fall on it. A gap between
    // consecutive lines is elided with a `...` rail.
    let mut i = 0;
    let mut prev_line: Option<usize> = None;
    while i < spans.len() {
        let line = spans[i].pos.line;
        let end = spans[i..].iter().position(|s| s.pos.line != line).map_or(spans.len(), |o| i + o);
        if prev_line.is_some_and(|prev| line > prev + 1) {
            lines.push(paint("...", COLOR_CYAN));
        }
        render_span_line(&mut lines, &bar, width, &spans[i..end]);
        prev_line = Some(line);
        i = end;
    }

    // A label on the final line runs into the help text, so part them with a blank rail. The line
    // lacks a label only when a lone span lands there with no label of its own.
    let last = spans.last().unwrap();
    let alone = spans.iter().filter(|s| s.pos.line == last.pos.line).count() == 1;
    if !help.is_empty() && !(alone && last.label.is_empty()) {
        lines.push(bar.clone());
    }

    return lines.join("\n") + &render_help(width, help);
}

/// Renders one source line, then the carets for every span on it. The last span's label sits
/// inline after its carets; earlier labels stack below, each joined to its carets by a `|`.
fn render_span_line(lines: &mut Vec<String>, bar: &str, width: usize, group: &[Span]) {
    let content = &group[0].pos.source.content;
    let (start, end) = group[0].pos.line_bounds(group[0].pos.start);
    lines.push(format!("{} {}", rail(&group[0].pos.line.to_string(), width), expand_tabs(&content[start..end])));

    let cols: Vec<usize> = group.iter().map(|s| display_width(&content[start..s.pos.start])).collect();

    // The caret row: every span's carets in its own glyph, then the last span's label inline.
    let mut row = String::new();
    let mut col = 0;
    for (k, span) in group.iter().enumerate() {
        let (glyph, color) = span.kind.style();
        let carets = display_width(&content[span.pos.start..span.pos.end.min(end)]).max(1);
        row.push_str(&" ".repeat(cols[k] - col));
        row.push_str(&paint(&glyph.to_string().repeat(carets), color));
        col = cols[k] + carets;
    }
    let last = group.last().unwrap();
    lines.push(format!("{bar} {row} {}", paint(&last.label, last.kind.style().1)));

    // Earlier labels stack under their carets, joined to the caret row by `|` connectors.
    if group.len() > 1 {
        let bars: Vec<(usize, SpanKind)> = group[..group.len() - 1].iter().enumerate().map(|(k, s)| (cols[k], s.kind)).collect();
        lines.push(format!("{bar} {}", connector_row(&bars, None)));
        for k in (0..group.len() - 1).rev() {
            lines.push(format!("{bar} {}", connector_row(&bars[..k], Some((cols[k], &group[k].label, group[k].kind)))));
        }
    }
}

/// A row with a `|` at each column in `bars`, then `label` placed at its column, if given. Each
/// connector and the label take the color of the span they belong to.
fn connector_row(bars: &[(usize, SpanKind)], label: Option<(usize, &str, SpanKind)>) -> String {
    let mut row = String::new();
    let mut col = 0;
    for &(at, kind) in bars {
        row.push_str(&" ".repeat(at - col));
        row.push_str(&paint("|", kind.style().1));
        col = at + 1;
    }
    if let Some((at, text, kind)) = label {
        row.push_str(&" ".repeat(at - col));
        row.push_str(&paint(text, kind.style().1));
    }
    return row;
}

/// The `help:` notes shown under a frame, each on its own gutter-aligned line. A note may carry
/// embedded newlines; continuation lines align under the note text, without a repeated `= help:`.
fn render_help(width: usize, help: &[String]) -> String {
    let prefix = paint(&format!("{} = help:", " ".repeat(width)), COLOR_CYAN);
    let indent = " ".repeat(width + " = help: ".len());
    let mut out = String::new();
    for note in help {
        let mut note_lines = note.split('\n');
        out.push_str(&format!("\n{prefix} {}", note_lines.next().unwrap()));
        for cont in note_lines {
            out.push_str(&format!("\n{indent}{cont}"));
        }
    }
    return out;
}

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
        let (line_start, _) = self.line_bounds(self.start);
        return display_width(&self.source.content[line_start..self.start]) + 1;
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
        let line = expand_tabs(&content[line_start..line_end]);

        // Keep the run on this one line. Always show at least one caret.
        let caret_end = self.end.min(line_end);
        let pad = " ".repeat(display_width(&content[line_start..self.start]));
        let carets = paint(&"^".repeat(display_width(&content[self.start..caret_end]).max(1)), COLOR_RED);
        let note = label.map_or(String::new(), |l| format!(" {}", paint(l, COLOR_RED)));

        // Show the line before for context, but a blank one is just noise, so skip it.
        let prev = (line_start > 0).then(|| {
            let prev_start = content[..line_start - 1].rfind('\n').map_or(0, |i| i + 1);
            (self.line - 1, expand_tabs(&content[prev_start..line_start - 1]))
        }).filter(|(_, text)| !text.trim().is_empty());

        let width = self.line.to_string().len();
        let bar = rail("", width);

        let mut out = format!("{bar}\n");
        if let Some((n, text)) = prev {
            out.push_str(&format!("{} {text}\n", rail(&n.to_string(), width)));
        }
        out.push_str(&format!("{} {line}\n{bar} {pad}{carets}{note}", rail(&self.line.to_string(), width)));
        // A labeled caret runs into the help text, so part them with a blank rail.
        if !note.is_empty() && !help.is_empty() {
            out.push_str(&format!("\n{bar}"));
        }
        out.push_str(&render_help(width, help));
        return out;
    }

    /// Renders the failure point plus the unclosed opener it belongs to.
    pub fn render_snippet_pair(&self, label: Option<&str>, opener: &SourcePosition, opener_label: &str, help: &[String]) -> String {
        let content = &self.source.content;
        let (p_start, p_end) = self.line_bounds(self.start);
        let p_col = display_width(&content[p_start..self.start]);
        let carets = "^".repeat(display_width(&content[self.start..self.end.min(p_end)]).max(1));
        let note = label.map_or(String::new(), |l| format!(" {}", paint(l, COLOR_RED)));
        let (o_start, o_end) = self.line_bounds(opener.start);
        let o_col = display_width(&content[o_start..opener.start]);
        let width = self.line.to_string().len();
        let bar = rail("", width);

        if self.line == opener.line {
            let line = expand_tabs(&content[p_start..p_end]);
            let lead = " ".repeat(o_col);
            let mid = " ".repeat(p_col.saturating_sub(o_col + 1));
            let ann = format!("{lead}{}{mid}{}", paint("^", COLOR_CYAN), paint(&carets, COLOR_RED));
            let stack = " ".repeat(o_col);
            let frame = format!("{bar}\n{num} {line}\n{bar} {ann}{note}\n{bar} {stack}{pipe}\n{bar} {stack}{olabel}",
                num = rail(&self.line.to_string(), width), pipe = paint("|", COLOR_CYAN), olabel = paint(opener_label, COLOR_CYAN));
            return frame + &render_help(width, help);
        }

        let o_line = expand_tabs(&content[o_start..o_end]);
        let p_line = expand_tabs(&content[p_start..p_end]);
        let frame = format!("{bar}\n{orail} {o_line}\n{bar} {o_pad}{ocaret} {olabel}\n{prail} {p_line}\n{bar} {p_pad}{pcarets}{note}",
            orail = rail(&opener.line.to_string(), width), prail = rail(&self.line.to_string(), width),
            o_pad = " ".repeat(o_col), p_pad = " ".repeat(p_col),
            ocaret = paint("^", COLOR_CYAN), olabel = paint(opener_label, COLOR_CYAN), pcarets = paint(&carets, COLOR_RED));
        // A labeled caret runs into the help text, so part them with a blank rail.
        let sep = if !note.is_empty() && !help.is_empty() { format!("\n{bar}") } else { String::new() };
        return frame + &sep + &render_help(width, help);
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

    if let Some(end) = find_at(&REGEX_STAR_MUT, input, input_index) {
        return Ok(Token::new(TokenType::StarMut, &input[input_index..end]));
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
use std::fmt;

use super::{paint, rail, render_spans, SourcePosition, Span, SpanKind, BOLD, COLOR_BOLD_RED, COLOR_BRIGHT_CYAN, COLOR_CYAN};

/// One render shape for every stage's errors, so diagnostics look the same everywhere.
pub struct Diagnostic {
    message: String,
    pos: SourcePosition,
    label: Option<String>,
    opener: Option<(SourcePosition, String)>,
    spans: Vec<Span>,
    help: Vec<String>,
    trace: Option<String>,
}

impl Diagnostic {
    pub fn new(message: impl Into<String>, pos: SourcePosition) -> Diagnostic {
        return Diagnostic {
            message: message.into(),
            pos,
            label: None,
            opener: None,
            spans: Vec::new(),
            help: Vec::new(),
            trace: None,
        };
    }

    pub fn with_label(mut self, label: impl Into<String>) -> Diagnostic {
        self.label = Some(label.into());
        return self;
    }

    /// Marks a second position, the delimiter this error failed to close.
    pub fn with_opener(mut self, pos: SourcePosition, label: impl Into<String>) -> Diagnostic {
        self.opener = Some((pos, label.into()));
        return self;
    }

    /// Adds a second labeled span, caretted alongside the primary in one frame.
    pub fn with_span(mut self, pos: SourcePosition, label: impl Into<String>) -> Diagnostic {
        self.spans.push(Span { pos, label: label.into(), kind: SpanKind::Primary });
        return self;
    }

    /// Adds a labeled context span, underlined alongside the primary to point at a related site.
    pub fn with_context_span(mut self, pos: SourcePosition, label: impl Into<String>) -> Diagnostic {
        self.spans.push(Span { pos, label: label.into(), kind: SpanKind::Context });
        return self;
    }

    /// Adds a `help:` note shown below the frame, for how to fix the error.
    pub fn with_help(mut self, help: impl Into<String>) -> Diagnostic {
        self.help.push(help.into());
        return self;
    }

    /// Attaches a runtime call trace, shown below the frame.
    pub fn with_trace(mut self, trace: impl Into<String>) -> Diagnostic {
        self.trace = Some(trace.into());
        return self;
    }
}

impl fmt::Display for Diagnostic {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let frame = if !self.spans.is_empty() {
            // Combine the primary and its extra spans, ordered by position, into one frame.
            let mut all = vec![Span {
                pos: self.pos.clone(),
                label: self.label.clone().unwrap_or_default(),
                kind: SpanKind::Primary
            }];
            all.extend(self.spans.iter().cloned());
            all.sort_by_key(|s| s.pos.start);
            render_spans(&all, &self.help)
        } else {
            match &self.opener {
                Some((opener, label)) => self.pos.render_snippet_pair(self.label.as_deref(), opener, label, &self.help),
                None => self.pos.render_snippet(self.label.as_deref(), &self.help),
            }
        };
        write!(f, "{} {}\n {} {}:{}\n{}",
            paint("error:", COLOR_BOLD_RED),
            paint(&self.message, BOLD),
            paint("-->", COLOR_BRIGHT_CYAN),
            self.pos,
            self.pos.column(),
            frame)?;

        if let Some(trace) = &self.trace {
            if self.label.is_some() && self.help.is_empty() {
                write!(f, "\n{}", rail("", self.pos.line.to_string().len()))?;
            }
            write!(f, "\n{}", paint(trace, COLOR_CYAN))?;
        }
        return Ok(());
    }
}

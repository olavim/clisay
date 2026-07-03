use std::fmt;

use super::{paint, render_spans, SourcePosition};

/// One render shape for every stage's errors, so diagnostics look the same everywhere.
pub struct Diagnostic {
    message: String,
    pos: SourcePosition,
    label: Option<String>,
    opener: Option<(SourcePosition, String)>,
    spans: Vec<(SourcePosition, String)>,
    help: Vec<String>,
}

impl Diagnostic {
    pub fn new(message: impl Into<String>, pos: SourcePosition) -> Diagnostic {
        return Diagnostic { message: message.into(), pos, label: None, opener: None, spans: Vec::new(), help: Vec::new() };
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
        self.spans.push((pos, label.into()));
        return self;
    }

    /// Adds a `help:` note shown below the frame, for how to fix the error.
    pub fn with_help(mut self, help: impl Into<String>) -> Diagnostic {
        self.help.push(help.into());
        return self;
    }
}

impl fmt::Display for Diagnostic {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let frame = if !self.spans.is_empty() {
            // Combine the primary and its extra spans, ordered by position, into one frame.
            let mut all = vec![(self.pos.clone(), self.label.clone().unwrap_or_default())];
            all.extend(self.spans.iter().cloned());
            all.sort_by_key(|(pos, _)| pos.start);
            render_spans(&all, &self.help)
        } else {
            match &self.opener {
                Some((opener, label)) => self.pos.render_snippet_pair(self.label.as_deref(), opener, label, &self.help),
                None => self.pos.render_snippet(self.label.as_deref(), &self.help),
            }
        };
        return write!(f, "{} {}\n {} {}:{}\n{}",
            paint("error:", "1;31"), paint(&self.message, "1"), paint("-->", "96"), self.pos, self.pos.column(), frame);
    }
}

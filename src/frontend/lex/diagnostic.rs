use std::fmt;

use super::{paint, SourcePosition};

/// One render shape for every stage's errors, so diagnostics look the same everywhere.
pub struct Diagnostic {
    message: String,
    pos: SourcePosition,
    label: Option<String>,
    opener: Option<(SourcePosition, String)>,
    help: Vec<String>,
}

impl Diagnostic {
    pub fn new(message: impl Into<String>, pos: SourcePosition) -> Diagnostic {
        return Diagnostic { message: message.into(), pos, label: None, opener: None, help: Vec::new() };
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

    /// Adds a `help:` note shown below the frame, for how to fix the error.
    pub fn with_help(mut self, help: impl Into<String>) -> Diagnostic {
        self.help.push(help.into());
        return self;
    }
}

impl fmt::Display for Diagnostic {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let frame = match &self.opener {
            Some((opener, label)) => self.pos.render_snippet_pair(self.label.as_deref(), opener, label, &self.help),
            None => self.pos.render_snippet(self.label.as_deref(), &self.help),
        };
        return write!(f, "{} {}\n {} {}:{}\n{}",
            paint("error:", "1;31"), paint(&self.message, "1"), paint("-->", "96"), self.pos, self.pos.column(), frame);
    }
}

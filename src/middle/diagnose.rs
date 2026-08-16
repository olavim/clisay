//! How a middle-end pass words a refusal. A pass says which `Hir` it reads and gets the rest, so
//! two passes reporting the same kind of problem render it the same way.

use anyhow::anyhow;

use crate::frontend::lex::{Diagnostic, SourcePosition};
use crate::middle::hir::{Hir, HirId};

pub trait Diagnose {
    fn hir(&self) -> &Hir;

    fn error<T>(&self, msg: String, node: &HirId<T>) -> anyhow::Error {
        anyhow!("{}", Diagnostic::new(msg, self.hir().pos(node).clone()))
    }

    /// An error carrying a `help:` note on how to fix it.
    fn error_help<T>(&self, msg: String, node: &HirId<T>, help: impl Into<String>) -> anyhow::Error {
        anyhow!("{}", Diagnostic::new(msg, self.hir().pos(node).clone()).with_help(help))
    }

    /// An error with a label on its own caret.
    fn error_labeled<T>(&self, msg: String, node: &HirId<T>, label: impl Into<String>) -> anyhow::Error {
        anyhow!("{}", Diagnostic::new(msg, self.hir().pos(node).clone()).with_label(label))
    }

    /// An error whose caret carries a label, plus a `help:` note on how to fix it.
    fn error_labeled_help<T>(&self, msg: String, node: &HirId<T>, label: impl Into<String>, help: impl Into<String>) -> anyhow::Error {
        anyhow!("{}", Diagnostic::new(msg, self.hir().pos(node).clone()).with_label(label).with_help(help))
    }

    /// An error with a primary caret at `primary` and a context caret at `site`.
    fn error_ctx(&self, msg: impl Into<String>, primary: &SourcePosition, label: impl Into<String>, site: &SourcePosition, site_label: impl Into<String>) -> anyhow::Error {
        anyhow!("{}", Diagnostic::new(msg, primary.clone())
            .with_label(label)
            .with_context_span(site.clone(), site_label))
    }

    /// A context-caret error that also carries a `help:` note.
    fn error_ctx_help(&self, msg: impl Into<String>, primary: &SourcePosition, label: impl Into<String>, site: &SourcePosition, site_label: impl Into<String>, help: impl Into<String>) -> anyhow::Error {
        anyhow!("{}", Diagnostic::new(msg, primary.clone())
            .with_label(label)
            .with_context_span(site.clone(), site_label)
            .with_help(help))
    }
}

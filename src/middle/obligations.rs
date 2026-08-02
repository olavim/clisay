//! The obligation vocabulary: the debt a value or a slot carries, the rules an obligation may
//! declare, and how a refusal of each is worded.

use crate::ast::Symbol;
use crate::middle::hir::{Hir, ObligationRules};

/// A set of obligation names.
#[derive(Clone, PartialEq, Eq, Default)]
pub struct Obligations(Vec<Symbol>);

impl Obligations {
    pub fn new() -> Obligations {
        Obligations(Vec::new())
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn contains(&self, name: &Symbol) -> bool {
        self.0.binary_search(name).is_ok()
    }

    /// Adds a name. Answers whether the set grew.
    pub fn insert(&mut self, name: Symbol) -> bool {
        match self.0.binary_search(&name) {
            Ok(_) => false,
            Err(at) => { self.0.insert(at, name); true },
        }
    }

    pub fn retain(&mut self, keep: impl FnMut(&Symbol) -> bool) {
        self.0.retain(keep);
    }

    pub fn iter(&self) -> std::slice::Iter<'_, Symbol> {
        self.0.iter()
    }

    /// The names this set and `other` share.
    pub fn intersection<'a>(&'a self, other: &'a Obligations) -> impl Iterator<Item = &'a Symbol> {
        self.0.iter().filter(|n| other.contains(n))
    }

    /// The names in this set that `other` does not have.
    pub fn difference<'a>(&'a self, other: &'a Obligations) -> impl Iterator<Item = &'a Symbol> {
        self.0.iter().filter(|n| !other.contains(n))
    }
}

impl Extend<Symbol> for Obligations {
    fn extend<T: IntoIterator<Item = Symbol>>(&mut self, names: T) {
        for name in names {
            self.insert(name);
        }
    }
}

impl<'a> Extend<&'a Symbol> for Obligations {
    fn extend<T: IntoIterator<Item = &'a Symbol>>(&mut self, names: T) {
        for name in names {
            self.insert(*name);
        }
    }
}

impl FromIterator<Symbol> for Obligations {
    fn from_iter<T: IntoIterator<Item = Symbol>>(names: T) -> Obligations {
        let mut out = Obligations::new();
        out.extend(names);
        out
    }
}

impl<const N: usize> From<[Symbol; N]> for Obligations {
    fn from(names: [Symbol; N]) -> Obligations {
        Obligations::from_iter(names)
    }
}

impl IntoIterator for Obligations {
    type Item = Symbol;
    type IntoIter = std::vec::IntoIter<Symbol>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a> IntoIterator for &'a Obligations {
    type Item = &'a Symbol;
    type IntoIter = std::slice::Iter<'a, Symbol>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

/// A rule an obligation may declare, paired with how a refusal spells it.
#[derive(Clone, Copy)]
pub enum Rule { NoPersist, NoReturn, BeforeDrop }

impl Rule {
    /// Whether these rules declare it.
    pub fn holds(self, rules: &ObligationRules) -> bool {
        match self {
            Rule::NoPersist => rules.no_persist,
            Rule::NoReturn => rules.no_return,
            Rule::BeforeDrop => rules.before_drop,
        }
    }

    /// The rule as written in a declaration, for the citation in a help line.
    pub fn spelling(self) -> &'static str {
        match self {
            Rule::NoPersist => "no persist",
            Rule::NoReturn => "no return",
            Rule::BeforeDrop => "discharge before drop",
        }
    }
}

/// The operation a rule refuses. Each site refuses in its own words and offers the way around it
/// that fits. A `Drop` is the value reaching the end of its scope without being discharged.
#[derive(Clone, Copy)]
pub enum Site { Field, Container, Capture, Return, Drop, ScopeEnd }

impl Site {
    /// The header completing "cannot ...", around the obligation list. `Drop` names no operation, so
    /// it reports what happened to the value instead.
    pub fn refusal(self, owed: &str) -> String {
        match self {
            Site::Field => format!("cannot store value owing {owed} in a field"),
            Site::Container => format!("cannot store value owing {owed} in a container"),
            Site::Capture => format!("cannot capture value owing {owed}"),
            Site::Return => format!("cannot return value owing {owed}"),
            Site::Drop => format!("this result owes {owed} and is never discharged"),
            Site::ScopeEnd => format!("value owing {owed} is never discharged"),
        }
    }

    /// The gerund completing "which prevents ...".
    pub fn prevents(self) -> &'static str {
        match self {
            Site::Field => "storing it in a field",
            Site::Container => "storing it in a container",
            Site::Capture => "capturing it in a closure",
            Site::Return => "returning it",
            Site::Drop | Site::ScopeEnd => "leaving it undischarged",
        }
    }

    /// What to do instead, for a built-in obligation with no declaration to cite. `opt` and `fails`
    /// name their own witness, since a `null` carries nothing and an `Err` carries a payload to keep.
    pub fn guidance(self, obligation: &str) -> &'static str {
        match (obligation, self) {
            ("opt", Site::Field | Site::Container) => "narrow it first, and store what that leaves behind",
            ("opt", Site::Capture) => "narrow it in this frame, and capture what that leaves behind",
            ("opt", Site::Return) => "narrow it here, or declare `opt` on the return",
            ("opt", Site::Drop) => "narrow it here, or bind it and narrow it later",
            ("opt", Site::ScopeEnd) => "narrow it with `??`, `!` or a test, or hand it to a slot that declares `opt`",
            ("fails", Site::Field | Site::Container) => "store what the `Err` carries, not the `Err` itself",
            ("fails", Site::Capture) => "handle the `Err` in this frame, and capture what it leaves behind",
            ("fails", Site::Return) => "handle the `Err` here, or declare `fails` on the return",
            ("fails", Site::Drop) => "handle the `Err` here, or bind it and handle it later",
            ("fails", Site::ScopeEnd) => "handle the `Err` with `??`, `!` or a test, or hand it to a slot that declares `fails`",
            (_, _) => unreachable!("user obligations get generic guidance in Checker::prohibition_help"),
        }
    }
}

/// The obligation names in a stable order, so a diagnostic does not follow the hash order.
pub fn sorted_obligation_names<'a>(hir: &'a Hir, obligations: &Obligations) -> Vec<&'a str> {
    let mut names: Vec<&str> = obligations.iter().map(|o| hir.text(*o)).collect();
    names.sort();
    names
}

/// The obligations spelled as clause atoms, like `fails taint`, for a suggested annotation.
pub fn obligation_atoms(hir: &Hir, obligations: &Obligations) -> String {
    sorted_obligation_names(hir, obligations).join(" ")
}

/// The obligations sorted and quoted for a diagnostic, like `'fails', 'opt'`.
pub fn quoted_obligation_list(hir: &Hir, obligations: &Obligations) -> String {
    sorted_obligation_names(hir, obligations).iter().map(|o| format!("'{o}'")).collect::<Vec<_>>().join(", ")
}

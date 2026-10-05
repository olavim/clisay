//! The obligation vocabulary.

use crate::ast::Symbol;
use crate::middle::hir::{Hir, ObligationRules};

#[derive(Clone, PartialEq, Eq, Default)]
pub struct Obligations(Vec<Symbol>);

impl Obligations {
    pub const fn new() -> Obligations {
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

    pub fn difference<'a>(&'a self, other: &'a Obligations) -> impl Iterator<Item = &'a Symbol> {
        self.0.iter().filter(|n| !other.contains(n))
    }

    pub fn union(&self, other: &Obligations) -> Obligations {
        self.iter().chain(other.iter()).copied().collect()
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

#[derive(Clone, Copy)]
pub enum ObligationRule { NoPersist, MustUse }

impl ObligationRule {
    pub fn holds(self, rules: &ObligationRules) -> bool {
        match self {
            ObligationRule::NoPersist => rules.no_persist,
            ObligationRule::MustUse => rules.must_use,
        }
    }

    pub fn spelling(self) -> &'static str {
        match self {
            ObligationRule::NoPersist => "no persist",
            ObligationRule::MustUse => "must use",
        }
    }
}

#[derive(Clone, Copy)]
pub enum Site {
    Field,
    Container,
    Slot,
    Capture,
    Drop,
    ScopeEnd,
}

impl Site {
    pub fn refusal(self, owed: &str) -> String {
        match self {
            Site::Field => format!("cannot store value owing {owed} in a field"),
            Site::Container => format!("cannot store value owing {owed} in a container"),
            Site::Slot => format!("cannot store value owing {owed}"),
            Site::Capture => format!("cannot capture value owing {owed}"),
            Site::Drop => format!("this result owes {owed} and is never used"),
            Site::ScopeEnd => format!("value owing {owed} is never used"),
        }
    }

    pub fn prevents(self) -> &'static str {
        match self {
            Site::Field => "storing it in a field",
            Site::Container => "storing it in a container",
            Site::Slot => "storing it",
            Site::Capture => "capturing it in a closure",
            Site::Drop | Site::ScopeEnd => "leaving it undischarged",
        }
    }

    pub fn guidance(self, obligation: &str) -> &'static str {
        match (obligation, self) {
            ("opt", Site::Field | Site::Container | Site::Slot) => "narrow it first, and store what that leaves behind",
            ("opt", Site::Capture) => "narrow it in this frame, and capture what that leaves behind",
            ("opt", Site::Drop) => "narrow it here, or bind it and narrow it later",
            ("opt", Site::ScopeEnd) => "narrow it with `??`, `!` or a test, or hand it to a slot that declares `opt`",
            ("fails", Site::Field | Site::Container | Site::Slot) => "store what the `Err` carries, not the `Err` itself",
            ("fails", Site::Capture) => "handle the `Err` in this frame, and capture what it leaves behind",
            ("fails", Site::Drop) => "handle the `Err` here, or bind it and handle it later",
            ("fails", Site::ScopeEnd) => "handle the `Err` with `??`, `!` or a test, or hand it to a slot that declares `fails`",
            (_, _) => unreachable!("user obligations get generic guidance in Checker::prohibition_help"),
        }
    }
}

pub fn sorted_obligation_names<'a>(hir: &'a Hir, obligations: &Obligations) -> Vec<&'a str> {
    let mut names: Vec<&str> = obligations.iter().map(|o| hir.text(*o)).collect();
    names.sort();
    names
}

pub fn obligation_atoms(hir: &Hir, obligations: &Obligations) -> String {
    sorted_obligation_names(hir, obligations).join(" ")
}

pub fn quoted_obligation_list(hir: &Hir, obligations: &Obligations) -> String {
    sorted_obligation_names(hir, obligations).iter().map(|o| format!("'{o}'")).collect::<Vec<_>>().join(", ")
}

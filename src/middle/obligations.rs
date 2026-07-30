//! The obligation set: the debt a value or a slot carries.

use crate::ast::Symbol;

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

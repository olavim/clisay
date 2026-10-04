//! What is known at each path below a root value.

use std::collections::{hash_map, HashMap};
use std::ops::{Deref, DerefMut};

use crate::middle::signatures::TypeTag;

use super::scope::Callables;
use super::{FlowPath, PathStep, PossibleFacts, ProvenFacts};

#[derive(Clone, PartialEq)]
pub struct PathMap<F>(HashMap<FlowPath, F>);

impl<F> PathMap<F> {
    pub fn new() -> Self {
        PathMap(HashMap::new())
    }

    /// The same facts, moved down under `path`.
    pub(super) fn under(&self, path: &[PathStep]) -> Self where F: Clone {
        self.iter().map(|(at, facts)| ([path, at].concat(), facts.clone())).collect()
    }
}

impl<F> Default for PathMap<F> {
    fn default() -> Self {
        PathMap::new()
    }
}

impl<F> Deref for PathMap<F> {
    type Target = HashMap<FlowPath, F>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<F> DerefMut for PathMap<F> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<F> FromIterator<(FlowPath, F)> for PathMap<F> {
    fn from_iter<I: IntoIterator<Item = (FlowPath, F)>>(iter: I) -> Self {
        PathMap(iter.into_iter().collect())
    }
}

impl<F> IntoIterator for PathMap<F> {
    type Item = (FlowPath, F);
    type IntoIter = hash_map::IntoIter<FlowPath, F>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a, F> IntoIterator for &'a PathMap<F> {
    type Item = (&'a FlowPath, &'a F);
    type IntoIter = hash_map::Iter<'a, FlowPath, F>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl PathMap<ProvenFacts> {
    pub(super) fn at(&self, step: PathStep) -> Option<&ProvenFacts> {
        self.get(&[step][..])
    }

    /// Facts below one step, with that step as the new root.
    pub(super) fn inside(&self, step: PathStep) -> Self {
        self.iter()
            .filter_map(|(path, facts)| match path.split_first() {
                Some((head, rest)) if *head == step => Some((rest.to_vec(), facts.clone())),
                _ => None,
            })
            .collect()
    }

    /// Everything but the root.
    pub(super) fn below_root(&self) -> Self {
        self.iter().filter(|(path, _)| !path.is_empty()).map(|(p, f)| (p.clone(), f.clone())).collect()
    }

    /// Changes what is proven at `path`, dropping the entry if empty.
    pub(super) fn update(&mut self, path: FlowPath, change: impl FnOnce(&mut ProvenFacts)) {
        let facts = self.entry(path.clone()).or_default();
        change(facts);
        if facts.is_empty() {
            self.remove(&path);
        }
    }

    /// Adds what `other` proves, where both are true of the same value.
    pub(super) fn add_proofs(&mut self, other: &Self) {
        for (path, theirs) in other {
            let ours = self.entry(path.clone()).or_default();
            ours.owed = match (ours.owed.take(), &theirs.owed) {
                (Some(mut owed), Some(theirs)) => { owed.retain(|o| theirs.contains(o)); Some(owed) },
                (ours, theirs) => ours.or_else(|| theirs.clone()),
            };
            if ours.tag == TypeTag::Unknown {
                ours.tag = theirs.tag.clone();
            }
        }
    }

    pub fn join(&mut self, other: &Self) {
        for (path, theirs) in other {
            if theirs.owed.is_some() {
                self.entry(path.clone()).or_default();
            }
        }
        self.retain(|path, facts| {
            let theirs = other.get(path);
            facts.owed = match (&facts.owed, theirs.and_then(|t| t.owed.as_ref())) {
                (Some(ours), Some(theirs)) => Some(ours.union(theirs)),
                _ => None,
            };
            if theirs.is_none_or(|t| t.tag != facts.tag) {
                facts.tag = TypeTag::Unknown;
            }
            !facts.is_empty()
        });
    }
}

impl PathMap<PossibleFacts> {
    /// Facts below one step, with that step as the new root.
    pub(super) fn inside(&self, step: PathStep) -> Self {
        let reached = |head: &PathStep| step == PathStep::EveryElement || *head == PathStep::EveryElement || *head == step;
        let mut inside = PathMap::new();
        for (path, facts) in self {
            let Some((head, rest)) = path.split_first() else { continue };
            if reached(head) {
                inside.entry(rest.to_vec()).or_insert_with(PossibleFacts::default).callables.extend(&facts.callables);
            }
        }
        inside
    }

    /// The callables the value itself may be.
    pub(super) fn root_callables(&self) -> Callables {
        self.get(&FlowPath::new()).map(|facts| facts.callables.clone()).unwrap_or_default()
    }

    /// Every callable the value may be or hold, anywhere in it.
    pub(super) fn callables(&self) -> Callables {
        self.values().flat_map(|facts| facts.callables.iter().copied()).collect()
    }

    pub fn join(&mut self, other: &Self) {
        for (path, facts) in other {
            self.entry(path.clone()).or_default().callables.extend(&facts.callables);
        }
    }
}

pub(super) fn possible_step(step: Option<PathStep>) -> PathStep {
    match step {
        Some(PathStep::Field(name)) => PathStep::Field(name),
        _ => PathStep::EveryElement,
    }
}

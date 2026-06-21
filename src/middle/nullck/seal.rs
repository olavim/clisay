//! The construction seal tracked while checking a type's `init`.

use std::collections::HashSet;

use crate::middle::hir::Symbol;

/// State of the construction seal. Inside an `init` body the partially-built `this` may assign
/// and read its own fields. It may not otherwise escape.
#[derive(Default)]
pub(super) struct Seal {
    assigned: Option<HashSet<Symbol>>,
    /// Whether `this` was used since the flag was last reset. Catches a closure in `init`
    /// capturing the partially-built `this`.
    this_seen: bool,
}

impl Seal {
    /// Whether checking is currently inside an init body.
    pub(super) fn in_init(&self) -> bool {
        self.assigned.is_some()
    }

    /// Enters an init body seeded with the fields already provided. Returns the previous state
    /// to hand back to `restore` on exit.
    pub(super) fn enter(&mut self, seed: HashSet<Symbol>) -> Option<HashSet<Symbol>> {
        self.assigned.replace(seed)
    }

    /// Suspends the seal for a nested function body so it is not seen as inside the init.
    pub(super) fn suspend(&mut self) -> Option<HashSet<Symbol>> {
        self.assigned.take()
    }

    /// Restores the assigned-set state saved by `enter` or `suspend`.
    pub(super) fn restore(&mut self, saved: Option<HashSet<Symbol>>) {
        self.assigned = saved;
    }

    pub(super) fn mark_assigned(&mut self, field: Symbol) {
        if let Some(assigned) = self.assigned.as_mut() {
            assigned.insert(field);
        }
    }

    pub(super) fn is_assigned(&self, field: Symbol) -> bool {
        self.assigned.as_ref().is_some_and(|a| a.contains(&field))
    }

    /// Whether `field` is read in the current init before it is assigned.
    pub(super) fn reads_before_assign(&self, field: Symbol) -> bool {
        self.assigned.as_ref().is_some_and(|a| !a.contains(&field))
    }

    /// Resets the `this`-seen flag for a nested body, returning the previous value.
    pub(super) fn take_this_seen(&mut self) -> bool {
        std::mem::replace(&mut self.this_seen, false)
    }

    pub(super) fn this_seen(&self) -> bool {
        self.this_seen
    }

    pub(super) fn set_this_seen(&mut self, seen: bool) {
        self.this_seen = seen;
    }
}

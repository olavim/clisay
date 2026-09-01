use crate::middle::diagnose::Diagnose;
use crate::middle::obligations::{obligation_atoms, quoted_obligation_list};
use std::collections::{HashMap, HashSet};

use crate::middle::hir::{HirExpr, HirId, Symbol, TypeId};
use crate::middle::obligations::Obligations;

use super::{Checker, Debt, Violation};

#[derive(Clone)]
pub struct WitnessSet {
    pub null: bool,
    pub witnesses: Vec<TypeId>,
    pub contains_user_witnesses: bool,
}

/// The witnesses a destination allows (a slot or a `!`).
pub struct Barrier {
    pub null_allowed: bool,
    pub allow_witnesses: Vec<TypeId>,
}

/// A runtime check codegen emits.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Guard {
    /// An unknown value against the witnesses its destination refuses.
    Boundary,
    /// A `!` operand owing only `opt`, where a null check alone suffices.
    NonNull,
    /// A value entering an immutable construction.
    Immutable,
}

#[derive(Default)]
pub struct ArgMarks {
    survive: Vec<(u8, Symbol)>,
}

/// The runtime checks codegen emits.
#[derive(Default)]
pub struct Barriers {
    /// Every per-node runtime check, in the order codegen emits them.
    pub(super) guards: HashMap<HirId<HirExpr>, Vec<Guard>>,
    /// Checks the pass proved unnecessary, recorded only under check-forcing.
    pub(super) elided: HashMap<HirId<HirExpr>, Vec<Guard>>,
    /// An unknown value guarded against the witnesses its destination does not allow: a value
    /// entering a slot, or a `!` on an unknown operand.
    pub(super) boundary_barriers: HashMap<HirId<HirExpr>, Barrier>,
    /// Discharge nodes (`??`, `?`, `!`) whose operand owes an object witness.
    pub(super) witness_tests: HashMap<HirId<HirExpr>, WitnessSet>,
    /// What each call does to its arguments, keyed by callee node.
    pub(super) arg_marks: HashMap<HirId<HirExpr>, ArgMarks>,
    /// Every registered object witness declaration, the VM's registry for recognizing a crossing
    /// value as a witness at a boundary barrier.
    pub(super) witness_decls: Vec<TypeId>,
    /// Immutable container literals with an unknown-capability element, whose elements are checked
    /// for mutability at construction so a mutable value cannot land in an immutable container.
    pub(super) seal_checks: HashSet<HirId<HirExpr>>,
    /// Paren-construction `Call` nodes (`K(args)`).
    pub(super) constructions: HashSet<HirId<HirExpr>>,
    /// Scopes holding an element writer slot, by node index. A scope gives back whatever its own
    /// locals still hold.
    pub(super) write_scopes: HashSet<usize>,
    /// Store targets one name is proven to reach. Their stores skip the one-writer arbitration
    /// that every other store runs.
    pub(super) unshared_stores: HashSet<HirId<HirExpr>>,
}

impl Barriers {
    /// Whether one name is proven to reach this store's target, so the store needs no arbitration.
    pub fn store_is_unshared(&self, target: &HirId<HirExpr>) -> bool {
        self.unshared_stores.contains(target)
    }

    /// Every runtime check this node carries, in emission order.
    pub fn guards(&self, node: &HirId<HirExpr>) -> &[Guard] {
        self.guards.get(node).map_or(&[], Vec::as_slice)
    }

    /// The runtime checks this node would carry if the pass hadn't proved them unnecessary.
    /// Empty unless check-forcing is on.
    pub fn elided(&self, node: &HirId<HirExpr>) -> &[Guard] {
        self.elided.get(node).map_or(&[], Vec::as_slice)
    }

    /// The boundary guard for an unknown value at this node, if one is needed.
    pub fn boundary(&self, node: &HirId<HirExpr>) -> Option<&Barrier> {
        self.boundary_barriers.get(node)
    }

    /// Every registered object witness declaration, for the VM's boundary-barrier registry.
    pub fn witness_decls(&self) -> &[TypeId] {
        &self.witness_decls
    }

    /// The witness set a discharge node tests, when its operand owes an object witness.
    pub fn witness_set(&self, node: &HirId<HirExpr>) -> Option<&WitnessSet> {
        self.witness_tests.get(node)
    }

    pub fn survive(&self, callee: &HirId<HirExpr>) -> Option<&[(u8, Symbol)]> {
        self.arg_marks.get(callee).map(|m| m.survive.as_slice()).filter(|p| !p.is_empty())
    }

    /// Whether this container literal needs a runtime check that no element is mutable.
    pub fn needs_seal_check(&self, node: &HirId<HirExpr>) -> bool {
        self.seal_checks.contains(node)
    }

    /// Whether this `Call` node is a paren construction `K(args)`.
    pub fn is_construction(&self, node: &HirId<HirExpr>) -> bool {
        self.constructions.contains(node)
    }

    /// Whether this scope has to give back element writer slots on the way out.
    pub fn releases_write_ownership<T>(&self, scope: &HirId<T>) -> bool {
        self.write_scopes.contains(&scope.index())
    }

    /// How many nodes carry a runtime check. A boundary's payload rides its guard, so it is one
    /// node here however many guards it asks for.
    pub fn len(&self) -> usize {
        self.guards.len()
    }

    pub fn is_empty(&self) -> bool {
        self.guards.is_empty()
    }
}

impl<'a> Checker<'a> {
    /// Records a runtime check the pass proved unnecessary. A no-op unless check-forcing is on,
    /// so neither the table nor the walk costs anything in an ordinary run.
    pub(super) fn record_elision(&mut self, node: &HirId<HirExpr>, guard: Guard) {
        if !self.ctx.force_checks {
            return;
        }
        let elided = self.out.elided.entry(*node).or_default();
        if let Err(at) = elided.binary_search(&guard) {
            elided.insert(at, guard);
        }
    }

    /// Records a runtime check for a node. Guards are kept in emission order, and a node asks for
    /// each at most once however many times the pass reaches it.
    pub(super) fn record_guard(&mut self, node: &HirId<HirExpr>, guard: Guard) {
        let guards = self.out.guards.entry(*node).or_default();
        if let Err(at) = guards.binary_search(&guard) {
            guards.insert(at, guard);
        }
    }

    /// Records that an opaque call must assert its callee borrows the given argument positions.
    pub(super) fn record_survive_barrier(&mut self, callee: &HirId<HirExpr>, positions: Vec<(u8, Symbol)>) {
        self.out.arg_marks.entry(*callee).or_default().survive = positions;
    }

    /// Marks an immutable container literal whose elements must be checked for mutability at runtime.
    pub(super) fn record_seal_check(&mut self, node: &HirId<HirExpr>) {
        self.out.seal_checks.insert(*node);
    }

    /// Marks a `Call` node as a paren construction `K(args)`.
    pub(super) fn record_construction(&mut self, node: &HirId<HirExpr>) {
        self.out.constructions.insert(*node);
    }

    /// Marks a scope that has to give back element writer slots.
    pub(super) fn record_write_scope(&mut self, scope: &HirId<HirExpr>) {
        self.out.write_scopes.insert(scope.index());
    }

    /// Records the guard for an unknown value reaching a destination accepting `accepted`. The
    /// guard allows those obligations' witnesses.
    pub(super) fn record_boundary_barrier(&mut self, node: &HirId<HirExpr>, accepted: &Obligations) {
        let null_allowed = accepted.contains(&self.ctx.sigs.opt);
        let mut allow_witnesses = Vec::new();
        for (ob, id) in self.ctx.sigs.object_witnesses() {
            if accepted.contains(&ob) && !allow_witnesses.contains(&id) {
                allow_witnesses.push(id);
            }
        }
        self.out.boundary_barriers.insert(*node, Barrier { null_allowed, allow_witnesses });
        self.record_guard(node, Guard::Boundary);
    }

    /// Classifies a value entering a non-null target. A non-null slot forbids `opt`, so only a
    /// value owing `opt` violates it. An unknown value records the non-null boundary guard.
    pub(super) fn non_null_violation(&mut self, value: &Debt, target: &HirId<HirExpr>) -> Option<Violation> {
        match value {
            Debt::Clean => None,
            Debt::Unknown => { self.record_boundary_barrier(target, &Obligations::new()); None },
            Debt::Void => Some(Violation::Void),
            Debt::Owed { obligations, definite, .. } if obligations.contains(&self.ctx.sigs.opt) => {
                Some(if *definite { Violation::Null } else { Violation::Nullable })
            },
            Debt::Owed { .. } => None,
        }
    }

    /// Checks a value entering a slot against the obligations the slot accepts.
    pub(super) fn check_into_slot(&mut self, debt: &Debt, accepted: &Obligations, name: Symbol, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let text = self.ctx.binding_display_name(name);
        let noun = if self.ctx.is_factory_field(name) { "field" } else { "binding" };
        let void = || format!("Cannot assign a void result to '{text}'; the call returns no value");

        if debt.is_void() {
            return Err(self.error(void(), node));
        }

        // An unknown value is guarded against every witness the slot does not accept.
        if matches!(debt, Debt::Unknown) {
            self.record_boundary_barrier(node, accepted);
            return Ok(());
        }

        let undeclared = self.ctx.unadmitted_obligations(debt, accepted);
        if !undeclared.is_empty() {
            let owed = quoted_obligation_list(self.ctx.hir, &undeclared);
            return Err(self.error_help(format!("cannot assign a value owing {owed} to '{text}'"), node,
                format!("discharge it first, or declare it on the {noun} (`{text}: {}`)", obligation_atoms(self.ctx.hir, &undeclared))));
        }

        if accepted.contains(&self.ctx.sigs.opt) {
            return Ok(());
        }

        match self.non_null_violation(debt, node) {
            None => Ok(()),
            Some(Violation::Void) => Err(self.error(void(), node)),
            Some(Violation::Null) => Err(self.error(format!("Cannot assign null to non-null {noun} '{text}'"), node)),
            Some(Violation::Nullable) => Err(self.error(format!("Cannot assign a nullable value to non-null {noun} '{text}'"), node)),
        }
    }
}

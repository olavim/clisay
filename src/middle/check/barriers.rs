use crate::middle::diagnose::Diagnose;
use crate::middle::obligations::{obligation_atoms, quoted_obligation_list};
use std::collections::HashMap;

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
    /// A `!` operand owing only `opt`.
    NonNull,
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
    /// Every registered object witness declaration, the VM's registry for recognizing a crossing
    /// value as a witness at a boundary barrier.
    pub(super) witness_decls: Vec<TypeId>,
}

impl Barriers {
    pub fn guards(&self, node: &HirId<HirExpr>) -> &[Guard] {
        self.guards.get(node).map_or(&[], Vec::as_slice)
    }

    pub fn elided(&self, node: &HirId<HirExpr>) -> &[Guard] {
        self.elided.get(node).map_or(&[], Vec::as_slice)
    }

    pub fn boundary(&self, node: &HirId<HirExpr>) -> Option<&Barrier> {
        self.boundary_barriers.get(node)
    }

    pub fn witness_decls(&self) -> &[TypeId] {
        &self.witness_decls
    }

    pub fn witness_set(&self, node: &HirId<HirExpr>) -> Option<&WitnessSet> {
        self.witness_tests.get(node)
    }

    pub fn len(&self) -> usize {
        self.guards.len()
    }

    pub fn is_empty(&self) -> bool {
        self.guards.is_empty()
    }
}

impl<'a> Checker<'a> {
    pub(super) fn record_elision(&mut self, node: &HirId<HirExpr>, guard: Guard) {
        if !self.ctx.force_checks {
            return;
        }
        let elided = self.out.elided.entry(*node).or_default();
        if let Err(at) = elided.binary_search(&guard) {
            elided.insert(at, guard);
        }
    }

    pub(super) fn record_guard(&mut self, node: &HirId<HirExpr>, guard: Guard) {
        let guards = self.out.guards.entry(*node).or_default();
        if let Err(at) = guards.binary_search(&guard) {
            guards.insert(at, guard);
        }
    }

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

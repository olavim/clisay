//! Runtime checks.

use std::collections::{HashMap, HashSet};

use crate::middle::hir::{HirExpr, HirId, Symbol};

use super::{Checker, Flow, Violation};

/// The runtime witnesses a discharge node must test: `null` for `opt`, and one type/trait name
/// per object witness.
#[derive(Clone)]
pub struct WitnessSet {
    pub null: bool,
    pub names: Vec<Symbol>,
    /// Whether the set names a witness other than the built-in `Err`, so codegen must use the
    /// `is` test rather than the fast bad/clean ops.
    pub contains_user_witnesses: bool,
}

/// The witnesses a destination allows (a slot or a `!`). Its boundary guard throws any registered
/// witness it does not allow when an unknown value reaches it.
pub struct Barrier {
    pub null_allowed: bool,
    pub allow_names: Vec<Symbol>,
}

/// The runtime checks codegen emits. A barrier tests an unknown value against the witnesses its
/// destination does not allow, whether the value enters a slot or is asserted clean by `!`.
#[derive(Default)]
pub struct Barriers {
    /// The null fast path: a `!` operand owing only `opt`, where a null check alone suffices.
    pub(super) null_barriers: HashSet<HirId<HirExpr>>,
    /// An unknown value guarded against the witnesses its destination does not allow: a value
    /// entering a slot, or a `!` on an unknown operand.
    pub(super) boundary_barriers: HashMap<HirId<HirExpr>, Barrier>,
    /// Discharge nodes (`??`, `?`, `!`) whose operand owes an object witness.
    pub(super) witness_tests: HashMap<HirId<HirExpr>, WitnessSet>,
    /// Every registered object witness name, the VM's registry for recognizing a crossing value
    /// as a witness at a boundary barrier.
    pub(super) witness_names: Vec<Symbol>,
}

impl Barriers {
    /// Whether a `!` operand at this node needs the built-in null assertion.
    pub fn has(&self, node: &HirId<HirExpr>) -> bool {
        self.null_barriers.contains(node)
    }

    /// The boundary guard for an unknown value at this node, if one is needed.
    pub fn boundary(&self, node: &HirId<HirExpr>) -> Option<&Barrier> {
        self.boundary_barriers.get(node)
    }

    /// Every registered object witness name, for the VM's boundary-barrier registry.
    pub fn witness_names(&self) -> &[Symbol] {
        &self.witness_names
    }

    /// The witness set a discharge node tests, when its operand owes an object witness.
    pub fn witness_set(&self, node: &HirId<HirExpr>) -> Option<&WitnessSet> {
        self.witness_tests.get(node)
    }

    pub fn len(&self) -> usize {
        self.null_barriers.len() + self.boundary_barriers.len()
    }

    pub fn is_empty(&self) -> bool {
        self.null_barriers.is_empty() && self.boundary_barriers.is_empty()
    }
}

impl<'a> Checker<'a> {
    /// Marks a `!` operand owing only `opt`, whose null state is asserted at runtime.
    pub(super) fn add_barrier(&mut self, node: &HirId<HirExpr>) {
        self.barriers.insert(*node);
    }

    /// Records the guard for an unknown value reaching a destination accepting `accepted`. The
    /// guard allows those obligations' witnesses.
    pub(super) fn record_boundary_barrier(&mut self, node: &HirId<HirExpr>, accepted: &HashSet<Symbol>) {
        let null_allowed = accepted.contains(&self.sigs.opt);
        let mut allow_names = Vec::new();
        for (ob, name) in self.sigs.object_witnesses() {
            if accepted.contains(&ob) && !allow_names.contains(&name) {
                allow_names.push(name);
            }
        }
        self.boundary_barriers.insert(*node, Barrier { null_allowed, allow_names });
    }

    /// Classifies a value entering a non-null target. A non-null slot forbids `opt`, so only a
    /// value owing `opt` violates it. An unknown value records the non-null boundary guard.
    pub(super) fn non_null_violation(&mut self, value: &Flow, target: &HirId<HirExpr>) -> Option<Violation> {
        match value {
            Flow::Clean => None,
            Flow::Unknown => { self.record_boundary_barrier(target, &HashSet::new()); None },
            Flow::Void => Some(Violation::Void),
            Flow::Bad { obligations, definite, .. } if obligations.contains(&self.sigs.opt) => {
                Some(if *definite { Violation::Null } else { Violation::Nullable })
            },
            Flow::Bad { .. } => None,
        }
    }

    /// Checks a value entering a slot against the obligations the slot accepts.
    pub(super) fn check_into_slot(&mut self, flow: &Flow, accepted: &HashSet<Symbol>, name: Symbol, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let text = self.hir.text(name);
        let void = || format!("Cannot assign a void result to '{text}'; the call returns no value");
        if flow.is_void() {
            return Err(self.error(void(), node));
        }
        // An unknown value is guarded against every witness the slot does not accept.
        if matches!(flow, Flow::Unknown) {
            self.record_boundary_barrier(node, accepted);
            return Ok(());
        }
        if accepted.contains(&self.sigs.opt) {
            return Ok(());
        }
        match self.non_null_violation(flow, node) {
            None => Ok(()),
            Some(Violation::Void) => Err(self.error(void(), node)),
            Some(Violation::Null) => Err(self.error(format!("Cannot assign null to non-null binding '{text}'"), node)),
            Some(Violation::Nullable) => Err(self.error(format!("Cannot assign a nullable value to non-null binding '{text}'"), node)),
        }
    }
}

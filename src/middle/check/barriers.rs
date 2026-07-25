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
    /// Opaque calls whose argument must survive, keyed by callee node to the argument positions
    /// the callee must borrow.
    pub(super) survive_barriers: HashMap<HirId<HirExpr>, Vec<u8>>,
    /// Calls that lend a mutable argument (callee node -> argument positions) to mark
    /// as borrowed for the call.
    pub(super) borrow_marks: HashMap<HirId<HirExpr>, Vec<u8>>,
    /// Every registered object witness name, the VM's registry for recognizing a crossing value
    /// as a witness at a boundary barrier.
    pub(super) witness_names: Vec<Symbol>,
    /// Immutable container literals with an unknown-capability element, whose elements are checked
    /// for mutability at construction so a mutable value cannot land in an immutable container.
    pub(super) seal_checks: HashSet<HirId<HirExpr>>,
    /// Plain (immutable) constructions, whose fields are deep-frozen once the object is sealed so an
    /// immutable instance is immutable all the way down.
    pub(super) deep_seals: HashSet<HirId<HirExpr>>,
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

    /// The argument positions an opaque call at this callee must assert the callee borrows.
    pub fn survive(&self, callee: &HirId<HirExpr>) -> Option<&[u8]> {
        self.survive_barriers.get(callee).map(Vec::as_slice)
    }

    /// The argument positions a call at this callee lends, to mark as borrowed for the call.
    pub fn borrow_marks(&self, callee: &HirId<HirExpr>) -> Option<&[u8]> {
        self.borrow_marks.get(callee).map(Vec::as_slice)
    }

    /// Whether this container literal needs a runtime check that no element is mutable.
    pub fn needs_seal_check(&self, node: &HirId<HirExpr>) -> bool {
        self.seal_checks.contains(node)
    }

    /// Whether this construction seals into an immutable object whose fields need deep-freezing.
    pub fn needs_deep_seal(&self, node: &HirId<HirExpr>) -> bool {
        self.deep_seals.contains(node)
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

    /// Records that an opaque call must assert its callee borrows the given argument positions.
    pub(super) fn record_survive_barrier(&mut self, callee: &HirId<HirExpr>, positions: Vec<u8>) {
        self.survive_barriers.insert(*callee, positions);
    }

    /// Records the argument positions a call lends, to mark as borrowed for its duration.
    pub(super) fn record_borrow_marks(&mut self, callee: &HirId<HirExpr>, positions: Vec<u8>) {
        if !positions.is_empty() {
            self.borrow_marks.insert(*callee, positions);
        }
    }

    /// Marks an immutable container literal whose elements must be checked for mutability at runtime.
    pub(super) fn record_seal_check(&mut self, node: &HirId<HirExpr>) {
        self.seal_checks.insert(*node);
    }

    /// Marks a plain construction whose fields deep-freeze once the object is sealed.
    pub(super) fn record_deep_seal(&mut self, node: &HirId<HirExpr>) {
        self.deep_seals.insert(*node);
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

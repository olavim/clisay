//! Object-witness flow: recognizing values that owe a type/trait witness and the runtime tests
//! codegen must emit for them.

use std::collections::HashSet;

use crate::middle::signatures::{TypeTag, Witness};
use crate::middle::hir::{HirExpr, HirId, Symbol};

use super::{Checker, Flow, Typed, WitnessSet};

impl<'a> Checker<'a> {
    /// A value owing `fails`: an `Err` witness.
    pub(super) fn fails_flow(&self) -> Flow {
        Flow::Bad { obligations: HashSet::from([self.sigs.fails]), definite: false, container: false }
    }

    pub(super) fn owes_object_witness(&self, flow: &Flow) -> bool {
        matches!(flow, Flow::Bad { obligations, .. }
            if obligations.iter().any(|o| matches!(self.sigs.witness(*o), Some(Witness::Type(_) | Witness::Trait(_)))))
    }

    /// The type witness of the built-in `fails` obligation.
    fn err_witness(&self) -> Option<Symbol> {
        match self.sigs.witness(self.sigs.fails) {
            Some(Witness::Type(e)) => Some(*e),
            _ => None,
        }
    }

    /// Records which witnesses a discharge node must test at runtime.
    pub(super) fn record_witness_test(&mut self, node: &HirId<HirExpr>, flow: &Flow) {
        let Flow::Bad { obligations, .. } = flow else { return };
        let err = self.err_witness();
        let mut set = WitnessSet { null: false, names: Vec::new(), contains_user_witnesses: false };
        for &o in obligations {
            match self.sigs.witness(o) {
                Some(Witness::Null) => set.null = true,
                Some(Witness::Type(w) | Witness::Trait(w)) => {
                    if !set.names.contains(w) {
                        set.names.push(*w);
                    }
                    if Some(*w) != err { set.contains_user_witnesses = true; }
                },
                None => {},
            }
        }
        // A recorded set always names an object witness: callers only record when
        // `owes_object_witness` holds. Codegen relies on this to fast-path an opt-only operand.
        debug_assert!(!set.names.is_empty(), "witness test recorded with no object witness");
        self.witness_tests.insert(*node, set);
    }

    /// Whether a value is a container carrying element obligations.
    pub(super) fn is_container(flow: &Flow) -> bool {
        matches!(flow, Flow::Bad { container: true, .. })
    }

    /// The witness type name when the value is confirmed to be a witness it owes.
    pub(super) fn confirmed_witness_name(&self, typed: &Typed) -> Option<&'a str> {
        let TypeTag::Concrete(tag) = &typed.tag else { return None };
        let Flow::Bad { obligations, .. } = &typed.flow else { return None };
        if !obligations.iter().all(|o| matches!(self.sigs.witness(*o), Some(Witness::Type(_) | Witness::Trait(_)))) {
            return None;
        }
        obligations.iter().find_map(|o| match self.sigs.witness(*o) {
            Some(Witness::Type(w)) if w == tag => Some(self.hir.text(*w)),
            _ => None,
        })
    }

    /// Whether the value is confirmed to be one of the witnesses it owes.
    pub(super) fn confirmed_witness(&self, typed: &Typed) -> bool {
        self.confirmed_witness_name(typed).is_some()
    }

    /// The tag a caught value narrows to. A single type witness confirms the value's type. A set,
    /// a trait witness, or `opt` leaves it unknown.
    pub(super) fn single_object_witness_tag(&self, caught: &HashSet<Symbol>) -> TypeTag {
        let mut it = caught.iter();
        match (it.next(), it.next()) {
            (Some(o), None) => match self.sigs.witness(*o) {
                Some(Witness::Type(name)) => TypeTag::Concrete(*name),
                _ => TypeTag::Unknown,
            },
            _ => TypeTag::Unknown,
        }
    }

    /// The result of a `?` chain. It carries the operand's obligations, since a bad operand
    /// short-circuits to that value. `opt` is always added because the chain can also yield null on
    /// the clean path: a null short-circuit when the operand owes `opt`, or a nullable/dynamic
    /// access. The exact member flow is discarded, so this is conservative but never unsound.
    pub(super) fn chain_result(&mut self, operand: &Flow, node: &HirId<HirExpr>) -> Typed {
        if self.owes_object_witness(operand) {
            self.record_witness_test(node, operand);
        }
        let mut obligations = match operand {
            Flow::Bad { obligations, .. } => obligations.clone(),
            _ => HashSet::new(),
        };
        obligations.insert(self.sigs.opt);
        Typed::of(Flow::Bad { obligations, definite: false, container: false }, TypeTag::Unknown)
    }
}

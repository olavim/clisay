use crate::middle::diagnose::Diagnose;
use crate::middle::obligations::{obligation_atoms, quoted_obligation_list};
use std::collections::{HashMap, HashSet};

use crate::middle::hir::{HirExpr, HirId, HirMatcher, HirStmt, Symbol, TypeId};
use crate::middle::signatures::CallableId;
use crate::middle::obligations::Obligations;

use super::{Checker, Debt};

/// The witnesses a slot allows.
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

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum CheckedSlot {
    Say(HirId<HirStmt>),
    Param(HirId<HirExpr>),
    PatternBinding(HirId<HirMatcher>, Symbol),
    Receiver(HirId<HirExpr>),
}

#[derive(Default)]
pub struct Barriers {
    /// Each assignment's target => how it orders its keys and value against the walk.
    pub(super) assign_strategies: fnv::FnvHashMap<HirId<HirExpr>, super::AssignStrategy>,
    /// Every per-node runtime check, in the order codegen emits them.
    pub(super) guards: HashMap<HirId<HirExpr>, Vec<Guard>>,
    /// Checks the pass proved unnecessary, recorded only under check-forcing.
    pub(super) elided: HashMap<HirId<HirExpr>, Vec<Guard>>,
    /// An unknown value entering a slot, guarded against the witnesses the slot does not allow.
    pub(super) boundary_barriers: HashMap<HirId<HirExpr>, Barrier>,
    /// Each `!` whose operand may be an object witness.
    pub(super) witness_asserts: HashSet<HirId<HirExpr>>,
    /// Every registered object witness declaration, the VM's registry for recognizing a crossing
    /// value as a witness at a boundary barrier.
    pub(super) witness_decls: Vec<TypeId>,
    /// Each expression that builds its value rather than reading one.
    pub(super) fresh_values: HashSet<HirId<HirExpr>>,
    /// Each call this pass decided every argument of.
    pub(super) settled_args: HashSet<HirId<HirExpr>>,
    /// Each call whose arguments are places exactly where the callee expects them.
    pub(super) proven_arg_kinds: HashSet<HirId<HirExpr>>,
    /// Each callable with a return that handed back nothing.
    pub(super) void_returns: HashSet<CallableId>,
    /// Each callable that returns a value.
    pub(super) value_returns: HashSet<CallableId>,
    /// Each callable with a return that handed back a value. Only this pass reads it, to tell a
    /// function that hands back nothing from one whose returns disagree.
    pub(super) valued_returns: HashSet<CallableId>,
    /// Each returned value this pass proved the function's return allows.
    pub(super) proven_returns: HashSet<HirId<HirExpr>>,
    /// Each expression statement whose value this pass proved can be discarded without a check.
    pub(super) discardable_values: HashSet<HirId<HirExpr>>,
    pub(super) checked_slot_clauses: HashMap<CheckedSlot, Obligations>,
    /// Each returned expression that hands back nothing, the way falling off the end does.
    pub(super) void_return_sites: HashSet<HirId<HirExpr>>,
    /// Whether check-forcing puts a test back on a return the pass proved.
    pub(super) force_return_tests: bool,
    /// Declarations used above their line, as `a` uses `b` in `fn a() { return b(); } fn b() {}`,
    /// or `say k = T;` does above `type T`.
    pub(super) forward_referenced: HashSet<usize>,
}

impl Barriers {
    pub fn returns_void(&self, callable: CallableId) -> bool {
        self.void_returns.contains(&callable)
    }

    pub fn returns_value(&self, callable: CallableId) -> bool {
        self.value_returns.contains(&callable)
    }

    pub fn return_is_proven(&self, value: &HirId<HirExpr>) -> bool {
        self.proven_returns.contains(value)
    }

    pub fn value_is_discardable(&self, value: &HirId<HirExpr>) -> bool {
        self.discardable_values.contains(value)
    }

    pub fn return_hands_back_nothing(&self, value: &HirId<HirExpr>) -> bool {
        self.void_return_sites.contains(value)
    }

    pub fn forces_return_tests(&self) -> bool {
        self.force_return_tests
    }

    pub fn guards(&self, node: &HirId<HirExpr>) -> &[Guard] {
        self.guards.get(node).map_or(&[], Vec::as_slice)
    }

    pub fn elided(&self, node: &HirId<HirExpr>) -> &[Guard] {
        self.elided.get(node).map_or(&[], Vec::as_slice)
    }

    pub fn boundary(&self, node: &HirId<HirExpr>) -> Option<&Barrier> {
        self.boundary_barriers.get(node)
    }

    pub fn is_forward_referenced(&self, decl: &HirId<HirStmt>) -> bool {
        self.forward_referenced.contains(&decl.index())
    }

    pub fn assign_strategy(&self, write: &HirId<HirExpr>) -> &super::AssignStrategy {
        self.assign_strategies.get(write).expect("every assignment has a strategy")
    }

    pub fn builds_its_value(&self, expr: &HirId<HirExpr>) -> bool {
        self.fresh_values.contains(expr)
    }

    pub fn args_settled(&self, callee: &HirId<HirExpr>) -> bool {
        self.settled_args.contains(callee)
    }

    pub fn checked_slot_clause(&self, slot: CheckedSlot) -> Option<&Obligations> {
        self.checked_slot_clauses.get(&slot)
    }

    pub fn arg_kinds_proven(&self, callee: &HirId<HirExpr>) -> bool {
        self.proven_arg_kinds.contains(callee)
    }

    pub fn witness_decls(&self) -> &[TypeId] {
        &self.witness_decls
    }

    pub fn asserts_witnesses(&self, node: &HirId<HirExpr>) -> bool {
        self.witness_asserts.contains(node)
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

    /// Checks a value entering a slot against the obligations the slot accepts.
    pub(super) fn check_into_slot(&mut self, debt: &Debt, accepted: &Obligations, name: Symbol, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let noun = if self.ctx.is_factory_field(name) { "field" } else { "binding" };
        self.check_into_named_slot(debt, accepted, name, noun, node)
    }

    fn obligations_refused_by_slot(&mut self, debt: &Debt, accepted: &Obligations, node: &HirId<HirExpr>) -> Option<Obligations> {
        if matches!(debt, Debt::Unknown) {
            self.record_boundary_barrier(node, accepted);
            return None;
        }
        let refused = self.ctx.unadmitted_obligations(debt, accepted);
        (!refused.is_empty()).then_some(refused)
    }

    /// Check `this = v` against what the receiver owes.
    pub(super) fn check_into_this(&mut self, debt: &Debt, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let owed = self.fn_ctx.receiver.clone();
        let slot = Slot::Named { text: "this".to_string(), noun: "receiver".to_string() };
        self.check_into(debt, &owed, &slot, node)
    }

    /// Check `@t = v`.
    pub(super) fn check_into_ref(&mut self, debt: &Debt, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let admits = self.ctx.ref_admits();
        self.check_into(debt, &admits, &Slot::Ref, node)
    }

    pub(super) fn check_into_named_slot(&mut self, debt: &Debt, accepted: &Obligations, name: Symbol, noun: &str, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let slot = Slot::Named { text: self.ctx.binding_display_name(name), noun: noun.to_string() };
        self.check_into(debt, accepted, &slot, node)
    }

    /// Whether the slot takes what it is handed, worded for the slot the caller named.
    fn check_into(&mut self, debt: &Debt, accepted: &Obligations, slot: &Slot, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        if debt.is_void() {
            return Err(self.error(slot.assign_void_error(), node));
        }

        let Some(refused) = self.obligations_refused_by_slot(debt, accepted, node) else {
            return Ok(());
        };

        let nullable = self.ctx.only_nullable(&refused);
        let msg = match (nullable, debt.is_definite()) {
            (true, definite) => slot.assign_null_error(definite),
            (false, _) => slot.assign_owing_error(&quoted_obligation_list(self.ctx.hir, &refused)),
        };
        match self.invalidated_narrowing_note(node, &refused) {
            Some(help) => Err(self.error_help(msg, node, help)),
            None if nullable => Err(self.error(msg, node)),
            None => Err(self.error_help(msg, node, slot.discharge_error_help(obligation_atoms(self.ctx.hir, &refused)))),
        }
    }
}

/// The slot a value is stored into, for the refusal to name. A `Ref` has no member name a program
/// can write, so it is described rather than quoted.
enum Slot {
    Named { text: String, noun: String },
    Ref,
}

impl Slot {
    fn assign_void_error(&self) -> String {
        match self {
            Slot::Named { text, .. } => format!("Cannot assign a void result to '{text}'; the call returns no value"),
            Slot::Ref => "Cannot store a void result in a `Ref`; the call returns no value".to_string(),
        }
    }

    fn assign_null_error(&self, definite: bool) -> String {
        match (self, definite) {
            (Slot::Named { text, noun }, true) => format!("Cannot assign null to non-null {noun} '{text}'"),
            (Slot::Named { text, noun }, false) => format!("Cannot assign a nullable value to non-null {noun} '{text}'"),
            (Slot::Ref, true) => "Cannot store null in a `Ref`".to_string(),
            (Slot::Ref, false) => "Cannot store a nullable value in a `Ref`".to_string(),
        }
    }

    fn assign_owing_error(&self, owed: &str) -> String {
        match self {
            Slot::Named { text, .. } => format!("cannot assign a value owing {owed} to '{text}'"),
            Slot::Ref => format!("cannot store a value owing {owed} in a `Ref`"),
        }
    }

    fn discharge_error_help(&self, atoms: String) -> String {
        match self {
            Slot::Named { text, noun } => format!("discharge it first, or declare it on the {noun} (`{text}: {atoms}`)"),
            Slot::Ref => "discharge it before the write".to_string(),
        }
    }
}

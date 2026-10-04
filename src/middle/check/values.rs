//! The value an expression walk hands back, and what its parent expression does with that value.

use crate::middle::hir::{HirExpr, HirId};
use crate::middle::signatures::TypeTag;

use super::{Checker, Debt, FlowPath, PathMap, PossibleFacts, ProvenFacts};

#[derive(Clone)]
pub(super) struct ValueState {
    pub(super) debt: Debt,
    pub(super) tag: TypeTag,
    pub(super) stored: bool,
    pub(super) proven: PathMap<ProvenFacts>,
    pub(super) possible: PathMap<PossibleFacts>,
}

impl ValueState {
    pub(super) fn unknown() -> ValueState {
        ValueState::of(Debt::Unknown, TypeTag::Unknown)
    }

    pub(super) fn nonnull() -> ValueState {
        ValueState::of(Debt::Clean, TypeTag::Unknown)
    }

    /// The same value with its root debt discharged.
    pub(super) fn as_clean(&self) -> ValueState {
        self.with_debt(Debt::Clean)
    }

    pub(super) fn with_debt(&self, debt: Debt) -> ValueState {
        ValueState::of(debt, self.tag.clone()).proving(self.proven.clone()).possibly(self.possible.clone())
    }

    pub(super) fn coalesced(left: ValueState, right: ValueState) -> ValueState {
        let tag = if left.tag == right.tag { left.tag } else { TypeTag::Unknown };
        let debt = if matches!(left.debt, Debt::Clean) { Debt::Clean } else { right.debt };
        let mut possible = left.possible;
        possible.join(&right.possible);
        ValueState::of(debt, tag).possibly(possible)
    }

    pub(super) fn of(debt: Debt, tag: TypeTag) -> ValueState {
        ValueState { debt, tag, stored: false, proven: PathMap::new(), possible: PathMap::new() }
    }

    pub(super) fn possibly_callable(mut self, callable: Option<usize>) -> ValueState {
        if let Some(callable) = callable {
            self.possible.entry(FlowPath::new()).or_default().callables.insert(callable);
        }
        self
    }

    pub(super) fn possibly(mut self, possible: PathMap<PossibleFacts>) -> ValueState {
        self.possible = possible;
        self
    }

    pub(super) fn as_stored(mut self) -> ValueState {
        self.stored = true;
        self
    }

    pub(super) fn proving(mut self, proven: PathMap<ProvenFacts>) -> ValueState {
        self.proven = proven;
        self
    }
}

/// The value of a walked expression. The parent that walked it has to say what it does with the
/// value before it can have it, so no value reaches a place, where this pass doesn't follow, unchecked.
#[must_use]
pub(super) struct UnroutedValueState(ValueState);

impl UnroutedValueState {
    pub(super) fn new(state: ValueState) -> UnroutedValueState {
        UnroutedValueState(state)
    }

    pub(super) fn debt(&self) -> &Debt {
        &self.0.debt
    }

    pub(super) fn tag(&self) -> &TypeTag {
        &self.0.tag
    }

    /// Hands the value on for `route`. A route that takes the value out of this pass's sight
    /// refuses a callable that is not ready yet.
    pub(super) fn route(self, c: &Checker, at: &HirId<HirExpr>, route: Route) -> Result<ValueState, anyhow::Error> {
        let state = self.0;
        match route {
            Route::CoalesceOperand
                | Route::Asserted
                | Route::Propagated
                | Route::MemberBase
                | Route::ContainerElement
                | Route::BraceField
                | Route::AssignmentResult
                | Route::NewLocal => {},
            Route::Reassigned(i) | Route::StoredInside(i) => c.refuse_unready_for_defer(i, &state.possible, at)?,
            Route::Returned => c.refuse_unready(&state.possible.callables(), at, "the caller may call it")?,
            Route::Thrown => c.refuse_unready(&state.possible.callables(), at, "whoever catches it may call it")?,
            Route::Argument => c.refuse_unready(&state.possible.callables(), at, "the function it is passed to may call it")?,
            Route::Receiver => c.refuse_unready(&state.possible.callables(), at, "the method called on it may call it")?,
            Route::Matched => c.refuse_unready(&state.possible.callables(), at, "match it below that declaration")?,
            Route::WrittenThroughAnchor
                | Route::ThisReassigned
                | Route::StoredInThis
                | Route::StoredInRef
                | Route::StoredInGlobal
                | Route::StoredInTemporary => c.refuse_unready(&state.possible.callables(), at,
                    "other code could call it from there; store it below that declaration")?,
            Route::Callee => c.refuse_unready(&state.possible.root_callables(), at, "call it below that declaration")?,
            // Nothing past here can hold the value, so what it may hold goes no further.
            Route::BinaryOperand
                | Route::UnaryOperand
                | Route::CompoundAssignTarget
                | Route::Condition
                | Route::StatementResult
                | Route::Body
                | Route::MemberKey
                | Route::RefHolder
                | Route::WriteTarget => return Ok(ValueState { possible: PathMap::new(), ..state }),
        }
        Ok(state)
    }
}

/// What a walked expression's value is for, in the expression around it.
pub(super) enum Route {
    // The value becomes the parent's value, or part of it.
    CoalesceOperand,
    Asserted,
    Propagated,
    MemberBase,
    ContainerElement,
    BraceField,
    AssignmentResult,

    // The value is kept in a local this pass follows.
    NewLocal,
    Reassigned(usize),
    StoredInside(usize),

    // The value goes where this pass cannot follow it.
    Returned,
    Thrown,
    Argument,
    Receiver,
    Matched,
    WrittenThroughAnchor,
    ThisReassigned,
    StoredInThis,
    StoredInRef,
    StoredInGlobal,
    StoredInTemporary,
    Callee,

    // The value is maybe read, but goes no further.
    BinaryOperand,
    UnaryOperand,
    CompoundAssignTarget,
    Condition,
    StatementResult,
    Body,
    MemberKey,
    RefHolder,
    WriteTarget,
}

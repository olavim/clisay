//! Whether a value may be used here: rejecting void results, undischarged obligations, and
//! escaping values.

use std::collections::HashSet;

use anyhow::anyhow;

use crate::frontend::lex::Diagnostic;

use crate::middle::signatures::{TypeTag, Witness};
use crate::middle::hir::{BinOp, HirExpr, HirId, HirLiteral, Symbol};

use super::{Checker, Flow, Typed};

impl<'a> Checker<'a> {
    /// A single-caret error for a confirmed witness used where its type is not allowed.
    pub(super) fn confirmed_use_error(&self, header: String, operand: &HirId<HirExpr>, witness: &str) -> anyhow::Error {
        anyhow!("{}", Diagnostic::new(header, self.hir.pos(operand).clone()).with_label(witness.to_string()))
    }

    pub(super) fn require_usable_value(&self, typed: &Typed, operand: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        debug_assert!(!self.confirmed_witness(typed), "a confirmed witness must be handled at its operation site, not advised to narrow");
        if typed.flow.is_void() {
            return Err(self.error("This call returns no value, so its result cannot be used here".to_string(), operand));
        }

        let Flow::Bad { obligations, .. } = &typed.flow else { return Ok(()) };

        let blocking: HashSet<Symbol> = obligations.iter().copied().filter(|o| !self.sigs.is_no_persist(*o)).collect();
        if blocking.is_empty() {
            return Ok(());
        }

        let name = match self.hir.get(operand) {
            HirExpr::Identifier(name) => Some(self.hir.text(*name)),
            _ => None,
        };

        let owed = self.quoted_obligation_list(&blocking);

        // The header stays generic so it is easy to search for. The caret and help carry the name.
        let mut diagnostic = Diagnostic::new(format!("unchecked value owes {owed}"), self.hir.pos(operand).clone());

        // Name the witness so the reader knows what to rule out, and how.
        let mut witnesses: Vec<&str> = blocking.iter().filter_map(|o| self.witness_name(*o)).collect();
        witnesses.sort();
        witnesses.dedup();

        if witnesses.is_empty() {
            diagnostic = diagnostic.with_help("discharge it before use");
        } else {
            let witness = witnesses.join(" or ");
            diagnostic = diagnostic.with_label(format!("might be {witness}"));
            diagnostic = match name {
                Some(name) => diagnostic.with_help(format!("make sure `{name}` is not {witness} before using it")),
                None => diagnostic.with_help(format!("make sure the value is not {witness} before using it")),
            };
        }

        Err(anyhow!("{}", diagnostic))
    }

    /// The `no persist` obligations in a set: usable in place, but rejected at a persist site.
    fn no_persist_only(&self, obligations: &HashSet<Symbol>) -> HashSet<Symbol> {
        obligations.iter().copied().filter(|o| self.sigs.is_no_persist(*o)).collect()
    }

    /// Whether any of these obligations is `no persist`.
    pub(super) fn owes_no_persist(&self, obligations: impl IntoIterator<Item = Symbol>) -> bool {
        obligations.into_iter().any(|o| self.sigs.is_no_persist(o))
    }

    /// Rejects persisting a value that owes a `no persist` obligation.
    pub(super) fn reject_escape(&self, flow: &Flow, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let Flow::Bad { obligations, .. } = flow else { return Ok(()) };
        let escaping = self.no_persist_only(obligations);
        if escaping.is_empty() {
            return Ok(());
        }
        let subject = self.quoted_subject(node);
        let owed = self.quoted_obligation_list(&escaping);
        Err(self.error(format!("{subject} owes {owed}; it cannot be stored or escape its scope"), node))
    }

    /// The flow after a discharge operator. A witnessed obligation is cleared by proof, but a
    /// `no persist` obligation has no witness to prove, so it stays.
    pub(super) fn discharged_flow(&self, flow: &Flow) -> Flow {
        let Flow::Bad { obligations, .. } = flow else { return Flow::Clean };
        let kept = self.no_persist_only(obligations);
        if kept.is_empty() {
            Flow::Clean
        } else {
            Flow::Bad { obligations: kept, definite: false, container: false }
        }
    }

    pub(super) fn invalid_operands(&self, op: BinOp, l: &HirId<HirExpr>, ln: &Typed, r: &HirId<HirExpr>, rn: &Typed) -> anyhow::Error {
        let (lt, rt) = (self.operand_type_name(ln, l), self.operand_type_name(rn, r));
        let header = format!("invalid operands of `{op}`: {lt} and {rt}");
        // Point the primary caret at the confirmed operand; the other side is a labeled span.
        let (primary, primary_ty, other, other_ty) = if self.confirmed_witness(ln) {
            (l, &lt, r, &rt)
        } else {
            (r, &rt, l, &lt)
        };
        anyhow!("{}", Diagnostic::new(header, self.hir.pos(primary).clone())
            .with_label(primary_ty.to_string())
            .with_span(self.hir.pos(other).clone(), other_ty.to_string()))
    }

    fn operand_type_name(&self, typed: &Typed, node: &HirId<HirExpr>) -> String {
        if let Some(witness) = self.confirmed_witness_name(typed) {
            return witness.to_string();
        }
        match self.hir.get(node) {
            HirExpr::Literal(HirLiteral::Number(_)) => "number".to_string(),
            HirExpr::Literal(HirLiteral::String(_)) => "string".to_string(),
            HirExpr::Literal(HirLiteral::Boolean(_)) => "boolean".to_string(),
            HirExpr::Literal(HirLiteral::Null) => "null".to_string(),
            _ => match &typed.tag {
                TypeTag::Concrete(name) => self.hir.text(*name).to_string(),
                _ => "value".to_string(),
            },
        }
    }

    /// The type or trait that witnesses an obligation at runtime, if it has one.
    fn witness_name(&self, obligation: Symbol) -> Option<&'a str> {
        match self.sigs.witness(obligation)? {
            Witness::Type(name) | Witness::Trait(name) => Some(self.hir.text(*name)),
            Witness::Null => Some("null"),
        }
    }
}

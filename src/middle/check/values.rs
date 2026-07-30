//! Whether a value may be used here: rejecting void results, undischarged obligations, and
//! escaping values.

use anyhow::anyhow;

use crate::frontend::lex::Diagnostic;

use crate::middle::signatures::{TypeTag, Witness};
use crate::middle::hir::{builtin_obligation_rules, BinOp, HirExpr, HirId, HirLiteral, Symbol};
use crate::middle::obligations::Obligations;

use super::{Checker, Flow, Rule, Site, Typed};

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

        let blocking: Obligations = obligations.iter().copied().filter(|o| self.sigs.rules_of(*o).to_use).collect();
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

    /// The obligations a value owes that a slot does not admit. `opt` is left out. The null axis has
    /// its own message at every slot, so reporting it here would say the same thing twice.
    pub(super) fn undeclared_obligations(&self, flow: &Flow, admits: &Obligations) -> Obligations {
        let Flow::Bad { obligations, .. } = flow else { return Obligations::new() };
        obligations.difference(admits).copied().filter(|o| *o != self.sigs.opt).collect()
    }

    /// The obligations in a set that a rule forbids.
    pub(super) fn owing_rule(&self, obligations: &Obligations, rule: Rule) -> Obligations {
        obligations.iter().copied().filter(|o| rule.holds(&self.sigs.rules_of(*o))).collect()
    }

    /// Refuses an operation the rule forbids on this value.
    pub(super) fn reject_at(&self, flow: &Flow, rule: Rule, site: Site, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let Flow::Bad { obligations, .. } = flow else { return Ok(()) };
        let blocked = self.owing_rule(obligations, rule);
        if blocked.is_empty() {
            return Ok(());
        }
        let owed = self.quoted_obligation_list(&blocked);
        let help = self.prohibition_help(&blocked, rule, site);
        Err(self.error_help(site.refusal(&owed), node, help))
    }

    /// The declaration a rule-based prohibition comes from, for the reader to go and read. A
    /// built-in obligation has none, so there is nothing to cite.
    fn rule_citation(&self, obligations: &Obligations, rule: Rule) -> Option<String> {
        let declared: Vec<String> = self.sorted_obligation_names(obligations).into_iter()
            .filter(|name| builtin_obligation_rules(name).is_none())
            .map(|name| format!("`{name}`"))
            .collect();
        match declared.len() {
            0 => None,
            1 => Some(format!("{} declares `{}`", declared[0], rule.spelling())),
            _ => Some(format!("{} declare `{}`", declared.join(" and "), rule.spelling())),
        }
    }

    /// What a flow owes. A flow that is not bad owes nothing.
    pub(super) fn owed_of(&self, flow: &Flow) -> Obligations {
        match flow {
            Flow::Bad { obligations, .. } => obligations.clone(),
            _ => Obligations::new(),
        }
    }

    fn unprovable_only(&self, obligations: &Obligations) -> Obligations {
        obligations.iter().copied().filter(|o| self.sigs.witness(*o).is_none()).collect()
    }

    /// A discharge proves a value is not in some bad state. An operand owing only witnessless
    /// obligations names no such state, so the form has nothing to prove and is rejected.
    pub(super) fn require_witnessed_operand(&self, flow: &Flow, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let Flow::Bad { obligations, .. } = flow else { return Ok(()) };
        if obligations.iter().any(|o| self.sigs.witness(*o).is_some()) {
            return Ok(());
        }
        let owed = self.quoted_obligation_list(obligations);
        let have = match obligations.len() {
            1 => "has",
            _ => "have",
        };
        Err(self.error_help(
            format!("cannot discharge a value owing {owed}"), node,
            format!("{owed} {have} no witness, so there is no bad state to rule out")))
    }

    /// Whether any of these obligations is `no persist`.
    pub(super) fn owes_no_persist(&self, obligations: impl IntoIterator<Item = Symbol>) -> bool {
        obligations.into_iter().any(|o| self.sigs.rules_of(o).no_persist)
    }

    /// The help behind a rule-based prohibition. A user obligation has a declaration to cite. A
    /// built-in has none, so it gets the guidance that applies to its values instead. Only built-ins
    /// are left when there is nothing to cite, so the first name identifies which witness to speak of.
    pub(super) fn prohibition_help(&self, obligations: &Obligations, rule: Rule, site: Site) -> String {
        match self.rule_citation(obligations, rule) {
            Some(citation) => format!("{citation}, which prevents {}", site.prevents()),
            None => site.guidance(self.sorted_obligation_names(obligations)[0]).to_string(),
        }
    }

    /// The flow after a discharge operator. A discharge answers the read question, so it clears the
    /// obligations that asked it and leaves the rest in place.
    pub(super) fn discharged_flow(&self, flow: &Flow) -> Flow {
        let Flow::Bad { obligations, .. } = flow else { return Flow::Clean };
        let kept = self.unprovable_only(obligations);
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

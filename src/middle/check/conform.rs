//! Whether what a value owes conforms to what its destination accepts.

use std::collections::{HashMap, HashSet};

use anyhow::anyhow;

use crate::frontend::lex::Diagnostic;
use crate::middle::diagnose::Diagnose;
use crate::middle::hir::{BinOp, HirExpr, HirId, HirLiteral, HirMatchElem, HirMatchRest, HirMatcher, HirStmt, Symbol, builtin_obligation_rules};
use crate::middle::obligations::{quoted_obligation_list, sorted_obligation_names, Obligations};
use crate::middle::native::{self, NativeSig};
use crate::middle::signatures::{CallableId, RetSig, TypeTag, Witness};

use super::narrow::collect_whole_value_binders;
use super::{ProvenFacts, PathMap, ELEMENTS};
use super::{Checker, Ctx, Debt, ObligationRule, OperandKind, Site, ValueState};

#[derive(Default, Clone)]
pub(super) struct BinderFacts {
    pub owed: Obligations,
    /// What is proven below the root of the value the name takes.
    pub proven: PathMap<ProvenFacts>,
    pub undeclared: bool,
    pub tag: TypeTag,
}

impl BinderFacts {
    /// What the name takes, at its root and inside it.
    pub(super) fn state(&self) -> ValueState {
        let debt = match (self.owed.is_empty(), self.undeclared) {
            (false, _) => Debt::Owed { obligations: self.owed.clone(), definite: false },
            (true, true) => Debt::Unknown,
            (true, false) => Debt::Clean,
        };
        ValueState::of(debt, self.tag.clone()).proving(self.proven.clone())
    }
}

#[derive(Default)]
pub(super) struct MatcherFacts(HashMap<Symbol, BinderFacts>);

impl MatcherFacts {
    pub(super) fn get(&self, name: &Symbol) -> Option<&BinderFacts> {
        self.0.get(name)
    }

    fn of(&mut self, name: Symbol) -> &mut BinderFacts {
        self.0.entry(name).or_default()
    }

    pub(super) fn add_owed(&mut self, name: Symbol, owed: &Obligations) {
        self.of(name).owed.extend(owed.iter().copied());
    }

    pub(super) fn merge(&mut self, other: MatcherFacts) {
        for (name, facts) in other.0 {
            let mine = self.of(name);
            mine.owed.extend(facts.owed);
            if facts.tag != TypeTag::Unknown {
                mine.tag = facts.tag;
            }
            mine.undeclared |= facts.undeclared;
            mine.proven.extend(facts.proven);
        }
    }
}

impl<'a> Ctx<'a> {
    pub(super) fn call_result(&self, callable: CallableId, receiver_tag: &TypeTag) -> ValueState {
        let debt = self.sigs.fn_sig_of(callable).map_or(Debt::Unknown, |s| self.ret_debt(&s.ret));
        let tag = self.sigs.ret_tag_of(callable).map_or(TypeTag::Unknown, |t| t.resolve(receiver_tag));
        ValueState::of(debt, tag)
    }

    pub(super) fn matcher_facts<T>(&self, matcher: &HirId<HirMatcher>, at: &HirId<T>) -> Result<MatcherFacts, anyhow::Error> {
        let mut out = MatcherFacts::default();
        self.walk_matcher(matcher, at, None, &mut out)?;
        Ok(out)
    }

    pub(super) fn condition_facts(&self, cond: &HirId<HirExpr>) -> Result<MatcherFacts, anyhow::Error> {
        match self.hir.get(cond) {
            HirExpr::Match(_, matcher) => self.matcher_facts(matcher, cond),
            HirExpr::Binary(BinOp::And | BinOp::Or, left, right) => {
                let mut out = self.condition_facts(left)?;
                out.merge(self.condition_facts(right)?);
                Ok(out)
            },
            _ => Ok(MatcherFacts::default()),
        }
    }

    fn walk_matcher<T>(&self, matcher: &HirId<HirMatcher>, at: &HirId<T>, enclosing_type_test: Option<&HirId<HirMatcher>>, out: &mut MatcherFacts) -> Result<(), anyhow::Error> {
        match self.hir.get(matcher) {
            HirMatcher::As(name, inner) => {
                out.of(*name).owed.extend(self.resolved().admitted_obligations(inner));
                if let Some(decl) = self.matcher_proves_type(inner) {
                    let facts = out.of(*name);
                    facts.tag = TypeTag::Concrete(decl);
                    facts.undeclared = false;
                }
                self.walk_matcher(inner, at, enclosing_type_test, out)?;
            },
            HirMatcher::Or(alternatives) => self.walk_or_matchers(alternatives, at, enclosing_type_test, out)?,
            HirMatcher::And(parts) => for part in parts {
                self.walk_matcher(part, at, enclosing_type_test, out)?;
            },
            HirMatcher::Type { shape: Some(shape), .. } => {
                for (name, owed) in self.collect_matcher_obligations_by_binding(matcher, shape) {
                    out.of(name).owed.extend(owed);
                }
                self.walk_matcher(shape, at, Some(matcher), out)?;
            },
            HirMatcher::Dict(shape) => self.walk_matcher(shape, at, None, out)?,
            HirMatcher::Shape { fields, rest } => {
                for field in fields {
                    // A nominal test answers for every key of its type. A structural one answers
                    // only for the fields it declares, and a dict answers for none.
                    if !self.proves_declared_field(enclosing_type_test, &field.key) {
                        for name in collect_whole_value_binders(self.hir, &field.value) { out.of(name).undeclared = true; }
                    }
                    self.walk_matcher(&field.value, at, None, out)?;
                }
                self.walk_rest(rest.as_ref(), out);
            },
            HirMatcher::Array(elements) => {
                for element in elements {
                    let HirMatchElem::Elem(m) = element else { continue };
                    for name in collect_whole_value_binders(self.hir, m) { out.of(name).undeclared = true; }
                    self.walk_matcher(m, at, None, out)?;
                }
                let rest = elements.iter().find_map(|e| match e {
                    HirMatchElem::Rest(rest) => Some(rest),
                    HirMatchElem::Elem(_) => None,
                });
                self.walk_rest(rest, out);
            },
            HirMatcher::Binder(..) | HirMatcher::Literal(..) | HirMatcher::Wildcard | HirMatcher::Type { .. } => {}
        }
        Ok(())
    }

    fn matcher_proves_type(&self, matcher: &HirId<HirMatcher>) -> Option<HirId<HirStmt>> {
        match self.hir.get(matcher) {
            HirMatcher::Type { nominal: true, .. } => {
                let stmt = self.bindings.type_ref(matcher)?;
                let HirStmt::Type(decl) = self.hir.get(&stmt) else { return None };
                (!self.hir.is_trait(decl.id)).then_some(stmt)
            },
            HirMatcher::As(_, inner) => self.matcher_proves_type(inner),
            HirMatcher::And(parts) => parts.iter().find_map(|p| self.matcher_proves_type(p)),
            HirMatcher::Or(alternatives) => {
                let mut proved = alternatives.iter().map(|a| self.matcher_proves_type(a));
                let first = proved.next()??;
                proved.all(|p| p == Some(first)).then_some(first)
            },
            HirMatcher::Wildcard | HirMatcher::Literal(..) | HirMatcher::Binder(..)
                | HirMatcher::Type { nominal: false, .. } | HirMatcher::Shape { .. }
                | HirMatcher::Dict(..) | HirMatcher::Array(..) => None,
        }
    }

    fn walk_rest(&self, rest: Option<&HirMatchRest>, out: &mut MatcherFacts) {
        let Some(rest) = rest else { return };
        let (Some(every), Some(name)) = (&rest.every, rest.binder) else { return };
        let owed = self.resolved().admitted_obligations(every);
        out.of(name).proven.insert(ELEMENTS.to_vec(), ProvenFacts { owed: Some(owed), tag: TypeTag::Unknown });
    }

    fn walk_or_matchers<T>(&self, matchers: &[HirId<HirMatcher>], at: &HirId<T>, enclosing_type_test: Option<&HirId<HirMatcher>>, out: &mut MatcherFacts) -> Result<(), anyhow::Error> {
        let binding = matchers.iter().find(|a| self.hir.get(*a).binds_anything(self.hir));
        let mut admitted = Obligations::new();
        for alt in matchers {
            self.walk_matcher(alt, at, enclosing_type_test, out)?;
            if self.hir.get(alt).binds_anything(self.hir) || binding.is_none() {
                continue;
            }
            let witnesses = self.resolved().bindingless_witness_obligations(alt);
            if witnesses.is_empty() {
                return Err(self.error_help("a non-witness alternative beside a destructure is a dead binding".to_string(), at,
                    "beside a destructure, an alternative must be a witness (`null` or a witness type)"));
            }
            admitted.extend(witnesses.iter().copied());
        }
        let Some(binding) = binding else { return Ok(()) };
        for name in self.hir.get(binding).binders(self.hir) {
            out.of(name).owed.extend(admitted.iter().copied());
        }
        Ok(())
    }

    pub(super) fn witness_use_error(&self, header: String, operand: &HirId<HirExpr>, witness: &str) -> anyhow::Error {
        anyhow!("{}", Diagnostic::new(header, self.hir.pos(operand).clone()).with_label(witness.to_string()))
    }

    pub(super) fn is_obligation_witness(&self, state: &ValueState) -> bool {
        self.obligation_witness_name(state).is_some()
    }

    pub(super) fn obligation_witness_name(&self, state: &ValueState) -> Option<&'a str> {
        self.obligation_witness_name_of(&state.debt, &state.tag)
    }

    fn obligation_witness_name_of(&self, debt: &Debt, tag: &TypeTag) -> Option<&'a str> {
        let TypeTag::Concrete(tag) = tag else { return None };
        let Debt::Owed { obligations, .. } = debt else { return None };

        // Every bad state the value might be in has to be an object. `opt` admits null, which the
        // tag does not describe.
        if obligations.iter().any(|o| matches!(self.sigs.witness_of(*o), Witness::Null)) {
            return None;
        }

        // A trait witness counts too: proving the type proves every trait it provides.
        let HirStmt::Type(decl) = self.hir.get(tag) else { return None };
        let witnessed = self.sigs.obligations_witnessed_by_decl(decl);
        obligations.iter().find(|o| witnessed.contains(o))?;
        self.type_name_of(tag).map(|name| self.hir.text(name))
    }

    pub(super) fn absent_member_error(&self, tag: &TypeTag, name: &str, node: &HirId<HirExpr>) -> anyhow::Error {
        let owner = match tag {
            TypeTag::Concrete(decl) => self.type_name_of(decl).map(|n| self.hir.text(n)),
            TypeTag::Native(ty) => Some(ty.name()),
            _ => None,
        };
        self.error(format!("{} doesn't have member \"{name}\"", owner.unwrap_or("This value")), node)
    }

    pub(super) fn may_have_member(&self, tag: &TypeTag, name: &str) -> bool {
        let decl = match tag {
            TypeTag::Concrete(decl) => decl,
            TypeTag::Native(ty) => return native::native_method(*ty, name).is_some(),
            _ => return true,
        };
        let Some(layout) = self.layout_of(decl) else { return true };
        self.hir.symbol_of(name).is_some_and(|field| layout.members.contains_key(&field))
    }

    /// What a fresh instance owes.
    pub(super) fn construction_debt(&self, decl: &HirId<HirStmt>) -> Debt {
        let obligations = match self.hir.get(decl) {
            HirStmt::Type(decl) => self.sigs.obligations_witnessed_by_decl(decl),
            _ => Obligations::new(),
        };
        match obligations.is_empty() {
            true => Debt::Clean,
            false => Debt::Owed { obligations, definite: true },
        }
    }

    pub(super) fn invalid_operands_error(&self, op: BinOp, l: &HirId<HirExpr>, ln: &ValueState, r: &HirId<HirExpr>, rn: &ValueState) -> anyhow::Error {
        let (lt, rt) = (self.operand_type_name(ln, l), self.operand_type_name(rn, r));
        let header = format!("invalid operands of `{op}`: {lt} and {rt}");
        // Point the primary caret at the confirmed operand; the other side is a labeled span.
        let (primary, primary_ty, other, other_ty) = if self.is_obligation_witness(ln) {
            (l, &lt, r, &rt)
        } else {
            (r, &rt, l, &lt)
        };
        anyhow!("{}", Diagnostic::new(header, self.hir.pos(primary).clone())
            .with_label(primary_ty.to_string())
            .with_span(self.hir.pos(other).clone(), other_ty.to_string()))
    }

    pub(super) fn native_ret_debt(&self, ret: native::RetSig) -> Debt {
        if ret.void {
            return Debt::Void;
        }
        let mut obligations = Obligations::new();
        if ret.set.opt { obligations.insert(self.sigs.opt); }
        if ret.set.fails { obligations.insert(self.sigs.fails); }
        if obligations.is_empty() {
            Debt::Clean
        } else {
            Debt::Owed { obligations, definite: false }
        }
    }

    fn native_obligations(&self, set: native::ObSet) -> Obligations {
        let mut out = Obligations::new();
        if set.opt { out.insert(self.sigs.opt); }
        if set.fails { out.insert(self.sigs.fails); }
        out
    }

    pub(super) fn operand_type_name(&self, state: &ValueState, node: &HirId<HirExpr>) -> String {
        if let Some(witness) = self.obligation_witness_name(state) {
            return witness.to_string();
        }
        match self.hir.get(node) {
            HirExpr::Literal(HirLiteral::Number(_)) => "number".to_string(),
            HirExpr::Literal(HirLiteral::String(_)) => "string".to_string(),
            HirExpr::Literal(HirLiteral::Boolean(_)) => "boolean".to_string(),
            HirExpr::Literal(HirLiteral::Null) => "null".to_string(),
            _ => match &state.tag {
                TypeTag::Concrete(decl) => match self.type_name_of(decl) {
                    Some(name) => self.hir.text(name).to_string(),
                    None => "value".to_string(),
                },
                _ => "value".to_string(),
            },
        }
    }

    pub(super) fn obligations_of(&self, debt: &Debt) -> Obligations {
        match debt {
            Debt::Owed { obligations, .. } => obligations.clone(),
            _ => Obligations::new(),
        }
    }

    pub(super) fn owes_object_witness(&self, debt: &Debt) -> bool {
        matches!(debt, Debt::Owed { obligations, .. }
            if obligations.iter().any(|o| matches!(self.sigs.witness_of(*o), Witness::Type(_) | Witness::Trait(_))))
    }

    pub(super) fn obligations_having_rule(&self, obligations: &Obligations, rule: ObligationRule) -> Obligations {
        obligations.iter().copied().filter(|o| rule.holds(&self.sigs.obligation_rules_of(*o))).collect()
    }

    pub(super) fn obligation_rule_prevents_help(&self, obligations: &Obligations, rule: ObligationRule, site: Site) -> String {
        match self.obligation_rule_citation(obligations, rule) {
            Some(citation) => format!("{citation}, which prevents {}", site.prevents()),
            None => site.guidance(sorted_obligation_names(self.hir, obligations)[0]).to_string(),
        }
    }

    fn proves_declared_field(&self, test: Option<&HirId<HirMatcher>>, key: &HirLiteral) -> bool {
        let (Some(test), HirLiteral::String(key)) = (test, key) else { return false };
        self.hir.symbol_of(key).is_some_and(|member| self.test_proves_declared_member(test, member))
    }

    pub(super) fn obligation_rule_reject_at(&self, debt: &Debt, rule: ObligationRule, site: Site, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let Debt::Owed { obligations, .. } = debt else { return Ok(()) };
        let blocked = self.obligations_having_rule(obligations, rule);
        if blocked.is_empty() {
            return Ok(());
        }
        let owed = quoted_obligation_list(self.hir, &blocked);
        let help = self.obligation_rule_prevents_help(&blocked, rule, site);
        Err(self.error_help(site.refusal(&owed), node, help))
    }

    pub(super) fn require_discharged_or_narrowed(&self, debt: &Debt, tag: &TypeTag, node: &HirId<HirExpr>, kind: OperandKind) -> Result<(), anyhow::Error> {
        let witness = self.obligation_witness_name_of(debt, tag).is_some();
        match kind {
            OperandKind::Whole => debug_assert!(!witness, "a confirmed witness must be handled at its operation site"),
            OperandKind::Base if witness => return Ok(()),
            OperandKind::Base => {},
        }
        self.refuse_blocking_debt(debt, node)
    }

    /// The rule itself, with no operand kind to excuse anything.
    pub(super) fn refuse_blocking_debt(&self, debt: &Debt, operand: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        if debt.is_void() {
            return Err(self.error("This call returns no value, so its result cannot be used here".to_string(), operand));
        }

        let Debt::Owed { obligations, .. } = debt else { return Ok(()) };

        let blocking: Obligations = obligations.iter().copied().filter(|o| self.sigs.obligation_rules_of(*o).to_use).collect();
        if blocking.is_empty() {
            return Ok(());
        }

        let name = match self.hir.get(operand) {
            HirExpr::Identifier(name) => Some(self.hir.text(*name)),
            _ => None,
        };

        let owed = quoted_obligation_list(self.hir, &blocking);

        // Name the witness so the reader knows what to rule out, and how.
        let mut witnesses: Vec<&str> = blocking.iter().map(|o| self.witness_name(*o)).collect();
        witnesses.sort();
        witnesses.dedup();
        let witness = witnesses.join(" or ");
        let subject = name.map_or("the value".to_string(), |name| format!("`{name}`"));

        // The header stays generic so it is easy to search for. The caret and help carry the name.
        Err(anyhow!("{}", Diagnostic::new(format!("unchecked value owes {owed}"), self.hir.pos(operand).clone())
            .with_label(format!("might be {witness}"))
            .with_help(format!("make sure {subject} is not {witness} before using it"))))
    }

    pub(super) fn ret_debt(&self, ret: &RetSig) -> Debt {
        if ret.void {
            Debt::Void
        } else if ret.obligations.is_empty() {
            Debt::Clean
        } else {
            Debt::Owed { obligations: ret.obligations.clone(), definite: false }
        }
    }

    pub(super) fn obligation_rule_citation(&self, obligations: &Obligations, rule: ObligationRule) -> Option<String> {
        let declared: Vec<String> = sorted_obligation_names(self.hir, obligations).into_iter()
            .filter(|name| builtin_obligation_rules(name).is_none())
            .map(|name| format!("`{name}`"))
            .collect();
        match declared.len() {
            0 => None,
            1 => Some(format!("{} declares `{}`", declared[0], rule.spelling())),
            _ => Some(format!("{} declare `{}`", declared.join(" and "), rule.spelling())),
        }
    }

    /// The tag a value caught by `v ?? e => ...` narrows to.
    pub(super) fn handle_caught_tag(&self, caught: &Obligations) -> TypeTag {
        let mut witnessed = caught.iter().map(|o| self.sigs.witness_of(*o));
        match (witnessed.next(), witnessed.next()) {
            (Some(Witness::Type(id)), None) => self.sigs.type_decl_of_id(*id).map_or(TypeTag::Unknown, TypeTag::Concrete),
            _ => TypeTag::Unknown,
        }
    }

    /// What the value owes that the slot does not admit.
    pub(super) fn unadmitted_obligations(&self, debt: &Debt, admits: &Obligations) -> Obligations {
        let Debt::Owed { obligations, .. } = debt else { return Obligations::new() };
        obligations.difference(admits).copied().collect()
    }

    pub(super) fn only_nullable(&self, unadmitted: &Obligations) -> bool {
        unadmitted.len() == 1 && unadmitted.contains(&self.sigs.opt)
    }

    pub(super) fn witness_name(&self, obligation: Symbol) -> &'a str {
        match self.sigs.witness_of(obligation) {
            Witness::Type(id) | Witness::Trait(id) => self.hir.text(self.hir.type_info(*id).expect("a witness names a declared type").name),
            Witness::Null => "null",
        }
    }

}

impl<'a> Checker<'a> {
    pub(super) fn store_into_container(&mut self, debt: &Debt, expr: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        self.ctx.obligation_rule_reject_at(debt, ObligationRule::NoPersist, Site::Container, expr)
    }

    pub(super) fn refuse_anchor_capture(&self, name: Symbol, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let Some(i) = self.capture_index(name) else { return Ok(()) };
        // An anchor parameter's name is a copy of the value at the anchor, so a closure can hold it.
        if !self.locals[i].is_anchor || self.locals[i].is_param {
            return Ok(());
        }
        Err(self.error_help(format!("Cannot capture `{}`; it is an anchor", self.ctx.hir.text(name)), node,
            "pass the anchor to the function instead of capturing it"))
    }

    pub(super) fn refuse_capture_of_persisting_value(&self, name: Symbol, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let Some(i) = self.capture_index(name) else { return Ok(()) };
        // A capture outlives the current value, so what the slot admits is what may be persisted.
        let owed = self.locals[i].clause_owed().clone();
        self.ctx.obligation_rule_reject_at(&Debt::Owed { obligations: owed, definite: false },
            ObligationRule::NoPersist, Site::Capture, node)
    }

    pub(super) fn transfer_obligations_into_receiver(&mut self, receiver: &HirId<HirExpr>, values: &[ValueState]) {
        let mut obligations = HashSet::new();
        for state in values {
            if let Debt::Owed { obligations: o, .. } = &state.debt {
                obligations.extend(o.iter().copied());
            }
        }
        if obligations.is_empty() {
            return;
        }
        let HirExpr::Identifier(name) = self.ctx.hir.get(receiver) else { return };
        let Some(i) = self.frame_index_of(*name) else { return };
        // The values went into a container, so what they owed is owed by its elements.
        self.locals[i].set_element_owed(obligations.into_iter().collect());
    }

    pub(super) fn record_witness_assert(&mut self, node: &HirId<HirExpr>) {
        self.out.witness_asserts.insert(*node);
    }

    pub(super) fn chained_result(&self, operand: &Debt, yielded: &Debt) -> ValueState {
        let mut obligations = self.ctx.obligations_of(operand);
        obligations.extend(self.ctx.obligations_of(yielded));
        let unknown = matches!(operand, Debt::Unknown) || matches!(yielded, Debt::Unknown);
        let debt = if !obligations.is_empty() {
            Debt::Owed { obligations, definite: false }
        } else if unknown {
            Debt::Unknown
        } else {
            Debt::Clean
        };
        ValueState::of(debt, TypeTag::Unknown)
    }
}

impl<'a> Checker<'a> {
    pub(super) fn check_arg_obligations(&self, callee: &HirId<HirExpr>, clauses: &[Obligations], arg_types: &[ValueState], args: &[HirId<HirExpr>]) -> Result<(), anyhow::Error> {
        for (i, admits) in clauses.iter().enumerate() {
            let Some(state) = arg_types.get(i) else { break };
            let undeclared = self.ctx.unadmitted_obligations(&state.debt, admits);
            // Nullability alone is answered by `check_arg`, which points at the parameter too.
            if undeclared.is_empty() || self.ctx.only_nullable(&undeclared) {
                continue;
            }
            let owed = quoted_obligation_list(self.ctx.hir, &undeclared);
            let subject = self.ctx.quoted_subject(&args[i]);
            let c = self.ctx.callee_display_name(callee);
            return Err(self.error_ctx_help(
                format!("cannot pass a value owing {owed}"),
                self.ctx.hir.pos(&args[i]), format!("{subject} owes {owed}"),
                self.ctx.hir.pos(callee), format!("{c} does not declare {owed} here"),
                "discharge it before the call, or declare it on the parameter"));
        }
        Ok(())
    }

    /// Checks each argument against what its parameter accepts.
    pub(super) fn check_args(&mut self, callee: &HirId<HirExpr>, params: &[Obligations], arg_types: &[ValueState], args: &[HirId<HirExpr>]) -> Result<(), anyhow::Error> {
        for (i, accepts) in params.iter().enumerate() {
            let Some(state) = arg_types.get(i) else { break };
            self.check_arg(callee, &state.debt, accepts, i, &args[i])?;
        }
        Ok(())
    }

    /// Checks a single argument value against a parameter.
    pub(super) fn check_arg(&mut self, callee: &HirId<HirExpr>, debt: &Debt, accepts: &Obligations, position: usize, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        if matches!(debt, Debt::Unknown) {
            self.record_boundary_barrier(node, accepts);
            return Ok(());
        }
        if debt.is_void() {
            return Err(self.error(format!("Argument {} is a void result; the call returns no value", position + 1), node));
        }
        let unadmitted = self.ctx.unadmitted_obligations(debt, accepts);
        if unadmitted.is_empty() || !self.ctx.only_nullable(&unadmitted) {
            return Ok(());
        }

        let site_label = format!("{} requires a non-null value here", self.ctx.callee_display_name(callee));
        match debt.is_definite() {
            true => Err(self.error_ctx("expected non-null argument", self.ctx.hir.pos(node), "this argument is null", self.ctx.hir.pos(callee), site_label)),
            false => Err(self.error_ctx_help("expected non-null argument", self.ctx.hir.pos(node), "this argument may be null", self.ctx.hir.pos(callee), site_label, "narrow it before the call")),
        }
    }

    pub(super) fn check_native_args(&mut self, callee: &HirId<HirExpr>, sig: &NativeSig, arg_types: &[ValueState], args: &[HirId<HirExpr>]) -> Result<(), anyhow::Error> {
        for (i, set) in sig.params.iter().enumerate() {
            let Some(state) = arg_types.get(i) else { break };
            if set.admits_any() && matches!(state.debt, Debt::Unknown) {
                continue;
            }
            self.check_arg(callee, &state.debt, &self.ctx.native_obligations(*set), i, &args[i])?;
        }
        Ok(())
    }

    /// Checks a value moving into a field.
    pub(super) fn check_into_field(&mut self, debt: &Debt, admits: &Obligations, field: Symbol, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        // Storing into a field persists the value, which a `no persist` value forbids. A local is
        // not a persist site, so the shared slot check does not ask this.
        self.ctx.obligation_rule_reject_at(debt, ObligationRule::NoPersist, Site::Field, node)?;
        self.check_into_named_slot(debt, admits, field, "field", node)
    }

    pub(super) fn check_into_brace_field(&mut self, decl: &HirId<HirStmt>, field: Symbol, debt: &Debt, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let Some(layout) = self.ctx.layout_of(decl) else { return Ok(()) };
        let admits = layout.owed(field);
        self.check_into_field(debt, &admits, field, node)
    }

    /// For an `lhs = rhs` assignment in a factory, the field `lhs` names, if it is left uninitialized.
    pub(super) fn never_initialized_factory_field(&self, lhs: &HirId<HirExpr>, rhs: &HirId<HirExpr>) -> Option<Symbol> {
        let HirExpr::Identifier(local) = self.ctx.hir.get(rhs) else { return None };
        if !self.ctx.is_factory_field(*local) {
            return None;
        }
        let HirExpr::Index { base: target, member, .. } = self.ctx.hir.get(lhs) else { return None };
        if !matches!(self.ctx.hir.get(target), HirExpr::This) {
            return None;
        }
        let i = self.frame_index_of(*local)?;
        if self.locals[i].assigned {
            return None;
        }
        self.ctx.string_member(member)
    }
}

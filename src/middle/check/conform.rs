//! Whether what a value owes conforms to what its destination accepts.

use std::collections::{HashMap, HashSet};

use anyhow::anyhow;

use crate::frontend::lex::Diagnostic;
use crate::middle::diagnose::Diagnose;
use crate::middle::hir::{builtin_obligation_rules, BinOp, HirExpr, HirFnDecl, HirId, HirLiteral, HirMatchElem, HirMatcher, HirStmt, Symbol};
use crate::middle::obligations::{quoted_obligation_list, sorted_obligation_names, Obligations};
use crate::middle::native::{self, NativeSig};
use crate::middle::signatures::{CallableId, Mutability, RetSig, TypeTag, Witness};
use crate::middle::hir::TypeId;

use super::narrow::collect_whole_value_binders;
use super::{Checker, Ctx, Debt, ObligationRule, Site, ValueState, Violation, WitnessSet};

fn merge_binder_obligations(into: &mut HashMap<Symbol, Obligations>, from: HashMap<Symbol, Obligations>) {
    for (name, obligations) in from {
        into.entry(name).or_default().extend(obligations);
    }
}

impl<'a> Ctx<'a> {
    pub(super) fn obligation_preventing_escape(&self, debt: &Debt) -> Option<Symbol> {
        let Debt::Owed { obligations, .. } = debt else { return None };
        obligations.iter().copied().find(|&o| {
            let rules = self.sigs.obligation_rules_of(o);
            rules.no_persist || rules.must_use
        })
    }

    pub(super) fn call_result(&self, callable: CallableId, receiver_tag: &TypeTag) -> ValueState {
        let debt = self.sigs.fn_sig_of(callable).map_or(Debt::Unknown, |s| self.ret_debt(&s.ret));
        let mutability = self.sigs.ret_mut_of_callable(callable);
        let tag = self.sigs.ret_tag_of(callable).map_or(TypeTag::Unknown, |t| t.resolve(receiver_tag));
        ValueState::of(debt, tag).with_mutability(mutability)
    }

    pub(super) fn collect_condition_witness_obligations(&self, cond: &HirId<HirExpr>) -> Result<HashMap<Symbol, Obligations>, anyhow::Error> {
        match self.hir.get(cond) {
            HirExpr::Match(_, matcher) => self.collect_matcher_witnessed_obligations(matcher, cond),
            HirExpr::Binary(BinOp::And | BinOp::Or, left, right) => {
                let mut out = self.collect_condition_witness_obligations(left)?;
                merge_binder_obligations(&mut out, self.collect_condition_witness_obligations(right)?);
                Ok(out)
            },
            _ => Ok(HashMap::new()),
        }
    }

    pub(super) fn collect_matcher_unknown_binders(&self, matcher: &HirId<HirMatcher>) -> HashSet<Symbol> {
        match self.hir.get(matcher) {
            HirMatcher::As(_, inner) => self.collect_matcher_unknown_binders(inner),
            HirMatcher::Or(parts) | HirMatcher::And(parts) =>
                parts.iter().flat_map(|part| self.collect_matcher_unknown_binders(part)).collect(),
            HirMatcher::Type { shape: Some(shape), .. } => self.unknown_binders_in_shape_matcher(shape, Some(matcher)),
            HirMatcher::Shape(_) | HirMatcher::Array(_) => self.unknown_binders_in_shape_matcher(matcher, None),
            _ => HashSet::new(),
        }
    }

    pub(super) fn collect_condition_unknown_binders(&self, cond: &HirId<HirExpr>) -> HashSet<Symbol> {
        match self.hir.get(cond) {
            HirExpr::Match(_, matcher) => self.collect_matcher_unknown_binders(matcher),
            HirExpr::Binary(BinOp::And | BinOp::Or, left, right) => {
                let mut out = self.collect_condition_unknown_binders(left);
                out.extend(self.collect_condition_unknown_binders(right));
                out
            },
            _ => HashSet::new(),
        }
    }

    pub(super) fn witness_use_error(&self, header: String, operand: &HirId<HirExpr>, witness: &str) -> anyhow::Error {
        anyhow!("{}", Diagnostic::new(header, self.hir.pos(operand).clone()).with_label(witness.to_string()))
    }

    pub(super) fn is_obligation_witness(&self, state: &ValueState) -> bool {
        self.obligation_witness_name(state).is_some()
    }

    pub(super) fn obligation_witness_name(&self, state: &ValueState) -> Option<&'a str> {
        let TypeTag::Concrete(tag) = &state.tag else { return None };
        let Debt::Owed { obligations, .. } = &state.debt else { return None };
        // Every bad state the value might be in has to be an object. `opt` admits null, which the
        // tag does not describe. A witnessless obligation names no bad state, so it admits nothing.
        if obligations.iter().any(|o| matches!(self.sigs.witness_of(*o), Some(Witness::Null))) {
            return None;
        }
        obligations.iter().find_map(|o| match self.sigs.witness_of(*o) {
            Some(Witness::Type(w)) if self.sigs.type_decl_of_id(*w) == Some(*tag) =>
                self.type_name_of(tag).map(|name| self.hir.text(name)),
            _ => None,
        })
    }

    pub(super) fn absent_member_error(&self, tag: &TypeTag, name: &str, node: &HirId<HirExpr>) -> anyhow::Error {
        let owner = match tag {
            TypeTag::Concrete(decl) => self.type_name_of(decl).map(|n| self.hir.text(n)),
            _ => None,
        };
        self.error(format!("{} doesn't have member \"{name}\"", owner.unwrap_or("This value")), node)
    }

    pub(super) fn may_have_member(&self, tag: &TypeTag, name: &str) -> bool {
        let TypeTag::Concrete(decl) = tag else { return true };
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
            false => Debt::Owed { obligations, definite: true, container: false },
        }
    }

    /// What a value owes after a discharge operator.
    pub(super) fn discharged_debt(&self, debt: &Debt) -> Debt {
        let Debt::Owed { obligations, .. } = debt else { return Debt::Clean };
        let kept = self.unprovable_only(obligations);
        if kept.is_empty() {
            Debt::Clean
        } else {
            Debt::Owed { obligations: kept, definite: false, container: false }
        }
    }

    pub(super) fn err_witness(&self) -> Option<TypeId> {
        match self.sigs.witness_of(self.sigs.fails) {
            Some(Witness::Type(e)) => Some(*e),
            _ => None,
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
            Debt::Owed { obligations, definite: false, container: false }
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
            if obligations.iter().any(|o| matches!(self.sigs.witness_of(*o), Some(Witness::Type(_) | Witness::Trait(_)))))
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

    pub(super) fn reject_receiver_witnessed_obligations(&self, decl: &HirFnDecl) -> Result<(), anyhow::Error> {
        let Some(clause) = &decl.receiver else { return Ok(()) };
        let Some(name) = clause.names.iter().find(|n| self.sigs.witness_of(**n).is_some()) else { return Ok(()) };
        let text = self.hir.text(*name);
        let pos = clause.pos.clone().unwrap_or_else(|| decl.sig_pos.clone());
        Err(anyhow!("{}", Diagnostic::new(format!("The receiver cannot owe '{text}'"), pos)
            .with_label(format!("'{text}' admits a value `this` cannot be"))
            .with_help("`this` is always an instance of the type; put the obligation on a parameter instead")))
    }

    pub(super) fn require_usable_value(&self, state: &ValueState, operand: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        debug_assert!(!self.is_obligation_witness(state), "a confirmed witness must be handled at its operation site, not advised to narrow");
        self.require_discharged(state, operand)
    }

    pub(super) fn require_discharged(&self, state: &ValueState, operand: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        if state.debt.is_void() {
            return Err(self.error("This call returns no value, so its result cannot be used here".to_string(), operand));
        }

        let Debt::Owed { obligations, .. } = &state.debt else { return Ok(()) };

        let blocking: Obligations = obligations.iter().copied().filter(|o| self.sigs.obligation_rules_of(*o).to_use).collect();
        if blocking.is_empty() {
            return Ok(());
        }

        let name = match self.hir.get(operand) {
            HirExpr::Identifier(name) => Some(self.hir.text(*name)),
            _ => None,
        };

        let owed = quoted_obligation_list(self.hir, &blocking);

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

    /// A discharge proves a value is not in some bad state. An operand owing only witnessless
    /// obligations names no such state, so the form has nothing to prove and is rejected.
    pub(super) fn require_witnessed_operand(&self, debt: &Debt, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let Debt::Owed { obligations, .. } = debt else { return Ok(()) };
        if obligations.iter().any(|o| self.sigs.witness_of(*o).is_some()) {
            return Ok(());
        }
        let owed = quoted_obligation_list(self.hir, obligations);
        let have = match obligations.len() {
            1 => "has",
            _ => "have",
        };
        Err(self.error_help(
            format!("cannot discharge a value owing {owed}"), node,
            format!("{owed} {have} no witness, so there is no bad state to rule out")))
    }

    pub(super) fn ret_debt(&self, ret: &RetSig) -> Debt {
        if ret.void {
            Debt::Void
        } else if ret.obligations.is_empty() {
            Debt::Clean
        } else {
            Debt::Owed { obligations: ret.obligations.clone(), definite: false, container: false }
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
        let mut witnessed = caught.iter().filter_map(|o| self.sigs.witness_of(*o));
        match (witnessed.next(), witnessed.next()) {
            (Some(Witness::Type(id)), None) => self.sigs.type_decl_of_id(*id).map_or(TypeTag::Unknown, TypeTag::Concrete),
            _ => TypeTag::Unknown,
        }
    }

    pub(super) fn unadmitted_obligations(&self, debt: &Debt, admits: &Obligations) -> Obligations {
        let Debt::Owed { obligations, .. } = debt else { return Obligations::new() };
        obligations.difference(admits).copied().filter(|o| *o != self.sigs.opt).collect()
    }

    /// The unknown binders of one shape. `test` is the type test the shape hangs off, where there
    /// is one.
    fn unknown_binders_in_shape_matcher(&self, shape: &HirId<HirMatcher>, test: Option<&HirId<HirMatcher>>) -> HashSet<Symbol> {
        let mut out = HashSet::new();
        match self.hir.get(shape) {
            HirMatcher::Shape(fields) => for field in fields {
                if !self.proves_declared_field(test, &field.key) {
                    out.extend(collect_whole_value_binders(self.hir, &field.value));
                }
                out.extend(self.collect_matcher_unknown_binders(&field.value));
            },
            HirMatcher::Array(elements) => for element in elements {
                if let HirMatchElem::Elem(m) = element {
                    out.extend(collect_whole_value_binders(self.hir, m));
                    out.extend(self.collect_matcher_unknown_binders(m));
                }
            },
            _ => {},
        }
        out
    }

    pub(super) fn unprovable_only(&self, obligations: &Obligations) -> Obligations {
        obligations.iter().copied().filter(|o| self.sigs.witness_of(*o).is_none()).collect()
    }

    /// The type or trait that witnesses an obligation at runtime, if it has one.
    pub(super) fn witness_name(&self, obligation: Symbol) -> Option<&'a str> {
        match self.sigs.witness_of(obligation)? {
            Witness::Type(id) | Witness::Trait(id) => self.hir.type_info(*id).map(|info| self.hir.text(info.name)),
            Witness::Null => Some("null"),
        }
    }

    /// The witness obligations each binder inherits from a bindingless alternative sharing its
    /// or-group. In `Node { next } | null` the `next` binder owes `opt`.
    pub(super) fn collect_matcher_witnessed_obligations<T>(&self, matcher: &HirId<HirMatcher>, at: &HirId<T>) -> Result<HashMap<Symbol, Obligations>, anyhow::Error> {
        match self.hir.get(matcher) {
            HirMatcher::Or(alternatives) => self.collect_or_matcher_witnessed_obligations(alternatives, at),
            HirMatcher::As(name, inner) => {
                let mut out = self.collect_matcher_witnessed_obligations(inner, at)?;
                // `x @ p | null` names the whole value, so `x` owes what that or-group admits.
                let admits = self.resolved().admitted_obligations(inner);
                if !admits.is_empty() {
                    out.entry(*name).or_default().extend(admits);
                }
                Ok(out)
            },
            HirMatcher::Type { shape: Some(shape), .. } => {
                let mut out = self.collect_matcher_obligations_by_binding(matcher, shape);
                merge_binder_obligations(&mut out, self.collect_matcher_witnessed_obligations(shape, at)?);
                Ok(out)
            },
            HirMatcher::Shape(fields) => self.merge_matcher_binder_obligations(fields.iter().map(|field| &field.value), at),
            HirMatcher::Array(elements) => self.merge_matcher_binder_obligations(elements.iter().filter_map(|element| match element {
                HirMatchElem::Elem(m) => Some(m),
                HirMatchElem::Rest(_) => None,
            }), at),
            HirMatcher::And(parts) => self.merge_matcher_binder_obligations(parts.iter(), at),
            _ => Ok(HashMap::new()),
        }
    }

    fn merge_matcher_binder_obligations<'m, T>(&self, matchers: impl Iterator<Item = &'m HirId<HirMatcher>>, at: &HirId<T>) -> Result<HashMap<Symbol, Obligations>, anyhow::Error> {
        let mut out = HashMap::new();
        for matcher in matchers {
            merge_binder_obligations(&mut out, self.collect_matcher_witnessed_obligations(matcher, at)?);
        }
        Ok(out)
    }

    fn collect_or_matcher_witnessed_obligations<T>(&self, alternatives: &[HirId<HirMatcher>], at: &HirId<T>) -> Result<HashMap<Symbol, Obligations>, anyhow::Error> {
        // With nothing to bind, there is no name to hand the group to.
        let Some(binding) = alternatives.iter().find(|a| self.hir.get(*a).binds_anything(self.hir)) else {
            return Ok(HashMap::new());
        };

        let mut out = HashMap::new();
        let mut admitted = Obligations::new();
        for alt in alternatives {
            if self.hir.get(alt).binds_anything(self.hir) {
                merge_binder_obligations(&mut out, self.collect_matcher_witnessed_obligations(alt, at)?);
                continue;
            }
            let witnesses = self.resolved().bindingless_witness_obligations(alt);
            if witnesses.is_empty() {
                return Err(self.error_help("a non-witness alternative beside a destructure is a dead binding".to_string(), at,
                    "beside a destructure, an alternative must be a witness (`null` or a witness type)"));
            }
            admitted.extend(witnesses.iter().copied());
        }

        for name in self.hir.get(binding).binders(self.hir) {
            out.entry(name).or_default().extend(admitted.iter().copied());
        }
        Ok(out)
    }
}

impl<'a> Checker<'a> {
    /// Records which witnesses a discharge node must test at runtime.
    pub(super) fn record_witness_test(&mut self, node: &HirId<HirExpr>, debt: &Debt) {
        let Debt::Owed { obligations, .. } = debt else { return };
        let err = self.ctx.err_witness();
        let mut set = WitnessSet { null: false, witnesses: Vec::new(), contains_user_witnesses: false };
        for &o in obligations {
            match self.ctx.sigs.witness_of(o) {
                Some(Witness::Null) => set.null = true,
                Some(Witness::Type(w) | Witness::Trait(w)) => {
                    if !set.witnesses.contains(w) {
                        set.witnesses.push(*w);
                    }
                    if Some(*w) != err { set.contains_user_witnesses = true; }
                },
                None => {},
            }
        }
        // A recorded set always names an object witness: callers only record when
        // `owes_object_witness` holds. Codegen relies on this to fast-path an opt-only operand.
        debug_assert!(!set.witnesses.is_empty(), "witness test recorded with no object witness");
        self.out.witness_tests.insert(*node, set);
    }

    /// Whether a value is a container carrying element obligations.
    pub(super) fn is_container(debt: &Debt) -> bool {
        matches!(debt, Debt::Owed { container: true, .. })
    }

    pub(super) fn chain_result_with(&mut self, operand: &Debt, yielded: &Debt, node: &HirId<HirExpr>) -> ValueState {
        if self.ctx.owes_object_witness(operand) {
            self.record_witness_test(node, operand);
        }
        let mut obligations = self.ctx.obligations_of(operand);
        obligations.extend(self.ctx.obligations_of(yielded));
        let debt = match obligations.is_empty() {
            true => Debt::Clean,
            false => Debt::Owed { obligations, definite: false, container: false },
        };
        ValueState::of(debt, TypeTag::Unknown)
    }
}

impl<'a> Checker<'a> {
    pub(super) fn check_arg_obligations(&self, callee: &HirId<HirExpr>, clauses: &[Obligations], arg_types: &[ValueState], args: &[HirId<HirExpr>]) -> Result<(), anyhow::Error> {
        for (i, admits) in clauses.iter().enumerate() {
            let Some(state) = arg_types.get(i) else { break };
            let undeclared = self.ctx.unadmitted_obligations(&state.debt, admits);
            if undeclared.is_empty() {
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
        if accepts.contains(&self.ctx.sigs.opt) && !debt.is_void() {
            return Ok(());
        }

        let n = position + 1;
        let site_label = format!("{} requires a non-null value here", self.ctx.callee_display_name(callee));
        match self.non_null_violation(debt, node) {
            None => Ok(()),
            Some(Violation::Void) => Err(self.error(format!("Argument {n} is a void result; the call returns no value"), node)),
            Some(Violation::Null) => Err(self.error_ctx("expected non-null argument", self.ctx.hir.pos(node), "this argument is null", self.ctx.hir.pos(callee), site_label)),
            Some(Violation::Nullable) => Err(self.error_ctx_help("expected non-null argument", self.ctx.hir.pos(node), "this argument may be null", self.ctx.hir.pos(callee), site_label, "narrow it before the call")),
        }
    }

    pub(super) fn check_native_args(&mut self, callee: &HirId<HirExpr>, sig: &NativeSig, arg_types: &[ValueState], args: &[HirId<HirExpr>]) -> Result<(), anyhow::Error> {
        let accepts: Vec<Obligations> = sig.params.iter().map(|p| self.ctx.native_obligations(*p)).collect();
        self.check_args(callee, &accepts, arg_types, args)
    }

    /// Downgrades a frozen argument's slot to immutable.
    pub(super) fn discharge_freeze(&mut self, arg: &HirId<HirExpr>) {
        let HirExpr::Identifier(name) = self.ctx.hir.get(arg) else { return };
        if let Some(i) = self.frame_index_of(*name) {
            self.locals[i].alias.mutability = Mutability::Immutable;
        }
    }

    /// Checks a value moving into a field.
    pub(super) fn check_into_field(&mut self, debt: &Debt, field_nullable: bool, field: Symbol, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        // Storing into a field persists the value, which a `no persist` value forbids.
        self.ctx.obligation_rule_reject_at(debt, ObligationRule::NoPersist, super::Site::Field, node)?;
        let text = self.ctx.hir.text(field);
        let void = || format!("Cannot assign a void result to field '{text}'; the call returns no value");
        if field_nullable {
            // A nullable field still rejects a void result, which is not a value.
            if debt.is_void() {
                return Err(self.error(void(), node));
            }
            // An unknown value into an `opt` field is guarded against every object witness it may be.
            if matches!(debt, Debt::Unknown) {
                self.record_boundary_barrier(node, &Obligations::from([self.ctx.sigs.opt]));
            }
            return Ok(());
        }
        match self.non_null_violation(debt, node) {
            None => Ok(()),
            Some(Violation::Void) => Err(self.error(void(), node)),
            Some(Violation::Null) => Err(self.error(format!("Cannot assign null to non-null field '{text}'"), node)),
            Some(Violation::Nullable) => Err(self.error(format!("Cannot assign a nullable value to non-null field '{text}'"), node)),
        }
    }

    pub(super) fn check_into_brace_field(&mut self, decl: &HirId<HirStmt>, field: Symbol, debt: &Debt, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let nullable = match self.ctx.layout_of(decl) {
            Some(layout) => layout.is_nullable(field),
            None => return Ok(()),
        };
        self.check_into_field(debt, nullable, field, node)
    }

    /// For an `lhs = rhs` assignment in a factory, the field `lhs` names, if it is left uninitialized.
    pub(super) fn never_initialized_factory_field(&self, lhs: &HirId<HirExpr>, rhs: &HirId<HirExpr>) -> Option<Symbol> {
        let HirExpr::Identifier(local) = self.ctx.hir.get(rhs) else { return None };
        if !self.ctx.is_factory_field(*local) {
            return None;
        }
        let HirExpr::Index(target, member, _) = self.ctx.hir.get(lhs) else { return None };
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

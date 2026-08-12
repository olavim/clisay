//! Whether what a value owes conforms to what its destination accepts.

use std::collections::HashMap;

use anyhow::anyhow;

use crate::frontend::lex::Diagnostic;
use crate::middle::diagnose::Diagnose;
use crate::middle::hir::{builtin_obligation_rules, BinOp, HirExpr, HirFnDecl, HirId, HirLiteral, HirMatchElem, HirMatcher, HirStmt, Symbol};
use crate::middle::obligations::{quoted_obligation_list, sorted_obligation_names, Obligations};
use crate::middle::native::{self, NativeSig};
use crate::middle::signatures::{Mutability, RetSig, TypeTag, Witness};
use crate::middle::hir::TypeId;

use super::{Checker, Flow, Rule, Site, Typed, Violation, WitnessSet};

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
        let owed = quoted_obligation_list(self.hir, &blocked);
        let help = self.prohibition_help(&blocked, rule, site);
        Err(self.error_help(site.refusal(&owed), node, help))
    }

    /// The declaration a rule-based prohibition comes from, for the reader to go and read. A
    /// built-in obligation has none, so there is nothing to cite.
    pub(super) fn rule_citation(&self, obligations: &Obligations, rule: Rule) -> Option<String> {
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

    /// What a flow owes. A flow that is not bad owes nothing.
    pub(super) fn owed_of(&self, flow: &Flow) -> Obligations {
        match flow {
            Flow::Bad { obligations, .. } => obligations.clone(),
            _ => Obligations::new(),
        }
    }

    pub(super) fn unprovable_only(&self, obligations: &Obligations) -> Obligations {
        obligations.iter().copied().filter(|o| self.sigs.witness(*o).is_none()).collect()
    }

    /// A discharge proves a value is not in some bad state. An operand owing only witnessless
    /// obligations names no such state, so the form has nothing to prove and is rejected.
    pub(super) fn require_witnessed_operand(&self, flow: &Flow, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let Flow::Bad { obligations, .. } = flow else { return Ok(()) };
        if obligations.iter().any(|o| self.sigs.witness(*o).is_some()) {
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
            None => site.guidance(sorted_obligation_names(self.hir, obligations)[0]).to_string(),
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

    pub(super) fn operand_type_name(&self, typed: &Typed, node: &HirId<HirExpr>) -> String {
        if let Some(witness) = self.confirmed_witness_name(typed) {
            return witness.to_string();
        }
        match self.hir.get(node) {
            HirExpr::Literal(HirLiteral::Number(_)) => "number".to_string(),
            HirExpr::Literal(HirLiteral::String(_)) => "string".to_string(),
            HirExpr::Literal(HirLiteral::Boolean(_)) => "boolean".to_string(),
            HirExpr::Literal(HirLiteral::Null) => "null".to_string(),
            _ => match &typed.tag {
                TypeTag::Concrete(decl) => match self.type_name_of(decl) {
                    Some(name) => self.hir.text(name).to_string(),
                    None => "value".to_string(),
                },
                _ => "value".to_string(),
            },
        }
    }

    /// The type or trait that witnesses an obligation at runtime, if it has one.
    pub(super) fn witness_name(&self, obligation: Symbol) -> Option<&'a str> {
        match self.sigs.witness(obligation)? {
            Witness::Type(id) | Witness::Trait(id) => self.hir.type_info(*id).map(|info| self.hir.text(info.name)),
            Witness::Null => Some("null"),
        }
    }
}

impl<'a> Checker<'a> {
    /// The flow a fresh instance carries. Constructing a type puts the value in the bad state of
    /// every obligation that type witnesses, its own and any carried by a trait it mixes.
    pub(super) fn construction_flow(&self, decl: &HirId<HirStmt>) -> Flow {
        let obligations = match self.hir.get(decl) {
            HirStmt::Type(decl) => self.sigs.obligations_witnessed_by_decl(decl),
            _ => Obligations::new(),
        };
        match obligations.is_empty() {
            true => Flow::Clean,
            false => Flow::Bad { obligations, definite: true, container: false },
        }
    }

    pub(super) fn owes_object_witness(&self, flow: &Flow) -> bool {
        matches!(flow, Flow::Bad { obligations, .. }
            if obligations.iter().any(|o| matches!(self.sigs.witness(*o), Some(Witness::Type(_) | Witness::Trait(_)))))
    }

    /// The type witness of the built-in `fails` obligation.
    pub(super) fn err_witness(&self) -> Option<TypeId> {
        match self.sigs.witness(self.sigs.fails) {
            Some(Witness::Type(e)) => Some(*e),
            _ => None,
        }
    }

    /// Records which witnesses a discharge node must test at runtime.
    pub(super) fn record_witness_test(&mut self, node: &HirId<HirExpr>, flow: &Flow) {
        let Flow::Bad { obligations, .. } = flow else { return };
        let err = self.err_witness();
        let mut set = WitnessSet { null: false, witnesses: Vec::new(), contains_user_witnesses: false };
        for &o in obligations {
            match self.sigs.witness(o) {
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
            Some(Witness::Type(w)) if self.sigs.decl_of_id(*w) == Some(*tag) =>
                self.type_name_of(tag).map(|name| self.hir.text(name)),
            _ => None,
        })
    }

    /// Whether the value is confirmed to be one of the witnesses it owes.
    pub(super) fn confirmed_witness(&self, typed: &Typed) -> bool {
        self.confirmed_witness_name(typed).is_some()
    }

    /// The tag a caught value narrows to. A single type witness confirms the value's type. A set,
    /// a trait witness, or `opt` leaves it unknown.
    pub(super) fn single_object_witness_tag(&self, caught: &Obligations) -> TypeTag {
        let mut it = caught.iter();
        match (it.next(), it.next()) {
            (Some(o), None) => match self.sigs.witness(*o) {
                Some(Witness::Type(id)) => self.sigs.decl_of_id(*id).map_or(TypeTag::Unknown, TypeTag::Concrete),
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
        let mut obligations = self.owed_of(operand);
        obligations.insert(self.sigs.opt);
        Typed::of(Flow::Bad { obligations, definite: false, container: false }, TypeTag::Unknown)
    }
}

impl<'a> Checker<'a> {
    /// Checks each argument's obligations against its parameter's clause. A parameter admits exactly
    /// what it declares.
    pub(super) fn check_arg_obligations(&mut self, callee: &HirId<HirExpr>, clauses: &[Obligations], arg_types: &[Typed], args: &[HirId<HirExpr>]) -> Result<(), anyhow::Error> {
        let mut handed: Vec<usize> = Vec::new();
        for (i, admits) in clauses.iter().enumerate() {
            let Some(typed) = arg_types.get(i) else { break };
            let undeclared = self.undeclared_obligations(&typed.flow, admits);
            if undeclared.is_empty() {
                // The debt moves to the callee, but only for what this parameter declares.
                handed.push(i);
                continue;
            }
            let owed = quoted_obligation_list(self.hir, &undeclared);
            let subject = self.quoted_subject(&args[i]);
            let c = self.callee_name(callee);
            return Err(self.error_ctx_help(
                format!("cannot pass a value owing {owed}"),
                self.hir.pos(&args[i]), format!("{subject} owes {owed}"),
                self.hir.pos(callee), format!("{c} does not declare {owed} here"),
                "discharge it before the call, or declare it on the parameter"));
        }
        for i in handed {
            self.mark_settled(&args[i], &clauses[i]);
        }
        Ok(())
    }

    /// Checks each argument against a callee's per-parameter nullability.
    pub(super) fn check_args(&mut self, callee: &HirId<HirExpr>, params: &[bool], arg_types: &[Typed], args: &[HirId<HirExpr>]) -> Result<(), anyhow::Error> {
        for (i, &param_nullable) in params.iter().enumerate() {
            if param_nullable {
                continue;
            }
            let Some(typed) = arg_types.get(i) else { break };
            self.check_arg(callee, &typed.flow, i, &args[i])?;
        }
        Ok(())
    }

    /// Checks a single argument value against a non-null parameter slot.
    pub(super) fn check_arg(&mut self, callee: &HirId<HirExpr>, flow: &Flow, position: usize, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let n = position + 1;
        let site_label = format!("{} requires a non-null value here", self.callee_name(callee));
        match self.non_null_violation(flow, node) {
            None => Ok(()),
            Some(Violation::Void) => Err(self.error(format!("Argument {n} is a void result; the call returns no value"), node)),
            Some(Violation::Null) => Err(self.error_ctx("expected non-null argument", self.hir.pos(node), "this argument is null", self.hir.pos(callee), site_label)),
            Some(Violation::Nullable) => Err(self.error_ctx_help("expected non-null argument", self.hir.pos(node), "this argument may be null", self.hir.pos(callee), site_label, "narrow it before the call")),
        }
    }

    /// Checks each argument against a native's per-parameter accepted obligation set.
    pub(super) fn check_native_args(&mut self, callee: &HirId<HirExpr>, sig: &NativeSig, arg_types: &[Typed], args: &[HirId<HirExpr>]) -> Result<(), anyhow::Error> {
        let nullable: Vec<bool> = sig.params.iter().map(|p| p.opt).collect();
        self.check_args(callee, &nullable, arg_types, args)
    }

    /// Whether a value owes a `no persist` obligation, so an opaque call must not persist it.
    pub(super) fn arg_owes_no_persist(&self, flow: &Flow) -> bool {
        matches!(flow, Flow::Bad { obligations, .. } if self.owes_no_persist(obligations.iter().copied()))
    }

    pub(super) fn ret_flow(&self, ret: &RetSig) -> Flow {
        if ret.void {
            Flow::Void
        } else if ret.obligations.is_empty() {
            Flow::Clean
        } else {
            Flow::Bad { obligations: ret.obligations.clone(), definite: false, container: false }
        }
    }

    /// The obligations a native call result carries, from its fixed return signature.
    pub(super) fn native_ret_flow(&self, ret: native::RetSig) -> Flow {
        if ret.void {
            return Flow::Void;
        }
        let mut obligations = Obligations::new();
        if ret.set.opt { obligations.insert(self.sigs.opt); }
        if ret.set.fails { obligations.insert(self.sigs.fails); }
        if obligations.is_empty() {
            Flow::Clean
        } else {
            Flow::Bad { obligations, definite: false, container: false }
        }
    }
    /// The nullability, type, and mutability of a call result, given the callee and receiver tag.
    pub(super) fn call_result(&self, stmt: HirId<HirStmt>, receiver_tag: &TypeTag) -> Typed {
        let flow = self.sigs.fns.get(&stmt).map_or(Flow::Unknown, |s| self.ret_flow(&s.ret));
        let mutability = self.sigs.ret_mut.get(&stmt).copied().unwrap_or(Mutability::Unknown);
        let tag = self.sigs.ret_tags.get(&stmt).map_or(TypeTag::Unknown, |t| t.resolve(receiver_tag));
        Typed::of(flow, tag).with_mutability(mutability)
    }

    /// Downgrades a frozen argument's slot to immutable in place.
    pub(super) fn discharge_freeze(&mut self, arg: &HirId<HirExpr>) {
        let HirExpr::Identifier(name) = self.hir.get(arg) else { return };
        if let Some(i) = self.frame_index_of(*name) {
            self.locals[i].alias.mutability = Mutability::Immutable;
        }
    }

    /// Checks a value moving into a field per the field's nullability.
    pub(super) fn check_into_field(&mut self, flow: &Flow, field_nullable: bool, field: Symbol, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        // Storing into a field persists the value, which a `no persist` value forbids.
        self.reject_outliving(flow, super::Site::Field, node)?;
        let text = self.hir.text(field);
        let void = || format!("Cannot assign a void result to field '{text}'; the call returns no value");
        if field_nullable {
            // A nullable field still rejects a void result, which is not a value.
            if flow.is_void() {
                return Err(self.error(void(), node));
            }
            // An unknown value into an `opt` field is guarded against every object witness it may be.
            if matches!(flow, Flow::Unknown) {
                self.record_boundary_barrier(node, &Obligations::from([self.sigs.opt]));
            }
            return Ok(());
        }
        match self.non_null_violation(flow, node) {
            None => Ok(()),
            Some(Violation::Void) => Err(self.error(void(), node)),
            Some(Violation::Null) => Err(self.error(format!("Cannot assign null to non-null field '{text}'"), node)),
            Some(Violation::Nullable) => Err(self.error(format!("Cannot assign a nullable value to non-null field '{text}'"), node)),
        }
    }

    /// Checks a brace-construction value against its field's declared nullability.
    pub(super) fn check_brace_field(&mut self, decl: &HirId<HirStmt>, field: Symbol, flow: &Flow, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let nullable = match self.layout_of(decl) {
            Some(layout) => layout.is_nullable(field),
            None => return Ok(()),
        };
        self.check_into_field(flow, nullable, field, node)
    }
    /// If `lhs = rhs` is a factory epilogue copy `this.<field> = $<field>` whose field-local is
    /// still unassigned, returns the field. An unassigned field-local is always a non-null field
    /// with no default, so this is exactly the "field never initialized" case.
    pub(super) fn uninitialized_epilogue_field(&self, lhs: &HirId<HirExpr>, rhs: &HirId<HirExpr>) -> Option<Symbol> {
        let HirExpr::Identifier(local) = self.hir.get(rhs) else { return None };
        if !self.is_field_local(*local) {
            return None;
        }
        let HirExpr::Index(target, member, _) = self.hir.get(lhs) else { return None };
        if !matches!(self.hir.get(target), HirExpr::This) {
            return None;
        }
        let i = self.frame_index_of(*local)?;
        if self.locals[i].assigned {
            return None;
        }
        self.string_member(member)
    }

    pub(super) fn collect_witness_obligations<T>(&self, matcher: &HirId<HirMatcher>, at: &HirId<T>, out: &mut HashMap<Symbol, Obligations>) -> Result<(), anyhow::Error> {
        match self.hir.get(matcher) {
            HirMatcher::Or(alternatives) => self.or_witness_obligations(alternatives, at, out),
            HirMatcher::As(name, inner) => {
                // `x @ p | null` names the whole value, so `x` owes what that or-group admits.
                let admits = self.resolved().admitted_obligations(inner);
                if !admits.is_empty() {
                    out.entry(*name).or_default().extend(admits);
                }
                self.collect_witness_obligations(inner, at, out)
            },
            HirMatcher::Type { nominal, shape: Some(shape), .. } => {
                if let (true, Some(decl)) = (*nominal, self.bindings.type_ref(matcher)) {
                    self.recover_shape_fields(&decl, shape, out);
                }
                self.collect_witness_obligations(shape, at, out)
            },
            HirMatcher::Shape(fields) => { for field in fields { self.collect_witness_obligations(&field.value, at, out)?; } Ok(()) },
            HirMatcher::Array(elements) => {
                for element in elements {
                    if let HirMatchElem::Elem(m) = element { self.collect_witness_obligations(m, at, out)?; }
                }
                Ok(())
            },
            HirMatcher::And(parts) => { for part in parts { self.collect_witness_obligations(part, at, out)?; } Ok(()) },
            _ => Ok(()),
        }
    }

    /// An or-group is one test written in several shapes. At most one alternative binds the value;
    /// the rest only say what it may be. Those tests are what the binders end up owing.
    fn or_witness_obligations<T>(&self, alternatives: &[HirId<HirMatcher>], at: &HirId<T>, out: &mut HashMap<Symbol, Obligations>) -> Result<(), anyhow::Error> {
        // With nothing to bind, there is no name to hand the group to.
        let Some(binding) = alternatives.iter().find(|a| self.hir.get(*a).binds_anything(self.hir)) else { return Ok(()) };

        let mut admitted = Obligations::new();
        for alt in alternatives {
            if self.hir.get(alt).binds_anything(self.hir) {
                self.collect_witness_obligations(alt, at, out)?;
                continue;
            }
            // A test beside a binding narrows what the binder receives. One that witnesses nothing
            // narrows nothing, so no value could reach the binder through it.
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
        Ok(())
    }

    pub(super) fn collect_condition_witness_obligations(&self, cond: &HirId<HirExpr>, out: &mut HashMap<Symbol, Obligations>) -> Result<(), anyhow::Error> {
        match self.hir.get(cond) {
            HirExpr::Match(_, matcher) => self.collect_witness_obligations(matcher, cond, out),
            HirExpr::Binary(BinOp::And | BinOp::Or, left, right) => {
                self.collect_condition_witness_obligations(left, out)?;
                self.collect_condition_witness_obligations(right, out)
            },
            _ => Ok(()),
        }
    }

    /// The witness obligations each binder inherits from a bindingless alternative sharing its
    /// or-group. In `Node { next } | null` the `next` binder owes `opt`. A bindingless alternative
    /// beside a destructure must be a witness. A non-witness there is a dead binding.
    pub(super) fn matcher_witness_obligations<T>(&self, matcher: &HirId<HirMatcher>, at: &HirId<T>) -> Result<HashMap<Symbol, Obligations>, anyhow::Error> {
        let mut out = HashMap::new();
        self.collect_witness_obligations(matcher, at, &mut out)?;
        Ok(out)
    }

    /// The witness obligations of the `~` matchers in a condition, so an `if`/`while` binder from a
    /// destructure-beside-witness owes the same as a `match` arm binder.
    pub(super) fn condition_witness_obligations(&self, cond: &HirId<HirExpr>) -> Result<HashMap<Symbol, Obligations>, anyhow::Error> {
        let mut out = HashMap::new();
        self.collect_condition_witness_obligations(cond, &mut out)?;
        Ok(out)
    }

    /// Inside a method `this` is a concrete instance of the type, so there is no null or `Err`
    /// receiver for a witnessed obligation to admit.
    pub(super) fn reject_receiver_witnesses(&self, decl: &HirFnDecl) -> Result<(), anyhow::Error> {
        let Some(clause) = &decl.receiver else { return Ok(()) };
        let Some(name) = clause.names.iter().find(|n| self.sigs.witness(**n).is_some()) else { return Ok(()) };
        let text = self.hir.text(*name);
        let pos = clause.pos.clone().unwrap_or_else(|| decl.sig_pos.clone());
        Err(anyhow!("{}", Diagnostic::new(format!("The receiver cannot owe '{text}'"), pos)
            .with_label(format!("'{text}' admits a value `this` cannot be"))
            .with_help("`this` is always an instance of the type; put the obligation on a parameter instead")))
    }

    /// Records a discharge that guards every witness the binding owes, such as `??`, `!` or `?!`.
    pub(super) fn mark_handled(&mut self, node: &HirId<HirExpr>) {
        let Some(i) = self.local_of(node) else { return };
        let mut handled = std::mem::take(&mut self.locals[i].handled);
        handled.extend(self.locals[i].owed.iter().copied());
        self.locals[i].handled = handled;
    }

    /// Records what a partial act settled: a test that rules out one witness, or a transfer into a
    /// slot declaring part of the debt.
    pub(super) fn mark_settled(&mut self, node: &HirId<HirExpr>, settled: &Obligations) {
        if let Some(i) = self.local_of(node) {
            self.locals[i].handled.extend(settled.iter().copied());
        }
    }

    /// Refuses a value that would outlive its binding.
    pub(super) fn reject_outliving(&self, flow: &Flow, site: Site, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        self.reject_at(flow, Rule::BeforeDrop, site, node)?;
        self.reject_at(flow, Rule::NoPersist, site, node)
    }
}

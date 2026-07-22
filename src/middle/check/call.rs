//! Call-site checks: callee, argument conformance, and result nullability.

use std::collections::HashSet;

use anyhow::anyhow;

use crate::frontend::lex::{Diagnostic, SourcePosition};
use crate::middle::hir::{Capability, HirExpr, HirId, HirStmt};
use crate::middle::signatures::RetSig;

use crate::middle::native::{self, Container, NativeSig};
use super::{Mutability, Checker, Flow, TypeTag, Typed, Violation};

impl<'a> Checker<'a> {
    pub(super) fn call(&mut self, callee: &HirId<HirExpr>, args: &[HirId<HirExpr>]) -> Result<Typed, anyhow::Error> {
        let arg_types: Vec<Typed> = args.iter().map(|a| self.expr(a)).collect::<Result<_, _>>()?;
        match self.hir.get(callee) {
            HirExpr::Identifier(name) => {
                let name = *name;
                if self.sigs.is_type(name) {
                    self.check_construction(name, &HashSet::new(), callee)?;
                    if let Some(init) = self.constructor_init(callee) {
                        self.check_call_args(callee, init, &arg_types, args)?;
                    }
                    return Ok(Typed::of(Flow::Clean, TypeTag::Concrete(name)));
                }
                if self.hir.text(name) == "Err" && self.frame_index_of(name).is_none() {
                    return Ok(Typed::of(self.fails_flow(), TypeTag::Unknown));
                }
                if let Some(stmt) = self.func_of(name) {
                    self.check_call_args(callee, stmt, &arg_types, args)?;
                    return Ok(self.call_result(stmt, &TypeTag::Unknown));
                }
                // A built-in global resolves by name when no local or function shadows it.
                if self.frame_index_of(name).is_none() {
                    if let Some(sig) = native::builtin(self.hir.text(name)) {
                        self.check_native_args(callee, &sig, &arg_types, args)?;
                        let result = Typed::of(self.native_ret_flow(sig.ret), TypeTag::Unknown);
                        // `freeze(x)` also discharges the mutation capability, handing its
                        // argument back immutable.
                        if self.hir.text(name) == "freeze" {
                            if let Some(arg) = args.first() { self.discharge_freeze(arg); }
                            return Ok(result.with_mutability(Mutability::Immutable));
                        }
                        return Ok(result);
                    }
                }
                self.check_opaque_call(callee, args, &arg_types)
            },
            HirExpr::Index(receiver, member, _) => self.method_call(callee, receiver, member, &arg_types, args),
            _ => self.check_opaque_call(callee, args, &arg_types),
        }
    }

    /// Checks an opaque call. Each mutable argument is either consumed or, when it is a borrow
    /// the caller cannot give away, guarded by a runtime assertion that the callee borrows it.
    fn check_opaque_call(&mut self, callee: &HirId<HirExpr>, args: &[HirId<HirExpr>], arg_types: &[Typed]) -> Result<Typed, anyhow::Error> {
        let mut survive = Vec::new();
        for (i, typed) in arg_types.iter().enumerate() {
            // A `no persist` value must survive: the opaque callee may not persist it.
            if self.arg_owes_no_persist(&typed.flow) {
                survive.push(i as u8);
                continue;
            }
            if typed.mutability != Mutability::Mutable {
                continue;
            }
            if self.arg_is_borrowed(&args[i]) {
                survive.push(i as u8);
            } else {
                // An owned mutable handed to an opaque call is consumed by it.
                self.move_source(&args[i]);
            }
        }
        if !survive.is_empty() {
            self.record_survive_barrier(callee, survive.clone());
            // Mark the borrowed args so the runtime panics if an opaque callee tries to persist them.
            self.record_borrow_marks(callee, survive);
        }
        self.indirect_call(callee)
    }

    pub(super) fn arg_is_borrowed(&self, arg: &HirId<HirExpr>) -> bool {
        let HirExpr::Identifier(name) = self.hir.get(arg) else { return false };
        self.frame_index_of(*name).is_some_and(|i| self.locals[i].borrowed)
    }

    /// Whether a value owes a `no persist` obligation, so an opaque call must not persist it.
    fn arg_owes_no_persist(&self, flow: &Flow) -> bool {
        matches!(flow, Flow::Bad { obligations, .. } if self.owes_no_persist(obligations.iter().copied()))
    }

    /// Downgrades a frozen argument's slot to immutable in place.
    fn discharge_freeze(&mut self, arg: &HirId<HirExpr>) {
        let HirExpr::Identifier(name) = self.hir.get(arg) else { return };
        if let Some(i) = self.frame_index_of(*name) {
            self.locals[i].mutability = Mutability::Immutable;
        }
    }

    /// A method call `receiver.name(args)`. Resolves against the receiver's type when it is
    /// known, then falls back to a native-type method, and finally to a dynamic boundary.
    fn method_call(&mut self, callee: &HirId<HirExpr>, receiver: &HirId<HirExpr>, member: &HirId<HirExpr>, arg_types: &[Typed], args: &[HirId<HirExpr>]) -> Result<Typed, anyhow::Error> {
        let Some(name) = self.member_text(member) else { return self.indirect_call(callee) };
        let receiver_typed = self.receiver(receiver)?;
        if matches!(receiver_typed.tag, TypeTag::SelfType) {
            return self.trait_member(name, member);
        }
        if let (TypeTag::Concrete(type_name), Some(method)) = (&receiver_typed.tag, self.hir.symbol_of(name)) {
            if let Some(stmt) = self.sigs.methods_by_type.get(&(*type_name, method)).copied() {
                self.check_call_args(callee, stmt, arg_types, args)?;
                return Ok(self.call_result(stmt, &receiver_typed.tag));
            }
        }
        // A native-type method resolves by name when no user method matches the receiver.
        if let Some(sig) = native::native_method(name) {
            self.check_native_args(callee, &sig, arg_types, args)?;
            if sig.container == Container::Preserves {
                for (typed, arg) in arg_types.iter().zip(args) {
                    self.store_into_container(&typed.flow, arg)?;
                }
                self.preserve_into_receiver(receiver, arg_types);
            }
            return Ok(Typed::of(self.native_ret_flow(sig.ret), TypeTag::Unknown));
        }
        Ok(Typed::unknown())
    }

    /// A call through a value: the callee must be non-null and its result is a dynamic boundary.
    fn indirect_call(&mut self, callee: &HirId<HirExpr>) -> Result<Typed, anyhow::Error> {
        let callee_typed = self.expr(callee)?;
        if let Some(witness) = self.confirmed_witness_name(&callee_typed) {
            let subject = self.arg_name(callee);
            return Err(self.confirmed_use_error(format!("{subject} is not callable"), callee, witness));
        }
        self.require_usable_value(&callee_typed, callee)?;
        Ok(Typed::unknown())
    }

    /// The nullability, type, and mutability of a call result, given the callee and receiver tag.
    fn call_result(&self, stmt: HirId<HirStmt>, receiver_tag: &TypeTag) -> Typed {
        let flow = self.sigs.fns.get(&stmt).map_or(Flow::Unknown, |s| self.ret_flow(&s.ret));
        let mutability = self.sigs.ret_mut.get(&stmt).copied().unwrap_or(Mutability::Unknown);
        let tag = self.sigs.ret_tags.get(&stmt).map_or(TypeTag::Unknown, |t| t.resolve(receiver_tag));
        Typed::of(flow, tag).with_mutability(mutability)
    }

    /// Checks a user call's arguments against the resolved function's declared parameters.
    fn check_call_args(&mut self, callee: &HirId<HirExpr>, callee_fn: HirId<HirStmt>, arg_types: &[Typed], args: &[HirId<HirExpr>]) -> Result<(), anyhow::Error> {
        // Read the params through the shared signatures borrow so the later check can take &mut self.
        let sigs = self.sigs;
        let Some(sig) = sigs.fns.get(&callee_fn) else { return Ok(()) };
        let nullable: Vec<bool> = sig.param_clauses.iter().map(|p| p.contains(&sigs.opt)).collect();
        self.check_arg_mutability(callee, callee_fn, &sig.param_markers, arg_types, args)?;
        self.check_args(callee, &nullable, arg_types, args)?;
        self.consume_move_args(&sig.param_markers, args);
        // A mutable argument lent to a non-consuming parameter is borrowed for the call, so mark it.
        let marks: Vec<u8> = sig.param_markers.iter().enumerate()
            .filter(|(i, m)| !m.is_move() && arg_types.get(*i).is_some_and(|t| t.mutability == Mutability::Mutable))
            .map(|(i, _)| i as u8)
            .collect();
        self.record_borrow_marks(callee, marks);
        Ok(())
    }

    /// Moves each argument passed to a `*mut` parameter. A plain `mut` parameter borrows, so
    /// it leaves the argument live.
    fn consume_move_args(&mut self, markers: &[Capability], args: &[HirId<HirExpr>]) {
        for (i, &marker) in markers.iter().enumerate() {
            if matches!(marker, Capability::MoveMut) {
                if let Some(arg) = args.get(i) { self.move_source(arg); }
            }
        }
    }

    /// Matches each argument's mutability against its parameter marker.
    fn check_arg_mutability(&self, callee: &HirId<HirExpr>, callee_fn: HirId<HirStmt>, markers: &[Capability], arg_types: &[Typed], args: &[HirId<HirExpr>]) -> Result<(), anyhow::Error> {
        for (i, &marker) in markers.iter().enumerate() {
            let Some(typed) = arg_types.get(i) else { break };
            if marker.is_mut() {
                if typed.mutability == Mutability::Immutable {
                    return Err(self.needs_mut_error(callee, &args[i]));
                }
                // A borrow cannot be given away, so it may not feed a consuming parameter.
                if marker.is_move() && self.arg_is_borrowed(&args[i]) {
                    return Err(self.consumes_borrow_error(callee, &args[i]));
                }
            } else if typed.mutability == Mutability::Mutable {
                // A read-only helper borrows the mutable, so it is admitted only where the callee
                // neither persists nor mutates it.
                if self.sigs.param_escapes_at(&callee_fn, i) {
                    return Err(self.keeps_argument_error(callee, &args[i]));
                }
                if self.sigs.param_mutates_at(&callee_fn, i) {
                    return Err(self.mutates_argument_error(callee, &args[i]));
                }
            }
        }
        Ok(())
    }

    /// The error for passing a mutable value to a parameter that persists it.
    fn keeps_argument_error(&self, callee: &HirId<HirExpr>, arg: &HirId<HirExpr>) -> anyhow::Error {
        let subject = self.quoted_subject(arg);
        let c = self.callee_name(callee);
        self.error_ctx(
            format!("{subject} is mutable and this function keeps its argument; freeze or copy it, or take it by '*mut'"),
            self.hir.pos(arg), format!("{subject} is mutable"),
            self.hir.pos(callee), format!("{c} keeps its argument"))
    }

    /// The error for passing a mutable value to a parameter the callee mutates in place. The fix is
    /// on the callee's parameter, so the help leads there. Freezing is no help, since the callee
    /// needs to write the value.
    fn mutates_argument_error(&self, callee: &HirId<HirExpr>, arg: &HirId<HirExpr>) -> anyhow::Error {
        let subject = self.quoted_subject(arg);
        let c = self.callee_name(callee);
        self.error_ctx_help(
            "mutable value lent to a parameter that mutates it".to_string(),
            self.hir.pos(arg), format!("{subject} is mutable"),
            self.hir.pos(callee), format!("{c} mutates its argument"),
            format!("declare the parameter `mut` to let it mutate the borrow, or pass a `copy` to leave {subject} unchanged"))
    }

    /// The error for handing a borrowed value to a parameter that consumes it.
    fn consumes_borrow_error(&self, callee: &HirId<HirExpr>, arg: &HirId<HirExpr>) -> anyhow::Error {
        let (a, c) = (self.arg_name(arg), self.callee_name(callee));
        let mut diag = Diagnostic::new(format!("cannot move borrowed value {a}"), self.hir.pos(arg).clone())
            .with_label(format!("{a} is moved here"));
        if let Some(param_pos) = self.borrowed_param_pos(arg) {
            let fname = self.fn_ctx.name.map_or("this function".to_string(), |s| format!("`{}`", self.hir.text(s)));
            diag = diag.with_context_span(param_pos.clone(), format!("{fname} only borrows {a} here; it does not own it"));
        }
        diag = diag
            .with_context_span(self.hir.pos(callee).clone(), format!("{c} consumes {a}"))
            .with_help(format!("take ownership of {a} with `*mut` to move it into {c}"));
        anyhow!("{}", diag)
    }

    /// The declaration span of the current function's parameter named by `arg`.
    fn borrowed_param_pos(&self, arg: &HirId<HirExpr>) -> Option<&SourcePosition> {
        let HirExpr::Identifier(name) = self.hir.get(arg) else { return None };
        self.fn_ctx.params.iter().find(|(sym, _)| sym == name).map(|(_, pos)| pos)
    }

    /// The error for passing an immutable value to a parameter that requires a mutable one.
    fn needs_mut_error(&self, callee: &HirId<HirExpr>, arg: &HirId<HirExpr>) -> anyhow::Error {
        self.arg_cap_error(callee, arg, "mutable", "immutable")
    }

    /// A capability-mismatch error: the parameter wants a `want` argument but got a `got` one.
    fn arg_cap_error(&self, callee: &HirId<HirExpr>, arg: &HirId<HirExpr>, want: &str, got: &str) -> anyhow::Error {
        let (a, c) = (self.arg_name(arg), self.callee_name(callee));
        self.error_ctx(format!("expected {want} argument"), self.hir.pos(arg), format!("{a} is {got}"),
            self.hir.pos(callee), format!("{c} expects {a} to be {want}"))
    }

    /// Single-quoted name of a value node for a message subject, or "this value" when unnamed.
    pub(super) fn quoted_subject(&self, node: &HirId<HirExpr>) -> String {
        match self.hir.get(node) {
            HirExpr::Identifier(name) => format!("'{}'", self.hir.text(*name)),
            _ => "this value".to_string(),
        }
    }

    /// Backtick-quoted name of an argument for a capability label.
    pub(super) fn arg_name(&self, arg: &HirId<HirExpr>) -> String {
        match self.hir.get(arg) {
            HirExpr::Identifier(name) => format!("`{}`", self.hir.text(*name)),
            _ => "this value".to_string(),
        }
    }

    /// Backtick-quoted name of a call's callee for a capability label.
    fn callee_name(&self, callee: &HirId<HirExpr>) -> String {
        match self.hir.get(callee) {
            HirExpr::Identifier(name) => format!("`{}`", self.hir.text(*name)),
            HirExpr::Index(_, member, true) => self.member_text(member).map_or_else(|| "this function".to_string(), |m| format!("`{m}`")),
            _ => "this function".to_string(),
        }
    }

    /// Checks each argument against a callee's per-parameter nullability.
    fn check_args(&mut self, callee: &HirId<HirExpr>, params: &[bool], arg_types: &[Typed], args: &[HirId<HirExpr>]) -> Result<(), anyhow::Error> {
        for (i, &param_nullable) in params.iter().enumerate() {
            if param_nullable {
                continue;
            }
            let Some(typed) = arg_types.get(i) else { break };
            self.check_arg(callee, &typed.flow, i, &args[i])?;
        }
        Ok(())
    }

    /// Checks each argument against a native's per-parameter accepted obligation set.
    fn check_native_args(&mut self, callee: &HirId<HirExpr>, sig: &NativeSig, arg_types: &[Typed], args: &[HirId<HirExpr>]) -> Result<(), anyhow::Error> {
        let nullable: Vec<bool> = sig.params.iter().map(|p| p.opt).collect();
        self.check_args(callee, &nullable, arg_types, args)
    }

    /// Flows a stored argument's obligations onto the receiver container, so pushing a pending
    /// value makes the array carry that obligation.
    fn preserve_into_receiver(&mut self, receiver: &HirId<HirExpr>, arg_types: &[Typed]) {
        let mut obligations = HashSet::new();
        for typed in arg_types {
            if let Flow::Bad { obligations: o, .. } = &typed.flow {
                obligations.extend(o.iter().copied());
            }
        }
        if obligations.is_empty() {
            return;
        }
        let HirExpr::Identifier(name) = self.hir.get(receiver) else { return };
        let Some(i) = self.frame_index_of(*name) else { return };
        self.locals[i].owed.extend(obligations);
        self.locals[i].container = true;
    }

    /// Checks a single argument value against a non-null parameter slot.
    fn check_arg(&mut self, callee: &HirId<HirExpr>, flow: &Flow, position: usize, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let n = position + 1;
        let site_label = format!("{} requires a non-null value here", self.callee_name(callee));
        match self.non_null_violation(flow, node) {
            None => Ok(()),
            Some(Violation::Void) => Err(self.error(format!("Argument {n} is a void result; the call returns no value"), node)),
            Some(Violation::Null) => Err(self.error_ctx("expected non-null argument", self.hir.pos(node), "this argument is null", self.hir.pos(callee), site_label)),
            Some(Violation::Nullable) => Err(self.error_ctx_help("expected non-null argument", self.hir.pos(node), "this argument may be null", self.hir.pos(callee), site_label, "narrow it before the call")),
        }
    }

    fn ret_flow(&self, ret: &RetSig) -> Flow {
        if ret.void {
            Flow::Void
        } else if ret.obligations.is_empty() {
            Flow::Clean
        } else {
            Flow::Bad { obligations: ret.obligations.clone(), definite: false, container: false }
        }
    }

    /// The obligations a native call result carries, from its fixed return signature.
    fn native_ret_flow(&self, ret: native::RetSig) -> Flow {
        if ret.void {
            return Flow::Void;
        }
        let mut obligations = HashSet::new();
        if ret.set.opt { obligations.insert(self.sigs.opt); }
        if ret.set.fails { obligations.insert(self.sigs.fails); }
        if obligations.is_empty() {
            Flow::Clean
        } else {
            Flow::Bad { obligations, definite: false, container: false }
        }
    }
}

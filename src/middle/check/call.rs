//! Call-site checks: callee, argument conformance, and result nullability.

use std::collections::HashSet;

use crate::middle::hir::{HirExpr, HirId, HirStmt};
use crate::middle::signatures::RetSig;

use super::native::{self, Container, NativeSig};
use super::{Checker, Flow, TypeTag, Typed, Violation};

impl<'a> Checker<'a> {
    pub(super) fn call(&mut self, callee: &HirId<HirExpr>, args: &[HirId<HirExpr>]) -> Result<Typed, anyhow::Error> {
        let arg_types: Vec<Typed> = args.iter().map(|a| self.expr(a)).collect::<Result<_, _>>()?;
        match self.hir.get(callee) {
            HirExpr::Identifier(name) => {
                let name = *name;
                if self.sigs.is_type(name) {
                    self.check_construction(name, &HashSet::new(), callee)?;
                    return Ok(Typed::of(Flow::Clean, TypeTag::Concrete(name)));
                }
                if self.hir.text(name) == "Err" && self.frame_index_of(name).is_none() {
                    return Ok(Typed::of(self.fails_flow(), TypeTag::Unknown));
                }
                if let Some(stmt) = self.func_of(name) {
                    self.check_call_args(stmt, &arg_types, args)?;
                    return Ok(self.call_result(stmt, &TypeTag::Unknown));
                }
                // A built-in global resolves by name when no local or function shadows it.
                if self.frame_index_of(name).is_none() {
                    if let Some(sig) = native::builtin(self.hir.text(name)) {
                        self.check_native_args(&sig, &arg_types, args)?;
                        return Ok(Typed::of(self.native_ret_flow(sig.ret), TypeTag::Unknown));
                    }
                }
                self.indirect_call(callee)
            },
            HirExpr::Index(receiver, member, _) => self.method_call(callee, receiver, member, &arg_types, args),
            _ => self.indirect_call(callee),
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
                self.check_call_args(stmt, arg_types, args)?;
                return Ok(self.call_result(stmt, &receiver_typed.tag));
            }
        }
        // A native-type method resolves by name when no user method matches the receiver.
        if let Some(sig) = native::native_method(name) {
            self.check_native_args(&sig, arg_types, args)?;
            if sig.container == Container::Preserves {
                // A stored argument persists into the container, which a `discharge to escape` value forbids.
                for (typed, arg) in arg_types.iter().zip(args) {
                    self.reject_escape(&typed.flow, arg)?;
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
            let subject = match self.hir.get(callee) {
                HirExpr::Identifier(name) => format!("`{}`", self.hir.text(*name)),
                _ => "this value".to_string(),
            };
            return Err(self.confirmed_use_error(format!("{subject} is not callable"), callee, witness));
        }
        self.require_usable_value(&callee_typed, callee)?;
        Ok(Typed::unknown())
    }

    /// The nullability and type of a call result, given the callee and receiver tag.
    fn call_result(&self, stmt: HirId<HirStmt>, receiver_tag: &TypeTag) -> Typed {
        let flow = self.sigs.fns.get(&stmt).map_or(Flow::Unknown, |s| self.ret_flow(&s.ret));
        let tag = self.sigs.ret_tags.get(&stmt).map_or(TypeTag::Unknown, |t| t.resolve(receiver_tag));
        Typed::of(flow, tag)
    }

    /// Checks a user call's arguments against the resolved function's declared parameters.
    fn check_call_args(&mut self, stmt: HirId<HirStmt>, arg_types: &[Typed], args: &[HirId<HirExpr>]) -> Result<(), anyhow::Error> {
        // Read the params through the shared signatures borrow so the later check can take &mut self.
        let sigs = self.sigs;
        let Some(sig) = sigs.fns.get(&stmt) else { return Ok(()) };
        let nullable: Vec<bool> = sig.params.iter().map(|p| p.contains(&sigs.opt)).collect();
        self.check_args(&nullable, arg_types, args)
    }

    /// Checks each argument against a callee's per-parameter nullability.
    fn check_args(&mut self, params: &[bool], arg_types: &[Typed], args: &[HirId<HirExpr>]) -> Result<(), anyhow::Error> {
        for (i, &param_nullable) in params.iter().enumerate() {
            if param_nullable {
                continue;
            }
            let Some(typed) = arg_types.get(i) else { break };
            self.check_arg(&typed.flow, i, &args[i])?;
        }
        Ok(())
    }

    /// Checks each argument against a native's per-parameter accepted obligation set.
    fn check_native_args(&mut self, sig: &NativeSig, arg_types: &[Typed], args: &[HirId<HirExpr>]) -> Result<(), anyhow::Error> {
        let nullable: Vec<bool> = sig.params.iter().map(|p| p.opt).collect();
        self.check_args(&nullable, arg_types, args)
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
    fn check_arg(&mut self, flow: &Flow, position: usize, node: &HirId<HirExpr>) -> Result<(), anyhow::Error> {
        let n = position + 1;
        match self.non_null_violation(flow, node) {
            None => Ok(()),
            Some(Violation::Void) => Err(self.error(format!("Argument {n} is a void result; the call returns no value"), node)),
            Some(Violation::Null) => Err(self.error(format!("Cannot pass null as argument {n}; the parameter is non-null"), node)),
            Some(Violation::Nullable) => Err(self.error(format!("Argument {n} may be null but the parameter is non-null; narrow it before the call"), node)),
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

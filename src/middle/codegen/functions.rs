use crate::middle::signatures::CallableId;
use crate::core::objects::{FLAG_RETURNS_VALUE, ObjFn, CaptureLocation};
use crate::core::value::Value;
use crate::middle::hir::{HirExpr, HirFnDecl, HirId, HirParam};
use crate::middle::obligations::Obligations;
use crate::middle::ir::{self, Inst, Label};
use crate::middle::bind::FnKind;

use super::{CompiledFn, Compiler, ReturnContract, Returning};

impl<'a> Compiler<'a> {
    fn compile_pattern_param_checks(&mut self, params: &[HirParam]) -> Result<(), anyhow::Error> {
        for param in params {
            let Some(pattern) = &param.pattern else { continue };
            let binders = self.bindings.match_binders(&param.name).unwrap_or_default();
            self.record_pattern_binder_slot_witness_sets(pattern, binders)?;
            self.reserve_slots(binders.len(), &param.name);
            self.expression(&param.name)?;
            self.compile_binding_matcher(pattern, &binders, &param.name)?;
            match self.hir.get(pattern).is_irrefutable(self.hir) {
                true => self.emit(Inst::Pop, &param.name),
                false => self.abort_on_pattern_param_match_fail(param)?,
            }
        }
        Ok(())
    }

    fn abort_on_pattern_param_match_fail(&mut self, param: &HirParam) -> Result<(), anyhow::Error> {
        // The parameter's own source spans the binder and the test, so quoting it names both.
        let message = format!("argument does not match `{}`", param.pos.snippet());
        self.abort_on_pattern_mismatch(message, &param.name)
    }

    pub(super) fn abort_on_pattern_mismatch<T: 'static>(&mut self, message: String, node: &HirId<T>) -> Result<(), anyhow::Error> {
        let matched_label = self.emit_pattern_mismatch_jumps(node);
        self.emit_throw_message(message, node)?;
        self.bind_label(matched_label);
        Ok(())
    }

    pub(super) fn emit_throw_message<T: 'static>(&mut self, message: String, node: &HirId<T>) -> Result<(), anyhow::Error> {
        let message = self.gc.intern(message);
        let idx = self.ir.add_constant(Value::from(message))?;
        self.emit(Inst::PushConstant(idx), node);
        self.emit(Inst::Throw, node);
        Ok(())
    }

    pub(super) fn emit_pattern_mismatch_jumps<T: 'static>(&mut self, node: &HirId<T>) -> Label {
        let matched = self.ir.new_label();
        let failed = self.ir.new_label();
        self.emit(Inst::JumpIfFalse(failed), node);
        self.emit(Inst::Jump(matched), node);
        self.bind_label(failed);
        matched
    }

    fn param_list_pool_id(&mut self, callable: CallableId, decl: &HirFnDecl, kind: FnKind) -> Result<u16, anyhow::Error> {
        let params = &decl.params;
        let clauses: &[Obligations] = self.sigs.fn_sig_of(callable).map_or(&[], |s| &s.param_clauses);
        let mut out = Vec::with_capacity(clauses.len());
        let mut any_anchor = false;
        for (position, owed) in clauses.to_vec().into_iter().enumerate() {
            let takes_anchor = params.get(position).is_some_and(|p| p.anchor);
            any_anchor |= takes_anchor;
            let witness_set_pool_id = self.witness_set_pool_id(&owed)?;
            out.push(witness_set_pool_id | if takes_anchor { ir::PARAM_IS_ANCHOR } else { 0 });
        }
        let receiver_copies = matches!(kind, FnKind::Method)
            && decl.receiver.as_ref().is_some_and(|r| !r.anchor);
        let wants_anchor_receiver = matches!(kind, FnKind::Method)
            && decl.receiver.as_ref().is_some_and(|r| r.anchor);
        let index = self.ir.intern_param_list(out.into_boxed_slice())?;
        Ok(index
            | if any_anchor { ir::PARAM_LIST_HAS_ANCHOR } else { 0 }
            | if receiver_copies { ir::RECEIVER_IS_COPIED } else { 0 }
            | if wants_anchor_receiver { ir::WANTS_ANCHOR_RECEIVER } else { 0 })
    }

    fn return_contract(&mut self, callable: CallableId) -> Result<Option<ReturnContract>, anyhow::Error> {
        let Some(ret) = self.sigs.fn_sig_of(callable).map(|s| s.ret.clone()) else { return Ok(None) };
        let allows_void = ret.void || self.barriers.returns_void(callable);
        let witness_set_pool_id = self.witness_set_pool_id(&ret.obligations)?;
        Ok(Some(ReturnContract { witness_set_pool_id, allows_void }))
    }

    fn share_params_that_outlive<T: 'static>(&mut self, node_id: &HirId<T>, decl: &HirFnDecl, kind: FnKind) {
        if matches!(kind, FnKind::Method) && decl.receiver.as_ref().is_some_and(|r| !r.anchor) {
            self.emit(Inst::ShareLocal(0), node_id);
        }
        for (position, param) in decl.params.iter().enumerate() {
            if param.anchor {
                continue;
            }
            if param.reassignable || self.bindings.is_captured(param.name.index()) {
                self.emit(Inst::ShareLocal(position as u8 + 1), node_id);
            }
        }
    }

    pub (super) fn function<T: 'static>(&mut self, node_id: &HirId<T>, callable: CallableId, decl: &HirFnDecl, kind: FnKind) -> Result<u8, anyhow::Error> {
        // Add a jump over the function's body after declaration.
        // The body should only be reachable via calls to the function.
        let skip = self.ir.new_label();
        self.emit(Inst::Jump(skip), node_id);

        let body = self.ir.new_label();
        self.bind_label(body);

        let contract = self.return_contract(callable)?;
        self.compiled_fns.push(CompiledFn { kind, callable, contract });
        let (_, slot_witness_set_pool_id) = self.with_frame(|c| {
            // A frame starts with the callee or receiver in slot 0 and one slot per parameter.
            let body_height = c.bindings.frame_stack_height_at(&decl.body) as usize;
            c.frame_stack_height = Some(1 + decl.params.len());
            c.record_param_slot_witness_sets(&decl.params)?;
            c.record_receiver_slot_witness_set(decl)?;
            c.share_params_that_outlive(node_id, decl, kind);
            c.emit_anchor_copy_ins(&decl.body, &decl.params)?;
            c.compile_pattern_param_checks(&decl.params)?;
            c.open_frame_temp_slots(&decl.body);
            c.expect_frame_stack_height(body_height, "function body", &decl.body);
            c.expression(&decl.body)?;
            c.exit_function(&decl.body, kind);
            Ok(())
        })?;
        self.bind_label(skip);

        self.compiled_fns.pop();

        let name = self.gc.intern(self.hir.text(decl.name));
        let arity = decl.params.len() as u8;
        let captures = self.bindings.captures(&decl.body).iter()
            .map(|u| match u.is_local {
                true => CaptureLocation { location: self.real_slot(u.location), is_local: true },
                false => *u,
            })
            .collect();

        let param_list_pool_id = self.param_list_pool_id(callable, decl, kind)?;
        let mut fn_obj = ObjFn::new(name, arity, 0, captures, param_list_pool_id, slot_witness_set_pool_id);
        if self.barriers.returns_value(callable) {
            fn_obj.header.set(FLAG_RETURNS_VALUE);
        }
        let func = self.gc.alloc(fn_obj);
        self.ir.record_fn_entry(func, body);

        self.ir.add_constant(Value::from(func))
    }

    fn exit_function(&mut self, body_id: &HirId<HirExpr>, kind: FnKind) {
        debug_assert!(matches!(self.hir.get(body_id), HirExpr::Block(_)),
            "lowering wraps every function body in a block");
        let ends_on_return = matches!(self.ir.code().last(), Some(Inst::Return | Inst::ReturnShared | Inst::ReturnFac));
        if ends_on_return && self.hir.definitely_returns(body_id) {
            return;
        }
        if let FnKind::Factory = kind {
            self.emit_factory_return(body_id);
        } else {
            self.emit(Inst::PushNull, body_id);
            self.emit_return(Returning::Nothing, body_id);
        }
    }
}

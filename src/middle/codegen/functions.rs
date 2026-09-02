use crate::middle::signatures::CallableId;
use crate::core::objects::{ObjFn, UpvalueLocation};
use crate::core::value::Value;
use crate::middle::hir::{HirExpr, HirFnDecl, HirId, HirParam};
use crate::middle::obligations::Obligations;
use crate::middle::ir::{Inst, Label};
use crate::middle::bind::FnKind;

use super::Compiler;

impl<'a> Compiler<'a> {
    fn compile_pattern_param_checks(&mut self, params: &[HirParam]) -> Result<(), anyhow::Error> {
        for param in params {
            let Some(pattern) = &param.pattern else { continue };
            let binders = self.bindings.match_binders(&param.name).unwrap_or_default();
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

    /// Throws where a pattern rejected the value on the stack. Whatever the check pass proved, the
    /// runtime is what refuses a value the pattern does not take.
    pub(super) fn abort_on_pattern_mismatch<T: 'static>(&mut self, message: String, node: &HirId<T>) -> Result<(), anyhow::Error> {
        let matched_label = self.emit_pattern_mismatch_jumps(node);
        let message = self.gc.intern(message);
        let idx = self.ir.add_constant(Value::from(message))?;
        self.emit(Inst::PushConstant(idx), node);
        self.emit(Inst::Throw, node);
        self.ir.bind(matched_label);
        Ok(())
    }

    pub(super) fn emit_pattern_mismatch_jumps<T: 'static>(&mut self, node: &HirId<T>) -> Label {
        let matched = self.ir.new_label();
        let failed = self.ir.new_label();
        self.emit(Inst::JumpIfFalse(failed), node);
        self.emit(Inst::Jump(matched), node);
        self.ir.bind(failed);
        matched
    }

    fn param_accepts(&mut self, callable: CallableId) -> Result<u16, anyhow::Error> {
        let clauses: &[Obligations] = self.sigs.fn_sig_of(callable).map_or(&[], |s| &s.param_clauses);
        let mut out = Vec::with_capacity(clauses.len());
        for owed in clauses.to_vec() {
            out.push(self.accepts_index(&owed, false)?);
        }
        self.ir.add_param_accepts(out.into_boxed_slice())
    }

    pub (super) fn function<T: 'static>(&mut self, node_id: &HirId<T>, callable: CallableId, decl: &HirFnDecl, kind: FnKind) -> Result<u8, anyhow::Error> {
        self.fn_kinds.push(kind);

        // Add a jump over the function's body after declaration.
        // The body should only be reachable via calls to the function.
        let skip = self.ir.new_label();
        self.emit(Inst::Jump(skip), node_id);

        let body = self.ir.new_label();
        self.ir.bind(body);

        let (_, slot_accepts) = self.with_frame(|c| {
            c.compile_pattern_param_checks(&decl.params)?;
            c.open_defer_frame(&decl.body);
            c.frame_slot_count = c.bindings.frame_slot_count_at(&decl.body) as usize;
            c.expression(&decl.body)?;
            c.exit_function(&decl.body, kind);
            Ok(())
        })?;
        self.ir.bind(skip);

        self.fn_kinds.pop();

        let name = self.gc.intern(self.hir.text(decl.name));
        let arity = decl.params.len() as u8;
        let upvalues = self.bindings.upvalues(&decl.body).iter()
            .map(|u| match u.is_local {
                true => UpvalueLocation { location: self.real_slot(u.location), is_local: true },
                false => *u,
            })
            .collect();

        let param_accepts = self.param_accepts(callable)?;
        let func = self.gc.alloc(ObjFn::new(name, arity, 0, upvalues, param_accepts, slot_accepts));
        self.ir.record_fn_entry(func, body);

        self.ir.add_constant(Value::from(func))
    }

    fn exit_function(&mut self, body_id: &HirId<HirExpr>, kind: FnKind) {
        if matches!(self.hir.get(body_id), HirExpr::Block(_)) {
            if !matches!(self.ir.code().last(), Some(Inst::Return | Inst::ReturnFac)) {
                if let FnKind::Factory = kind {
                    self.emit(Inst::LoadLocal(0), body_id);
                    self.emit(Inst::ReturnFac, body_id);
                } else {
                    self.emit(Inst::PushNull, body_id);
                    self.emit(Inst::Return, body_id);
                }
            }
        } else {
            self.emit(Inst::Return, body_id);
        }
    }
}

use crate::core::objects::{ObjFn, UpvalueLocation};
use crate::core::value::Value;
use crate::middle::hir::{HirExpr, HirFnDecl, HirId, HirParam, HirStmt};
use crate::middle::ir::Inst;
use crate::middle::bind::FnKind;

use super::Compiler;

/// A bitmask with one bit set per parameter the predicate holds for, capped at 64.
fn param_bits(flags: impl IntoIterator<Item = bool>) -> u64 {
    flags.into_iter().take(64).enumerate()
        .filter(|(_, set)| *set)
        .fold(0u64, |mask, (i, _)| mask | (1u64 << i))
}

/// The parameters a declaration takes by `*`. Parameters past 63 are read as borrowing.
fn declared_retains(decl: &HirFnDecl) -> u64 {
    param_bits(decl.params.iter().map(|p| p.clause.capability.is_retain()))
}

/// What a callable does with each of its arguments.
#[derive(Clone, Copy)]
pub(super) struct ParamMasks {
    /// The arguments the call hands write-ownership of, rather than lending for its duration.
    pub retains: u64,
    /// The arguments the body lets out of the caller's reach.
    pub escapes: u64,
}

impl<'a> Compiler<'a> {
    /// What a callable does with each argument.
    pub(super) fn declared_masks(&self, stmt: &HirId<HirStmt>, decl: &HirFnDecl) -> ParamMasks {
        let escapes = param_bits((0..decl.params.len()).map(|i| self.sigs.param_escapes_at(stmt, i)));
        ParamMasks { retains: declared_retains(decl), escapes }
    }

    pub(super) fn lambda_masks(&self, expr: &HirId<HirExpr>, decl: &HirFnDecl) -> ParamMasks {
        let arity = decl.params.len();
        let escapes = self.sigs.lambda_param_escapes.get(expr)
            .map(|e| param_bits(e.iter().copied()))
            .unwrap_or_else(|| if arity >= 64 { u64::MAX } else { (1u64 << arity) - 1 });
        ParamMasks { retains: declared_retains(decl) | escapes, escapes }
    }

    /// Matches each pattern parameter against its slot on entry, publishing the pattern's binders
    /// into slots reserved ahead of the body. A pattern that can fail throws on a non-match.
    fn compile_entry_steps(&mut self, params: &[HirParam]) -> Result<(), anyhow::Error> {
        for param in params {
            let Some(pattern) = &param.pattern else { continue };
            let binders = self.bindings.match_binders(&param.name).unwrap_or_default().to_vec();
            self.reserve_slots(binders.len(), &param.name);
            self.expression(&param.name)?;
            self.compile_binding_matcher(pattern, &binders, &param.name)?;
            match self.hir.get(pattern).is_irrefutable(self.hir) {
                true => self.emit(Inst::Pop, &param.name),
                false => self.abort_on_entry_mismatch(param)?,
            }
        }
        Ok(())
    }

    /// Throws when an entry pattern rejects its argument. The pattern is a precondition the caller
    /// has to meet, so the blame belongs to the argument. A `try` around the call catches it, since
    /// it throws rather than ending the run with a diagnostic.
    fn abort_on_entry_mismatch(&mut self, param: &HirParam) -> Result<(), anyhow::Error> {
        let matched = self.ir.new_label();
        let failed = self.ir.new_label();
        self.emit(Inst::JumpIfFalse(failed), &param.name);
        self.emit(Inst::Jump(matched), &param.name);

        self.ir.bind(failed);
        // The parameter's own source spans the binder and the test, so quoting it names both.
        let message = self.gc.intern(format!("argument does not match `{}`", param.pos.snippet()));
        let idx = self.ir.add_constant(Value::from(message))?;
        self.emit(Inst::PushConstant(idx), &param.name);
        self.emit(Inst::Throw, &param.name);
        self.ir.bind(matched);
        Ok(())
    }

    pub (super) fn function<T: 'static>(&mut self, node_id: &HirId<T>, decl: &HirFnDecl, kind: FnKind, masks: ParamMasks) -> Result<u8, anyhow::Error> {
        self.fn_kinds.push(kind);

        // Add a jump over the function's body after declaration.
        // The body should only be reachable via calls to the function.
        let skip = self.ir.new_label();
        self.emit(Inst::Jump(skip), node_id);

        let body = self.ir.new_label();
        self.ir.bind(body);

        let caller_depth = self.depth;
        // A binder names a slot of the frame that made it, so a nested body starts with none.
        let caller_binders = std::mem::take(&mut self.handle_binder_slots);
        self.compile_entry_steps(&decl.params)?;
        self.depth = self.bindings.depth_at(&decl.body) as usize;
        self.expression(&decl.body)?;
        self.exit_function(&decl.body, kind);
        self.depth = caller_depth;
        self.handle_binder_slots = caller_binders;
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

        let escape_mask = masks.retains | masks.escapes;

        // A method declaring `mut this` needs the call to prove its receiver is mutable.
        let mut_receiver = decl.receiver.as_ref().is_some_and(|r| r.capability.is_mut());
        let retain_receiver = decl.receiver.as_ref().is_some_and(|r| r.capability.is_retain());

        let func = self.gc.alloc(ObjFn::new(name, arity, 0, upvalues, escape_mask, masks.retains, mut_receiver, retain_receiver));
        self.ir.record_fn_entry(func, body);

        self.ir.add_constant(Value::from(func))
    }

    fn exit_function(&mut self, body_id: &HirId<HirExpr>, kind: FnKind) {
        if matches!(self.hir.get(body_id), HirExpr::Block(_)) {
            if !matches!(self.ir.code().last(), Some(Inst::Return | Inst::ReturnFac)) {
                // A factory hands back `this` via `RETURN_FAC`, which seals it per the frame bit.
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

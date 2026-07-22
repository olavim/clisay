use crate::core::objects::ObjFn;
use crate::core::value::Value;
use crate::middle::hir::{HirExpr, HirFnDecl, HirId, HirStmt};
use crate::middle::ir::Inst;
use crate::middle::bind::FnKind;

use super::Compiler;

/// A bitmask with one bit set per parameter the predicate holds for, capped at 64.
fn param_bits(flags: impl IntoIterator<Item = bool>) -> u64 {
    flags.into_iter().take(64).enumerate()
        .filter(|(_, set)| *set)
        .fold(0u64, |mask, (i, _)| mask | (1u64 << i))
}

impl<'a> Compiler<'a> {
    /// The persist mask of a named function or method, read from the escape summary.
    pub(super) fn persist_mask(&self, stmt: &HirId<HirStmt>) -> u64 {
        self.sigs.param_escapes.get(stmt).map(|e| param_bits(e.iter().copied())).unwrap_or(0)
    }

    /// The persist mask of a lambda, read from its per-lambda escape summary. An unanalyzed lambda
    /// conservatively marks every parameter as escaping so the barrier rejects a borrow into it.
    pub(super) fn lambda_persist_mask(&self, expr: &HirId<HirExpr>, arity: usize) -> u64 {
        self.sigs.lambda_param_escapes.get(expr)
            .map(|e| param_bits(e.iter().copied()))
            .unwrap_or_else(|| if arity >= 64 { u64::MAX } else { (1u64 << arity) - 1 })
    }

    pub (super) fn function<T: 'static>(&mut self, node_id: &HirId<T>, decl: &HirFnDecl, kind: FnKind, persist_mask: u64) -> Result<u8, anyhow::Error> {
        self.fn_kinds.push(kind);

        // Add a jump over the function's body after declaration.
        // The body should only be reachable via calls to the function.
        let skip = self.ir.new_label();
        self.emit(Inst::Jump(skip), node_id);

        let body = self.ir.new_label();
        self.ir.bind(body);

        self.expression(&decl.body)?;
        self.exit_function(&decl.body, kind);
        self.ir.bind(skip);

        self.fn_kinds.pop();

        let name = self.gc.intern(self.hir.text(decl.name));
        let arity = decl.params.len() as u8;
        let upvalues = self.bindings.upvalues(&decl.body).to_vec();

        // A parameter lets its argument escape if it takes it by `*mut` or persists it.
        // Parameters past 63 are read as borrowing.
        let move_mask = param_bits(decl.params.iter().map(|p| p.clause.capability.is_move()));
        let escape_mask = move_mask | persist_mask;

        let func = self.gc.alloc(ObjFn::new(name, arity, 0, upvalues, escape_mask));
        self.ir.record_fn_entry(func, body);

        self.ir.add_constant(Value::from(func))
    }

    fn exit_function(&mut self, body_id: &HirId<HirExpr>, kind: FnKind) {
        if matches!(self.hir.get(body_id), HirExpr::Block(_)) {
            if !matches!(self.ir.code().last(), Some(Inst::Return)) {
                if let FnKind::Initializer = kind {
                    self.emit(Inst::LoadLocal(0), body_id);
                } else {
                    self.emit(Inst::PushNull, body_id);
                }
                self.emit(Inst::Return, body_id);
            }
        } else {
            self.emit(Inst::Return, body_id);
        }
    }
}

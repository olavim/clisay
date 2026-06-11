use crate::core::objects::ObjFn;
use crate::core::value::Value;
use crate::middle::hir::{HirExpr, HirFnDecl, HirId};
use crate::middle::ir::Inst;
use crate::middle::resolve::FnKind;

use super::Compiler;

impl<'a> Compiler<'a> {
    pub (super) fn function<T: 'static>(&mut self, node_id: &HirId<T>, decl: &HirFnDecl, kind: FnKind) -> Result<u8, anyhow::Error> {
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
        let func = self.gc.alloc(ObjFn::new(name, arity, 0, upvalues));
        self.ir.record_entry(func, body);

        self.ir.add_constant(Value::from(func))
    }

    fn exit_function(&mut self, body_id: &HirId<HirExpr>, kind: FnKind) {
        if matches!(self.hir.get(body_id), HirExpr::Block(_)) {
            if !matches!(self.ir.code().last(), Some(Inst::Return)) {
                if let FnKind::Initializer = kind {
                    self.emit(Inst::GetLocal(0), body_id);
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

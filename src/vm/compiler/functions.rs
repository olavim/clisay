use crate::parser::{ASTId, FnDecl, Stmt};
use crate::vm::objects::ObjFn;
use crate::vm::opcode;
use crate::vm::value::Value;

use super::{Compiler, FnFrame, FnKind};

impl<'a> Compiler<'a> {
    pub (super) fn enter_function(&mut self, kind: FnKind) {
        self.fn_frames.push(FnFrame {
            upvalues: Vec::new(),
            local_offset: if self.locals.is_empty() { 0 } else { self.locals.len() as u8 - 1 },
            class_frame: self.class_frames.last().map(|_| self.class_frames.len() as u8 - 1),
            kind
        });

        self.scope_depth += 1;
    }

    pub (super) fn exit_function<T: 'static>(&mut self, node_id: &ASTId<T>) -> FnFrame {
        let frame = self.fn_frames.pop().unwrap();

        // Make sure there is a return statement at the end of the function
        if self.chunk.code[self.chunk.code.len() - 1] != opcode::RETURN {
            if let FnKind::Initializer = frame.kind {
                self.emit(opcode::GET_LOCAL, node_id);
                self.emit(0, node_id);
            } else {
                self.emit(opcode::PUSH_NULL, node_id);
            }
            self.emit(opcode::RETURN, node_id);
        }

        self.scope_depth -= 1;

        while !self.locals.is_empty() && self.locals.last().unwrap().depth > self.scope_depth {
            self.locals.pop();
        }

        frame
    }

    pub (super) fn function(&mut self, stmt: &ASTId<Stmt>, decl: &Box<FnDecl>, kind: FnKind) -> Result<u8, anyhow::Error> {
        self.enter_function(kind);

        let jump_ref = self.emit_jump(opcode::JUMP, 0, stmt);
        let ip_start = self.chunk.code.len();
        let arity = decl.params.len() as u8;

        for param in &decl.params {
            let param = self.gc.intern(param);
            self.declare_local(param, true)?;
        }

        self.expression(&decl.body)?;
        let frame = self.exit_function(&decl.body);
        self.patch_jump(jump_ref)?;

        let name = self.gc.intern(&decl.name);
        let func = self.gc.alloc(ObjFn::new(name, arity, ip_start, frame.upvalues));

        self.chunk.add_constant(Value::from(func))
    }
}
use crate::compiler_error;
use crate::parser::{ASTId, Expr, FnDecl};
use crate::vm::objects::{ObjFn, ObjString};
use crate::vm::opcode;
use crate::vm::value::Value;

use super::{Compiler, FnFrame, FnKind, Local};

impl<'a> Compiler<'a> {
    pub (super) fn enter_function(&mut self, kind: FnKind, self_name: *mut ObjString) {
        self.scope_depth += 1;

        // Reserve relative slot 0 of the new frame for the callee — at runtime
        // `stack_start` points at the closure being called (or the instance, for a
        // method), so the frame's own locals (params, body) must begin at slot 1.
        // Any enclosing-scope locals (including hoisted sibling fn/class names) sit at
        // lower indices than this `local_offset`, so they resolve as upvalues rather
        // than polluting this frame's local range. Naming the callee after the
        // function lets it refer to itself by name (recursion) as GET_LOCAL 0.
        let local_offset = self.locals.len() as u8;
        self.locals.push(Local {
            name: self_name,
            depth: self.scope_depth,
            is_mutable: false,
            is_captured: false
        });

        self.fn_frames.push(FnFrame {
            upvalues: Vec::new(),
            local_offset,
            class_frame: self.class_frames.last().map(|_| self.class_frames.len() as u8 - 1),
            kind
        });
    }

    pub (super) fn exit_function(&mut self, body_id: &ASTId<Expr>) -> FnFrame {
        let frame = self.fn_frames.pop().unwrap();
        self.scope_depth -= 1;

        while !self.locals.is_empty() && self.locals.last().unwrap().depth > self.scope_depth {
            self.locals.pop();
        }

        let has_last_expr = match self.ast.get(body_id) {
            Expr::Block(_, last_expr) => last_expr.is_some(),
            _ => true
        };

        // Make sure there is a return statement at the end of the function
        if has_last_expr {
            self.emit(opcode::RETURN, body_id);
        } else if self.chunk.code[self.chunk.code.len() - 1] != opcode::RETURN {
            if let FnKind::Initializer = frame.kind {
                self.emit(opcode::GET_LOCAL, body_id);
                self.emit(0, body_id);
            } else {
                self.emit(opcode::PUSH_NULL, body_id);
            }
            self.emit(opcode::RETURN, body_id);
        }

        frame
    }

    pub (super) fn function<T: 'static>(&mut self, node_id: &ASTId<T>, decl: &FnDecl, kind: FnKind) -> Result<u8, anyhow::Error> {
        // The callee slot is named after the function so recursion resolves to it; for
        // methods/initializers the callee is the instance (reached via `this`), so use
        // the `@init` preset as a sentinel that no user identifier can match.
        let self_name = match kind {
            FnKind::Function => self.gc.intern(&decl.name),
            _ => self.gc.preset_identifiers.init
        };
        self.enter_function(kind, self_name);

        let jump_ref = self.emit_jump(opcode::JUMP, 0, node_id);
        let ip_start = self.chunk.code.len();
        let arity = decl.params.len() as u8;

        for param in &decl.params {
            let Expr::Identifier(param_name) = self.ast.get(param) else {
                compiler_error!(self, node_id, "Invalid parameter: Expected identifier");
            };
            let param_name = self.gc.intern(param_name);
            self.declare_local(param_name, true, param)?;
        }

        self.expression(&decl.body)?;
        let frame = self.exit_function(&decl.body);
        self.patch_jump(jump_ref)?;

        let name = self.gc.intern(&decl.name);
        let func = self.gc.alloc(ObjFn::new(name, arity, ip_start, frame.upvalues));

        self.chunk.add_constant(Value::from(func))
    }
}
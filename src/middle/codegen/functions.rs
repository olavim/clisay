use crate::ast::{AstId, Expr, FnDecl};
use crate::core::objects::{ObjFn, ObjString};
use crate::core::value::Value;
use crate::middle::ir::Inst;

use super::{Compiler, FnFrame, FnKind, Local};

impl<'a> Compiler<'a> {
    pub (super) fn enter_function(&mut self, kind: FnKind, self_name: *mut ObjString) {
        self.scope_depth += 1;

        // Reserve relative slot 0 of the new frame for the callee. At runtime
        // the stack start points at the closure or instance being called, so
        // the frame's own locals (params, body) must begin at slot 1.
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

    pub (super) fn exit_function(&mut self, body_id: &AstId<Expr>) -> FnFrame {
        let frame = self.fn_frames.pop().unwrap();
        self.scope_depth -= 1;

        while !self.locals.is_empty() && self.locals.last().unwrap().depth > self.scope_depth {
            self.locals.pop();
        }

        if matches!(self.ast.get(body_id), Expr::Block(_)) {
            if !matches!(self.ir.code().last(), Some(Inst::Return)) {
                if let FnKind::Initializer = frame.kind {
                    self.emit(Inst::GetLocal(0), body_id);
                } else {
                    self.emit(Inst::PushNull, body_id);
                }
                self.emit(Inst::Return, body_id);
            }
        } else {
            self.emit(Inst::Return, body_id);
        }

        frame
    }

    pub (super) fn function<T: 'static>(&mut self, node_id: &AstId<T>, decl: &FnDecl, kind: FnKind) -> Result<u8, anyhow::Error> {
        // The callee slot is named after the function so recursion resolves to it.
        let self_name = match kind {
            FnKind::Function => self.gc.intern(&decl.name),
            _ => self.gc.preset_identifiers.init
        };
        self.enter_function(kind, self_name);

        // Jump over the inlined body; `body` marks its entry (resolved to a byte
        // offset and stored as the fn's `ip_start` at assembly time).
        let skip = self.ir.new_label();
        self.emit(Inst::Jump(skip), node_id);
        let body = self.ir.new_label();
        self.ir.bind(body);
        let arity = decl.params.len() as u8;

        for param in &decl.params {
            // The parser only ever produces identifier params (see `parse_params`
            // and the lambda arrow handler), so this is infallible.
            let Expr::Identifier(param_name) = self.ast.get(param) else {
                unreachable!("parser guarantees parameters are identifiers");
            };
            let param_name = self.gc.intern(param_name);
            self.declare_local(param_name, true, param)?;
        }

        self.expression(&decl.body)?;
        let frame = self.exit_function(&decl.body);
        self.ir.bind(skip);

        let name = self.gc.intern(&decl.name);
        let func = self.gc.alloc(ObjFn::new(name, arity, 0, frame.upvalues));
        self.ir.record_entry(func, body);

        self.ir.add_constant(Value::from(func))
    }
}
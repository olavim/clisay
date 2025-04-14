use anyhow::bail;

use crate::parser::{ASTId, FieldInit, Stmt};
use crate::vm::opcode;

use super::{Compiler, FnKind};


impl<'a> Compiler<'a> {
    pub (super) fn statement(&mut self, stmt_id: &ASTId<Stmt>) -> Result<(), anyhow::Error> {
        match self.ast.get(stmt_id) {
            Stmt::Return(expr) => {
                if let FnKind::Initializer = self.fn_frames.last().unwrap().kind {
                    if expr.is_some() {
                        bail!("Cannot return a value from a class initializer");
                    }

                    self.emit(opcode::GET_LOCAL, stmt_id);
                    self.emit(0, stmt_id);
                } else if let Some(expr) = expr {
                    self.expression(expr)?;
                } else {
                    self.emit(opcode::PUSH_NULL, stmt_id);
                }

                self.emit(opcode::RETURN, stmt_id);
            },
            Stmt::Fn(decl) => {
                let name = self.gc.intern(&decl.name);
                self.declare_local(name, false)?;
                let const_idx = self.function(stmt_id, decl, FnKind::Function)?;
                self.emit(opcode::PUSH_CLOSURE, stmt_id);
                self.emit(const_idx, stmt_id);
            },
            Stmt::Class(decl) => self.class_declaration(stmt_id, decl)?,
            Stmt::Say(FieldInit { name, value }) => {
                let name = self.gc.intern(name);
                let local = self.declare_local(name, true)?;

                if let Some(expr) = value {
                    self.expression(expr)?;
                    self.emit(opcode::SET_LOCAL, stmt_id);
                } else {
                    self.emit(opcode::GET_LOCAL, stmt_id);
                }
                self.emit(local, stmt_id);
            },
            Stmt::Expression(expr) => {
                self.expression(expr)?;
                self.emit(opcode::POP, stmt_id);
            },
            Stmt::While(cond, body) => {
                let pos = self.chunk.code.len() as u16;

                self.expression(cond)?;
                let jump_ref = self.emit_jump(opcode::JUMP_IF_FALSE, 0, stmt_id);
                self.statement_body(body)?;
                self.emit_jump(opcode::JUMP, pos, stmt_id);
                self.patch_jump(jump_ref)?;
            }
        };

        Ok(())
    }

    fn statement_body(&mut self, body: &Vec<ASTId<Stmt>>) -> Result<(), anyhow::Error> {
        for stmt_id in body {
            self.statement(stmt_id)?;
        }
        Ok(())
    }
}
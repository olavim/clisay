use anyhow::bail;

use crate::parser::{ASTId, Expr, FieldInit, Stmt};
use crate::vm::opcode;

use super::{Compiler, FnKind, TryCatchPosition, TryFrame};


impl<'a> Compiler<'a> {
    pub (super) fn statement(&mut self, stmt_id: &ASTId<Stmt>) -> Result<(), anyhow::Error> {
        match self.ast.get(stmt_id) {
            Stmt::Return(expr) => {
                // If returning from a try or catch block with a finally block, inline the finally block before returning.
                if let Some(TryFrame {
                    position: pos @ (TryCatchPosition::Try | TryCatchPosition::Catch),
                    finally: Some(finally)
                }) = self.try_frames.last().cloned() {
                    if matches!(pos, TryCatchPosition::Try) {
                        self.emit(opcode::POP_TRY, stmt_id);
                    }

                    let Stmt::Finally(finally) = self.ast.get(&finally) else { unreachable!() };
                    self.statement_body(finally)?;
                }

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
            Stmt::Throw(expr) => {
                self.expression(expr)?;
                self.emit(opcode::THROW, stmt_id);
            },
            Stmt::Try(try_body, catch, finally) => {
                self.try_frames.push(TryFrame {
                    position: TryCatchPosition::Try,
                    finally: *finally
                });
                let frame_idx = self.try_frames.len() - 1;

                if try_body.len() > 0 {
                    let node_id = &try_body[0];

                    let push_try_ref = self.emit_jump(opcode::PUSH_TRY, 0, node_id);
                    self.statement_body(try_body)?;
                    self.emit(opcode::POP_TRY, node_id);
                    let try_jump = self.emit_jump(opcode::JUMP, 0, node_id);
                    self.patch_jump(push_try_ref)?;
    
                    if let Some(Stmt::Catch(param, body)) = catch.map(|c| self.ast.get(&c)) {
                        if let Some(param) = param {
                            let Expr::Identifier(name) = self.ast.get(param) else { unreachable!() };
                            let name = self.gc.intern(name);
                            self.declare_local(name, false)?;
                        }
    
                        self.try_frames[frame_idx].position = TryCatchPosition::Catch;
                        self.statement_body(body)?;
                    }
    
                    self.patch_jump(try_jump)?;
                }


                if let Some(Stmt::Finally(body)) = finally.map(|c| self.ast.get(&c)) {
                    if body.len() > 0 {
                        self.try_frames[frame_idx].position = TryCatchPosition::Finally;
                        self.statement_body(body)?;
                        if catch.is_none() {
                            self.emit(opcode::THROW, &finally.unwrap());
                        }
                    }
                }

                self.try_frames.pop();
            },
            Stmt::Catch(_, _) | Stmt::Finally(_) => unreachable!(),
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
use crate::compiler_error;
use crate::ast::{AstId, CatchClause, Expr, FieldInit, Stmt};
use crate::middle::ir::Inst;
use crate::middle::resolve::FnKind;

use super::{Compiler, TryCatchPosition, TryFrame};


impl<'a> Compiler<'a> {
    pub (super) fn statement(&mut self, stmt_id: &AstId<Stmt>) -> Result<(), anyhow::Error> {
        match self.ast.get(stmt_id) {
            Stmt::Return(expr) => {
                // If returning from a try or catch block with a finally block, inline the finally block before returning.
                if let Some(TryFrame {
                    position: pos @ (TryCatchPosition::Try | TryCatchPosition::Catch),
                    finally: Some(finally)
                }) = self.try_frames.last().cloned() {
                    if matches!(pos, TryCatchPosition::Try) {
                        self.emit(Inst::PopTry, stmt_id);
                    }

                    let idx = self.try_frames.len() - 1;
                    self.try_frames[idx].position = TryCatchPosition::Finally;
                    self.inline_block(&finally)?;
                    self.try_frames[idx].position = pos;
                }

                if let FnKind::Initializer = *self.fn_kinds.last().unwrap() {
                    if expr.is_some() {
                        compiler_error!(self, stmt_id, "Cannot return a value from a class initializer");
                    }

                    self.emit(Inst::GetLocal(0), stmt_id);
                } else if let Some(expr) = expr {
                    self.expression(expr)?;
                } else {
                    self.emit(Inst::PushNull, stmt_id);
                }

                self.emit(Inst::Return, stmt_id);
            },
            Stmt::Throw(expr) => {
                if let Some(TryFrame {
                    position: TryCatchPosition::Catch,
                    finally: Some(finally)
                }) = self.try_frames.last().cloned() {
                    self.compile_finally(&finally)?;
                }

                self.expression(expr)?;
                self.emit(Inst::Throw, stmt_id);
            },
            Stmt::Try(try_body, catch, finally) => {
                self.try_frames.push(TryFrame {
                    position: TryCatchPosition::Try,
                    finally: *finally
                });

                let Expr::Block(stmts) = self.ast.get(try_body) else { unreachable!() };
                if !stmts.is_empty() {
                    let node_id = &stmts[0];
                    let handler = self.ir.new_label();
                    self.emit(Inst::PushTry(handler), node_id);

                    self.expression_stmt(try_body)?;

                    self.emit(Inst::PopTry, node_id);
                    let end = self.ir.new_label();
                    self.emit(Inst::Jump(end), node_id);
                    self.ir.bind(handler);

                    if let Some(catch) = catch {
                        self.compile_catch(catch)?;
                    } else {
                        if let Some(finally) = finally {
                            self.compile_finally(finally)?;
                        }
                        self.emit(Inst::Throw, node_id);
                    }

                    self.ir.bind(end);
                }

                if let Some(finally) = finally {
                    self.compile_finally(finally)?;
                }

                self.try_frames.pop();
            },
            Stmt::Fn(decl) => {
                // The slot was reserved by hoisting so forward references resolve.
                let slot = self.bindings.slot(stmt_id);

                let const_idx = self.function(stmt_id, decl, FnKind::Function)?;
                self.emit(Inst::PushClosure(const_idx), stmt_id);

                // Store the closure into the reserved slot and discard the placeholder.
                self.emit(Inst::SetLocal(slot), stmt_id);
                self.emit(Inst::Pop, stmt_id);
            },
            Stmt::Class(decl) => self.class_declaration(stmt_id, decl)?,
            Stmt::Say(FieldInit { value, .. }) => {
                let slot = self.bindings.slot(stmt_id);

                let inst = if let Some(expr) = value {
                    self.expression(expr)?;
                    Inst::SetLocal(slot)
                } else {
                    Inst::GetLocal(slot)
                };
                self.emit(inst, stmt_id);
            },
            Stmt::Expression(expr) => {
                self.expression_stmt(expr)?;
            },
            Stmt::While(cond, body) => {
                let loop_start = self.ir.new_label();
                self.ir.bind(loop_start);

                let exit = self.emit_conditional_jump(cond, stmt_id)?;

                self.expression_stmt(body)?;

                self.emit(Inst::Jump(loop_start), stmt_id);
                self.ir.bind(exit);
            },
            Stmt::If(cond, then, otherwise) => {
                let else_target = self.emit_conditional_jump(cond, stmt_id)?;

                self.expression_stmt(then)?;

                if let Some(otherwise) = otherwise {
                    let end = self.ir.new_label();
                    self.emit(Inst::Jump(end), stmt_id);
                    self.ir.bind(else_target);
                    self.statement(otherwise)?;
                    self.ir.bind(end);
                } else {
                    self.ir.bind(else_target);
                }
            },
            Stmt::Block(body) => {
                self.expression_stmt(body)?;
            }
        };

        Ok(())
    }

    fn statement_body(&mut self, body: &Vec<AstId<Stmt>>) -> Result<(), anyhow::Error> {
        self.hoist_declarations(body)?;
        for stmt_id in body {
            self.statement(stmt_id)?;
        }
        Ok(())
    }

    pub (super) fn scoped_body<T: 'static>(&mut self, body: &Vec<AstId<Stmt>>, node_id: &AstId<T>) -> Result<(), anyhow::Error> {
        self.statement_body(body)?;
        self.exit_scope(node_id);
        Ok(())
    }

    /// Compiles the statements of an `Expr::Block` body directly into the current
    /// scope, without its own cleanup.
    fn inline_block(&mut self, body: &AstId<Expr>) -> Result<(), anyhow::Error> {
        let Expr::Block(stmts) = self.ast.get(body) else { unreachable!() };
        self.statement_body(stmts)
    }

    fn compile_catch(&mut self, catch: &CatchClause) -> Result<(), anyhow::Error> {
        let idx = self.try_frames.len() - 1;
        self.try_frames[idx].position = TryCatchPosition::Catch;

        self.inline_block(&catch.body)?;
        self.exit_scope(&catch.body);
        Ok(())
    }

    fn compile_finally(&mut self, finally: &AstId<Expr>) -> Result<(), anyhow::Error> {
        let idx = self.try_frames.len() - 1;
        self.try_frames[idx].position = TryCatchPosition::Finally;
        self.expression_stmt(finally)
    }

    /// Emit a `PUSH_NULL` placeholder for every `fn`/`class` declared directly in
    /// `body`, holding its (resolver-assigned) slot until the declaration is
    /// compiled into it - which is what lets forward references resolve.
    fn hoist_declarations(&mut self, body: &Vec<AstId<Stmt>>) -> Result<(), anyhow::Error> {
        for stmt_id in body {
            if matches!(self.ast.get(stmt_id), Stmt::Fn(_) | Stmt::Class(_)) {
                self.emit(Inst::PushNull, stmt_id);
            }
        }
        Ok(())
    }
}

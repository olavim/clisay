use crate::compiler_error;
use crate::parser::{ASTId, CatchClause, Expr, FieldInit, Stmt};
use crate::runtime::opcode;

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

                    // Mark the frame as inside its finally so a `return` within the
                    // inlined finally doesn't recurse back here, then restore the
                    // position so sibling returns in the same body still inline it.
                    let idx = self.try_frames.len() - 1;
                    self.try_frames[idx].position = TryCatchPosition::Finally;
                    self.inline_block(&finally)?;
                    self.try_frames[idx].position = pos;
                }

                if let FnKind::Initializer = self.fn_frames.last().unwrap().kind {
                    if expr.is_some() {
                        compiler_error!(self, stmt_id, "Cannot return a value from a class initializer");
                    }

                    self.emit_operand(opcode::GET_LOCAL, 0, stmt_id);
                } else if let Some(expr) = expr {
                    self.expression(expr)?;
                } else {
                    self.emit(opcode::PUSH_NULL, stmt_id);
                }

                self.emit(opcode::RETURN, stmt_id);
            },
            Stmt::Throw(expr) => {
                if let Some(TryFrame {
                    position: TryCatchPosition::Catch,
                    finally: Some(finally)
                }) = self.try_frames.last().cloned() {
                    self.compile_finally(&finally)?;
                }

                self.expression(expr)?;
                self.emit(opcode::THROW, stmt_id);
            },
            Stmt::Try(try_body, catch, finally) => {
                self.try_frames.push(TryFrame {
                    position: TryCatchPosition::Try,
                    finally: *finally
                });

                let Expr::Block(stmts) = self.ast.get(try_body) else { unreachable!() };
                if !stmts.is_empty() {
                    // Attribute the try scaffolding (and an uncaught re-throw) to the
                    // first body statement, matching where the protected code lives.
                    let node_id = &stmts[0];
                    let push_try_ref = self.emit_jump(opcode::PUSH_TRY, 0, node_id);

                    self.expression_stmt(try_body)?;

                    self.emit(opcode::POP_TRY, node_id);
                    let try_jump = self.emit_jump(opcode::JUMP, 0, node_id);
                    self.patch_jump(push_try_ref)?;

                    if let Some(catch) = catch {
                        self.compile_catch(catch)?;
                    } else {
                        if let Some(finally) = finally {
                            self.compile_finally(finally)?;
                        }
                        self.emit(opcode::THROW, node_id);
                    }

                    self.patch_jump(try_jump)?;
                }

                if let Some(finally) = finally {
                    self.compile_finally(finally)?;
                }

                self.try_frames.pop();
            },
            Stmt::Fn(decl) => {
                let name = self.gc.intern(&decl.name);
                
                // The slot was reserved by `hoist_declarations` before any body in this
                // scope was compiled, which is what lets forward references resolve.
                let slot = self.resolve_local(name)
                    .expect("fn declarations are reserved by hoist_declarations before compilation");

                let const_idx = self.function(stmt_id, decl, FnKind::Function)?;
                self.emit_operand(opcode::PUSH_CLOSURE, const_idx, stmt_id);

                // Store the closure into the reserved slot and discard the placeholder.
                self.emit_operand(opcode::SET_LOCAL, slot, stmt_id);
                self.emit(opcode::POP, stmt_id);
            },
            Stmt::Class(decl) => self.class_declaration(stmt_id, decl)?,
            Stmt::Say(FieldInit { name, value }) => {
                let name = self.gc.intern(name);
                let slot = self.declare_local(name, true, stmt_id)?;

                let op = if let Some(expr) = value {
                    self.expression(expr)?;
                    opcode::SET_LOCAL
                } else {
                    opcode::GET_LOCAL
                };
                self.emit_operand(op, slot, stmt_id);
            },
            Stmt::Expression(expr) => {
                self.expression_stmt(expr)?;
            },
            Stmt::While(cond, body) => {
                let pos = self.chunk.code.len() as u16;

                let jump_ref = self.emit_conditional_jump(cond, stmt_id)?;

                self.expression_stmt(body)?;

                self.emit_jump(opcode::JUMP, pos, stmt_id);
                self.patch_jump(jump_ref)?;
            },
            Stmt::If(cond, then, otherwise) => {
                let jump_ref = self.emit_conditional_jump(cond, stmt_id)?;

                self.expression_stmt(then)?;

                if let Some(otherwise) = otherwise {
                    let else_jump_ref = self.emit_jump(opcode::JUMP, 0, stmt_id);
                    self.patch_jump(jump_ref)?;
                    self.statement(otherwise)?;
                    self.patch_jump(else_jump_ref)?;
                } else {
                    self.patch_jump(jump_ref)?;
                }
            },
            Stmt::Block(body) => {
                self.expression_stmt(body)?;
            }
        };

        Ok(())
    }

    fn statement_body(&mut self, body: &Vec<ASTId<Stmt>>) -> Result<(), anyhow::Error> {
        self.hoist_declarations(body)?;
        for stmt_id in body {
            self.statement(stmt_id)?;
        }
        Ok(())
    }

    /// Compiles `body` in its own lexical scope: enter a scope, compile the
    /// statements (with declaration hoisting), then exit and clean up locals.
    pub (super) fn scoped_body<T: 'static>(&mut self, body: &Vec<ASTId<Stmt>>, node_id: &ASTId<T>) -> Result<(), anyhow::Error> {
        self.enter_scope();
        self.statement_body(body)?;
        self.exit_scope(node_id);
        Ok(())
    }

    /// Compiles the statements of an `Expr::Block` body directly into the current
    /// scope, without opening a new one.
    fn inline_block(&mut self, body: &ASTId<Expr>) -> Result<(), anyhow::Error> {
        let Expr::Block(stmts) = self.ast.get(body) else { unreachable!() };
        self.statement_body(stmts)
    }

    /// Compiles a `catch` clause: marks the enclosing try frame as being in its
    /// catch, then binds the parameter and body in a single shared scope.
    fn compile_catch(&mut self, catch: &CatchClause) -> Result<(), anyhow::Error> {
        let idx = self.try_frames.len() - 1;
        self.try_frames[idx].position = TryCatchPosition::Catch;

        self.enter_scope();
        if let Some(param) = &catch.param {
            let Expr::Identifier(name) = self.ast.get(param) else { unreachable!() };
            let name = self.gc.intern(name);
            self.declare_local(name, false, param)?;
        }
        self.inline_block(&catch.body)?;
        self.exit_scope(&catch.body);
        Ok(())
    }

    /// Compiles a `finally` block (scoped, value discarded), marking the enclosing
    /// try frame as being inside its finally so a nested return/throw won't re-run it.
    fn compile_finally(&mut self, finally: &ASTId<Expr>) -> Result<(), anyhow::Error> {
        let idx = self.try_frames.len() - 1;
        self.try_frames[idx].position = TryCatchPosition::Finally;
        self.expression_stmt(finally)
    }

    /// Reserve a local slot for every `fn`/`class` declared directly in `body`
    /// before any statement is compiled, so a declaration can be referenced before
    /// it appears in source (e.g. mutual recursion). A `PUSH_NULL` placeholder holds
    /// the slot until the declaration is compiled into it.
    pub (super) fn hoist_declarations(&mut self, body: &Vec<ASTId<Stmt>>) -> Result<(), anyhow::Error> {
        for stmt_id in body {
            let name = match self.ast.get(stmt_id) {
                Stmt::Fn(decl) => decl.name.clone(),
                Stmt::Class(decl) => decl.name.clone(),
                _ => continue
            };
            let name = self.gc.intern(&name);
            self.declare_local(name, false, stmt_id)?;
            self.emit(opcode::PUSH_NULL, stmt_id);
        }
        Ok(())
    }
}
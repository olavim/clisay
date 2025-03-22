use std::ops::Range;

use anyhow::bail;

use super::{gc::GcRef, operator::Operator, parser::{ASTId, Expr, Literal, Stmt, AST}, BytecodeChunk, FnUpvalue, Function, Gc, OpCode, Value};

struct Local {
    name: GcRef<String>,
    depth: u8,
    is_captured: bool
}

struct FnFrame {
    upvalues: Vec<FnUpvalue>,
    local_offset: u8
}

pub struct Compiler<'a> {
    chunk: &'a mut BytecodeChunk,
    ast: &'a AST,
    gc: &'a mut Gc,
    locals: Vec<Local>,
    scope_depth: u8,
    fn_frames: Vec<FnFrame>
}

impl<'a> Compiler<'a> {
    pub fn compile<'b>(ast: &'b AST, gc: &'b mut Gc) -> Result<BytecodeChunk, anyhow::Error> {
        let mut chunk = BytecodeChunk::new();

        let mut compiler = Compiler {
            chunk: &mut chunk,
            ast,
            gc,
            locals: Vec::new(),
            scope_depth: 0,
            fn_frames: Vec::new()
        };

        let stmt_id = compiler.ast.get_root();
        compiler.statement(&stmt_id)?;
        Ok(chunk)
    }

    fn emit<T: 'static>(&mut self, op: OpCode, node_id: &ASTId<T>) {
        let pos = self.ast.pos(node_id);
        self.chunk.write(op, pos);
    }

    fn emit_jump<T: 'static>(&mut self, op: OpCode, node_id: &ASTId<T>) -> usize {
        self.emit(op, node_id);
        return self.chunk.code.len() - 1;
    }

    fn get_jump(&mut self) -> OpCode {
        let pos = self.chunk.code.len() as u16;
        OpCode::Jump((pos >> 8) as u8, pos as u8)
    }

    fn patch_jump(&mut self, jump_ref: usize) -> Result<(), anyhow::Error> {
        if self.chunk.code.len() > u16::MAX as usize {
            bail!("Jump too large");
        }

        let pos = self.chunk.code.len() as u16;
        let op = self.chunk.code[jump_ref];
        self.chunk.code[jump_ref] = match op {
            OpCode::JumpIfFalse(_, _) => OpCode::JumpIfFalse((pos >> 8) as u8, pos as u8),
            OpCode::Jump(_, _) => OpCode::Jump((pos >> 8) as u8, pos as u8),
            _ => bail!("Invalid jump opcode")
        };
        Ok(())
    }

    fn enter_scope(&mut self) {
        self.scope_depth += 1;
    }

    fn exit_scope<T: 'static>(&mut self, node_id: &ASTId<T>) {
        self.scope_depth -= 1;
        while !self.locals.is_empty() && self.locals.last().unwrap().depth > self.scope_depth {
            if self.locals.last().unwrap().is_captured {
                self.emit(OpCode::CloseUpvalue(self.locals.len() as u8 - 1), node_id);
            } else {
                self.emit(OpCode::Pop, node_id);
            }
            self.locals.pop();
        }
    }

    fn enter_function(&mut self) {
        self.scope_depth += 1;
    }

    fn exit_function<T: 'static>(&mut self, node_id: &ASTId<T>) {
        if self.chunk.code[self.chunk.code.len() - 1] != OpCode::Return {
            self.emit(OpCode::PushNull, node_id);
            self.emit(OpCode::Return, node_id);
        }

        self.scope_depth -= 1;

        while !self.locals.is_empty() && self.locals.last().unwrap().depth > self.scope_depth {
            self.locals.pop();
        }
    }

    fn declare_local(&mut self, name: GcRef<String>) -> Result<u8, anyhow::Error> {
        if self.locals.len() >= u8::MAX as usize {
            bail!("Too many variables in scope");
        }

        if self.locals.iter().rev().any(|local| local.depth == self.scope_depth && local.name == name) {
            bail!("Variable '{}' already declared in this scope", self.gc.get(&name));
        }

        self.locals.push(Local { name, depth: self.scope_depth, is_captured: false });
        Ok((self.locals.len() - 1) as u8)
    }

    fn resolve_local(&self, name: GcRef<String>) -> Option<u8> {
        let local_offset = match self.fn_frames.last() {
            Some(frame) => frame.local_offset,
            None => 0
        };

        return self.resolve_local_in_range(name, local_offset..self.locals.len() as u8);
    }

    fn resolve_local_in_range(&self, name: GcRef<String>, range: Range<u8>) -> Option<u8> {
        for i in range.clone().rev() {
            let local = &self.locals[i as usize];
            if local.name == name {
                return Some((i - range.start) as u8);
            }
        }
        
        None
    }

    fn resolve_upvalue(&mut self, name: GcRef<String>) -> Result<Option<u8>, anyhow::Error> {
        if self.fn_frames.is_empty() {
            return Ok(None);
        }

        self.resolve_frame_upvalue(name, self.fn_frames.len() - 1)
    }

    fn resolve_frame_upvalue(&mut self, name: GcRef<String>, frame_idx: usize) -> Result<Option<u8>, anyhow::Error> {
        let range_start = if frame_idx == 0 { 0 } else { self.fn_frames[frame_idx - 1].local_offset };
        let range_end = self.fn_frames[frame_idx].local_offset;
        
        if let Some(idx) = self.resolve_local_in_range(name, range_start..range_end) {
            self.locals[(range_start + idx) as usize].is_captured = true;
            return Ok(Some(self.add_upvalue(idx, true, frame_idx)?));
        }

        if frame_idx == 0 {
            return Ok(None);
        }

        if let Some(idx) = self.resolve_frame_upvalue(name, frame_idx - 1)? {
            return Ok(Some(self.add_upvalue(idx, false, frame_idx)?));
        }

        Ok(None)
    }

    fn add_upvalue(&mut self, location: u8, is_local: bool, frame_idx: usize) -> Result<u8, anyhow::Error> {
        for i in 0..self.fn_frames[frame_idx].upvalues.len() {
            let upvalue = &self.fn_frames[frame_idx].upvalues[i];
            if upvalue.location == location && upvalue.is_local == is_local {
                return Ok(i as u8);
            }
        }

        if self.fn_frames[frame_idx].upvalues.len() >= u8::MAX as usize {
            bail!("Too many upvalues");
        }

        let upvalue = FnUpvalue { location, is_local };
        self.fn_frames[frame_idx].upvalues.push(upvalue);
        return Ok((self.fn_frames[frame_idx].upvalues.len() - 1) as u8);
    }

    fn statement(&mut self, stmt_id: &ASTId<Stmt>) -> Result<(), anyhow::Error> {
        match self.ast.get(stmt_id) {
            Stmt::Block(stmts) => self.block(stmts)?,
            Stmt::Return(expr) => {
                if let Some(expr) = expr {
                    self.expression(expr)?;
                } else {
                    self.emit(OpCode::PushNull, stmt_id);
                }
                self.emit(OpCode::Return, stmt_id);
            },
            Stmt::Fn(name, params, body) => self.function_declaration(stmt_id, name, params, body)?,
            Stmt::If(cond, then, otherwise) => self.if_stmt(cond, then, otherwise)?,
            Stmt::Say(name, expr) => {
                let local = self.declare_local(*name)?;

                if let Some(expr) = expr {
                    self.expression(expr)?;
                    self.emit(OpCode::SetLocal(local), stmt_id);
                } else {
                    self.emit(OpCode::GetLocal(local), stmt_id);
                }
            },
            Stmt::Expression(expr) => {
                self.expression(expr)?;
                self.emit(OpCode::Pop, stmt_id);
            },
            Stmt::While(cond, body) => {
                let loop_start = self.get_jump();
                self.expression(cond)?;
                let jump_ref = self.emit_jump(OpCode::JumpIfFalse(0, 0), stmt_id);
                self.statement(body)?;
                self.emit(loop_start, stmt_id);
                self.patch_jump(jump_ref)?;
            }
        };

        Ok(())
    }

    fn block(&mut self, stmts: &Vec<ASTId<Stmt>>) -> Result<(), anyhow::Error> {
        self.enter_scope();
        for stmt in stmts {
            self.statement(stmt)?;
        }
        self.exit_scope(stmts.last().unwrap());
        Ok(())
    }

    fn if_stmt(&mut self, cond: &ASTId<Expr>, then: &ASTId<Stmt>, otherwise: &Option<ASTId<Stmt>>) -> Result<(), anyhow::Error> {
        self.expression(cond)?;
        let jump_ref = self.emit_jump(OpCode::JumpIfFalse(0, 0), cond);
        self.statement(then)?;
        let else_jump_ref = self.emit_jump(OpCode::Jump(0, 0), then);
        self.patch_jump(jump_ref)?;
        if let Some(otherwise) = otherwise {
            self.statement(otherwise)?;
        }
        self.patch_jump(else_jump_ref)?;
        Ok(())
    }

    fn function_declaration(&mut self, stmt: &ASTId<Stmt>, name: &GcRef<String>, params: &Vec<GcRef<String>>, body: &ASTId<Stmt>) -> Result<(), anyhow::Error> {
        let local_idx = self.declare_local(*name)?;

        self.fn_frames.push(FnFrame {
            upvalues: Vec::new(),
            local_offset: local_idx
        });

        let jump_ref = self.emit_jump(OpCode::Jump(0, 0), stmt);

        self.enter_function();

        let ip_start = self.chunk.code.len();
        let arity = params.len() as u8;

        for param in params {
            self.declare_local(*param)?;
        }

        self.statement(body)?;
        self.exit_function(body);

        self.patch_jump(jump_ref)?;

        let frame = self.fn_frames.pop().unwrap();
        let func = self.gc.alloc(Function {
            name: self.locals[local_idx as usize].name,
            arity,
            ip_start,
            upvalues: frame.upvalues
        });

        let idx = self.chunk.add_constant(Value::Function(func))?;
        self.emit(OpCode::PushClosure(idx), body);
        
        Ok(())
    }

    fn expression(&mut self, expr: &ASTId<Expr>) -> Result<(), anyhow::Error> {
        match self.ast.get(expr) {
            Expr::Unary(op, expr) => self.unary_expression(op, expr)?,
            Expr::Binary(op, left, right) => self.binary_expression(op, left, right)?,
            Expr::Ternary(_cond, _then, _otherwise) => unimplemented!(),
            Expr::Call(expr, args) => self.call_expression(expr, args)?,
            Expr::Literal(lit) => self.literal(expr, lit)?,
            Expr::Identifier(name) => self.identifier(expr, name, false)?
        };

        Ok(())
    }
    
    fn unary_expression(&mut self, op: &Operator, expr: &ASTId<Expr>) -> Result<(), anyhow::Error> {
        self.expression(expr)?;
        self.emit(OpCode::from_operator(op), expr);
        Ok(())
    }

    fn binary_expression(&mut self, op: &Operator, left: &ASTId<Expr>, right: &ASTId<Expr>) -> Result<(), anyhow::Error> {
        if let Operator::Assign(_) = op {
            let Expr::Identifier(name) = self.ast.get(left) else {
                bail!("Invalid assignment target at {}", self.ast.pos(left));
            };

            self.expression(right)?;
            self.identifier(left, name, true)?;
            return Ok(());
        }

        self.expression(left)?;
        self.expression(right)?;
        self.emit(OpCode::from_operator(op), right);
        Ok(())
    }
    
    fn call_expression(&mut self, expr: &ASTId<Expr>, args: &Vec<ASTId<Expr>>) -> Result<(), anyhow::Error> {
        self.expression(expr)?;
        for arg in args {
            self.expression(arg)?;
        }
        self.emit(OpCode::Call(args.len() as u8), expr);
        Ok(())
    }
    
    fn literal(&mut self, expr: &ASTId<Expr>, literal: &Literal) -> Result<(), anyhow::Error> {
        match literal {
            Literal::Number(num) => {
                let idx = self.chunk.add_constant(Value::Number(*num))?;
                self.emit(OpCode::PushConstant(idx), expr);
            },
            Literal::String(str_ref) => {
                let idx = self.chunk.add_constant(Value::String(*str_ref))?;
                self.emit(OpCode::PushConstant(idx), expr);
            },
            Literal::Null => { self.emit(OpCode::PushNull, expr); },
            Literal::Boolean(true) => { self.emit(OpCode::PushTrue, expr); },
            Literal::Boolean(false) => { self.emit(OpCode::PushFalse, expr); }
        };

        return Ok(());
    }
    
    fn identifier(&mut self, expr: &ASTId<Expr>, name: &GcRef<String>, assign: bool) -> Result<(), anyhow::Error> {
        let (get_op, set_op) = if let Some(local_idx) = self.resolve_local(*name) {
            (OpCode::GetLocal(local_idx), OpCode::SetLocal(local_idx))
        } else if let Some(upvalue_idx) = self.resolve_upvalue(*name)? {
            (OpCode::GetUpvalue(upvalue_idx), OpCode::SetUpvalue(upvalue_idx))
        } else {
            let const_idx = self.chunk.add_constant(Value::String(*name))?;
            (OpCode::GetGlobal(const_idx), OpCode::SetGlobal(const_idx))
        };

        let op = if assign { set_op } else { get_op };
        self.emit(op, expr);
        return Ok(());
    }
}
use std::collections::HashMap;
use std::ops::Range;

use anyhow::anyhow;
use anyhow::bail;

use super::chunk::BytecodeChunk;
use super::gc::Gc;
use super::objects::ObjClass;
use super::objects::ClassMember;
use super::objects::ObjString;
use super::objects::{ObjFn, UpvalueLocation, Value};
use super::operator::Operator;
use super::parser::{ASTId, ClassDecl, Expr, FieldInit, FnDecl, Literal, Stmt, AST};
use super::OpCode;

struct Local {
    name: *mut ObjString,
    depth: u8,
    is_captured: bool
}

struct FnFrame {
    upvalues: Vec<UpvalueLocation>,
    local_offset: u8,
    class_frame: Option<u8>
}

#[derive(Clone, Copy)]
enum FnType {
    None,
    Function,
    Method,
    Initializer
}

struct ClassFrame {
    class: ObjClass,
    superclass: Option<*mut ObjClass>
}

pub struct Compiler<'a> {
    chunk: &'a mut BytecodeChunk,
    ast: &'a AST,
    gc: &'a mut Gc,
    locals: Vec<Local>,
    scope_depth: u8,
    fn_frames: Vec<FnFrame>,
    class_frames: Vec<ClassFrame>,
    fn_type: FnType,
    classes: HashMap<*mut ObjString, u8>
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
            fn_frames: Vec::new(),
            class_frames: Vec::new(),
            fn_type: FnType::None,
            classes: HashMap::default()
        };

        let stmt_id = compiler.ast.get_root();
        compiler.enter_function();
        compiler.statement(&stmt_id)?;
        compiler.exit_function(&stmt_id);
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
        self.fn_frames.push(FnFrame {
            upvalues: Vec::new(),
            local_offset: if self.locals.is_empty() { 0 } else { self.locals.len() as u8 - 1 },
            class_frame: self.class_frames.last().map(|_| self.class_frames.len() as u8 - 1)
        });

        self.scope_depth += 1;
    }

    fn exit_function<T: 'static>(&mut self, node_id: &ASTId<T>) {
        if self.chunk.code[self.chunk.code.len() - 1] != OpCode::Return {
            if let FnType::Initializer = self.fn_type {
                self.emit(OpCode::GetLocal(0), node_id);
            } else {
                self.emit(OpCode::PushNull, node_id);
            }
            self.emit(OpCode::Return, node_id);
        }

        self.scope_depth -= 1;

        while !self.locals.is_empty() && self.locals.last().unwrap().depth > self.scope_depth {
            self.locals.pop();
        }
    }

    #[inline]
    fn current_class(&self) -> &ObjClass {
        &self.class_frames[self.class_frames.len() - 1].class
    }

    #[inline]
    fn current_class_mut(&mut self) -> &mut ObjClass {
        let last = self.class_frames.len() - 1;
        &mut self.class_frames[last].class
    }

    #[inline]
    fn current_superclass(&self) -> Option<*mut ObjClass> {
        let frame = &self.class_frames[self.class_frames.len() - 1];
        frame.superclass
    }

    fn declare_local(&mut self, name: *mut ObjString) -> Result<u8, anyhow::Error> {
        if self.locals.len() >= u8::MAX as usize {
            bail!("Too many variables in scope");
        }

        if self.locals.iter().rev().any(|local| local.depth == self.scope_depth && local.name == name) {
            bail!("Variable '{}' already declared in this scope", unsafe { &(*name).value });
        }

        self.chunk.add_constant(Value::String(name.clone()))?;
        self.locals.push(Local { name, depth: self.scope_depth, is_captured: false });
        Ok((self.locals.len() - 1) as u8)
    }

    fn resolve_local(&self, name: *mut ObjString) -> Option<u8> {
        let local_offset = match self.fn_frames.last() {
            Some(frame) => frame.local_offset,
            None => 0
        };

        return self.resolve_local_in_range(name, local_offset..self.locals.len() as u8);
    }

    fn resolve_local_in_range(&self, name: *mut ObjString, range: Range<u8>) -> Option<u8> {
        for i in range.clone().rev() {
            let local = &self.locals[i as usize];
            if local.name == name {
                return Some((i - range.start) as u8);
            }
        }
        
        None
    }

    fn resolve_upvalue(&mut self, name: *mut ObjString, max_class_frame: Option<u8>) -> Result<Option<u8>, anyhow::Error> {
        if self.fn_frames.is_empty() {
            return Ok(None);
        }

        self.resolve_frame_upvalue(name, self.fn_frames.len() - 1, max_class_frame)
    }

    fn resolve_frame_upvalue(&mut self, name: *mut ObjString, frame_idx: usize, max_class_frame: Option<u8>) -> Result<Option<u8>, anyhow::Error> {
        let class_frame = self.fn_frames[frame_idx].class_frame;

        if max_class_frame.is_some() && class_frame.is_some() && class_frame.unwrap() < max_class_frame.unwrap() {
            return Ok(None);
        }

        if max_class_frame.is_some() && class_frame.is_none() {
            return Ok(None);
        }

        let range_start = if frame_idx == 0 { 0 } else { self.fn_frames[frame_idx - 1].local_offset };
        let range_end = self.fn_frames[frame_idx].local_offset;
        
        if let Some(idx) = self.resolve_local_in_range(name, range_start..range_end) {
            self.locals[(range_start + idx) as usize].is_captured = true;
            return Ok(Some(self.add_upvalue(idx, true, frame_idx)?));
        }

        if frame_idx == 0 {
            return Ok(None);
        }

        if let Some(idx) = self.resolve_frame_upvalue(name, frame_idx - 1, max_class_frame)? {
            return Ok(Some(self.add_upvalue(idx, false, frame_idx)?));
        }

        Ok(None)
    }

    fn resolve_member_class(&self, name: *mut ObjString) -> Option<u8> {
        for i in (0..self.class_frames.len()).rev() {
            let class = &self.class_frames[i].class;
            if class.resolve(name).is_some() {
                return Some(i as u8);
            }
        }

        None
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

        let upvalue = UpvalueLocation { location, is_local };
        self.fn_frames[frame_idx].upvalues.push(upvalue);
        return Ok((self.fn_frames[frame_idx].upvalues.len() - 1) as u8);
    }

    fn statement(&mut self, stmt_id: &ASTId<Stmt>) -> Result<(), anyhow::Error> {
        match self.ast.get(stmt_id) {
            Stmt::Block(stmts) => self.block(stmt_id, stmts)?,
            Stmt::Return(expr) => {
                if let FnType::Initializer = self.fn_type {
                    if expr.is_some() {
                        bail!("Cannot return a value from a class initializer");
                    }

                    self.emit(OpCode::GetLocal(0), stmt_id);
                } else if let Some(expr) = expr {
                    self.expression(expr)?;
                } else {
                    self.emit(OpCode::PushNull, stmt_id);
                }

                self.emit(OpCode::Return, stmt_id);
            },
            Stmt::Fn(decl) => {
                self.declare_local(decl.name)?;
                let prev_fn_type = self.fn_type;
                self.fn_type = FnType::Function;
                let const_idx = self.function(stmt_id, decl)?;
                self.fn_type = prev_fn_type;
                self.emit(OpCode::PushClosure(const_idx), stmt_id);
            },
            Stmt::Class(decl) => self.class_declaration(stmt_id, decl)?,
            Stmt::If(cond, then, otherwise) => self.if_stmt(cond, then, otherwise)?,
            Stmt::Say(FieldInit { name, value }) => {
                let local = self.declare_local(*name)?;

                if let Some(expr) = value {
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

    fn block(&mut self, stmt: &ASTId<Stmt>, stmts: &Vec<ASTId<Stmt>>) -> Result<(), anyhow::Error> {
        self.enter_scope();
        for stmt in stmts {
            self.statement(stmt)?;
        }
        self.exit_scope(stmt);
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

    fn method(&mut self, stmt: &ASTId<Stmt>, decl: &Box<FnDecl>) -> Result<u8, anyhow::Error> {
        let prev_fn_type = self.fn_type;
        self.fn_type = FnType::Method;
        let const_idx = self.function(stmt, decl)?;
        self.fn_type = prev_fn_type;
        Ok(const_idx)
    }

    fn initializer(&mut self, stmt: &ASTId<Stmt>, decl: &Box<FnDecl>) -> Result<u8, anyhow::Error> {
        let prev_fn_type = self.fn_type;
        self.fn_type = FnType::Initializer;
        let const_idx = self.function(stmt, decl)?;
        self.fn_type = prev_fn_type;
        Ok(const_idx)
    }

    fn function(&mut self, stmt: &ASTId<Stmt>, decl: &Box<FnDecl>) -> Result<u8, anyhow::Error> {
        if let FnType::None = self.fn_type {
            bail!("Function declaration outside of function");
        }

        self.enter_function();

        let jump_ref = self.emit_jump(OpCode::Jump(0, 0), stmt);
        let ip_start = self.chunk.code.len();
        let arity = decl.params.len() as u8;

        for param in &decl.params {
            self.declare_local(*param)?;
        }

        self.statement(&decl.body)?;
        self.exit_function(&decl.body);

        self.patch_jump(jump_ref)?;

        let frame = self.fn_frames.pop().unwrap();
        let func = self.gc.alloc(ObjFn::new(decl.name, arity, ip_start, frame.upvalues));

        self.chunk.add_constant(Value::Function(func))
    }

    fn class_declaration(&mut self, stmt: &ASTId<Stmt>, decl: &Box<ClassDecl>) -> Result<(), anyhow::Error> {
        self.declare_local(decl.name)?;
        self.enter_scope();

        let superclass = match decl.superclass {
            Some(name) => {
                let const_idx = self.classes.get(&name)
                    .ok_or(anyhow!("Class '{}' not declared", unsafe { &(*name).value }))?;
                let Value::Class(gc_ref) = self.chunk.constants[*const_idx as usize] else { unreachable!(); };
                Some(gc_ref)
            },
            None => None
        };

        self.class_frames.push(ClassFrame {
            class: ObjClass::new(decl.name, superclass.map(|gc_ref| unsafe { &*gc_ref })),
            superclass
        });

        // Declare fields and methods before compiling init and method bodies
        for &field in &decl.fields {
            self.current_class_mut().declare_field(field);
        }

        for stmt_id in &decl.methods {
            let Stmt::Fn(decl) = self.ast.get(stmt_id) else { unreachable!(); };
            self.current_class_mut().declare_method(decl.name);
        }

        let Stmt::Fn(init_fn_decl) = self.ast.get(&decl.init) else { unreachable!(); };
        let const_idx = self.initializer(stmt, init_fn_decl)?;
        let Value::Function(init) = self.chunk.constants[const_idx as usize] else { unreachable!(); };
        let init_str = self.gc.intern("init");
        self.current_class_mut().declare_method(init_str);
        self.current_class_mut().define_method(init_str, init);

        for stmt_id in &decl.methods {
            let Stmt::Fn(decl) = self.ast.get(stmt_id) else { unreachable!(); };
            let const_idx = self.method(stmt_id, decl)?;
            let Value::Function(gc_ref) = self.chunk.constants[const_idx as usize] else { unreachable!(); };
            self.current_class_mut().define_method(decl.name, gc_ref);
        }

        let class = self.class_frames.pop().unwrap().class;
        self.exit_scope(stmt);

        let idx = self.chunk.add_constant(Value::Class(self.gc.alloc(class)))?;
        self.classes.insert(decl.name, idx);
        self.emit(OpCode::PushClass(idx), stmt);
        
        Ok(())
    }

    fn expression(&mut self, expr: &ASTId<Expr>) -> Result<(), anyhow::Error> {
        match self.ast.get(expr) {
            Expr::Unary(op, expr) => self.unary_expression(op, expr)?,
            Expr::Binary(op, left, right) => self.binary_expression(op, left, right)?,
            Expr::Ternary(_cond, _then, _otherwise) => unimplemented!(),
            Expr::Call(expr, args) => self.call_expression(expr, args)?,
            Expr::Literal(lit) => self.literal(expr, lit)?,
            Expr::Identifier(name) => self.identifier(expr, *name, false)?,
            Expr::This => self.this(expr)?,
            Expr::Super => bail!("Invalid use of 'super'")
        };

        Ok(())
    }

    fn this(&mut self, expr: &ASTId<Expr>) -> Result<(), anyhow::Error> {
        if self.class_frames.is_empty() {
            bail!("Cannot use 'this' outside of a class method");
        }

        self.emit(OpCode::GetLocal(0), expr);
        Ok(())
    }

    fn super_kw(&mut self, expr: &ASTId<Expr>) -> Result<(), anyhow::Error> {
        if self.class_frames.is_empty() {
            bail!("Cannot use 'super' outside of a class method");
        }

        self.emit(OpCode::GetLocal(0), expr);
        Ok(())
    }
    
    fn unary_expression(&mut self, op: &Operator, expr: &ASTId<Expr>) -> Result<(), anyhow::Error> {
        self.expression(expr)?;
        self.emit(OpCode::from_operator(op), expr);
        Ok(())
    }

    fn binary_expression(&mut self, op: &Operator, left: &ASTId<Expr>, right: &ASTId<Expr>) -> Result<(), anyhow::Error> {
        if let Operator::Assign(_) = op {
            match self.ast.get(left) {
                Expr::Identifier(name) => {
                    self.expression(right)?;
                    self.identifier(left, *name, true)?;
                    return Ok(());
                },
                Expr::Binary(Operator::MemberAccess, obj, member) => {
                    self.set_member(obj, member, right)?;
                    return Ok(());
                },
                _ => bail!("Invalid assignment target at {}", self.ast.pos(left))
            }
        }

        if let Operator::MemberAccess = op {
            self.get_member(left, right)?;
            return Ok(());
        }

        self.expression(left)?;
        self.expression(right)?;
        self.emit(OpCode::from_operator(op), right);
        Ok(())
    }

    fn get_member(&mut self, expr: &ASTId<Expr>, member_expr: &ASTId<Expr>) -> Result<(), anyhow::Error> {
        let Expr::Identifier(member) = self.ast.get(member_expr) else {
            bail!("Invalid member access at {}", self.ast.pos(member_expr));
        };

        self.expression(expr)?;

        match self.ast.get(expr) {
            Expr::This => {
                let id = match self.current_class().resolve(*member) {
                    Some(ClassMember::Field(id)) => id,
                    Some(ClassMember::Method(id)) => id,
                    None => bail!("Class '{}' has no member '{}'", unsafe { &(*self.current_class().name).value }, unsafe { &(**member).value })
                };
                self.emit(OpCode::GetPropertyId(id), expr);
                Ok(())
            },
            _ => {
                let const_idx = self.chunk.add_constant(Value::String(*member))?;
                self.emit(OpCode::GetProperty(const_idx), expr);
                Ok(())
            }
        }
    }

    fn set_member(&mut self, expr: &ASTId<Expr>, member_expr: &ASTId<Expr>, value_expr: &ASTId<Expr>) -> Result<(), anyhow::Error> {
        let Expr::Identifier(member) = self.ast.get(member_expr) else {
            bail!("Invalid member access at {}", self.ast.pos(member_expr));
        };

        self.expression(value_expr)?;
        self.expression(expr)?;

        match self.ast.get(expr) {
            Expr::This => {
                let id = match self.current_class().resolve(*member) {
                    Some(ClassMember::Field(id)) => id,
                    Some(ClassMember::Method(id)) => id,
                    None => bail!("Class '{}' has no member '{}'", unsafe { &(*self.current_class().name).value }, unsafe { &(**member).value })
                };
                self.emit(OpCode::SetPropertyId(id), expr);
                Ok(())
            },
            _ => {
                let const_idx = self.chunk.add_constant(Value::String(*member))?;
                self.emit(OpCode::SetProperty(const_idx), expr);
                Ok(())
            }
        }
    }
    
    fn call_expression(&mut self, expr: &ASTId<Expr>, args: &Vec<ASTId<Expr>>) -> Result<(), anyhow::Error> {
        match self.ast.get(expr) {
            Expr::Super => {
                if self.class_frames.is_empty() {
                    bail!("Cannot use 'super' outside of class context");
                }

                let init_str = self.gc.intern("init");
                let member_id = self.current_superclass()
                    .map(|class| unsafe { &*class })
                    .ok_or(anyhow!("Cannot use 'super' outside of child class context"))?
                    .resolve_id(init_str)
                    .unwrap();

                self.emit(OpCode::GetLocal(0), expr);
                self.emit(OpCode::GetPropertyId(member_id), expr);
            },
            _ => { self.expression(expr)? }
        };

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
    
    fn identifier(&mut self, expr: &ASTId<Expr>, name: *mut ObjString, assign: bool) -> Result<(), anyhow::Error> {
        let class_frame_idx = self.resolve_member_class(name);

        let (get_op, set_op) = if let Some(local_idx) = self.resolve_local(name) {
            (OpCode::GetLocal(local_idx), OpCode::SetLocal(local_idx))
        } else if let Some(upvalue_idx) = self.resolve_upvalue(name, class_frame_idx)? {
            (OpCode::GetUpvalue(upvalue_idx), OpCode::SetUpvalue(upvalue_idx))
        } else if class_frame_idx.is_some() {
            self.emit(OpCode::GetLocal(0), expr);
            let const_idx = self.chunk.add_constant(Value::String(name))?;
            (OpCode::GetProperty(const_idx), OpCode::SetProperty(const_idx))
        } else {
            let const_idx = self.chunk.add_constant(Value::String(name))?;
            (OpCode::GetGlobal(const_idx), OpCode::SetGlobal(const_idx))
        };

        let op = if assign { set_op } else { get_op };
        self.emit(op, expr);
        return Ok(());
    }
}
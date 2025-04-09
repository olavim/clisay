use std::ops::Range;

use anyhow::anyhow;
use anyhow::bail;
use fnv::FnvHashMap;

use super::chunk::BytecodeChunk;
use super::gc::Gc;
use super::objects::ObjClass;
use super::objects::ClassMember;
use super::objects::ObjString;
use super::objects::{ObjFn, UpvalueLocation};
use super::opcode;
use super::opcode::OpCode;
use super::operator::Operator;
use super::parser::{ASTId, ClassDecl, Expr, FieldInit, FnDecl, Literal, Stmt, AST};
use super::value::Value;

struct Local {
    name: *mut ObjString,
    depth: u8,
    is_mutable: bool,
    is_captured: bool
}

#[derive(Clone, Copy, PartialEq)]
enum AccessKind {
    Get,
    Set
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
    classes: FnvHashMap<*mut ObjString, u8>
}

macro_rules! compiler_error {
    ($self:ident, $msg:expr, $node:expr) => { return Err($self.error($msg, $node)) };
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
            classes: FnvHashMap::default()
        };

        let stmt_id = compiler.ast.get_root();
        compiler.enter_function();
        compiler.statement(&stmt_id)?;
        compiler.exit_function(&stmt_id);
        Ok(chunk)
    }

    fn error<T: 'static>(&self, msg: impl Into<String>, node_id: &ASTId<T>) -> anyhow::Error {
        let pos = self.ast.pos(node_id);
        anyhow!("{}\n\tat {}", msg.into(), pos)
    }

    fn emit<T: 'static>(&mut self, byte: u8, node_id: &ASTId<T>) {
        let pos = self.ast.pos(node_id);
        self.chunk.write(byte, pos);
    }

    fn emit_jump<T: 'static>(&mut self, op: OpCode, pos: u16, node_id: &ASTId<T>) -> u16 {
        self.emit(op, node_id);
        self.emit(pos as u8, node_id);
        self.emit((pos >> 8) as u8, node_id);
        return self.chunk.code.len() as u16 - 3;
    }

    fn patch_jump(&mut self, jump_ref: u16) -> Result<(), anyhow::Error> {
        if self.chunk.code.len() > u16::MAX as usize {
            bail!("Jump too large");
        }

        let pos = self.chunk.code.len() as u16;
        self.chunk.code[jump_ref as usize + 1] = pos as u8;
        self.chunk.code[jump_ref as usize + 2] = (pos >> 8) as u8;
        Ok(())
    }

    fn enter_scope(&mut self) {
        self.scope_depth += 1;
    }

    fn exit_scope<T: 'static>(&mut self, node_id: &ASTId<T>) {
        self.scope_depth -= 1;
        while !self.locals.is_empty() && self.locals.last().unwrap().depth > self.scope_depth {
            if self.locals.last().unwrap().is_captured {
                self.emit(opcode::CLOSE_UPVALUE, node_id);
                self.emit(self.locals.len() as u8 - 1, node_id);
            } else {
                self.emit(opcode::POP, node_id);
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
        if self.chunk.code[self.chunk.code.len() - 1] != opcode::RETURN {
            if let FnType::Initializer = self.fn_type {
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
    }

    fn declare_local(&mut self, name: *mut ObjString, is_mutable: bool) -> Result<u8, anyhow::Error> {
        if self.locals.len() >= u8::MAX as usize {
            bail!("Too many variables in scope");
        }

        if self.locals.iter().rev().any(|local| local.depth == self.scope_depth && local.name == name) {
            bail!("Variable '{}' already declared in this scope", unsafe { &(*name).value });
        }

        self.chunk.add_constant(Value::from(name))?;
        self.locals.push(Local { name, depth: self.scope_depth, is_mutable, is_captured: false });
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
                self.declare_local(decl.name, false)?;
                let prev_fn_type = self.fn_type;
                self.fn_type = FnType::Function;
                let const_idx = self.function(stmt_id, decl)?;
                self.fn_type = prev_fn_type;
                self.emit(opcode::PUSH_CLOSURE, stmt_id);
                self.emit(const_idx, stmt_id);
            },
            Stmt::Class(decl) => self.class_declaration(stmt_id, decl)?,
            Stmt::If(cond, then, otherwise) => self.if_stmt(cond, then, otherwise)?,
            Stmt::Say(FieldInit { name, value }) => {
                let local = self.declare_local(*name, true)?;

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
                self.statement(body)?;
                self.emit_jump(opcode::JUMP, pos, stmt_id);
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
        let jump_ref = self.emit_jump(opcode::JUMP_IF_FALSE, 0, cond);
        self.statement(then)?;
        let else_jump_ref = self.emit_jump(opcode::JUMP, 0, then);
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

        let jump_ref = self.emit_jump(opcode::JUMP, 0, stmt);
        let ip_start = self.chunk.code.len();
        let arity = decl.params.len() as u8;

        for param in &decl.params {
            self.declare_local(*param, true)?;
        }

        self.statement(&decl.body)?;
        self.exit_function(&decl.body);

        self.patch_jump(jump_ref)?;

        let frame = self.fn_frames.pop().unwrap();
        let func = self.gc.alloc(ObjFn::new(decl.name, arity, ip_start, frame.upvalues));

        self.chunk.add_constant(Value::from(func))
    }

    fn class_declaration(&mut self, stmt: &ASTId<Stmt>, decl: &Box<ClassDecl>) -> Result<(), anyhow::Error> {
        self.declare_local(decl.name, false)?;
        self.enter_scope();

        let superclass = match decl.superclass {
            Some(name) => {
                let const_idx = self.classes.get(&name)
                    .ok_or(anyhow!("Class '{}' not declared", unsafe { &(*name).value }))?;
                let value = self.chunk.constants[*const_idx as usize];
                Some(value.as_object().as_class())
            },
            None => None
        };

        let mut frame = ClassFrame {
            class: ObjClass::new(decl.name, superclass.map(|ptr| unsafe { &*ptr })),
            superclass
        };

        // Declare fields and methods before compiling init and method bodies
        for &field in &decl.fields {
            frame.class.declare_field(field);
        }

        for stmt_id in &decl.methods {
            let Stmt::Fn(decl) = self.ast.get(stmt_id) else { unreachable!(); };
            frame.class.declare_method(decl.name);
        }

        // Push class frame to make the current class visible in method bodies
        self.class_frames.push(frame);

        let Stmt::Fn(init_fn_decl) = self.ast.get(&decl.init) else { unreachable!(); };
        let const_idx = self.initializer(stmt, init_fn_decl)?;
        let init_const = self.chunk.constants[const_idx as usize];
        let init_str = self.gc.intern("init");

        let frame = self.class_frames.last_mut().unwrap();
        frame.class.declare_method(init_str);
        frame.class.define_method(init_str, init_const.as_object().as_function());

        for stmt_id in &decl.methods {
            let Stmt::Fn(decl) = self.ast.get(stmt_id) else { unreachable!(); };
            let const_idx = self.method(stmt_id, decl)?;
            let funct_const = self.chunk.constants[const_idx as usize];
            let frame = self.class_frames.last_mut().unwrap();
            frame.class.define_method(decl.name, funct_const.as_object().as_function());
        }

        let class = self.class_frames.pop().unwrap().class;
        self.exit_scope(stmt);

        let idx = self.chunk.add_constant(Value::from(self.gc.alloc(class)))?;
        self.classes.insert(decl.name, idx);
        self.emit(opcode::PUSH_CLASS, stmt);
        self.emit(idx, stmt);
        
        Ok(())
    }

    fn expression(&mut self, expr: &ASTId<Expr>) -> Result<(), anyhow::Error> {
        match self.ast.get(expr) {
            Expr::Unary(op, expr) => self.unary_expression(op, expr)?,
            Expr::Binary(op, left, right) => self.binary_expression(op, left, right)?,
            Expr::Ternary(cond, then, otherwise) => self.if_expression(cond, then, otherwise)?,
            Expr::Call(expr, args) => self.call_expression(expr, args)?,
            Expr::Array(exprs) => self.array_expression(expr, exprs)?,
            Expr::Index(expr, id) => self.member_access(expr, id, AccessKind::Get)?,
            Expr::Literal(lit) => self.literal(expr, lit)?,
            Expr::Identifier(name) => self.identifier(expr, *name, false)?,
            Expr::This => self.this(expr)?,
            Expr::Super => self.super_(expr)?
        };

        Ok(())
    }

    fn this(&mut self, expr: &ASTId<Expr>) -> Result<(), anyhow::Error> {
        if self.class_frames.is_empty() {
            bail!("Cannot use 'this' outside of a class method");
        }

        self.emit(opcode::GET_LOCAL, expr);
        self.emit(0, expr);
        Ok(())
    }

    fn super_(&mut self, expr: &ASTId<Expr>) -> Result<(), anyhow::Error> {
        let Some(frame) = self.class_frames.last() else {
            bail!("Cannot use 'super' outside of a class method");
        };
        if frame.superclass.is_none() {
            bail!("Cannot use 'super' outside of a child class method");
        }

        self.emit(opcode::GET_LOCAL, expr);
        self.emit(0, expr);
        Ok(())
    }

    fn if_expression(&mut self, cond: &ASTId<Expr>, then: &ASTId<Expr>, otherwise: &ASTId<Expr>) -> Result<(), anyhow::Error> {
        self.expression(cond)?;
        let jump_ref = self.emit_jump(opcode::JUMP_IF_FALSE, 0, cond);
        self.expression(then)?;
        let else_jump_ref = self.emit_jump(opcode::JUMP, 0, then);
        self.patch_jump(jump_ref)?;
        self.expression(otherwise)?;
        self.patch_jump(else_jump_ref)?;
        Ok(())
    }
    
    fn unary_expression(&mut self, op: &Operator, expr: &ASTId<Expr>) -> Result<(), anyhow::Error> {
        self.expression(expr)?;
        self.emit(op.as_opcode(), expr);
        Ok(())
    }

    fn binary_expression(&mut self, op: &Operator, left: &ASTId<Expr>, right: &ASTId<Expr>) -> Result<(), anyhow::Error> {
        if let Operator::Assign(_) = op {
            match self.ast.get(left) {
                Expr::Identifier(name) => {
                    if let Some(local) = self.locals.iter().rev().find(|local| local.name == *name) {
                        if !local.is_mutable {
                            compiler_error!(self, format!("Invalid assignment: '{}' is immutable", unsafe { &(**name).value }), left);
                        }
                    }

                    self.expression(right)?;
                    self.identifier(left, *name, true)?;
                    return Ok(());
                },
                Expr::Binary(Operator::MemberAccess, obj, member) => {
                    self.expression(right)?;
                    self.member_access(obj, member, AccessKind::Set)?;
                    return Ok(());
                },
                Expr::Index(obj, member) => {
                    self.expression(right)?;
                    self.member_access(obj, member, AccessKind::Set)?;
                    return Ok(());
                },
                _ => compiler_error!(self, "Invalid assignment", left)
            }
        }

        if let Operator::MemberAccess = op {
            self.member_access(left, right, AccessKind::Get)?;
            return Ok(());
        }

        self.expression(left)?;
        self.expression(right)?;
        self.emit(op.as_opcode(), right);
        Ok(())
    }

    fn member_access(&mut self, expr: &ASTId<Expr>, member_expr: &ASTId<Expr>, access_kind: AccessKind) -> Result<(), anyhow::Error> {
        self.expression(expr)?;

        let (operand, get_op, set_op) = match (self.ast.get(expr), self.ast.get(member_expr)) {
            (Expr::This, Expr::Literal(Literal::String(member))) => {
                let frame = self.class_frames.last().unwrap();
                let id = self.resolve_class_member(expr, &frame.class, *member)?;
                (Some(id), opcode::GET_PROPERTY_ID, opcode::SET_PROPERTY_ID)
            },
            (Expr::Super, Expr::Literal(Literal::String(member))) => {
                let frame = self.class_frames.last().unwrap();
                let superclass = unsafe { &*frame.superclass.unwrap() };
                let id = self.resolve_class_member(expr, superclass, *member)?;
                (Some(id), opcode::GET_PROPERTY_ID, opcode::SET_PROPERTY_ID)
            },
            _ => {
                self.expression(member_expr)?;
                (None, opcode::GET_INDEX, opcode::SET_INDEX)
            }
        };

        let op = match access_kind {
            AccessKind::Get => get_op,
            AccessKind::Set => set_op
        };

        self.emit(op, expr);
        if let Some(operand) = operand {
            self.emit(operand, expr);
        }
        Ok(())
    }

    fn resolve_class_member(&self, expr: &ASTId<Expr>, class: &ObjClass, member: *mut ObjString) -> Result<u8, anyhow::Error> {
        let id = match class.resolve(member) {
            Some(ClassMember::Field(id)) => id,
            Some(ClassMember::Method(id)) => id,
            None => unsafe {
                compiler_error!(self, format!("Class '{}' has no member '{}'", (*class.name).value, (*member).value), expr)
            }
        };
        Ok(id)
    }
    
    fn call_expression(&mut self, expr: &ASTId<Expr>, args: &Vec<ASTId<Expr>>) -> Result<(), anyhow::Error> {
        match self.ast.get(expr) {
            Expr::Super => {
                let frame = self.class_frames.last().unwrap();
                let superclass = frame.superclass.unwrap();
                let init_str = self.gc.intern("init");
                let member_id = unsafe { &*superclass }.resolve_id(init_str).unwrap();

                self.emit(opcode::GET_LOCAL, expr);
                self.emit(0, expr);

                self.emit(opcode::GET_PROPERTY_ID, expr);
                self.emit(member_id, expr);
            },
            _ => { self.expression(expr)? }
        };

        for arg in args {
            self.expression(arg)?;
        }
        self.emit(opcode::CALL, expr);
        self.emit(args.len() as u8, expr);
        Ok(())
    }
    
    fn array_expression(&mut self, expr: &ASTId<Expr>, exprs: &Vec<ASTId<Expr>>) -> Result<(), anyhow::Error> {
        for item in exprs {
            self.expression(item)?;
        }
        self.emit(opcode::ARRAY, expr);
        self.emit(exprs.len() as u8, expr);
        Ok(())
    }
    
    fn literal(&mut self, expr: &ASTId<Expr>, literal: &Literal) -> Result<(), anyhow::Error> {
        match literal {
            Literal::Number(num) => {
                let idx = self.chunk.add_constant(Value::from(*num))?;
                self.emit(opcode::PUSH_CONSTANT, expr);
                self.emit(idx, expr);
            },
            Literal::String(str_ref) => {
                let idx = self.chunk.add_constant(Value::from(*str_ref))?;
                self.emit(opcode::PUSH_CONSTANT, expr);
                self.emit(idx, expr);
            },
            Literal::Null => { self.emit(opcode::PUSH_NULL, expr); },
            Literal::Boolean(true) => { self.emit(opcode::PUSH_TRUE, expr); },
            Literal::Boolean(false) => { self.emit(opcode::PUSH_FALSE, expr); }
        };

        return Ok(());
    }
    
    fn identifier(&mut self, expr: &ASTId<Expr>, name: *mut ObjString, assign: bool) -> Result<(), anyhow::Error> {
        let class_frame_idx = self.resolve_member_class(name);

        let (operand, get_op, set_op) = if let Some(local_idx) = self.resolve_local(name) {
            (local_idx, opcode::GET_LOCAL, opcode::SET_LOCAL)
        } else if let Some(upvalue_idx) = self.resolve_upvalue(name, class_frame_idx)? {
            (upvalue_idx, opcode::GET_UPVALUE, opcode::SET_UPVALUE)
        } else if let Some(id) = self.class_frames.last().and_then(|f| f.class.resolve_id(name)) {
            // Implicit 'this'
            self.emit(opcode::GET_LOCAL, expr);
            self.emit(0, expr);
            (id, opcode::GET_PROPERTY_ID, opcode::SET_PROPERTY_ID)
        } else {
            let const_idx = self.chunk.add_constant(Value::from(name))?;
            (const_idx, opcode::GET_GLOBAL, opcode::SET_GLOBAL)
        };

        let op = if assign { set_op } else { get_op };
        self.emit(op, expr);
        self.emit(operand, expr);
        return Ok(());
    }
}
use std::ops::Range;

use anyhow::anyhow;
use anyhow::bail;
use fnv::FnvHashMap;

use super::chunk::BytecodeChunk;
use super::gc::Gc;
use super::objects::ObjClass;
use super::objects::ObjString;
use super::objects::{ObjFn, UpvalueLocation};
use super::opcode;
use super::opcode::OpCode;
use crate::parser;
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
    ast: &'a parser::AST,
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
    pub fn compile<'b>(ast: &'b parser::AST, gc: &'b mut Gc) -> Result<BytecodeChunk, anyhow::Error> {
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

    fn error<T: 'static>(&self, msg: impl Into<String>, node_id: &parser::ASTId<T>) -> anyhow::Error {
        let pos = self.ast.pos(node_id);
        anyhow!("{}\n\tat {}", msg.into(), pos)
    }

    fn emit<T: 'static>(&mut self, byte: u8, node_id: &parser::ASTId<T>) {
        let pos = self.ast.pos(node_id);
        self.chunk.write(byte, pos);
    }

    fn emit_jump<T: 'static>(&mut self, op: OpCode, pos: u16, node_id: &parser::ASTId<T>) -> u16 {
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

    fn exit_scope<T: 'static>(&mut self, node_id: &parser::ASTId<T>) {
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

    fn exit_function<T: 'static>(&mut self, node_id: &parser::ASTId<T>) {
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

    fn statement(&mut self, stmt_id: &parser::ASTId<parser::Stmt>) -> Result<(), anyhow::Error> {
        match self.ast.get(stmt_id) {
            parser::Stmt::Block(stmts) => self.block(stmt_id, stmts)?,
            parser::Stmt::Return(expr) => {
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
            parser::Stmt::Fn(decl) => {
                let name = self.gc.intern(&decl.name);
                self.declare_local(name, false)?;
                let prev_fn_type = self.fn_type;
                self.fn_type = FnType::Function;
                let const_idx = self.function(stmt_id, decl)?;
                self.fn_type = prev_fn_type;
                self.emit(opcode::PUSH_CLOSURE, stmt_id);
                self.emit(const_idx, stmt_id);
            },
            parser::Stmt::Class(decl) => self.class_declaration(stmt_id, decl)?,
            parser::Stmt::If(cond, then, otherwise) => self.if_stmt(cond, then, otherwise)?,
            parser::Stmt::Say(parser::FieldInit { name, value }) => {
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
            parser::Stmt::Expression(expr) => {
                self.expression(expr)?;
                self.emit(opcode::POP, stmt_id);
            },
            parser::Stmt::While(cond, body) => {
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

    fn block(&mut self, stmt: &parser::ASTId<parser::Stmt>, stmts: &Vec<parser::ASTId<parser::Stmt>>) -> Result<(), anyhow::Error> {
        self.enter_scope();
        for stmt in stmts {
            self.statement(stmt)?;
        }
        self.exit_scope(stmt);
        Ok(())
    }

    fn if_stmt(&mut self, cond: &parser::ASTId<parser::Expr>, then: &parser::ASTId<parser::Stmt>, otherwise: &Option<parser::ASTId<parser::Stmt>>) -> Result<(), anyhow::Error> {
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

    fn function(&mut self, stmt: &parser::ASTId<parser::Stmt>, decl: &Box<parser::FnDecl>) -> Result<u8, anyhow::Error> {
        if let FnType::None = self.fn_type {
            bail!("Function declaration outside of function");
        }

        self.enter_function();

        let jump_ref = self.emit_jump(opcode::JUMP, 0, stmt);
        let ip_start = self.chunk.code.len();
        let arity = decl.params.len() as u8;

        for param in &decl.params {
            let param = self.gc.intern(param);
            self.declare_local(param, true)?;
        }

        self.statement(&decl.body)?;
        self.exit_function(&decl.body);

        self.patch_jump(jump_ref)?;

        let frame = self.fn_frames.pop().unwrap();
        let name = self.gc.intern(&decl.name);
        let func = self.gc.alloc(ObjFn::new(name, arity, ip_start, frame.upvalues));

        self.chunk.add_constant(Value::from(func))
    }

    fn class_declaration(&mut self, stmt: &parser::ASTId<parser::Stmt>, decl: &Box<parser::ClassDecl>) -> Result<(), anyhow::Error> {
        let name = self.gc.intern(&decl.name);
        self.declare_local(name, false)?;
        self.enter_scope();

        let superclass = match &decl.superclass {
            Some(name) => {
                let const_idx = self.classes.get(&self.gc.intern(name))
                    .ok_or(anyhow!("Class '{}' not declared", name))?;
                let value = self.chunk.constants[*const_idx as usize];
                Some(value.as_object().as_class_ptr())
            },
            None => None
        };

        let mut frame = ClassFrame {
            class: ObjClass::new(self.gc.intern(&decl.name), superclass.map(|ptr| unsafe { &*ptr })),
            superclass
        };

        // Declare fields and methods before compiling init and method bodies
        for field in &decl.fields {
            frame.class.declare_field(self.gc.intern(field));
        }

        for stmt_id in &decl.methods {
            let parser::Stmt::Fn(decl) = self.ast.get(stmt_id) else { unreachable!(); };
            frame.class.declare_method(self.gc.intern(&decl.name));
        }

        if let Some(_) = &decl.getter {
            frame.class.declare_method(self.gc.preset_identifiers.get);
        }

        if let Some(_) = &decl.setter {
            frame.class.declare_method(self.gc.preset_identifiers.set);
        }

        // Push class frame to make the current class visible in method bodies
        self.class_frames.push(frame);

        if let Some(stmt_id) = &decl.getter {
            let function_ptr = self.class_method(stmt_id)?;
            let frame = self.class_frames.last_mut().unwrap();
            frame.class.define_method(self.gc.preset_identifiers.get, function_ptr);
        }

        if let Some(stmt_id) = &decl.setter {
            let function_ptr = self.class_method(stmt_id)?;
            let frame = self.class_frames.last_mut().unwrap();
            frame.class.define_method(self.gc.preset_identifiers.set, function_ptr);
        }

        // Compile and assign class initializer function
        let init_func_ptr = self.initializer(&decl.init)?;
        let frame = self.class_frames.last_mut().unwrap();
        frame.class.declare_method(self.gc.preset_identifiers.init);
        frame.class.define_method(self.gc.preset_identifiers.init, init_func_ptr);

        // Compile and add methods to class object
        for stmt_id in &decl.methods {
            let parser::Stmt::Fn(decl) = self.ast.get(stmt_id) else { unreachable!(); };
            let function_ptr = self.class_method(stmt_id)?;
            let frame = self.class_frames.last_mut().unwrap();
            frame.class.define_method(self.gc.intern(&decl.name), function_ptr);
        }

        let class = self.class_frames.pop().unwrap().class;
        self.exit_scope(stmt);

        let idx = self.chunk.add_constant(Value::from(self.gc.alloc(class)))?;
        self.classes.insert(self.gc.intern(&decl.name), idx);
        self.emit(opcode::PUSH_CLASS, stmt);
        self.emit(idx, stmt);
        
        Ok(())
    }

    fn initializer(&mut self, stmt: &parser::ASTId<parser::Stmt>) -> Result<*mut ObjFn, anyhow::Error> {
        let parser::Stmt::Fn(decl) = self.ast.get(&stmt) else { unreachable!(); };

        let prev_fn_type = self.fn_type;
        self.fn_type = FnType::Initializer;
        let const_idx = self.function(stmt, decl)?;
        self.fn_type = prev_fn_type;

        let func_const = self.chunk.constants[const_idx as usize];
        Ok(func_const.as_object().as_function_ptr())
    }

    fn class_method(&mut self, stmt: &parser::ASTId<parser::Stmt>) -> Result<*mut ObjFn, anyhow::Error> {
        let parser::Stmt::Fn(decl) = self.ast.get(stmt) else { unreachable!(); };

        let prev_fn_type = self.fn_type;
        self.fn_type = FnType::Method;
        let const_idx = self.function(stmt, decl)?;
        self.fn_type = prev_fn_type;

        let func_const = self.chunk.constants[const_idx as usize];
        Ok(func_const.as_object().as_function_ptr())
    }

    fn expression(&mut self, expr: &parser::ASTId<parser::Expr>) -> Result<(), anyhow::Error> {
        match self.ast.get(expr) {
            parser::Expr::Unary(op, expr) => self.unary_expression(op, expr)?,
            parser::Expr::Binary(op, left, right) => self.binary_expression(op, left, right)?,
            parser::Expr::Ternary(cond, then, otherwise) => self.if_expression(cond, then, otherwise)?,
            parser::Expr::Call(expr, args) => self.call_expression(expr, args)?,
            parser::Expr::Array(exprs) => self.array_expression(expr, exprs)?,
            parser::Expr::Index(expr, id) => self.index(expr, id, None)?,
            parser::Expr::Literal(lit) => self.literal(expr, lit)?,
            parser::Expr::Identifier(name) => {
                let name = self.gc.intern(name);
                self.identifier(expr, name, false)?
            },
            parser::Expr::This => self.this(expr)?,
            parser::Expr::Super => self.super_(expr)?
        };

        Ok(())
    }

    fn this(&mut self, expr: &parser::ASTId<parser::Expr>) -> Result<(), anyhow::Error> {
        if self.class_frames.is_empty() {
            bail!("Cannot use 'this' outside of a class method");
        }

        self.emit(opcode::GET_LOCAL, expr);
        self.emit(0, expr);
        Ok(())
    }

    fn super_(&mut self, expr: &parser::ASTId<parser::Expr>) -> Result<(), anyhow::Error> {
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

    fn if_expression(&mut self, cond: &parser::ASTId<parser::Expr>, then: &parser::ASTId<parser::Expr>, otherwise: &parser::ASTId<parser::Expr>) -> Result<(), anyhow::Error> {
        self.expression(cond)?;
        let jump_ref = self.emit_jump(opcode::JUMP_IF_FALSE, 0, cond);
        self.expression(then)?;
        let else_jump_ref = self.emit_jump(opcode::JUMP, 0, then);
        self.patch_jump(jump_ref)?;
        self.expression(otherwise)?;
        self.patch_jump(else_jump_ref)?;
        Ok(())
    }
    
    fn unary_expression(&mut self, op: &parser::Operator, expr: &parser::ASTId<parser::Expr>) -> Result<(), anyhow::Error> {
        self.expression(expr)?;
        self.emit(opcode::from_operator(op), expr);
        Ok(())
    }

    fn binary_expression(&mut self, op: &parser::Operator, left: &parser::ASTId<parser::Expr>, right: &parser::ASTId<parser::Expr>) -> Result<(), anyhow::Error> {
        if let parser::Operator::Assign(_) = op {
            match self.ast.get(left) {
                parser::Expr::Identifier(name) => {
                    let intern_name = self.gc.intern(name);
                    if let Some(local) = self.locals.iter().rev().find(|local| local.name == intern_name) {
                        if !local.is_mutable {
                            compiler_error!(self, format!("Invalid assignment: '{}' is immutable", name), left);
                        }
                    }

                    self.expression(right)?;
                    self.identifier(left, intern_name, true)?;
                    return Ok(());
                },
                parser::Expr::Index(obj, member) => {
                    self.index(obj, member, Some(right))?;
                    return Ok(());
                },
                _ => compiler_error!(self, "Invalid assignment", left)
            }
        }

        if let parser::Operator::MemberAccess = op {
            self.index(left, right, None)?;
            return Ok(());
        }

        self.expression(left)?;
        self.expression(right)?;
        self.emit(opcode::from_operator(op), right);
        Ok(())
    }

    fn index(&mut self, expr: &parser::ASTId<parser::Expr>, member_expr_id: &parser::ASTId<parser::Expr>, assign_expr: Option<&parser::ASTId<parser::Expr>>) -> Result<(), anyhow::Error> {
        match self.ast.get(expr) {
            parser::Expr::This => {
                let member_expr = self.ast.get(member_expr_id);
                let frame = self.class_frames.last().unwrap();
                let member_id = if let parser::Expr::Literal(parser::Literal::String(name)) = member_expr {
                    frame.class.resolve_id(self.gc.intern(name))
                } else {
                    None
                };

                match (assign_expr, member_id) {
                    (None, Some(id)) => {
                        self.expression(expr)?;
                        self.emit(opcode::GET_PROPERTY_ID, expr);
                        self.emit(id, expr);
                    },
                    (Some(assign_expr), Some(id)) => {
                        self.expression(assign_expr)?;
                        self.expression(expr)?;
                        self.emit(opcode::SET_PROPERTY_ID, assign_expr);
                        self.emit(id, assign_expr);
                    },
                    (None, None) => match frame.class.resolve_id(self.gc.preset_identifiers.get) {
                        Some(getter_id) => {
                            self.expression(expr)?;
                            self.emit(opcode::GET_PROPERTY_ID, expr);
                            self.emit(getter_id, expr);
                            self.expression(member_expr_id)?;
                            self.emit(opcode::CALL, expr);
                            self.emit(1, expr);
                        },
                        None => {
                            let class_name = unsafe { &(*frame.class.name).value };
                            if let parser::Expr::Literal(parser::Literal::String(member_name)) = member_expr {
                                compiler_error!(self, format!("Invalid index: {class_name} doesn't have member {member_name} and doesn't have a getter"), expr)
                            } else {
                                compiler_error!(self, format!("Invalid index: {class_name} doesn't have a getter"), expr)
                            }
                        }
                    },
                    (Some(assign_expr), None) => match frame.class.resolve_id(self.gc.preset_identifiers.set) {
                        Some(setter_id) => {
                            self.expression(expr)?;
                            self.emit(opcode::GET_PROPERTY_ID, assign_expr);
                            self.emit(setter_id, assign_expr);
                            self.expression(member_expr_id)?;
                            self.expression(assign_expr)?;
                            self.emit(opcode::CALL, assign_expr);
                            self.emit(2, assign_expr);
                        },
                        None => {
                            let class_name = unsafe { &(*frame.class.name).value };
                            if let parser::Expr::Literal(parser::Literal::String(member_name)) = member_expr {
                                compiler_error!(self, format!("Invalid index: {class_name} doesn't have member {member_name} and doesn't have a setter"), assign_expr)
                            } else {
                                compiler_error!(self, format!("Invalid index: {class_name} doesn't have a setter"), assign_expr)
                            }
                        }
                    }
                }
            },
            parser::Expr::Super => {
                let member_expr = self.ast.get(member_expr_id);
                let frame = self.class_frames.last().unwrap();
                let superclass = unsafe { &*frame.superclass.unwrap() };
                let member_id = if let parser::Expr::Literal(parser::Literal::String(name)) = member_expr {
                    superclass.resolve_id(self.gc.intern(name))
                } else {
                    None
                };

                match (assign_expr, member_id) {
                    (None, Some(id)) => {
                        self.expression(expr)?;
                        self.emit(opcode::GET_PROPERTY_ID, expr);
                        self.emit(id, expr);
                    },
                    (Some(assign_expr), Some(id)) => {
                        self.expression(assign_expr)?;
                        self.expression(expr)?;
                        self.emit(opcode::SET_PROPERTY_ID, expr);
                        self.emit(id, expr);
                    },
                    (None, None) => match superclass.resolve_id(self.gc.preset_identifiers.get) {
                        Some(getter_id) => {
                            self.expression(expr)?;
                            self.emit(opcode::GET_PROPERTY_ID, expr);
                            self.emit(getter_id, expr);
                            self.expression(member_expr_id)?;
                            self.emit(opcode::CALL, expr);
                            self.emit(1, expr);
                        },
                        None => {
                            let class_name = unsafe { &(*superclass.name).value };
                            if let parser::Expr::Literal(parser::Literal::String(member_name)) = member_expr {
                                compiler_error!(self, format!("Invalid index: {class_name} doesn't have member {member_name} and doesn't have a getter"), expr)
                            } else {
                                compiler_error!(self, format!("Invalid index: {class_name} doesn't have a getter"), expr)
                            }
                        }
                    },
                    (Some(assign_expr), None) => match superclass.resolve_id(self.gc.preset_identifiers.set) {
                        Some(setter_id) => {
                            self.expression(expr)?;
                            self.emit(opcode::GET_PROPERTY_ID, expr);
                            self.emit(setter_id, expr);
                            self.expression(member_expr_id)?;
                            self.expression(assign_expr)?;
                            self.emit(opcode::CALL, expr);
                            self.emit(2, expr);
                        },
                        None => {
                            let class_name = unsafe { &(*superclass.name).value };
                            if let parser::Expr::Literal(parser::Literal::String(member_name)) = member_expr {
                                compiler_error!(self, format!("Invalid index: {class_name} doesn't have member {member_name} and doesn't have a setter"), expr)
                            } else {
                                compiler_error!(self, format!("Invalid index: {class_name} doesn't have a setter"), expr)
                            }
                        }
                    }
                }
            },
            _ => {
                if let Some(assign_expr) = assign_expr {
                    self.expression(assign_expr)?;
                }

                self.expression(expr)?;
                self.expression(member_expr_id)?;
                
                match assign_expr {
                    None => self.emit(opcode::GET_INDEX, expr),
                    Some(_) => self.emit(opcode::SET_INDEX, expr)
                };
            }
        };

        Ok(())
    }
    
    fn call_expression(&mut self, expr: &parser::ASTId<parser::Expr>, args: &Vec<parser::ASTId<parser::Expr>>) -> Result<(), anyhow::Error> {
        match self.ast.get(expr) {
            parser::Expr::Super => {
                let frame = self.class_frames.last().unwrap();
                let superclass = frame.superclass.unwrap();
                let init_str = self.gc.intern("@init");
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
    
    fn array_expression(&mut self, expr: &parser::ASTId<parser::Expr>, exprs: &Vec<parser::ASTId<parser::Expr>>) -> Result<(), anyhow::Error> {
        for item in exprs {
            self.expression(item)?;
        }
        self.emit(opcode::ARRAY, expr);
        self.emit(exprs.len() as u8, expr);
        Ok(())
    }
    
    fn literal(&mut self, expr: &parser::ASTId<parser::Expr>, literal: &parser::Literal) -> Result<(), anyhow::Error> {
        match literal {
            parser::Literal::Number(num) => {
                let idx = self.chunk.add_constant(Value::from(*num))?;
                self.emit(opcode::PUSH_CONSTANT, expr);
                self.emit(idx, expr);
            },
            parser::Literal::String(str) => {
                let str = self.gc.intern(str);
                let idx = self.chunk.add_constant(Value::from(str))?;
                self.emit(opcode::PUSH_CONSTANT, expr);
                self.emit(idx, expr);
            },
            parser::Literal::Null => { self.emit(opcode::PUSH_NULL, expr); },
            parser::Literal::Boolean(true) => { self.emit(opcode::PUSH_TRUE, expr); },
            parser::Literal::Boolean(false) => { self.emit(opcode::PUSH_FALSE, expr); }
        };

        return Ok(());
    }
    
    fn identifier(&mut self, expr: &parser::ASTId<parser::Expr>, name: *mut ObjString, assign: bool) -> Result<(), anyhow::Error> {
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
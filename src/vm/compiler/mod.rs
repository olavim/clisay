use std::ops::Range;

use anyhow::anyhow;
use anyhow::bail;
use fnv::FnvHashMap;

use super::chunk::BytecodeChunk;
use super::gc::Gc;
use super::objects::ObjClass;
use super::objects::ObjString;
use super::objects::UpvalueLocation;
use super::opcode;
use super::opcode::OpCode;
use crate::parser::ASTId;
use crate::parser::Expr;
use crate::parser::AST;
use super::value::Value;

mod expressions;
mod statements;
mod functions;
mod classes;

struct Local {
    name: *mut ObjString,
    depth: u8,
    is_mutable: bool,
    is_captured: bool
}

struct FnFrame {
    upvalues: Vec<UpvalueLocation>,
    local_offset: u8,
    class_frame: Option<u8>,
    kind: FnKind
}

#[derive(Clone, Copy)]
enum FnKind {
    Inlambda,
    Function,
    Method,
    Initializer
}

struct ClassFrame {
    class: ObjClass,
    superclass: Option<ClassCompilation>
}

#[derive(Clone, Copy)]
struct ClassCompilation {
    class: *mut ObjClass,
    next_member_id: u8
}

pub struct Compiler<'a> {
    chunk: BytecodeChunk,
    ast: &'a AST,
    gc: &'a mut Gc,
    locals: Vec<Local>,
    scope_depth: u8,
    fn_frames: Vec<FnFrame>,
    class_frames: Vec<ClassFrame>,
    classes: FnvHashMap<*mut ObjString, ClassCompilation>
}

#[macro_export]
macro_rules! compiler_error {
    ($self:ident, $node:expr, $($arg:tt)*) => { return Err($self.error(format!($($arg)*), $node)) };
}

impl<'a> Compiler<'a> {
    pub fn compile<'b>(ast: &'b AST, gc: &'b mut Gc) -> Result<BytecodeChunk, anyhow::Error> {
        let mut compiler = Compiler {
            chunk: BytecodeChunk::new(),
            ast,
            gc,
            locals: Vec::new(),
            scope_depth: 0,
            fn_frames: Vec::new(),
            class_frames: Vec::new(),
            classes: FnvHashMap::default()
        };

        let stmt_id = compiler.ast.get_root();
        compiler.statement(&stmt_id)?;
        Ok(compiler.finish())
    }

    fn error<T: 'static>(&self, msg: impl Into<String>, node_id: &ASTId<T>) -> anyhow::Error {
        let pos = self.ast.pos(node_id);
        anyhow!("{}\n\tat {}", msg.into(), pos)
    }

    fn finish(mut self) -> BytecodeChunk {
        self.emit(opcode::PUSH_NULL, &self.ast.get_root());
        self.emit(opcode::RETURN, &self.ast.get_root());
        self.chunk
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
    
    fn emit_count(&mut self, expr: &ASTId<Expr>) {
        self.emit(expr.as_comma_separated(self.ast).len() as u8, expr);
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

    fn current_class_frame(&self) -> &ClassFrame {
        self.class_frames.last().unwrap()
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

    fn resolve_upvalue(&mut self, name: *mut ObjString) -> Result<Option<u8>, anyhow::Error> {
        if self.fn_frames.is_empty() {
            return Ok(None);
        }

        let max_class_frame = self.resolve_member_class(name);
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

    fn resolve_member_class(&self, name: *mut ObjString) -> Option<u8> {
        for i in (0..self.class_frames.len()).rev() {
            let class = &self.class_frames[i].class;
            if class.resolve(name).is_some() {
                return Some(i as u8);
            }
        }

        None
    }
}
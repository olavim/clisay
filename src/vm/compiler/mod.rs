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
use crate::parser::Literal;
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

#[derive(Clone, Copy)]
enum TryCatchPosition {
    Try,
    Catch,
    Finally
}

#[derive(Clone)]
struct TryFrame {
    position: TryCatchPosition,
    finally: Option<ASTId<Expr>>
}

pub struct Compiler<'a> {
    chunk: BytecodeChunk,
    ast: &'a AST,
    gc: &'a mut Gc,
    locals: Vec<Local>,
    scope_depth: u8,
    fn_frames: Vec<FnFrame>,
    try_frames: Vec<TryFrame>,
    class_frames: Vec<ClassFrame>,
    classes: FnvHashMap<*mut ObjString, ClassCompilation>,
    setter_pos: Option<usize>
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
            try_frames: Vec::new(),
            class_frames: Vec::new(),
            classes: FnvHashMap::default(),
            setter_pos: None
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

    /// Emits a two-byte instruction: an opcode followed by a single operand byte.
    fn emit_operand<T: 'static>(&mut self, op: OpCode, operand: u8, node_id: &ASTId<T>) {
        self.emit(op, node_id);
        self.emit(operand, node_id);
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

    fn binary_jump_op(op: &crate::parser::Operator) -> Option<OpCode> {
        use crate::parser::Operator;
        Some(match op {
            Operator::LessThan => opcode::JUMP_IF_GE,
            Operator::LessThanEqual => opcode::JUMP_IF_GT,
            Operator::GreaterThan => opcode::JUMP_IF_LE,
            Operator::GreaterThanEqual => opcode::JUMP_IF_LT,
            Operator::LogicalEqual => opcode::JUMP_IF_NEQ,
            Operator::LogicalNotEqual => opcode::JUMP_IF_EQ,
            _ => return None
        })
    }

    /// Fused compare-and-branch variant for the common `local <cmp> number`.
    fn local_const_jump_op(op: &crate::parser::Operator) -> Option<OpCode> {
        use crate::parser::Operator;
        Some(match op {
            Operator::LessThan => opcode::JUMP_IF_GE_LOCAL_CONST,
            Operator::LessThanEqual => opcode::JUMP_IF_GT_LOCAL_CONST,
            Operator::GreaterThan => opcode::JUMP_IF_LE_LOCAL_CONST,
            Operator::GreaterThanEqual => opcode::JUMP_IF_LT_LOCAL_CONST,
            _ => return None
        })
    }

    /// If `left` resolves to a local and `right` is a numeric literal, returns
    /// `(local_slot, const_idx)` so the caller can emit a fused LOCAL_CONST op.
    fn local_const_operands(&mut self, left: &ASTId<Expr>, right: &ASTId<Expr>) -> Result<Option<(u8, u8)>, anyhow::Error> {
        let Expr::Identifier(name) = self.ast.get(left) else { return Ok(None) };
        let name = self.gc.intern(name);
        let Some(local_idx) = self.resolve_local(name) else { return Ok(None) };

        let Expr::Literal(Literal::Number(num)) = self.ast.get(right) else { return Ok(None) };
        let const_idx = self.chunk.add_constant(Value::from(*num))?;
        Ok(Some((local_idx, const_idx)))
    }

    fn emit_conditional_jump<T: 'static>(&mut self, cond: &ASTId<Expr>, node_id: &ASTId<T>) -> Result<u16, anyhow::Error> {
        if let Expr::Binary(op, left, right) = self.ast.get(cond) {
            let (op, left, right) = (op.clone(), *left, *right);

            // Fused `local <cmp> number`: operands live in the instruction, so the
            // jump's 2-byte offset stays at +1/+2 (emitted first) and `patch_jump`
            // works unchanged; the slot/const bytes trail it.
            if let (Some(fused_op), Some((local_idx, const_idx))) = (Self::local_const_jump_op(&op), self.local_const_operands(&left, &right)?) {
                let jump_ref = self.emit_jump(fused_op, 0, node_id);
                self.emit(local_idx, node_id);
                self.emit(const_idx, node_id);
                return Ok(jump_ref);
            }

            if let Some(jump_op) = Self::binary_jump_op(&op) {
                self.expression(&left)?;
                self.expression(&right)?;
                return Ok(self.emit_jump(jump_op, 0, node_id));
            }
        }

        self.expression(cond)?;
        Ok(self.emit_jump(opcode::JUMP_IF_FALSE, 0, node_id))
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
                self.emit_operand(opcode::CLOSE_UPVALUE, self.locals.len() as u8 - 1, node_id);
            } else {
                self.emit(opcode::POP, node_id);
            }
            self.locals.pop();
        }
    }

    /// Declares a new local variable in the current scope. Returns the slot index of the variable,
    /// which is relative to the current function's `local_offset` (i.e. the operand for `GET_LOCAL`/`SET_LOCAL`).
    fn declare_local<T: 'static>(&mut self, name: *mut ObjString, is_mutable: bool, node_id: &ASTId<T>) -> Result<u8, anyhow::Error> {
        if self.locals.len() >= u8::MAX as usize {
            bail!("Too many variables in scope");
        }

        if self.locals.iter().rev().any(|local| local.depth == self.scope_depth && local.name == name) {
            compiler_error!(self, node_id, "Variable '{}' already declared in this scope", unsafe { &(*name).value });
        }

        self.chunk.add_constant(Value::from(name))?;
        self.locals.push(Local { name, depth: self.scope_depth, is_mutable, is_captured: false });

        let local_offset = self.fn_frames.last().map_or(0, |frame| frame.local_offset);
        Ok((self.locals.len() - 1) as u8 - local_offset)
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
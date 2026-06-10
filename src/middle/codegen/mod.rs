use std::ops::Range;

use anyhow::anyhow;
use anyhow::bail;
use fnv::FnvHashMap;

use crate::ast;
use crate::core::gc::Gc;
use crate::core::objects::ObjClass;
use crate::core::objects::ObjString;
use crate::core::objects::UpvalueLocation;
use crate::core::value::Value;
use crate::middle::ir::{Inst, Ir, Label};
use crate::ast::AstId;
use crate::ast::Expr;
use crate::ast::FnDecl;
use crate::ast::Stmt;
use crate::ast::Ast;

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
    finally: Option<AstId<Expr>>
}

pub struct Compiler<'a> {
    ir: Ir,
    ast: &'a Ast,
    gc: &'a mut Gc,
    locals: Vec<Local>,
    scope_depth: u8,
    fn_frames: Vec<FnFrame>,
    try_frames: Vec<TryFrame>,
    class_frames: Vec<ClassFrame>,
    classes: FnvHashMap<*mut ObjString, ClassCompilation>
}

#[macro_export]
macro_rules! compiler_error {
    ($self:ident, $node:expr, $($arg:tt)*) => { return Err($self.error(format!($($arg)*), $node)) };
}

impl<'a> Compiler<'a> {
    pub fn compile<'b>(ast: &'b Ast, gc: &'b mut Gc) -> Result<Ir, anyhow::Error> {
        let mut compiler = Compiler {
            ir: Ir::new(),
            ast,
            gc,
            locals: Vec::new(),
            scope_depth: 0,
            fn_frames: Vec::new(),
            try_frames: Vec::new(),
            class_frames: Vec::new(),
            classes: FnvHashMap::default()
        };

        let stmt_id = compiler.ast.get_root();
        compiler.statement(&stmt_id)?;
        Ok(compiler.finish())
    }

    fn error<T: 'static>(&self, msg: impl Into<String>, node_id: &AstId<T>) -> anyhow::Error {
        let pos = self.ast.pos(node_id);
        anyhow!("{}\n\tat {}", msg.into(), pos)
    }

    fn finish(mut self) -> Ir {
        self.emit(Inst::PushNull, &self.ast.get_root());
        self.emit(Inst::Return, &self.ast.get_root());
        self.ir
    }

    fn emit<T: 'static>(&mut self, inst: Inst, node_id: &AstId<T>) {
        let pos = self.ast.pos(node_id);
        self.ir.emit(inst, pos);
    }

    fn binary_jump_inst(op: &ast::Operator) -> Option<fn(Label) -> Inst> {
        use ast::Operator;
        Some(match op {
            Operator::LessThan => Inst::JumpIfGe,
            Operator::LessThanEqual => Inst::JumpIfGt,
            Operator::GreaterThan => Inst::JumpIfLe,
            Operator::GreaterThanEqual => Inst::JumpIfLt,
            Operator::LogicalEqual => Inst::JumpIfNeq,
            Operator::LogicalNotEqual => Inst::JumpIfEq,
            _ => return None
        })
    }

    /// Fused compare-and-branch variant for the common `local <cmp> number`.
    fn local_const_jump_inst(op: &ast::Operator) -> Option<fn(Label, u8, u8) -> Inst> {
        use ast::Operator;
        Some(match op {
            Operator::LessThan => Inst::JumpIfGeLocalConst,
            Operator::LessThanEqual => Inst::JumpIfGtLocalConst,
            Operator::GreaterThan => Inst::JumpIfLeLocalConst,
            Operator::GreaterThanEqual => Inst::JumpIfLtLocalConst,
            _ => return None
        })
    }

    /// The comparison with its operands swapped (`a < b` => `b > a`), letting a
    /// `const <cmp> local` condition reuse the `local <cmp> const` fused ops.
    fn flip_cmp(op: &ast::Operator) -> ast::Operator {
        use ast::Operator;
        match op {
            Operator::LessThan => Operator::GreaterThan,
            Operator::LessThanEqual => Operator::GreaterThanEqual,
            Operator::GreaterThan => Operator::LessThan,
            Operator::GreaterThanEqual => Operator::LessThanEqual,
            other => other.clone()
        }
    }

    /// Emits a conditional branch to a fresh (unbound) label and returns it; the
    /// caller should bind the label at the jump's destination.
    fn emit_conditional_jump<T: 'static>(&mut self, cond: &AstId<Expr>, node_id: &AstId<T>) -> Result<Label, anyhow::Error> {
        let target = self.ir.new_label();

        if let Expr::Binary(op, left, right) = self.ast.get(cond) {
            let (op, left, right) = (op.clone(), *left, *right);

            // Fused `local <cmp> number`. Operands may be in either order:
            // `const <cmp> local` reuses the same ops with the comparison flipped.
            let mut fused = None;
            if let Some(to_inst) = Self::local_const_jump_inst(&op) {
                fused = self.try_local_const(&left, &right)?.map(|(l, c)| (to_inst, l, c));
            }
            if fused.is_none() {
                if let Some(to_inst) = Self::local_const_jump_inst(&Self::flip_cmp(&op)) {
                    fused = self.try_local_const(&right, &left)?.map(|(l, c)| (to_inst, l, c));
                }
            }
            if let Some((to_inst, local_idx, const_idx)) = fused {
                self.emit(to_inst(target, local_idx, const_idx), node_id);
                return Ok(target);
            }

            if let Some(to_inst) = Self::binary_jump_inst(&op) {
                self.expression(&left)?;
                self.expression(&right)?;
                self.emit(to_inst(target), node_id);
                return Ok(target);
            }
        }

        self.expression(cond)?;
        self.emit(Inst::JumpIfFalse(target), node_id);
        Ok(target)
    }

    fn current_class_frame(&self) -> &ClassFrame {
        self.class_frames.last().unwrap()
    }

    /// Unwraps a `Stmt::Fn` node into its declaration. Callers only ever pass
    /// statements the parser guarantees are functions (methods, initializers).
    fn fn_decl(&self, stmt: &AstId<Stmt>) -> &'a FnDecl {
        let Stmt::Fn(decl) = self.ast.get(stmt) else {
            unreachable!("expected a function statement");
        };
        decl
    }

    fn enter_scope(&mut self) {
        self.scope_depth += 1;
    }

    fn exit_scope<T: 'static>(&mut self, node_id: &AstId<T>) {
        self.scope_depth -= 1;
        while !self.locals.is_empty() && self.locals.last().unwrap().depth > self.scope_depth {
            if self.locals.last().unwrap().is_captured {
                self.emit(Inst::CloseUpvalue(self.locals.len() as u8 - 1), node_id);
            } else {
                self.emit(Inst::Pop, node_id);
            }
            self.locals.pop();
        }
    }

    /// Declares a new local variable in the current scope. Returns the slot index of the variable,
    /// which is relative to the current function's `local_offset`.
    fn declare_local<T: 'static>(&mut self, name: *mut ObjString, is_mutable: bool, node_id: &AstId<T>) -> Result<u8, anyhow::Error> {
        if self.locals.len() >= u8::MAX as usize {
            bail!("Too many variables in scope");
        }

        if self.locals.iter().rev().any(|local| local.depth == self.scope_depth && local.name == name) {
            compiler_error!(self, node_id, "Variable '{}' already declared in this scope", unsafe { &(*name).value });
        }

        self.ir.add_constant(Value::from(name))?;
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
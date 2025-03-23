mod vm;
mod gc;
mod operator;
mod parser;
mod compiler;

use core::fmt;
use std::{any::Any, mem};
use anyhow::bail;

use compiler::Compiler;
use gc::{GcTraceable, GcRef};
use operator::Operator;

pub use gc::Gc;
pub use parser::Parser;
pub use vm::Vm;

use crate::lexer::SourcePosition;

#[derive(Clone)]
pub struct BytecodeChunk {
    code: Vec<OpCode>,
    constants: Vec<Value>,
    code_pos: Vec<SourcePosition>
}

impl BytecodeChunk {
    fn new() -> BytecodeChunk {
        return BytecodeChunk { code: Vec::new(), constants: Vec::new(), code_pos: Vec::new() };
    }

    fn write(&mut self, op: OpCode, pos: &SourcePosition) {
        self.code.push(op);
        self.code_pos.push(pos.clone());
    }

    fn add_constant(&mut self, value: Value) -> Result<u8, anyhow::Error> {
        if self.constants.len() >= u8::MAX as usize {
            bail!("Too many constants");
        }

        self.constants.push(value);
        Ok((self.constants.len() - 1) as u8)
    }
}

impl GcTraceable for BytecodeChunk {
    fn fmt(&self, gc: &Gc) -> String {
        let mut string = String::new();

        macro_rules! push_fmt {
            ($($t:tt)*) => {{
                string.push_str(&format!($($t)*));
            }};
        }

        let mut i = 0;
        while i < self.code.len() {
            let op = self.code[i];
            i += 1;

            push_fmt!("{i}: ");

            match op {
                OpCode::Return => push_fmt!("RET"),
                OpCode::Pop => push_fmt!("POP"),
                OpCode::Call(arg_count) => push_fmt!("CALL {arg_count}"),
                OpCode::Jump(lpos, rpos) => push_fmt!("JUMP <{}>", (lpos as u16) << 8 | (rpos as u16)),
                OpCode::JumpIfFalse(lpos, rpos) => push_fmt!("JUMP_F <{}>", (lpos as u16) << 8 | (rpos as u16)),
                OpCode::CloseUpvalue(idx) => push_fmt!("CLOSE_UPVALUE <{}>", idx),
                OpCode::GetGlobal(const_idx) => {
                    push_fmt!("GET_GLOBAL {}", self.constants[const_idx as usize].fmt(gc));
                }
                OpCode::SetGlobal(const_idx) => {
                    push_fmt!("SET_GLOBAL {}", self.constants[const_idx as usize].fmt(gc));
                },
                OpCode::PushConstant(const_idx) => {
                    push_fmt!("CONST {}", self.constants[const_idx as usize].fmt(gc));
                }
                OpCode::PushNull => push_fmt!("NULL"),
                OpCode::PushTrue => push_fmt!("TRUE"),
                OpCode::PushFalse => push_fmt!("FALSE"),
                OpCode::Add => push_fmt!("ADD"),
                OpCode::Subtract => push_fmt!("SUB"),
                OpCode::Multiply => push_fmt!("MUL"),
                OpCode::Divide => push_fmt!("DIV"),
                OpCode::Negate => push_fmt!("NEG"),
                OpCode::Equal => push_fmt!("EQ"),
                OpCode::NotEqual => push_fmt!("NEQ"),
                OpCode::LessThan => push_fmt!("LT"),
                OpCode::LessThanEqual => push_fmt!("LTE"),
                OpCode::GreaterThan => push_fmt!("GT"),
                OpCode::GreaterThanEqual => push_fmt!("GTE"),
                OpCode::Not => push_fmt!("NOT"),
                OpCode::PushClosure(idx) => {
                    let function = &self.constants[idx as usize];

                    if let Value::Function(gc_ref) = function {
                        let function = gc.get(gc_ref);
                        push_fmt!("{}", function.fmt(gc));
                    }
                },
                OpCode::GetLocal(loc_idx) => push_fmt!("GET_LOCAL <{}>", loc_idx),
                OpCode::SetLocal(loc_idx) => push_fmt!("SET_LOCAL <{}>", loc_idx),
                OpCode::GetUpvalue(loc_idx) => push_fmt!("GET_UPVAL <{}>", loc_idx),
                OpCode::SetUpvalue(loc_idx) => push_fmt!("GET_UPVAL <{}>", loc_idx)
            }

            push_fmt!("\n");
        }
        
        string
    }

    fn mark_refs(&self, gc: &mut Gc) {
        for constant in &self.constants {
            constant.mark_refs(gc);
        }
    }

    fn size(&self) -> usize {
        mem::size_of::<BytecodeChunk>()
            + self.code.capacity() * mem::size_of::<OpCode>()
            + self.constants.capacity() * mem::size_of::<Value>()
            + self.code_pos.capacity() * mem::size_of::<SourcePosition>()
    }

    fn as_any(&self) -> &dyn Any {
        return self;
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

pub struct NativeFunction {
    name: GcRef<String>,
    arity: u8,
    function: fn(vm: &mut Vm)
}

impl GcTraceable for NativeFunction {
    fn fmt(&self, gc: &Gc) -> String {
        format!("<native fn {}>", gc.get(&self.name))
    }

    fn mark_refs(&self, gc: &mut Gc) {
        gc.mark_object(&self.name);
    }

    fn size(&self) -> usize {
        mem::size_of::<NativeFunction>()
    }

    fn as_any(&self) -> &dyn Any {
        return self;
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

#[derive(Clone, Copy)]
struct FnUpvalue {
    is_local: bool,
    location: u8
}

pub struct Function {
    name: GcRef<String>,
    arity: u8,
    ip_start: usize,
    upvalues: Vec<FnUpvalue>
}

impl GcTraceable for Function {
    fn fmt(&self, gc: &Gc) -> String {
        format!("<fn {}>", gc.get(&self.name))
    }

    fn mark_refs(&self, gc: &mut Gc) {
        gc.mark_object(&self.name);
    }

    fn size(&self) -> usize {
        mem::size_of::<Function>() + self.upvalues.capacity() * mem::size_of::<FnUpvalue>()
    }

    fn as_any(&self) -> &dyn Any {
        return self;
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

pub struct Closure {
    function: GcRef<Function>,
    upvalues: Vec<GcRef<Upvalue>>
}

impl Closure {
    fn new(function: GcRef<Function>) -> Closure {
        return Closure { function, upvalues: Vec::new() };
    }
}

impl GcTraceable for Closure {
    fn fmt(&self, gc: &Gc) -> String {
        format!("<closure {}>", gc.get(&gc.get(&self.function).name))
    }

    fn mark_refs(&self, gc: &mut Gc) {
        gc.mark_object(&self.function);

        for upvalue in &self.upvalues {
            gc.mark_object(upvalue);
        }
    }

    fn size(&self) -> usize {
        mem::size_of::<Closure>() + self.upvalues.capacity() * mem::size_of::<GcRef<Upvalue>>()
    }

    fn as_any(&self) -> &dyn Any {
        return self;
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

struct Upvalue {
    location: u8,
    closed: Option<Value>
}

impl GcTraceable for Upvalue {
    fn fmt(&self, _gc: &Gc) -> String {
        format!("<up {}>", self.location)
    }

    fn mark_refs(&self, gc: &mut Gc) {
        if let Some(value) = &self.closed {
            value.mark_refs(gc);
        }
    }

    fn size(&self) -> usize {
        mem::size_of::<Upvalue>()
    }

    fn as_any(&self) -> &dyn Any {
        return self;
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

#[derive(Clone, Copy)]
pub enum Value {
    Null,
    Number(f64),
    Boolean(bool),
    String(GcRef<String>),
    Closure(GcRef<Closure>),
    Function(GcRef<Function>),
    NativeFunction(GcRef<NativeFunction>)
}

impl GcTraceable for Value {
    fn fmt(&self, gc: &Gc) -> String {
        match self {
            Value::Null => format!("null"),
            Value::Number(num) => format!("{num}"),
            Value::Boolean(b) => format!("{b}"),

            // GcRef types
            Value::String(gc_ref) => gc.get(gc_ref).fmt(gc),
            Value::Function(gc_ref) => gc.get(gc_ref).fmt(gc),
            Value::Closure(gc_ref) => gc.get(gc_ref).fmt(gc),
            Value::NativeFunction(gc_ref) => gc.get(gc_ref).fmt(gc)
        }
    }

    fn mark_refs(&self, gc: &mut Gc) {
        match self {
            Value::String(gc_ref) => gc.mark_object(gc_ref),
            Value::Function(gc_ref) => gc.mark_object(gc_ref),
            Value::Closure(gc_ref) => gc.mark_object(gc_ref),
            Value::NativeFunction(gc_ref) => gc.mark_object(gc_ref),
            _ => {}
        }
    }

    fn size(&self) -> usize {
        mem::size_of::<Value>()
    }
    
    fn as_any(&self) -> &dyn Any {
        unreachable!();
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        unreachable!();
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Value) -> bool {
        return match (self, other) {
            (Value::Null, Value::Null) => true,
            (Value::Number(a), Value::Number(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Function(a), Value::Function(b)) => a == b,
            (Value::Closure(a), Value::Closure(b)) => a == b,
            (Value::NativeFunction(a), Value::NativeFunction(b)) => a == b,
            _ => false
        };
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Value) -> Option<std::cmp::Ordering> {
        return match (self, other) {
            (Value::Number(a), Value::Number(b)) => a.partial_cmp(b),
            _ => None
        };
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            Value::Null => "null",
            Value::Number(_) => "number",
            Value::Boolean(_) => "boolean",
            Value::String(_) => "string",
            Value::Function(_) => "function",
            Value::Closure(_) => "function",
            Value::NativeFunction(_) => "function"
        })
    }
}

#[derive(Clone, Copy, PartialEq)]
enum OpCode {
    Return,
    Pop,
    Call(u8),
    Jump(u8, u8),
    JumpIfFalse(u8, u8),
    CloseUpvalue(u8),

    PushConstant(u8),
    PushNull,
    PushTrue,
    PushFalse,
    PushClosure(u8),

    GetGlobal(u8),
    SetGlobal(u8),
    GetLocal(u8),
    SetLocal(u8),
    GetUpvalue(u8),
    SetUpvalue(u8),

    // Arithmetic
    Add,
    Subtract,
    Multiply,
    Divide,
    Negate,

    // Logical
    Equal,
    NotEqual,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
    Not
}

impl OpCode {
    fn from_operator(op: &Operator) -> OpCode {
        return match op {
            // Infix
            Operator::Add => OpCode::Add,
            Operator::Subtract => OpCode::Subtract,
            Operator::Multiply => OpCode::Multiply,
            Operator::Divide => OpCode::Divide,
            Operator::LogicalEqual => OpCode::Equal,
            Operator::LogicalNotEqual => OpCode::NotEqual,
            Operator::LessThan => OpCode::LessThan,
            Operator::LessThanEqual => OpCode::LessThanEqual,
            Operator::GreaterThan => OpCode::GreaterThan,
            Operator::GreaterThanEqual => OpCode::GreaterThanEqual,
            Operator::LogicalNot => OpCode::Not,

            // Prefix
            Operator::Negate => OpCode::Negate,
            _ => unreachable!()
        };
    }
}

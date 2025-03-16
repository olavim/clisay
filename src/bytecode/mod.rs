mod vm;
mod gc;
mod operator;
mod compiler;

use core::fmt;
use std::any::Any;
use anyhow::bail;

use compiler::Compiler;
use gc::{GcObj, GcRef};
use operator::Operator;

pub use gc::Gc;
pub use vm::Vm;

#[derive(Clone)]
pub struct BytecodeChunk {
    code: Vec<u8>,
    constants: Vec<Value>
}

impl BytecodeChunk {
    fn new() -> BytecodeChunk {
        return BytecodeChunk { code: Vec::new(), constants: Vec::new() };
    }

    fn write_byte(&mut self, byte: impl Into<u8>) {
        self.code.push(byte.into());
    }

    fn add_constant(&mut self, value: Value) -> Result<u8, anyhow::Error> {
        if self.constants.len() >= u8::MAX as usize {
            bail!("Too many constants");
        }

        self.constants.push(value);
        Ok((self.constants.len() - 1) as u8)
    }

    fn init_jump(&mut self, byte: impl Into<u8>) -> usize {
        self.write_byte(byte);
        self.write_byte(0);
        self.write_byte(0);
        return self.code.len() - 3;
    }

    fn patch_jump(&mut self, jump_ref: usize) -> Result<(), anyhow::Error> {
        if self.code.len() > u16::MAX as usize {
            bail!("Jump too large");
        }

        let pos = self.code.len() as u16;
        self.code[jump_ref + 1] = (pos >> 8) as u8;
        self.code[jump_ref + 2] = pos as u8;
        Ok(())
    }
}

struct NativeFunction {
    name: GcRef<String>,
    arity: u8,
    function: fn(vm: &Vm, &[Value]) -> Value
}

impl GcObj for NativeFunction {
    fn fmt(&self, f: &mut fmt::Formatter, gc: &Gc) -> fmt::Result {
        return write!(f, "<native fn {}>", gc.get(&self.name));
    }

    fn as_any(&self) -> &dyn Any {
        return self;
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

struct Function {
    name: GcRef<String>,
    arity: u8,
    ip_start: usize,
    upvalue_count: u8
}

impl GcObj for Function {
    fn fmt(&self, f: &mut fmt::Formatter, gc: &Gc) -> fmt::Result {
        return write!(f, "<fn {}>", gc.get(&self.name));
    }

    fn as_any(&self) -> &dyn Any {
        return self;
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

struct Closure {
    name: GcRef<String>,
    function: GcRef<Function>,
    upvalues: Vec<GcRef<Upvalue>>
}

impl GcObj for Closure {
    fn fmt(&self, f: &mut fmt::Formatter, gc: &Gc) -> fmt::Result {
        return write!(f, "<closure {}>", gc.get(&self.name));
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

impl GcObj for Upvalue {
    fn fmt(&self, f: &mut fmt::Formatter, _gc: &Gc) -> fmt::Result {
        return write!(f, "<up {}>", self.location);
    }

    fn as_any(&self) -> &dyn Any {
        return self;
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

#[derive(Clone, Copy)]
enum Value {
    Null,
    Number(f64),
    Boolean(bool),
    String(GcRef<String>),
    Closure(GcRef<Closure>),
    Function(GcRef<Function>),
    NativeFunction(GcRef<NativeFunction>)
}

impl GcObj for Value {
    fn fmt(&self, f: &mut fmt::Formatter, gc: &Gc) -> fmt::Result {
        return match self {
            Value::Null => write!(f, "null"),
            Value::Number(num) => write!(f, "{num}"),
            Value::Boolean(b) => write!(f, "{b}"),

            // GcRef types
            Value::String(gc_ref) => gc.get(gc_ref).fmt(f, gc),
            Value::Function(gc_ref) => gc.get(gc_ref).fmt(f, gc),
            Value::Closure(gc_ref) => gc.get(gc_ref).fmt(f, gc),
            Value::NativeFunction(gc_ref) => gc.get(gc_ref).fmt(f, gc)
        };
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

#[derive(Clone, Copy)]
#[repr(u8)]
enum OpCode {
    Return = 0,
    Pop = 1,
    Call = 2,
    Jump = 3,
    JumpIfFalse = 4,
    CloseUpvalue = 5,

    PushConstant = 10,
    PushNull = 11,
    PushTrue = 12,
    PushFalse = 13,
    PushClosure = 14,

    GetGlobal = 20,
    SetGlobal = 21,
    GetLocal = 22,
    SetLocal = 23,
    GetUpvalue = 24,
    SetUpvalue = 25,

    // Arithmetic
    Add = 100,
    Subtract = 101,
    Multiply = 102,
    Divide = 103,
    Negate = 104,

    // Logical
    Equal = 120,
    NotEqual = 121,
    LessThan = 122,
    LessThanEqual = 123,
    GreaterThan = 124,
    GreaterThanEqual = 125,
    Not = 126
}

impl OpCode {
    fn from_byte(byte: u8) -> OpCode {
        return match byte {
            0 => OpCode::Return,
            1 => OpCode::Pop,
            2 => OpCode::Call,
            3 => OpCode::Jump,
            4 => OpCode::JumpIfFalse,
            5 => OpCode::CloseUpvalue,

            10 => OpCode::PushConstant,
            11 => OpCode::PushNull,
            12 => OpCode::PushTrue,
            13 => OpCode::PushFalse,
            14 => OpCode::PushClosure,

            20 => OpCode::GetGlobal,
            21 => OpCode::SetGlobal,
            22 => OpCode::GetLocal,
            23 => OpCode::SetLocal,
            24 => OpCode::GetUpvalue,
            25 => OpCode::SetUpvalue,

            100 => OpCode::Add,
            101 => OpCode::Subtract,
            102 => OpCode::Multiply,
            103 => OpCode::Divide,
            104 => OpCode::Negate,
            
            120 => OpCode::Equal,
            121 => OpCode::NotEqual,
            122 => OpCode::LessThan,
            123 => OpCode::LessThanEqual,
            124 => OpCode::GreaterThan,
            125 => OpCode::GreaterThanEqual,
            126 => OpCode::Not,
            _ => unreachable!()
        };
    }

    fn from_operator(op: Operator) -> OpCode {
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

            // Postfix
            Operator::Call => OpCode::Call,
            _ => unreachable!()
        };
    }
}

impl Into<u8> for OpCode {
    fn into(self) -> u8 {
        return self as u8;
    }
}

impl fmt::Display for OpCode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        return write!(f, "{:#04x} {}", *self as u8, match self {
            OpCode::Return => "RET",
            OpCode::Pop => "POP",
            OpCode::Call => "CALL",
            OpCode::Jump => "JUMP",
            OpCode::JumpIfFalse => "JUMP_IF_FALSE",
            OpCode::CloseUpvalue => "CLOSE_UPVALUE",

            OpCode::PushConstant => "PUSH_CONST",
            OpCode::PushNull => "PUSH_NULL",
            OpCode::PushTrue => "PUSH_TRUE",
            OpCode::PushFalse => "PUSH_FALSE",
            OpCode::PushClosure => "PUSH_CLOSURE",

            OpCode::GetGlobal => "GET_GLOBAL",
            OpCode::SetGlobal => "SET_GLOBAL",
            OpCode::GetLocal => "GET_LOCAL",
            OpCode::SetLocal => "SET_LOCAL",
            OpCode::GetUpvalue => "GET_UPVALUE",
            OpCode::SetUpvalue => "SET_UPVALUE",

            OpCode::Add => "ADD",
            OpCode::Subtract => "SUB",
            OpCode::Multiply => "MUL",
            OpCode::Divide => "DIV",
            OpCode::Negate => "NEG",
            OpCode::Equal => "EQ",
            OpCode::NotEqual => "NEQ",
            OpCode::LessThan => "LT",
            OpCode::LessThanEqual => "LTE",
            OpCode::GreaterThan => "GT",
            OpCode::GreaterThanEqual => "GTE",
            OpCode::Not => "NOT"
        });
    }
}

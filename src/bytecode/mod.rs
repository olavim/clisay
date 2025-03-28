mod vm;
mod gc;
mod operator;
mod objects;
mod parser;
mod compiler;

use operator::Operator;

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
    PushClass(u8),

    GetGlobal(u8),
    SetGlobal(u8),
    GetLocal(u8),
    SetLocal(u8),
    GetUpvalue(u8),
    SetUpvalue(u8),
    GetProperty(u8),
    SetProperty(u8),
    GetThisProperty(u8),
    SetThisProperty(u8),

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

pub fn run(file_name: &str, src: &str) -> Result<Vec<String>, anyhow::Error> {
    vm::Vm::run(file_name, src)
}
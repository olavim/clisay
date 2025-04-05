mod chunk;
mod vm;
mod gc;
mod operator;
mod objects;
mod value;
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
    GetPropertyId(u8),
    SetPropertyId(u8),

    // Arithmetic
    Add,
    Subtract,
    Multiply,
    Divide,
    Negate,
    LeftShift,
    RightShift,
    BitAnd,
    BitOr,
    BitXor,
    BitNot,

    // Logical
    Equal,
    NotEqual,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
    Not,
    And,
    Or
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
            Operator::BitAnd => OpCode::BitAnd,
            Operator::BitOr => OpCode::BitOr,
            Operator::BitXor => OpCode::BitXor,
            Operator::LeftShift => OpCode::LeftShift,
            Operator::RightShift => OpCode::RightShift,
            Operator::LogicalAnd => OpCode::And,
            Operator::LogicalOr => OpCode::Or,
            Operator::Ternary => unreachable!(),
            Operator::MemberAccess => unreachable!(),
            Operator::Assign(_) => unreachable!(),

            // Prefix
            Operator::Negate => OpCode::Negate,
            Operator::LogicalNot => OpCode::Not,
            Operator::BitNot => OpCode::BitNot,
            Operator::Group => unreachable!(),

            // Postfix
            Operator::Call => unreachable!()
        };
    }
}

pub fn run(file_name: &str, src: &str) -> Result<Vec<String>, anyhow::Error> {
    vm::Vm::run(file_name, src)
}
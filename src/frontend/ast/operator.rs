use std::fmt;

#[derive(Clone, Debug)]
pub enum Operator {
    // Infix
    Add,
    Subtract,
    Multiply,
    Divide,
    LeftShift,
    RightShift,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
    LogicalEqual,
    LogicalNotEqual,
    LogicalAnd,
    LogicalOr,
    BitAnd,
    BitOr,
    BitXor,
    Is, // expr is TypeOrTrait
    Has, // expr has spec
    MemberAccess, // expr.member
    Comma, // expr, expr
    Arrow, // expr => expr
    Coalesce, // a ?? b
    Assign(Option<Box<Operator>>),

    // Prefix
    Negate,
    LogicalNot,
    BitNot,
    Group, // (expr)
    Array, // [expr, ...]
    Dict,  // { key: expr, ... }

    // Postfix
    Call,  // expr(expr, ...)
    Index, // expr[expr]
    SafeMemberAccess, // expr?.member
    SafeIndex, // expr?[expr]
    Assert // expr!
}

impl Operator {
    pub fn assign(op: Operator) -> Operator {
        return Operator::Assign(Some(Box::new(op)));
    }
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        return write!(f, "{}", match self {
            Operator::Add => "+",
            Operator::Subtract => "-",
            Operator::Multiply => "*",
            Operator::Divide => "/",
            Operator::LeftShift => "<<",
            Operator::RightShift => ">>",
            Operator::LessThan => "<",
            Operator::LessThanEqual => "<=",
            Operator::GreaterThan => ">",
            Operator::GreaterThanEqual => ">=",
            Operator::LogicalEqual => "==",
            Operator::LogicalNotEqual => "!=",
            Operator::LogicalAnd => "&&",
            Operator::LogicalOr => "||",
            Operator::BitAnd => "&",
            Operator::BitOr => "|",
            Operator::BitXor => "^",
            Operator::Is => "is",
            Operator::Has => "has",
            Operator::Negate => "-",
            Operator::LogicalNot => "!",
            Operator::BitNot => "~",
            Operator::Group => "<group>",
            Operator::Call => "<call>",
            Operator::MemberAccess => ".",
            Operator::Comma => ",",
            Operator::Array => "<array>",
            Operator::Dict => "<dict>",
            Operator::Arrow => "=>",
            Operator::Coalesce => "??",
            Operator::Index => "<index>",
            Operator::SafeMemberAccess => "?.",
            Operator::SafeIndex => "?[",
            Operator::Assert => "!",
            Operator::Assign(None) => "=",
            Operator::Assign(Some(op)) => match **op {
                Operator::Add => "+=",
                Operator::Subtract => "-=",
                Operator::LeftShift => "<<=",
                Operator::RightShift => ">>=",
                Operator::Multiply => "*=",
                Operator::Divide => "/=",
                Operator::BitAnd => "&=",
                Operator::BitOr => "|=",
                Operator::BitXor => "^=",
                Operator::LogicalAnd => "&&=",
                Operator::LogicalOr => "||=",
                _ => panic!("Invalid assignment operator")
            }
        });
    }
}

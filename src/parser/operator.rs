use std::fmt;

#[derive(Clone)]
pub enum Operator {
    // Infix
    Plus,
    Minus,
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
    Ternary,
    Assign(Option<Box<Operator>>),

    // Prefix
    Negative,
    LogicalNot,
    BitNot,

    // Postfix
    MemberAccess,

    // Ambiguous
    Parenthesis
}

impl Operator {
    pub fn assign(op: Operator) -> Operator {
        return Operator::Assign(Some(Box::new(op)));
    }

    pub fn infix_precedence(&self) -> Option<u8> {
        let bp = match self {
            Operator::Assign(_) => 1,
            Operator::Ternary => 2,
            Operator::LogicalOr => 3,
            Operator::LogicalAnd => 4,
            Operator::LogicalEqual | Operator::LogicalNotEqual => 5,
            Operator::BitOr => 6,
            Operator::BitXor => 7,
            Operator::BitAnd => 8,
            Operator::LessThan | Operator::LessThanEqual |
            Operator::GreaterThan | Operator::GreaterThanEqual => 9,
            Operator::LeftShift | Operator::RightShift => 10,
            Operator::Plus | Operator::Minus => 11,
            Operator::Multiply | Operator::Divide => 12,
            Operator::MemberAccess => 16,
            _ => return None
        };
        Some(bp)
    }

    pub fn prefix_precedence(&self) -> Option<u8> {
        let bp = match self {
            Operator::Negative | Operator::LogicalNot | Operator::BitNot => 13,
            Operator::Parenthesis => 16,
            _ => return None
        };
        Some(bp)
    }

    pub fn postfix_precedence(&self) -> Option<u8> {
        let bp = match self {
            Operator::Parenthesis => 14,
            _ => return None
        };
        Some(bp)
    }

    pub fn is_left_associative(&self) -> bool {
        return match self {
            Operator::Assign(_) => false,
            Operator::Ternary => false,
            Operator::Negative => false,
            Operator::LogicalNot => false,
            Operator::BitNot => false,
            _ => true
        };
    }
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        return write!(f, "{}", match self {
            Operator::Plus => "+",
            Operator::Minus => "-",
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
            Operator::Ternary => "?:",
            Operator::Negative => "-",
            Operator::LogicalNot => "!",
            Operator::BitNot => "~",
            Operator::Parenthesis => "()",
            Operator::MemberAccess => ".",
            Operator::Assign(None) => "=",
            Operator::Assign(Some(op)) => match **op {
                Operator::Plus => "+=",
                Operator::Minus => "-=",
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

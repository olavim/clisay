use std::fmt;

#[derive(Clone)]
pub enum BinaryOperator {
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
    Assign(Option<Box<BinaryOperator>>)
}

impl BinaryOperator {
    pub fn assign(op: BinaryOperator) -> BinaryOperator {
        return BinaryOperator::Assign(Some(Box::new(op)));
    }

    pub fn precedence(&self) -> u8 {
        return match self {
            BinaryOperator::Assign(_) => 1,
            BinaryOperator::Ternary => 2,
            BinaryOperator::LogicalOr => 3,
            BinaryOperator::LogicalAnd => 4,

            BinaryOperator::LogicalEqual => 5,
            BinaryOperator::LogicalNotEqual => 5,

            BinaryOperator::BitOr => 6,
            BinaryOperator::BitXor => 7,
            BinaryOperator::BitAnd => 8,

            BinaryOperator::LessThan => 9,
            BinaryOperator::LessThanEqual => 9,
            BinaryOperator::GreaterThan => 9,
            BinaryOperator::GreaterThanEqual => 9,

            BinaryOperator::LeftShift => 10,
            BinaryOperator::RightShift => 10,

            BinaryOperator::Plus => 11,
            BinaryOperator::Minus => 11,

            BinaryOperator::Multiply => 12,
            BinaryOperator::Divide => 12,
        };
    }

    pub fn is_left_associative(&self) -> bool {
        return match self {
            BinaryOperator::Assign(_) => false,
            _ => true
        };
    }
}

impl fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        return write!(f, "{}", match self {
            BinaryOperator::Plus => "+",
            BinaryOperator::Minus => "-",
            BinaryOperator::Multiply => "*",
            BinaryOperator::Divide => "/",
            BinaryOperator::LeftShift => "<<",
            BinaryOperator::RightShift => ">>",
            BinaryOperator::LessThan => "<",
            BinaryOperator::LessThanEqual => "<=",
            BinaryOperator::GreaterThan => ">",
            BinaryOperator::GreaterThanEqual => ">=",
            BinaryOperator::LogicalEqual => "==",
            BinaryOperator::LogicalNotEqual => "!=",
            BinaryOperator::LogicalAnd => "&&",
            BinaryOperator::LogicalOr => "||",
            BinaryOperator::BitAnd => "&",
            BinaryOperator::BitOr => "|",
            BinaryOperator::BitXor => "^",
            BinaryOperator::Ternary => "?:",
            BinaryOperator::Assign(None) => "=",
            BinaryOperator::Assign(Some(op)) => match **op {
                BinaryOperator::Plus => "+=",
                BinaryOperator::Minus => "-=",
                BinaryOperator::LeftShift => "<<=",
                BinaryOperator::RightShift => ">>=",
                BinaryOperator::Multiply => "*=",
                BinaryOperator::Divide => "/=",
                BinaryOperator::BitAnd => "&=",
                BinaryOperator::BitOr => "|=",
                BinaryOperator::BitXor => "^=",
                BinaryOperator::LogicalAnd => "&&=",
                BinaryOperator::LogicalOr => "||=",
                _ => panic!("Invalid assignment operator")
            }
        });
    }
}

#[derive(Clone)]
pub enum UnaryOperator {
    Negative,
    LogicalNot,
    BitNot
}

impl fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        return write!(f, "{}", match self {
            UnaryOperator::Negative => "-",
            UnaryOperator::LogicalNot => "!",
            UnaryOperator::BitNot => "~"
        });
    }
}

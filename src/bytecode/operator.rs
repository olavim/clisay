use std::fmt;

use crate::lexer::{TokenStream, TokenType};

use super::OpCode;

#[derive(Clone)]
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
    Ternary,
    MemberAccess,
    Assign(Option<Box<Operator>>),

    // Prefix
    Negate,
    LogicalNot,
    BitNot,
    Group,

    // Postfix
    Call
}

impl Operator {
    pub fn from_opcode(op: OpCode) -> Operator {
        return match op {
            OpCode::Add => Operator::Add,
            OpCode::Subtract => Operator::Subtract,
            OpCode::Multiply => Operator::Multiply,
            OpCode::Divide => Operator::Divide,
            OpCode::Negate => Operator::Negate,
            OpCode::Equal => Operator::LogicalEqual,
            OpCode::NotEqual => Operator::LogicalNotEqual,
            OpCode::LessThan => Operator::LessThan,
            OpCode::LessThanEqual => Operator::LessThanEqual,
            OpCode::GreaterThan => Operator::GreaterThan,
            OpCode::GreaterThanEqual => Operator::GreaterThanEqual,
            OpCode::Not => Operator::LogicalNot,
            _ => panic!("Invalid opcode")
        };
    }

    pub fn assign(op: Operator) -> Operator {
        return Operator::Assign(Some(Box::new(op)));
    }

    /// Parses a prefix operator on top of the token stream.
    /// The token stream is advanced only if a valid prefix operator is found.
    /// 
    /// ## Arguments
    /// * `stream` - The token stream to parse
    /// * `min_precedence` - The minimum precedence of the operator to parse
    pub fn parse_prefix(stream: &mut TokenStream, min_precedence: u8) -> Option<Operator> {
        let op = match &stream.peek(0).kind {
            TokenType::LeftParen => Operator::Group,
            TokenType::Minus => Operator::Negate,
            TokenType::Exclamation => Operator::LogicalNot,
            TokenType::Tilde => Operator::BitNot,
            _ => return None
        };
    
        if op.has_lower_prefix_precedence(min_precedence) {
            return None;
        }
    
        stream.next();
        return Some(op.clone());
    }
    
    /// Parses an infix operator on top of the token stream.
    /// The token stream is advanced only if a valid infix operator is found.
    /// 
    /// ## Arguments
    /// * `stream` - The token stream to parse
    /// * `min_precedence` - The minimum precedence of the operator to parse
    pub fn parse_infix(stream: &mut TokenStream, min_precedence: u8) -> Option<Operator> {
        let mut current_op: Option<Operator> = None;
        let mut peek = 0;

        let op = loop {
            current_op = match (&current_op, &stream.peek(peek).kind) {
                (None, TokenType::LessThan) => Some(Operator::LessThan),
                (None, TokenType::GreaterThan) => Some(Operator::GreaterThan),
                (None, TokenType::Plus) => Some(Operator::Add),
                (None, TokenType::Minus) => Some(Operator::Subtract),
                (None, TokenType::Multiply) => Some(Operator::Multiply),
                (None, TokenType::Divide) => Some(Operator::Divide),
                (None, TokenType::Amp) => Some(Operator::BitAnd),
                (None, TokenType::Pipe) => Some(Operator::BitOr),
                (None, TokenType::Hat) => Some(Operator::BitXor),
                (None, TokenType::Equal) => Some(Operator::Assign(None)),
                (None, TokenType::Exclamation) => Some(Operator::LogicalNot),
                (None, TokenType::Question) => Some(Operator::Ternary),
                (None, TokenType::Dot) => Some(Operator::MemberAccess),
        
                (Some(Operator::LessThan), TokenType::LessThan) => Some(Operator::LeftShift),
                (Some(Operator::GreaterThan), TokenType::GreaterThan) => Some(Operator::RightShift),
                (Some(Operator::BitAnd), TokenType::Amp) => Some(Operator::LogicalAnd),
                (Some(Operator::BitOr), TokenType::Pipe) => Some(Operator::LogicalOr),
                (Some(Operator::LogicalNot), TokenType::Equal) => Some(Operator::LogicalNotEqual),
        
                (Some(assign_op @ (
                    Operator::LogicalOr | Operator::LogicalAnd |
                    Operator::Add | Operator::Subtract |
                    Operator::Multiply | Operator::Divide |
                    Operator::BitAnd | Operator::BitOr | Operator::BitXor |
                    Operator::LeftShift | Operator::RightShift
                )), TokenType::Equal) => Some(Operator::assign(assign_op.clone())),
        
                (Some(Operator::Assign(None)), TokenType::Equal) => Some(Operator::LogicalEqual),
                (Some(Operator::LessThan), TokenType::Equal) => Some(Operator::LessThanEqual),
                (Some(Operator::GreaterThan), TokenType::Equal) => Some(Operator::GreaterThanEqual),
                (Some(op), _) if op.infix_precedence().is_some() => break op,
                (_, _) => return None
            };
            peek += 1;
        };
    
        if op.has_lower_infix_precedence(min_precedence) {
            return None;
        }
    
        stream.advance(peek);
        return Some(op.clone());
    }
    
    /// Parses a postfix operator on top of the token stream.
    /// The token stream is advanced only if a valid postfix operator is found.
    /// 
    /// ## Arguments
    /// * `stream` - The token stream to parse
    /// * `min_precedence` - The minimum precedence of the operator to parse
    pub fn parse_postfix(stream: &mut TokenStream, min_precedence: u8) -> Option<Operator> {
        let op = match &stream.peek(0).kind {
            TokenType::LeftParen => Operator::Call,
            _ => return None
        };
    
        if op.has_lower_postfix_precedence(min_precedence) {
            return None;
        }
    
        stream.next();
        return Some(op.clone());
    }

    fn has_lower_prefix_precedence(&self, precedence: u8) -> bool {
        let prec = self.prefix_precedence().unwrap();
        return prec < precedence || (self.is_left_associative() && prec == precedence);
    }

    fn has_lower_infix_precedence(&self, precedence: u8) -> bool {
        let prec = self.infix_precedence().unwrap();
        return prec < precedence || (self.is_left_associative() && prec == precedence);
    }

    fn has_lower_postfix_precedence(&self, precedence: u8) -> bool {
        let prec = self.postfix_precedence().unwrap();
        return prec < precedence || (self.is_left_associative() && prec == precedence);
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
            Operator::Add | Operator::Subtract => 11,
            Operator::Multiply | Operator::Divide => 12,
            Operator::MemberAccess => 16,
            _ => return None
        };
        Some(bp)
    }

    pub fn prefix_precedence(&self) -> Option<u8> {
        let bp = match self {
            Operator::Negate | Operator::LogicalNot | Operator::BitNot => 13,
            Operator::Group => 16,
            _ => return None
        };
        Some(bp)
    }

    pub fn postfix_precedence(&self) -> Option<u8> {
        let bp = match self {
            Operator::Call => 16,
            _ => return None
        };
        Some(bp)
    }

    fn is_left_associative(&self) -> bool {
        return match self {
            Operator::Assign(_) => false,
            Operator::Ternary => false,
            Operator::Negate => false,
            Operator::LogicalNot => false,
            Operator::BitNot => false,
            Operator::Group => false,
            _ => true
        };
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
            Operator::Ternary => "?:",
            Operator::Negate => "-",
            Operator::LogicalNot => "!",
            Operator::BitNot => "~",
            Operator::Group => "<group>",
            Operator::Call => "<call>",
            Operator::MemberAccess => ".",
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

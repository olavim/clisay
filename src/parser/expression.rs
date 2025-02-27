use std::fmt;

use crate::lexer::SourcePosition;

use super::{BinaryOperator, UnaryOperator};

#[derive(Clone)]
pub struct ASTExpression {
    pub kind: ASTExpressionKind,
    pub pos: SourcePosition
}

impl ASTExpression {
    pub fn new(kind: ASTExpressionKind, pos: SourcePosition) -> ASTExpression {
        return ASTExpression { kind, pos };
    }
}

#[derive(Clone)]
pub enum ASTExpressionKind {
    Ternary(Box<ASTExpression>, Box<ASTExpression>, Box<ASTExpression>),
    Binary(BinaryOperator, Box<ASTExpression>, Box<ASTExpression>),
    Unary(UnaryOperator, Box<ASTExpression>),
    Call(Box<ASTExpression>, Vec<ASTExpression>),
    MemberAccess(Box<ASTExpression>, String),
    Identifier(String),
    This,
    Number(f64),
    String(String),
    Boolean(bool)
}

impl fmt::Display for ASTExpressionKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        return write!(f, "{}", match self {
            ASTExpressionKind::Ternary(cond, left, right) => format!("{} ? {} : {}", cond.kind, left.kind, right.kind),
            ASTExpressionKind::Binary(op, left, right) => format!("({} {} {})", left.kind, op, right.kind),
            ASTExpressionKind::Unary(op, expr) => format!("{}{}", op, expr.kind),
            ASTExpressionKind::Call(name, args) => {
                let mut result = format!("{}(", name.kind);
                for (i, arg) in args.iter().enumerate() {
                    result.push_str(&format!("{}", arg.kind));
                    if i < args.len() - 1 {
                        result.push_str(", ");
                    }
                }
                result.push_str(")");
                result
            },
            ASTExpressionKind::MemberAccess(expr, member) => format!("{}.{}", expr.kind, member),
            ASTExpressionKind::Identifier(id) => id.to_string(),
            ASTExpressionKind::This => String::from("this"),
            ASTExpressionKind::Number(num) => num.to_string(),
            ASTExpressionKind::String(s) => s.to_string(),
            ASTExpressionKind::Boolean(b) => b.to_string()
        });
    }
}

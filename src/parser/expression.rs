use std::fmt;

use anyhow::bail;

use crate::lexer::{SourcePosition, TokenType};

use super::{token_stream::TokenStream, BinaryOperator, ParseResult, UnaryOperator};

#[derive(Clone)]
pub enum ASTExpressionKind {
    Ternary(Box<ASTExpression>, Box<ASTExpression>, Box<ASTExpression>),
    Binary(BinaryOperator, Box<ASTExpression>, Box<ASTExpression>),
    Unary(UnaryOperator, Box<ASTExpression>),
    Call(Box<ASTExpression>, Vec<ASTExpression>),
    MemberAccess(Box<ASTExpression>, String),
    Identifier(String),
    This,
    Super(Box<ASTExpression>),
    SuperCall(Vec<ASTExpression>),
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
            ASTExpressionKind::Super(_) => String::from("super"),
            ASTExpressionKind::SuperCall(_) => String::from("super()"),
            ASTExpressionKind::This => String::from("this"),
            ASTExpressionKind::Number(num) => num.to_string(),
            ASTExpressionKind::String(s) => s.to_string(),
            ASTExpressionKind::Boolean(b) => b.to_string()
        });
    }
}

#[derive(Clone)]
pub struct ASTExpression {
    pub kind: ASTExpressionKind,
    pub pos: SourcePosition
}

impl ASTExpression {
    pub fn new(kind: ASTExpressionKind, pos: SourcePosition) -> ASTExpression {
        return ASTExpression { kind, pos };
    }

    pub fn parse(stream: &mut TokenStream) -> ParseResult<ASTExpression> {
        return parse_expression(stream, 0);
    }
}

fn next_binary_operator(stream: &mut TokenStream, prev_op: Option<BinaryOperator>) -> ParseResult<BinaryOperator> {
    let token = stream.next()?.clone();
    let res = match (prev_op, &token.kind) {
        (None, TokenType::LessThan) => next_binary_operator(stream, Some(BinaryOperator::LessThan)),
        (None, TokenType::GreaterThan) => next_binary_operator(stream, Some(BinaryOperator::GreaterThan)),
        (None, TokenType::Plus) => next_binary_operator(stream, Some(BinaryOperator::Plus)),
        (None, TokenType::Minus) => next_binary_operator(stream, Some(BinaryOperator::Minus)),
        (None, TokenType::Multiply) => next_binary_operator(stream, Some(BinaryOperator::Multiply)),
        (None, TokenType::Divide) => next_binary_operator(stream, Some(BinaryOperator::Divide)),
        (None, TokenType::Amp) => next_binary_operator(stream, Some(BinaryOperator::BitAnd)),
        (None, TokenType::Pipe) => next_binary_operator(stream, Some(BinaryOperator::BitOr)),
        (None, TokenType::Hat) => next_binary_operator(stream, Some(BinaryOperator::BitXor)),
        (None, TokenType::Equal) => next_binary_operator(stream, Some(BinaryOperator::Assign(None))),
        (None, TokenType::Exclamation) => {
            stream.expect(TokenType::Equal)?;
            Ok(BinaryOperator::LogicalNotEqual)
        },
        (None, TokenType::Question) => Ok(BinaryOperator::Ternary),

        (Some(BinaryOperator::LessThan), TokenType::LessThan) => next_binary_operator(stream, Some(BinaryOperator::LeftShift)),
        (Some(BinaryOperator::GreaterThan), TokenType::GreaterThan) => next_binary_operator(stream, Some(BinaryOperator::RightShift)),
        (Some(BinaryOperator::BitAnd), TokenType::Amp) => next_binary_operator(stream, Some(BinaryOperator::LogicalAnd)),
        (Some(BinaryOperator::BitOr), TokenType::Pipe) => next_binary_operator(stream, Some(BinaryOperator::LogicalOr)),
        
        (Some(BinaryOperator::LogicalOr), TokenType::Equal) => Ok(BinaryOperator::assign(BinaryOperator::LogicalOr)),
        (Some(BinaryOperator::LogicalAnd), TokenType::Equal) => Ok(BinaryOperator::assign(BinaryOperator::LogicalAnd)),
        (Some(BinaryOperator::Plus), TokenType::Equal) => Ok(BinaryOperator::assign(BinaryOperator::Plus)),
        (Some(BinaryOperator::Minus), TokenType::Equal) => Ok(BinaryOperator::assign(BinaryOperator::Minus)),
        (Some(BinaryOperator::Multiply), TokenType::Equal) => Ok(BinaryOperator::assign(BinaryOperator::Multiply)),
        (Some(BinaryOperator::Divide), TokenType::Equal) => Ok(BinaryOperator::assign(BinaryOperator::Divide)),
        (Some(BinaryOperator::BitAnd), TokenType::Equal) => Ok(BinaryOperator::assign(BinaryOperator::BitAnd)),
        (Some(BinaryOperator::BitOr), TokenType::Equal) => Ok(BinaryOperator::assign(BinaryOperator::BitOr)),
        (Some(BinaryOperator::BitXor), TokenType::Equal) => Ok(BinaryOperator::assign(BinaryOperator::BitXor)),
        (Some(BinaryOperator::LeftShift), TokenType::Equal) => Ok(BinaryOperator::assign(BinaryOperator::LeftShift)),
        (Some(BinaryOperator::RightShift), TokenType::Equal) => Ok(BinaryOperator::assign(BinaryOperator::RightShift)),
        (Some(BinaryOperator::Assign(None)), TokenType::Equal) => Ok(BinaryOperator::LogicalEqual),
        (Some(BinaryOperator::LessThan), TokenType::Equal) => Ok(BinaryOperator::LessThanEqual),
        (Some(BinaryOperator::GreaterThan), TokenType::Equal) => Ok(BinaryOperator::GreaterThanEqual),
        (Some(op), _) => {
            stream.back();
            Ok(op)
        },
        _ => bail!("Unexpected token {}", token)
    };

    return res;
}

fn peek_binary_operator(stream: &mut TokenStream, look_ahead: usize) -> ParseResult<BinaryOperator> {
    stream.save_position();
    if look_ahead == 1 {
        stream.next()?;
    }
    let res = next_binary_operator(stream, None);
    stream.restore_position();
    return res;
}

// A half-baked Pratt parser that handles binary and ternary operators like a pratt parser,
// but declines back to a recursive descent parser for unary and postfix operators.
fn parse_expression(stream: &mut TokenStream, parent_precedence: u8) -> ParseResult<ASTExpression> {
    let mut left = parse_unary_expression(stream)?;
    let pos = left.pos.clone();

    while let Ok(op) = peek_binary_operator(stream, 0) {
        if op.precedence() < parent_precedence || (op.is_left_associative() && op.precedence() == parent_precedence) {
            break;
        }
        next_binary_operator(stream, None)?;

        // Special handling for ternary operator
        if let BinaryOperator::Ternary = op {
            let then = parse_expression(stream, 0)?;
            stream.expect(TokenType::Colon)?;
            let otherwise = parse_expression(stream, 0)?;
            let kind = ASTExpressionKind::Ternary(Box::from(left), Box::from(then), Box::from(otherwise));
            left = ASTExpression::new(kind, pos.clone());
            continue;
        }

        let mut right = parse_expression(stream, op.precedence())?;

        // Normalize compound assignment, for example convert (a += 1) to (a = a + 1)
        if let BinaryOperator::Assign(Some(assign_op)) = &op {
            let kind = ASTExpressionKind::Binary(assign_op.as_ref().clone(), Box::from(left.clone()), Box::from(right));
            right = ASTExpression::new(kind, pos.clone());
        }

        let kind = ASTExpressionKind::Binary(op, Box::from(left), Box::from(right));
        left = ASTExpression::new(kind, pos.clone());
    }

    return Ok(left);
}

fn parse_unary_expression(stream: &mut TokenStream) -> ParseResult<ASTExpression> {
    let next = stream.peek(0)?.clone();
    let pos = next.pos.clone();

    let unary_op = match next.kind {
        TokenType::Minus => UnaryOperator::Negative,
        TokenType::Exclamation => UnaryOperator::LogicalNot,
        TokenType::Tilde => UnaryOperator::BitNot,
        _ => return parse_postfix_expression(stream)
    };

    let kind = ASTExpressionKind::Unary(unary_op, Box::from(parse_unary_expression(stream)?));
    return Ok(ASTExpression::new(kind, pos));
}

fn parse_postfix_expression(stream: &mut TokenStream) -> ParseResult<ASTExpression> {
    let mut expr = parse_primary_expression(stream)?;

    loop {
        expr = match stream.peek(0)?.kind {
            TokenType::LeftParenthesis => parse_postfix_call_expression(stream, expr)?,
            TokenType::Dot => parse_postfix_access_expression(stream, expr)?,
            _ => break
        }
    }

    return Ok(expr);
}

fn parse_postfix_call_expression(stream: &mut TokenStream, expr: ASTExpression) -> ParseResult<ASTExpression> {
    let pos = stream.peek(0)?.pos.clone();
    let args = parse_call_args(stream)?;
    return Ok(ASTExpression::new(ASTExpressionKind::Call(Box::from(expr), args), pos));
}

fn parse_call_args(stream: &mut TokenStream) -> ParseResult<Vec<ASTExpression>> {
    stream.expect(TokenType::LeftParenthesis)?;

    let mut args: Vec<ASTExpression> = Vec::new();
    while !matches!(&stream.peek_type(0)?, TokenType::RightParenthesis | TokenType::EOF) {
        args.push(parse_expression(stream, 0)?);

        if *stream.peek_type(0)? != TokenType::RightParenthesis {
            stream.expect(TokenType::Comma)?;
        }
    }

    stream.expect(TokenType::RightParenthesis)?;
    return Ok(args);
}

fn parse_postfix_access_expression(stream: &mut TokenStream, expr: ASTExpression) -> ParseResult<ASTExpression> {
    let pos = stream.expect(TokenType::Dot)?.pos.clone();
    let member = stream.expect(TokenType::Identifier)?.lexeme.clone();
    return Ok(ASTExpression::new(ASTExpressionKind::MemberAccess(Box::from(expr), member), pos));
}

fn parse_primary_expression(stream: &mut TokenStream) -> ParseResult<ASTExpression> {
    let token = stream.peek(0)?.clone();
    let pos = token.pos.clone();

    let expr: ASTExpression = match token.kind {
        TokenType::LeftParenthesis => {
            stream.next()?;
            let expr = parse_expression(stream, 0)?;
            stream.expect(TokenType::RightParenthesis)?;
            expr
        },
        TokenType::StringLiteral => {
            let val = stream.next()?.lexeme.clone();
            // Strip quotes
            let kind = ASTExpressionKind::String(String::from(&val[1..val.len() - 1]));
            ASTExpression::new(kind, pos)
        },
        TokenType::NumericLiteral => {
            let kind = ASTExpressionKind::Number(stream.next()?.lexeme.parse().unwrap());
            ASTExpression::new(kind, pos)
        },
        TokenType::True => {
            stream.next()?;
            let kind = ASTExpressionKind::Boolean(true);
            ASTExpression::new(kind, pos)
        },
        TokenType::False => {
            stream.next()?;
            let kind = ASTExpressionKind::Boolean(false);
            ASTExpression::new(kind, pos)
        },
        TokenType::This => {
            stream.next()?;
            let kind = ASTExpressionKind::This;
            ASTExpression::new(kind, pos)
        },
        TokenType::Identifier => {
            let kind = ASTExpressionKind::Identifier(stream.next()?.lexeme.parse().unwrap());
            ASTExpression::new(kind, pos)
        },
        _ => bail!("Unexpected token {} at {}", token, token.pos)
    };

    return Ok(expr);
}

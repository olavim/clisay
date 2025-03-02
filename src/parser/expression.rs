use std::fmt;

use anyhow::bail;

use crate::lexer::{SourcePosition, TokenType};

use super::{token_stream::TokenStream, Operator, ParseResult};

#[derive(Clone)]
pub enum ASTExpressionKind {
    Ternary(Box<ASTExpression>, Box<ASTExpression>, Box<ASTExpression>),
    Binary(Operator, Box<ASTExpression>, Box<ASTExpression>),
    Unary(Operator, Box<ASTExpression>),
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

fn next_operator(stream: &mut TokenStream, prev_op: Option<Operator>) -> ParseResult<Operator> {
    let token = stream.next()?.clone();
    let res = match (prev_op, &token.kind) {
        (None, TokenType::LessThan) => next_operator(stream, Some(Operator::LessThan)),
        (None, TokenType::GreaterThan) => next_operator(stream, Some(Operator::GreaterThan)),
        (None, TokenType::Plus) => next_operator(stream, Some(Operator::Plus)),
        (None, TokenType::Minus) => next_operator(stream, Some(Operator::Minus)),
        (None, TokenType::Multiply) => next_operator(stream, Some(Operator::Multiply)),
        (None, TokenType::Divide) => next_operator(stream, Some(Operator::Divide)),
        (None, TokenType::Amp) => next_operator(stream, Some(Operator::BitAnd)),
        (None, TokenType::Pipe) => next_operator(stream, Some(Operator::BitOr)),
        (None, TokenType::Hat) => next_operator(stream, Some(Operator::BitXor)),
        (None, TokenType::Equal) => next_operator(stream, Some(Operator::Assign(None))),
        (None, TokenType::Exclamation) => next_operator(stream, Some(Operator::LogicalNot)),
        (None, TokenType::Tilde) => Ok(Operator::BitNot),
        (None, TokenType::Question) => Ok(Operator::Ternary),
        (None, TokenType::LeftParenthesis) => Ok(Operator::Parenthesis),
        (None, TokenType::Dot) => Ok(Operator::MemberAccess),

        (Some(Operator::LessThan), TokenType::LessThan) => next_operator(stream, Some(Operator::LeftShift)),
        (Some(Operator::GreaterThan), TokenType::GreaterThan) => next_operator(stream, Some(Operator::RightShift)),
        (Some(Operator::BitAnd), TokenType::Amp) => next_operator(stream, Some(Operator::LogicalAnd)),
        (Some(Operator::BitOr), TokenType::Pipe) => next_operator(stream, Some(Operator::LogicalOr)),
        (Some(Operator::LogicalNot), TokenType::Equal) => Ok(Operator::LogicalNotEqual),

        (Some(op @ (
            Operator::LogicalOr | Operator::LogicalAnd |
            Operator::Plus | Operator::Minus |
            Operator::Multiply | Operator::Divide |
            Operator::BitAnd | Operator::BitOr | Operator::BitXor |
            Operator::LeftShift | Operator::RightShift
        )), TokenType::Equal) => Ok(Operator::assign(op)),

        (Some(Operator::Assign(None)), TokenType::Equal) => Ok(Operator::LogicalEqual),
        (Some(Operator::LessThan), TokenType::Equal) => Ok(Operator::LessThanEqual),
        (Some(Operator::GreaterThan), TokenType::Equal) => Ok(Operator::GreaterThanEqual),
        (Some(op), _) => {
            stream.back();
            Ok(op)
        },
        _ => bail!("Unexpected token {}", token)
    };

    return res;
}

fn peek_operator(stream: &mut TokenStream, look_ahead: usize) -> ParseResult<Operator> {
    stream.save_position();
    if look_ahead == 1 {
        stream.next()?;
    }
    let res = next_operator(stream, None);
    stream.restore_position();
    return res;
}

// Pratt parser
fn parse_expression(stream: &mut TokenStream, min_precedence: u8) -> ParseResult<ASTExpression> {
    let mut left = match peek_operator(stream, 0) {
        Ok(op) => match op.prefix_precedence() {
            Some(_) => parse_prefix_expression(stream)?,
            _ => bail!("Unexpected operator {}", op)
        },
        _ => parse_primary_expression(stream)?
    };

    while let Ok(op) = peek_operator(stream, 0) {
        if let Some(prec) = op.postfix_precedence() {
            if prec < min_precedence || (op.is_left_associative() && prec == min_precedence) {
                break;
            }
            left = parse_postfix_expression(stream, left)?;
            continue;
        }

        if let Some(prec) = op.infix_precedence() {
            if prec < min_precedence || (op.is_left_associative() && prec == min_precedence) {
                break;
            }
            left = parse_infix_expression(stream, left)?;
            continue;
        }

        break;
    }

    return Ok(left);
}

fn parse_infix_expression(stream: &mut TokenStream, expr: ASTExpression) -> ParseResult<ASTExpression> {
    let pos = stream.peek(0)?.pos.clone();
    let op = next_operator(stream, None)?;

    let kind = match &op {
        Operator::Assign(Some(assign_op)) => {
            // Normalize compound assignment, for example convert (a += 1) to (a = a + 1)
            let mut right = parse_expression(stream, op.infix_precedence().unwrap())?;
            let kind = ASTExpressionKind::Binary(assign_op.as_ref().clone(), Box::from(expr.clone()), Box::from(right));
            right = ASTExpression::new(kind, pos.clone());
            ASTExpressionKind::Binary(op, Box::from(expr), Box::from(right))
        },
        Operator::MemberAccess => {
            let member = stream.expect(TokenType::Identifier)?.lexeme.clone();
            ASTExpressionKind::MemberAccess(Box::from(expr), member)
        },
        Operator::Ternary => {
            let left = parse_expression(stream, 0)?;
            stream.expect(TokenType::Colon)?;
            let right = parse_expression(stream, 0)?;
            ASTExpressionKind::Ternary(Box::from(expr), Box::from(left), Box::from(right))
        },
        _ => {
            let right = parse_expression(stream, op.infix_precedence().unwrap())?;
            ASTExpressionKind::Binary(op, Box::from(expr), Box::from(right))
        }
    };

    return Ok(ASTExpression::new(kind, pos.clone()));
}

fn parse_prefix_expression(stream: &mut TokenStream) -> ParseResult<ASTExpression> {
    let pos = stream.peek(0)?.pos.clone();
    let op = next_operator(stream, None)?;
    let kind = match &op {
        Operator::Parenthesis => {
            let expr = Box::from(parse_expression(stream, 0)?);
            stream.expect(TokenType::RightParenthesis)?;
            expr.kind
        },
        _ => {
            let right = Box::from(parse_expression(stream, op.prefix_precedence().unwrap())?);
            ASTExpressionKind::Unary(op, right)
        }
    };
    return Ok(ASTExpression::new(kind, pos));
}

fn parse_postfix_expression(stream: &mut TokenStream, expr: ASTExpression) -> ParseResult<ASTExpression> {
    let pos = stream.peek(0)?.pos.clone();
    let op = next_operator(stream, None)?;
    match op {
        Operator::Parenthesis => {
            let args = parse_call_args(stream)?;
            Ok(ASTExpression::new(ASTExpressionKind::Call(Box::from(expr), args), pos))
        },
        _ => unreachable!()
    }
}

fn parse_call_args(stream: &mut TokenStream) -> ParseResult<Vec<ASTExpression>> {
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

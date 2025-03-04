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
    Super,
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
            ASTExpressionKind::Super => String::from("super"),
            ASTExpressionKind::SuperCall(_) => String::from("super()"),
            ASTExpressionKind::This => String::from("this"),
            ASTExpressionKind::Number(num) => num.to_string(),
            ASTExpressionKind::String(s) => format!("\"{}\"", s),
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

// Pratt parser
fn parse_expression(stream: &mut TokenStream, min_precedence: u8) -> ParseResult<ASTExpression> {
    let mut left = match Operator::parse_prefix(stream, 0) {
        Some(op) => parse_prefix_expression(stream, op)?,
        _ => parse_atom(stream)?
    };

    loop {
        if let Some(op) = Operator::parse_postfix(stream, min_precedence) {
            left = parse_postfix_expression(stream, op, left)?;
        } else if let Ok(Some(op)) = Operator::parse_infix(stream, min_precedence) {
            left = parse_infix_expression(stream, op, left)?;
        } else {
            break;
        }
    }

    return Ok(left);
}

fn parse_infix_expression(stream: &mut TokenStream, op: Operator, expr: ASTExpression) -> ParseResult<ASTExpression> {
    let pos = expr.pos.clone();

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

fn parse_prefix_expression(stream: &mut TokenStream, op: Operator) -> ParseResult<ASTExpression> {
    let pos = stream.peek(0).pos.clone();
    let kind = match &op {
        Operator::Group => {
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

fn parse_postfix_expression(stream: &mut TokenStream, op: Operator, expr: ASTExpression) -> ParseResult<ASTExpression> {
    let pos = expr.pos.clone();
    match op {
        Operator::Call => {
            let args = parse_call_args(stream)?;
            Ok(ASTExpression::new(ASTExpressionKind::Call(Box::from(expr), args), pos))
        },
        _ => unreachable!()
    }
}

fn parse_call_args(stream: &mut TokenStream) -> ParseResult<Vec<ASTExpression>> {
    let mut args: Vec<ASTExpression> = Vec::new();
    
    while !stream.match_next(TokenType::RightParenthesis) {
        if args.len() > 0 {
            stream.expect(TokenType::Comma)?;
        }
        args.push(parse_expression(stream, 0)?);
    }

    stream.expect(TokenType::RightParenthesis)?;
    return Ok(args);
}

fn parse_atom(stream: &mut TokenStream) -> ParseResult<ASTExpression> {
    let token = stream.peek(0).clone();
    let pos = token.pos.clone();

    let expr: ASTExpression = match token.kind {
        TokenType::StringLiteral => {
            let val = stream.next().lexeme.clone();
            // Strip quotes
            let kind = ASTExpressionKind::String(String::from(&val[1..val.len() - 1]));
            ASTExpression::new(kind, pos)
        },
        TokenType::NumericLiteral => {
            let kind = ASTExpressionKind::Number(stream.next().lexeme.parse().unwrap());
            ASTExpression::new(kind, pos)
        },
        TokenType::True => {
            stream.next();
            let kind = ASTExpressionKind::Boolean(true);
            ASTExpression::new(kind, pos)
        },
        TokenType::False => {
            stream.next();
            let kind = ASTExpressionKind::Boolean(false);
            ASTExpression::new(kind, pos)
        },
        TokenType::This => {
            stream.next();
            let kind = ASTExpressionKind::This;
            ASTExpression::new(kind, pos)
        },
        TokenType::Super => {
            stream.next();
            let kind = ASTExpressionKind::Super;
            ASTExpression::new(kind, pos)
        },
        TokenType::Identifier => {
            let kind = ASTExpressionKind::Identifier(stream.next().lexeme.parse().unwrap());
            ASTExpression::new(kind, pos)
        },
        _ => bail!("Unexpected token {} at {}", token, token.pos)
    };

    return Ok(expr);
}

use std::collections::HashSet;
use std::fmt;

use anyhow::bail;

use crate::lexer::{SourcePosition, TokenStream, TokenType};
use crate::parser::ASTExpression;

use super::{ASTExpressionKind, Operator, ParseResult};

#[derive(Clone)]
pub struct ASTVariableDeclaration {
    pub name: String,
    pub value: Option<ASTExpression>
}

impl fmt::Display for ASTVariableDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        return write!(f, "{}", match &self.value {
            Some(value) => format!("{} = {}", self.name, value.kind),
            None => format!("{}", self.name)
        });
    }
}

#[derive(Clone)]
pub struct ASTFunctionDeclaration {
    pub name: String,
    pub params: Vec<String>,
    pub body: Box<ASTStatement>
}

#[derive(Clone)]
pub struct ASTClassDeclaration {
    pub name: String,
    pub superclass: Option<String>,
    pub init: ASTFunctionDeclaration,
    pub fields: Vec<String>,
    pub methods: Vec<ASTFunctionDeclaration>
}

#[derive(Clone)]
pub enum StatementKind {
    Block(Box<ASTStatement>),
    Compound(Vec<ASTStatement>),
    Expression(ASTExpression),
    Return(Option<ASTExpression>),
    Say(ASTVariableDeclaration),
    Fn(ASTFunctionDeclaration),
    If(ASTExpression, Box<ASTStatement>, Option<Box<ASTStatement>>),
    While(ASTExpression, Box<ASTStatement>),
    Class(ASTClassDeclaration)
}

#[derive(Clone)]
pub struct ASTStatement {
    pub kind: StatementKind,
    pub pos: SourcePosition
}

impl ASTStatement {
    pub fn new(kind: StatementKind, pos: SourcePosition) -> ASTStatement {
        return ASTStatement { kind, pos };
    }

    pub fn parse(stream: &mut TokenStream) -> ParseResult<ASTStatement> {
        return parse_compound_statement(stream);
    }
}

fn parse_statement(stream: &mut TokenStream) -> ParseResult<ASTStatement> {
    let statement = match stream.peek(0).kind {
        TokenType::Return => parse_return_statement(stream)?,
        TokenType::Say => parse_say_statement(stream)?,
        TokenType::Fn => parse_fn_statement(stream)?,
        TokenType::If => parse_if_statement(stream)?,
        TokenType::While => parse_while_statement(stream)?,
        TokenType::Class => parse_class_statement(stream)?,
        TokenType::LeftBrace => parse_block_statement(stream)?,
        _ => parse_expression_statement(stream)?
    };

    return Ok(statement);
}

fn parse_block_statement(stream: &mut TokenStream) -> ParseResult<ASTStatement> {
    let pos = stream.peek(0).pos.clone();
    let curlies = stream.next_if(TokenType::LeftBrace).is_some();
    let stmt = parse_compound_statement(stream)?;
    if curlies {
        stream.expect(TokenType::RightBrace)?;
    }
    return Ok(ASTStatement::new(StatementKind::Block(Box::new(stmt)), pos));
}

fn parse_compound_statement(stream: &mut TokenStream) -> ParseResult<ASTStatement> {
    let pos = stream.peek(0).pos.clone();
    let mut statements: Vec<ASTStatement> = Vec::new();
    while !matches!(&stream.peek(0).kind, TokenType::RightBrace | TokenType::EOF) {
        statements.push(parse_statement(stream)?);
    }
    return Ok(ASTStatement::new(StatementKind::Compound(statements), pos));
}

fn parse_return_statement(stream: &mut TokenStream) -> ParseResult<ASTStatement> {
    let pos = stream.expect(TokenType::Return)?.pos.clone();

    if let Some(_) = stream.next_if(TokenType::Semicolon) {
        return Ok(ASTStatement::new(StatementKind::Return(None), pos));
    }

    let expr = ASTExpression::parse(stream)?;
    stream.expect(TokenType::Semicolon)?;
    return Ok(ASTStatement::new(StatementKind::Return(Some(expr)), pos));
}

fn parse_say_statement(stream: &mut TokenStream) -> ParseResult<ASTStatement> {
    let pos = stream.expect(TokenType::Say)?.pos.clone();
    let name = stream.expect(TokenType::Identifier)?.lexeme.clone();

    let mut value: Option<ASTExpression> = None;
    if let Some(_) = stream.next_if(TokenType::Equal) {
        value = Some(ASTExpression::parse(stream)?);
    }

    stream.expect(TokenType::Semicolon)?;
    return Ok(ASTStatement::new(StatementKind::Say(ASTVariableDeclaration { name, value }), pos));
}

fn parse_fn_statement(stream: &mut TokenStream) -> ParseResult<ASTStatement> {
    let pos = stream.expect(TokenType::Fn)?.pos.clone();
    let name = stream.expect(TokenType::Identifier)?.lexeme.clone();
    let parameters = parse_callable_params(stream)?;
    let body = Box::new(parse_callable_decl_body(stream)?);
    let decl = ASTFunctionDeclaration { name, params: parameters, body };
    return Ok(ASTStatement::new(StatementKind::Fn(decl), pos));
}

fn parse_callable_params(stream: &mut TokenStream) -> ParseResult<Vec<String>> {
    stream.expect(TokenType::LeftParen)?;

    let mut seen_params: HashSet<String> = HashSet::new();
    let mut params = Vec::new();

    while stream.match_next(TokenType::Identifier) {
        let name = String::from(stream.next().lexeme.clone());
        if seen_params.contains(&name) {
            bail!("Duplicate parameter declaration: {}", name);
        }
        seen_params.insert(name.clone());
        params.push(name.clone());
        stream.next_if(TokenType::Comma);
    }

    stream.expect(TokenType::RightParen)?;
    return Ok(params);
}

fn parse_callable_decl_body(stream: &mut TokenStream) -> ParseResult<ASTStatement> {
    stream.expect(TokenType::LeftBrace)?;
    let body = parse_compound_statement(stream)?;
    stream.expect(TokenType::RightBrace)?;
    return Ok(body);
}

fn parse_class_statement(stream: &mut TokenStream) -> ParseResult<ASTStatement> {
    let pos = stream.expect(TokenType::Class)?.pos.clone();
    let name = stream.expect(TokenType::Identifier)?.lexeme.clone();
    let superclass = match stream.peek(0).kind {
        TokenType::Colon => {
            stream.next();
            Some(stream.expect(TokenType::Identifier)?.lexeme.clone())
        },
        _ => None
    };

    stream.expect(TokenType::LeftBrace)?;

    let mut members: HashSet<String> = HashSet::new();
    let mut fields: Vec<ASTVariableDeclaration> = Vec::new();
    let mut methods: Vec<ASTFunctionDeclaration> = Vec::new();

    let mut init_seen = false;
    let mut init_supercall: Option<ASTStatement> = None;
    let mut init_parameters: Vec<String> = Vec::new();
    let mut init_body: Option<ASTStatement> = None;

    while !stream.match_next(TokenType::RightBrace) {
        let token = stream.peek(0);
        let pos = token.pos.clone();

        match token.kind {
            TokenType::Identifier => {
                let name = stream.next().lexeme.clone();
                let mut value: Option<ASTExpression> = None;
                if let Some(_) = stream.next_if(TokenType::Equal) {
                    value = Some(ASTExpression::parse(stream)?);
                }
                stream.expect(TokenType::Semicolon)?;

                if members.contains(&name) {
                    bail!("Duplicate member declaration: {} at {}", name, pos.clone());
                }

                members.insert(name.clone());
                fields.push(ASTVariableDeclaration { name, value });
            },
            TokenType::Init => {
                if init_seen {
                    bail!("Duplicate init declaration at {}", pos.clone());
                }

                init_seen = true;

                stream.expect(TokenType::Init)?;
                init_parameters = parse_callable_params(stream)?;
                stream.expect(TokenType::LeftBrace)?;
        
                if let Some(token) = stream.next_if(TokenType::Super) {
                    let pos = token.pos.clone();
                    let args = parse_call_args(stream)?;
                    let expr = ASTExpression::new(ASTExpressionKind::SuperCall(args), pos.clone());
                    stream.expect(TokenType::Semicolon)?;
                    init_supercall = Some(ASTStatement::new(StatementKind::Expression(expr), pos));
                }
        
                init_body = Some(parse_compound_statement(stream)?);
                stream.expect(TokenType::RightBrace)?;
            },
            TokenType::Fn => {
                let StatementKind::Fn(fn_decl) = parse_fn_statement(stream)?.kind else { unreachable!() };
                if members.contains(&fn_decl.name) {
                    bail!("Duplicate member declaration: {}", name);
                }

                members.insert(fn_decl.name.clone());
                methods.push(fn_decl)
            },
            _ => bail!("Unexpected token {} at {}", token, pos.clone())
        };
    }

    stream.expect(TokenType::RightBrace)?;

    let mut init_stmts: Vec<ASTStatement> = Vec::new();

    if let Some(supercall) = &init_supercall {
        init_stmts.push(supercall.clone());
    } else if superclass.is_some() {
        let expr = ASTExpression::new(ASTExpressionKind::SuperCall(Vec::new()), pos.clone());
        init_stmts.push(ASTStatement::new(StatementKind::Expression(expr), pos.clone()));
    }

    for field in &fields {
        if field.value.is_some() {
            let pos = pos.clone();
            let this = ASTExpression::new(ASTExpressionKind::This, pos.clone());
            let identifier = ASTExpression::new(ASTExpressionKind::MemberAccess(Box::new(this), field.name.clone()), pos.clone());
            let expr = ASTExpressionKind::Binary(Operator::Assign(None), Box::new(identifier), Box::new(field.value.clone().unwrap()));
            let stmt = StatementKind::Expression(ASTExpression::new(expr, pos.clone()));
            init_stmts.push(ASTStatement::new(stmt, pos.clone()));
        }
    }

    if let Some(init_body) = &init_body {
        init_stmts.push(init_body.clone());
    }

    let init = ASTFunctionDeclaration {
        name: format!("{}.init", name),
        params: init_parameters,
        body: Box::new(ASTStatement::new(StatementKind::Compound(init_stmts), pos.clone()))
    };

    let class_decl = ASTClassDeclaration { 
        name, 
        superclass, 
        init, 
        fields: fields.iter().map(|field| field.name.clone()).collect(), 
        methods 
    };
    return Ok(ASTStatement::new(StatementKind::Class(class_decl), pos));
}

fn parse_call_args(stream: &mut TokenStream) -> ParseResult<Vec<ASTExpression>> {
    stream.expect(TokenType::LeftParen)?;

    let mut args: Vec<ASTExpression> = Vec::new();
    while !stream.match_next(TokenType::RightParen) {
        if args.len() > 0 {
            stream.expect(TokenType::Comma)?;
        }
        args.push(ASTExpression::parse(stream)?);
    }

    stream.expect(TokenType::RightParen)?;
    return Ok(args);
}

fn parse_if_statement(stream: &mut TokenStream) -> ParseResult<ASTStatement> {
    let pos = stream.expect(TokenType::If)?.pos.clone();
    let condition = ASTExpression::parse(stream)?;
    let then = Box::new(parse_block_statement(stream)?);
    let otherwise = match stream.next_if(TokenType::Else) {
        Some(_) => Some(Box::new(parse_block_statement(stream)?)),
        _ => None
    };

    return Ok(ASTStatement::new(StatementKind::If(condition, then, otherwise), pos));
}

fn parse_while_statement(stream: &mut TokenStream) -> ParseResult<ASTStatement> {
    let pos = stream.expect(TokenType::While)?.pos.clone();
    let condition = ASTExpression::parse(stream)?;
    let body = Box::new(parse_block_statement(stream)?);
    return Ok(ASTStatement::new(StatementKind::While(condition, body), pos));
}

fn parse_expression_statement(stream: &mut TokenStream) -> ParseResult<ASTStatement> {
    let expr = ASTExpression::parse(stream)?;
    stream.expect(TokenType::Semicolon)?;
    return Ok(ASTStatement::new(StatementKind::Expression(expr.clone()), expr.pos));
}
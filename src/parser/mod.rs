mod operator;
mod ast;
mod statement;
mod expression;

use std::collections::HashSet;

use anyhow::bail;
pub use ast::AST;
pub use expression::ASTExpressionKind;
use statement::ClassDeclaration;
use statement::ClassInit;
pub use statement::ASTStatement;
pub use expression::ASTExpression;
pub use operator::BinaryOperator;
pub use operator::UnaryOperator;
pub use statement::StatementKind;
pub use statement::VariableDeclaration;
pub use statement::FunctionDeclaration;

use super::lexer::Token;
use super::lexer::TokenType;

type ParseResult<T> = Result<T, anyhow::Error>;

struct Parser<'a> {
    tokens: &'a Vec<Token>,
    pos: usize
}

impl Parser<'_> {
    fn new(tokens: &Vec<Token>) -> Parser {
        return Parser { tokens, pos: 0 };
    }

    fn peek(&self, look_ahead: usize) -> ParseResult<&Token> {
        if self.pos + look_ahead >= self.tokens.len() {
            bail!("Unexpected end of file");
        }

        return Ok(self.tokens.get(self.pos + look_ahead).unwrap());
    }

    fn peek_type(&self, look_ahead: usize) -> ParseResult<&TokenType> {
        return Ok(&self.peek(look_ahead)?.kind);
    }

    fn next(&mut self) -> ParseResult<&Token> {
        if self.pos >= self.tokens.len() {
            bail!("Unexpected end of file");
        }

        let token = self.tokens.get(self.pos);
        self.pos += 1;
        return Ok(token.unwrap());
    }

    fn next_if(&mut self, token_type: TokenType) -> ParseResult<Option<&Token>> {
        if self.peek(0)?.kind == token_type {
            return Ok(Some(self.next()?));
        }

        return Ok(None);
    }

    fn next_binary_operator(&mut self, prev_op: Option<BinaryOperator>) -> ParseResult<BinaryOperator> {
        let token = self.next()?.clone();
        let res = match (prev_op, &token.kind) {
            (None, TokenType::LessThan) => self.next_binary_operator(Some(BinaryOperator::LessThan)),
            (None, TokenType::GreaterThan) => self.next_binary_operator(Some(BinaryOperator::GreaterThan)),
            (None, TokenType::Plus) => self.next_binary_operator(Some(BinaryOperator::Plus)),
            (None, TokenType::Minus) => self.next_binary_operator(Some(BinaryOperator::Minus)),
            (None, TokenType::Multiply) => self.next_binary_operator(Some(BinaryOperator::Multiply)),
            (None, TokenType::Divide) => self.next_binary_operator(Some(BinaryOperator::Divide)),
            (None, TokenType::Amp) => self.next_binary_operator(Some(BinaryOperator::BitAnd)),
            (None, TokenType::Pipe) => self.next_binary_operator(Some(BinaryOperator::BitOr)),
            (None, TokenType::Hat) => self.next_binary_operator(Some(BinaryOperator::BitXor)),
            (None, TokenType::Equal) => self.next_binary_operator(Some(BinaryOperator::Assign(None))),
            (None, TokenType::Exclamation) => {
                self.expect(TokenType::Equal)?;
                Ok(BinaryOperator::LogicalNotEqual)
            },
            (Some(BinaryOperator::LessThan), TokenType::LessThan) => self.next_binary_operator(Some(BinaryOperator::LeftShift)),
            (Some(BinaryOperator::GreaterThan), TokenType::GreaterThan) => self.next_binary_operator(Some(BinaryOperator::RightShift)),
            (Some(BinaryOperator::BitAnd), TokenType::Amp) => self.next_binary_operator(Some(BinaryOperator::LogicalAnd)),
            (Some(BinaryOperator::BitOr), TokenType::Pipe) => self.next_binary_operator(Some(BinaryOperator::LogicalOr)),
            
            (Some(BinaryOperator::LogicalOr), TokenType::Equal) => Ok(BinaryOperator::assignment(BinaryOperator::LogicalOr)),
            (Some(BinaryOperator::LogicalAnd), TokenType::Equal) => Ok(BinaryOperator::assignment(BinaryOperator::LogicalAnd)),
            (Some(BinaryOperator::Plus), TokenType::Equal) => Ok(BinaryOperator::assignment(BinaryOperator::Plus)),
            (Some(BinaryOperator::Minus), TokenType::Equal) => Ok(BinaryOperator::assignment(BinaryOperator::Minus)),
            (Some(BinaryOperator::Multiply), TokenType::Equal) => Ok(BinaryOperator::assignment(BinaryOperator::Multiply)),
            (Some(BinaryOperator::Divide), TokenType::Equal) => Ok(BinaryOperator::assignment(BinaryOperator::Divide)),
            (Some(BinaryOperator::BitAnd), TokenType::Equal) => Ok(BinaryOperator::assignment(BinaryOperator::BitAnd)),
            (Some(BinaryOperator::BitOr), TokenType::Equal) => Ok(BinaryOperator::assignment(BinaryOperator::BitOr)),
            (Some(BinaryOperator::BitXor), TokenType::Equal) => Ok(BinaryOperator::assignment(BinaryOperator::BitXor)),
            (Some(BinaryOperator::LeftShift), TokenType::Equal) => Ok(BinaryOperator::assignment(BinaryOperator::LeftShift)),
            (Some(BinaryOperator::RightShift), TokenType::Equal) => Ok(BinaryOperator::assignment(BinaryOperator::RightShift)),
            (Some(BinaryOperator::Assign(None)), TokenType::Equal) => Ok(BinaryOperator::LogicalEqual),
            (Some(BinaryOperator::LessThan), TokenType::Equal) => Ok(BinaryOperator::LessThanEqual),
            (Some(BinaryOperator::GreaterThan), TokenType::Equal) => Ok(BinaryOperator::GreaterThanEqual),
            (Some(op), _) => {
                self.pos -= 1;
                Ok(op)
            },
            _ => bail!("Unexpected token {}", token)
        };

        return res;
    }

    fn peek_binary_operator(&mut self, look_ahead: usize) -> ParseResult<BinaryOperator> {
        let pos = self.pos;
        self.pos += look_ahead;
        let res = self.next_binary_operator(None);
        self.pos = pos;
        return res;
    }

    fn expect(&mut self, token_type: TokenType) -> ParseResult<&Token> {
        let token = self.next()?;

        return if token.kind == token_type {
            Ok(token)
        } else {
            bail!("Unexpected token {} at {}: Expected {} but got {}", token, token.pos, token_type, token)
        }
    }

    fn parse(&mut self) -> ParseResult<AST> {
        return Ok(AST::new(self.parse_compound_statement()?));
    }

    fn parse_compound_statement(&mut self) -> ParseResult<ASTStatement> {
        let pos = self.peek(0)?.pos.clone();
        let in_block = self.next_if(TokenType::LeftCurlyBracket)?.is_some();
        let mut statements: Vec<ASTStatement> = Vec::new();

        while !matches!(&self.peek(0)?.kind, TokenType::RightCurlyBracket | TokenType::EOF) {
            statements.push(self.parse_statement()?);
        }

        if in_block {
            self.expect(TokenType::RightCurlyBracket)?;
        }

        return Ok(ASTStatement::new(StatementKind::Compound(statements), pos));
    }

    fn parse_statement(&mut self) -> ParseResult<ASTStatement> {
        let statement = match self.peek(0)?.kind {
            TokenType::Return => self.parse_return_statement()?,
            TokenType::Say => self.parse_say_statement()?,
            TokenType::Fn => self.parse_fn_statement()?,
            TokenType::If => self.parse_if_statement()?,
            TokenType::While => self.parse_while_statement()?,
            TokenType::Class => self.parse_class_statement()?,
            _ => self.parse_expression_statement()?
        };

        return Ok(statement);
    }

    fn parse_return_statement(&mut self) -> ParseResult<ASTStatement> {
        let pos = self.expect(TokenType::Return)?.pos.clone();

        if let Ok(TokenType::Semicolon) = self.peek_type(0) {
            self.next()?;
            return Ok(ASTStatement::new(StatementKind::Return(None), pos));
        }

        let expr = self.parse_ternary_expression()?;
        self.expect(TokenType::Semicolon)?;
        return Ok(ASTStatement::new(StatementKind::Return(Some(expr)), pos));
    }

    fn parse_say_statement(&mut self) -> ParseResult<ASTStatement> {
        let pos = self.expect(TokenType::Say)?.pos.clone();
        let name = self.expect(TokenType::Identifier)?.lexeme.clone();

        let mut value: Option<ASTExpression> = None;
        if let Ok(TokenType::Equal) = self.peek_type(0) {
            self.expect(TokenType::Equal)?;
            value = Some(self.parse_expression()?);
        }

        self.expect(TokenType::Semicolon)?;
        return Ok(ASTStatement::new(StatementKind::Say(VariableDeclaration { name, value }), pos));
    }

    fn parse_fn_statement(&mut self) -> ParseResult<ASTStatement> {
        let pos = self.expect(TokenType::Fn)?.pos.clone();
        let name = self.expect(TokenType::Identifier)?.lexeme.clone();
        let parameters = self.parse_callable_params()?;
        let body = Box::from(self.parse_callable_decl_body()?);
        let decl = FunctionDeclaration { name, parameters, body };
        return Ok(ASTStatement::new(StatementKind::Fn(decl), pos));
    }

    fn parse_callable_params(&mut self) -> ParseResult<Vec<String>> {
        self.expect(TokenType::LeftParenthesis)?;

        let mut seen_params: HashSet<String> = HashSet::new();
        let mut params = Vec::new();

        while let Ok(TokenType::Identifier) = self.peek_type(0) {
            let name = String::from(self.next()?.lexeme.clone());
            if seen_params.contains(&name) {
                bail!("Duplicate parameter declaration: {}", name);
            }
            seen_params.insert(name.clone());
            params.push(name.clone());
            self.next_if(TokenType::Comma)?;
        }

        self.expect(TokenType::RightParenthesis)?;
        return Ok(params);
    }

    fn parse_callable_decl_body(&mut self) -> ParseResult<ASTStatement> {
        self.expect(TokenType::LeftCurlyBracket)?;
        let body = self.parse_compound_statement()?;
        self.expect(TokenType::RightCurlyBracket)?;
        return Ok(body);
    }

    fn parse_class_statement(&mut self) -> ParseResult<ASTStatement> {
        let pos = self.expect(TokenType::Class)?.pos.clone();
        let name = self.expect(TokenType::Identifier)?.lexeme.clone();
        let super_class = match self.peek_type(0)? {
            TokenType::Colon => {
                self.next()?;
                Some(self.expect(TokenType::Identifier)?.lexeme.clone())
            },
            _ => None
        };

        self.expect(TokenType::LeftCurlyBracket)?;

        let mut members: HashSet<String> = HashSet::new();
        let mut fields: Vec<VariableDeclaration> = Vec::new();
        let mut init: Option<ClassInit> = None;
        let mut methods: Vec<FunctionDeclaration> = Vec::new();

        while let Ok(token) = self.peek(0) {
            let pos = token.pos.clone();

            match token.kind {
                TokenType::RightCurlyBracket => break,
                TokenType::Identifier => {
                    let name = self.next()?.lexeme.clone();
                    let mut value: Option<ASTExpression> = None;
                    if let Ok(TokenType::Equal) = self.peek_type(0) {
                        self.expect(TokenType::Equal)?;
                        value = Some(self.parse_expression()?);
                    }
                    self.expect(TokenType::Semicolon)?;

                    if members.contains(&name) {
                        bail!("Duplicate member declaration: {} at {}", name, pos.clone());
                    }

                    members.insert(name.clone());
                    fields.push(VariableDeclaration { name, value });
                },
                TokenType::Init => {
                    if init.is_some() {
                        bail!("Duplicate init declaration at {}", pos.clone());
                    }

                    init = Some(self.parse_init_statement()?);
                },
                TokenType::Fn => {
                    match self.parse_fn_statement()?.kind {
                        StatementKind::Fn(fn_decl) => {
                            if members.contains(&fn_decl.name) {
                                bail!("Duplicate member declaration: {}", name);
                            }
        
                            members.insert(fn_decl.name.clone());
                            methods.push(fn_decl)
                        },
                        _ => bail!("Expected function declaration at {}", pos.clone())
                    }
                },
                _ => bail!("Unexpected token {} at {}", token, pos.clone())
            };
        }

        self.expect(TokenType::RightCurlyBracket)?;

        let mut init_stmts: Vec<ASTStatement> = fields.iter()
            .filter(|field| field.value.is_some())
            .map(|field| {
                let pos = pos.clone();
                let this = ASTExpression::new(ASTExpressionKind::This, pos.clone());
                let identifier = ASTExpression::new(ASTExpressionKind::MemberAccess(Box::new(this), field.name.clone()), pos.clone());
                let expr = ASTExpressionKind::Binary(BinaryOperator::Assign(None), Box::new(identifier), Box::new(field.value.clone().unwrap()));
                let stmt = StatementKind::Expression(ASTExpression::new(expr, pos.clone()));
                ASTStatement::new(stmt, pos)
            })
            .collect();
        
        if let Some(init) = &init {
            init_stmts.push(init.body.as_ref().clone());
        }

        let init = match init {
            Some(init) => ClassInit {
                parameters: init.parameters,
                super_args: init.super_args,
                body: Box::new(ASTStatement::new(StatementKind::Compound(init_stmts), pos.clone()))
            },
            None => ClassInit {
                parameters: Vec::new(),
                super_args: Vec::new(),
                body: Box::new(ASTStatement::new(StatementKind::Compound(init_stmts), pos.clone()))
            }
        };

        let class_decl = ClassDeclaration { 
            name, 
            superclass: super_class, 
            init, 
            fields: fields.iter().map(|field| field.name.clone()).collect(), 
            methods 
        };
        return Ok(ASTStatement::new(StatementKind::Class(class_decl), pos));
    }

    fn parse_init_statement(&mut self) -> ParseResult<ClassInit> {
        self.expect(TokenType::Init)?;
        let parameters = self.parse_callable_params()?;
        self.expect(TokenType::LeftCurlyBracket)?;

        let super_args = match self.peek_type(0)? {
            TokenType::Super => {
                self.next()?;
                let args = self.parse_call_args()?;
                self.expect(TokenType::Semicolon)?;
                args
            },
            _ => Vec::new()
        };

        let body = Box::new(self.parse_compound_statement()?);
        self.expect(TokenType::RightCurlyBracket)?;
        
        return Ok(ClassInit {
            parameters,
            super_args,
            body
        });
    }

    fn parse_if_statement(&mut self) -> ParseResult<ASTStatement> {
        let pos = self.expect(TokenType::If)?.pos.clone();
        let condition = self.parse_expression()?;
        let then = Box::from(self.parse_compound_statement()?);
        let otherwise = match self.peek_type(0) {
            Ok(TokenType::Else) => {
                self.next()?;
                Some(Box::from(self.parse_compound_statement()?))
            },
            _ => None
        };

        return Ok(ASTStatement::new(StatementKind::If(condition, then, otherwise), pos));
    }

    fn parse_while_statement(&mut self) -> ParseResult<ASTStatement> {
        let pos = self.expect(TokenType::While)?.pos.clone();
        let condition = self.parse_expression()?;
        let body = Box::from(self.parse_compound_statement()?);
        return Ok(ASTStatement::new(StatementKind::While(condition, body), pos));
    }

    fn parse_expression_statement(&mut self) -> ParseResult<ASTStatement> {
        let expr = self.parse_expression()?;
        self.expect(TokenType::Semicolon)?;
        return Ok(ASTStatement::new(StatementKind::Expression(expr.clone()), expr.pos));
    }

    fn parse_expression(&mut self) -> ParseResult<ASTExpression> {
        return self.parse_assignment_expression();
    }

    fn parse_assignment_expression(&mut self) -> ParseResult<ASTExpression> {
        if let Ok(TokenType::Identifier) = self.peek_type(0) {
            if let Ok(BinaryOperator::Assign(maybe_op)) = self.peek_binary_operator(1) {
                let left = self.parse_primary_expression()?;
                let pos = left.pos.clone();
                self.next_binary_operator(None)?;
                let mut right = self.parse_assignment_expression()?;

                // Convert to normal assignment expression, for example: a += 1 -> a = a + 1
                right = match maybe_op {
                    Some(op) => {
                        let kind = ASTExpressionKind::Binary(*op, Box::from(left.clone()), Box::from(right));
                        ASTExpression::new(kind, pos.clone())
                    },
                    None => right,
                };

                let kind = ASTExpressionKind::Binary(BinaryOperator::Assign(None), Box::from(left), Box::from(right));
                return Ok(ASTExpression::new(kind, pos));
            }
        }

        return self.parse_ternary_expression();
    }

    fn parse_ternary_expression(&mut self) -> ParseResult<ASTExpression> {
        let mut left = self.parse_binary_expression(0)?;

        if let Ok(TokenType::Question) = self.peek_type(0) {
            self.next()?;
            let middle = self.parse_ternary_expression()?;
            self.expect(TokenType::Colon)?;
            let right = self.parse_ternary_expression()?;

            let kind = ASTExpressionKind::Ternary(Box::from(left.clone()), Box::from(middle), Box::from(right));
            left = ASTExpression::new(kind, left.pos.clone());
        }

        return Ok(left);
    }

    fn parse_binary_expression(&mut self, parent_precedence: u8) -> ParseResult<ASTExpression> {
        let mut left = self.parse_unary_expression()?;
        let pos = left.pos.clone();

        while let Ok(op) = self.peek_binary_operator(0) {
            if op.precedence() < parent_precedence || (op.is_left_associative() && op.precedence() == parent_precedence) {
                break;
            }

            self.next_binary_operator(None)?;
            let right = self.parse_binary_expression(op.precedence())?;

            let kind = ASTExpressionKind::Binary(op, Box::from(left), Box::from(right));
            left = ASTExpression::new(kind, pos.clone());
        }

        return Ok(left);
    }

    fn parse_unary_expression(&mut self) -> ParseResult<ASTExpression> {
        let next = self.peek(0)?.clone();
        let pos = next.pos.clone();

        let unary_op = match next.kind {
            TokenType::Minus => UnaryOperator::Negative,
            TokenType::Exclamation => UnaryOperator::LogicalNot,
            TokenType::Tilde => UnaryOperator::BitNot,
            _ => return self.parse_postfix_expression()
        };

        let kind = ASTExpressionKind::Unary(unary_op, Box::from(self.parse_unary_expression()?));
        return Ok(ASTExpression::new(kind, pos));
    }

    fn parse_postfix_expression(&mut self) -> ParseResult<ASTExpression> {
        let mut expr = self.parse_primary_expression()?;

        loop {
            expr = match self.peek(0)?.kind {
                TokenType::LeftParenthesis => self.parse_postfix_call_expression(expr)?,
                TokenType::Dot => self.parse_postfix_access_expression(expr)?,
                _ => break
            }
        }

        return Ok(expr);
    }

    fn parse_postfix_call_expression(&mut self, expr: ASTExpression) -> ParseResult<ASTExpression> {
        let pos = self.peek(0)?.pos.clone();
        let args = self.parse_call_args()?;
        return Ok(ASTExpression::new(ASTExpressionKind::Call(Box::from(expr), args), pos));
    }

    fn parse_call_args(&mut self) -> ParseResult<Vec<ASTExpression>> {
        self.expect(TokenType::LeftParenthesis)?;

        let mut args: Vec<ASTExpression> = Vec::new();
        while !matches!(&self.peek_type(0)?, TokenType::RightParenthesis | TokenType::EOF) {
            args.push(self.parse_expression()?);

            if *self.peek_type(0)? != TokenType::RightParenthesis {
                self.expect(TokenType::Comma)?;
            }
        }

        self.expect(TokenType::RightParenthesis)?;
        return Ok(args);
    }

    fn parse_postfix_access_expression(&mut self, expr: ASTExpression) -> ParseResult<ASTExpression> {
        let pos = self.expect(TokenType::Dot)?.pos.clone();
        let member = self.expect(TokenType::Identifier)?.lexeme.clone();
        return Ok(ASTExpression::new(ASTExpressionKind::MemberAccess(Box::from(expr), member), pos));
    }

    fn parse_primary_expression(&mut self) -> ParseResult<ASTExpression> {
        let token = self.peek(0)?.clone();
        let pos = token.pos.clone();

        let expr: ASTExpression = match token.kind {
            TokenType::LeftParenthesis => {
                self.next()?;
                let expr = self.parse_expression()?;
                self.expect(TokenType::RightParenthesis)?;
                expr
            },
            TokenType::StringLiteral => {
                let val = self.next()?.lexeme.clone();
                // Strip quotes
                let kind = ASTExpressionKind::String(String::from(&val[1..val.len() - 1]));
                ASTExpression::new(kind, pos)
            },
            TokenType::NumericLiteral => {
                let kind = ASTExpressionKind::Number(self.next()?.lexeme.parse().unwrap());
                ASTExpression::new(kind, pos)
            },
            TokenType::True => {
                self.next()?;
                let kind = ASTExpressionKind::Boolean(true);
                ASTExpression::new(kind, pos)
            },
            TokenType::False => {
                self.next()?;
                let kind = ASTExpressionKind::Boolean(false);
                ASTExpression::new(kind, pos)
            },
            TokenType::This => {
                self.next()?;
                let kind = ASTExpressionKind::This;
                ASTExpression::new(kind, pos)
            },
            TokenType::Identifier => {
                let kind = ASTExpressionKind::Identifier(self.next()?.lexeme.parse().unwrap());
                ASTExpression::new(kind, pos)
            },
            _ => bail!("Unexpected token {} at {}", token, token.pos)
        };

        return Ok(expr);
    }
}

pub fn parse(tokens: &Vec<Token>) -> ParseResult<AST> {
    let mut parser = Parser::new(tokens);
    return parser.parse();
}
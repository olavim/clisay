mod token_stream;
mod operator;
mod ast;
mod statement;
mod expression;

pub use ast::AST;
pub use expression::ASTExpressionKind;
pub use statement::ASTStatement;
pub use expression::ASTExpression;
pub use operator::Operator;
pub use statement::StatementKind;
use token_stream::TokenStream;

use super::lexer::Token;

type ParseResult<T> = Result<T, anyhow::Error>;

struct Parser<'a> {
    stream: TokenStream<'a>
}

impl Parser<'_> {
    fn new(tokens: &Vec<Token>) -> Parser {
        let stream = TokenStream::new(tokens);
        return Parser { stream };
    }

    fn parse(&mut self) -> ParseResult<AST> {
        return Ok(AST::new(ASTStatement::parse(&mut self.stream)?));
    }
}

pub fn parse(tokens: &Vec<Token>) -> ParseResult<AST> {
    let mut parser = Parser::new(tokens);
    return parser.parse();
}
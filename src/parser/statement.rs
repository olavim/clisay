use std::fmt;

use crate::lexer::SourcePosition;
use crate::parser::ASTExpression;

#[derive(Clone)]
pub struct VariableDeclaration {
    pub name: String,
    pub value: Option<ASTExpression>
}

impl fmt::Display for VariableDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        return write!(f, "{}", match &self.value {
            Some(value) => format!("{} = {}", self.name, value.kind),
            None => format!("{}", self.name)
        });
    }
}

#[derive(Clone)]
pub struct FunctionDeclaration {
    pub name: String,
    pub parameters: Vec<String>,
    pub body: Box<ASTStatement>
}

#[derive(Clone)]
pub struct ClassInit {
    pub parameters: Vec<String>,
    pub super_args: Vec<ASTExpression>,
    pub body: Box<ASTStatement>
}

#[derive(Clone)]
pub struct ClassDeclaration {
    pub name: String,
    pub superclass: Option<String>,
    pub init: ClassInit,
    pub fields: Vec<String>,
    pub methods: Vec<FunctionDeclaration>
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
}

#[derive(Clone)]
pub enum StatementKind {
    Compound(Vec<ASTStatement>),
    Expression(ASTExpression),
    Return(Option<ASTExpression>),
    Say(VariableDeclaration),
    Fn(FunctionDeclaration),
    If(ASTExpression, Box<ASTStatement>, Option<Box<ASTStatement>>),
    While(ASTExpression, Box<ASTStatement>),
    Class(ClassDeclaration)
}

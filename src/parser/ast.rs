use super::statement::ASTStatement;

pub struct AST {
    pub stmt: ASTStatement
}

impl AST {
    pub fn new(stmt: ASTStatement) -> AST {
        return AST { stmt };
    }
}

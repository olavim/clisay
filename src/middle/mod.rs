//! The middle end: turns the AST into back-end `ir`/bytecode.

pub mod ir;
pub mod resolve;
pub mod codegen;
pub mod optimize;

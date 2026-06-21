//! The middle end: turns the AST into back-end `ir`/bytecode.

pub mod hir;
pub mod names;
pub mod lower;
pub mod ir;
pub mod bind;
pub mod signatures;
pub mod check;
pub mod codegen;
pub mod optimize;

//! The middle end: turns the AST into back-end `ir`/bytecode.

pub mod hir;
pub mod native;
pub mod names;
pub mod lower;
pub mod ir;
pub mod bind;
pub mod obligations;
pub mod shape;
pub mod signatures;
pub mod diagnose;
pub mod walk;
pub mod check;
pub mod codegen;
pub mod optimize;

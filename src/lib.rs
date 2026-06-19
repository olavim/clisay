#[cfg(debug_assertions)]
#[cfg_attr(debug_assertions, path = "debug_output.rs")]
mod output;

#[cfg(not(debug_assertions))]
mod output {
    pub struct Output;
    impl Output {
        #[inline(always)]
        pub fn println(value: impl Into<String>) {
            println!("{}", value.into());
        }
    }
}

mod core;
mod frontend;
mod middle;
mod backend;
mod runtime;

pub(crate) use frontend::ast;
pub use output::Output;

use crate::backend::assemble::assemble;
use crate::core::gc::Gc;
use crate::frontend::lex::{tokenize, TokenStream};
use crate::frontend::parse::Parser;
use crate::middle::codegen::Compiler;
use crate::middle::lower::lower;
use crate::middle::names::resolve as resolve_names;
use crate::middle::optimize::optimize;
use crate::middle::bind::resolve as resolve_bindings;

pub fn run(file_name: &str, src: &str) -> Result<Vec<String>, anyhow::Error> {
    let mut gc = Gc::new();
    let tokens = tokenize(String::from(file_name), String::from(src))?;
    let ast = Parser::parse(&mut TokenStream::new(&tokens))?;
    let names = resolve_names(&ast)?;
    let hir = lower(ast, &names)?;
    let bindings = resolve_bindings(&hir)?;
    let ir = Compiler::compile(&hir, &mut gc, &bindings)?;
    let ir = optimize(ir);
    let chunk = assemble(ir)?;
    runtime::execute(chunk, gc)
}
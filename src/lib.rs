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
use crate::middle::optimize::optimize;
use crate::middle::resolve::resolve;

pub fn run(file_name: &str, src: &str) -> Result<Vec<String>, anyhow::Error> {
    let mut gc = Gc::new();
    let tokens = tokenize(String::from(file_name), String::from(src))?;
    let ast = Parser::parse(&mut TokenStream::new(&tokens))?;
    let bindings = resolve(&ast)?;
    let ir = Compiler::compile(&ast, &mut gc, &bindings)?;
    let ir = optimize(ir);
    let chunk = assemble(ir)?;
    runtime::execute(chunk, gc)
}
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

mod lexer;
mod parser;
mod vm;

pub use output::Output;

pub fn run(file_name: &str, src: &str) -> Result<Vec<String>, anyhow::Error> {
    vm::run(file_name, src)
}
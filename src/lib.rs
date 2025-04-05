#[cfg(test)]
mod _tests;

mod lexer;
mod vm;

pub fn run(file_name: &str, src: &str) -> Result<Vec<String>, anyhow::Error> {
    vm::run(file_name, src)
}
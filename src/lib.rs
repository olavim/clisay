#[cfg(test)]
mod _tests;

mod lexer;
mod bytecode;

pub fn run(file_name: &str, src: &str) -> Result<Vec<String>, anyhow::Error> {
    bytecode::run(file_name, src)
}
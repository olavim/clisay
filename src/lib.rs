use anyhow::anyhow;

mod lexer;
mod parser;
mod interpreter;

pub fn run(file_name: &str, src: &str) -> Result<Vec<String>, anyhow::Error> {
    let tokens = lexer::tokenize(String::from(file_name), String::from(src))?;
    let ast = parser::parse(&tokens)?;
    match interpreter::evaluate(&ast) {
        Ok(out) => Ok(out),
        Err(err) => Err(anyhow!("{}", err))
    }
}

#[cfg(test)]
mod _tests;
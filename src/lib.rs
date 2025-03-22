mod lexer;
// mod parser;
// mod interpreter;
mod bytecode;

// pub fn run(file_name: &str, src: &str) -> Result<Vec<String>, anyhow::Error> {
//     let tokens = lexer::tokenize(String::from(file_name), String::from(src))?;
//     let ast = parser::parse(&tokens)?;
//     match interpreter::run(&ast) {
//         Ok(out) => Ok(out),
//         Err(err) => Err(anyhow!("{}", err))
//     }
// }

pub fn run(file_name: &str, src: &str) -> Result<Vec<String>, anyhow::Error> {
    bytecode::Vm::run(file_name, src)
}

#[cfg(test)]
mod _tests;
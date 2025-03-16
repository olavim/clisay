use anyhow::anyhow;

mod lexer;
mod parser;
mod interpreter;
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
    let mut vm = bytecode::Vm::new();
    vm.run(file_name, src)?;
    Ok(vec![])
}

#[cfg(test)]
mod _tests;
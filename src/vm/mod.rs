mod stack;
mod opcode;
mod chunk;
mod objects;
mod vm;
mod gc;
mod operator;
mod value;
mod parser;
mod compiler;

pub fn run(file_name: &str, src: &str) -> Result<Vec<String>, anyhow::Error> {
    vm::Vm::run(file_name, src)
}
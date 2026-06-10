mod stack;
mod native;
mod vm;

pub mod gc;
pub mod chunk;
pub mod opcode;
pub mod objects;
pub mod value;

pub fn run(file_name: &str, src: &str) -> Result<Vec<String>, anyhow::Error> {
    vm::Vm::run(file_name, src)
}
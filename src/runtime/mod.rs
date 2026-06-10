mod vm;

pub fn run(file_name: &str, src: &str) -> Result<Vec<String>, anyhow::Error> {
    vm::Vm::run(file_name, src)
}
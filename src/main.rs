use std::env;
use std::io::IsTerminal;

use clisay::run;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: clisay <file>");
        std::process::exit(1);
    }

    // Color diagnostics only when stderr is a terminal, so piped output stays plain.
    clisay::enable_color(std::io::stderr().is_terminal());

    let file = args[1].as_str();
    let src = std::fs::read_to_string(file).unwrap();
    if let Err(err) = run(file, &src) {
        eprintln!("{err}");
        std::process::exit(1);
    }
}

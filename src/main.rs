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
    // The compiler passes recurse with expression depth, so a deeply nested program can exhaust the
    // main thread's stack. Run on a worker thread with a generous one.
    let file = file.to_string();
    let result = std::thread::Builder::new()
        .stack_size(256 * 1024 * 1024)
        .spawn(move || run(&file, &src))
        .unwrap()
        .join()
        .unwrap();
    if let Err(err) = result {
        eprintln!("{err}");
        std::process::exit(1);
    }
}

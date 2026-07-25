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
    let color = std::io::stderr().is_terminal();

    let file = args[1].as_str();
    let src = std::fs::read_to_string(file).unwrap();
    let file = file.to_string();

    // The compiler passes recurse with expression depth, so a deeply nested program can exhaust the
    // main thread's stack. Run on a worker thread with a generous one.
    let result = std::thread::Builder::new()
        .stack_size(256 * 1024 * 1024)
        .spawn(move || {
            // A diagnostic renders to a string on this worker thread as the error is built,
            // so the color flag must be set here, not on the main thread.
            clisay::enable_color(color);
            run(&file, &src)
        })
        .unwrap()
        .join()
        .unwrap();
    if let Err(err) = result {
        eprintln!("{err}");
        std::process::exit(1);
    }
}

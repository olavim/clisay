use std::env;
use std::io::IsTerminal;

use clisay::{run_with, RunConfig};

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: clisay <file>");
        std::process::exit(1);
    }

    // Color diagnostics only when stderr is a terminal, so piped output stays plain.
    let color = std::io::stderr().is_terminal();
    let config = RunConfig {
        optimize: env::var_os("CLISAY_NO_OPTIMIZE").is_none(),
        force_checks: cfg!(debug_assertions) && env::var_os("CLISAY_FORCE_CHECKS").is_some(),
        floor_only: cfg!(debug_assertions) && env::var_os("CLISAY_FLOOR_ONLY").is_some(),
    };

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
            run_with(&file, &src, config)
        })
        .unwrap()
        .join()
        .unwrap();
    if let Err(err) = result {
        eprintln!("{err}");
        std::process::exit(1);
    }
}

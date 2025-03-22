use std::env;

use clisay::run;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: clisay <file>");
        std::process::exit(1);
    }

    let file = args[1].as_str();
    let src = std::fs::read_to_string(file).unwrap();
    _ = run(file, &src);
}

#[test]
fn test_compiler() {
    let file = "tests/perf.say";
    let src = std::fs::read_to_string(file).unwrap();
    match run(file, &src) {
        Ok(_) => (),
        Err(err) => {
            eprintln!("{}", err.to_string());
            eprintln!("{}", err.backtrace());
            panic!();
        }
    }
}
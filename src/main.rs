use clisay::run;

fn main() {
    let file = "tests/perf.say";
    let src = std::fs::read_to_string(file).unwrap();
    _ = run(file, &src);
}
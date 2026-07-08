//! Deterministic instruction-count benchmarks via iai-callgrind (callgrind).
//! Linux/WSL only (needs valgrind). Run with:  cargo bench --bench iai

#[cfg(unix)]
use iai_callgrind::{library_benchmark, library_benchmark_group, main};
#[cfg(unix)]
use std::hint::black_box;

#[cfg(unix)]
fn run_say(file: &str) {
    let src = std::fs::read_to_string(file).unwrap();
    black_box(clisay::run(black_box(file), black_box(&src)).unwrap());
}

#[cfg(unix)]
#[library_benchmark]
fn fib() {
    run_say("benches/fib.say");
}

#[cfg(unix)]
#[library_benchmark]
fn loops() {
    run_say("benches/loop.say");
}

#[cfg(unix)]
#[library_benchmark]
fn deep_sum() {
    run_say("benches/deep_sum.say");
}

#[cfg(unix)]
#[library_benchmark]
fn method_calls() {
    run_say("benches/method_calls.say");
}

#[cfg(unix)]
#[library_benchmark]
fn strings() {
    run_say("benches/strings.say");
}

#[cfg(unix)]
#[library_benchmark]
fn arrays() {
    run_say("benches/arrays.say");
}

#[cfg(unix)]
#[library_benchmark]
fn alloc_gc() {
    run_say("benches/alloc_gc.say");
}

#[cfg(unix)]
#[library_benchmark]
fn compare_dict() {
    run_say("benches/compare_dict.say");
}

#[cfg(unix)]
library_benchmark_group!(
    name = workloads;
    benchmarks = fib, loops, deep_sum, method_calls, strings, arrays, alloc_gc, compare_dict
);

#[cfg(unix)]
main!(library_benchmark_groups = workloads);

#[cfg(not(unix))]
fn main() {
    eprintln!("the iai benches run only on Linux/WSL (they require valgrind)");
}

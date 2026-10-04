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
fn anchors_calls() {
    run_say("benches/anchors_calls.say");
}

#[cfg(unix)]
#[library_benchmark]
fn anchors_accesses() {
    run_say("benches/anchors_accesses.say");
}

#[cfg(unix)]
#[library_benchmark]
fn anchors_resolved() {
    run_say("benches/anchors_resolved.say");
}

#[cfg(unix)]
#[library_benchmark]
fn flagged_share_deep() {
    run_say("benches/flagged_share_deep.say");
}

#[cfg(unix)]
#[library_benchmark]
fn flagged_share_deep_after_shallow() {
    run_say("benches/flagged_share_deep_after_shallow.say");
}

#[cfg(unix)]
#[library_benchmark]
fn anchors_calls_bare() {
    run_say("benches/anchors_calls_bare.say");
}

#[cfg(unix)]
#[library_benchmark]
fn nest_inline() {
    run_say("benches/nest_inline.say");
}

#[cfg(unix)]
#[library_benchmark]
fn nest_anchor() {
    run_say("benches/nest_anchor.say");
}

#[cfg(unix)]
#[library_benchmark]
fn nest_bare() {
    run_say("benches/nest_bare.say");
}

#[cfg(unix)]
#[library_benchmark]
fn sort_anchor() {
    run_say("benches/sort_anchor.say");
}

#[cfg(unix)]
#[library_benchmark]
fn sort_inline() {
    run_say("benches/sort_inline.say");
}

#[cfg(unix)]
#[library_benchmark]
fn nest_anchor_number() {
    run_say("benches/nest_anchor_number.say");
}

#[cfg(unix)]
#[library_benchmark]
fn nest_byvalue() {
    run_say("benches/nest_byvalue.say");
}

#[cfg(unix)]
#[library_benchmark]
fn anchors_accesses_slot() {
    run_say("benches/anchors_accesses_slot.say");
}

#[cfg(unix)]
#[library_benchmark]
fn anchor_cache_reads() {
    run_say("benches/anchor_cache_reads.say");
}

#[cfg(unix)]
#[library_benchmark]
fn anchor_cache_writes() {
    run_say("benches/anchor_cache_writes.say");
}

#[cfg(unix)]
#[library_benchmark]
fn anchor_cache_writes6() {
    run_say("benches/anchor_cache_writes6.say");
}

#[cfg(unix)]
#[library_benchmark]
fn anchor_cache_reads4() {
    run_say("benches/anchor_cache_reads4.say");
}

#[cfg(unix)]
#[library_benchmark]
fn anchors_local() {
    run_say("benches/anchors_local.say");
}

#[cfg(unix)]
#[library_benchmark]
fn anchors_calls_byvalue() {
    run_say("benches/anchors_calls_byvalue.say");
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
#[library_benchmark]
fn equal_shared() {
    run_say("benches/equal_shared.say");
}

#[cfg(unix)]
#[library_benchmark]
fn element_stores() {
    run_say("benches/element_stores.say");
}

#[cfg(unix)]
#[library_benchmark]
fn field_stores() {
    run_say("benches/field_stores.say");
}

#[cfg(unix)]
#[library_benchmark]
fn store_objects() {
    run_say("benches/store_objects.say");
}

#[cfg(unix)]
#[library_benchmark]
fn tail_recursion() {
    run_say("benches/tail_recursion.say");
}

#[cfg(unix)]
#[library_benchmark]
fn grow_in_place() {
    run_say("benches/grow_in_place.say");
}

#[cfg(unix)]
#[library_benchmark]
fn grow_by_value() {
    run_say("benches/grow_by_value.say");
}

#[cfg(unix)]
#[library_benchmark]
fn path_direct() {
    run_say("benches/path_direct.say");
}

#[cfg(unix)]
#[library_benchmark]
fn path_anchor_once() {
    run_say("benches/path_anchor_once.say");
}

#[cfg(unix)]
#[library_benchmark]
fn path_anchor_each() {
    run_say("benches/path_anchor_each.say");
}

#[cfg(unix)]
#[library_benchmark]
fn path_none() {
    run_say("benches/path_none.say");
}

#[cfg(unix)]
#[library_benchmark]
fn reshared_writes() {
    run_say("benches/reshared_writes.say");
}

#[cfg(unix)]
#[library_benchmark]
fn unshared_writes() {
    run_say("benches/unshared_writes.say");
}

#[cfg(unix)]
#[library_benchmark]
fn mixed_args() {
    run_say("benches/mixed_args.say");
}

#[cfg(unix)]
#[library_benchmark]
fn ref_mutate() {
    run_say("benches/ref_mutate.say");
}

// A deep call chain, where the signature pass carries one fact from the bottom to the top.
#[cfg(unix)]
#[library_benchmark]
fn sig_chain() {
    run_say("benches/sig_chain.say");
}

#[cfg(unix)]
#[library_benchmark]
fn loop_baseline() {
    run_say("benches/loop_baseline.say");
}

#[cfg(unix)]
#[library_benchmark]
fn loop_rebind_baseline() {
    run_say("benches/loop_rebind_baseline.say");
}

#[cfg(unix)]
#[library_benchmark]
fn write_binding() {
    run_say("benches/write_binding.say");
}

#[cfg(unix)]
#[library_benchmark]
fn write_binding_anchor() {
    run_say("benches/write_binding_anchor.say");
}

#[cfg(unix)]
#[library_benchmark]
fn write_field() {
    run_say("benches/write_field.say");
}

#[cfg(unix)]
#[library_benchmark]
fn write_field_anchor() {
    run_say("benches/write_field_anchor.say");
}

#[cfg(unix)]
#[library_benchmark]
fn write_element() {
    run_say("benches/write_element.say");
}

#[cfg(unix)]
#[library_benchmark]
fn write_element_anchor() {
    run_say("benches/write_element_anchor.say");
}

#[cfg(unix)]
#[library_benchmark]
fn read_binding() {
    run_say("benches/read_binding.say");
}

#[cfg(unix)]
#[library_benchmark]
fn read_binding_anchor() {
    run_say("benches/read_binding_anchor.say");
}

#[cfg(unix)]
#[library_benchmark]
fn read_field() {
    run_say("benches/read_field.say");
}

#[cfg(unix)]
#[library_benchmark]
fn read_field_anchor() {
    run_say("benches/read_field_anchor.say");
}

#[cfg(unix)]
#[library_benchmark]
fn read_element() {
    run_say("benches/read_element.say");
}

#[cfg(unix)]
#[library_benchmark]
fn read_element_anchor() {
    run_say("benches/read_element_anchor.say");
}

#[cfg(unix)]
#[library_benchmark]
fn arg_field_anchor_reads() {
    run_say("benches/arg_field_anchor_reads.say");
}

#[cfg(unix)]
#[library_benchmark]
fn arg_field_anchor_writes() {
    run_say("benches/arg_field_anchor_writes.say");
}

#[cfg(unix)]
#[library_benchmark]
fn arg_binding_anchor_reads() {
    run_say("benches/arg_binding_anchor_reads.say");
}

#[cfg(unix)]
#[library_benchmark]
fn arg_binding_anchor_writes() {
    run_say("benches/arg_binding_anchor_writes.say");
}

#[cfg(unix)]
#[library_benchmark]
fn arg_field_direct_reads() {
    run_say("benches/arg_field_direct_reads.say");
}

#[cfg(unix)]
#[library_benchmark]
fn arg_field_direct_writes() {
    run_say("benches/arg_field_direct_writes.say");
}

#[cfg(unix)]
library_benchmark_group!(
    name = workloads;
    benchmarks = fib, loops, deep_sum, method_calls, strings, arrays, alloc_gc, compare_dict, equal_shared, element_stores, field_stores, store_objects, tail_recursion, anchors_calls, anchors_calls_bare, anchors_calls_byvalue, anchors_accesses, anchors_resolved, anchors_accesses_slot, anchors_local, anchor_cache_reads, anchor_cache_reads4, anchor_cache_writes, anchor_cache_writes6, nest_inline, nest_anchor, nest_bare, nest_byvalue, nest_anchor_number, sort_anchor, sort_inline, grow_in_place, grow_by_value, path_none, path_direct, path_anchor_once, path_anchor_each, reshared_writes, unshared_writes, mixed_args, ref_mutate, sig_chain, flagged_share_deep, flagged_share_deep_after_shallow, loop_baseline, loop_rebind_baseline, write_binding, write_binding_anchor, write_field, write_field_anchor, write_element, write_element_anchor, read_binding, read_binding_anchor, read_field, read_field_anchor, read_element, read_element_anchor, arg_field_anchor_reads, arg_field_anchor_writes, arg_binding_anchor_reads, arg_binding_anchor_writes, arg_field_direct_reads, arg_field_direct_writes
);

#[cfg(unix)]
main!(library_benchmark_groups = workloads);

#[cfg(not(unix))]
fn main() {
    eprintln!("the iai benches run only on Linux/WSL (they require valgrind)");
}

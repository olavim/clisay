use criterion::{criterion_group, criterion_main, Criterion};

fn criterion_benchmark(c: &mut Criterion) {
    let file = "benches/fib.say";
    let src = std::fs::read_to_string(file).unwrap();

    let mut group = c.benchmark_group("fib");
    group.sample_size(20);
    group.bench_function("fib", |b| b.iter(|| {
        clisay::run(file, &src).unwrap();
    }));
    group.finish();
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
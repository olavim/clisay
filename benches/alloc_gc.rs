use criterion::{criterion_group, criterion_main, Criterion};

fn criterion_benchmark(c: &mut Criterion) {
    let file = "benches/alloc_gc.say";
    let src = std::fs::read_to_string(file).unwrap();

    let mut group = c.benchmark_group("alloc_gc");
    group.sample_size(10);
    group.bench_function("alloc_gc", |b| b.iter(|| {
        clisay::run(file, &src).unwrap();
    }));
    group.finish();
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);

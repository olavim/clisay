use criterion::{criterion_group, criterion_main, Criterion};

fn criterion_benchmark(c: &mut Criterion) {
    let file = "benches/deep_sum.say";
    let src = std::fs::read_to_string(file).unwrap();

    let mut group = c.benchmark_group("deep_sum");
    group.sample_size(10);
    group.bench_function("deep_sum", |b| b.iter(|| {
        clisay::run(file, &src).unwrap();
    }));
    group.finish();
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
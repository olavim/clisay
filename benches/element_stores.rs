use criterion::{criterion_group, criterion_main, Criterion};

fn criterion_benchmark(c: &mut Criterion) {
    let file = "benches/element_stores.say";
    let src = std::fs::read_to_string(file).unwrap();

    let mut group = c.benchmark_group("element_stores");
    group.sample_size(20);
    group.bench_function("element_stores", |b| b.iter(|| {
        clisay::run(file, &src).unwrap();
    }));
    group.finish();
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
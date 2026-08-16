//! The escape-summary pass, on the call-graph shapes that decide its cost.

use criterion::{black_box, criterion_group, criterion_main, Criterion};

/// N independent functions, no calls. One visit each.
fn wide(n: usize) -> String {
    let mut src = String::from("type Box { pub mut v; init(this) { this.v = 0; } }\n");
    for i in 0..n {
        src.push_str(&format!("fn f{i}(mut x) {{ x.v = {i}; }}\n"));
    }
    src
}

/// A linear chain f0 -> f1 -> ... -> fN. Acyclic, so the ordering alone settles it.
fn chain(n: usize) -> String {
    let mut src = String::from("type Box { pub mut v; init(this) { this.v = 0; } }\n");
    src.push_str(&format!("fn f{}(mut x) {{ x.v = 1; }}\n", n - 1));
    for i in (0..n - 1).rev() {
        src.push_str(&format!("fn f{i}(mut x) {{ f{}(x); }}\n", i + 1));
    }
    src
}

/// One cycle of K mutually recursive functions with nothing to propagate. The component still has
/// to be shown stable.
fn cycle(k: usize) -> String {
    let mut src = String::from("type Box { pub mut v; init(this) { this.v = 0; } }\n");
    for i in 0..k {
        src.push_str(&format!(
            "fn f{i}(mut x, n) {{ if (n > 0) {{ f{}(x, n - 1); }} }}\n",
            (i + 1) % k
        ));
    }
    src
}

/// One cycle of K functions where a persist at the far end has to travel the whole ring back to
/// `f0`. This is the shape that costs the most rounds.
fn cycle_propagating(k: usize) -> String {
    let mut src = String::from(
        "type Box { pub mut v; init(this) { this.v = 0; } }\ntype Sink { pub mut k; init(this) { this.k = 0; } }\nsay sink = mut Sink();\n",
    );
    for i in 0..k {
        let body = match i == k - 1 {
            true => "sink.k = x; if (n > 0) { f0(x, n - 1); }".to_string(),
            false => format!("if (n > 0) {{ f{}(x, n - 1); }}", i + 1),
        };
        src.push_str(&format!("fn f{i}(mut x, n) {{ {body} }}\n"));
    }
    src
}

/// M separate two-member cycles: many small components rather than one large one.
fn small_cycles(m: usize) -> String {
    let mut src = String::from("type Box { pub mut v; init(this) { this.v = 0; } }\n");
    for i in 0..m {
        src.push_str(&format!("fn p{i}(mut x, n) {{ if (n > 0) {{ q{i}(x, n - 1); }} }}\n"));
        src.push_str(&format!("fn q{i}(mut x, n) {{ if (n > 0) {{ p{i}(x, n - 1); }} }}\n"));
    }
    src
}

fn bench(c: &mut Criterion, name: &str, src: &str) {
    let (hir, bindings) = clisay::internals::bind(src);
    let mut group = c.benchmark_group("escape_order");
    group.sample_size(30);
    group.bench_function(name, |b| {
        b.iter(|| clisay::internals::signatures(black_box(&hir), black_box(&bindings)))
    });
    group.finish();
}

fn benchmark(c: &mut Criterion) {
    bench(c, "wide_200", &wide(200));
    bench(c, "chain_100", &chain(100));
    bench(c, "cycle_8", &cycle(8));
    bench(c, "cycle_32", &cycle(32));
    bench(c, "cycle_propagating_8", &cycle_propagating(8));
    bench(c, "cycle_propagating_32", &cycle_propagating(32));
    bench(c, "small_cycles_50", &small_cycles(50));
}

criterion_group!(benches, benchmark);
criterion_main!(benches);

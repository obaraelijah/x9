use criterion::{black_box, criterion_group, criterion_main, Criterion};
use x9::parser::read;

fn parse_benchmark(c: &mut Criterion) {
    let program = "(doall (take 100 (map fib (range)))) (+ 1 1)";
    c.bench_function("parse doall", |b| {
        b.iter(|| {
            for i in read(&program) {
                black_box(i.unwrap());
            }
        })
    });
}

fn eval_benchmark(c: &mut Criterion) {
    todo!()
}

criterion_group!(name = benches;
                 config = Criterion::default().sample_size(75);
                 targets = parse_benchmark);
criterion_main!(benches);
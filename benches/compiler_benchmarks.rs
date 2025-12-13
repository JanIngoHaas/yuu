use criterion::{black_box, criterion_group, criterion_main, Criterion};
use yuu::utils::pipeline::Pipeline;

fn load_test_file(filename: &str) -> (String, String) {
    let path = format!("bench_data/{}", filename);
    let source = std::fs::read_to_string(&path)
        .unwrap_or_else(|_| panic!("Failed to load test file: {}", path));
    (source, filename.to_string())
}

fn bench_parsing(c: &mut Criterion) {
    let (source, filename) = load_test_file("gigantic_benchmark.yuu");

    c.bench_function("parsing", |b| {
        b.iter(|| {
            let mut pipeline = Pipeline::new(black_box(source.clone()), black_box(filename.clone()));
            pipeline.calc_ast().expect("Parse failed");
            black_box(pipeline);
        });
    });
}

fn bench_compile_to_executable(c: &mut Criterion) {
    let (source, filename) = load_test_file("gigantic_benchmark.yuu");

    c.bench_function("compile_to_executable", |b| {
        b.iter(|| {
            let mut pipeline = Pipeline::new(black_box(source.clone()), black_box(filename.clone()));
            let _executable = pipeline.calc_executable().expect("Compilation to executable failed");
            black_box(pipeline);
        });
    });
}

criterion_group!(benches, bench_parsing, bench_compile_to_executable);
criterion_main!(benches);
#[macro_use]
extern crate criterion;

#[global_allocator]
static ALLOC: jemallocator::Jemalloc = jemallocator::Jemalloc;

use ::combine::{stream::position, EasyParser};
use criterion::{black_box, Criterion, Throughput};
use parslers::parsler::Parsler;
use parslers_benchmark::{combine, nom};

mod optimised_parser {
    #![allow(warnings, unused)]
    include!(concat!(env!("OUT_DIR"), "/optimised.rs"));
}

mod reduced_parser {
    #![allow(warnings, unused)]
    include!(concat!(env!("OUT_DIR"), "/reduced.rs"));
}

mod usage_analysed_parser {
    #![allow(warnings, unused)]
    include!(concat!(env!("OUT_DIR"), "/usage_analysed.rs"));
}

mod unoptimised_parser {
    #![allow(warnings, unused)]
    include!(concat!(env!("OUT_DIR"), "/unoptimised.rs"));
}

static LOSTKNG: &str = include_str!("../LostKng.b");
fn branflakes(c: &mut Criterion) {
    // test once to make sure it parses correctly
    nom::brainfuck(LOSTKNG).unwrap();
    optimised_parser::brainfuck(&mut LOSTKNG.chars()).unwrap();
    optimised_parser::brainfuck_validate(&mut LOSTKNG.chars()).unwrap();

    parslers_branflakes::branflakes_parser(&LOSTKNG).unwrap();

    unoptimised_parser::brainfuck(black_box(&mut LOSTKNG.chars())).unwrap();
    reduced_parser::brainfuck(black_box(&mut LOSTKNG.chars())).unwrap();
    usage_analysed_parser::brainfuck(black_box(&mut LOSTKNG.chars())).unwrap();

    let unstaged_branflakes = parslers_benchmark::parslers::brainfuck_program();
    unstaged_branflakes.parse(&mut LOSTKNG.chars()).unwrap();

    let mut group = c.benchmark_group("branflakes");

    group.throughput(Throughput::Bytes(LOSTKNG.as_bytes().len() as u64));

    group.bench_function("handwritten", |b| {
        b.iter(|| parslers_branflakes::branflakes_parser(black_box(&LOSTKNG)).unwrap());
    });

    group.bench_function("handwritten_validate", |b| {
        b.iter(|| parslers_branflakes::branflakes_parser_validate(black_box(&LOSTKNG)).unwrap());
    });
    group.bench_function("nom", |b| {
        b.iter(|| nom::brainfuck(black_box(LOSTKNG)).unwrap());
    });

    group.bench_function("parslers", |b| {
        b.iter(|| optimised_parser::brainfuck(black_box(&mut LOSTKNG.chars())).unwrap());
    });

    group.bench_function("parslers_validate", |b| {
        b.iter(|| optimised_parser::brainfuck_validate(black_box(&mut LOSTKNG.chars())).unwrap());
    });

    group.bench_function("parslers_unoptimised", |b| {
        b.iter(|| unoptimised_parser::brainfuck(black_box(&mut LOSTKNG.chars())).unwrap());
    });

    group.bench_function("parslers_reduced", |b| {
        b.iter(|| reduced_parser::brainfuck(black_box(&mut LOSTKNG.chars())).unwrap());
    });

    group.bench_function("parslers_usage_analysed", |b| {
        b.iter(|| usage_analysed_parser::brainfuck(black_box(&mut LOSTKNG.chars())).unwrap());
    });

    group.bench_function("parslers_unstaged", |b| {
        b.iter(|| {
            unstaged_branflakes
                .parse(black_box(&mut LOSTKNG.chars()))
                .unwrap()
        });
    });
    group.finish();
}

criterion_group!(benches, branflakes);
criterion_main!(benches);

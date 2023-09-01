#[macro_use]
extern crate criterion;

#[global_allocator]
static ALLOC: jemallocator::Jemalloc = jemallocator::Jemalloc;

use ::combine::{stream::position, EasyParser};
use criterion::{black_box, Criterion, Throughput};
use parslers_benchmark::{combine, nom};

use parslers::parsler::Parsler;
use pest_grammars::json::*;

use pest::Parser;

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

static CANADA: &str = include_str!("../canada.json");
fn canada_json(c: &mut Criterion) {
    // test once to make sure it parses correctly
    nom::json(CANADA).unwrap();
    optimised_parser::json(&mut CANADA.chars()).unwrap();
    optimised_parser::json_validate(&mut CANADA.chars()).unwrap();
    combine::json_value()
        .easy_parse(position::Stream::new(&CANADA[..]))
        .unwrap();

    let unstaged_json = parslers_benchmark::parslers::json();
    unstaged_json.parse(&mut CANADA.chars()).unwrap();

    let mut group = c.benchmark_group("json canada");

    group.throughput(Throughput::Bytes(CANADA.as_bytes().len() as u64));

    // println!("data:\n{:?}", json(data));
    group.bench_function("nom", |b| {
        b.iter(|| nom::json(black_box(CANADA)).unwrap());
    });

    group.bench_function("parslers", |b| {
        b.iter(|| optimised_parser::json(black_box(&mut CANADA.chars())).unwrap());
    });

    group.bench_function("parslers_unoptimised", |b| {
        b.iter(|| unoptimised_parser::json(black_box(&mut CANADA.chars())).unwrap());
    });

    group.bench_function("parslers_reduced", |b| {
        b.iter(|| reduced_parser::json(black_box(&mut CANADA.chars())).unwrap());
    });

    group.bench_function("parslers_usage_analysed", |b| {
        b.iter(|| usage_analysed_parser::json(black_box(&mut CANADA.chars())).unwrap());
    });

    group.bench_function("parslers_validate", |b| {
        b.iter(|| optimised_parser::json_validate(black_box(&mut CANADA.chars())).unwrap());
    });

    group.bench_function("serde_json", |b| {
        b.iter(|| serde_json::from_str::<serde_json::Value>(black_box(CANADA)).unwrap());
    });
    group.bench_function("combine", |b| {
        b.iter(|| {
            combine::json_value()
                .easy_parse(position::Stream::new(&CANADA[..]))
                .unwrap()
        });
    });

    group.bench_function("parslers_unstaged", |b| {
        b.iter(|| unstaged_json.parse(black_box(&mut CANADA.chars())).unwrap());
    });
    group.bench_function("pest", |b| {
        b.iter(|| JsonParser::parse(Rule::json, CANADA).unwrap())
    });

    group.finish();
}

criterion_group!(benches, canada_json);
criterion_main!(benches);

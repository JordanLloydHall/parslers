#![allow(incomplete_features)]
#![feature(
    unboxed_closures,
    fn_traits,
    lazy_cell,
    impl_trait_in_fn_trait_return,
    impl_trait_in_assoc_type
)]

use auxiliary::*;
use parslers::builder::Builder;
use parslers::parsler::*;
use parslers::reflect::*;
use parslers_branflakes::Branflakes;
use parslers_macro::reflect;

#[reflect]
fn json_bool(b: bool) -> parslers_json::Json {
    parslers_json::Json::Bool(b)
}

#[reflect]
fn json_string(s: String) -> parslers_json::Json {
    parslers_json::Json::String(s)
}

#[reflect]
fn json_number(f: f64) -> parslers_json::Json {
    parslers_json::Json::Number(f)
}

#[reflect]
fn json_array(a: Vec<parslers_json::Json>) -> parslers_json::Json {
    parslers_json::Json::Array(a)
}

#[reflect]
fn json_object(a: std::collections::HashMap<String, parslers_json::Json>) -> parslers_json::Json {
    parslers_json::Json::Object(a)
}

#[reflect]
fn zip(a: String) -> Box<dyn FnOnce(parslers_json::Json) -> (String, parslers_json::Json)> {
    Box::new(move |b| (a, b))
}

#[reflect]
fn parse_double(s: String) -> f64 {
    fast_float::parse(s).unwrap()
}

#[reflect]
fn digit(c: char) -> bool {
    ('0'..='9').contains(&c)
}

#[reflect]
fn digit19(c: char) -> bool {
    ('1'..='9').contains(&c)
}

fn array() -> impl Parsler<Output = parslers_json::Json> + Clone {
    ws(match_char('['))
        .then(many(json().before(opt(ws(match_char(','))))).map(append))
        .ap(json().map(option).or(pure(None)))
        .before(ws(match_char(']')))
        .map(json_array)
}

fn string() -> impl Parsler<Output = String> + Clone {
    match_char('\"')
        .then(Recognise(many(not('\"'))))
        .before(ws(match_char('\"')))
}

fn object_item() -> impl Parsler<Output = (String, parslers_json::Json)> + Clone {
    string().before(ws(match_char(':'))).map(zip).ap(json())
}

#[reflect]
pub fn append<A: 'static>(mut v: Vec<A>) -> impl FnOnce(Option<A>) -> Vec<A> {
    move |mut a| {
        if let Some(a) = a.take() {
            v.push(a);
        }
        v
    }
}

#[reflect]
pub fn option<A>(a: A) -> Option<A> {
    Some(a)
}

fn number() -> impl Parsler<Output = f64> + Clone {
    Recognise(
        opt(match_char('-'))
            .then(
                match_char('0')
                    .then(pure(()))
                    .or(Satisfy(digit19).then(many(Satisfy(digit))).then(pure(()))),
            )
            .then(opt(match_char('.').then(many(Satisfy(digit)))))
            .then(opt(match_char('e')
                .or(match_char('E'))
                .then(opt(match_char('-').or(match_char('+'))))
                .then(many(Satisfy(digit))))),
    )
    .map(parse_double)
}

#[reflect]
fn parse_usize(s: String) -> usize {
    s.parse().unwrap()
}

fn object() -> impl Parsler<Output = parslers_json::Json> + Clone {
    ws(match_char('{'))
        .then(many_map((object_item()).before(opt(ws(match_char(','))))).map(insert_opt))
        .ap(object_item().map(option).or(pure(None)))
        .before(ws(match_char('}')))
        .map(json_object)
}

fn json() -> impl Parsler<Output = parslers_json::Json> + Clone {
    let boolean = ws(tag("true"))
        .then(pure(true))
        .or(ws(tag("false")).then(pure(false)))
        .map(json_bool);

    let null = ws(tag("null")).then(pure(parslers_json::Json::Null));

    let number = ws(number()).map(json_number);

    name("json", || {
        null.or(boolean)
            .or(string().map(json_string))
            .or(number)
            .or(array())
            .or(object())
    })
}

#[reflect]
fn brainfuck_val(
    p: Vec<parslers_branflakes::Branflakes>,
) -> parslers_branflakes::BranflakesProgram {
    parslers_branflakes::BranflakesProgram(p)
}
#[reflect]
fn brainfuck_loop(p: parslers_branflakes::BranflakesProgram) -> parslers_branflakes::Branflakes {
    parslers_branflakes::Branflakes::Loop(p)
}

fn brainfuck_program() -> impl Parsler<Output = parslers_branflakes::BranflakesProgram> + Clone {
    let left = match_char('<').then(pure(Branflakes::Left));
    let right = match_char('>').then(pure(Branflakes::Right));
    let add = match_char('+').then(pure(Branflakes::Add));
    let sub = match_char('-').then(pure(Branflakes::Sub));
    let print = match_char('.').then(pure(Branflakes::Print));
    let read = match_char(',').then(pure(Branflakes::Read));
    let loop_ = || {
        match_char('[')
            .then(brainfuck_program())
            .map(brainfuck_loop)
            .before(match_char(']'))
    };
    name("brainfuck", move || {
        many(
            left.or(right)
                .or(add)
                .or(sub)
                .or(print)
                .or(read)
                .or(loop_()),
        )
        .map(brainfuck_val)
    })
}

fn main() {
    let out_dir = std::env::var("OUT_DIR").unwrap();

    Builder::new("json", json())
        .add_parser("json_validate", json().then(pure(())))
        .add_parser("brainfuck", brainfuck_program())
        .add_parser("brainfuck_validate", brainfuck_program().then(pure(())))
        .reduce()
        .usage_analysis()
        .build(&format!("{out_dir}/optimised.rs"));

    Builder::new("json", json())
        .add_parser("brainfuck", brainfuck_program())
        .reduce()
        .build(&format!("{out_dir}/reduced.rs"));

    Builder::new("json", json())
        .add_parser("brainfuck", brainfuck_program())
        .usage_analysis()
        .build(&format!("{out_dir}/usage_analysed.rs"));

    Builder::new("json", json())
        .add_parser("brainfuck", brainfuck_program())
        .build(&format!("{out_dir}/unoptimised.rs"));
}

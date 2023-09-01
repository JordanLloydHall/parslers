#![allow(incomplete_features)]
#![feature(
    unboxed_closures,
    fn_traits,
    type_name_of_val,
    lazy_cell,
    impl_trait_in_fn_trait_return,
    return_position_impl_trait_in_trait,
    impl_trait_in_assoc_type
)]

use parslers::{
    builder::Builder,
    parsler::{auxiliary::*, *},
    reflect::*,
};
use parslers_macro::reflect;

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

#[reflect]
fn zip(a: String) -> Box<dyn FnOnce(parslers_json::Json) -> (String, parslers_json::Json)> {
    Box::new(move |b| (a, b))
}

#[reflect]
fn json_array(a: Vec<parslers_json::Json>) -> parslers_json::Json {
    parslers_json::Json::Array(a)
}

#[reflect]
fn json_object(a: std::collections::HashMap<String, parslers_json::Json>) -> parslers_json::Json {
    parslers_json::Json::Object(a)
}

// fn separated_list<P>(p: P, sep: char) -> impl Parsler<Output = Vec<P::Output>> + Clone
// where
//     P: Parsler + Clone + 'static,
//     P::Output: Reflect + Clone + 'static,
// {
//     many(p.clone().before(opt(ws(match_char(',')))))
//         .map(append)
//         .ap(p.map(option).or(pure(None)))
// }

fn array() -> impl Parsler<Output = parslers_json::Json> + Clone {
    ws(match_char('['))
        .then(many(json().before(opt(ws(match_char(','))))).map(append))
        .ap(json().map(option).or(pure(None)))
        .before(ws(match_char(']')))
        .map(json_array)
}

fn object() -> impl Parsler<Output = parslers_json::Json> + Clone {
    ws(match_char('{'))
        .then(many_map((object_item()).before(opt(ws(match_char(','))))).map(insert_opt))
        .ap(object_item().map(option).or(pure(None)))
        .before(ws(match_char('}')))
        .map(json_object)
}

fn object_item() -> impl Parsler<Output = (String, parslers_json::Json)> + Clone {
    string().before(ws(match_char(':'))).map(zip).ap(json())
}

fn json() -> impl Parsler<Output = parslers_json::Json> + Clone {
    let null = ws(tag("null")).then(pure(parslers_json::Json::Null));

    name("json", || null.or(array()).or(object()))
}

fn string() -> impl Parsler<Output = String> + Clone {
    match_char('\"')
        .then(Recognise(many(not('\"'))))
        .before(ws(match_char('\"')))
}

fn main() {
    let out_dir = std::env::var("OUT_DIR").unwrap();
    Builder::new("json", json())
        .reduce()
        .usage_analysis()
        .build(&format!("{out_dir}/json.rs"));
}

use std::collections::HashMap;

use parslers::reflect::Reflect;

use parslers_macro::Reflected;

#[derive(Clone, Debug, Reflected)]
pub enum Json {
    Null,
    Bool(bool),
    Number(f64),
    String(String),
    Array(Vec<Json>),
    Object(HashMap<String, Json>),
}

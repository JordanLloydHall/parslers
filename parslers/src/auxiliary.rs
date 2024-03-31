use std::{collections::HashMap, rc::Rc};

use parslers_reflect::Reflect;

use crate::{f, parser::*};

pub fn opt<'a>(p: impl Parser<'a> + Clone) -> impl Parser<'a, Output = ()> + Clone {
    Or(p.then(PureVal::new(())), PureVal::new(()))
}

pub fn ws<'a, P: Parser<'a> + Clone>(p: P) -> impl Parser<'a, Output = P::Output> + Clone {
    p.before(many(one_of(" \t\n\r".chars())))
}

pub fn tag<'a>(s: &'static str) -> impl Parser<'a, Output = &'static str> + Clone {
    let mut iter = s
        .chars()
        .map(|c| Rc::new(match_char(c)) as Rc<dyn Parser<'a, Output = char>>);

    let first = iter.next().unwrap();
    let folded = iter.fold(first, |a, b| Rc::new(a.then(b)));
    folded.then(PureVal::new(s))
}

pub fn one_of<'a>(
    chars: impl IntoIterator<Item = char>,
) -> std::rc::Rc<dyn Parser<'a, Output = char> + 'a> {
    chars
        .into_iter()
        .map(|c| Rc::new(match_char(c)) as Rc<dyn Parser<Output = char>>)
        .reduce(|a, b| Rc::new(a.or(b)))
        .unwrap()
}

pub fn insert_opt<A: Eq + std::hash::Hash, B>(
    mut v: std::collections::HashMap<A, B>,
) -> impl FnOnce(Option<(A, B)>) -> std::collections::HashMap<A, B> {
    move |a| {
        if let Some(a) = a {
            v.insert(a.0, a.1);
        }
        v
    }
}

pub fn many_map<
    'a,
    A: Clone + Reflect + Eq + std::hash::Hash + 'a,
    B: Clone + Reflect + 'a,
    P: Parser<'a, Output = (A, B)> + Clone,
>(
    p: P,
) -> impl Parser<'a, Output = HashMap<A, B>> + Clone {
    Loop(PureVal::new(HashMap::new()), p.map(f!(insert::<A, B>)))
}

pub fn insert<A: Eq + std::hash::Hash, B>(
    a: (A, B),
) -> impl FnOnce(std::collections::HashMap<A, B>) -> std::collections::HashMap<A, B> {
    move |mut v| {
        v.insert(a.0, a.1);
        v
    }
}

pub fn match_char<'a>(c: char) -> impl Parser<'a, Output = char> + Clone {
    let mut arr = [false; 256];
    arr[c as u32 as usize] = true;

    Satisfy(arr)
}

pub fn many<'a, P>(p: P) -> impl Parser<'a, Output = Vec<P::Output>> + Clone
where
    P: Parser<'a> + Clone + 'a,
    P::Output: Clone + Reflect,
{
    let p = p.map(f!(append::<P::Output>));
    let l = Loop(PureVal::new(vec![]), p);
    l
}

pub fn append<A>(a: A) -> impl FnOnce(Vec<A>) -> Vec<A> {
    move |mut v| {
        v.push(a);
        v
    }
}

pub fn dynamic<'a, P>(p: P) -> Rc<dyn Parser<'a, Output = P::Output> + 'a>
where
    P: Parser<'a> + 'a,
{
    Rc::new(p)
}

pub fn not<'a>(c: char) -> impl Parser<'a, Output = char> + Clone {
    let mut arr = [true; 256];
    arr[c as u32 as usize] = false;

    Satisfy(arr)
}

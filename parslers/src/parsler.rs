use std::collections::HashMap;
use std::{hash::Hasher, marker::PhantomData, rc::Rc, str::Chars};

use std::cmp::Eq;
use std::hash::Hash;

use either::Either;
use parslers_macro::reflect;

use crate::ast::AnalysedParser;
use crate::{ast, code_gen::CompileContext, reflect::Reflect};

impl std::hash::Hash for dyn DynHashable {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.dyn_hash(state)
    }
}

pub trait DynHashable {
    fn dyn_hash(&self, hasher: &mut dyn Hasher);
}

impl<H: Hash + ?Sized> DynHashable for H {
    fn dyn_hash(&self, mut state: &mut dyn Hasher) {
        self.hash(&mut state);
    }
}

impl<T> Hash for dyn Parsler<Output = T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.dyn_hash(state);
    }
}

pub trait Parsler: DynHashable {
    type Output;

    fn parse(&self, input: &mut Chars) -> Result<Self::Output, String>;
    fn compile(&self, info: &mut CompileContext) -> ast::Parser;

    fn ap<B, C, P>(self, p: P) -> Ap<C, Self::Output, P, Self>
    where
        Self: Sized,
        Self::Output: FnOnce(B) -> C,
        P: Parsler<Output = B>,
    {
        Ap(self, p)
    }

    fn map<B, F>(self, f: F) -> Map<B, Self, F>
    where
        Self: Sized,
        F: FnOnce(Self::Output) -> B + Clone + Reflect,
    {
        Map(self, f)
    }

    fn then<P>(self, p: P) -> Then<Self, P>
    where
        Self: Sized,
        P: Parsler,
    {
        Then(self, p)
    }

    fn or<P>(self, p: P) -> Or<Self, P>
    where
        Self: Sized,
        P: Parsler<Output = Self::Output>,
    {
        Or(self, p)
    }

    fn before<P>(self, p: P) -> Before<Self, P>
    where
        Self: Sized,
        P: Parsler,
    {
        Before(self, p)
    }

    fn attempt(self) -> Attempt<Self>
    where
        Self: Sized,
    {
        Attempt(self)
    }

    fn look(self) -> Look<Self>
    where
        Self: Sized,
    {
        Look(self)
    }

    fn neg_look(self) -> NegLook<Self>
    where
        Self: Sized,
    {
        NegLook(self)
    }
}

pub fn name<P: Parsler + 'static>(
    name: &str,
    f: impl FnOnce() -> P + Clone + 'static,
) -> Rc<dyn Parsler<Output = P::Output>> {
    Rc::new(NamedParser(LazyParser::new(f), name.to_owned()))
}

#[derive(Clone, Debug)]
pub struct LazyParser<P, F>(std::cell::OnceCell<P>, F)
where
    P: Parsler,
    F: FnOnce() -> P + Clone;

impl<P: Parsler, F: FnOnce() -> P + Clone> LazyParser<P, F> {
    pub fn new(f: F) -> Self {
        Self(std::cell::OnceCell::new(), f)
    }
}

impl<P: Parsler, F: FnOnce() -> P + Clone> Hash for LazyParser<P, F> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        std::any::type_name::<Self>().hash(state);
    }
}

impl<P: Parsler, F: FnOnce() -> P + Clone> Parsler for LazyParser<P, F> {
    type Output = P::Output;

    fn parse(&self, input: &mut Chars) -> Result<Self::Output, String> {
        self.0.get_or_init(self.1.clone()).parse(input)
    }

    fn compile(&self, info: &mut CompileContext) -> ast::Parser {
        self.0.get_or_init(self.1.clone()).compile(info)
    }
}

#[derive(Clone, Debug)]
pub struct Satisfy<F: FnOnce(char) -> bool + Reflect + Clone>(pub F);

impl<F: FnOnce(char) -> bool + Reflect + Clone> Hash for Satisfy<F> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.reflect().hash(state);
    }
}

impl<F: FnOnce(char) -> bool + Reflect + Clone> Parsler for Satisfy<F> {
    type Output = char;

    fn parse(&self, input: &mut Chars) -> Result<Self::Output, String> {
        let old_input = input.clone();
        if let Some(c) = input.next() {
            if self.0.clone()(c) {
                return Ok(c);
            }
        }
        *input = old_input;
        Err(format!("Expected satisfy {:?}", self.0.name()))
    }

    fn compile(&self, info: &mut CompileContext) -> ast::Parser {
        let name = info.add_function(&self.0);

        ast::Parser::Satisfy(ast::Func { name })
    }
}

#[derive(Debug)]
pub struct Ap<B, F: FnOnce(P1::Output) -> B, P1: Parsler, P2: Parsler<Output = F>>(pub P2, pub P1);

impl<B, F: FnOnce(P1::Output) -> B, P1: Parsler + Clone, P2: Parsler<Output = F> + Clone> Clone
    for Ap<B, F, P1, P2>
{
    fn clone(&self) -> Self {
        Self(self.0.clone(), self.1.clone())
    }
}

impl<B, F: FnOnce(P1::Output) -> B, P1: Parsler, P2: Parsler<Output = F>> Hash
    for Ap<B, F, P1, P2>
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.dyn_hash(state);
        self.1.dyn_hash(state);
    }
}

impl<B, F: FnOnce(P1::Output) -> B, P1: Parsler, P2: Parsler<Output = F>> Parsler
    for Ap<B, F, P1, P2>
{
    type Output = B;
    fn parse(&self, input: &mut Chars) -> Result<Self::Output, String> {
        let f = self.0.parse(input)?;
        let a = self.1.parse(input)?;
        Ok(f(a))
    }

    fn compile(&self, info: &mut CompileContext) -> ast::Parser {
        let left = self.1.compile(info);
        let right = self.0.compile(info);

        ast::Parser::Ap(
            Box::new(AnalysedParser::new(left)),
            Box::new(AnalysedParser::new(right)),
        )
    }
}

#[derive(Debug)]
pub struct Map<B, P1, F>(pub P1, pub F)
where
    P1: Parsler,
    F: FnOnce(P1::Output) -> B + Clone + Reflect;

impl<B, P1: Parsler + Clone, F: FnOnce(P1::Output) -> B + Clone + Reflect> Clone for Map<B, P1, F> {
    fn clone(&self) -> Self {
        Self(self.0.clone(), self.1.clone())
    }
}

impl<B, P1: Parsler, F: FnOnce(P1::Output) -> B + Clone + Reflect> Hash for Map<B, P1, F> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.dyn_hash(state);
        self.1.reflect().dyn_hash(state);
    }
}

impl<B, P1, F> Parsler for Map<B, P1, F>
where
    P1: Parsler,
    F: FnOnce(P1::Output) -> B + Clone + Reflect,
{
    type Output = B;
    fn parse(&self, input: &mut Chars) -> Result<Self::Output, String> {
        let a = self.0.parse(input)?;
        Ok(self.1.clone()(a))
    }

    fn compile(&self, info: &mut CompileContext) -> ast::Parser {
        let name = info.add_function(&self.1);
        let p = self.0.compile(info);

        ast::Parser::Ap(
            Box::new(AnalysedParser::new(p)),
            Box::new(AnalysedParser::new(ast::Parser::Pure(ast::PureVal::Func(
                ast::Func { name },
            )))),
        )
    }
}

#[derive(Clone, Debug)]
pub struct Then<P1: Parsler, P2: Parsler>(pub P1, pub P2);

impl<P1: Parsler, P2: Parsler> Hash for Then<P1, P2> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.dyn_hash(state);
        self.1.dyn_hash(state);
    }
}

impl<P1: Parsler, P2: Parsler> Parsler for Then<P1, P2> {
    type Output = P2::Output;
    fn parse(&self, input: &mut Chars) -> Result<Self::Output, String> {
        let _ = self.0.parse(input)?;
        let b = self.1.parse(input)?;
        Ok(b)
    }

    fn compile(&self, info: &mut CompileContext) -> ast::Parser {
        let left = self.0.compile(info);
        let right = self.1.compile(info);

        ast::Parser::Then(
            Box::new(AnalysedParser::new(left)),
            Box::new(AnalysedParser::new(right)),
        )
    }
}

#[derive(Clone, Debug)]
pub struct Before<P1: Parsler, P2: Parsler>(pub P1, pub P2);

impl<P1: Parsler, P2: Parsler> Hash for Before<P1, P2> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.dyn_hash(state);
        self.1.dyn_hash(state);
    }
}

impl<P1: Parsler, P2: Parsler> Parsler for Before<P1, P2> {
    type Output = P1::Output;
    fn parse(&self, input: &mut Chars) -> Result<Self::Output, String> {
        let a = self.0.parse(input)?;
        let _ = self.1.parse(input)?;
        Ok(a)
    }

    fn compile(&self, info: &mut CompileContext) -> ast::Parser {
        let left = self.0.compile(info);
        let right = self.1.compile(info);

        ast::Parser::Before(
            Box::new(AnalysedParser::new(left)),
            Box::new(AnalysedParser::new(right)),
        )
    }
}

#[derive(Clone, Debug)]
pub struct Or<P1: Parsler, P2: Parsler<Output = P1::Output>>(pub P1, pub P2);

impl<P1: Parsler, P2: Parsler<Output = P1::Output>> Hash for Or<P1, P2> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.dyn_hash(state);
        self.1.dyn_hash(state);
    }
}

impl<P1: Parsler, P2: Parsler<Output = P1::Output>> Parsler for Or<P1, P2> {
    type Output = P1::Output;
    fn parse(&self, input: &mut Chars) -> Result<Self::Output, String> {
        match self.0.parse(input) {
            Err(_) => self.1.parse(input),
            other => other,
        }
    }

    fn compile(&self, info: &mut CompileContext) -> ast::Parser {
        let left = self.0.compile(info);
        let right = self.1.compile(info);

        ast::Parser::Or(
            Box::new(AnalysedParser::new(left)),
            Box::new(AnalysedParser::new(right)),
        )
    }
}

#[derive(Debug)]
pub struct Branch<
    L,
    R,
    O,
    F1: FnOnce(L) -> O + Reflect,
    F2: FnOnce(R) -> O + Reflect,
    P1: Parsler<Output = Either<L, R>>,
    P2: Parsler<Output = F1>,
    P3: Parsler<Output = F2>,
>(pub P1, pub P2, pub P3);

impl<
        L,
        R,
        O,
        F1: FnOnce(L) -> O + Reflect,
        F2: FnOnce(R) -> O + Reflect,
        P1: Parsler<Output = Either<L, R>> + Clone,
        P2: Parsler<Output = F1> + Clone,
        P3: Parsler<Output = F2> + Clone,
    > Clone for Branch<L, R, O, F1, F2, P1, P2, P3>
{
    fn clone(&self) -> Self {
        Branch(self.0.clone(), self.1.clone(), self.2.clone())
    }
}

impl<
        L,
        R,
        O,
        F1: FnOnce(L) -> O + Reflect,
        F2: FnOnce(R) -> O + Reflect,
        P1: Parsler<Output = Either<L, R>>,
        P2: Parsler<Output = F1>,
        P3: Parsler<Output = F2>,
    > Hash for Branch<L, R, O, F1, F2, P1, P2, P3>
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.dyn_hash(state);
        self.1.dyn_hash(state);
        self.2.dyn_hash(state);
    }
}

impl<
        L,
        R,
        O,
        F1: FnOnce(L) -> O + Reflect,
        F2: FnOnce(R) -> O + Reflect,
        P1: Parsler<Output = Either<L, R>>,
        P2: Parsler<Output = F1>,
        P3: Parsler<Output = F2>,
    > Parsler for Branch<L, R, O, F1, F2, P1, P2, P3>
{
    type Output = O;
    fn parse(&self, input: &mut Chars) -> Result<Self::Output, String> {
        match self.0.parse(input)? {
            Either::Left(l) => self.1.parse(input).map(|f| f(l)),
            Either::Right(r) => self.2.parse(input).map(|f| f(r)),
        }
    }

    fn compile(&self, info: &mut CompileContext) -> ast::Parser {
        let branch = self.0.compile(info);
        let left = self.1.compile(info);
        let right = self.2.compile(info);

        ast::Parser::Branch(
            Box::new(AnalysedParser::new(branch)),
            Box::new(AnalysedParser::new(left)),
            Box::new(AnalysedParser::new(right)),
        )
    }
}

#[derive(Debug)]
pub struct Empty<O>(pub PhantomData<O>);

impl<O> Clone for Empty<O> {
    fn clone(&self) -> Self {
        Empty(PhantomData)
    }
}

impl<O> Default for Empty<O> {
    fn default() -> Self {
        Empty(PhantomData)
    }
}

impl<O> Hash for Empty<O> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        std::any::type_name::<Self>().hash(state);
    }
}

impl<O> Parsler for Empty<O> {
    type Output = O;
    fn parse(&self, _input: &mut Chars) -> Result<Self::Output, String> {
        Err("Expected empty".to_owned())
    }

    fn compile(&self, _info: &mut CompileContext) -> ast::Parser {
        ast::Parser::Empty
    }
}

#[derive(Clone, Debug)]
pub struct Attempt<P1: Parsler>(pub P1);

impl<P1: Parsler> Hash for Attempt<P1> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.dyn_hash(state);
    }
}

impl<P1: Parsler> Parsler for Attempt<P1> {
    type Output = P1::Output;
    fn parse(&self, input: &mut Chars) -> Result<Self::Output, String> {
        let copied_input = input.clone();
        let a = self.0.parse(input);
        if a.is_err() {
            *input = copied_input;
        }
        a
    }

    fn compile(&self, info: &mut CompileContext) -> ast::Parser {
        let p = self.0.compile(info);
        ast::Parser::Try(Box::new(AnalysedParser::new(p)))
    }
}

pub struct Look<P1: Parsler>(pub P1);

impl<P1: Parsler> Hash for Look<P1> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.dyn_hash(state);
    }
}

impl<P1: Parsler> Parsler for Look<P1> {
    type Output = P1::Output;
    fn parse(&self, input: &mut Chars) -> Result<Self::Output, String> {
        let a = self.0.parse(input)?;
        Ok(a)
    }

    fn compile(&self, info: &mut CompileContext) -> ast::Parser {
        let p = self.0.compile(info);
        ast::Parser::Look(Box::new(AnalysedParser::new(p)))
    }
}

pub struct NegLook<P1: Parsler>(pub P1);

impl<P1: Parsler> Hash for NegLook<P1> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.dyn_hash(state);
    }
}

impl<P1: Parsler<Output = ()>> Parsler for NegLook<P1> {
    type Output = ();
    fn parse(&self, input: &mut Chars) -> Result<Self::Output, String> {
        match self.0.parse(input) {
            Ok(_) => Err("Expected neg look".to_owned()),
            Err(_) => Ok(()),
        }
    }

    fn compile(&self, info: &mut CompileContext) -> ast::Parser {
        let p = self.0.compile(info);
        ast::Parser::NegLook(Box::new(AnalysedParser::new(p)))
    }
}

#[derive(Clone, Debug)]
pub struct Pure<A: Reflect>(pub A);

impl<A: Reflect> Hash for Pure<A> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.reflect().hash(state);
    }
}

impl<A: Clone + Reflect> Parsler for Pure<A> {
    type Output = A;
    fn parse(&self, _input: &mut Chars) -> Result<Self::Output, String> {
        let val = self.0.clone();
        Ok(val)
    }

    fn compile(&self, info: &mut CompileContext) -> ast::Parser {
        if let Ok(_) = syn::parse_str::<syn::ItemFn>(&self.0.reflect()) {
            let name = info.add_function(&self.0);
            ast::Parser::Pure(ast::PureVal::Func(ast::Func { name }))
        } else {
            ast::Parser::Pure(ast::PureVal::Val(self.0.reflect()))
        }
    }
}

pub fn pure<A: Reflect>(a: A) -> Pure<A> {
    Pure(a)
}

#[derive(Clone, Debug)]
pub struct NamedParser<P: Parsler>(pub P, pub String);

impl<P: Parsler> Hash for NamedParser<P> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.dyn_hash(state);
        self.1.dyn_hash(state)
    }
}

fn hash(h: impl Hash) -> u64 {
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    h.hash(&mut hasher);
    hasher.finish()
}

use crate::code_gen::NamedParserStatus;

impl<P: Parsler> Parsler for NamedParser<P> {
    type Output = P::Output;

    fn parse(&self, input: &mut Chars) -> Result<Self::Output, String> {
        self.0.parse(input)
    }

    fn compile(&self, info: &mut CompileContext) -> ast::Parser {
        let hash = hash(self);
        let name = format!("{}_{}", self.1, hash);
        match info.register_parser(&name) {
            NamedParserStatus::Registered => ast::Parser::Ident(name),
            NamedParserStatus::Unregistered => {
                let p = self.0.compile(info);
                info.insert_parser::<P>(&name, p.clone());
                ast::Parser::Ident(name)
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct Loop<F, P0, PN>(pub P0, pub PN)
where
    F: FnOnce(P0::Output) -> P0::Output,
    P0: Parsler,
    PN: Parsler<Output = F>;

impl<F: FnOnce(P0::Output) -> P0::Output, P0: Parsler, PN: Parsler<Output = F>> Hash
    for Loop<F, P0, PN>
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.dyn_hash(state);
        self.1.dyn_hash(state);
    }
}

impl<F, P0, PN> Parsler for Loop<F, P0, PN>
where
    F: FnOnce(P0::Output) -> P0::Output,
    P0: Parsler,
    PN: Parsler<Output = F>,
{
    type Output = P0::Output;

    fn parse(&self, input: &mut Chars) -> Result<Self::Output, String> {
        let mut v = self.0.parse(input)?;
        while let Ok(a) = self.1.parse(input) {
            v = a(v);
        }
        Ok(v)
    }

    fn compile(&self, info: &mut CompileContext) -> ast::Parser {
        let p0 = self.0.compile(info);
        let pn = self.1.compile(info);
        ast::Parser::Loop(
            Box::new(AnalysedParser::new(p0)),
            Box::new(AnalysedParser::new(pn)),
        )
    }
}

#[derive(Clone, Debug)]
pub struct Recognise<P: Parsler>(pub P);

impl<P: Parsler> Hash for Recognise<P> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.dyn_hash(state);
    }
}

impl<P: Parsler> Parsler for Recognise<P> {
    type Output = String;

    fn parse(&self, input: &mut Chars) -> Result<Self::Output, String> {
        let start = input.clone().as_str();
        let _ = self.0.parse(input)?;
        Ok(start[..start.len() - input.as_str().len()].to_owned())
    }

    fn compile(&self, info: &mut CompileContext) -> ast::Parser {
        let p = self.0.compile(info);
        ast::Parser::Recognise(Box::new(AnalysedParser::new(p)))
    }
}

impl<P: Parsler + ?Sized + Hash> Parsler for Box<P> {
    type Output = P::Output;
    fn parse(&self, input: &mut Chars) -> Result<Self::Output, String> {
        self.as_ref().parse(input)
    }

    fn compile(&self, info: &mut CompileContext) -> ast::Parser {
        self.as_ref().compile(info)
    }
}

impl<P: Parsler + ?Sized + Hash> Parsler for Rc<P> {
    type Output = P::Output;
    fn parse(&self, input: &mut Chars) -> Result<Self::Output, String> {
        self.as_ref().parse(input)
    }

    fn compile(&self, info: &mut CompileContext) -> ast::Parser {
        self.as_ref().compile(info)
    }
}

pub mod auxiliary {
    use super::*;
    #[reflect]
    pub fn cons<A: 'static>(a: A) -> impl FnOnce(Vec<A>) -> Vec<A> {
        move |mut v| {
            v.push(a);
            v
        }
    }

    #[reflect]
    pub fn append<A: 'static>(a: A) -> impl FnOnce(Vec<A>) -> Vec<A> {
        move |mut v| {
            v.push(a);
            v
        }
    }

    #[reflect]
    pub fn insert<A: 'static + Eq + std::hash::Hash, B: 'static>(
        a: (A, B),
    ) -> impl FnOnce(std::collections::HashMap<A, B>) -> std::collections::HashMap<A, B> {
        move |mut v| {
            v.insert(a.0, a.1);
            v
        }
    }

    #[reflect]
    pub fn insert_opt<A: 'static + Eq + std::hash::Hash, B: 'static>(
        mut v: std::collections::HashMap<A, B>,
    ) -> impl FnOnce(Option<(A, B)>) -> std::collections::HashMap<A, B> {
        move |a| {
            if let Some(a) = a {
                v.insert(a.0, a.1);
            }
            v
        }
    }

    #[reflect]
    fn singleton<A>(a: A) -> Vec<A> {
        vec![a]
    }

    #[reflect]
    fn concat<A: 'static>(mut v: Vec<A>) -> impl FnOnce(Vec<A>) -> Vec<A> {
        move |mut a| {
            v.append(&mut a);
            v
        }
    }

    #[reflect]
    fn reverse<A>(mut v: Vec<A>) -> Vec<A> {
        v.reverse();
        v
    }

    pub fn some_rev<P>(p: P) -> Rc<dyn Parsler<Output = Vec<P::Output>>>
    where
        P: Parsler + Clone + 'static,
        P::Output: Clone + 'static + Reflect,
    {
        name("some_rev", || p.clone().map(append).ap(many_rev(p)))
    }

    // #[reflect]
    // pub fn emtpy_vec<T>() -> Vec<T> {
    //     vec![]
    // }

    pub fn many_rev<P>(p: P) -> Rc<dyn Parsler<Output = Vec<P::Output>>>
    where
        P: Parsler + Clone + 'static,
        P::Output: Clone + 'static + Reflect,
    {
        name("many_rev", || some_rev(p).or(pure(vec![])))
    }

    pub fn many<P>(p: P) -> Rc<dyn Parsler<Output = Vec<P::Output>>>
    where
        P: Parsler + Clone + 'static,
        P::Output: Clone + 'static + Reflect,
    {
        dynamic(Loop(pure(vec![]), Map(p, append)))
    }

    pub trait IntoPair<A, B> {
        fn into_pair(self) -> (A, B);
    }

    impl<A, B> IntoPair<A, B> for (A, B) {
        fn into_pair(self) -> (A, B) {
            self
        }
    }

    pub fn many_map<
        A: Clone + 'static + Reflect + Eq + Hash,
        B: Clone + 'static + Reflect,
        P: Parsler<Output = (A, B)> + Clone + 'static,
    >(
        p: P,
    ) -> impl Parsler<Output = HashMap<A, B>> + Clone {
        dynamic(Loop(pure(HashMap::new()), Map(p, insert)))
    }

    pub fn some<P>(p: P) -> Rc<dyn Parsler<Output = Vec<P::Output>>>
    where
        P: Parsler + Clone + 'static,
        P::Output: Clone + 'static,
    {
        dynamic(Loop(p.clone().map(singleton), pure(append).ap(p)))
    }

    #[reflect]
    fn singleton_map<A: 'static + Eq + std::hash::Hash, B: 'static>(
        a: (A, B),
    ) -> std::collections::HashMap<A, B> {
        let mut m = HashMap::new();
        m.insert(a.0, a.1);
        m
    }

    pub fn some_map<
        A: Clone + 'static + Reflect + Eq + Hash,
        B: Clone + 'static + Reflect,
        P: Parsler<Output = (A, B)> + Clone + 'static,
    >(
        p: P,
    ) -> impl Parsler<Output = HashMap<A, B>> + Clone {
        dynamic(Loop(p.clone().map(singleton_map), Map(p, insert)))
    }

    pub fn match_char(c: char) -> impl Parsler<Output = char> + Clone {
        #[derive(Copy, Clone, Debug, Hash)]
        struct CharMatch(char);
        impl Reflect for CharMatch {
            fn name(&self) -> &'static str {
                "match_char"
            }
            fn reflect(&self) -> String {
                format!("fn match_char(a: char) -> bool {{ a == {:?}}}", self.0)
            }
        }

        impl FnOnce<(char,)> for CharMatch {
            type Output = bool;

            extern "rust-call" fn call_once(self, (c,): (char,)) -> Self::Output {
                c == self.0
            }
        }

        Satisfy(CharMatch(c))
    }

    pub fn one_of(
        chars: impl IntoIterator<Item = char>,
    ) -> std::rc::Rc<dyn Parsler<Output = char>> {
        chars
            .into_iter()
            .map(|c| Rc::new(match_char(c)) as Rc<dyn Parsler<Output = char>>)
            .reduce(|a, b| Rc::new(a.or(b)))
            .unwrap()
    }

    pub fn not_one_of(
        chars: impl IntoIterator<Item = char>,
    ) -> std::rc::Rc<dyn Parsler<Output = char>> {
        chars
            .into_iter()
            .map(|c| Rc::new(not(c)) as Rc<dyn Parsler<Output = char>>)
            .reduce(|a, b| Rc::new(a.then(b)))
            .unwrap()
    }

    #[reflect]
    fn id<A>(a: A) -> A {
        a
    }

    #[allow(non_camel_case_types)]
    struct emty<A>(PhantomData<A>);

    impl<A> FnOnce<((),)> for emty<A> {
        type Output = A;
        extern "rust-call" fn call_once(self, _: ((),)) -> Self::Output {
            panic!("emty")
        }
    }

    impl<A> Reflect for emty<A> {
        fn name(&self) -> &'static str {
            "emty"
        }
        fn reflect(&self) -> String {
            format!(
                "fn emty(a: ()) -> {} {{ panic!(\"emty\") }}",
                std::any::type_name::<A>()
            )
        }
    }

    pub fn filtered_by<
        P: Parsler + Clone,
        F: FnOnce(&P::Output) -> bool + Reflect + Clone + Hash,
    >(
        p: P,
        f: F,
    ) -> impl Parsler<Output = P::Output> + Clone {
        struct Cond<O, F>(F, PhantomData<O>);

        impl<O, F: Clone> Clone for Cond<O, F> {
            fn clone(&self) -> Self {
                Cond(self.0.clone(), PhantomData)
            }
        }

        impl<O, F: Reflect + Clone + Hash> Hash for Cond<O, F> {
            fn hash<H: Hasher>(&self, state: &mut H) {
                self.0.hash(state);
            }
        }

        impl<I, F: FnOnce(&I) -> bool + Reflect> Reflect for Cond<I, F> {
            fn reflect(&self) -> String {
                format!(
                    "
                    fn cond(x: {}) -> either::Either<(), {}> {{
                        {}
                        if {{{}}}(&x) {{
                            either::Either::Right(x)
                        }} else {{
                            either::Either::Left(())
                        }}
                    }}
                    ",
                    std::any::type_name::<I>(),
                    std::any::type_name::<I>(),
                    self.0.reflect(),
                    self.0.name().split("::").last().unwrap(),
                )
                .to_owned()
            }
        }

        impl<O, F: FnOnce(&O) -> bool + Reflect> FnOnce<(O,)> for Cond<O, F> {
            type Output = either::Either<(), O>;

            extern "rust-call" fn call_once(self, (a,): (O,)) -> Self::Output {
                if (self.0)(&a) {
                    either::Either::Right(a)
                } else {
                    either::Either::Left(())
                }
            }
        }

        Branch(
            Ap(pure(Cond(f, PhantomData)), p),
            Empty::<emty<P::Output>>::default(),
            pure(id),
        )
    }

    #[reflect]
    pub fn collect_string(chars: Vec<char>) -> String {
        chars.into_iter().collect()
    }

    pub fn ident(
        is_not_keyword: impl FnOnce(&String) -> bool + Reflect + Clone + Hash,
    ) -> impl Parsler<Output = String> {
        filtered_by(some(one_of('a'..='z')).map(collect_string), is_not_keyword)
    }

    pub fn tag(s: &str) -> impl Parsler<Output = &str> + Clone {
        let mut iter = s
            .chars()
            .map(|c| Rc::new(match_char(c)) as Rc<dyn Parsler<Output = char>>);

        let first = iter.next().unwrap();
        iter.fold(first, |a, b| Rc::new(a.then(b))).then(pure(s))
    }

    pub fn not(c: char) -> impl Parsler<Output = char> + Clone {
        #[derive(Copy, Clone, Debug, Hash)]
        struct CharNot(char);
        impl Reflect for CharNot {
            fn name(&self) -> &'static str {
                "not"
            }
            fn reflect(&self) -> String {
                format!("fn not(a: char) -> bool {{ a != {:?}}}", self.0)
            }
        }

        impl FnOnce<(char,)> for CharNot {
            type Output = bool;

            extern "rust-call" fn call_once(self, (c,): (char,)) -> Self::Output {
                c != self.0
            }
        }

        Satisfy(CharNot(c))
    }

    pub fn opt(p: impl Parsler + Clone) -> impl Parsler<Output = ()> + Clone {
        Or(p.then(pure(())), pure(()))
    }

    pub fn ws<P: Parsler + Clone>(p: P) -> impl Parsler<Output = P::Output> + Clone {
        p.before(many(one_of(" \t\n\r".chars())))
    }

    

    pub fn dynamic<P>(p: P) -> Rc<dyn Parsler<Output = P::Output>>
    where
        P: Parsler + 'static,
    {
        Rc::new(p)
    }
}

#[cfg(test)]
mod tests {
    use super::{auxiliary::*, *};
    #[test]
    fn ident_works_correctly() {
        #[reflect]
        fn is_not_keyword(s: &String) -> bool {
            s != "abc"
        }
        let mut input = "abcd".chars();
        let result = ident(is_not_keyword).parse(&mut input);
        assert_eq!(result, Ok("abcd".to_owned()));
    }
}

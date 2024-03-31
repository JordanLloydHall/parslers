use std::collections::HashMap;

use proc_macro2::TokenStream;

use crate::parser::ItemContext;
use quote::quote;

pub(crate) struct CompileContext {
    pub(crate) recursive_parsers: HashMap<usize, Parser>,
}

impl CompileContext {
    pub(crate) fn new() -> Self {
        CompileContext {
            recursive_parsers: HashMap::new(),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub(crate) enum Parser {
    NamedParser(usize),
    PureVal(String),
    PureFun(ItemContext<()>),
    Satisfy([bool; 256]),
    Atomic(Box<Parser>),
    Look(Box<Parser>),
    NegLook(Box<Parser>),
    Ap(Box<Parser>, Box<Parser>),
    Then(Box<Parser>, Box<Parser>),
    Before(Box<Parser>, Box<Parser>),
    Or(Box<Parser>, Box<Parser>),
    Recognise(Box<Parser>),
    Empty,
    // Branch(
    //     Box<Parser>,
    //     Box<Parser>,
    //     Box<Parser>,
    // ),
    Loop(Box<Parser>, Box<Parser>),
}

impl Parser {
    pub fn compile(&self) -> TokenStream {
        match self {
            Parser::NamedParser(name) => {
                let ident = syn::parse_str::<syn::Expr>(&format!("p_{name}")).unwrap();
                quote! {#ident(input)}
            }
            Parser::PureVal(pure_val) => {
                let ident = syn::parse_str::<syn::Expr>(&pure_val).unwrap();
                quote! { Ok(#ident) }
            }
            Parser::PureFun(ctx) => {
                let ident = syn::parse_str::<syn::Expr>(&format!("{}::{}", ctx.module_path, ctx.import_path)).unwrap();
                quote! { Ok(#ident) }
            }
            Parser::Satisfy(arr) => {
                let vals = arr
                    .iter()
                    .enumerate()
                    .filter(|(_i, c)| **c)
                    .map(|(i, _c)| i as u8 as char);

                quote! {
                    let old_input = input.clone();
                    input.next()
                    .ok_or("Found EOF when character was expected")
                    .and_then(|c|
                        if #(c == #vals )||* {
                            Ok(c)
                        } else {
                            *input = old_input;
                            Err("Expected a specific character")
                        }
                    )
                }
            }
            Parser::Atomic(p) => {
                let parser = p.compile();
                quote! {
                    let copied_input = input.clone();
                    let result = {#parser};
                    if result.is_err() {
                        *input = copied_input;
                    }
                    result
                }
            }
            Parser::Look(p) => {
                let parser = p.compile();
                quote! {
                    let copied_input = input.clone();
                    let result = {#parser};
                    *input = copied_input;
                    result
                }
            }
            Parser::NegLook(p) => {
                let p = p.compile();
                quote! {
                    let copied_input = input.clone();
                    let result = {#p};
                    *input = copied_input;
                    if result.is_ok() {
                        Err("Expected negative look".to_owned())
                    } else {
                        Ok(())
                    }
                }
            }
            Parser::Ap(p, q) => {
                let p = p.compile();
                let q = q.compile();

                quote! {
                    {#q}.and_then(|f| {#p}.map(|x| f(x)))
                }
            }
            Parser::Then(p, q) => {
                let p = p.compile();
                let q = q.compile();

                quote! {
                    {#p}.and_then(|_| {#q})
                }
            }
            Parser::Before(p, q) => {
                let p = p.compile();
                let q = q.compile();

                quote! {
                    {#p}.and_then(|res| {#q}.map(|_| res))
                }
            }
            Parser::Or(p, q) => {
                let p = p.compile();
                let q = q.compile();

                quote! {
                    {#p}.or_else(|_: &'static str| {#q})
                }
            }
            Parser::Recognise(p) => {
                let parser = p.compile();

                quote! {
                    let start = input.clone().as_str();
                    {#parser}.map(|_| start[..start.len() - input.as_str().len()].to_owned())
                }
            }
            Parser::Empty => quote! { Err("Expected empty") },
            // Parser::Branch(b, l, r) => {
            //     let b1 = b.compile();
            //     let l1 = l.compile();
            //     let r1 = r.compile();

            //     let l: TokenStream = if l.returns_func {
            //         quote! { {#l1}.map(|f| f(l)) }
            //     } else {
            //         quote! { {#l1} }
            //     };

            //     let r = if r.returns_func {
            //         quote! { {#r1}.map(|f| f(r1)) }
            //     } else {
            //         quote! { {#r1} }
            //     };

            //     quote! {
            //         {#b1}.and_then(|b| match b {
            //             either::Either::Left(l) => #l,
            //             either::Either::Right(r) => #r,
            //         })
            //     }
            // }
            Parser::Loop(p0, pn) => {
                let p0 = p0.compile();
                let pn = pn.compile();
                quote! {
                    {#p0}.map(|mut v| {
                        while let Ok(x) = {#pn} {
                            v = x(v);
                        }
                        v
                    })
                }
            }
        }
    }
}

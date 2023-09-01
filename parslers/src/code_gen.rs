use std::collections::{HashMap, HashSet};

use crate::{
    ast::{self, AnalysedParser, Statement},
    parsler::Parsler,
    reflect::Reflect,
};
use proc_macro2::TokenStream;
use quote::quote;

pub struct CompileContext {
    functions: HashMap<String, String>,
    pub named_parsers: HashMap<String, Option<ast::Statement>>,
    pub parsers_with_unused: HashSet<String>,
}

pub enum NamedParserStatus {
    Registered,
    Unregistered,
}

impl CompileContext {
    pub fn new() -> Self {
        Self {
            functions: HashMap::new(),
            named_parsers: HashMap::new(),
            parsers_with_unused: HashSet::new(),
        }
    }

    pub fn add_function(&mut self, func: &impl Reflect) -> String {
        let num_functions = self.functions.len();
        self.functions
            .entry(func.reflect())
            .or_insert(format!("f{}", num_functions))
            .clone()
    }

    pub fn register_parser(&mut self, name: &str) -> NamedParserStatus {
        let res = self.named_parsers.get(name).cloned();
        if let None = res {
            self.named_parsers.insert(name.to_owned(), None);
        }
        match res {
            Some(_) => NamedParserStatus::Registered,
            None => NamedParserStatus::Unregistered,
        }
    }

    pub fn insert_parser<P: Parsler>(&mut self, name: &str, parser: ast::Parser) {
        let p = self.named_parsers.get_mut(name);

        let type_ = std::any::type_name::<P::Output>();
        let type_ = syn::parse_str(type_).unwrap();

        match p {
            Some(Some(_)) => panic!("Parser with name '{}' already exists", name),
            None => panic!("Parser with name '{}' was never registered", name),
            Some(p) => {
                *p = Some(Statement {
                    public: false,
                    ident: name.to_owned(),
                    type_,
                    parser: AnalysedParser::new(parser),
                })
            }
        }
    }

    pub fn register_unused_function(&mut self, name: String) -> String {
        let new_name = format!("{}_unused", name);
        self.parsers_with_unused.insert(name);
        new_name
    }

    pub fn optimise_named_parsers(&mut self) {
        for (_name, parser) in self.named_parsers.iter_mut() {
            if let Some(s) = parser {
                let _size = s.parser.size();
                let _depth = s.parser.depth();
                s.parser = s.parser.clone().reduce();
                // eprintln!(
                //     "Reduced parser '{}' from ({}, {}) to ({}, {})",
                //     s.ident,
                //     size,
                //     depth,
                //     s.parser.size(),
                //     s.parser.depth()
                // );
                s.parser
                    .output_used_analysis(true, &mut self.parsers_with_unused);
            }
        }

        let mut finished = HashSet::new();
        while finished.len() != self.parsers_with_unused.len() {
            let mut new = HashSet::new();
            for name in self.parsers_with_unused.iter() {
                let mut parser = self
                    .named_parsers
                    .get(name)
                    .unwrap()
                    .as_ref()
                    .unwrap()
                    .clone();
                let new_name = format!("{}_unused", name);

                let type_ = syn::parse_str::<syn::Type>("()").unwrap();
                parser.parser.output_used_analysis(false, &mut new);
                self.named_parsers.insert(
                    new_name.clone(),
                    Some(Statement {
                        public: false,
                        ident: new_name,
                        type_,
                        parser: parser.parser.clone(),
                    }),
                );
                finished.insert(name.clone());
            }
            self.parsers_with_unused = self.parsers_with_unused.union(&new).cloned().collect();
        }
    }
}

pub fn compile<P: Parsler>(name: &str, parser: P, context: &mut CompileContext) -> ast::Statement {
    let type_ = std::any::type_name::<P::Output>();
    let type_ = syn::parse_str(type_).unwrap();

    let parser = parser.compile(context);
    Statement {
        public: true,
        ident: name.to_owned(),
        type_,
        parser: AnalysedParser::new(parser),
    }
}

pub fn gen_statement(statements: &[ast::Statement], context: &mut CompileContext) -> TokenStream {
    let statements = statements
        .iter()
        .map(|statement| {
            let ident = syn::Ident::new(&statement.ident, proc_macro2::Span::call_site());
            let type_ = statement.type_.clone();
            let parser = statement.parser.compile();

            quote! {
                pub fn #ident(input: &mut std::str::Chars) -> Result<#type_, &'static str> {
                    #parser
                }
            }
        })
        .collect::<Vec<_>>();

    let functions = context
        .functions
        .iter()
        .map(|(body, name)| {
            let mut body = syn::parse_str::<syn::ItemFn>(body).unwrap();

            body.sig.ident = syn::parse_str::<syn::Ident>(name).unwrap();

            quote! {
                #[inline(always)]
                #body
            }
        })
        .collect::<Vec<_>>();

    let aux_parsers = context
        .named_parsers
        .iter()
        .map(|(name, parser)| {
            let (ts, ty) = match parser {
                None => panic!("Parser with name '{}' was never finished", name),
                Some(s) => (s.parser.compile(), s.type_.clone()),
            };

            let ident = syn::Ident::new(name, proc_macro2::Span::call_site());
            quote! {
                fn #ident(input: &mut std::str::Chars) -> Result<#ty, &'static str> {
                    #ts
                }
            }
        })
        .collect::<Vec<_>>();
    quote! {
        extern crate alloc;
        #[inline(always)]
        fn compose_<A, B, C, G, F>(g: G) -> impl FnOnce(F) -> (Box<dyn FnOnce(A) -> C>)
        where
            F: FnOnce(A) -> B + 'static,
            G: FnOnce(B) -> C + 'static,
        {
            move |f| Box::new(move |x| g(f(x)))
        }
        #(#functions)*
        #(#aux_parsers)*
        #(#statements)*
    }
}

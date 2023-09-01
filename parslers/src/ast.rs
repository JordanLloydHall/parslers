use std::collections::HashSet;

use proc_macro2::TokenStream;

use quote::quote;

#[derive(Debug, PartialEq, Clone)]
pub struct Spec {
    pub statements: Vec<Statement>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Statement {
    pub public: bool,
    pub ident: String,
    pub type_: syn::Type,
    pub parser: AnalysedParser,
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub struct AnalysedParser {
    pub output_used: bool,
    pub returns_func: bool,
    pub cut_point: Option<bool>,
    pub parser: Parser,
}

impl AnalysedParser {
    pub fn new(parser: Parser) -> Self {
        AnalysedParser {
            output_used: true,
            returns_func: true,
            cut_point: None,
            parser,
        }
    }

    pub fn size(&self) -> usize {
        self.parser.size()
    }

    pub fn depth(&self) -> usize {
        self.parser.depth()
    }

    pub fn reduce(self) -> Self {
        AnalysedParser {
            output_used: self.output_used,
            returns_func: self.returns_func,
            cut_point: self.cut_point,
            parser: self.parser.reduce_down().reduce_up(),
        }
    }

    pub fn returns_func_analysis(&mut self) -> bool {
        let returns_func = self.parser.returns_func_analysis();

        self.returns_func = returns_func;

        returns_func
    }

    pub fn output_used_analysis(&mut self, used: bool, ctx: &mut HashSet<String>) {
        self.parser.output_used_analysis(used, ctx);

        self.output_used = used;
    }

    pub fn compile(&self) -> TokenStream {
        if self.output_used {
            self.parser.compile_used(self.returns_func)
        } else {
            self.parser.compile_unused(self.returns_func)
        }
    }

    pub fn cut_analysis(&mut self, backtracks: bool) -> bool {
        match &mut self.parser {
            Parser::Ident(_) => false,
            Parser::Pure(_) => false,
            Parser::Satisfy(_) => {
                if backtracks {
                    self.cut_point = Some(!backtracks);
                }
                true
            }
            Parser::Try(p) => {
                let cut = p.cut_analysis(true);
                self.cut_point = Some(cut && !backtracks);
                cut
            }
            Parser::Look(p) => {
                p.cut_analysis(backtracks);
                self.cut_point = Some(false);
                false
            }
            Parser::NegLook(p) => {
                p.cut_analysis(true);
                self.cut_point = Some(false);
                false
            }
            Parser::Ap(p, q) => {
                let p_cut = p.cut_analysis(backtracks);
                let q_cut = q.cut_analysis(backtracks || p_cut);

                p_cut || q_cut
            }
            Parser::Then(p, q) => {
                let p_cut = p.cut_analysis(backtracks);
                let q_cut = q.cut_analysis(backtracks || p_cut);

                p_cut || q_cut
            }
            Parser::Before(p, q) => {
                let p_cut = p.cut_analysis(backtracks);
                let q_cut = q.cut_analysis(backtracks || p_cut);

                p_cut || q_cut
            }
            Parser::Or(_, q) => q.cut_analysis(backtracks),
            Parser::Recognise(p) => p.cut_analysis(backtracks),
            Parser::Empty => false,
            Parser::Branch(b, l, r) => {
                let b_cut = b.cut_analysis(backtracks);
                let l_cut = l.cut_analysis(backtracks || b_cut);
                let r_cut = r.cut_analysis(backtracks || b_cut);

                b_cut || (l_cut && r_cut)
            }

            Parser::Loop(p0, pn) => {
                let p0_cut = p0.cut_analysis(backtracks);
                let pn_cut = pn.cut_analysis(backtracks || p0_cut);

                p0_cut || pn_cut
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum Parser {
    Ident(String),
    Pure(PureVal),
    Satisfy(Func),
    Try(Box<AnalysedParser>),
    Look(Box<AnalysedParser>),
    NegLook(Box<AnalysedParser>),
    Ap(Box<AnalysedParser>, Box<AnalysedParser>),
    Then(Box<AnalysedParser>, Box<AnalysedParser>),
    Before(Box<AnalysedParser>, Box<AnalysedParser>),
    Or(Box<AnalysedParser>, Box<AnalysedParser>),
    Recognise(Box<AnalysedParser>),
    Empty,
    Branch(
        Box<AnalysedParser>,
        Box<AnalysedParser>,
        Box<AnalysedParser>,
    ),
    Loop(Box<AnalysedParser>, Box<AnalysedParser>),
}

impl Parser {
    pub fn size(&self) -> usize {
        match self {
            Parser::Pure(_) => 1,
            Parser::Satisfy(_) => 1,
            Parser::Try(p) => 1 + p.size(),
            Parser::Look(p) => 1 + p.size(),
            Parser::NegLook(p) => 1 + p.size(),
            Parser::Ap(p, q) => 1 + p.size() + q.size(),
            Parser::Then(p, q) => 1 + p.size() + q.size(),
            Parser::Before(p, q) => 1 + p.size() + q.size(),
            Parser::Or(p, q) => 1 + p.size() + q.size(),
            Parser::Empty => 1,
            Parser::Branch(p, q, r) => 1 + p.size() + q.size() + r.size(),
            Parser::Ident(_) => 1,
            Parser::Recognise(p) => 1 + p.size(),
            Parser::Loop(p0, pn) => 1 + p0.size() + pn.size(),
        }
    }

    pub fn depth(&self) -> usize {
        match self {
            Parser::Pure(_) => 1,
            Parser::Satisfy(_) => 1,
            Parser::Try(p) => 1 + p.depth(),
            Parser::Look(p) => 1 + p.depth(),
            Parser::NegLook(p) => 1 + p.depth(),
            Parser::Ap(p, q) => 1 + p.depth().max(q.depth()),
            Parser::Then(p, q) => 1 + p.depth().max(q.depth()),
            Parser::Before(p, q) => 1 + p.depth().max(q.depth()),
            Parser::Or(p, q) => 1 + p.depth().max(q.depth()),
            Parser::Empty => 1,
            Parser::Branch(p, q, r) => 1 + p.depth().max(q.depth()).max(r.depth()),
            Parser::Ident(_) => 1,
            Parser::Recognise(p) => 1 + p.depth(),
            Parser::Loop(p0, pn) => 1 + p0.depth().max(pn.depth()),
        }
    }

    pub fn reduce_up(self) -> Self {
        match self {
            Parser::Ap(
                box AnalysedParser {
                    parser: Parser::Pure(PureVal::Val(val)),
                    ..
                },
                box AnalysedParser {
                    parser: Parser::Pure(PureVal::Func(f)),
                    ..
                },
            ) => Parser::Pure(PureVal::Val(format!("{}({})", f.name, val))),
            Parser::Ap(
                box AnalysedParser {
                    parser: Parser::Pure(PureVal::Func(f2)),
                    ..
                },
                box AnalysedParser {
                    parser: Parser::Pure(PureVal::Func(f1)),
                    ..
                },
            ) => {
                Parser::Pure(PureVal::Func(Func {
                    name: format!("{}({})", f1.name, f2.name),
                }))
            }
            Parser::Ap(
                box AnalysedParser {
                    parser: Parser::Pure(PureVal::Val(val)),
                    ..
                },
                p,
            ) => Parser::Ap(
                p,
                Box::new(AnalysedParser::new(Parser::Pure(PureVal::Val(format!(
                    "|f| f({})",
                    val
                ))))),
            ),
            Parser::Or(
                box AnalysedParser {
                    parser: Parser::Or(box p, box q),
                    ..
                },
                box r,
            ) => Parser::Or(
                Box::new(AnalysedParser::new(p.parser.reduce_up())),
                Box::new(AnalysedParser::new(Parser::Or(
                    Box::new(AnalysedParser::new(q.parser.reduce_up())),
                    Box::new(AnalysedParser::new(r.parser.reduce_up())),
                ))),
            )
            .reduce_up(),
            Parser::Or(
                box AnalysedParser {
                    parser: Parser::Empty,
                    ..
                },
                box p,
            )
            | Parser::Or(
                box p,
                box AnalysedParser {
                    parser: Parser::Empty,
                    ..
                },
            ) => p.parser.reduce_up(),
            Parser::Ap(
                _,
                box AnalysedParser {
                    parser: Parser::Empty,
                    ..
                },
            ) => Parser::Empty,
            Parser::Or(
                box AnalysedParser {
                    parser: Parser::Pure(x),
                    ..
                },
                _,
            ) => Parser::Pure(x),
            Parser::Try(box AnalysedParser {
                parser: Parser::Pure(x),
                ..
            }) => Parser::Pure(x),
            Parser::Look(box AnalysedParser {
                parser: Parser::Empty,
                ..
            }) => Parser::Empty,
            Parser::Ap(
                box AnalysedParser {
                    parser: Parser::Ap(w, v),
                    ..
                },
                u,
            ) => Parser::Ap(
                w,
                Box::new(AnalysedParser::new(
                    Parser::Ap(
                        v,
                        Box::new(AnalysedParser::new(
                            Parser::Ap(
                                u,
                                Box::new(AnalysedParser::new(Parser::Pure(PureVal::Func(Func {
                                    name: "compose_".to_owned(),
                                })))),
                            )
                            .reduce_up(),
                        )),
                    )
                    .reduce_up(),
                )),
            )
            .reduce_up(),
            other => other,
        }
    }

    pub fn reduce_down(self) -> Self {
        let p = match self {
            Parser::Pure(p) => Parser::Pure(p),
            Parser::Satisfy(s) => Parser::Satisfy(s),
            Parser::Try(box p) => {
                Parser::Try(Box::new(AnalysedParser::new(p.parser.reduce_down())))
            }
            Parser::Look(box p) => {
                Parser::Look(Box::new(AnalysedParser::new(p.parser.reduce_down())))
            }
            Parser::NegLook(box p) => {
                Parser::NegLook(Box::new(AnalysedParser::new(p.parser.reduce_down())))
            }
            Parser::Ap(box p, box q) => Parser::Ap(
                Box::new(AnalysedParser::new(p.parser.reduce_down())),
                Box::new(AnalysedParser::new(q.parser.reduce_down())),
            ),
            Parser::Then(box p, box q) => Parser::Then(
                Box::new(AnalysedParser::new(p.parser.reduce_down())),
                Box::new(AnalysedParser::new(q.parser.reduce_down())),
            ),
            Parser::Before(box p, box q) => Parser::Before(
                Box::new(AnalysedParser::new(p.parser.reduce_down())),
                Box::new(AnalysedParser::new(q.parser.reduce_down())),
            ),
            Parser::Or(box p, box q) => Parser::Or(
                Box::new(AnalysedParser::new(p.parser.reduce_down())),
                Box::new(AnalysedParser::new(q.parser.reduce_down())),
            ),
            Parser::Empty => Parser::Empty,
            Parser::Branch(box p, box q, box r) => Parser::Branch(
                Box::new(AnalysedParser::new(p.parser.reduce_down())),
                Box::new(AnalysedParser::new(q.parser.reduce_down())),
                Box::new(AnalysedParser::new(r.parser.reduce_down())),
            ),
            Parser::Ident(id) => Parser::Ident(id),
            Parser::Recognise(box p) => {
                Parser::Recognise(Box::new(AnalysedParser::new(p.parser.reduce_down())))
            }
            Parser::Loop(box p0, box pn) => Parser::Loop(
                Box::new(AnalysedParser::new(p0.parser.reduce_down())),
                Box::new(AnalysedParser::new(pn.parser.reduce_down())),
            ),
        };

        p.reduce_up()
    }

    pub fn returns_func_analysis(&mut self) -> bool {
        match self {
            Parser::Ident(_f) => true,
            Parser::Pure(PureVal::Func(_)) => true,
            Parser::Pure(PureVal::Val(_)) => false,
            Parser::Satisfy(_) => false,
            Parser::Try(p) => p.returns_func_analysis(),
            Parser::Look(p) => p.returns_func_analysis(),
            Parser::NegLook(p) => {
                p.returns_func_analysis();
                false
            }
            Parser::Ap(p, q) => {
                p.returns_func_analysis();
                q.returns_func_analysis();
                true
            }
            Parser::Then(q, p) => {
                q.returns_func_analysis();
                p.returns_func_analysis()
            }
            Parser::Before(p, q) => {
                q.returns_func_analysis();
                p.returns_func_analysis()
            }
            Parser::Or(p, q) => q.returns_func_analysis() & p.returns_func_analysis(),
            Parser::Recognise(p) => {
                p.returns_func_analysis();
                false
            }
            Parser::Empty => false,
            Parser::Branch(b, l, r) => {
                b.returns_func_analysis();
                l.returns_func_analysis() & r.returns_func_analysis()
            }
            Parser::Loop(p0, pn) => {
                p0.returns_func_analysis();
                pn.returns_func_analysis();
                false
            }
        }
    }

    fn output_used_analysis(&mut self, used: bool, ctx: &mut HashSet<String>) {
        match self {
            // Parser::Ident(_) => todo!(),
            // Parser::Pure(_) => todo!(),
            // Parser::Satisfy(_) => todo!(),
            Parser::Try(p) => p.output_used_analysis(used, ctx),
            Parser::Look(p) => p.output_used_analysis(used, ctx),
            Parser::NegLook(p) => p.output_used_analysis(used, ctx),
            Parser::Ap(p, q) => {
                p.output_used_analysis(used, ctx);
                q.output_used_analysis(used, ctx);
            }
            Parser::Then(p, q) => {
                p.output_used_analysis(false, ctx);
                q.output_used_analysis(used, ctx);
            }
            Parser::Before(p, q) => {
                p.output_used_analysis(used, ctx);
                q.output_used_analysis(false, ctx);
            }
            Parser::Or(p, q) => {
                p.output_used_analysis(used, ctx);
                q.output_used_analysis(used, ctx);
            }
            Parser::Recognise(p) => {
                p.output_used_analysis(false, ctx);
            }
            // Parser::Empty => todo!(),
            Parser::Branch(b, l, r) => {
                b.output_used_analysis(true, ctx);
                l.output_used_analysis(used, ctx);
                r.output_used_analysis(used, ctx);
            }
            Parser::Ident(id) => {
                if !used {
                    let new_name = format!("{}_unused", id);
                    if !ctx.contains(id.strip_suffix("_unused").unwrap_or(id)) {
                        ctx.insert(id.clone());
                    }
                    *id = new_name;
                }
                // println!("{} {}, {:?}", id, used, ctx);
            }
            Parser::Loop(p0, pn) => {
                p0.output_used_analysis(used, ctx);
                pn.output_used_analysis(used, ctx);
            }
            _ => {}
        }
    }

    pub fn compile_used(&self, _returns_func: bool) -> TokenStream {
        match self {
            Parser::Ident(s) => {
                let ident = syn::parse_str::<syn::Expr>(s).unwrap();
                quote! {
                    #ident(input)
                }
            }
            Parser::Pure(pure_val) => match pure_val {
                PureVal::Val(val) => {
                    let val = syn::parse_str::<syn::Expr>(&dbg!(val)).unwrap();
                    quote! { Ok(#val) }
                }
                PureVal::Func(Func { name }) => {
                    let ident = syn::parse_str::<syn::Expr>(&name).unwrap();
                    quote! { Ok(#ident) }
                }
            },
            Parser::Satisfy(ident) => {
                let ident = syn::parse_str::<syn::Expr>(&ident.name).unwrap();
                quote! {
                    let old_input = input.clone();
                    input.next()
                    .ok_or("Found EOF when character was expected")
                    .and_then(|c|
                        if (#ident)(c) {
                            Ok(c)
                        } else {
                            *input = old_input;
                            Err("Expected a specific character")
                        }
                    )
                }
            }
            Parser::Try(p) => {
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
            Parser::Branch(b, l, r) => {
                let b1 = b.compile();
                let l1 = l.compile();
                let r1 = r.compile();

                let l: TokenStream = if l.returns_func {
                    quote! { {#l1}.map(|f| f(l)) }
                } else {
                    quote! { {#l1} }
                };

                let r = if r.returns_func {
                    quote! { {#r1}.map(|f| f(r1)) }
                } else {
                    quote! { {#r1} }
                };

                quote! {
                    {#b1}.and_then(|b| match b {
                        either::Either::Left(l) => #l,
                        either::Either::Right(r) => #r,
                    })
                }
            }
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

    pub fn compile_unused(&self, _returns_func: bool) -> TokenStream {
        match self {
            Parser::Ident(s) => {
                let ident = syn::parse_str::<syn::Expr>(s).unwrap();
                quote! {
                    #ident(input)
                }
            }
            Parser::Pure(pure_val) => match pure_val {
                PureVal::Val(_val) => {
                    quote! { Ok(()) }
                }
                PureVal::Func(Func { name: _ }) => {
                    quote! { Ok(()) }
                }
            },
            Parser::Satisfy(ident) => {
                let ident = syn::parse_str::<syn::Expr>(&ident.name).unwrap();
                quote! {
                    let old_input = input.clone();
                    input.next()
                    .ok_or_else(|| "Found EOF when character was expected")
                    .and_then(|c|
                        if (#ident)(c) {
                            Ok(())
                        } else {
                            *input = old_input;
                            Err("expected a specific character")
                        }
                    )
                }
            }
            Parser::Try(p) => {
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
                    {#q}.and_then(|_| {#p})
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
                    {#p}.and_then(|_| {#q})
                }
            }
            Parser::Or(p, q) => {
                let p = p.compile();
                let q = q.compile();

                quote! {
                    {#p}.or_else(|_| {#q})
                }
            }
            Parser::Recognise(p) => {
                let parser = p.compile();

                quote! {
                    {#parser}
                }
            }
            Parser::Empty => quote! { Err("Expected empty") },
            Parser::Branch(b, l, r) => {
                let b1 = b.compile();
                let l1 = l.compile();
                let r1 = r.compile();

                quote! {
                    {#b1}.and_then(|b| match b {
                        either::Either::Left(l) => {#l1},
                        either::Either::Right(r) => {#r1},
                    })
                }
            }
            Parser::Loop(p0, pn) => {
                let p0 = p0.compile();
                let pn = pn.compile();
                quote! {
                    {#p0}.map(|v| {
                        while let Ok(x) = {#pn} {}
                        ()
                    })
                }
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum PureVal {
    Val(String),
    Func(Func),
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub struct Func {
    pub name: String,
}

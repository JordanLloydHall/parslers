use std::{
    rc::{Rc, Weak},
    str::Chars,
};

use parslers_reflect::Reflect;

use super::compiler;
pub trait Parser<'a> {
    type Output;

    fn compile(&self, context: &mut compiler::CompileContext) -> compiler::Parser;

    fn parse(&self, input: &mut Chars<'a>) -> Result<Self::Output, String>;

    fn ap<B, C, P>(self, p: P) -> Ap<P, Self>
    where
        Self: Sized,
        Self::Output: FnOnce(B) -> C,
        P: Parser<'a, Output = B>,
    {
        Ap(self, p)
    }

    fn map<B>(self, f: ItemContext<fn(Self::Output) -> B>) -> Map<Self, fn(Self::Output) -> B>
    where
        Self: Sized,
    {
        Map(self, f)
    }

    fn then<P>(self, p: P) -> Then<Self, P>
    where
        Self: Sized,
        P: Parser<'a>,
    {
        Then(self, p)
    }

    fn or<P>(self, p: P) -> Or<Self, P>
    where
        Self: Sized,
        P: Parser<'a, Output = Self::Output>,
    {
        Or(self, p)
    }

    fn before<P>(self, p: P) -> Before<Self, P>
    where
        Self: Sized,
        P: Parser<'a>,
    {
        Before(self, p)
    }

    fn attempt(self) -> Atomic<Self>
    where
        Self: Sized,
    {
        Atomic(self)
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

#[derive(Clone, Debug)]
pub struct Satisfy(pub [bool; 256]);

impl<'a> Parser<'a> for Satisfy {
    type Output = char;

    fn parse(&self, input: &mut Chars<'a>) -> Result<Self::Output, String> {
        let old_input = input.clone();
        if let Some(c) = input.next() {
            if self.0[c as u32 as usize] {
                return Ok(c);
            }
        }
        *input = old_input;
        Err(format!("Expected satisfy"))
    }

    fn compile(&self, _context: &mut compiler::CompileContext) -> compiler::Parser {
        compiler::Parser::Satisfy(self.0)
    }
}

#[derive(Debug, Clone)]
pub struct Ap<P1, P2>(pub P2, pub P1);

impl<
        'a,
        B,
        F: FnOnce(<P1 as Parser<'a>>::Output) -> B,
        P1: Parser<'a>,
        P2: Parser<'a, Output = F>,
    > Parser<'a> for Ap<P1, P2>
{
    type Output = B;
    fn parse(&self, input: &mut Chars<'a>) -> Result<Self::Output, String> {
        let f = self.0.parse(input)?;
        let a = self.1.parse(input)?;
        Ok(f(a))
    }

    fn compile(&self, context: &mut compiler::CompileContext) -> compiler::Parser {
        compiler::Parser::Ap(
            Box::new(self.0.compile(context)),
            Box::new(self.1.compile(context)),
        )
    }
}

#[derive(Debug, Clone)]
pub struct Map<P1, F>(pub P1, pub ItemContext<F>);

impl<'a, B, P1: Parser<'a>> Parser<'a> for Map<P1, fn(P1::Output) -> B> {
    type Output = B;
    fn parse(&self, input: &mut Chars<'a>) -> Result<Self::Output, String> {
        let a = self.0.parse(input)?;
        Ok((self.1.t)(a))
    }

    fn compile(&self, context: &mut compiler::CompileContext) -> compiler::Parser {
        let p = self.0.compile(context);

        compiler::Parser::Ap(
            Box::new(p),
            Box::new(compiler::Parser::PureFun(ItemContext {
                import_path: self.1.import_path,
                crate_name: self.1.crate_name,
                module_path: self.1.module_path,
                type_name: self.1.type_name,
                t: (),
            })),
        )
    }
}

#[derive(Clone, Debug)]
pub struct Then<P1, P2>(pub P1, pub P2);

impl<'a, P1: Parser<'a>, P2: Parser<'a>> Parser<'a> for Then<P1, P2> {
    type Output = P2::Output;
    fn parse(&self, input: &mut Chars<'a>) -> Result<Self::Output, String> {
        self.0.parse(input)?;
        self.1.parse(input)
    }

    fn compile(&self, context: &mut compiler::CompileContext) -> compiler::Parser {
        compiler::Parser::Then(
            Box::new(self.0.compile(context)),
            Box::new(self.1.compile(context)),
        )
    }
}

#[derive(Clone, Debug)]
pub struct Before<P1, P2>(pub P1, pub P2);

impl<'a, P1: Parser<'a>, P2: Parser<'a>> Parser<'a> for Before<P1, P2> {
    type Output = P1::Output;
    fn parse(&self, input: &mut Chars<'a>) -> Result<Self::Output, String> {
        let output = self.0.parse(input)?;
        self.1.parse(input)?;
        Ok(output)
    }

    fn compile(&self, context: &mut compiler::CompileContext) -> compiler::Parser {
        compiler::Parser::Before(
            Box::new(self.0.compile(context)),
            Box::new(self.1.compile(context)),
        )
    }
}

#[derive(Clone, Debug)]
pub struct Or<P1, P2>(pub P1, pub P2);

impl<'a, P1: Parser<'a>, P2: Parser<'a, Output = P1::Output>> Parser<'a> for Or<P1, P2> {
    type Output = P1::Output;
    fn parse(&self, input: &mut Chars<'a>) -> Result<Self::Output, String> {
        match self.0.parse(input) {
            Err(_) => self.1.parse(input),
            other => other,
        }
    }

    fn compile(&self, context: &mut compiler::CompileContext) -> compiler::Parser {
        compiler::Parser::Or(
            Box::new(self.0.compile(context)),
            Box::new(self.1.compile(context)),
        )
    }
}

// #[derive(Debug)]
// pub struct Branch<
//     L,
//     R,
//     O,
//     F1: FnOnce(L) -> O,
//     F2: FnOnce(R) -> O,
//     P1: for<'a> Parser<Output<'a> = Either<L, R>>,
//     P2: for<'a> Parser<Output<'a> = F1>,
//     P3: for<'a> Parser<Output<'a> = F2>,
// >(pub P1, pub P2, pub P3);

#[derive(Debug, Clone)]
pub struct Empty;

impl<'a> Parser<'a> for Empty {
    type Output = ();
    fn parse(&self, _input: &mut Chars<'a>) -> Result<Self::Output, String> {
        Err("Expected Empty".to_owned())
    }

    fn compile(&self, _context: &mut compiler::CompileContext) -> compiler::Parser {
        compiler::Parser::Empty
    }
}

#[derive(Clone, Debug)]
pub struct Atomic<P1>(pub P1);

impl<'a, P: Parser<'a>> Parser<'a> for Atomic<P> {
    type Output = P::Output;
    fn parse(&self, input: &mut Chars<'a>) -> Result<Self::Output, String> {
        let copied_input = input.clone();
        let a = self.0.parse(input);
        if a.is_err() {
            *input = copied_input;
        }
        a
    }

    fn compile(&self, context: &mut compiler::CompileContext) -> compiler::Parser {
        compiler::Parser::Atomic(Box::new(self.0.compile(context)))
    }
}

#[derive(Clone, Debug)]
pub struct Look<P1>(pub P1);

impl<'a, P: Parser<'a>> Parser<'a> for Look<P> {
    type Output = ();
    fn parse(&self, input: &mut Chars<'a>) -> Result<Self::Output, String> {
        let old = input.clone();
        self.0.parse(input)?;
        *input = old;
        Ok(())
    }

    fn compile(&self, context: &mut compiler::CompileContext) -> compiler::Parser {
        compiler::Parser::Look(Box::new(self.0.compile(context)))
    }
}

#[derive(Clone, Debug)]

pub struct NegLook<P1>(pub P1);

impl<'a, P: Parser<'a>> Parser<'a> for NegLook<P> {
    type Output = ();
    fn parse(&self, input: &mut Chars<'a>) -> Result<Self::Output, String> {
        let copied_input = input.clone();
        let result = self.0.parse(input);
        *input = copied_input;
        if result.is_ok() {
            Err("Expected negative look".to_owned())
        } else {
            Ok(())
        }
    }

    fn compile(&self, context: &mut compiler::CompileContext) -> compiler::Parser {
        compiler::Parser::NegLook(Box::new(self.0.compile(context)))
    }
}

#[derive(Debug, Clone)]
pub struct PureVal<A: Clone + Reflect>(pub A);

impl<A: Clone + Reflect> PureVal<A> {
    pub fn new(a: A) -> Self {
        Self(a.clone())
    }
}

impl<'a, A: Clone + Reflect> Parser<'a> for PureVal<A> {
    type Output = A;
    fn parse(&self, _input: &mut Chars<'a>) -> Result<Self::Output, String> {
        Ok(self.0.clone())
    }

    fn compile(&self, _context: &mut compiler::CompileContext) -> compiler::Parser {
        compiler::Parser::PureVal(self.0.reflect())
    }
}

#[derive(Debug)]
pub struct PureFun<A, B>(pub ItemContext<fn(A) -> B>);

impl<'a, A, B> Parser<'a> for PureFun<A, B> {
    type Output = fn(A) -> B;
    fn parse(&self, _input: &mut Chars<'a>) -> Result<Self::Output, String> {
        Ok(self.0.t)
    }

    fn compile(&self, _context: &mut compiler::CompileContext) -> compiler::Parser {
        compiler::Parser::PureFun(ItemContext {
            import_path: self.0.import_path,
            crate_name: self.0.crate_name,
            module_path: self.0.module_path,
            type_name: self.0.type_name,
            t: (),
        })
    }
}

impl<A, B> Clone for PureFun<A, B> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

#[derive(Clone, Debug)]
pub struct Recognise<P>(pub P);

impl<'a, P: Parser<'a>> Parser<'a> for Recognise<P> {
    type Output = &'a str;

    fn parse(&self, input: &mut Chars<'a>) -> Result<Self::Output, String> {
        let start = input.clone().as_str();
        let _ = self.0.parse(input)?;
        Ok(&start[..start.len() - input.as_str().len()])
    }

    fn compile(&self, context: &mut compiler::CompileContext) -> compiler::Parser {
        compiler::Parser::Recognise(Box::new(self.0.compile(context)))
    }
}

// impl<'a, P: Parser<'a> + ?Sized + Clone> Parser<'a> for Box<P> {
//     type Output = P::Output;
//     fn parse(&self, input: &mut Chars<'a>) -> Result<Self::Output, String> {
//         self.as_ref().parse(input)
//     }

//     fn compile(&self, context: &mut compiler::CompileContext) -> compiler::Parser {
//         self.as_ref().compile(context)
//     }
// }

impl<'a, P: Parser<'a> + ?Sized> Parser<'a> for Rc<P> {
    type Output = P::Output;
    fn parse(&self, input: &mut Chars<'a>) -> Result<Self::Output, String> {
        self.as_ref().parse(input)
    }

    fn compile(&self, context: &mut compiler::CompileContext) -> compiler::Parser {
        if Rc::<P>::weak_count(&self) != 0 {
            let ptr = Rc::<P>::as_ptr(&self) as *const () as usize;
            if !context.recursive_parsers.contains_key(&ptr) {
                let compiled = self.as_ref().compile(context);
                context.recursive_parsers.insert(ptr, compiled);
            }

            compiler::Parser::NamedParser(ptr)
        } else {
            self.as_ref().compile(context)
        }
    }
}

#[derive(Clone, Debug)]
pub struct Loop<P0, PN>(pub P0, pub PN);

impl<'a, F, P0, PN> Parser<'a> for Loop<P0, PN>
where
    F: FnOnce(P0::Output) -> P0::Output,
    P0: Parser<'a>,
    PN: Parser<'a, Output = F>,
{
    type Output = P0::Output;

    fn parse(&self, input: &mut Chars<'a>) -> Result<Self::Output, String> {
        let mut v = self.0.parse(input)?;
        while let Ok(a) = self.1.parse(input) {
            v = a(v);
        }
        Ok(v)
    }
    fn compile(&self, context: &mut compiler::CompileContext) -> compiler::Parser {
        compiler::Parser::Loop(
            Box::new(self.0.compile(context)),
            Box::new(self.1.compile(context)),
        )
    }
}

// #[derive(Clone, Debug)]
// pub struct Recurse<P>(Rc<P>);

// impl<P> Recurse<P> {
//     pub fn new()
// }

// impl<'a, P: Parser<'a>> Parser<'a> for Recurse<P> {
//     type Output = P::Output;

//     fn compile(&self, context: &mut compiler::CompileContext) -> compiler::Parser {
//         todo!()
//     }

//     fn parse(&self, input: &mut Chars<'a>) -> Result<Self::Output, String> {
//         self.0.as_ref().parse(input)
//     }
// }

// struct NamedParser

// fn named(name: &str, f: F) ->

impl<'a, P: Parser<'a> + ?Sized> Parser<'a> for Weak<P> {
    type Output = P::Output;

    fn compile(&self, _context: &mut compiler::CompileContext) -> compiler::Parser {
        compiler::Parser::NamedParser(self.as_ptr() as *const () as usize)
    }

    fn parse(&self, input: &mut Chars<'a>) -> Result<Self::Output, String> {
        self.upgrade()
            .map(|p| p.parse(input))
            .unwrap_or(Err("Parser not found".to_owned()))
    }
}

#[macro_export]
macro_rules! f {
        ($p:expr) => {{
            ItemContext {
                import_path: stringify!($p),
                crate_name: ::std::env!("CARGO_PKG_NAME", "Failed to load the CARGO_PKG_NAME environment variable. Are you using a custom build system?"),
                module_path: module_path!(),
                type_name: ::std::any::type_name_of_val(&$p),
                t: $p
            }
        }};
    }

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct ItemContext<T> {
    pub import_path: &'static str,
    pub crate_name: &'static str,
    pub module_path: &'static str,
    pub type_name: &'static str,
    pub t: T,
}

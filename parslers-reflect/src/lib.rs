use std::{collections::HashMap, rc::Rc};

pub trait Reflect {
    fn name(&self) -> &'static str {
        std::any::type_name::<Self>()
    }

    fn reflect(&self) -> String;
}

impl Reflect for Box<dyn Reflect> {
    fn reflect(&self) -> String {
        format!("Box::new({})", self.as_ref().reflect())
    }
}

impl Reflect for char {
    fn reflect(&self) -> String {
        format!("'{}'", self)
    }
}

impl<K: Reflect, V: Reflect> Reflect for HashMap<K, V> {
    fn reflect(&self) -> String {
        format!(
            "std::collections::HashMap::from([{}])",
            self.iter()
                .map(|(k, v)| format!("({}, {})", k.reflect(), v.reflect()))
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

impl Reflect for String {
    fn reflect(&self) -> String {
        format!("\"{}\".to_owned()", self)
    }
}

impl Reflect for str {
    fn reflect(&self) -> String {
        format!("\"{}\"", self)
    }
}

impl<T: Reflect + ?Sized> Reflect for &T {
    fn reflect(&self) -> String {
        (*self).reflect()
    }
}

impl<A, B> Reflect for (A, B)
where
    A: Reflect,
    B: Reflect,
{
    fn reflect(&self) -> String {
        format!("({}, {})", self.0.reflect(), self.1.reflect())
    }
}

impl<A> Reflect for Vec<A>
where
    A: Reflect,
{
    fn reflect(&self) -> String {
        format!(
            "Vec::<{}>::from([{}])",
            std::any::type_name::<A>(),
            self.iter()
                .map(|a| a.reflect())
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

impl<A> Reflect for Box<A>
where
    A: Reflect,
{
    fn reflect(&self) -> String {
        format!("Box::new({})", self.as_ref().reflect())
    }
}

impl<A> Reflect for Rc<A>
where
    A: Reflect,
{
    fn reflect(&self) -> String {
        format!("Rc::new({})", self.as_ref().reflect())
    }
}

impl<A> Reflect for Option<A>
where
    A: Reflect,
{
    fn reflect(&self) -> String {
        match self {
            Some(a) => format!("Some({})", a.reflect()),
            None => "None".to_owned(),
        }
    }
}

impl<A, B> Reflect for either::Either<A, B>
where
    A: Reflect,
    B: Reflect,
{
    fn reflect(&self) -> String {
        match self {
            either::Either::Left(a) => format!("either::Either::Left({})", a.reflect()),
            either::Either::Right(b) => format!("either::Either::Right({})", b.reflect()),
        }
    }
}

impl Reflect for u8 {
    fn reflect(&self) -> String {
        format!("{}u8", self)
    }
}

impl Reflect for u16 {
    fn reflect(&self) -> String {
        format!("{}u16", self)
    }
}

impl Reflect for u32 {
    fn reflect(&self) -> String {
        format!("{}u32", self)
    }
}

impl Reflect for u64 {
    fn reflect(&self) -> String {
        format!("{}u64", self)
    }
}

impl Reflect for u128 {
    fn reflect(&self) -> String {
        format!("{}u128", self)
    }
}

impl Reflect for usize {
    fn reflect(&self) -> String {
        format!("{}usize", self)
    }
}

impl Reflect for i8 {
    fn reflect(&self) -> String {
        format!("{}i8", self)
    }
}

impl Reflect for i16 {
    fn reflect(&self) -> String {
        format!("{}i16", self)
    }
}

impl Reflect for i32 {
    fn reflect(&self) -> String {
        format!("{}i32", self)
    }
}

impl Reflect for i64 {
    fn reflect(&self) -> String {
        format!("{}i64", self)
    }
}

impl Reflect for i128 {
    fn reflect(&self) -> String {
        format!("{}i128", self)
    }
}

impl Reflect for isize {
    fn reflect(&self) -> String {
        format!("{}isize", self)
    }
}

impl Reflect for f32 {
    fn reflect(&self) -> String {
        format!("{}f32", self)
    }
}

impl Reflect for f64 {
    fn reflect(&self) -> String {
        format!("{}f64", self)
    }
}

impl Reflect for bool {
    fn reflect(&self) -> String {
        format!("{}", self)
    }
}

impl<const N: usize, A> Reflect for [A; N]
where
    A: Reflect,
{
    fn reflect(&self) -> String {
        format!(
            "[{}]",
            self.iter()
                .map(|a| a.reflect())
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

impl Reflect for () {
    fn reflect(&self) -> String {
        "()".to_owned()
    }
}

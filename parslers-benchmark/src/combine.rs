use std::collections::HashMap;

use combine::{
    error::{Commit, ParseError},
    parser,
    parser::{
        char::{char, digit, spaces, string},
        choice::{choice, optional},
        repeat::{many, many1, sep_by},
        sequence::between,
        token::{any, satisfy, satisfy_map},
    },
    Parser, Stream, StreamOnce,
};

#[derive(PartialEq, Debug)]
pub enum Value {
    Number(f64),
    String(String),
    Bool(bool),
    Null,
    Object(HashMap<String, Value>),
    Array(Vec<Value>),
}

fn lex<Input, P>(p: P) -> impl Parser<Input, Output = P::Output>
where
    P: Parser<Input>,
    Input: Stream<Token = char>,
    <Input as StreamOnce>::Error: ParseError<
        <Input as StreamOnce>::Token,
        <Input as StreamOnce>::Range,
        <Input as StreamOnce>::Position,
    >,
{
    p.skip(spaces())
}

fn integer<Input>() -> impl Parser<Input, Output = i64>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    lex(many1(digit()))
        .map(|s: String| {
            let mut n = 0;
            for c in s.chars() {
                n = n * 10 + (c as i64 - '0' as i64);
            }
            n
        })
        .expected("integer")
}

fn number<Input>() -> impl Parser<Input, Output = f64>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    let i = char('0').map(|_| 0.0).or(integer().map(|x| x as f64));
    let fractional = many(digit()).map(|digits: String| {
        let mut magnitude = 1.0;
        digits.chars().fold(0.0, |acc, d| {
            magnitude /= 10.0;
            match d.to_digit(10) {
                Some(d) => acc + (d as f64) * magnitude,
                None => panic!("Not a digit"),
            }
        })
    });

    let exp = satisfy(|c| c == 'e' || c == 'E').with(optional(char('-')).and(integer()));
    lex(optional(char('-'))
        .and(i)
        .map(|(sign, n)| if sign.is_some() { -n } else { n })
        .and(optional(char('.')).with(fractional))
        .map(|(x, y)| if x >= 0.0 { x + y } else { x - y })
        .and(optional(exp))
        .map(|(n, exp_option)| match exp_option {
            Some((sign, e)) => {
                let e = if sign.is_some() { -e } else { e };
                n * 10.0f64.powi(e as i32)
            }
            None => n,
        }))
    .expected("number")
}

fn json_char<Input>() -> impl Parser<Input, Output = char>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    parser(|input: &mut Input| {
        let (c, committed) = any().parse_lazy(input).into_result()?;
        let mut back_slash_char = satisfy_map(|c| {
            Some(match c {
                '"' => '"',
                '\\' => '\\',
                '/' => '/',
                'b' => '\u{0008}',
                'f' => '\u{000c}',
                'n' => '\n',
                'r' => '\r',
                't' => '\t',
                _ => return None,
            })
        });
        match c {
            '\\' => committed.combine(|_| back_slash_char.parse_stream(input).into_result()),
            '"' => Err(Commit::Peek(Input::Error::empty(input.position()).into())),
            _ => Ok((c, committed)),
        }
    })
}

fn json_string<Input>() -> impl Parser<Input, Output = String>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    between(char('"'), lex(char('"')), many(json_char())).expected("string")
}

fn object<Input>() -> impl Parser<Input, Output = Value>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    let field = (json_string(), lex(char(':')), json_value()).map(|t| (t.0, t.2));
    let fields = sep_by(field, lex(char(',')));
    between(lex(char('{')), lex(char('}')), fields)
        .map(Value::Object)
        .expected("object")
}

#[inline]
pub fn json_value<Input>() -> impl Parser<Input, Output = Value>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    json_value_()
}

// We need to use `parser!` to break the recursive use of `value` to prevent the returned parser
// from containing itself
parser! {
    #[inline]
    fn json_value_[Input]()(Input) -> Value
        where [ Input: Stream<Token = char> ]
    {
        let array = between(
            lex(char('[')),
            lex(char(']')),
            sep_by(json_value(), lex(char(','))),
        ).map(Value::Array);

        choice((
            json_string().map(Value::String),
            object(),
            array,
            number().map(Value::Number),
            lex(string("false").map(|_| Value::Bool(false))),
            lex(string("true").map(|_| Value::Bool(true))),
            lex(string("null").map(|_| Value::Null)),
        ))
    }
}

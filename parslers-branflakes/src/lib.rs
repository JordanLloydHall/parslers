use parslers::reflect::Reflect;
use parslers_macro::*;

#[derive(Clone, Debug, Reflected)]

pub struct BranflakesProgram(pub Vec<Branflakes>);

#[derive(Clone, Debug, Reflected)]

pub enum Branflakes {
    Add,
    Sub,
    Left,
    Right,
    Read,
    Print,
    Loop(BranflakesProgram),
}

pub fn branflakes_parser(input: &str) -> Result<BranflakesProgram, String> {
    let mut stack = vec![];
    let mut curr_vec = vec![];
    for c in input.chars() {
        match c {
            '+' => curr_vec.push(Branflakes::Add),
            '-' => curr_vec.push(Branflakes::Sub),
            '.' => curr_vec.push(Branflakes::Print),
            ',' => curr_vec.push(Branflakes::Read),
            '<' => curr_vec.push(Branflakes::Left),
            '>' => curr_vec.push(Branflakes::Right),
            '[' => {
                stack.push(curr_vec);
                curr_vec = vec![];
            }
            ']' => {
                let last = curr_vec;
                let top_of_stack = stack.pop().ok_or("Missing '['".to_owned())?;
                curr_vec = top_of_stack;
                curr_vec.push(Branflakes::Loop(BranflakesProgram(last)));
            }
            _ => return Err("Encountered invalid char".to_owned()),
        }
    }

    Ok(BranflakesProgram(curr_vec))
}

pub fn branflakes_parser_validate(input: &str) -> Result<(), String> {
    let mut nested_level = 0;
    for c in input.chars() {
        match c {
            '+' | '-' | '.' | ',' | '<' | '>' => {}
            '[' => nested_level += 1,
            ']' => {
                nested_level -= 1;
            }
            _ => return Err("Encountered invalid char".to_owned()),
        }
    }

    if nested_level == 0 {
        Ok(())
    } else {
        Err("Missing ']'".to_owned())
    }
}

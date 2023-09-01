mod json {
    #![allow(warnings, unused)]
    include!(concat!(env!("OUT_DIR"), "/json.rs"));
}

fn main() {
    let input = "{\"hello\" : null, \"world\" : [null, null, null]}";
    let chars = &mut input.chars();

    let result = json::json(chars);
    println!("{:?}", result);
    println!("{:?}", chars);
}

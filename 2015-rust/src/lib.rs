use std::{env, fmt, fs};

pub fn solve<I, S>(parse: fn(String) -> I, first: Option<fn(I) -> S>, second: Option<fn(I) -> S>)
where
    I: Clone,
    S: fmt::Debug,
{
    let input = input(parse);

    if let Some(first) = first {
        println!("Part #1: {:?}", first(input.clone()));
    } else {
        println!("Part #1: Not solved");
    }

    if let Some(second) = second {
        println!("Part #2: {:?}", second(input));
    } else {
        println!("Part #2: Not solved");
    }
}

fn input<T>(parse: fn(String) -> T) -> T {
    let mut content = env::args()
        .nth(1)
        .map(fs::read_to_string)
        .expect("An input file should be read successfully")
        .expect("An input file should be provider as the first argument");
    content.pop();
    parse(content)
}

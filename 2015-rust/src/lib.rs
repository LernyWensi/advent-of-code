use std::{borrow::Borrow, env, fmt, fs};

enum Part {
    One,
    Two,
}

impl fmt::Display for Part {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", match self {
            Self::One => '1',
            Self::Two => '2',
        })
    }
}

impl Part {
    fn solve<Input, Solution>(&self, solver: Option<impl Fn(&Input) -> Solution>, input: &Input)
    where
        Input: ?Sized,
        Solution: fmt::Debug,
    {
        let s = solver.map(|solver| solver(input));
        println!(
            "Part #{self}: {solution:?}",
            solution = s
                .as_ref()
                .map_or(&"Not solved" as &dyn fmt::Debug, |solution| solution)
        );
    }
}

pub fn solve<Parsed, Input, Solution>(
    parse: impl Fn(&str) -> Parsed,
    first: Option<impl Fn(&Input) -> Solution>,
    second: Option<impl Fn(&Input) -> Solution>,
) where
    Parsed: Borrow<Input>,
    Input: ?Sized,
    Solution: fmt::Debug,
{
    let input = env::args()
        .nth(1)
        .map(|file| fs::read_to_string(file).expect("An input file should be read successfully"))
        .map(|input| parse(input.trim_end()))
        .expect("An input file should be provider as the first argument");

    let input = input.borrow();
    Part::One.solve(first, input);
    Part::Two.solve(second, input);
}

#[macro_export]
macro_rules! assert_first {
    ($input:expr, $first:expr) => {
        let parsed = super::parse($input);
        assert_eq!(super::first(&parsed), $first);
    };
}

#[macro_export]
macro_rules! assert_second {
    ($input:expr, $second:expr) => {
        let parsed = super::parse($input);
        assert_eq!(super::second(&parsed), $second);
    };
}

#![feature(string_into_chars)]

use std::ops::ControlFlow;

#[derive(Clone, Debug)]
enum Direction {
    Up,
    Down,
}

impl From<char> for Direction {
    fn from(char: char) -> Self {
        match char {
            '(' => Self::Up,
            ')' => Self::Down,
            invalid_char => {
                panic!("Invalid character '{invalid_char}' for `Direction`; expected '(' or ')'",)
            }
        }
    }
}

#[allow(clippy::needless_pass_by_value)]
fn parse(input: String) -> Vec<Direction> {
    input.into_chars().map(Direction::from).collect()
}

#[allow(clippy::needless_pass_by_value)]
fn first(input: Vec<Direction>) -> i32 {
    input
        .iter()
        .fold(0, |floor, instruction| match instruction {
            Direction::Up => floor + 1,
            Direction::Down => floor - 1,
        })
}

#[allow(clippy::needless_pass_by_value)]
fn second(input: Vec<Direction>) -> i32 {
    input
        .iter()
        .try_fold((0, 0), |(floor, position), instruction| {
            let floor = match instruction {
                Direction::Up => floor + 1,
                Direction::Down => floor - 1,
            };
            let position = position + 1;

            if floor == -1 {
                ControlFlow::Break(position)
            } else {
                ControlFlow::Continue((floor, position))
            }
        })
        .break_value()
        .expect("No break value found; the input did not lead to the basement")
}

fn main() {
    advent_of_code::solve(parse, Some(first), Some(second));
}

#[cfg(test)]
mod tests {
    #[test]
    fn first() {
        let parsed = super::parse("(())".to_owned());
        assert_eq!(super::first(parsed), 0);

        let parsed = super::parse("()()".to_owned());
        assert_eq!(super::first(parsed), 0);

        let parsed = super::parse("(((".to_owned());
        assert_eq!(super::first(parsed), 3);

        let parsed = super::parse("(()(()(".to_owned());
        assert_eq!(super::first(parsed), 3);

        let parsed = super::parse("))(((((".to_owned());
        assert_eq!(super::first(parsed), 3);

        let parsed = super::parse("())".to_owned());
        assert_eq!(super::first(parsed), -1);

        let parsed = super::parse("))(".to_owned());
        assert_eq!(super::first(parsed), -1);

        let parsed = super::parse(")))".to_owned());
        assert_eq!(super::first(parsed), -3);

        let parsed = super::parse(")())())".to_owned());
        assert_eq!(super::first(parsed), -3);
    }

    #[test]
    fn second() {
        let parsed = super::parse(")".to_owned());
        assert_eq!(super::second(parsed), 1);

        let parsed = super::parse("()())".to_owned());
        assert_eq!(super::second(parsed), 5);
    }
}

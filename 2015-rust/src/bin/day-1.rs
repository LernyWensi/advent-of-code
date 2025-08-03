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

fn parse(input: &str) -> Vec<Direction> {
    input.chars().map(Direction::from).collect()
}

fn first(input: &[Direction]) -> i32 {
    input
        .iter()
        .fold(0, |floor, instruction| match instruction {
            Direction::Up => floor + 1,
            Direction::Down => floor - 1,
        })
}

fn second(input: &[Direction]) -> i32 {
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
        advent_of_code::assert_first!("(())", 0);
        advent_of_code::assert_first!("()()", 0);
        advent_of_code::assert_first!("(((", 3);
        advent_of_code::assert_first!("(()(()(", 3);
        advent_of_code::assert_first!("))(((((", 3);
        advent_of_code::assert_first!("())", -1);
        advent_of_code::assert_first!("))(", -1);
        advent_of_code::assert_first!(")))", -3);
        advent_of_code::assert_first!(")())())", -3);
    }

    #[test]
    fn second() {
        advent_of_code::assert_second!(")", 1);
        advent_of_code::assert_second!("()())", 5);
    }
}

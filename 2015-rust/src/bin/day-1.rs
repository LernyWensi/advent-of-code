use std::ops::ControlFlow;

use advent_of_code::solve;

#[derive(Debug)]
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

#[derive(Default, Debug)]
struct Floor(i32);

impl Floor {
    const fn traverse(mut self, direction: &Direction) -> Self {
        match direction {
            Direction::Up => self.0 += 1,
            Direction::Down => self.0 -= 1,
        }
        self
    }

    const fn is_basement(&self) -> bool {
        self.0 < 0
    }
}

fn parse(input: &str) -> Vec<Direction> {
    input.chars().map(Direction::from).collect()
}

fn first(input: &[Direction]) -> Option<i32> {
    Some(input.iter().fold(Floor::default(), Floor::traverse).0)
}

fn second(input: &[Direction]) -> Option<i32> {
    Some(
        input
            .iter()
            .try_fold((Floor::default(), 0), |(floor, position), direction| {
                let floor = floor.traverse(direction);
                let position = position + 1;

                if floor.is_basement() {
                    ControlFlow::Break(position)
                } else {
                    ControlFlow::Continue((floor, position))
                }
            })
            .break_value()
            .expect("No break value found; the input did not lead to the basement"),
    )
}

fn main() {
    solve(parse, first, second);
}

#[cfg(test)]
mod tests {
    use advent_of_code::{assert_first, assert_second};

    #[test]
    fn first() {
        assert_first!("(())", Some(0));
        assert_first!("()()", Some(0));
        assert_first!("(((", Some(3));
        assert_first!("(()(()(", Some(3));
        assert_first!("))(((((", Some(3));
        assert_first!("())", Some(-1));
        assert_first!("))(", Some(-1));
        assert_first!(")))", Some(-3));
        assert_first!(")())())", Some(-3));
    }

    #[test]
    fn second() {
        assert_second!(")", Some(1));
        assert_second!("()())", Some(5));
    }
}

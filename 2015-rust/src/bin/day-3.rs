#![feature(iterator_try_collect)]

use std::collections::HashSet;

#[derive(Clone, Debug)]
enum Direction {
    North,
    South,
    East,
    West,
}

impl From<char> for Direction {
    fn from(char: char) -> Self {
        match char {
            '^' => Self::North,
            'v' => Self::South,
            '>' => Self::East,
            '<' => Self::West,
            invalid_char => panic!(
                "Invalid character '{invalid_char}' for `Direction`; expected one of: '^', 'v', \
                 '>', '<'",
            ),
        }
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Copy, Default, Debug)]
struct Position {
    x: i64,
    y: i64,
}

impl Position {
    const fn move_in_direction(&mut self, direction: &Direction) {
        match direction {
            Direction::North => self.y += 1,
            Direction::South => self.y -= 1,
            Direction::East => self.x += 1,
            Direction::West => self.x -= 1,
        }
    }
}

#[allow(clippy::needless_pass_by_value)]
fn parse(input: String) -> Vec<Direction> {
    input
        .chars()
        .map(Direction::from)
        .collect::<Vec<Direction>>()
}

#[allow(clippy::needless_pass_by_value)]
fn first(input: Vec<Direction>) -> usize {
    let mut position = Position::default();
    let mut visited = HashSet::<Position>::from([position]);

    for direction in &input {
        position.move_in_direction(direction);
        visited.insert(position);
    }

    visited.len()
}

#[allow(clippy::needless_pass_by_value)]
fn second(input: Vec<Direction>) -> usize {
    let mut turn_flag = true;
    let mut positions = (Position::default(), Position::default());
    let mut visited = HashSet::<Position>::from([Position::default()]);

    for direction in &input {
        if turn_flag {
            positions.0.move_in_direction(direction);
            visited.insert(positions.0);
            turn_flag = false;
        } else {
            positions.1.move_in_direction(direction);
            visited.insert(positions.1);
            turn_flag = true;
        }
    }

    visited.len()
}

fn main() {
    advent_of_code::solve(parse, Some(first), Some(second));
}

#[cfg(test)]
mod tests {
    #[test]
    fn first() {
        let parsed = super::parse(">".to_owned());
        assert_eq!(super::first(parsed), 2);

        let parsed = super::parse("^>v<".to_owned());
        assert_eq!(super::first(parsed), 4);

        let parsed = super::parse("^v^v^v^v^v".to_owned());
        assert_eq!(super::first(parsed), 2);
    }

    #[test]
    fn second() {
        let parsed = super::parse("^v".to_owned());
        assert_eq!(super::second(parsed), 3);

        let parsed = super::parse("^>v<".to_owned());
        assert_eq!(super::second(parsed), 3);

        let parsed = super::parse("^v^v^v^v^v".to_owned());
        assert_eq!(super::second(parsed), 11);
    }
}

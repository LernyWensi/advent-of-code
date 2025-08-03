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

fn parse(input: &str) -> Vec<Direction> {
    input.chars().map(Direction::from).collect()
}

fn first(input: &[Direction]) -> usize {
    let mut position = Position::default();
    let mut visited = HashSet::<Position>::from([position]);

    for direction in input {
        position.move_in_direction(direction);
        visited.insert(position);
    }

    visited.len()
}

fn second(input: &[Direction]) -> usize {
    let mut turn_flag = true;
    let mut positions = (Position::default(), Position::default());
    let mut visited = HashSet::<Position>::from([Position::default()]);

    for direction in input {
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
        advent_of_code::assert_first!(">", 2);
        advent_of_code::assert_first!("^>v<", 4);
        advent_of_code::assert_first!("^v^v^v^v^v", 2);
    }

    #[test]
    fn second() {
        advent_of_code::assert_second!("^v", 3);
        advent_of_code::assert_second!("^>v<", 3);
        advent_of_code::assert_second!("^v^v^v^v^v", 11);
    }
}

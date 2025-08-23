use std::collections::HashSet;

use advent_of_code::solve;

#[derive(Debug)]
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

#[derive(Hash, PartialEq, Eq, Clone, Copy, Default, Debug)]
struct Position {
    x: i32,
    y: i32,
}

impl Position {
    const fn translate(&mut self, direction: &Direction) {
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

fn first(input: &[Direction]) -> Option<usize> {
    let mut position = Position::default();
    let mut visited = HashSet::from([position]);

    for direction in input {
        position.translate(direction);
        visited.insert(position);
    }

    Some(visited.len())
}

fn second(input: &[Direction]) -> Option<usize> {
    let mut turn_flag = true;
    let mut positions = (Position::default(), Position::default());
    let mut visited = HashSet::<Position>::from([Position::default()]);

    for direction in input {
        if turn_flag {
            positions.0.translate(direction);
            visited.insert(positions.0);
            turn_flag = false;
        } else {
            positions.1.translate(direction);
            visited.insert(positions.1);
            turn_flag = true;
        }
    }

    Some(visited.len())
}

fn main() {
    solve(parse, first, second);
}

#[cfg(test)]
mod tests {
    use advent_of_code::{assert_first, assert_second};

    #[test]
    fn first() {
        assert_first!(">", Some(2));
        assert_first!("^>v<", Some(4));
        assert_first!("^v^v^v^v^v", Some(2));
    }

    #[test]
    fn second() {
        assert_second!("^v", Some(3));
        assert_second!("^>v<", Some(3));
        assert_second!("^v^v^v^v^v", Some(11));
    }
}

#![feature(iterator_try_collect)]

use std::collections::HashSet;

#[derive(Debug)]
enum Direction {
    North,
    South,
    East,
    West,
}

impl TryFrom<char> for Direction {
    type Error = &'static str;

    fn try_from(char: char) -> Result<Self, Self::Error> {
        match char {
            '^' => Ok(Self::North),
            'v' => Ok(Self::South),
            '>' => Ok(Self::East),
            '<' => Ok(Self::West),
            _ => Err(
                "Invalid direction; the direction can only be one of the following: ^, v, >, or <",
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

fn main() {
    let input = advent_of_code::get_input();

    let mut position = Position::default();
    let mut visited = HashSet::<Position>::from([position]);

    input
        .chars()
        .map(Direction::try_from)
        .try_collect::<Vec<Direction>>()
        .expect("Should successfully parse a .txt file")
        .iter()
        .for_each(|direction| {
            position.move_in_direction(direction);
            visited.insert(position);
        });

    println!(
        "The number of houses that receive at least one present is {}",
        visited.len()
    );
}

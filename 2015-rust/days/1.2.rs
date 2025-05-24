use std::ops::ControlFlow;

fn main() {
    let input = advent_of_code::get_input();

    let position = input
        .chars()
        .try_fold((0, 0), |(floor, position), instruction| {
            if floor == -1 {
                return ControlFlow::Break(position);
            }
            match instruction {
                '(' => ControlFlow::Continue((floor + 1, position + 1)),
                ')' => ControlFlow::Continue((floor - 1, position + 1)),
                invalid_char => {
                    panic!(
                        "A .txt file should contain only '(' or ')' characters, got {invalid_char}"
                    )
                }
            }
        })
        .break_value()
        .expect("Should have stopped at floor -1 at some point");

    println!(
        "The position of the character that causes Santa to first enter the basement is {position}"
    );
}

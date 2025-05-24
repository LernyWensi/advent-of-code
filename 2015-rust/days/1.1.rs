fn main() {
    let input = advent_of_code::get_input();

    let floor = input
        .chars()
        .fold(0, |floor, instruction| match instruction {
            '(' => floor + 1,
            ')' => floor - 1,
            invalid_char => {
                panic!("A .txt file should contain only '(' or ')' characters, got {invalid_char}")
            }
        });

    println!("The instructions take Santa to the {floor} floor");
}

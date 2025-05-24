fn main() {
    let input = advent_of_code::get_input();

    let total = input
        .lines()
        .map(|line| {
            line.split('x')
                .map(|value| str::parse(value).expect("Failed to parse digit in line"))
                .collect::<Vec<u32>>()
        })
        .fold(0, |total, line| {
            let [length, width, height] = line.as_slice() else {
                panic!("Invalid line in .txt file: {line:?}");
            };

            total
                + length * width * height
                + ([length + width, width + height, height + length]
                    .iter()
                    .map(|value| value * 2)
                    .min()
                    .expect("Should be able to determine the minimum area of one of the faces"))
        });

    println!("They should order a total of {total}");
}

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

            let lw = length * width;
            let wh = width * height;
            let hl = height * length;

            total
                + (2 * lw)
                + (2 * wh)
                + (2 * hl)
                + ([lw, wh, hl]
                    .iter()
                    .min()
                    .expect("Vector of sides must not to be empty"))
        });

    println!("They should order a total of {total}");
}

#[derive(Clone, Debug)]
struct Dimensions {
    length: u32,
    width: u32,
    height: u32,
}

impl From<&str> for Dimensions {
    fn from(line: &str) -> Self {
        let [length, width, height] = line
            .split('x')
            .map(|value| str::parse(value).expect("Failed to parse the dimension value"))
            .collect::<Vec<u32>>()[..]
        else {
            panic!("Input string must be in the format 'LENGTHxWIDTHxHEIGHT'");
        };

        Self {
            length,
            width,
            height,
        }
    }
}

fn parse(input: &str) -> Vec<Dimensions> {
    input.lines().map(Dimensions::from).collect()
}

fn first(input: &[Dimensions]) -> u32 {
    input.iter().fold(0, |total, dimensions| {
        let Dimensions {
            length,
            width,
            height,
        } = dimensions;

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
                .expect("Failed to find the minimum area; dimensions may be invalid"))
    })
}

fn second(input: &[Dimensions]) -> u32 {
    input.iter().fold(0, |total, dimensions| {
        let Dimensions {
            length,
            width,
            height,
        } = dimensions;

        total
            + length * width * height
            + ([length + width, width + height, height + length]
                .iter()
                .map(|value| value * 2)
                .min()
                .expect("Failed to determine the minimum perimeter of one of the faces"))
    })
}

fn main() {
    advent_of_code::solve(parse, Some(first), Some(second));
}

#[cfg(test)]
mod tests {
    #[test]
    fn first() {
        advent_of_code::assert_first!("2x3x4", 58);
        advent_of_code::assert_first!("1x1x10", 43);
    }

    #[test]
    fn second() {
        advent_of_code::assert_second!("2x3x4", 34);
        advent_of_code::assert_second!("1x1x10", 14);
    }
}

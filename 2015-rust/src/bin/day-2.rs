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
            .map(|value| str::parse(value).expect("Failed to parse dimension value"))
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

#[allow(clippy::needless_pass_by_value)]
fn parse(input: String) -> Vec<Dimensions> {
    input.lines().map(Dimensions::from).collect()
}

#[allow(clippy::needless_pass_by_value)]
fn first(input: Vec<Dimensions>) -> u32 {
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

#[allow(clippy::needless_pass_by_value)]
fn second(input: Vec<Dimensions>) -> u32 {
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
        let parsed = super::parse("2x3x4".to_owned());
        assert_eq!(super::first(parsed), 58);

        let parsed = super::parse("1x1x10".to_owned());
        assert_eq!(super::first(parsed), 43);
    }

    #[test]
    fn second() {
        let parsed = super::parse("2x3x4".to_owned());
        assert_eq!(super::second(parsed), 34);

        let parsed = super::parse("1x1x10".to_owned());
        assert_eq!(super::second(parsed), 14);
    }
}

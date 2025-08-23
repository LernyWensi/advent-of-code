#![feature(array_chunks)]

use std::fmt::Write;

use advent_of_code::solve;

#[derive(Debug)]
struct Md5(Vec<u8>);

impl Md5 {
    const A: u32 = 0x6745_2301;
    const B: u32 = 0xEFCD_AB89;
    const C: u32 = 0x98BA_DCFE;
    const D: u32 = 0x1032_5476;
    #[rustfmt::skip]
    const K: [u32; 64] = [
        /* 00..03 */ 0xd76a_a478, 0xe8c7_b756, 0x2420_70db, 0xc1bd_ceee,
        /* 04..07 */ 0xf57c_0faf, 0x4787_c62a, 0xa830_4613, 0xfd46_9501,
        /* 08..11 */ 0x6980_98d8, 0x8b44_f7af, 0xffff_5bb1, 0x895c_d7be,
        /* 12..15 */ 0x6b90_1122, 0xfd98_7193, 0xa679_438e, 0x49b4_0821,
        /* 16..19 */ 0xf61e_2562, 0xc040_b340, 0x265e_5a51, 0xe9b6_c7aa,
        /* 20..23 */ 0xd62f_105d, 0x0244_1453, 0xd8a1_e681, 0xe7d3_fbc8,
        /* 24..27 */ 0x21e1_cde6, 0xc337_07d6, 0xf4d5_0d87, 0x455a_14ed,
        /* 28..31 */ 0xa9e3_e905, 0xfcef_a3f8, 0x676f_02d9, 0x8d2a_4c8a,
        /* 32..35 */ 0xfffa_3942, 0x8771_f681, 0x6d9d_6122, 0xfde5_380c,
        /* 36..39 */ 0xa4be_ea44, 0x4bde_cfa9, 0xf6bb_4b60, 0xbebf_bc70,
        /* 40..43 */ 0x289b_7ec6, 0xeaa1_27fa, 0xd4ef_3085, 0x0488_1d05,
        /* 44..47 */ 0xd9d4_d039, 0xe6db_99e5, 0x1fa2_7cf8, 0xc4ac_5665,
        /* 48..51 */ 0xf429_2244, 0x432a_ff97, 0xab94_23a7, 0xfc93_a039,
        /* 52..55 */ 0x655b_59c3, 0x8f0c_cc92, 0xffef_f47d, 0x8584_5dd1,
        /* 56..59 */ 0x6fa8_7e4f, 0xfe2c_e6e0, 0xa301_4314, 0x4e08_11a1,
        /* 60..63 */ 0xf753_7e82, 0xbd3a_f235, 0x2ad7_d2bb, 0xeb86_d391,
    ];
    #[rustfmt::skip]
    const S: [u32; 64] = [
        /* 00..15 */ 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22,
        /* 16..31 */ 5,  9, 14, 20, 5,  9, 14, 20, 5,  9, 14, 20, 5,  9, 14, 20,
        /* 32..47 */ 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23,
        /* 48..63 */ 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21,
    ];

    fn from_str(message: &str) -> Self {
        let mut bytes = message.as_bytes().to_vec();

        bytes.push(0x80);
        // Last 8 bytes are reserved for the original message length in bits.
        while bytes.len() % 64 != 56 {
            bytes.push(0);
        }
        bytes.extend_from_slice(&(message.len() * 8).to_le_bytes());

        Self(bytes)
    }

    #[allow(clippy::many_single_char_names, non_snake_case)]
    fn hash(self) -> String {
        let mut A = Self::A;
        let mut B = Self::B;
        let mut C = Self::C;
        let mut D = Self::D;

        for chunk in self.0.chunks_exact(64) {
            let words = chunk
                .array_chunks::<4>()
                .map(|&bytes| u32::from_le_bytes(bytes))
                .collect::<Vec<u32>>();

            let mut a = A;
            let mut b = B;
            let mut c = C;
            let mut d = D;

            for i in 0..64 {
                #[rustfmt::skip]
                let (f, g) = match i {
                    0..16 => (
                         /* f */ (b & c) | (!b & d),
                         /* g */ i
                    ),
                    16..32 => (
                         /* f */ (d & b) | (!d & c),
                         /* g */ (5 * i + 1) % 16
                    ),
                    32..48 => (
                         /* f */ b ^ c ^ d,
                         /* g */ (3 * i + 5) % 16
                    ),
                    48..64 => (
                         /* f */ c ^ (b | !d),
                         /* g */ (7 * i) % 16
                    ),
                    _ => unreachable!(),
                };

                let f = f
                    .wrapping_add(a)
                    .wrapping_add(Self::K[i])
                    .wrapping_add(words[g]);
                a = d;
                d = c;
                c = b;
                b = b.wrapping_add(f.rotate_left(Self::S[i]));
            }

            A = A.wrapping_add(a);
            B = B.wrapping_add(b);
            C = C.wrapping_add(c);
            D = D.wrapping_add(d);
        }

        [A, B, C, D]
            .iter()
            .fold(String::with_capacity(32), |mut hash, word| {
                for byte in word.to_le_bytes() {
                    write!(&mut hash, "{byte:02x}").unwrap();
                }
                hash
            })
    }
}

struct Secret(String);

impl Secret {
    fn followed_by(&self, decimal: u32) -> String {
        Md5::from_str(&format!("{}{}", self.0, decimal)).hash()
    }
}

fn parse(input: &str) -> Secret {
    Secret(input.to_string())
}

fn first(input: &Secret) -> Option<u32> {
    Some(
        (0..u32::MAX)
            .find(|&index| input.followed_by(index).starts_with("00000"))
            .expect("No input results in a hash with a prefix of '00000' (5 zeros)"),
    )
}

fn second(input: &Secret) -> Option<u32> {
    Some(
        (0..u32::MAX)
            .find(|&index| input.followed_by(index).starts_with("000000"))
            .expect("No input results in a hash with a prefix of '000000' (6 zeros)"),
    )
}

fn main() {
    solve(parse, first, second);
}

#[cfg(test)]
mod tests {
    use advent_of_code::assert_first;

    #[test]
    fn first() {
        assert_first!("abcdef", Some(609_043));
        assert_first!("pqrstuv", Some(1_048_970));
    }
}

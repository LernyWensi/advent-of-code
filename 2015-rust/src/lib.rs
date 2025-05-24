use std::{env, fs};

pub fn get_input() -> String {
    let mut string = env::args()
        .nth(1)
        .map(fs::read_to_string)
        .expect("A .txt file should be read successfully")
        .expect("A .txt file should be provider as the first argument");
    string.pop();
    string
}

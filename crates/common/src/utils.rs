use std::{error::Error, path::PathBuf, str::FromStr};

pub const SYMBOLS: [char; 5] = ['^', '/', '*', '+', '-'];
pub const DIGITS: [char; 11] = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.'];

pub fn read_input() -> Result<String, Box<dyn Error>> {
    let mut input = String::new();
    std::io::stdin().read_line(&mut input)?;
    Ok(input)
}

pub fn get_path(path: String) -> String {
    let mut new_path = env!("CARGO_MANIFEST_DIR").to_string();
    new_path.push_str(&format!("/src/{}", path));
    return new_path;
}

pub fn clear() {
    print!("{esc}[2J{esc}[1;1H", esc = 27 as char);
}

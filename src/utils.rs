use std::error::Error;

pub const SYMBOLS: [char; 5] = ['^', '/', '*', '+', '-'];
pub const DIGITS: [char; 11] = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.'];

pub fn read_input() -> Result<String, Box<dyn Error>> {
    let mut input = String::new();
    std::io::stdin().read_line(&mut input)?;
    Ok(input)
}

pub fn clear() {
    print!("{esc}[2J{esc}[1;1H", esc = 27 as char);
}

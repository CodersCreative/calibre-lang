use std::{path::PathBuf, str::FromStr};

pub fn get_path(path: String) -> String {
    let mut new_path = env!("CARGO_MANIFEST_DIR").to_string();
    new_path.push_str(&format!("/src/{}", path));
    new_path
}

pub fn get_stdlib_path() -> PathBuf {
    PathBuf::from_str(&get_path("stdlib/main.cl".to_string())).unwrap()
}

pub fn get_globals_path() -> PathBuf {
    PathBuf::from_str(&get_path("global/main.cl".to_string())).unwrap()
}

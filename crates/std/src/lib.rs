use std::path::PathBuf;

pub fn get_path(path: &str) -> String {
    let mut new_path = env!("CARGO_MANIFEST_DIR").to_string();
    new_path.push_str(&format!("/src/{}", path));
    new_path
}

pub fn get_stdlib_path() -> PathBuf {
    PathBuf::from(get_path("stdlib/main.cal"))
}

pub fn get_stdlib_module_path(name: &str) -> PathBuf {
    let path = format!("stdlib/{}/main.cal", name);
    PathBuf::from(get_path(&path))
}

pub fn get_globals_path() -> PathBuf {
    PathBuf::from(get_path("global/main.cal"))
}

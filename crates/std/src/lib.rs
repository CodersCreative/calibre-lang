use std::path::PathBuf;
use tracing::{debug, instrument};

#[instrument(skip_all)]
pub fn get_path(path: &str) -> String {
    debug!("getting std path: {}", path);
    let mut new_path = env!("CARGO_MANIFEST_DIR").to_string();
    new_path.push_str(&format!("/src/{}", path));
    new_path
}

#[instrument(skip_all)]
pub fn get_stdlib_path() -> PathBuf {
    debug!("getting stdlib path");
    PathBuf::from(get_path("stdlib/main.cal"))
}

#[instrument(skip_all)]
pub fn get_stdlib_module_path(name: &str) -> PathBuf {
    debug!(name = %name, "getting stdlib module path");
    let path = format!("stdlib/{}/main.cal", name);
    PathBuf::from(get_path(&path))
}

#[instrument(skip_all)]
pub fn get_globals_path() -> PathBuf {
    debug!("getting globals path");
    PathBuf::from(get_path("global/main.cal"))
}

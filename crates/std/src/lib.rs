use std::path::PathBuf;
use tracing::{debug, instrument};

pub static STD_DIR: include_dir::Dir<'static> =
    include_dir::include_dir!("$CARGO_MANIFEST_DIR/src");

#[instrument(skip_all)]
pub fn get_stdlib_path() -> PathBuf {
    debug!("getting stdlib path");
    PathBuf::from("stdlib/main.cal")
}

#[instrument(skip_all)]
pub fn get_stdlib_module_path(name: &str) -> PathBuf {
    debug!(name = %name, "getting stdlib module path");
    PathBuf::from(format!("stdlib/{}/main.cal", name))
}

#[instrument(skip_all)]
pub fn get_globals_path() -> PathBuf {
    debug!("getting globals path");
    PathBuf::from("global/main.cal")
}

#[instrument(skip_all)]
pub fn get_stdlib_file(path: &str) -> Option<&'static str> {
    debug!("getting stdlib file: {}", path);
    STD_DIR.get_file(path).and_then(|f| f.contents_utf8())
}

#[instrument(skip_all)]
pub fn get_stdlib_dir(path: &str) -> Option<&'static include_dir::Dir<'static>> {
    debug!("getting stdlib dir: {}", path);
    STD_DIR.get_dir(path)
}

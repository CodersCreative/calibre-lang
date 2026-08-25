use crate::config::Config;
use smol::fs;
use std::{error::Error, path::PathBuf, str::FromStr};

const DEFAULT_MAIN: &'static str = "const main := fn => print(\"Hello, World!\");";

pub async fn execute(path: Option<String>, no_std: bool) -> Result<(), Box<dyn Error>> {
    let config = Config {
        no_std,
        package: crate::config::Package {
            name: path.clone().unwrap_or_default(),
            ..Default::default()
        },
        ..Default::default()
    };

    let path = PathBuf::from_str(&if let Some(mut path) = path {
        if path.ends_with("/") {
            path
        } else {
            path.push('/');
            path
        }
    } else {
        String::new()
    })
    .unwrap_or_default();

    let config_path = path.join("calibre.toml");
    let main_path = path.join("src/main.cal");

    fs::create_dir_all(main_path.parent().unwrap()).await?;

    fs::write(main_path, DEFAULT_MAIN).await?;
    fs::write(
        config_path,
        toml::to_string_pretty(&config).unwrap_or_default(),
    )
    .await?;
    Ok(())
}

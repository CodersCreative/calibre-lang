use crate::config::Config;
use derive_builder::Builder;
use smol::fs;
use std::{error::Error, path::PathBuf, str::FromStr};
use tracing::instrument;

const DEFAULT_MAIN: &str = "const main := fn => print(\"Hello, World!\");";

#[derive(Builder, Debug)]
pub struct New {
    path: Option<String>,
    no_std: bool,
}

impl New {
    #[instrument]
    pub async fn execute(self) -> Result<(), Box<dyn Error>> {
        let config = Config {
            no_std: self.no_std,
            package: crate::config::Package {
                name: self.path.clone().unwrap_or_default(),
                ..Default::default()
            },
            ..Default::default()
        };

        let path = PathBuf::from_str(&if let Some(mut path) = self.path {
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

        let main_path = path.join("src/main.cal");

        fs::create_dir_all(main_path.parent().unwrap()).await?;
        fs::write(main_path, DEFAULT_MAIN).await?;

        fs::create_dir_all(path.join("include")).await?;
        fs::write(
            path.join("calibre.toml"),
            toml::to_string_pretty(&config).unwrap_or_default(),
        )
        .await?;
        Ok(())
    }
}

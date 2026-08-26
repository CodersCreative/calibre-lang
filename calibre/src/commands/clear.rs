use derive_builder::Builder;
use rustc_hash::FxHashSet;

use crate::config::load_project_from;
use std::{
    error::Error,
    path::{Path, PathBuf},
};

#[derive(Builder, Default)]
pub struct Clear {}

impl Clear {
    pub fn execute(self) -> Result<(), Box<dyn Error>> {
        fn clear_target(target: &Path) -> Result<bool, Box<dyn Error>> {
            if !target.exists() || !target.is_dir() {
                return Ok(false);
            }

            let calibre_dir = target.join("calibre");
            let mut entries = std::fs::read_dir(target)?
                .flatten()
                .map(|e| e.file_name())
                .take(2)
                .collect::<Vec<_>>();

            let only_calibre = entries.len() == 1
                && entries.pop().as_deref() == Some(std::ffi::OsStr::new("calibre"));

            if only_calibre {
                std::fs::remove_dir_all(target)?;
                println!("Removed {}", target.display());
                return Ok(true);
            }

            if calibre_dir.exists() {
                std::fs::remove_dir_all(&calibre_dir)?;
                println!("Removed {}", calibre_dir.display());
                return Ok(true);
            }

            Ok(false)
        }

        let cwd = std::env::current_dir()?;
        let project = load_project_from(&cwd).map_err(|e| format!("config error: {e}"))?;
        let mut targets_to_clear = FxHashSet::<PathBuf>::default();

        if let Some(project) = &project {
            targets_to_clear.insert(project.root.join("target").canonicalize()?);
        }

        let cwd_target = cwd.join("target");
        targets_to_clear.insert(cwd_target.canonicalize()?);

        let mut removed_any = false;
        for target in targets_to_clear {
            removed_any |= clear_target(&target)?;
        }

        if !removed_any {
            if let Some(project) = &project {
                println!(
                    "Nothing to clear at {}",
                    project.root.join("target").display()
                );
            } else {
                println!("Nothing to clear at {}", cwd.join("target").display());
            }
        }

        Ok(())
    }
}

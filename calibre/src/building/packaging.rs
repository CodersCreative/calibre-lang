use crate::{CalibreEngine, CalibreError};
use calibre_lir::environment::LirRegistry;
use calibre_mir::manifest::Manifest;
use serde::{Deserialize, Serialize};
use std::fs::{self, File};
use std::path::{Path, PathBuf};

// I think I'm gonna use the LIR because its the last phase before specialization for the runtime selected
#[derive(Clone, Serialize, Deserialize)]
pub struct PackagedProgramBlob {
    pub manifest: Manifest,
    pub program: LirRegistry,
}

pub trait CalibrePackaging {
    fn package_root(&self) -> Option<PathBuf>;

    fn try_load_packaged_program(
        &self,
        path: impl AsRef<Path>,
    ) -> Result<Option<PackagedProgramBlob>, CalibreError>;

    fn package(
        &self,
        path: impl AsRef<Path>,
        manifest: Manifest,
        program: LirRegistry,
    ) -> Result<(), CalibreError>;
}

impl CalibrePackaging for CalibreEngine {
    fn package_root(&self) -> Option<PathBuf> {
        if let Some(path) = &self.cache_dir {
            return Some(path.clone());
        }

        if let Some(path) = &self.source_path
            && let Ok(Some(project)) = crate::config::load_project_from(path)
        {
            return Some(project.root.join("target").join("packages").join("calibre"));
        }

        let cwd = std::env::current_dir().ok()?;
        Some(cwd.join("target").join("packages").join("calibre"))
    }

    fn package(
        &self,
        path: impl AsRef<Path>,
        manifest: Manifest,
        program: LirRegistry,
    ) -> Result<(), CalibreError> {
        let file = File::create(path)?;

        let mut writer = std::io::BufWriter::new(file);

        let package = PackagedProgramBlob { manifest, program };

        bincode::serialize_into(&mut writer, &package)
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, e))
            .map_err(CalibreError::Io)
    }

    fn try_load_packaged_program(
        &self,
        path: impl AsRef<Path>,
    ) -> Result<Option<PackagedProgramBlob>, CalibreError> {
        let file = match File::open(&path) {
            Ok(file) => file,
            Err(err) if err.kind() == std::io::ErrorKind::NotFound => return Ok(None),
            Err(err) => return Err(CalibreError::Io(err)),
        };

        let mut reader = std::io::BufReader::new(file);
        match bincode::deserialize_from::<_, PackagedProgramBlob>(&mut reader) {
            Ok(cache) => Ok(Some(cache)),
            Err(_) => {
                let _ = fs::remove_file(path);
                Ok(None)
            }
        }
    }
}

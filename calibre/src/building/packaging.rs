use crate::{CalibreEngine, CalibreError};
use calibre_lir::environment::{LirEnvironment, LirRegistry};
use calibre_mir::{environment::MiddleEnvironment, manifest::Manifest};
use calibre_parser::Parser;
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

    fn compile_and_package(
        &self,
        source: String,
        output_path: PathBuf,
        include_tests: bool,
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
            return Some(project.root.join("target").join("calibre").join("packages"));
        }

        let cwd = std::env::current_dir().ok()?;
        Some(cwd.join("target").join("calibre").join("packages"))
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

    fn compile_and_package(
        &self,
        source: String,
        output_path: PathBuf,
        _include_tests: bool,
    ) -> Result<(), CalibreError> {
        let full_source = self.compose_source(&source);
        let path = self
            .source_path
            .clone()
            .unwrap_or_else(|| PathBuf::from("<embedded>"));

        let mut parser = Parser::default();
        parser.set_source_path(Some(path.clone()));

        let ast = parser.produce_ast(&full_source);
        if !parser.errors.is_empty() {
            return Err(CalibreError::Parse {
                path,
                contents: full_source,
                errors: parser.errors,
            });
        }

        let (mut env, _scope, mut mir) = if let Some(metadata) = &self.package_metadata {
            MiddleEnvironment::new_and_evaluate_with_package(
                ast.clone(),
                path.clone(),
                Some(metadata.clone()),
                self.no_std,
                self.type_check,
            )
        } else {
            return Err(CalibreError::MissingPackageRoot);
        };

        let mir_errors = env.context.take_errors();
        if !mir_errors.is_empty() {
            return Err(CalibreError::Middle {
                path,
                ast_artifacts: Some(Box::new(ast)),
                contents: full_source,
                error: Box::new(calibre_mir::errors::MiddleErr::Multiple(mir_errors)),
            });
        }

        calibre_mir::inline::inline_small_calls(&mut mir, 20);

        let lir = LirEnvironment::lower(&env, mir);

        self.package(output_path, Manifest::from(&env), lir)
    }
}

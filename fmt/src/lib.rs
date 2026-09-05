use calibre_frontend::config::ProjectContext;
use calibre_parser::{
    Parser, ParserError,
    ast::{formatter::Formatter, nodes::AstNodeType},
};
use std::{
    error::Error,
    fmt, fs,
    io::Write,
    path::{Path, PathBuf},
};
use tracing::{debug, instrument};

#[cfg(any(feature = "wasm", target_arch = "wasm32"))]
pub mod wasm;

#[cfg(feature = "ffi")]
pub mod ffi;

#[cfg(feature = "python")]
pub mod python;

#[derive(Debug)]
pub enum FormatError {
    Read {
        path: PathBuf,
        source: std::io::Error,
    },
    Write {
        path: PathBuf,
        source: std::io::Error,
    },
    SourceParseFailed {
        path: PathBuf,
        contents: String,
        errors: Vec<ParserError>,
    },
    FormattedParseFailed {
        path: PathBuf,
        formatted: String,
        errors: Vec<ParserError>,
    },
    FormatterFailed {
        path: PathBuf,
        message: String,
    },
}

impl fmt::Display for FormatError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Read { path, .. } => write!(f, "failed to read {}", path.display()),
            Self::Write { path, .. } => write!(f, "failed to write {}", path.display()),
            Self::SourceParseFailed { path, .. } => {
                write!(
                    f,
                    "cannot format {} because source has parse errors",
                    path.display()
                )
            }
            Self::FormattedParseFailed { path, .. } => write!(
                f,
                "refusing to write {}: formatted output failed to parse",
                path.display()
            ),
            Self::FormatterFailed { path, message } => {
                write!(f, "failed to format {}: {}", path.display(), message)
            }
        }
    }
}

impl Error for FormatError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::Read { source, .. } => Some(source),
            Self::Write { source, .. } => Some(source),
            _ => None,
        }
    }
}

fn parse_errors(text: &str) -> Result<(), Vec<ParserError>> {
    let mut parser = Parser::default();
    let _ = parser.produce_ast(text);
    if parser.errors.is_empty() {
        Ok(())
    } else {
        Err(parser.errors)
    }
}

#[instrument(skip_all, fields(path = ?path.as_ref(), output = ?output.as_ref()))]
pub fn format_file(
    formatter: &mut Formatter,
    path: impl AsRef<Path>,
    output: impl AsRef<Path>,
) -> Result<(), Box<dyn Error>> {
    debug!("formatting file");
    let path = path.as_ref();
    let output = output.as_ref();

    let contents = fs::read_to_string(path).map_err(|source| FormatError::Read {
        path: path.to_path_buf(),
        source,
    })?;

    if let Err(errors) = parse_errors(&contents) {
        debug!(error_count = errors.len(), "source parse failed");
        return Err(Box::new(FormatError::SourceParseFailed {
            path: path.to_path_buf(),
            contents,
            errors,
        }));
    }

    debug!("applying formatter");
    let out = formatter.start_format(&contents, None).map_err(|err| {
        Box::new(FormatError::FormatterFailed {
            path: path.to_path_buf(),
            message: err.to_string(),
        }) as Box<dyn Error>
    })?;

    if let Err(errors) = parse_errors(&out) {
        debug!(error_count = errors.len(), "formatted parse failed");
        return Err(Box::new(FormatError::FormattedParseFailed {
            path: path.to_path_buf(),
            formatted: out,
            errors,
        }));
    }

    debug!("writing formatted output");
    fs::File::create(output)
        .and_then(|mut file| file.write_all(out.as_bytes()))
        .map_err(|source| FormatError::Write {
            path: output.to_path_buf(),
            source,
        })?;
    Ok(())
}

#[instrument(skip_all, fields(path = ?path.as_ref()))]
pub fn format_all(formatter: &mut Formatter, path: impl AsRef<Path>) -> Result<(), Box<dyn Error>> {
    debug!("formatting all imports");
    let contents = fs::read_to_string(path.as_ref())?;
    let imports = formatter.get_imports(&contents)?;

    let Some(base) = path.as_ref().parent() else {
        debug!("no parent directory, skipping");
        return Ok(());
    };

    for import in imports {
        let AstNodeType::ImportStatement {
            module,
            alias: _,
            values: _,
        } = import.node_type
        else {
            continue;
        };

        if module.len() == 1 {
            for candidate in [
                base.join(format!("{}.cal", module[0])),
                base.join(format!("{}/mod.cal", module[0])),
                base.join(format!("{}/main.cal", module[0])),
            ] {
                if candidate.exists() {
                    debug!(candidate = ?candidate, "formatting import");
                    format_all(formatter, &candidate)?;
                    break;
                }
            }
        }
    }

    format_file(formatter, path.as_ref(), path.as_ref())
}

#[instrument(skip_all, fields(root = ?root.as_ref()))]
pub fn format_recursive(
    formatter: &mut Formatter,
    root: impl AsRef<Path>,
) -> Result<(), Box<dyn Error>> {
    debug!("formatting recursively");
    fn walk(formatter: &mut Formatter, dir: &PathBuf) -> Result<(), Box<dyn Error>> {
        for entry in fs::read_dir(dir)? {
            let entry = entry?;
            let path = entry.path();
            if path.is_dir() {
                walk(formatter, &path)?;
                continue;
            }
            if path.extension().and_then(|x| x.to_str()) == Some("cal") {
                debug!(file = ?path, "formatting file");
                format_file(formatter, &path, &path)?;
            }
        }
        Ok(())
    }

    let root = if let Ok(Some(project)) = ProjectContext::load(root.as_ref()) {
        project.root.clone()
    } else if root.as_ref().is_dir() {
        root.as_ref().to_path_buf()
    } else if let Some(parent) = root.as_ref().parent() {
        parent.to_path_buf()
    } else {
        std::env::current_dir()?
    };

    walk(formatter, &root)
}

pub fn default_all_entry_path(cwd: impl AsRef<Path>) -> PathBuf {
    if let Ok(Some(project)) = ProjectContext::load(cwd.as_ref()) {
        let base = project.root.join(&project.config.package.src);
        let candidate = if base.is_dir() {
            base.join("main.cal")
        } else {
            base.clone()
        };
        if candidate.exists() {
            return candidate;
        }
    }

    let main = cwd.as_ref().join("main.cal");
    if main.exists() {
        return main;
    }

    let src_main = cwd.as_ref().join("src/main.cal");
    if src_main.exists() {
        return src_main;
    }

    main
}

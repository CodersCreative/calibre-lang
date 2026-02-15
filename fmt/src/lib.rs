use std::{error::Error, fmt, fs, io::Write, path::PathBuf};

use calibre_parser::{
    Parser, ParserError,
    ast::{NodeType, formatter::Formatter},
    lexer::Tokenizer,
};

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
    let mut tokenizer = Tokenizer::new(true);
    let tokens = match tokenizer.tokenize(text) {
        Ok(t) => t,
        Err(err) => return Err(vec![ParserError::Lexer(err)]),
    }
    .into_iter()
    .filter(|x| x.token_type != calibre_parser::lexer::TokenType::Comment)
    .collect();
    let mut parser = Parser::default();
    let _ = parser.produce_ast(tokens);
    if parser.errors.is_empty() {
        Ok(())
    } else {
        Err(parser.errors)
    }
}

pub fn format_file(
    formatter: &mut Formatter,
    path: &PathBuf,
    output: &PathBuf,
) -> Result<(), Box<dyn Error>> {
    let contents = fs::read_to_string(path).map_err(|source| FormatError::Read {
        path: path.clone(),
        source,
    })?;

    if let Err(errors) = parse_errors(&contents) {
        return Err(Box::new(FormatError::SourceParseFailed {
            path: path.clone(),
            contents,
            errors,
        }));
    }

    let out = formatter.start_format(contents, None).map_err(|err| {
        Box::new(FormatError::FormatterFailed {
            path: path.clone(),
            message: err.to_string(),
        }) as Box<dyn Error>
    })?;

    if let Err(errors) = parse_errors(&out) {
        return Err(Box::new(FormatError::FormattedParseFailed {
            path: path.clone(),
            formatted: out,
            errors,
        }));
    }

    fs::File::create(output)
        .and_then(|mut file| file.write_all(out.as_bytes()))
        .map_err(|source| FormatError::Write {
            path: output.clone(),
            source,
        })?;
    Ok(())
}

pub fn format_all(formatter: &mut Formatter, path: &PathBuf) -> Result<(), Box<dyn Error>> {
    let imports = formatter.get_imports(fs::read_to_string(path)?)?;

    let Some(base) = path.parent() else {
        return Ok(());
    };

    for import in imports {
        let NodeType::ImportStatement {
            module,
            alias: _,
            values: _,
        } = import.node_type
        else {
            continue;
        };

        if module.len() == 1 {
            let path = base.join(format!("{}.cal", module[0]));
            if path.exists() {
                format_all(formatter, &path)?;
            } else {
                let path = base.join(format!("{}/mod.cal", module[0]));
                if path.exists() {
                    format_all(formatter, &path)?;
                } else {
                    let path = base.join(format!("{}/main.cal", module[0]));
                    if path.exists() {
                        format_all(formatter, &path)?;
                    }
                }
            }
        }
    }

    format_file(formatter, path, path)
}

pub fn format_recursive(formatter: &mut Formatter, root: &PathBuf) -> Result<(), Box<dyn Error>> {
    fn walk(formatter: &mut Formatter, dir: &PathBuf) -> Result<(), Box<dyn Error>> {
        for entry in fs::read_dir(dir)? {
            let entry = entry?;
            let path = entry.path();
            if path.is_dir() {
                walk(formatter, &path)?;
                continue;
            }
            if path.extension().and_then(|x| x.to_str()) == Some("cal") {
                format_file(formatter, &path, &path)?;
            }
        }
        Ok(())
    }

    let root = if root.is_dir() {
        root.clone()
    } else if let Some(parent) = root.parent() {
        parent.to_path_buf()
    } else {
        std::env::current_dir()?
    };

    walk(formatter, &root)
}

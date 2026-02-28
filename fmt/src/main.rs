use calibre_diagnostics::{emit_error, emit_parser_errors};
use calibre_fmt::{FormatError, format_all, format_file, format_recursive};
use calibre_parser::ast::formatter::Formatter;
use clap::Parser;
use std::{env, error::Error, path::PathBuf, str::FromStr};

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[arg(index(1))]
    path: Option<String>,
    #[arg(short, long)]
    output: Option<String>,
    #[arg(short, long)]
    all: bool,
    #[arg(short = 'r', long = "recursive")]
    recursive: bool,
    #[arg(long, default_value_t = 200)]
    max_width: usize,
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();

    let path = if let Some(p) = args.path {
        PathBuf::from_str(&p)?
    } else if args.recursive {
        env::current_dir()?
    } else {
        PathBuf::from_str("./main.cal")?
    };
    let mut formatter = Formatter {
        max_width: args.max_width,
        ..Default::default()
    };

    let result = if args.recursive {
        format_recursive(&mut formatter, &path)
    } else if args.all {
        format_all(&mut formatter, &path)
    } else {
        let output = if let Some(x) = args.output {
            PathBuf::from_str(&x)?
        } else {
            path.clone()
        };

        format_file(&mut formatter, &path, &output)
    };

    if let Err(err) = result {
        if let Some(fmt_err) = err.downcast_ref::<FormatError>() {
            match fmt_err {
                FormatError::SourceParseFailed {
                    path,
                    contents,
                    errors,
                } => {
                    emit_parser_errors(path, contents, errors);
                }
                FormatError::FormattedParseFailed {
                    path,
                    formatted,
                    errors,
                } => {
                    emit_error(
                        path,
                        formatted,
                        "Formatter generated invalid syntax; refusing to write file.".to_string(),
                        None,
                    );
                    emit_parser_errors(path, formatted, errors);
                }
                FormatError::FormatterFailed { path, message } => {
                    let content = std::fs::read_to_string(path).unwrap_or_default();
                    emit_error(path, &content, message.clone(), None);
                }
                FormatError::Read { path, .. } | FormatError::Write { path, .. } => {
                    emit_error(path, "", fmt_err.to_string(), None);
                }
            }
            return Err("format failed".into());
        }
        return Err(err);
    }

    Ok(())
}

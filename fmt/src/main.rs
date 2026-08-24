use calibre_diagnostics::{emit_calibre_errors, emit_error};
use calibre_fmt::{FormatError, default_all_entry_path, format_all, format_file, format_recursive};
use calibre_parser::ast::formatter::Formatter;
use clap::Parser;
use std::{env, error::Error, path::PathBuf, str::FromStr};
use tracing::info;
use tracing_flame::FlameLayer;
use tracing_subscriber::{EnvFilter, Registry, fmt, layer::SubscriberExt};

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[arg(index(1))]
    input: Option<String>,
    #[arg(long)]
    stdin: bool,
    #[arg(short, long)]
    output: Option<String>,
    #[arg(short, long)]
    all: bool,
    #[arg(short, long)]
    recursive: bool,
    #[arg(long, default_value_t = 100)]
    max_width: usize,
    #[arg(long, default_value_t = false)]
    flamegraph: bool,
    #[arg(long, default_value = "error")]
    log: String,
    #[arg(long, default_value = "calibre.log")]
    log_file: String,
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();

    let file_appender = tracing_appender::rolling::never(".", &args.log_file);
    let (non_blocking, _guard) = tracing_appender::non_blocking(file_appender);

    let fmt_layer = fmt::Layer::default()
        .with_ansi(false)
        .pretty()
        .with_writer(non_blocking);
    let filter_layer = EnvFilter::from_str(&args.log)?;

    let _flame_guard = if args.flamegraph {
        let (flame_layer, guard) = FlameLayer::with_file("./tracing.folded").unwrap();
        let subscriber = Registry::default()
            .with(fmt_layer)
            .with(flame_layer)
            .with(filter_layer);
        tracing::subscriber::set_global_default(subscriber)?;
        Some(guard)
    } else {
        let subscriber = Registry::default().with(fmt_layer).with(filter_layer);
        tracing::subscriber::set_global_default(subscriber)?;
        None
    };

    let cwd = env::current_dir()?;
    info!("starting formatter with max_width: {}", args.max_width);

    let mut formatter = Formatter {
        max_width: args.max_width,
        ..Default::default()
    };

    let result = if args.stdin {
        if let Some(x) = args.input {
            match formatter.start_format(&x, None) {
                Ok(x) => {
                    print!("{x}");
                    return Ok(());
                }
                Err(e) => Err(e),
            }
        } else {
            return Ok(());
        }
    } else {
        let path = if let Some(x) = args.input {
            PathBuf::from_str(&x)?
        } else if args.recursive {
            cwd.clone()
        } else if args.all {
            default_all_entry_path(&cwd)
        } else {
            PathBuf::from_str("./main.cal")?
        };

        if args.recursive {
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
        }
    };

    if let Err(err) = result {
        if let Some(fmt_err) = err.downcast_ref::<FormatError>() {
            match fmt_err {
                FormatError::SourceParseFailed {
                    path,
                    contents,
                    errors,
                } => {
                    emit_calibre_errors(path, contents, errors);
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
                    emit_calibre_errors(path, formatted, errors);
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

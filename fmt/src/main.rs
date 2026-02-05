use cal_fmt::{format_all, format_file};
use calibre_parser::ast::formatter::Formatter;
use clap::Parser;
use std::{error::Error, path::PathBuf, str::FromStr};

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[arg(index(1))]
    path: Option<String>,
    #[arg(short, long)]
    output: Option<String>,
    #[arg(short, long)]
    all: bool,
    #[arg(long, default_value_t = 200)]
    max_width: usize,
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();

    let path = if let Some(p) = args.path {
        PathBuf::from_str(&p)?
    } else {
        PathBuf::from_str("./main.cl")?
    };
    let mut formatter = Formatter {
        max_width: args.max_width,
        ..Default::default()
    };

    if args.all {
        format_all(&mut formatter, &path)
    } else {
        let output = if let Some(x) = args.output {
            PathBuf::from_str(&x)?
        } else {
            path.clone()
        };

        format_file(&mut formatter, &path, &output)
    }
}

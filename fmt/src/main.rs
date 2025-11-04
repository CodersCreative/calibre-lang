use cal_fmt::Formatter;
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
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();

    let path = if let Some(p) = args.path {
        PathBuf::from_str(&p)?
    } else {
        PathBuf::from_str("./main.cl")?
    };
    let mut formatter = Formatter::default();

    if args.all {
        formatter.format_all(&path)
    } else {
        let output = if let Some(x) = args.output {
            PathBuf::from_str(&x)?
        } else {
            path.clone()
        };

        formatter.format_file(&path, &output)
    }
}

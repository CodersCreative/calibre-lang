use clap::{Parser, Subcommand, ValueEnum};
use std::path::PathBuf;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
pub struct Args {
    #[arg(long, default_value_t = false)]
    pub flamegraph: bool,
    #[arg(long, default_value = "error")]
    pub log: String,
    #[arg(long)]
    pub log_file: Option<PathBuf>,
    #[command(subcommand)]
    pub command: Option<Commands>,
}

#[derive(Debug, Default, Clone, Parser, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
pub enum Verbosity {
    All,
    Ast,
    Mir,
    Lir,
    Byte,
    #[default]
    None,
}

impl Verbosity {
    pub fn is_level(&self, other: &Verbosity) -> bool {
        matches!(self, Verbosity::All) || self == other
    }
}

#[derive(Subcommand, Debug)]
pub enum Commands {
    New {
        path: Option<String>,
        #[arg(long, default_value_t = false)]
        no_std: bool,
    },
    Run {
        paths: Vec<String>,
        #[arg(short, long)]
        example: Option<String>,
        #[arg(long)]
        verbosity: Option<Verbosity>,
        #[arg(long, default_value_t = false)]
        no_cache: bool,
        #[arg(long, default_value_t = true)]
        parallel: bool,
        #[arg(long)]
        no_std: Option<bool>,
        #[arg(last = true)]
        program_args: Vec<String>,
    },
    Clear,
    Test {
        path: Option<String>,
        #[arg(short, long)]
        example: Option<String>,
        #[arg(short, long, default_value_t = false)]
        recursive: bool,
        #[arg(short, long, default_value_t = false)]
        verbose: bool,
        #[arg(long)]
        tests: Vec<String>,
        #[arg(long)]
        suites: Vec<String>,
    },
    Bench {
        path: Option<String>,
        #[arg(short, long)]
        example: Option<String>,
        #[arg(short, long, default_value_t = false)]
        recursive: bool,
        #[arg(short, long, default_value_t = false)]
        verbose: bool,
        #[arg(long, default_value_t = 3)]
        warmup: usize,
        #[arg(long, default_value_t = 8)]
        min_runs: usize,
        #[arg(long, default_value_t = 200)]
        max_runs: usize,
        #[arg(long, default_value_t = 300)]
        time_limit_ms: u64,
        #[arg(long)]
        benchmarks: Vec<String>,
        #[arg(long)]
        suites: Vec<String>,
    },
    #[command(external_subcommand)]
    External(Vec<String>),
}

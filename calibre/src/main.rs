use crate::commands::{
    bench::BenchmarksBuilder, clear::Clear, new::NewBuilder, repl::Repl, run::RunBuilder,
    test::TestingBuilder,
};
use clap::Parser;
use cli::{Args, Commands};
use std::error::Error;
use std::path::PathBuf;
use std::str::FromStr;
use tracing::info;
use tracing_flame::FlameLayer;
use tracing_subscriber::{EnvFilter, Registry, fmt, layer::SubscriberExt};

mod cli;
mod commands;
mod config;

fn main() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();

    let (_guard, _flame_guard) = if let Some(path) = args.log_file {
        let file_appender = tracing_appender::rolling::never(
            path.parent()
                .map(|x: &std::path::Path| x.to_path_buf())
                .unwrap_or(PathBuf::from(".")),
            path.file_name()
                .map(|x: &std::ffi::OsStr| x.to_string_lossy().to_string())
                .unwrap_or(String::from("calibre.log")),
        );
        let (non_blocking, guard) = tracing_appender::non_blocking(file_appender);

        let fmt_layer = fmt::Layer::default()
            .with_ansi(false)
            .pretty()
            .with_writer(non_blocking);
        let filter_layer = EnvFilter::from_str(&args.log)?;

        (
            Some(guard),
            if args.flamegraph {
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
            },
        )
    } else {
        (None, None)
    };

    info!("tracing initialized");

    fn run_with_large_stack<F>(f: F) -> Result<(), Box<dyn Error>>
    where
        F: FnOnce() -> Result<(), String> + Send + 'static,
    {
        let handle = std::thread::Builder::new()
            .name("calibre-main".to_string())
            .stack_size(64 * 1024 * 1024)
            .spawn(f)?;
        match handle.join() {
            Ok(res) => res.map_err(|e| e.into()),
            Err(_) => Err("calibre runtime thread panicked".into()),
        }
    }

    run_with_large_stack(move || {
        smol::block_on(async move {
            match args.command {
                Some(Commands::New { path, no_std }) => {
                    NewBuilder::default()
                        .path(path)
                        .no_std(no_std)
                        .build()?
                        .execute()
                        .await
                }
                Some(Commands::Run {
                    path,
                    example,
                    verbosity,
                    no_std,
                    program_args,
                    no_cache,
                }) => {
                    RunBuilder::default()
                        .path(path)
                        .example(example)
                        .verbosity(verbosity)
                        .no_std(no_std)
                        .program_args(program_args)
                        .cache_enabled(!no_cache)
                        .build()?
                        .execute()
                        .await
                }
                Some(Commands::Clear) => Clear::default().execute(),
                Some(Commands::Test {
                    path,
                    example,
                    recursive,
                    verbose,
                    tests,
                    suites,
                }) => {
                    TestingBuilder::default()
                        .wanted(&tests)
                        .suites(&suites)
                        .path(path)
                        .example(example)
                        .recursive(recursive)
                        .verbose(verbose)
                        .build()?
                        .execute()
                        .await
                }
                Some(Commands::Bench {
                    path,
                    example,
                    recursive,
                    verbose,
                    warmup,
                    min_runs,
                    max_runs,
                    time_limit_ms,
                    benchmarks,
                    suites,
                }) => {
                    BenchmarksBuilder::default()
                        .wanted(&benchmarks)
                        .suites(&suites)
                        .path(path)
                        .example(example)
                        .recursive(recursive)
                        .warmup(warmup)
                        .min_runs(min_runs)
                        .max_runs(max_runs)
                        .time_limit_ms(time_limit_ms)
                        .verbose(verbose)
                        .build()?
                        .execute()
                        .await
                }
                Some(Commands::External(cmd)) => commands::utils::run_external_subcommand(&cmd),
                None => Repl::default().execute().await,
            }
        })
        .map_err(|e| e.to_string())
    })
}

use crate::commands::RunSuiteBuilder;
use crate::commands::utils::{
    fmt_ms, fmt_ms_f64, run_named_function_once, stddev_ms, vm_config_from_project,
};
use crate::config::load_project_from;
use calibre::CompileMode;
use derive_builder::Builder;
use std::error::Error;
use std::time::Duration;
use tracing::instrument;

#[derive(Builder, Default, Clone, Debug)]
pub struct Benchmarks<'a> {
    wanted: &'a [String],
    suites: &'a [String],
    path: Option<String>,
    example: Option<String>,
    recursive: bool,
    warmup: usize,
    min_runs: usize,
    max_runs: usize,
    time_limit_ms: u64,
    verbose: bool,
    type_check: bool,
}

impl<'a> Benchmarks<'a> {
    #[instrument]
    pub async fn execute(self) -> Result<(), Box<dyn Error>> {
        let cwd = std::env::current_dir()?;
        let project = load_project_from(&cwd).map_err(|e| format!("config error: {e}"))?;
        let vm_config = vm_config_from_project(project.as_ref());

        let benches = RunSuiteBuilder::default()
            .compile_mode(CompileMode::Bench)
            .wanted(self.wanted)
            .suites(self.suites)
            .path(self.path)
            .example(self.example)
            .recursive(self.recursive)
            .type_check(self.type_check)
            .build()?
            .execute()
            .await?;

        if benches.is_empty() {
            println!("No benchmarks found");
            return Ok(());
        }

        println!("running {} benchmarks", benches.len());
        println!(
            "benchmark settings: warmup={} min_runs={} max_runs={} time_limit={}ms",
            self.warmup, self.min_runs, self.max_runs, self.time_limit_ms
        );

        let mut rows: Vec<(String, usize, f64, f64, Duration, Duration, Duration)> = Vec::new();

        let mut failed = Vec::new();
        let mut ignored = Vec::new();

        for (path, registry, mappings, test) in benches {
            if test.skip || test.todo {
                let reason = test.skip_reason.as_ref().or(test.todo_reason.as_ref());
                if let Some(reason) = reason {
                    println!(
                        "benchmark {:?} ({})... ignored: {}",
                        test.name, path, reason
                    );
                } else {
                    println!("benchmark {:?} ({})... ignored", test.name, path);
                }
                ignored.push(test.name);
                continue;
            }

            let mut warmup_failed = None;
            for _ in 0..self.warmup {
                let warmup_result = run_named_function_once(
                    &vm_config,
                    registry.clone(),
                    mappings.clone(),
                    &test.function_name,
                    !self.verbose,
                );
                if let Err((msg, _captured)) = warmup_result {
                    warmup_failed = Some(msg);
                    break;
                }
            }

            if let Some(msg) = warmup_failed {
                failed.push((test.name, format!("warmup failed: {msg}")));
                continue;
            }

            let mut samples = Vec::new();
            let mut total = Duration::ZERO;
            let target = Duration::from_millis(self.time_limit_ms);
            let mut error = None;

            for _ in 0..self.max_runs.max(1) {
                let run_result = run_named_function_once(
                    &vm_config,
                    registry.clone(),
                    mappings.clone(),
                    &test.function_name,
                    !self.verbose,
                );
                match run_result {
                    Ok((d, _captured)) => {
                        samples.push(d);
                        total += d;
                    }
                    Err((msg, _captured)) => {
                        error = Some(msg);
                        break;
                    }
                }
                if samples.len() >= self.min_runs.max(1) && total >= target {
                    break;
                }
            }

            if let Some(msg) = error {
                failed.push((test.name, msg));
                continue;
            }

            samples.sort();
            let iters = samples.len().max(1);
            let min = *samples.first().unwrap_or(&Duration::ZERO);
            let max = *samples.last().unwrap_or(&Duration::ZERO);
            let p50 = samples[iters / 2];
            let mean = total.as_secs_f64() * 1000.0 / iters as f64;
            let stddev = stddev_ms(&samples, mean);
            rows.push((test.name.to_string(), iters, mean, stddev, p50, min, max));
        }

        if !rows.is_empty() {
            rows.sort_by(|a, b| a.2.partial_cmp(&b.2).unwrap_or(std::cmp::Ordering::Equal));
            let fastest = rows.first().map(|x| x.2).unwrap_or(1.0).max(1e-9);

            // TODO Use a minimal TUI library for this
            println!();
            println!(
                "{:<24}\t{:>10}\t{:>24}\t{:>10}\t{:>10}\t{:>10}\t{:>10}",
                "Benchmark", "Runs", "Time (mean ± σ)", "Median", "Min", "Max", "Relative"
            );

            for (ident, iters, mean, stddev, p50, min, max) in &rows {
                println!(
                    "{:<24}\t{:>10}\t{:>24}\t{:>10}\t{:>10}\t{:>10}\t{:>10}",
                    ident,
                    iters,
                    format!("{} ± {} ms", fmt_ms_f64(*mean), fmt_ms_f64(*stddev)),
                    fmt_ms(*p50),
                    fmt_ms(*min),
                    fmt_ms(*max),
                    format!("{:.2}x", *mean / fastest)
                );
            }

            let slowest = rows
                .iter()
                .map(|(_, _, mean, _, _, _, _)| *mean)
                .fold(0.0f64, f64::max)
                .max(1e-9);
            println!("\nsummary:");
            for (ident, _, mean, _, _, _, _) in rows {
                let ratio = (mean / slowest).clamp(0.0, 1.0);
                let width = (ratio * 18.0).round() as usize;
                let bar = "=".repeat(width.max(1));
                println!("\t{:<24} {}  ({:.2}x)", ident, bar, mean / fastest);
            }
        }

        if failed.is_empty() {
            Ok(())
        } else {
            println!("\nbenchmark failures:");
            for (ident, msg) in failed {
                println!("\t{ident}: {msg}");
            }
            Err("benchmarks failed".into())
        }
    }
}

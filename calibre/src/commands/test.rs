use crate::commands::run_suite;
use crate::commands::utils::{run_named_function_once, vm_config_from_project};
use crate::config::load_project_from;
use calibre::CompileMode;
use std::error::Error;

pub async fn execute(
    wanted: &[String],
    suites: &[String],
    path: Option<String>,
    example: Option<String>,
    recursive: bool,
    verbose: bool,
) -> Result<(), Box<dyn Error>> {
    let cwd = std::env::current_dir()?;
    let project = load_project_from(&cwd).map_err(|e| format!("config error: {e}"))?;
    let vm_config = vm_config_from_project(project.as_ref());
    let cases = run_suite(CompileMode::Test, wanted, suites, path, example, recursive).await?;

    if cases.is_empty() {
        println!("running 0 tests");
        println!("\ntest result: ok. 0 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out");
        return Ok(());
    }

    println!("running {} tests", cases.len());
    let mut failures = Vec::new();
    let mut passed = 0usize;
    let mut ignored = 0usize;

    for (path, registry, mappings, test) in cases {
        let label = format!("{:?} ({})", test.name, path);

        if test.skip || test.todo {
            ignored += 1;
            let reason = test.skip_reason.as_ref().or(test.todo_reason.as_ref());
            if let Some(reason) = reason {
                println!("test {label} ... ignored: {reason}");
            } else {
                println!("test {label} ... ignored");
            }
            continue;
        }

        let run_result = run_named_function_once(
            &vm_config,
            registry,
            mappings,
            &test.function_name,
            !verbose,
        );

        match run_result {
            Ok((_dur, _captured)) => {
                if test.panics {
                    println!("test {label} ... FAILED (expected panic but succeeded)");
                    failures.push((
                        label,
                        "expected panic but succeeded".to_string(),
                        String::new(),
                    ));
                } else {
                    passed += 1;
                    println!("test {label} ... ok");
                }
            }
            Err((msg, captured)) => {
                if test.panics {
                    passed += 1;
                    println!("test {label} ... ok (panicked as expected)");
                } else {
                    println!("test {label} ... FAILED");
                    failures.push((label, msg, captured));
                }
            }
        }
    }

    if !failures.is_empty() {
        println!("\nfailures:");
        for (label, msg, captured) in &failures {
            println!("\t{label}");
            println!("\t\t{msg}");
            if !verbose && !captured.trim().is_empty() {
                println!("\t\tcaptured output:");
                for line in captured.lines() {
                    println!("\t\t\t{line}");
                }
            }
        }
    }

    let failed = failures.len();
    let filtered_out = wanted.len().saturating_sub(passed + failed + ignored);
    let result = if failed == 0 { "ok" } else { "FAILED" };
    println!(
        "\ntest result: {result}. {passed} passed; {failed} failed; {ignored} ignored; 0 measured; {filtered_out} filtered out"
    );
    if failed == 0 {
        Ok(())
    } else {
        Err("tests failed".into())
    }
}

use crate::commands::RunSuiteBuilder;
use crate::commands::utils::run_named_function_once;
use calibre::CompileMode;
use calibre_frontend::config::ProjectContext;
use calibre_vm::config::VMConfig;
use derive_builder::Builder;
use std::error::Error;
use tracing::instrument;

#[derive(Builder, Debug)]
pub struct Testing<'a> {
    wanted: &'a [String],
    suites: &'a [String],
    path: Option<String>,
    example: Option<String>,
    recursive: bool,
    verbose: bool,
}

impl<'a> Testing<'a> {
    #[instrument]
    pub async fn execute(self) -> Result<(), Box<dyn Error>> {
        let cwd = std::env::current_dir()?;
        let project = ProjectContext::load(&cwd).map_err(|e| format!("config error: {e}"))?;
        let vm_config = project.as_ref().map(VMConfig::from).unwrap_or_default();

        let cases = RunSuiteBuilder::default()
            .compile_mode(CompileMode::Test)
            .wanted(self.wanted)
            .suites(self.suites)
            .path(self.path)
            .example(self.example)
            .recursive(self.recursive)
            .type_check(true)
            .build()?
            .execute()
            .await?;

        if cases.is_empty() {
            println!("running 0 tests");
            println!(
                "\ntest result: ok. 0 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out"
            );
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
                !self.verbose,
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
                        failures.push((
                            label,
                            msg,
                            captured
                                .into_iter()
                                .map(|x| x.to_string())
                                .collect::<Vec<_>>()
                                .join(""),
                        ));
                    }
                }
            }
        }

        if !failures.is_empty() {
            println!("\nfailures:");
            for (label, msg, captured) in &failures {
                println!("\t{label}");
                println!("\t\t{msg}");
                if !self.verbose && !captured.trim().is_empty() {
                    println!("\t\tcaptured output:");
                    for line in captured.lines() {
                        println!("\t\t\t{line}");
                    }
                }
            }
        }

        let failed = failures.len();
        let filtered_out = self.wanted.len().saturating_sub(passed + failed + ignored);
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
}

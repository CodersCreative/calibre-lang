use ariadne::{Color, Label, Report, ReportKind, Source};
use calibre_parser::{CalibreError, Parser, formatter::Formatter, lexer::Token};
use rayon::iter::IntoParallelRefIterator;
use rayon::prelude::*;
use std::{
    fs,
    path::{Path, PathBuf},
};
use walkdir::WalkDir;

#[derive(Debug)]
pub struct TestFailure {
    pub file: PathBuf,
    pub validation_type: ValidationType,
    pub message: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ValidationType {
    Lexer,
    Parser,
    Formatter,
}

#[derive(Default)]
pub struct TestHarness {
    failures: Vec<TestFailure>,
}

impl TestHarness {
    pub fn run(&mut self, dir: impl AsRef<Path>) -> Result<(), Vec<TestFailure>> {
        println!("Running parser tests...");
        println!("Directory: {}\n", dir.as_ref().display());

        let cal_files = self.find_cal_files(dir);
        println!("Found {} test files:", cal_files.len());

        let handles = cal_files
            .par_iter()
            .map(|file| {
                let mut harness = Self::default();
                harness.run_test(file);
                harness
            })
            .collect::<Vec<_>>();

        handles
            .into_iter()
            .for_each(|mut x| self.failures.append(&mut x.failures));

        self.print_summary();

        if self.failures.is_empty() {
            Ok(())
        } else {
            Err(std::mem::take(&mut self.failures))
        }
    }

    fn find_cal_files(&self, dir: impl AsRef<Path>) -> Vec<PathBuf> {
        let mut files = Vec::new();

        for entry in WalkDir::new(dir)
            .follow_links(true)
            .into_iter()
            .filter_map(|e| e.ok())
        {
            let path = entry.path();
            if path.extension().map(|ext| ext == "cal").unwrap_or(false) {
                files.push(path.to_path_buf());
            }
        }

        files.sort();
        files
    }

    fn run_test(&mut self, cal_file: &Path) {
        let base_name = cal_file.file_stem().unwrap().to_str().unwrap();
        let lex_file = cal_file.with_extension("lex");

        // TODO Find a nice way to test the ast
        // let ast_file = cal_file.with_extension("ast");

        println!("Testing: {}", base_name);

        let source = match fs::read_to_string(cal_file) {
            Ok(s) => s,
            Err(e) => {
                self.failures.push(TestFailure {
                    file: cal_file.to_path_buf(),
                    validation_type: ValidationType::Parser,
                    message: format!("Failed to read source file: {}", e),
                });
                return;
            }
        };

        // Lexer
        if lex_file.exists() {
            self.validate_lexer(cal_file, &source, &lex_file);
        }

        // Formatter
        self.validate_formatter(cal_file, &source);
    }

    fn validate_lexer(&mut self, cal_file: &Path, source: &str, lex_file: &Path) {
        let expected_tokens = match fs::read_to_string(lex_file) {
            Ok(s) => s,
            Err(e) => {
                self.failures.push(TestFailure {
                    file: cal_file.to_path_buf(),
                    validation_type: ValidationType::Lexer,
                    message: format!("Failed to read .lex file: {}", e),
                });
                return;
            }
        };

        let expected_lines: Vec<&str> = expected_tokens
            .lines()
            .map(|line| line.trim())
            .filter(|line| !line.is_empty())
            .collect();

        let parser = Parser::default();
        let actual_tokens = match parser.lex(source) {
            Ok(tokens) => tokens,
            Err(errs) => {
                let message = self.format_errors(cal_file, source, &errs);
                self.failures.push(TestFailure {
                    file: cal_file.to_path_buf(),
                    validation_type: ValidationType::Lexer,
                    message,
                });
                return;
            }
        };

        let actual_lines: Vec<&str> = actual_tokens
            .iter()
            .filter(|x| !matches!(x, Token::LineComment(_) | Token::BlockComment(_)))
            .map(|token| token.variant_name())
            .collect();

        if expected_lines != actual_lines {
            let mut message = String::new();
            message.push_str("Token mismatch:\n");

            let max_len = expected_lines.len().max(actual_lines.len());
            for i in 0..max_len {
                let expected = expected_lines
                    .get(i)
                    .map(|s| s.to_string())
                    .unwrap_or_else(|| "<none>".to_string());
                let actual = actual_lines
                    .get(i)
                    .map(|s| s.to_string())
                    .unwrap_or_else(|| "<none>".to_string());

                if expected != actual {
                    message.push_str(&format!(
                        "  Line {}: expected '{}', got '{}'\n",
                        i + 1,
                        expected,
                        actual
                    ));
                }
            }

            self.failures.push(TestFailure {
                file: cal_file.to_path_buf(),
                validation_type: ValidationType::Lexer,
                message,
            });
        }
    }

    fn validate_formatter(&mut self, file: &Path, source: &str) {
        let mut parser = Parser::default();
        let ast = parser.produce_ast(source);

        if !parser.errors.is_empty() {
            let message = self.format_errors(file, source, &parser.errors);
            self.failures.push(TestFailure {
                file: file.to_path_buf(),
                validation_type: ValidationType::Parser,
                message,
            });

            return;
        }

        let mut formatter = Formatter::default();
        let formatted = match formatter.start_format(source, None) {
            Ok(formatted) => formatted,
            Err(e) => {
                self.failures.push(TestFailure {
                    file: file.to_path_buf(),
                    validation_type: ValidationType::Formatter,
                    message: format!("Formatter error: {}", e),
                });
                return;
            }
        };

        // Try to re-parse the formatted output
        let mut parser = Parser::default();
        let ast2 = parser.produce_ast(&formatted);

        if !parser.errors.is_empty() {
            let message = self.format_errors(file, &formatted, &parser.errors);
            self.failures.push(TestFailure {
                file: file.to_path_buf(),
                validation_type: ValidationType::Formatter,
                message: format!(
                    "Formatter produced invalid code. Re-parse errors:\n{}\nOriginal debug:\n{:#?}\n\nFormatted debug:\n{:#?}\n\nFormatted output:\n{}",
                    message, ast, ast2, formatted
                ),
            });
            return;
        }

        if ast != ast2 {
            self.failures.push(TestFailure { file: file.to_path_buf(), validation_type: ValidationType::Formatter, message: format!(
                    "Formatter produced different code. Original debug:\n{:#?}\n\nFormatted debug:\n{:#?}\n\nFormatted output:\n{}",
                    ast,ast2, formatted
                ) });
        }
    }

    fn format_errors(&self, file: &Path, source: &str, errors: &[impl CalibreError]) -> String {
        let mut output = Vec::new();
        let file_id = file.to_string_lossy().to_string();

        for err in errors {
            let span = err.span();

            let mut report = Report::build(ReportKind::Error, (&file_id, span.to_range()))
                .with_code(err.code().to_string())
                .with_message(err.to_string());

            if !span.is_none() {
                report = report.with_label(
                    Label::new((&file_id, span.to_range()))
                        .with_message(err.to_string())
                        .with_color(Color::Red),
                );
            }

            if let Some(hint) = err.hint() {
                report = report.with_note(format!("hint: {hint}"));
            }

            report = report.with_note(format!("step: {}", err.step()));

            report
                .finish()
                .write((&file_id, Source::from(source)), &mut output)
                .unwrap();
        }

        String::from_utf8(output).unwrap_or_else(|_| "Failed to format errors".to_string())
    }

    fn print_summary(&self) {
        let total = self.failures.len();
        if total == 0 {
            println!("\nAll tests passed!");
        } else {
            println!("\n{} test(s) failed:", total);

            for failure in &self.failures {
                let stage = match failure.validation_type {
                    ValidationType::Lexer => "Lexer",
                    ValidationType::Parser => "Parser",
                    ValidationType::Formatter => "Formatter",
                };

                println!(
                    "\n-----------------------------\n({}): {}",
                    stage, failure.message
                );
            }
        }
    }
}

#[test]
fn run_harness() -> Result<(), ()> {
    let dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests");
    let mut harness = TestHarness::default();
    harness.run(dir).map_err(|_| ())
}

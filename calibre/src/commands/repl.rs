use crate::commands::utils::{is_persistent_decl, is_repl_file};
use calibre_lir::environment::LirEnvironment;
use calibre_mir::{environment::MiddleEnvironment, errors::MiddleErr};
use calibre_vm::{VM, config::VMConfig, conversion::VMRegistry, value::RuntimeValue};
use derive_builder::Builder;
use rustyline::DefaultEditor;
use rustyline::error::ReadlineError;
use smol::fs;
use std::{
    error::Error,
    path::{Path, PathBuf},
};
use tracing::instrument;

#[derive(Builder, Debug, Default)]
pub struct Repl {
    initial_session: Vec<String>,
}

impl Repl {
    #[instrument]
    pub async fn execute(self) -> Result<(), Box<dyn Error>> {
        let mut session = self.initial_session;
        let repl_path = PathBuf::from("<repl>");
        let mut editor = DefaultEditor::new()?;

        loop {
            let input = match editor.readline(">>> ") {
                Ok(line)
                    if line.eq_ignore_ascii_case("exit") || line.eq_ignore_ascii_case("quit") =>
                {
                    println!("exitting");
                    break;
                }
                Ok(line) if line.is_empty() => continue,
                Ok(line) => {
                    editor.add_history_entry(line.to_string())?;
                    line
                }
                Err(ReadlineError::Interrupted) => {
                    eprintln!("ctrl-d");
                    break;
                }
                Err(ReadlineError::Eof) => {
                    eprintln!("ctrl-c");
                    break;
                }
                Err(e) => {
                    eprintln!("Error : {}", e);
                    break;
                }
            };

            let line = input.trim();

            if let Some(rest) = line.strip_prefix("save ") {
                let path = PathBuf::from(rest.trim());
                let mut out = format!("// REPL {}\n", env!("CARGO_PKG_VERSION"));
                if !session.is_empty() {
                    out.push_str(&session.join("\n"));
                    out.push('\n');
                }
                fs::write(&path, out).await?;
                println!("Saved session to {}", path.display());
                continue;
            }

            if let Some(rest) = line.strip_prefix("load ") {
                let path = PathBuf::from(rest.trim());
                let contents = fs::read_to_string(&path).await?;
                if !is_repl_file(&contents) {
                    println!("Not a REPL file: {}", path.display());
                    continue;
                }
                session = contents.lines().skip(1).map(|x| x.to_string()).collect();
                println!("Loaded session from {}", path.display());
                continue;
            }

            let mut contents = String::new();
            if !session.is_empty() {
                contents.push_str(&session.join(";\n"));
                contents.push_str(";\n");
                contents = contents.replace("//", ";//").replace("/*", ";/*");
            }
            contents.push_str(line);

            let result = RunSource {
                contents,
                path: &repl_path,
                vm_config: VMConfig::default(),
            }
            .execute()
            .await;

            if let Ok((Some(_), txt)) = result {
                session.push(line.to_string());
                if !is_persistent_decl(line) {
                    println!("{}", txt);
                }
            }
        }

        Ok(())
    }
}

struct RunSource<'a> {
    contents: String,
    path: &'a Path,
    vm_config: VMConfig,
}

impl<'a> RunSource<'a> {
    async fn execute(self) -> Result<(Option<RuntimeValue>, String), Box<dyn Error>> {
        let mut parser = calibre_parser::Parser::default();

        let program = parser.produce_ast(&self.contents);

        if !parser.errors.is_empty() {
            calibre_diagnostics::emit_calibre_errors(self.path, &self.contents, &parser.errors);
            return Err(String::from("parse failed").into());
        }

        let (mut env, scope, middle_node) =
            MiddleEnvironment::new_and_evaluate(program, self.path.to_path_buf(), false);

        let mir_errors = env.context.take_errors();
        if !mir_errors.is_empty() {
            calibre_diagnostics::emit_mir_error(
                self.path,
                &self.contents,
                &MiddleErr::Multiple(mir_errors),
            );
            return Err(String::from("compile failed").into());
        }

        let middle_result = (env, scope, middle_node);

        let lir_result = LirEnvironment::lower_with_root(
            &middle_result.0,
            middle_result.2.clone(),
            "__repl".to_string(),
        );

        let mappings: Vec<String> = middle_result
            .0
            .symbols
            .variables
            .iter()
            .map(|x| x.0.to_string())
            .collect();

        let mut vm: VM = VM::new(VMRegistry::from(lir_result), mappings, self.vm_config);
        let mut globals = vm.registry.globals.clone();
        let repl_global = globals.remove("__repl");

        for (_, global) in globals {
            if let Err(err) = vm.run_global(&global) {
                calibre_diagnostics::emit_calibre_error(self.path, &self.contents, &err, None);
                return Err(String::from("runtime error").into());
            }
        }

        let Some(repl_global) = repl_global else {
            calibre_diagnostics::emit_error(
                self.path,
                &self.contents,
                "Missing REPL scope".to_string(),
                None,
            );
            return Err(String::from("runtime error").into());
        };

        let value = match vm.run_global(&repl_global) {
            Ok(value) => value,
            Err(err) => {
                calibre_diagnostics::emit_calibre_error(self.path, &self.contents, &err, None);
                return Err(String::from("runtime error").into());
            }
        };

        match value {
            RuntimeValue::Null => Ok((None, String::new())),
            other => {
                let txt = other.display(&mut vm);
                Ok((Some(other), txt))
            }
        }
    }
}

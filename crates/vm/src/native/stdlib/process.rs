use std::io::Write;
use std::process::{Command, Stdio};
use std::sync::Arc;

use calibre_parser::ast::ObjectMap;
use dumpster::sync::Gc;

use crate::{
    VM,
    error::RuntimeError,
    native::NativeFunction,
    value::{GcMap, RuntimeValue},
};

#[derive(Debug, Clone)]
struct RawExecOptions {
    command: String,
    args: Vec<String>,
    cwd: Option<String>,
    shell: bool,
    stdin: Option<String>,
    check: bool,
}

fn to_str(value: RuntimeValue) -> Result<String, RuntimeError> {
    match value {
        RuntimeValue::Str(v) => Ok(v.to_string()),
        other => Err(RuntimeError::UnexpectedType(other)),
    }
}

fn to_bool(value: RuntimeValue) -> Result<bool, RuntimeError> {
    match value {
        RuntimeValue::Bool(v) => Ok(v),
        other => Err(RuntimeError::UnexpectedType(other)),
    }
}

fn to_str_list(env: &VM, value: RuntimeValue) -> Result<Vec<String>, RuntimeError> {
    let RuntimeValue::List(values) = value else {
        return Err(RuntimeError::InvalidFunctionCall);
    };

    let mut out = Vec::with_capacity(values.0.len());
    for value in values.0.iter().cloned() {
        let value = env.resolve_value_for_op_ref(&value)?;
        out.push(to_str(value)?);
    }

    Ok(out)
}

fn field(map: &Gc<GcMap>, key: &str) -> Option<RuntimeValue> {
    map.as_ref().0.get(key).cloned()
}

fn parse_optional_string(value: RuntimeValue) -> Result<Option<String>, RuntimeError> {
    match value {
        RuntimeValue::Option(None) => Ok(None),
        RuntimeValue::Option(Some(v)) => to_str(v.as_ref().clone()).map(Some),
        RuntimeValue::Str(v) => Ok(Some(v.to_string())),
        other => Err(RuntimeError::UnexpectedType(other)),
    }
}

fn parse_options(env: &VM, options: RuntimeValue) -> Result<RawExecOptions, RuntimeError> {
    let RuntimeValue::Aggregate(_, map) = options else {
        return Err(RuntimeError::InvalidFunctionCall);
    };

    let Some(command) = field(&map, "command") else {
        return Err(RuntimeError::InvalidFunctionCall);
    };

    let command = to_str(env.resolve_value_for_op_ref(&command)?)?;
    let args = match field(&map, "args") {
        Some(value) => to_str_list(env, env.resolve_value_for_op_ref(&value)?)?,
        None => Vec::new(),
    };
    let cwd = match field(&map, "cwd") {
        Some(value) => parse_optional_string(env.resolve_value_for_op_ref(&value)?)?,
        None => None,
    };
    let shell = match field(&map, "shell") {
        Some(value) => to_bool(env.resolve_value_for_op_ref(&value)?)?,
        None => false,
    };
    let stdin = match field(&map, "stdin") {
        Some(value) => parse_optional_string(env.resolve_value_for_op_ref(&value)?)?,
        None => None,
    };
    let check = match field(&map, "check") {
        Some(value) => to_bool(env.resolve_value_for_op_ref(&value)?)?,
        None => false,
    };

    Ok(RawExecOptions {
        command,
        args,
        cwd,
        shell,
        stdin,
        check,
    })
}

fn format_command_line(command: &str, args: &[String]) -> String {
    if args.is_empty() {
        command.to_string()
    } else {
        format!("{command} {}", args.join(" "))
    }
}

fn process_result(command: String, status: i64, stdout: String, stderr: String) -> RuntimeValue {
    RuntimeValue::Aggregate(
        Some(String::from("ProcessResult")),
        Gc::new(GcMap(ObjectMap::from(vec![
            (
                String::from("command"),
                RuntimeValue::Str(Arc::new(command)),
            ),
            (String::from("status"), RuntimeValue::Int(status)),
            (String::from("success"), RuntimeValue::Bool(status == 0)),
            (String::from("stdout"), RuntimeValue::Str(Arc::new(stdout))),
            (String::from("stderr"), RuntimeValue::Str(Arc::new(stderr))),
        ]))),
    )
}

fn shell_command_line(command: &str, args: &[String]) -> String {
    format_command_line(command, args)
}

fn execute_raw(options: RawExecOptions) -> Result<RuntimeValue, String> {
    let mut command = if options.shell {
        #[cfg(target_os = "windows")]
        {
            let mut cmd = Command::new("cmd");
            cmd.arg("/C")
                .arg(shell_command_line(&options.command, &options.args));
            cmd
        }

        #[cfg(not(target_os = "windows"))]
        {
            let mut cmd = Command::new("sh");
            cmd.arg("-lc")
                .arg(shell_command_line(&options.command, &options.args));
            cmd
        }
    } else {
        let mut cmd = Command::new(&options.command);
        cmd.args(options.args.iter());
        cmd
    };

    if let Some(cwd) = options.cwd.as_deref()
        && !cwd.is_empty()
    {
        command.current_dir(cwd);
    }

    if options.stdin.is_some() {
        command.stdin(Stdio::piped());
    }

    let line = format_command_line(&options.command, &options.args);
    let mut child = command.spawn().map_err(|e| e.to_string())?;

    if let Some(input) = options.stdin
        && let Some(mut stdin) = child.stdin.take()
    {
        stdin
            .write_all(input.as_bytes())
            .map_err(|e| format!("failed to write stdin: {e}"))?;
    }

    let output = child.wait_with_output().map_err(|e| e.to_string())?;
    let status = output.status.code().map_or(-1, |x| x as i64);
    let stdout = String::from_utf8_lossy(&output.stdout).to_string();
    let stderr = String::from_utf8_lossy(&output.stderr).to_string();

    if options.check && status != 0 {
        let summary = if stderr.is_empty() {
            format!("command failed with status {status}: {line}")
        } else {
            format!("command failed with status {status}: {line}; stderr: {stderr}")
        };
        return Err(summary);
    }

    Ok(process_result(line, status, stdout, stderr))
}

pub struct ProcessRawExec;

impl NativeFunction for ProcessRawExec {
    fn name(&self) -> String {
        String::from("process.raw_exec")
    }

    fn run(&self, env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let mut parsed = None;
        for arg in args {
            let value = env.resolve_value_for_op_ref(&arg)?;
            if let RuntimeValue::Aggregate(_, map) = &value
                && map.as_ref().0.get("command").is_some()
            {
                parsed = Some(parse_options(env, value)?);
                break;
            }
        }
        let Some(options) = parsed else {
            return Err(RuntimeError::InvalidFunctionCall);
        };

        let result = match execute_raw(options) {
            Ok(value) => RuntimeValue::Result(Ok(Gc::new(value))),
            Err(err) => RuntimeValue::Result(Err(Gc::new(RuntimeValue::Str(Arc::new(err))))),
        };

        Ok(result)
    }
}

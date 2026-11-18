use crate::values::{
    Value, ValueTag,
    enums::{calibre_enum_discriminant, calibre_enum_get},
};
use std::{process::exit, sync::LazyLock};
use wasm_sync::Mutex;

enum ExitOutcome {
    Continue,
    Code(i64),
    Fail(String),
}

impl ExitOutcome {
    pub fn get(tag: ValueTag, payload: i64) -> Self {
        match Value::from_tag(tag, payload) {
            Value::Int(x) => ExitOutcome::Code(x),
            Value::Byte(x) => ExitOutcome::Code(x as i64),
            Value::UInt(x) => ExitOutcome::Code(x as i64),
            Value::Enum(agg) if !agg.is_null() => {
                if calibre_enum_discriminant(agg) == 0 {
                    ExitOutcome::Continue
                } else {
                    let payload_ptr = calibre_enum_get(agg);

                    if payload_ptr.is_null() {
                        ExitOutcome::Code(1)
                    } else {
                        let message = unsafe { (*payload_ptr).to_string() };
                        ExitOutcome::Fail(message)
                    }
                }
            }
            _ => ExitOutcome::Continue,
        }
    }

    pub fn apply(self) {
        match self {
            ExitOutcome::Continue => {}
            ExitOutcome::Code(code) => exit(code as i32),
            ExitOutcome::Fail(message) => {
                eprintln!("Error : {:?}", message);
                exit(1);
            }
        }
    }
}

#[derive(Default)]
struct FinExitState {
    code: i32,
    error: Option<String>,
}

static FIN_EXIT: LazyLock<Mutex<FinExitState>> =
    LazyLock::new(|| Mutex::new(FinExitState::default()));

#[unsafe(no_mangle)]
pub extern "C" fn calibre_exit_check(tag: ValueTag, payload: i64, phase: i64) {
    let outcome = ExitOutcome::get(tag, payload);

    if phase == 1 {
        if let Ok(mut state) = FIN_EXIT.lock() {
            match outcome {
                ExitOutcome::Continue => {}
                ExitOutcome::Code(code) => state.code = code as i32,
                ExitOutcome::Fail(message) => {
                    if state.error.is_none() {
                        state.error = Some(message);
                    }

                    state.code = 1;
                }
            }
        }

        return;
    }

    outcome.apply();
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_exit_flush() {
    let (code, error) = match FIN_EXIT.lock() {
        Ok(mut state) => (state.code, state.error.take()),
        Err(_) => (0, None),
    };

    let had_error = if let Some(message) = error {
        eprintln!("Error : {:?}", message);
        true
    } else {
        false
    };

    let code = if had_error && code == 0 { 1 } else { code };
    if code != 0 {
        exit(code);
    }
}

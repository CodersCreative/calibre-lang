use crate::{
    TaskState, VM,
    error::RuntimeError,
    native::{NativeFunction, pop_or_null},
    value::RuntimeValue,
};
use dumpster::sync::Gc;
use std::sync::{Arc, Mutex};

#[derive(Debug, Clone)]
pub struct GeneratorState {
    pub vm: VM,
    pub function_name: Arc<String>,
    pub captures: Arc<Vec<(String, RuntimeValue)>>,
    pub task_state: TaskState,
    pub index: i64,
    pub completed: bool,
}

#[derive(Debug, Clone)]
pub struct GeneratorResumeFn {
    pub state: Arc<Mutex<GeneratorState>>,
}

impl NativeFunction for GeneratorResumeFn {
    fn run(&self, _env: &mut VM, _args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let mut state = self
            .state
            .lock()
            .map_err(|_| RuntimeError::UnexpectedType(RuntimeValue::Null))?;

        if state.completed {
            return Ok(RuntimeValue::Option(None));
        }

        let Some(func) = state
            .vm
            .resolve_function_by_name(state.function_name.as_str())
        else {
            state.completed = true;
            return Ok(RuntimeValue::Option(None));
        };

        let captures = state.captures.clone();
        let mut task_state = std::mem::take(&mut state.task_state);
        let status = state.vm.run_function_with_budget(
            func.as_ref(),
            Vec::new(),
            captures,
            usize::MAX,
            &mut task_state,
        )?;
        state.task_state = task_state;

        if let Some(yielded) = state.task_state.yielded.take() {
            state.index += 1;
            return Ok(RuntimeValue::Option(Some(Gc::new(yielded))));
        }

        if status.is_some() {
            state.completed = true;
        }

        Ok(RuntimeValue::Option(None))
    }

    fn name(&self) -> String {
        String::from("gen_resume")
    }
}
pub struct GeneratorSuspendFn();

impl NativeFunction for GeneratorSuspendFn {
    fn name(&self) -> String {
        String::from("gen_suspend")
    }

    fn run(
        &self,
        _env: &mut VM,
        mut args: Vec<RuntimeValue>,
    ) -> Result<RuntimeValue, RuntimeError> {
        let value = pop_or_null(&mut args);
        Ok(RuntimeValue::GeneratorSuspend(Box::new(value)))
    }
}

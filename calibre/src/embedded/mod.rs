use crate::standalone::CalibreStandalone;
use crate::{CalibreEngine, CalibreError};
use calibre_vm::{VM, error::RuntimeError, native::NativeFunction, value::RuntimeValue};
use std::sync::Arc;

#[derive(Clone)]
pub struct NativeBinding {
    pub name: String,
    pub value: RuntimeValue,
}

pub type NativeFnCallback =
    dyn Fn(&mut VM, Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> + Send + Sync;

#[derive(Clone)]
pub struct ClosureNative {
    pub name: String,
    pub callback: Arc<NativeFnCallback>,
}

impl NativeFunction for ClosureNative {
    fn run(&self, env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        (self.callback)(env, args)
    }

    fn name(&self) -> String {
        self.name.clone()
    }
}

pub trait CalibreEmbedded {
    fn run(&mut self, source: impl Into<String>) -> Result<RuntimeValue, CalibreError>;

    fn with_prelude(self, source: impl Into<String>) -> Self;

    fn with_global(self, name: impl Into<String>, value: RuntimeValue) -> Self;

    fn with_native_function<N>(self, func: N) -> Self
    where
        N: NativeFunction + 'static;

    fn with_native_closure<F>(self, name: impl Into<String>, func: F) -> Self
    where
        F: Fn(&mut VM, Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError>
            + Send
            + Sync
            + 'static;
}

impl CalibreEmbedded for CalibreEngine {
    fn run(&mut self, source: impl Into<String>) -> Result<RuntimeValue, CalibreError> {
        self.run_source(source).map(|r| r.return_value)
    }

    fn with_prelude(mut self, source: impl Into<String>) -> Self {
        self.prelude.push(source.into());
        self
    }

    fn with_global(mut self, name: impl Into<String>, value: RuntimeValue) -> Self {
        self.bindings.push(NativeBinding {
            name: name.into(),
            value,
        });
        self
    }

    fn with_native_function<N>(mut self, func: N) -> Self
    where
        N: NativeFunction + 'static,
    {
        let name = func.name();

        self.bindings.push(NativeBinding {
            name,
            value: RuntimeValue::NativeFunction(Arc::new(func)),
        });

        self
    }

    fn with_native_closure<F>(mut self, name: impl Into<String>, func: F) -> Self
    where
        F: Fn(&mut VM, Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError>
            + Send
            + Sync
            + 'static,
    {
        let native = ClosureNative {
            name: name.into(),
            callback: Arc::new(func),
        };

        self.bindings.push(NativeBinding {
            name: native.name.clone(),
            value: RuntimeValue::NativeFunction(Arc::new(native)),
        });

        self
    }
}

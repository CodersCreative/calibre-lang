use calibre_common::errors::ValueErr;

use crate::runtime::{
    scope::InterpreterEnvironment, values::{RuntimeType, RuntimeValue}
};

impl RuntimeValue {
    pub fn make_similar(
        self,
        env: &InterpreterEnvironment,
        scope: &u64,
        rhs: Self,
    ) -> Result<(Self, Self), ValueErr<RuntimeValue, RuntimeType>> {
        if self.is_number() && rhs.is_number() {
            match (self, rhs) {
                (RuntimeValue::Float(x), RuntimeValue::Float(y)) => Ok((RuntimeValue::Float(x), RuntimeValue::Float(y))),
                (RuntimeValue::Int(x), RuntimeValue::Float(y)) => Ok((RuntimeValue::Float(x as f64), RuntimeValue::Float(y))),
                (RuntimeValue::Float(x), RuntimeValue::Int(y)) => Ok((RuntimeValue::Float(x), RuntimeValue::Float(y as f64))),
                (RuntimeValue::Int(x), RuntimeValue::Int(y)) => Ok((RuntimeValue::Int(x), RuntimeValue::Int(y))),
                _ => unimplemented!(),
            }
        } else {
            let rhs = rhs.into_type(env, scope, &(&self).into())?;
            Ok((self, rhs))
        }
    }
}

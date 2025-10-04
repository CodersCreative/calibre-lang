use crate::runtime::{
    scope::InterpreterEnvironment,
    values::{RuntimeType, RuntimeValue},
};

impl RuntimeValue {
    pub fn is_number(&self) -> bool {
        matches!(self, RuntimeValue::Float(_) | RuntimeValue::Int(_))
    }

    pub fn is_type(&self, env: &InterpreterEnvironment, scope: &u64, t: &RuntimeType) -> bool {
        if t == &RuntimeType::Dynamic {
            return true;
        }

        match self {
            RuntimeValue::Ref(_, x) => x == t,
            RuntimeValue::Null => false,
            RuntimeValue::NativeFunction(_) => false,
            RuntimeValue::Struct(_, _, _) => match self.into_type(env, scope, t) {
                Ok(_) => true,
                Err(_) => false,
            },
            RuntimeValue::Option(d, x) => d.is_none() || x == t,
            RuntimeValue::Result(d, x) => {
                if x == t {
                    true
                } else if let RuntimeType::Result(x, y) = x {
                    if d.is_ok() { &**x == t } else { &**y == t }
                } else {
                    false
                }
            }
            RuntimeValue::Tuple(data) => match t {
                RuntimeType::Tuple(data_types) => {
                    if data.len() != data_types.len() {
                        false
                    } else {
                        let mut valid = true;
                        for i in 0..data.len() {
                            if !data[i].is_type(env, scope, &data_types[i]) {
                                valid = false;
                                break;
                            }
                        }
                        valid
                    }
                }
                _ => false,
            },
            RuntimeValue::Enum(_, x, _, _) => match t {
                RuntimeType::Enum(_, y) => x == y,
                RuntimeType::Struct(_, y) => Some(x) == y.as_ref(),
                _ => false,
            },
            RuntimeValue::Function {
                parameters: val_parameters,
                return_type: val_type,
                is_async: val_is_async,
                ..
            } => match t {
                RuntimeType::Function {
                    return_type,
                    parameters,
                    is_async,
                } => {
                    if is_async != val_is_async {
                        return false;
                    };

                    if let Some(x) = &**return_type {
                        if let Some(y) = val_type {
                            if x != y {
                                return false;
                            }
                        } else {
                            return false;
                        }
                    }

                    if val_parameters.len() != parameters.len() {
                        return false;
                    };

                    true
                }
                _ => false,
            },
            RuntimeValue::List { data_type, .. } => match t {
                RuntimeType::List(z) => {
                    if data_type == z {
                        true
                    } else {
                        false
                    }
                }
                _ => false,
            },
            _ => *t == self.into(),
        }
    }
}

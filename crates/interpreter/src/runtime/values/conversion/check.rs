use crate::runtime::{
    scope::InterpreterEnvironment,
    values::{RuntimeType, RuntimeValue},
};

impl RuntimeValue {
    pub fn is_number(&self) -> bool {
        matches!(self, RuntimeValue::Float(_) | RuntimeValue::Int(_))
    }

    pub fn is_type(&self, env: &InterpreterEnvironment, t: &RuntimeType) -> bool {
        if t == &RuntimeType::Dynamic {
            return true;
        }

        match self {
            RuntimeValue::Ref(_, x) => x == t,
            RuntimeValue::Null => false,
            RuntimeValue::NativeFunction(_) => false,

            RuntimeValue::Option(d, x) => d.is_none() || x == t,
            RuntimeValue::Result(d, x) => {
                if x == t {
                    true
                } else if let RuntimeType::Result { ok, err } = x {
                    if d.is_ok() { &**ok == t } else { &**err == t }
                } else {
                    false
                }
            }
            RuntimeValue::Aggregate(Some(x), _) => match t {
                RuntimeType::Struct(y) => x == y,
                _ => false,
            },
            RuntimeValue::Aggregate(None, data) => match t {
                RuntimeType::Tuple(data_types) => {
                    if data.len() != data_types.len() {
                        false
                    } else {
                        let mut valid = true;
                        for i in 0..data.len() {
                            if let Some(x) = data.get(&i.to_string()) {
                                if !x.is_type(env, &data_types[i]) {
                                    valid = false;
                                    break;
                                }
                            } else {
                                return false;
                            }
                        }
                        valid
                    }
                }
                _ => false,
            },
            RuntimeValue::Enum(x, _, _) => match t {
                RuntimeType::Enum(y) => x == y,
                RuntimeType::Struct(y) => x == y,
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

                    if **return_type != *val_type {
                        return false;
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

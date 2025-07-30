use std::{cell::RefCell, rc::Rc};

use crate::runtime::{
    scope::{Environment, Scope},
    values::{RuntimeType, RuntimeValue},
};

impl RuntimeValue {
    pub fn is_number(&self) -> bool {
        matches!(
            self,
            RuntimeValue::Float(_)
                | RuntimeValue::Double(_)
                | RuntimeValue::Int(_)
                | RuntimeValue::Long(_)
                | RuntimeValue::UInt(_)
                | RuntimeValue::ULong(_)
                | RuntimeValue::Link(_, _, RuntimeType::Int)
                | RuntimeValue::Link(_, _, RuntimeType::UInt)
                | RuntimeValue::Link(_, _, RuntimeType::Long)
                | RuntimeValue::Link(_, _, RuntimeType::ULong)
                | RuntimeValue::Link(_, _, RuntimeType::Float)
                | RuntimeValue::Link(_, _, RuntimeType::Double)
        )
    }

    pub fn is_type(&self, env : &Environment, scope: &u64, t: &RuntimeType) -> bool {
        if t == &RuntimeType::Dynamic {
            return true;
        }

        match self {
            RuntimeValue::Link(_, _, x) => x == t,
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
            RuntimeValue::Enum(w, x, _, _) => match t {
                RuntimeType::Enum(z, y) => x == y && w == z,
                RuntimeType::Struct(z, y) => Some(x) == y.as_ref() && w == z,
                _ => false,
            },
            RuntimeValue::Function {
                parameters: val_parameters,
                body: _,
                return_type: val_type,
                is_async: val_is_async,
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
            RuntimeValue::List { data: _, data_type } => match t {
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

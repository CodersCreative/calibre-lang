use calibre_common::{environment::Type, errors::ValueErr};
use calibre_parser::ast::{ObjectType, ParserDataType};
use std::{collections::HashMap, fmt::Debug};

use crate::runtime::scope::CheckerEnvironment;

pub mod helper;

#[derive(Clone, PartialEq, PartialOrd, Debug)]
pub enum RuntimeType {
    Float,
    Dynamic,
    Int,
    Bool,
    Str,
    Char,
    Tuple(Vec<RuntimeType>),
    List(Box<Option<RuntimeType>>),
    Range,
    Option(Box<RuntimeType>),
    Result(Box<RuntimeType>, Box<RuntimeType>),
    Function {
        return_type: Box<Option<RuntimeType>>,
        parameters: Vec<(String, RuntimeType, bool)>,
        is_async: bool,
    },
    Enum(u64, String, Option<ObjectType<RuntimeType>>),
    Struct(u64, Option<String>, ObjectType<RuntimeType>),
    Ref(Box<RuntimeType>),
    Null,
    NativeFunction(Box<RuntimeType>),
}

impl calibre_common::environment::RuntimeType for RuntimeType {}
impl calibre_common::environment::RuntimeValue for RuntimeType {
    fn string(_txt: String) -> Self {
        Self::Str
    }

    fn constants() -> std::collections::HashMap<String, Self> {
        HashMap::from([
            (String::from("PI"), RuntimeType::Float),
            (String::from("FLOAT_MAX"), RuntimeType::Float),
            (String::from("INT_MAX"), RuntimeType::Int),
            (String::from("FLOAT_MIN"), RuntimeType::Float),
            (String::from("INT_MIN"), RuntimeType::Int),
            (String::from("true"), RuntimeType::Bool),
            (String::from("false"), RuntimeType::Bool),
        ])
    }

    fn natives() -> HashMap<String, Self> {
        let lst: Vec<(&'static str, RuntimeType)> = vec![
            ("print", RuntimeType::Null),
            (
                "ok",
                RuntimeType::Result(
                    Box::new(RuntimeType::Dynamic),
                    Box::new(RuntimeType::Dynamic),
                ),
            ),
            (
                "err",
                RuntimeType::Result(
                    Box::new(RuntimeType::Dynamic),
                    Box::new(RuntimeType::Dynamic),
                ),
            ),
            ("some", RuntimeType::Option(Box::new(RuntimeType::Dynamic))),
            ("len", RuntimeType::Int),
            ("panic", RuntimeType::Null),
            ("tuple", RuntimeType::Dynamic),
            ("trim", RuntimeType::Str),
            ("console.out", RuntimeType::Null),
            ("console.input", RuntimeType::Str),
            ("console.err", RuntimeType::Null),
            ("console.clear", RuntimeType::Null),
            ("thread.wait", RuntimeType::Null),
            ("random.generate", RuntimeType::Float),
            ("random.bool", RuntimeType::Bool),
            ("random.ratio", RuntimeType::Bool),
        ];

        let mut map = HashMap::new();

        for val in lst {
            map.insert(
                val.0.to_string(),
                RuntimeType::NativeFunction(Box::new(val.1)),
            );
        }

        map
    }
}
impl From<ParserDataType> for RuntimeType {
    fn from(value: ParserDataType) -> Self {
        match value {
            ParserDataType::Float => Self::Float,
            ParserDataType::Dynamic => Self::Dynamic,
            ParserDataType::Int => Self::Int,
            ParserDataType::Bool => Self::Bool,
            ParserDataType::Str => Self::Str,
            ParserDataType::Char => Self::Char,
            ParserDataType::Tuple(x) => {
                Self::Tuple(x.into_iter().map(|x| RuntimeType::from(x)).collect())
            }
            ParserDataType::List(x) => Self::List(Box::new(match *x {
                Some(x) => Some(RuntimeType::from(x)),
                None => None,
            })),
            ParserDataType::Ref(x, _) => Self::Ref(Box::new(RuntimeType::from(*x))),
            ParserDataType::Range => Self::Range,
            ParserDataType::Struct(x) => Self::Struct(0, x, ObjectType::Tuple(Vec::new())),
            ParserDataType::Function {
                return_type,
                parameters,
                is_async,
            } => Self::Function {
                return_type: Box::new(match *return_type {
                    Some(x) => Some(RuntimeType::from(x)),
                    None => None,
                }),
                parameters: parameters
                    .into_iter()
                    .map(|x| (String::new(), RuntimeType::from(x), false))
                    .collect(),
                is_async,
            },
            ParserDataType::Option(x) => Self::Option(Box::new(RuntimeType::from(*x))),
            ParserDataType::Result(x, y) => Self::Result(
                Box::new(RuntimeType::from(*x)),
                Box::new(RuntimeType::from(*y)),
            ),
        }
    }
}

impl RuntimeType {
    pub fn into_type(
        &self,
        env: &CheckerEnvironment,
        scope: &u64,
        t: &RuntimeType,
    ) -> Result<RuntimeType, ValueErr<RuntimeType, RuntimeType>> {
        if t == &RuntimeType::Dynamic {
            return Ok(self.clone());
        }

        let typ = t.clone();
        let panic_type = || -> Result<RuntimeType, ValueErr<RuntimeType, RuntimeType>> {
            return Err(ValueErr::Conversion(self.clone(), typ));
        };

        let result_case = || -> Result<RuntimeType, ValueErr<RuntimeType, RuntimeType>> {
            if let RuntimeType::Result(x, y) = t.clone() {
                if self.is_type(&*x) || self.is_type(&*y) {
                    Ok(t.clone())
                } else if let Ok(_) = self.into_type(env, scope, &*x) {
                    Ok(t.clone())
                } else if let Ok(_) = self.into_type(env, scope, &*y) {
                    Ok(t.clone())
                } else {
                    Err(ValueErr::Conversion(self.clone(), t.clone()))
                }
            } else {
                Err(ValueErr::Conversion(self.clone(), t.clone()))
            }
        };

        let option_case = || -> Result<RuntimeType, ValueErr<RuntimeType, RuntimeType>> {
            if let RuntimeType::Option(x) = t.clone() {
                if let Ok(_) = self.into_type(env, scope, &*x) {
                    return Ok(t.clone());
                }
            }
            return Err(ValueErr::Conversion(self.clone(), t.clone()));
        };

        let list_case = || -> Result<RuntimeType, ValueErr<RuntimeType, RuntimeType>> {
            if let RuntimeType::List(x) = t.clone() {
                if let Some(x) = *x {
                    if let Ok(_) = self.into_type(env, scope, &x) {
                        return Ok(t.clone());
                    } else {
                        Err(ValueErr::Conversion(self.clone(), t.clone()))
                    }
                } else {
                    Ok(t.clone())
                }
            } else {
                return Err(ValueErr::Conversion(self.clone(), t.clone()));
            }
        };

        if self.is_type(t) {
            return Ok(t.clone());
        }

        match (self.clone(), t.clone()) {
            (RuntimeType::Int, RuntimeType::Range)
            | (RuntimeType::Range, RuntimeType::Int)
            | (RuntimeType::Float, RuntimeType::Range)
            | (RuntimeType::Range, RuntimeType::Float) => Ok(t.clone()),
            (RuntimeType::Int, RuntimeType::Str)
            | (RuntimeType::Str, RuntimeType::Int)
            | (RuntimeType::Float, RuntimeType::Str)
            | (RuntimeType::Str, RuntimeType::Float) => Ok(t.clone()),
            (RuntimeType::Int, RuntimeType::Char)
            | (RuntimeType::Char, RuntimeType::Int)
            | (RuntimeType::Float, RuntimeType::Char)
            | (RuntimeType::Char, RuntimeType::Float) => Ok(t.clone()),
            (
                RuntimeType::Function {
                    return_type: val_type,
                    parameters: val_parameters,
                    is_async: val_is_async,
                },
                RuntimeType::Function {
                    return_type,
                    parameters,
                    is_async,
                },
            ) => {
                if is_async != val_is_async {
                    return panic_type();
                };

                if let Some(x) = &*return_type {
                    if let Some(y) = *val_type {
                        if !y.is_type(x) {
                            return panic_type();
                        }
                    } else {
                        return panic_type();
                    }
                }

                if val_parameters.len() != parameters.len() {
                    return panic_type();
                };

                Ok(self.clone())
            }
            (RuntimeType::Struct(_, _, _), RuntimeType::Struct(_, None, _)) => Ok(self.clone()),
            (
                RuntimeType::Struct(o, _, ObjectType::Tuple(x)),
                RuntimeType::Struct(_, Some(identifier), _),
            ) => {
                let Ok(Type::Struct(ObjectType::Tuple(properties))) =
                    env.get_object_type(&o, &identifier)
                else {
                    return panic_type();
                };
                let mut new_values = Vec::new();

                for (i, property) in properties.iter().enumerate() {
                    if let Some(val) = x.get(i as usize) {
                        new_values.push(val.into_type(env, scope, &property)?);
                    } else {
                        return panic_type();
                    }
                }

                Ok(RuntimeType::Struct(
                    o,
                    Some(identifier.to_string()),
                    ObjectType::Tuple(new_values),
                ))
            }
            (
                RuntimeType::Struct(o, _, ObjectType::Map(x)),
                RuntimeType::Struct(_, Some(identifier), _),
            ) => {
                let Type::Struct(ObjectType::Map(properties)) =
                    env.get_object_type(&o, &identifier)?
                else {
                    return panic_type();
                };
                let mut new_values = HashMap::new();

                for property in properties.iter() {
                    if let Some(val) = x.get(property.0) {
                        new_values
                            .insert(property.0.clone(), val.into_type(env, scope, &property.1)?);
                    } else {
                        return panic_type();
                    }
                }

                Ok(RuntimeType::Struct(
                    o,
                    Some(identifier.to_string()),
                    ObjectType::Map(new_values),
                ))
            }
            (RuntimeType::Enum(_, _, Some(z)), RuntimeType::Struct(w, None, _)) => {
                Ok(RuntimeType::Struct(w, None, z.clone()))
            }
            (RuntimeType::Enum(_, _, Some(z)), RuntimeType::Struct(w, Some(_), _)) => {
                match RuntimeType::Struct(w, None, z.clone()).into_type(env, scope, t) {
                    Ok(x) => Ok(x),
                    Err(_) => panic_type(),
                }
            }
            (RuntimeType::Enum(_, x, _), RuntimeType::Enum(_, y, _)) if x == y => Ok(t.clone()),
            (RuntimeType::List(_), t) => Ok(RuntimeType::List(Box::new(Some(t)))),
            (RuntimeType::Option(x), t) => {
                if let Ok(_) = x.into_type(env, scope, &t) {
                    Ok(t)
                } else {
                    panic_type()
                }
            }
            (RuntimeType::Result(x, y), t) => {
                if let Ok(_) = x.into_type(env, scope, &t) {
                    Ok(t)
                } else if let Ok(_) = y.into_type(env, scope, &t) {
                    Ok(t)
                } else {
                    panic_type()
                }
            }
            (_, RuntimeType::Str) => Ok(RuntimeType::Str),
            (_, RuntimeType::List(_)) => list_case(),
            (_, RuntimeType::Option(_)) => option_case(),
            (_, RuntimeType::Result(_, _)) => result_case(),
            (RuntimeType::Char, _) => RuntimeType::Str.into_type(env, scope, t),
            _ => panic_type(),
        }
    }

    pub fn is_type(&self, other: &RuntimeType) -> bool {
        if self == &RuntimeType::Dynamic || other == &RuntimeType::Dynamic {
            true
        } else if self == &RuntimeType::Int && other == &RuntimeType::Float
            || self == &RuntimeType::Float && other == &RuntimeType::Int
        {
            true
        } else {
            self == other
        }
    }
}

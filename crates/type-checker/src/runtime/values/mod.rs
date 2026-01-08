use calibre_common::{
    environment::{InterpreterFrom, Type},
    errors::ValueErr,
};
use calibre_parser::ast::{ObjectType, ParserDataType, ParserInnerType};
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
    List(Option<Box<RuntimeType>>),
    Range,
    Option(Box<RuntimeType>),
    Result(Box<RuntimeType>, Box<RuntimeType>),
    Function {
        return_type: Option<Box<RuntimeType>>,
        parameters: Vec<(String, RuntimeType, bool)>,
        is_async: bool,
    },
    Enum(u64, Option<ObjectType<RuntimeType>>),
    Struct(Option<u64>, ObjectType<RuntimeType>),
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

impl InterpreterFrom<ParserDataType> for RuntimeType {
    type Interpreter = CheckerEnvironment;
    fn interpreter_from(
        env: &Self::Interpreter,
        scope: &u64,
        value: ParserDataType,
    ) -> Result<Self, calibre_common::errors::ScopeErr> {
        Ok(match value.data_type {
            ParserInnerType::Float => Self::Float,
            ParserInnerType::Dynamic => Self::Dynamic,
            ParserInnerType::Int => Self::Int,
            ParserInnerType::Bool => Self::Bool,
            ParserInnerType::Str => Self::Str,
            ParserInnerType::Char => Self::Char,
            ParserInnerType::Scope(mut nodes) => {
                let typ = nodes.remove(nodes.len() - 1);
                let list = nodes
                    .into_iter()
                    .map(|x| {
                        let ParserInnerType::Struct(Some(x)) = x.data_type else {
                            panic!()
                        };
                        x
                    })
                    .collect();
                let scope = env.get_scope_list(scope.clone(), list)?;

                RuntimeType::interpreter_from(env, &scope, typ)?
            }
            ParserInnerType::Tuple(x) => Self::Tuple(
                x.into_iter()
                    .map(|x| RuntimeType::interpreter_from(env, scope, x).unwrap())
                    .collect(),
            ),
            ParserInnerType::List(x) => Self::List(match x {
                Some(x) => Some(Box::new(RuntimeType::interpreter_from(env, scope, *x)?)),
                None => None,
            }),
            ParserInnerType::Ref(x, _) => {
                Self::Ref(Box::new(RuntimeType::interpreter_from(env, scope, *x)?))
            }
            ParserInnerType::Range => Self::Range,

            ParserInnerType::Struct(x) => {
                if let Some(x) = x {
                    let y = env.get_object_pointer(scope, &x)?;

                    if let Some(obj) = env.objects.get(&y) {
                        match &obj.object_type {
                            Type::Enum(_) => Self::Enum(y, None),
                            Type::Struct(_) => Self::Struct(Some(y), ObjectType::Tuple(Vec::new())),
                            Type::NewType(x) => x.clone(),
                        }
                    } else {
                        Self::Struct(Some(y), ObjectType::Tuple(Vec::new()))
                    }
                } else {
                    Self::Struct(None, ObjectType::Tuple(Vec::new()))
                }
            }
            ParserInnerType::Function {
                return_type,
                parameters,
                is_async,
            } => Self::Function {
                return_type: match return_type {
                    Some(x) => Some(Box::new(RuntimeType::interpreter_from(env, scope, *x)?)),
                    None => None,
                },
                parameters: parameters
                    .into_iter()
                    .map(|x| {
                        (
                            String::new(),
                            RuntimeType::interpreter_from(env, scope, x).unwrap(),
                            false,
                        )
                    })
                    .collect(),
                is_async,
            },
            ParserInnerType::Option(x) => {
                Self::Option(Box::new(RuntimeType::interpreter_from(env, scope, *x)?))
            }
            ParserInnerType::Result(x, y) => Self::Result(
                Box::new(RuntimeType::interpreter_from(env, scope, *x)?),
                Box::new(RuntimeType::interpreter_from(env, scope, *y)?),
            ),
        })
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
                if let Some(x) = x {
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
            | (RuntimeType::Range, RuntimeType::Float)
            | (RuntimeType::Int, RuntimeType::Str)
            | (RuntimeType::Bool, RuntimeType::Int)
            | (RuntimeType::Int, RuntimeType::Bool)
            | (RuntimeType::Bool, RuntimeType::Float)
            | (RuntimeType::Float, RuntimeType::Bool)
            | (RuntimeType::Str, RuntimeType::Int)
            | (RuntimeType::Float, RuntimeType::Str)
            | (RuntimeType::Str, RuntimeType::Float)
            | (RuntimeType::Int, RuntimeType::Char)
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

                if let Some(x) = return_type {
                    if let Some(y) = val_type {
                        if !y.is_type(&*x) {
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
            (RuntimeType::Struct(_, _), RuntimeType::Struct(None, _)) => Ok(self.clone()),
            (
                RuntimeType::Struct(Some(_o), ObjectType::Tuple(x)),
                RuntimeType::Struct(Some(t_o), _),
            ) => {
                let Ok(Type::Struct(ObjectType::Tuple(properties))) = env.get_object_type(&t_o)
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
                    Some(t_o),
                    ObjectType::Tuple(new_values),
                ))
            }
            (
                RuntimeType::Struct(Some(_o), ObjectType::Map(x)),
                RuntimeType::Struct(Some(t_o), _),
            ) => {
                let Type::Struct(ObjectType::Map(properties)) = env.get_object_type(&t_o)? else {
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

                Ok(RuntimeType::Struct(Some(t_o), ObjectType::Map(new_values)))
            }
            (RuntimeType::Enum(_, Some(z)), RuntimeType::Struct(None, _)) => {
                Ok(RuntimeType::Struct(None, z.clone()))
            }
            (RuntimeType::Enum(_, Some(z)), RuntimeType::Struct(Some(_), _)) => {
                match RuntimeType::Struct(None, z.clone()).into_type(env, scope, t) {
                    Ok(x) => Ok(x),
                    Err(_) => panic_type(),
                }
            }
            (RuntimeType::Enum(x, _), RuntimeType::Enum(y, _)) if x == y => Ok(t.clone()),
            (RuntimeType::List(_), t) => Ok(RuntimeType::List(Some(Box::new(t)))),
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
        } else {
            self == other
        }
    }
}

pub mod helper;

use core::panic;
use std::{
    cell::RefCell, fmt::Debug, mem::discriminant, ops::Deref, rc::Rc, str::FromStr,
    string::ParseError,
};

use helper::{Block, Map};

use crate::{
    ast::RefMutability,
    runtime::scope::{
        Scope,
        structs::{get_struct, resolve_struct},
    },
};

#[derive(Clone, PartialEq, PartialOrd, Debug)]
pub enum NativeFunctions {
    Print,
}

impl NativeFunctions {}

#[derive(Clone, PartialEq, PartialOrd, Debug)]
pub enum RuntimeType {
    Float,
    Integer,
    Bool,
    Str,
    Char,
    List(Box<Option<RuntimeType>>),
    Function {
        return_type: Box<Option<RuntimeType>>,
        parameters: Vec<(RuntimeType, RefMutability)>,
        is_async: bool,
    },
    Struct(Option<String>),
}

impl FromStr for RuntimeType {
    type Err = ParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "int" => RuntimeType::Integer,
            "float" => RuntimeType::Float,
            "struct" => RuntimeType::Struct(None),
            "bool" => RuntimeType::Bool,
            "string" => RuntimeType::Str,
            "char" => RuntimeType::Char,
            _ => RuntimeType::Struct(Some(s.to_string())),
        })
    }
}

impl Into<RuntimeType> for RuntimeValue {
    fn into(self) -> RuntimeType {
        match self {
            Self::Null => panic!("Tried to convert null value to type"),
            Self::Float(_) => RuntimeType::Float,
            Self::Integer(_) => RuntimeType::Integer,
            Self::Struct(_, x) => RuntimeType::Struct(x),
            Self::Bool(_) => RuntimeType::Bool,
            Self::Str(_) => RuntimeType::Str,
            Self::Char(_) => RuntimeType::Char,
            Self::List { data, data_type } => RuntimeType::List(data_type),
            Self::Function {
                identifier,
                parameters,
                body,
                return_type,
                is_async,
            } => RuntimeType::Function {
                return_type: match return_type {
                    Some(x) => Box::new(Some(x)),
                    None => Box::new(None),
                },
                parameters: parameters
                    .iter()
                    .map(|x| (x.1.clone(), x.2.clone()))
                    .collect(),
                is_async,
            },
            Self::NativeFunction(_) => panic!("Cannot get type of native functions"),
        }
    }
}

#[derive(Clone, PartialEq, PartialOrd)]
pub enum RuntimeValue {
    Null,
    Float(f64),
    Integer(i64),
    Struct(Map<RuntimeValue>, Option<String>),
    Bool(bool),
    Str(String),
    Char(char),
    List {
        data: Vec<RuntimeValue>,
        data_type: Box<Option<RuntimeType>>,
    },
    // Option(Option<Box<RuntimeValue>>),
    // Result(Result<Box<RuntimeValue>, Box<RuntimeValue>>),
    Function {
        identifier: String,
        parameters: Vec<(String, RuntimeType, RefMutability)>,
        body: Block,
        return_type: Option<RuntimeType>,
        is_async: bool,
        // scope : Rc<Ref>
    },
    NativeFunction(NativeFunctions),
}

impl ToString for RuntimeValue {
    fn to_string(&self) -> String {
        match self {
            Self::Null => String::from("null"),
            Self::Float(x) => x.to_string(),
            Self::Integer(x) => x.to_string(),
            Self::Bool(x) => x.to_string(),
            Self::Struct(x, _) => format!("{:?}", x),
            Self::NativeFunction(x) => format!("native function : {:?}", x),
            Self::List { data, data_type: _ } => format!("{:?}", data),
            Self::Str(x) => x.to_string(),
            Self::Char(x) => x.to_string(),
            Self::Function {
                identifier,
                parameters,
                body: _,
                return_type,
                is_async: _,
            } => {
                format!("{:?} ({:?}) -> {:?}", identifier, parameters, return_type)
            }
        }
    }
}

impl Debug for RuntimeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

impl RuntimeValue {
    pub fn is_number(&self) -> bool {
        match self {
            RuntimeValue::Float(_) => true,
            RuntimeValue::Integer(_) => true,
            _ => false,
        }
    }

    pub fn call_native(&self, args: Vec<RuntimeValue>, scope: Rc<RefCell<Scope>>) -> RuntimeValue {
        if let Self::NativeFunction(func) = self {
            match func {
                NativeFunctions::Print => {
                    let mut output = String::new();

                    for arg in args {
                        output.push_str(&format!("{} ", arg.to_string()));
                    }

                    println!("{}", output.trim());

                    RuntimeValue::Null
                }
            }
        } else {
            panic!();
        }
    }

    pub fn is_type(&self, scope: Rc<RefCell<Scope>>, t: RuntimeType) -> bool {
        match self {
            RuntimeValue::Null => false,
            RuntimeValue::NativeFunction(_) => false,
            RuntimeValue::Struct(_, _) => {
                self.into_type(scope, t);
                true
            }
            RuntimeValue::Str(_) => match t {
                RuntimeType::Str => true,
                _ => false,
            },
            RuntimeValue::Bool(_) => match t {
                RuntimeType::Bool => true,
                _ => false,
            },
            RuntimeValue::Integer(_) => match t {
                RuntimeType::Integer => true,
                _ => false,
            },
            RuntimeValue::Float(_) => match t {
                RuntimeType::Float => true,
                _ => false,
            },
            RuntimeValue::Char(_) => match t {
                RuntimeType::Char => true,
                _ => false,
            },
            RuntimeValue::Function {
                identifier: _,
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
                    if is_async != *val_is_async {
                        return false;
                    };

                    if let Some(x) = *return_type {
                        if let Some(y) = val_type {
                            if x != *y {
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
                    if *data_type == z {
                        true
                    } else {
                        false
                    }
                }
                _ => false,
            },
        }
    }

    pub fn into_type(&self, scope: Rc<RefCell<Scope>>, t: RuntimeType) -> RuntimeValue {
        let panic_type = || {
            panic!("Cannot convert {:?} into {:?}", self, t);
            RuntimeValue::Null
        };

        let list_case = || {
            if let RuntimeType::List(x) = t.clone() {
                RuntimeValue::List {
                    data: vec![if let Some(x) = *x.clone() {
                        self.into_type(scope.clone(), x)
                    } else {
                        self.clone()
                    }],
                    data_type: x,
                }
            } else {
                panic!()
            }
        };

        match self {
            RuntimeValue::Integer(x) => match t {
                RuntimeType::Integer => self.clone(),
                RuntimeType::Float => RuntimeValue::Float(*x as f64),
                RuntimeType::Bool => RuntimeValue::Bool(match x {
                    0 => false,
                    1 => true,
                    _ => {
                        panic_type();
                        false
                    }
                }),
                RuntimeType::Struct(_) => panic_type(),
                RuntimeType::Str => RuntimeValue::Str(self.to_string()),
                RuntimeType::List(_) => list_case(),
                RuntimeType::Char => panic_type(),
                RuntimeType::Function { .. } => panic_type(),
            },
            RuntimeValue::Float(x) => match t {
                RuntimeType::Integer => RuntimeValue::Integer(*x as i64),
                RuntimeType::Float => self.clone(),
                RuntimeType::Bool => RuntimeValue::Bool(match x {
                    0.0 => false,
                    1.0 => true,
                    _ => {
                        panic_type();
                        false
                    }
                }),
                RuntimeType::Struct(_) => panic_type(),
                RuntimeType::List(_) => list_case(),
                RuntimeType::Str => RuntimeValue::Str(self.to_string()),
                RuntimeType::Char => panic_type(),
                RuntimeType::Function { .. } => panic_type(),
            },
            RuntimeValue::Str(x) => match t {
                RuntimeType::Integer => RuntimeValue::Integer(x.parse().unwrap()),
                RuntimeType::Float => RuntimeValue::Float(x.parse().unwrap()),
                RuntimeType::Bool => panic_type(),
                RuntimeType::Str => self.clone(),
                RuntimeType::Char => RuntimeValue::Char(x.chars().nth(0).unwrap()),
                RuntimeType::Struct(_) => panic_type(),
                RuntimeType::List(typ) if *typ == Some(RuntimeType::Char) => RuntimeValue::List {
                    data: x.chars().map(|c| RuntimeValue::Char(c)).collect(),
                    data_type: Box::new(Some(RuntimeType::Char)),
                },
                RuntimeType::List(_) => list_case(),
                RuntimeType::Function { .. } => panic_type(),
            },

            RuntimeValue::Null => panic_type(),
            RuntimeValue::Char(x) => {
                RuntimeValue::into_type(&RuntimeValue::Str(x.clone().to_string()), scope, t)
            }
            RuntimeValue::Struct(x, _) => match t {
                RuntimeType::Struct(None) => self.clone(),
                RuntimeType::Str => RuntimeValue::Str(self.to_string()),
                RuntimeType::Char => panic_type(),
                RuntimeType::List(_) => list_case(),
                RuntimeType::Struct(Some(identifier)) => {
                    let properties = get_struct(resolve_struct(scope, &identifier), &identifier);
                    for property in properties {
                        if !x.0.contains_key(&property.0) {
                            panic!("Struct Declaration is missing {:?}", property);
                        }
                    }
                    RuntimeValue::Struct(x.clone(), Some(identifier))
                }
                RuntimeType::Function { .. } => match t {
                    RuntimeType::List(_) => list_case(),
                    _ => panic_type(),
                },
                _ => panic_type(),
            },
            RuntimeValue::Function {
                identifier: _,
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
                    if is_async != *val_is_async {
                        panic!("Check whether the function or type is async.");
                    };

                    if let Some(x) = *return_type {
                        if let Some(y) = val_type {
                            if x != *y {
                                panic!("Function has wrong return type");
                            }
                        } else {
                            panic!("Expects a return type");
                        }
                    }

                    if val_parameters.len() != parameters.len() {
                        panic!("Parameters are not equal");
                    };

                    self.clone()
                }
                _ => panic_type(),
            },
            RuntimeValue::List { data, data_type: _ } => {
                if data.len() > 0 {
                    let t2 = discriminant(&data[0]);
                    let filtered: Vec<&RuntimeValue> =
                        data.iter().filter(|x| discriminant(*x) == t2).collect();

                    if data.len() == filtered.len() {
                        RuntimeValue::List {
                            data: data.clone(),
                            data_type: Box::new(Some(t)),
                        }
                    } else {
                        RuntimeValue::List {
                            data: data
                                .iter()
                                .map(|x| x.into_type(scope.clone(), t.clone()))
                                .collect(),
                            data_type: Box::new(Some(t)),
                        }
                    }
                } else {
                    RuntimeValue::List {
                        data: Vec::new(),
                        data_type: Box::new(Some(t)),
                    }
                }
            }

            _ => panic_type(),
        }
    }
}

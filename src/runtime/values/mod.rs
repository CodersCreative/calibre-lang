pub mod helper;

use core::panic;
use std::{
    cell::RefCell, collections::HashMap, fmt::Debug, mem::discriminant, rc::Rc, str::FromStr,
    string::ParseError,
};

use helper::Block;
use rand::seq::IndexedRandom;
use rustyline::DefaultEditor;
use thiserror::Error;

use crate::{
    ast::RefMutability,
    runtime::{
        scope::{
            Object, Scope, ScopeErr,
            objects::{get_object, resolve_object},
        },
        values::helper::ObjectType,
    },
};

#[derive(Error, Debug, Clone)]
pub enum ValueErr {
    #[error("Unable to convert: {0:?} -> {1:?}.")]
    Conversion(RuntimeValue, RuntimeType),
    #[error("{0}")]
    Scope(ScopeErr),
}

impl From<ScopeErr> for ValueErr {
    fn from(value: ScopeErr) -> Self {
        Self::Scope(value)
    }
}
#[derive(Clone, PartialEq, PartialOrd, Debug)]
pub enum NativeFunctions {
    Print,
    Input,
    Trim,
    Range,
}

impl NativeFunctions {}

#[derive(Clone, PartialEq, PartialOrd, Debug)]
pub enum RuntimeType {
    Float,
    Integer,
    Bool,
    Str,
    Char,
    Enum(String),
    Tuple(Vec<RuntimeType>),
    List(Box<Option<RuntimeType>>),
    Range,
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
            Self::Enum(x, _, _) => RuntimeType::Enum(x),
            Self::Struct(_, x) => RuntimeType::Struct(x),
            Self::Bool(_) => RuntimeType::Bool,
            Self::Str(_) => RuntimeType::Str,
            Self::Char(_) => RuntimeType::Char,
            Self::Range(_, _) => RuntimeType::Range,
            Self::Tuple(data) => RuntimeType::Tuple(data.into_iter().map(|x| x.into()).collect()),
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

#[derive(Clone, PartialEq, PartialOrd, Debug)]
pub enum RuntimeValue {
    Null,
    Float(f64),
    Integer(i64),
    Range(i32, i32),
    Struct(ObjectType<RuntimeValue>, Option<String>),
    Bool(bool),
    Str(String),
    Char(char),
    Enum(String, usize, Option<ObjectType<RuntimeValue>>),
    Tuple(Vec<RuntimeValue>),
    List {
        data: Vec<RuntimeValue>,
        data_type: Box<Option<RuntimeType>>,
    },
    // Option(Option<Box<RuntimeValue>>),
    // Result(Result<Box<RuntimeValue>, Box<RuntimeValue>>),
    Function {
        identifier: String,
        parameters: Vec<(String, RuntimeType, RefMutability, Option<RuntimeValue>)>,
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
            Self::Enum(x, y, z) => format!("{:?}({:?}) -> {:?}", x, y, z),
            Self::Range(from, to) => format!("{} -> {}", from, to),
            Self::Integer(x) => x.to_string(),
            Self::Bool(x) => x.to_string(),
            Self::Struct(x, _) => format!("{:?}", x),
            Self::NativeFunction(x) => format!("native function : {:?}", x),
            Self::List { data, data_type: _ } => format!("{:?}", data),
            Self::Tuple(data) => format!("{:?}", data),
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

// impl Debug for RuntimeValue {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "{}", self.to_string())
//     }
// }

impl RuntimeValue {
    pub fn is_number(&self) -> bool {
        match self {
            RuntimeValue::Float(_) => true,
            RuntimeValue::Integer(_) => true,
            _ => false,
        }
    }

    pub fn call_native(
        &self,
        args: Vec<(RuntimeValue, Option<RuntimeValue>)>,
        scope: Rc<RefCell<Scope>>,
    ) -> RuntimeValue {
        if let Self::NativeFunction(func) = self {
            match func {
                NativeFunctions::Print => {
                    let mut output = String::new();

                    for arg in args {
                        output.push_str(&format!("{} ", arg.0.to_string()));
                    }

                    println!("{}", output.trim());

                    RuntimeValue::Null
                }
                NativeFunctions::Range => {
                    if args.len() <= 1 {
                        let RuntimeValue::Integer(amt) = args[0].0 else {
                            panic!()
                        };
                        RuntimeValue::Range(0, amt as i32)
                    } else if args.len() == 2 {
                        let RuntimeValue::Integer(start) = args[0].0 else {
                            panic!()
                        };
                        let RuntimeValue::Integer(stop) = args[1].0 else {
                            panic!()
                        };
                        RuntimeValue::Range(start as i32, stop as i32)
                    } else if args.len() == 3 {
                        let RuntimeValue::Integer(start) = args[0].0 else {
                            panic!()
                        };
                        let RuntimeValue::Integer(stop) = args[1].0 else {
                            panic!()
                        };
                        let RuntimeValue::Integer(step) = args[2].0 else {
                            panic!()
                        };
                        RuntimeValue::List {
                            data: (start..stop)
                                .step_by(step as usize)
                                .map(|x| RuntimeValue::Integer(x))
                                .collect(),
                            data_type: Box::new(Some(RuntimeType::Integer)),
                        }
                    } else {
                        RuntimeValue::Null
                    }
                }
                NativeFunctions::Trim => {
                    if let Some((RuntimeValue::Str(x), _)) = args.get(0) {
                        RuntimeValue::Str(x.trim().to_string())
                    } else {
                        RuntimeValue::Null
                    }
                }
                NativeFunctions::Input => {
                    let mut editor = DefaultEditor::new().unwrap();
                    let txt = match args.get(0) {
                        Some(x) => x.0.clone(),
                        None => RuntimeValue::Str("".to_string()),
                    };

                    let readline = editor.readline(&txt.to_string());
                    match readline {
                        Ok(line) => RuntimeValue::Str(line),
                        Err(_) => RuntimeValue::Null,
                    }
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
            RuntimeValue::Struct(_, _) => match self.into_type(scope, t) {
                Ok(_) => true,
                Err(_) => false,
            },
            RuntimeValue::Tuple(data) => match t {
                RuntimeType::Tuple(data_types) => {
                    if data.len() != data_types.len() {
                        false
                    } else {
                        let mut valid = true;
                        for i in 0..data.len() {
                            if !data[i].is_type(scope.clone(), data_types[i].clone()) {
                                valid = false;
                                break;
                            }
                        }
                        valid
                    }
                }
                _ => false,
            },
            RuntimeValue::Str(_) => match t {
                RuntimeType::Str => true,
                _ => false,
            },
            RuntimeValue::Enum(x, _, _) => match t {
                RuntimeType::Enum(y) => *x == y,
                _ => false,
            },
            RuntimeValue::Range(_, _) => match t {
                RuntimeType::Range => true,
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

    pub fn into_type(
        &self,
        scope: Rc<RefCell<Scope>>,
        t: RuntimeType,
    ) -> Result<RuntimeValue, ValueErr> {
        let typ = t.clone();
        let panic_type = || {
            return Err(ValueErr::Conversion(self.clone(), typ));
        };

        let list_case = || {
            if let RuntimeType::List(x) = t.clone() {
                Ok(RuntimeValue::List {
                    data: vec![if let Some(x) = *x.clone() {
                        self.into_type(scope.clone(), x)?
                    } else {
                        self.clone()
                    }],
                    data_type: x,
                })
            } else {
                return Err(ValueErr::Conversion(self.clone(), t.clone()));
            }
        };

        match self {
            RuntimeValue::Integer(x) => match t {
                RuntimeType::Integer => Ok(self.clone()),
                RuntimeType::Range => Ok(RuntimeValue::Range(0, *x as i32)),
                RuntimeType::Float => Ok(RuntimeValue::Float(*x as f64)),
                RuntimeType::Bool => Ok(RuntimeValue::Bool(match x {
                    0 => false,
                    1 => true,
                    _ => {
                        return panic_type();
                    }
                })),
                RuntimeType::Struct(_) => panic_type(),
                RuntimeType::Str => Ok(RuntimeValue::Str(self.to_string())),
                RuntimeType::List(_) => list_case(),
                RuntimeType::Tuple(_) => panic_type(),
                RuntimeType::Char => panic_type(),
                RuntimeType::Function { .. } => panic_type(),
                RuntimeType::Enum { .. } => panic_type(),
            },
            RuntimeValue::Tuple(data) => match t {
                RuntimeType::Tuple(data_types) => {
                    if data.len() == data_types.len() {
                        let mut valid = Vec::new();
                        for i in 0..data.len() {
                            valid.push(data[i].into_type(scope.clone(), data_types[i].clone())?);
                        }
                        Ok(RuntimeValue::Tuple(valid))
                    } else {
                        panic_type()
                    }
                }
                _ => panic_type(),
            },
            RuntimeValue::Float(x) => match t {
                RuntimeType::Integer => Ok(RuntimeValue::Integer(*x as i64)),
                RuntimeType::Range => Ok(RuntimeValue::Range(0, *x as i32)),
                RuntimeType::Float => Ok(self.clone()),
                RuntimeType::Bool => Ok(RuntimeValue::Bool(match x {
                    0.0 => false,
                    1.0 => true,
                    _ => {
                        return panic_type();
                    }
                })),
                RuntimeType::Struct(_) => panic_type(),
                RuntimeType::List(_) => list_case(),
                RuntimeType::Str => Ok(RuntimeValue::Str(self.to_string())),
                RuntimeType::Char => panic_type(),
                RuntimeType::Function { .. } => panic_type(),
                RuntimeType::Enum { .. } => panic_type(),
                RuntimeType::Tuple(_) => panic_type(),
            },
            RuntimeValue::Range(from, to) => match t {
                RuntimeType::Range => Ok(self.clone()),
                RuntimeType::Integer => Ok(RuntimeValue::Integer(*to as i64)),
                RuntimeType::Float => Ok(RuntimeValue::Float(*to as f64)),
                RuntimeType::List(_) => Ok(RuntimeValue::List {
                    data: (*from..*to)
                        .into_iter()
                        .map(|x| RuntimeValue::Integer(x as i64))
                        .collect(),
                    data_type: Box::new(Some(RuntimeType::Integer)),
                }),
                _ => panic_type(),
            },
            RuntimeValue::Str(x) => match t {
                RuntimeType::Integer => Ok(RuntimeValue::Integer(x.parse().unwrap())),
                RuntimeType::Float => Ok(RuntimeValue::Float(x.parse().unwrap())),
                RuntimeType::Bool => panic_type(),
                RuntimeType::Str => Ok(self.clone()),
                RuntimeType::Char => Ok(RuntimeValue::Char(x.chars().nth(0).unwrap())),
                RuntimeType::Struct(_) => panic_type(),
                RuntimeType::List(typ) if *typ == Some(RuntimeType::Char) => {
                    Ok(RuntimeValue::List {
                        data: x.chars().map(|c| RuntimeValue::Char(c)).collect(),
                        data_type: Box::new(Some(RuntimeType::Char)),
                    })
                }
                RuntimeType::List(_) => list_case(),
                RuntimeType::Tuple(_) => panic_type(),
                RuntimeType::Range => panic_type(),
                RuntimeType::Function { .. } => panic_type(),
                RuntimeType::Enum { .. } => panic_type(),
            },

            RuntimeValue::Null => panic_type(),
            RuntimeValue::Char(x) => {
                RuntimeValue::into_type(&RuntimeValue::Str(x.clone().to_string()), scope, t)
            }
            RuntimeValue::Enum(x, _, z) => match t.clone() {
                RuntimeType::Struct(None) => {
                    if let Some(z) = z {
                        Ok(RuntimeValue::Struct(z.clone(), None))
                    } else {
                        panic_type()
                    }
                }
                RuntimeType::Struct(Some(y)) => {
                    if let Some(z) = z {
                        match RuntimeValue::Struct(z.clone(), None).into_type(scope.clone(), t) {
                            Ok(x) => Ok(x),
                            Err(_) => self.into_type(scope, RuntimeType::Enum(y)),
                        }
                    } else {
                        self.into_type(scope, RuntimeType::Enum(y))
                    }
                }
                RuntimeType::Enum(y) => {
                    if x == &y {
                        Ok(self.clone())
                    } else {
                        panic_type()
                    }
                }
                RuntimeType::Str => Ok(RuntimeValue::Str(self.to_string())),
                RuntimeType::Char => panic_type(),
                RuntimeType::Range => panic_type(),
                RuntimeType::List(_) => list_case(),
                _ => panic_type(),
            },
            RuntimeValue::Struct(ObjectType::Tuple(x), _) => match t {
                RuntimeType::Struct(None) => Ok(self.clone()),
                RuntimeType::Str => Ok(RuntimeValue::Str(self.to_string())),
                RuntimeType::Char => panic_type(),
                RuntimeType::List(_) => list_case(),
                RuntimeType::Range => panic_type(),
                RuntimeType::Struct(Some(identifier)) => {
                    let Object::Struct(ObjectType::Tuple(properties)) =
                        get_object(resolve_object(scope.clone(), &identifier)?, &identifier)?
                    else {
                        return panic_type();
                    };
                    let mut new_values = Vec::new();

                    for (i, property) in properties.iter().enumerate() {
                        if let Some(val) = x.get(i as usize) {
                            new_values.push(val.into_type(scope.clone(), property.clone())?);
                        } else {
                            return panic_type();
                        }
                    }

                    Ok(RuntimeValue::Struct(
                        ObjectType::Tuple(new_values),
                        Some(identifier),
                    ))
                }
                RuntimeType::Function { .. } => match t {
                    RuntimeType::List(_) => list_case(),
                    _ => panic_type(),
                },
                _ => panic_type(),
            },
            RuntimeValue::Struct(ObjectType::Map(x), _) => match t {
                RuntimeType::Struct(None) => Ok(self.clone()),
                RuntimeType::Str => Ok(RuntimeValue::Str(self.to_string())),
                RuntimeType::Char => panic_type(),
                RuntimeType::List(_) => list_case(),
                RuntimeType::Range => panic_type(),
                RuntimeType::Struct(Some(identifier)) => {
                    let Object::Struct(ObjectType::Map(properties)) =
                        get_object(resolve_object(scope.clone(), &identifier)?, &identifier)?
                    else {
                        return panic_type();
                    };
                    let mut new_values = HashMap::new();

                    for property in &properties {
                        if let Some(val) = x.get(property.0) {
                            new_values.insert(
                                property.0.clone(),
                                val.into_type(scope.clone(), property.1.clone())?,
                            );
                        } else {
                            return panic_type();
                        }
                    }

                    Ok(RuntimeValue::Struct(
                        ObjectType::Map(new_values),
                        Some(identifier),
                    ))
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
                        return panic_type();
                    };

                    if let Some(x) = *return_type {
                        if let Some(y) = val_type {
                            if x != *y {
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
                _ => panic_type(),
            },
            RuntimeValue::List { data, data_type } => {
                if let Some(RuntimeType::Struct(Some(x))) = *data_type.clone() {
                    if let RuntimeType::Struct(Some(ref y)) = t {
                        if &x == y {
                            return Ok(RuntimeValue::Struct(
                                ObjectType::Tuple(data.to_vec()),
                                Some(x),
                            ));
                        }
                    }
                }
                if data.len() > 0 {
                    let t2 = discriminant(&data[0]);
                    let filtered: Vec<&RuntimeValue> =
                        data.iter().filter(|x| discriminant(*x) == t2).collect();

                    Ok(if data.len() == filtered.len() {
                        RuntimeValue::List {
                            data: data.clone(),
                            data_type: Box::new(Some(t)),
                        }
                    } else {
                        let mut dta = Vec::new();
                        for d in data {
                            dta.push(d.into_type(scope.clone(), t.clone())?);
                        }
                        RuntimeValue::List {
                            data: dta,
                            data_type: Box::new(Some(t)),
                        }
                    })
                } else {
                    Ok(RuntimeValue::List {
                        data: Vec::new(),
                        data_type: Box::new(Some(t)),
                    })
                }
            }

            _ => panic_type(),
        }
    }
}

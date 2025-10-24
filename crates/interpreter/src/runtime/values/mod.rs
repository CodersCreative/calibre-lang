use crate::runtime::scope::InterpreterEnvironment;
use crate::{
    native::{self, NativeFunction},
    runtime::values::helper::MatchBlock,
};
use calibre_common::environment::{InterpreterFrom, Type};
use calibre_common::errors::ScopeErr;
use calibre_parser::ast::{ObjectType, ParserDataType};
use helper::Block;
use std::{collections::HashMap, f64::consts::PI, fmt::Debug, rc::Rc};

pub mod conversion;
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
        parameters: Vec<RuntimeType>,
        is_async: bool,
    },
    Enum(u64),
    Struct(Option<u64>),
    Ref(Box<RuntimeType>),
}

impl calibre_common::environment::RuntimeType for RuntimeType {}
impl calibre_common::environment::RuntimeValue for RuntimeValue {
    fn string(txt: String) -> Self {
        Self::Str(txt)
    }

    fn constants() -> std::collections::HashMap<String, Self> {
        HashMap::from([
            (String::from("PI"), RuntimeValue::Float(PI)),
            (String::from("FLOAT_MAX"), RuntimeValue::Float(f64::MAX)),
            (String::from("INT_MAX"), RuntimeValue::Int(i64::MAX)),
            (String::from("FLOAT_MIN"), RuntimeValue::Float(f64::MIN)),
            (String::from("INT_MIN"), RuntimeValue::Int(i64::MIN)),
            (String::from("true"), RuntimeValue::Bool(true)),
            (String::from("false"), RuntimeValue::Bool(false)),
        ])
    }

    fn natives() -> HashMap<String, Self> {
        let lst: Vec<(&'static str, Rc<dyn NativeFunction>)> = vec![
            ("print", Rc::new(native::stdlib::console::Out())),
            ("ok", Rc::new(native::global::OkFn())),
            ("err", Rc::new(native::global::ErrFn())),
            ("some", Rc::new(native::global::SomeFn())),
            ("len", Rc::new(native::global::Len())),
            ("trim", Rc::new(native::global::Trim())),
            ("tuple", Rc::new(native::global::TupleFn())),
            ("panic", Rc::new(native::global::PanicFn())),
            ("console.out", Rc::new(native::stdlib::console::Out())),
            ("console.input", Rc::new(native::stdlib::console::Input())),
            ("console.err", Rc::new(native::stdlib::console::ErrFn())),
            ("console.clear", Rc::new(native::stdlib::console::Clear())),
            ("thread.wait", Rc::new(native::stdlib::thread::Wait())),
            (
                "random.generate",
                Rc::new(native::stdlib::random::Generate()),
            ),
            ("random.bool", Rc::new(native::stdlib::random::Bool())),
            ("random.ratio", Rc::new(native::stdlib::random::Ratio())),
        ];

        let mut map = HashMap::new();

        for val in lst {
            map.insert(val.0.to_string(), RuntimeValue::NativeFunction(val.1));
        }

        map
    }
}

impl InterpreterFrom<ParserDataType> for RuntimeType {
    type Interpreter = InterpreterEnvironment;
    fn interpreter_from(
        env: &Self::Interpreter,
        scope: &u64,
        value: ParserDataType,
    ) -> Result<Self, ScopeErr> {
        Ok(match value {
            ParserDataType::Float => Self::Float,
            ParserDataType::Dynamic => Self::Dynamic,
            ParserDataType::Int => Self::Int,
            ParserDataType::Bool => Self::Bool,
            ParserDataType::Str => Self::Str,
            ParserDataType::Char => Self::Char,
            ParserDataType::Tuple(x) => Self::Tuple(
                x.into_iter()
                    .map(|x| RuntimeType::interpreter_from(env, scope, x).unwrap())
                    .collect(),
            ),
            ParserDataType::List(x) => Self::List(match x {
                Some(x) => Some(Box::new(RuntimeType::interpreter_from(env, scope, *x)?)),
                None => None,
            }),
            ParserDataType::Scope(mut nodes) => {
                let typ = nodes.remove(nodes.len() - 1);
                let list = nodes
                    .into_iter()
                    .map(|x| {
                        let ParserDataType::Struct(Some(x)) = x else {
                            panic!()
                        };
                        x
                    })
                    .collect();
                let scope = env.get_scope_list(scope.clone(), list)?;

                RuntimeType::interpreter_from(env, &scope, typ)?
            }
            ParserDataType::Range => Self::Range,
            ParserDataType::Struct(x) => {
                if let Some(x) = x {
                    let y = env.get_object_pointer(scope, &x)?;

                    if let Some(obj) = env.objects.get(&y) {
                        match &obj.object_type {
                            Type::Enum(_) => Self::Enum(y),
                            Type::Struct(_) => Self::Struct(Some(y)),
                            Type::NewType(x) => x.clone(),
                        }
                    } else {
                        Self::Struct(Some(y))
                    }
                } else {
                    Self::Struct(None)
                }
            }
            ParserDataType::Function {
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
                    .map(|x| RuntimeType::interpreter_from(env, scope, x).unwrap())
                    .collect(),
                is_async,
            },
            ParserDataType::Option(x) => {
                Self::Option(Box::new(RuntimeType::interpreter_from(env, scope, *x)?))
            }
            ParserDataType::Result(x, y) => Self::Result(
                Box::new(RuntimeType::interpreter_from(env, scope, *x)?),
                Box::new(RuntimeType::interpreter_from(env, scope, *y)?),
            ),
            ParserDataType::Ref(x, _) => {
                Self::Ref(Box::new(RuntimeType::interpreter_from(env, scope, *x)?))
            }
        })
    }
}

impl Into<RuntimeType> for &RuntimeValue {
    fn into(self) -> RuntimeType {
        match self {
            RuntimeValue::Null => RuntimeType::Dynamic,
            RuntimeValue::Float(_) => RuntimeType::Float,
            RuntimeValue::Int(_) => RuntimeType::Int,
            RuntimeValue::Ref(_, x) => x.clone(),
            RuntimeValue::Enum(y, _, _) => RuntimeType::Enum(*y),
            RuntimeValue::Struct(y, _) => RuntimeType::Struct(*y),
            RuntimeValue::Bool(_) => RuntimeType::Bool,
            RuntimeValue::Option(_, x) => x.clone(),
            RuntimeValue::Result(_, x) => x.clone(),
            RuntimeValue::Str(_) => RuntimeType::Str,
            RuntimeValue::Char(_) => RuntimeType::Char,
            RuntimeValue::Range(_, _) => RuntimeType::Range,
            RuntimeValue::Tuple(data) => {
                RuntimeType::Tuple(data.into_iter().map(|x| x.into()).collect())
            }
            RuntimeValue::List { data_type, .. } => RuntimeType::List(data_type.clone()),
            RuntimeValue::Function {
                parameters,
                return_type,
                is_async,
                ..
            } => RuntimeType::Function {
                return_type: match return_type {
                    Some(x) => Some(Box::new(x.clone())),
                    None => None,
                },
                parameters: parameters.iter().map(|x| x.1.clone()).collect(),
                is_async: *is_async,
            },
            RuntimeValue::NativeFunction(_) => RuntimeType::Dynamic,
        }
    }
}

#[derive(Clone, PartialEq, PartialOrd, Debug)]
pub enum RuntimeValue {
    Null,
    Float(f64),
    Int(i64),
    Range(i64, i64),
    Bool(bool),
    Str(String),
    Char(char),
    Struct(Option<u64>, ObjectType<RuntimeValue>),
    Enum(u64, usize, Option<ObjectType<RuntimeValue>>),
    Tuple(Vec<RuntimeValue>),
    Ref(u64, RuntimeType),
    List {
        data: Vec<RuntimeValue>,
        data_type: Option<Box<RuntimeType>>,
    },
    Option(Option<Box<RuntimeValue>>, RuntimeType),
    Result(Result<Box<RuntimeValue>, Box<RuntimeValue>>, RuntimeType),
    Function {
        parameters: Vec<(String, RuntimeType, Option<RuntimeValue>)>,
        body: FunctionType,
        return_type: Option<RuntimeType>,
        is_async: bool,
    },
    NativeFunction(Rc<dyn NativeFunction>),
}

#[derive(Clone, PartialEq, PartialOrd, Debug)]
pub enum FunctionType {
    Regular(Block),
    Match(MatchBlock),
}

fn print_list<T: ToString>(data: &Vec<T>, open: char, close: char) -> String {
    let mut txt = String::from(open);

    for val in data.iter() {
        txt.push_str(&format!("{}, ", val.to_string()));
    }

    let _ = (txt.pop(), txt.pop());
    txt.push(close);

    txt
}

impl ToString for RuntimeValue {
    fn to_string(&self) -> String {
        match self {
            Self::Null => String::from("null"),
            Self::Float(x) => x.to_string(),
            Self::Int(x) => x.to_string(),
            Self::Enum(x, y, z) => format!("{:?}({:?}) -> {:?}", x, y, z),
            Self::Range(from, to) => format!("{}..{}", from, to),
            Self::Ref(_, ty) => format!("link -> {:?}", ty),
            Self::Bool(x) => x.to_string(),
            Self::Struct(x, y) => format!("{y:?} = {x:?}"),
            Self::NativeFunction(_) => format!("native function"),
            Self::List { data, data_type: _ } => print_list(data, '[', ']'),
            Self::Tuple(data) => print_list(data, '(', ')'),
            Self::Option(x, _) => format!("{:?}", x),
            Self::Result(x, _) => format!("{:?}", x),
            Self::Str(x) => x.to_string(),
            Self::Char(x) => x.to_string(),
            Self::Function {
                parameters,
                body: _,
                return_type,
                is_async: _,
            } => {
                format!("({:?}) -> {:?}", parameters, return_type)
            }
        }
    }
}

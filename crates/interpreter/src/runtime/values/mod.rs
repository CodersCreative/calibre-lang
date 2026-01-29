use crate::native::{self, NativeFunction};
use crate::runtime::scope::InterpreterEnvironment;
use calibre_common::environment::InterpreterFrom;
use calibre_common::errors::ScopeErr;
use calibre_mir::environment::MiddleTypeDefType;
use calibre_mir_ty::MiddleNode;
use calibre_parser::ast::{ObjectMap, ParserDataType, ParserInnerType};
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
    List(Box<RuntimeType>),
    Range,
    Option(Box<RuntimeType>),
    Result {
        ok: Box<RuntimeType>,
        err: Box<RuntimeType>,
    },
    Function {
        return_type: Box<RuntimeType>,
        parameters: Vec<RuntimeType>,
        is_async: bool,
    },
    Enum(String),
    Struct(String),
    Ref(Box<RuntimeType>),
    Null,
    Type,
    NativeFn(Box<RuntimeType>),
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
            (
                String::from("none"),
                RuntimeValue::Option(None, RuntimeType::Option(Box::new(RuntimeType::Dynamic))),
            ),
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
            ("discriminant", Rc::new(native::global::DiscriminantFn())),
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

impl InterpreterFrom<ParserDataType<MiddleNode>> for RuntimeType {
    type Interpreter = InterpreterEnvironment;
    fn interpreter_from(
        env: &Self::Interpreter,
        scope: &u64,
        value: ParserDataType<MiddleNode>,
    ) -> Result<Self, ScopeErr> {
        Ok(match value.data_type {
            ParserInnerType::Null
            | ParserInnerType::Auto(_)
            | ParserInnerType::DollarIdentifier(_)
            | ParserInnerType::StructWithGenerics { .. } => Self::Null,
            ParserInnerType::NativeFunction(x) => {
                Self::NativeFn(Box::new(RuntimeType::interpreter_from(env, scope, *x)?))
            }
            ParserInnerType::Float => Self::Float,
            ParserInnerType::Dynamic => Self::Dynamic,
            ParserInnerType::Int => Self::Int,
            ParserInnerType::Bool => Self::Bool,
            ParserInnerType::Str => Self::Str,
            ParserInnerType::Char => Self::Char,
            ParserInnerType::Tuple(x) => Self::Tuple(
                x.into_iter()
                    .map(|x| RuntimeType::interpreter_from(env, scope, x).unwrap())
                    .collect(),
            ),
            ParserInnerType::List(x) => {
                Self::List(Box::new(RuntimeType::interpreter_from(env, scope, *x)?))
            }
            ParserInnerType::Scope(_nodes) => unreachable!(),
            ParserInnerType::Range => Self::Range,
            ParserInnerType::Struct(x) => {
                if let Some(obj) = env.objects.get(&x) {
                    match &obj.object_type {
                        MiddleTypeDefType::Enum(_) => Self::Enum(x),
                        MiddleTypeDefType::Struct(_) => Self::Struct(x),
                        MiddleTypeDefType::NewType(x) => {
                            RuntimeType::interpreter_from(env, scope, x.clone()).unwrap()
                        }
                    }
                } else {
                    Self::Struct(x)
                }
            }
            ParserInnerType::Function {
                return_type,
                parameters,
                is_async,
            } => Self::Function {
                return_type: Box::new(RuntimeType::interpreter_from(env, scope, *return_type)?),
                parameters: parameters
                    .into_iter()
                    .map(|x| RuntimeType::interpreter_from(env, scope, x).unwrap())
                    .collect(),
                is_async,
            },
            ParserInnerType::Option(x) => {
                Self::Option(Box::new(RuntimeType::interpreter_from(env, scope, *x)?))
            }
            ParserInnerType::Result { ok, err } => Self::Result {
                ok: Box::new(RuntimeType::interpreter_from(env, scope, *ok)?),
                err: Box::new(RuntimeType::interpreter_from(env, scope, *err)?),
            },
            ParserInnerType::Ref(x, _) => {
                Self::Ref(Box::new(RuntimeType::interpreter_from(env, scope, *x)?))
            }
            ParserInnerType::Comp { .. } => unreachable!(),
        })
    }
}

impl Into<RuntimeType> for &RuntimeValue {
    fn into(self) -> RuntimeType {
        match self {
            RuntimeValue::Type(_) => RuntimeType::Type,
            RuntimeValue::Float(_) => RuntimeType::Float,
            RuntimeValue::Int(_) => RuntimeType::Int,
            RuntimeValue::Ref(_, x) => x.clone(),
            RuntimeValue::Enum(y, _, _) => RuntimeType::Enum(y.clone()),
            RuntimeValue::Bool(_) => RuntimeType::Bool,
            RuntimeValue::Option(_, x) => x.clone(),
            RuntimeValue::Result(_, x) => x.clone(),
            RuntimeValue::Str(_) => RuntimeType::Str,
            RuntimeValue::Char(_) => RuntimeType::Char,
            RuntimeValue::Range(_, _) => RuntimeType::Range,
            RuntimeValue::Aggregate(Some(x), _) => RuntimeType::Struct(x.to_string()),
            RuntimeValue::Aggregate(None, data) => {
                RuntimeType::Tuple(data.0.iter().map(|x| (&x.1).into()).collect())
            }
            RuntimeValue::List { data_type, .. } => RuntimeType::List(data_type.clone()),
            RuntimeValue::Function {
                parameters,
                return_type,
                is_async,
                ..
            } => RuntimeType::Function {
                return_type: Box::new(return_type.clone()),
                parameters: parameters.iter().map(|x| x.1.clone()).collect(),
                is_async: *is_async,
            },
            RuntimeValue::NativeFunction(_x) => {
                RuntimeType::NativeFn(Box::new(RuntimeType::Dynamic))
            }
            RuntimeValue::Null => RuntimeType::Null,
        }
    }
}

#[derive(Clone, PartialEq, PartialOrd, Debug)]
pub enum RuntimeValue {
    Null,
    Float(f64),
    Type(RuntimeType),
    Int(i64),
    Range(i64, i64),
    Bool(bool),
    Str(String),
    Char(char),
    Aggregate(Option<String>, ObjectMap<RuntimeValue>),
    Enum(String, usize, Option<Box<RuntimeValue>>),
    Ref(String, RuntimeType),
    List {
        data: Vec<RuntimeValue>,
        data_type: Box<RuntimeType>,
    },
    Option(Option<Box<RuntimeValue>>, RuntimeType),
    Result(Result<Box<RuntimeValue>, Box<RuntimeValue>>, RuntimeType),
    Function {
        parameters: Vec<(String, RuntimeType)>,
        body: Block,
        return_type: RuntimeType,
        is_async: bool,
    },
    NativeFunction(Rc<dyn NativeFunction>),
}

fn print_list<T: ToString>(data: &Vec<T>, open: char, close: char) -> String {
    let mut txt = String::from(open);

    for val in data.iter() {
        txt.push_str(&format!("{}, ", val.to_string()));
    }

    txt = txt.trim().trim_end_matches(",").trim().to_string();
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
            Self::Aggregate(x, y) => format!("{y:?} = {x:?}"),
            Self::NativeFunction(_) => format!("native function"),
            Self::List { data, data_type: _ } => print_list(data, '[', ']'),
            Self::Option(x, _) => format!("{:?}", x),
            Self::Result(x, _) => format!("{:?}", x),
            Self::Str(x) => x.to_string(),
            Self::Char(x) => x.to_string(),
            Self::Type(x) => format!("Type {:?}", x),
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

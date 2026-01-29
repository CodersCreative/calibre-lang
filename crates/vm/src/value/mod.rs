use std::{collections::HashMap, f64::consts::PI, fmt::Display, rc::Rc};

use calibre_lir::BlockId;
use calibre_mir_ty::MiddleNode;
use calibre_parser::ast::{ObjectMap, ParserDataType};

use crate::{
    conversion::VMLiteral,
    native::{self, NativeFunction},
};

pub mod operation;

#[derive(Debug, Clone, Default)]
pub enum RuntimeValue {
    #[default]
    Null,
    Float(f64),
    Int(i64),
    Range(i64, i64),
    Bool(bool),
    Str(String),
    Char(char),
    Aggregate(Option<String>, ObjectMap<RuntimeValue>),
    Enum(String, usize, Option<Box<RuntimeValue>>),
    Ref(String),
    List(Vec<RuntimeValue>),
    Option(Option<Box<RuntimeValue>>),
    Result(Result<Box<RuntimeValue>, Box<RuntimeValue>>),
    NativeFunction(Rc<dyn NativeFunction>),
    Function {
        name: String,
        captures: Vec<String>,
    },
}

impl RuntimeValue {
    pub fn constants() -> HashMap<String, Self> {
        HashMap::from([
            (String::from("PI"), RuntimeValue::Float(PI)),
            (String::from("FLOAT_MAX"), RuntimeValue::Float(f64::MAX)),
            (String::from("INT_MAX"), RuntimeValue::Int(i64::MAX)),
            (String::from("FLOAT_MIN"), RuntimeValue::Float(f64::MIN)),
            (String::from("INT_MIN"), RuntimeValue::Int(i64::MIN)),
            (String::from("true"), RuntimeValue::Bool(true)),
            (String::from("false"), RuntimeValue::Bool(false)),
            (String::from("none"), RuntimeValue::Option(None)),
        ])
    }

    pub fn natives() -> HashMap<String, Self> {
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

impl From<VMLiteral> for RuntimeValue {
    fn from(value: VMLiteral) -> Self {
        match value {
            VMLiteral::Int(x) => Self::Int(x),
            VMLiteral::Float(x) => Self::Float(x),
            VMLiteral::Char(x) => Self::Char(x),
            VMLiteral::String(x) => Self::Str(x),
            VMLiteral::Null => Self::Null,
            VMLiteral::Closure { label, captures } => Self::Function {
                name: label,
                captures,
            },
        }
    }
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
            Self::Ref(ty) => format!("link -> {:?}", ty),
            Self::Bool(x) => x.to_string(),
            Self::Aggregate(x, y) => format!("{y:?} = {x:?}"),
            Self::NativeFunction(_) => format!("native function"),
            Self::List(data) => print_list(data, '[', ']'),
            Self::Option(x) => format!("{:?}", x),
            Self::Result(x) => format!("{:?}", x),
            Self::Str(x) => x.to_string(),
            Self::Char(x) => x.to_string(),
            Self::Function { name, captures } => {
                format!("fn {}(..)", name)
            }
        }
    }
}
pub enum TerminateValue {
    None,
    Jump(BlockId),
    Return(RuntimeValue),
}

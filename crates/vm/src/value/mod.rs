use std::fmt::Display;

use calibre_lir::BlockId;
use calibre_mir_ty::MiddleNode;
use calibre_parser::ast::{ObjectMap, ParserDataType};

use crate::conversion::VMLiteral;

pub mod operation;

#[derive(Debug, Clone, Default)]
pub enum RuntimeValue {
    #[default]
    Null,
    Float(f64),
    Type(ParserDataType<MiddleNode>),
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
    Function {
        name: String,
        captures: Vec<String>,
    },
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

impl Display for RuntimeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

pub enum TerminateValue {
    None,
    Jump(BlockId),
    Return(RuntimeValue),
}

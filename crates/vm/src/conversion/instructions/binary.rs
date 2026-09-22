use crate::conversion::Reg;
use calibre_parser::ast::{
    binary::BinaryOperator,
    comparison::{BooleanOperator, ComparisonOperator},
    nodes::binary::AsFailureMode,
    types::ParserDataType,
};
use serde::{Deserialize, Serialize};
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct VMAs {
    pub dst: Reg,
    pub src: Reg,
    pub data_type: ParserDataType,
    pub failure_mode: AsFailureMode,
}

impl Display for VMAs {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "%r{} = %r{} AS{} {}",
            self.dst,
            self.src,
            match self.failure_mode {
                AsFailureMode::Panic => "!",
                AsFailureMode::Option => "?",
                AsFailureMode::Result => "",
            },
            self.data_type
        )
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct VMIs {
    pub dst: Reg,
    pub src: Reg,
    pub data_type: ParserDataType,
}

impl Display for VMIs {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "%r{} = %r{} IS {}", self.dst, self.src, self.data_type)
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct VMBinary {
    pub dst: Reg,
    pub op: BinaryOperator,
    pub left: Reg,
    pub right: Reg,
}

impl Display for VMBinary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "%r{} = BINARY %r{} {} %r{}",
            self.dst, self.left, self.op, self.right
        )
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct VMComparison {
    pub dst: Reg,
    pub op: ComparisonOperator,
    pub left: Reg,
    pub right: Reg,
}

impl Display for VMComparison {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "%r{} = COMPARE %r{} {} %r{}",
            self.dst, self.left, self.op, self.right
        )
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct VMBoolean {
    pub dst: Reg,
    pub op: BooleanOperator,
    pub left: Reg,
    pub right: Reg,
}

impl Display for VMBoolean {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "%r{} = BOOLEAN %r{} {} %r{}",
            self.dst, self.left, self.op, self.right
        )
    }
}

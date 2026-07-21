use crate::ast::MiddleNode;
use crate::errors::MiddleErr;
use calibre_parser::{
    Location,
    ast::{
        Node, ParserDataType, VarType,
        binary::BinaryOperator,
        comparison::{BooleanOperator, ComparisonOperator},
    },
};
use rustc_hash::FxHashMap;
use std::{fmt::Debug, str::FromStr};

pub mod node;
pub mod resolve;

#[derive(Debug, Clone, Default)]
pub struct Symbols {
    pub variables: FxHashMap<String, MiddleVariable>,
    pub resolved_variables: Vec<String>,
    pub overloads: Vec<MiddleOverload>,
    pub generic_fn_templates:
        FxHashMap<String, (Vec<String>, calibre_parser::ast::FunctionHeader, Node)>,
    pub function_param_defaults: FxHashMap<String, Vec<FunctionParamDefault>>,
    pub fn_specializations: FxHashMap<String, String>,
    pub specialization_decls_by_scope: FxHashMap<u64, Vec<MiddleNode>>,
    pub func_defers: Vec<Node>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionParamDefault {
    pub name: String,
    pub explicit_default: Option<MiddleNode>,
    pub implicit_none: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MiddleVariable {
    pub data_type: ParserDataType,
    pub var_type: VarType,
    pub location: Option<Location>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct MiddleOverload {
    pub operator: Operator,
    pub parameters: Vec<ParserDataType>,
    pub return_type: ParserDataType,
    pub func: Node,
    pub generic_params: Vec<String>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Operator {
    Binary(BinaryOperator),
    Comparison(ComparisonOperator),
    Boolean(BooleanOperator),
    Index,
    IndexAssign,
    In,
    As,
}

impl FromStr for Operator {
    type Err = MiddleErr;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s == "[]" {
            Ok(Self::Index)
        } else if s == "[]=" {
            Ok(Self::IndexAssign)
        } else if s == "in" {
            Ok(Self::In)
        } else if s == "as" {
            Ok(Self::As)
        } else if let Some(x) = BinaryOperator::from_symbol(s) {
            Ok(Self::Binary(x))
        } else if let Some(x) = ComparisonOperator::from_operator(s) {
            Ok(Self::Comparison(x))
        } else if let Some(x) = BooleanOperator::from_operator(s) {
            Ok(Self::Boolean(x))
        } else {
            Err(MiddleErr::Scope(format!("unknown operator {s}")))
        }
    }
}

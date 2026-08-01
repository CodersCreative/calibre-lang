use crate::ast::MiddleNode;
use calibre_parser::{
    Location,
    ast::{
        Operator,
        nodes::{FunctionHeader, Node, VarType},
        types::ParserDataType,
    },
};
use rustc_hash::FxHashMap;
use std::fmt::Debug;

pub mod node;
pub mod overloads;
pub mod resolve;

#[derive(Debug, Clone, Default)]
pub struct Symbols {
    pub variables: FxHashMap<String, MiddleVariable>,
    pub resolved_variables: Vec<String>,
    pub overloads: Vec<MiddleOverload>,
    pub generic_fn_templates: FxHashMap<String, (Vec<String>, FunctionHeader, Node)>,
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

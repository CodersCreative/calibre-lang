use crate::{ast::MiddleNode, scoping::ScopeId};
use calibre_parser::{
    Location,
    ast::{
        Operator,
        nodes::{AstNode, FunctionHeader, VarType},
        types::ParserDataType,
    },
};
use rustc_hash::FxHashMap;
use std::fmt::Debug;
use ustr::{Ustr, UstrMap};

pub mod node;
pub mod overloads;
pub mod resolve;

#[derive(Debug, Clone, Default)]
pub struct Symbols {
    pub variables: UstrMap<MiddleVariable>,
    pub native_mappings: UstrMap<Ustr>,
    pub overloads: Vec<MiddleOverload>,
    pub generic_fn_templates: UstrMap<(Vec<Ustr>, FunctionHeader, AstNode)>,
    pub function_param_defaults: UstrMap<Vec<FunctionParamDefault>>,
    pub fn_specializations: UstrMap<Ustr>,
    pub specialization_decls_by_scope: FxHashMap<ScopeId, Vec<MiddleNode>>,
    pub func_defers: Vec<AstNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionParamDefault {
    pub name: Ustr,
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
    pub func: AstNode,
    pub generic_params: Vec<Ustr>,
}

use crate::{
    ast::MiddleNode, environment::MiddleEnvironment, scoping::ScopeId, translate::MirLowering,
};
use calibre_parser::{
    Location,
    ast::{
        Operator,
        nodes::{AstNode, VarType, functions::FunctionHeader},
        types::{ParserDataType, ParserInnerType},
    },
};
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};
use std::{fmt::Debug, rc::Rc};
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
    pub specialization_decls_by_scope: FxHashMap<ScopeId, Vec<MiddleNode>>,

    pub name_to_param_defaults: UstrMap<usize>,
    pub function_param_defaults: FxHashMap<usize, Rc<[FunctionParamDefault]>>,
    pub function_specializations: UstrMap<Ustr>,
    pub function_defers: Vec<AstNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionParamDefault {
    pub name: Ustr,
    pub explicit_default: Option<MiddleNode>,
    pub implicit_none: bool,
}

impl FunctionParamDefault {
    pub fn get(env: &mut MiddleEnvironment, scope: ScopeId, header: &FunctionHeader) -> Rc<[Self]> {
        header
            .parameters
            .iter()
            .map(|(name, declared_ty, default)| FunctionParamDefault {
                name: Ustr::from(&name.to_string()),
                explicit_default: default
                    .clone()
                    .map(|node| {
                        let span = node.span;
                        Box::new(node.lower_or_empty(env, scope, span))
                    })
                    .map(|x| *x),
                implicit_none: default.is_none()
                    && matches!(
                        declared_ty,
                        Some(ParserDataType {
                            data_type: ParserInnerType::Option(_),
                            ..
                        })
                    ),
            })
            .collect()
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MiddleVariable {
    pub data_type: ParserDataType,
    pub var_type: VarType,
    pub location: Option<Location>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct MiddleOverload {
    pub operator: Operator,
    pub parameters: Vec<ParserDataType>,
    pub return_type: ParserDataType,
    pub func: AstNode,
    pub generic_params: Vec<Ustr>,
}

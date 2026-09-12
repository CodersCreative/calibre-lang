use crate::{environment::MiddleEnvironment, errors::MiddleErr, scoping::ScopeId};
use calibre_parser::ast::{
    nodes::{AstNode, VarType, matching::MatchArmType},
    types::ParserDataType,
};
use ustr::Ustr;

#[derive(Debug, Clone)]
pub struct PatternTranslation {
    pub condition: AstNode,
    pub bindings: Vec<BindingDeclaration>,
}

#[derive(Debug, Clone)]
pub struct BindingDeclaration {
    pub name: Ustr,
    pub value: AstNode,
    pub var_type: VarType,
    pub data_type: Option<ParserDataType>,
}

pub trait PatternTranslator {
    fn translate(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        pattern: &MatchArmType,
        value: &AstNode,
    ) -> Result<PatternTranslation, MiddleErr>;
}

pub trait BindingExtractor {
    fn extract(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        pattern: &MatchArmType,
        value: &AstNode,
    ) -> Vec<BindingDeclaration>;
}

pub mod enum_pattern;
pub mod list_pattern;
pub mod string_pattern;
pub mod struct_pattern;
pub mod tuple_pattern;
pub mod value_pattern;

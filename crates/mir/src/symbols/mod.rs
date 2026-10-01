use crate::{
    MirRenamable, MirRenameState,
    ast::{MiddleNode, types::MirDataType},
    environment::MiddleEnvironment,
    scoping::{FullyQualifiedPath, ScopeId},
    symbols::resolve::Key,
    translate::MirLowering,
};
use calibre_parser::{
    Location,
    ast::{
        Operator,
        idents::{ParserText, PotentialDollarIdentifier, PotentialGenericTypeIdentifier},
        nodes::{AstNode, VarType, functions::FunctionHeader},
        types::{ParserDataType, ParserInnerType},
    },
};
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};
use std::{fmt::Debug, fmt::Display, rc::Rc, sync::Arc};
use ustr::{Ustr, UstrMap};

pub mod node;
pub mod overloads;
pub mod resolve;

#[derive(Debug, Clone, Default)]
pub struct Symbols {
    pub variables: FxHashMap<VariableKey, MiddleVariable>,
    pub native_mappings: UstrMap<Key>,
    pub overloads: Vec<MiddleOverload>,
    pub generic_fn_templates: FxHashMap<VariableKey, (Vec<Ustr>, FunctionHeader, AstNode)>,
    pub specialization_decls_by_scope: FxHashMap<ScopeId, Vec<MiddleNode>>,

    pub name_to_param_defaults: FxHashMap<VariableKey, usize>,
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

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct VariableKey {
    pub fully_qualified_path: Arc<FullyQualifiedPath>,
    pub shadow_counter: Option<u32>,
}

impl VariableKey {
    pub fn name(&self) -> &Ustr {
        self.fully_qualified_path.name.as_ref().unwrap()
    }
}

impl Display for VariableKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.fully_qualified_path)
    }
}

impl From<VariableKey> for PotentialDollarIdentifier {
    fn from(value: VariableKey) -> Self {
        Self::Identifier(ParserText::from(value.name().to_string()))
    }
}

impl From<VariableKey> for PotentialGenericTypeIdentifier {
    fn from(value: VariableKey) -> Self {
        Self::Identifier(PotentialDollarIdentifier::from(value))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct TypeKey {
    pub fully_qualified_path: Arc<FullyQualifiedPath>,
}

impl TypeKey {
    pub fn name(&self) -> &Ustr {
        self.fully_qualified_path.name.as_ref().unwrap()
    }
}

impl Display for TypeKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.fully_qualified_path)
    }
}

impl From<TypeKey> for PotentialDollarIdentifier {
    fn from(value: TypeKey) -> Self {
        Self::Identifier(ParserText::from(value.name().to_string()))
    }
}

impl From<TypeKey> for PotentialGenericTypeIdentifier {
    fn from(value: TypeKey) -> Self {
        Self::Identifier(PotentialDollarIdentifier::from(value))
    }
}

impl MirRenamable for VariableKey {
    fn rename(&mut self, state: &mut MirRenameState) {
        *self = state.mapped_variable_or_original(self.clone());
    }
}

impl MirRenamable for TypeKey {
    fn rename(&mut self, state: &mut MirRenameState) {
        *self = state.mapped_type_or_original(self.clone());
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MiddleVariable {
    pub data_type: MirDataType,
    pub var_type: VarType,
    pub location: Option<Location>,
    pub key: VariableKey,
}

impl MiddleVariable {
    pub fn fully_qualified_name(&self) -> String {
        format!("{}", self.key.fully_qualified_path)
    }

    pub fn matches_fqp_prefix(&self, prefix: &FullyQualifiedPath) -> bool {
        self.key.fully_qualified_path.as_ref() == prefix
            || self.key.fully_qualified_path.is_child_of(prefix)
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct MiddleOverload {
    pub operator: Operator,
    pub parameters: Vec<MirDataType>,
    pub return_type: MirDataType,
    pub func: AstNode,
    pub generic_params: Vec<Ustr>,
}

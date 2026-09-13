use std::str::FromStr;

use crate::{
    IdentifiersUsed, Span,
    ast::{
        ObjectType, Operator,
        idents::{ParserText, PotentialDollarIdentifier, PotentialGenericTypeIdentifier},
        nodes::{
            AstNode, AstNodeType,
            functions::{AstFunction, FunctionHeader},
        },
        types::{GenericTypes, ParserDataType},
    },
};
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TypeDefType {
    Enum {
        variants: Vec<(PotentialDollarIdentifier, Option<ParserDataType>)>,
        default_variant: Option<usize>,
        default_value: Option<Box<AstNode>>,
    },
    Struct {
        fields: ObjectType<(ParserDataType, Option<AstNode>)>,
    },
    NewType(Box<ParserDataType>),
}

impl TypeDefType {
    pub fn substitute(&self, subst: &FxHashMap<String, ParserDataType>) -> TypeDefType {
        match self {
            TypeDefType::Struct { fields } => TypeDefType::Struct {
                fields: match fields {
                    ObjectType::Map(xs) => ObjectType::Map(
                        xs.iter()
                            .map(|(k, (v, _default))| (*k, (v.substitute(subst), None)))
                            .collect(),
                    ),
                    ObjectType::Tuple(xs) => ObjectType::Tuple(
                        xs.iter()
                            .map(|(v, _default)| (v.substitute(subst), None))
                            .collect(),
                    ),
                },
            },
            TypeDefType::Enum {
                variants,
                default_variant,
                default_value,
            } => TypeDefType::Enum {
                variants: variants
                    .iter()
                    .map(|(k, v)| (k.clone(), v.as_ref().map(|p| p.substitute(subst))))
                    .collect(),
                default_variant: *default_variant,
                default_value: default_value.clone(),
            },
            TypeDefType::NewType(inner) => TypeDefType::NewType(Box::new(inner.substitute(subst))),
        }
    }
}

impl IdentifiersUsed for TypeDefType {
    fn identifiers_used(&self) -> Vec<&String> {
        let mut names = Vec::new();
        match self {
            TypeDefType::Enum { variants, .. } => {
                for (_, potential_type) in variants {
                    if let Some(potential) = potential_type {
                        names.extend(potential.identifiers_used());
                    }
                }
            }
            TypeDefType::Struct { fields } => {
                if let ObjectType::Map(field_map) = fields {
                    for (_, (potential_type, default_value)) in field_map {
                        names.extend(potential_type.identifiers_used());
                        if let Some(default) = default_value {
                            names.extend(default.identifiers_used());
                        }
                    }
                }
            }
            TypeDefType::NewType(inner) => {
                names.extend(inner.identifiers_used());
            }
        }
        names
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Overload {
    pub operator: ParserText,
    pub body: Box<AstNode>,
    pub header: FunctionHeader,
}

impl From<Overload> for AstNode {
    fn from(val: Overload) -> AstNode {
        AstNode::new(
            val.operator.span,
            AstNodeType::FunctionDeclaration(AstFunction {
                header: val.header,
                body: val.body,
            }),
        )
    }
}

impl Overload {
    pub fn span(&self) -> &Span {
        &self.operator.span
    }

    pub fn verify(&self) -> Result<(), String> {
        let operator = Operator::from_str(&self.operator.text)?;
        match operator {
            Operator::As if !self.header.return_type.is_result() => Err(format!(
                "Expect result return type (Err!Ok) found {}",
                self.header.return_type
            )),
            Operator::In if !self.header.return_type.is_bool() => Err(format!(
                "Expect bool return type found {}",
                self.header.return_type
            )),
            Operator::Binary(_) | Operator::Comparison(_) | Operator::Binary(_)
                if self.header.return_type.is_null() || self.header.return_type.is_auto() =>
            {
                Err(format!(
                    "Expect known non-null return type found {}",
                    self.header.return_type
                ))
            }
            Operator::Index if self.header.parameters.len() != 2 => Err(format!(
                "Expect 2 parameters found {}",
                self.header.parameters.len()
            )),
            Operator::IndexAssign if self.header.parameters.len() != 3 => Err(format!(
                "Expect 3 parameters found {}",
                self.header.parameters.len()
            )),
            _ => Ok(()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TraitMemberKind {
    Const,
    Type,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TraitMember {
    pub kind: TraitMemberKind,
    pub identifier: PotentialDollarIdentifier,
    pub data_type: ParserDataType,
    pub value: Option<Box<AstNode>>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstImpl {
    pub generics: GenericTypes,
    pub target: ParserDataType,
    pub variables: Vec<AstNode>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstImplTrait {
    pub generics: GenericTypes,
    pub trait_ident: PotentialGenericTypeIdentifier,
    pub target: ParserDataType,
    pub variables: Vec<AstNode>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstTrait {
    pub identifier: PotentialGenericTypeIdentifier,
    pub implied_traits: Vec<PotentialDollarIdentifier>,
    pub members: Vec<TraitMember>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstType {
    pub identifier: PotentialGenericTypeIdentifier,
    pub object: TypeDefType,
    pub overloads: Vec<Overload>,
}

use crate::{
    IdentifiersUsed, Span,
    ast::{
        ObjectType, Operator,
        idents::{ParserText, PotentialDollarIdentifier, PotentialGenericTypeIdentifier},
        nodes::{
            AstNode, AstNodeType,
            functions::{AstFunction, FunctionHeader},
            misc::StandaloneTag,
        },
        types::{GenericTypes, ParserDataType},
    },
};
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};
use std::str::FromStr;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TypeDefType {
    Enum {
        variants: Vec<(
            Vec<StandaloneTag>,
            PotentialDollarIdentifier,
            Option<ParserDataType>,
        )>,
        default_variant: Option<usize>,
        default_value: Option<Box<AstNode>>,
    },
    Struct {
        fields: ObjectType<(Vec<StandaloneTag>, ParserDataType, Option<AstNode>)>,
    },
    NewType(Box<ParserDataType>),
}

impl TypeDefType {
    pub fn substitute(self, subst: &FxHashMap<String, ParserDataType>) -> TypeDefType {
        match self {
            TypeDefType::Struct { fields } => TypeDefType::Struct {
                fields: match fields {
                    ObjectType::Map(xs) => ObjectType::Map(
                        xs.into_iter()
                            .map(|(k, (tags, v, _))| (k, (tags, v.substitute(subst), None)))
                            .collect(),
                    ),
                    ObjectType::Tuple(xs) => ObjectType::Tuple(
                        xs.into_iter()
                            .map(|(tags, v, _)| (tags, v.substitute(subst), None))
                            .collect(),
                    ),
                },
            },
            TypeDefType::Enum {
                variants,
                default_value,
                default_variant,
            } => TypeDefType::Enum {
                variants: variants
                    .into_iter()
                    .map(|(tags, k, v)| (tags, k, v.map(|p| p.substitute(subst))))
                    .collect(),
                default_value,
                default_variant,
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
                for (_, _, potential_type) in variants {
                    if let Some(potential) = potential_type {
                        names.extend(potential.identifiers_used());
                    }
                }
            }
            TypeDefType::Struct { fields } => {
                if let ObjectType::Map(field_map) = fields {
                    for (_, (_, potential_type, default_value)) in field_map {
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

    pub fn verify(&self) -> Result<Operator, String> {
        let operator = Operator::from_str(&self.operator.text)?;
        match &operator {
            Operator::As => {
                if self.header.parameters.len() != 1 {
                    return Err(format!(
                        "An `as` overload requires that there only be 1 parameter but {} was supplied.",
                        self.header.parameters.len(),
                    ));
                }

                if !self.header.return_type.is_result() {
                    return Err(format!(
                        "An `as` overload requires that the return type be an result (`Err!Ok`) not {}",
                        self.header.return_type,
                    ));
                }
            }
            Operator::IndexAssign => {
                if self.header.parameters.len() != 3 {
                    return Err(format!(
                        "A `[]=` overload requires that there be 3 parameters but {} were supplied.",
                        self.header.parameters.len(),
                    ));
                }
            }
            Operator::Index => {
                if self.header.parameters.len() != 2 {
                    return Err(format!(
                        "A `[]` overload requires that there be 2 parameters but {} were supplied.",
                        self.header.parameters.len(),
                    ));
                }

                if !self.header.return_type.is_option() {
                    return Err(format!(
                        "A `[]` overload requires that the return type be an option (`T?`) not {}",
                        self.header.return_type,
                    ));
                }
            }
            Operator::In => {
                if self.header.parameters.len() != 2 {
                    return Err(format!(
                        "An `in` overload requires that there be 2 parameters but {} were supplied.",
                        self.header.parameters.len(),
                    ));
                }

                if !self.header.return_type.is_bool() && !self.header.return_type.is_option() {
                    return Err(format!(
                        "An `in` overload requires that the return type either be a `bool` or option (`T?`) not {}",
                        self.header.return_type,
                    ));
                }
            }
            Operator::Binary(x) => {
                if self.header.parameters.len() != 2 {
                    return Err(format!(
                        "A `{}` overload requires that there be 2 parameters but {} were supplied.",
                        x,
                        self.header.parameters.len(),
                    ));
                }

                if self.header.return_type.is_null() {
                    return Err(format!(
                        "A `{}` overload requires that a return type be present",
                        x,
                    ));
                }
            }
            Operator::Comparison(x) => {
                if self.header.parameters.len() != 2 {
                    return Err(format!(
                        "A `{}` overload requires that there be 2 parameters but {} were supplied.",
                        x,
                        self.header.parameters.len(),
                    ));
                }

                if self.header.return_type.is_null() {
                    return Err(format!(
                        "A `{}` overload requires that a return type be present",
                        x,
                    ));
                }
            }
            Operator::Boolean(x) => {
                if self.header.parameters.len() != 2 {
                    return Err(format!(
                        "A `{}` overload requires that there be 2 parameters but {} were supplied.",
                        x,
                        self.header.parameters.len(),
                    ));
                }

                if self.header.return_type.is_null() {
                    return Err(format!(
                        "A `{}` overload requires that a return type be present",
                        x,
                    ));
                }
            }
        }

        Ok(operator)
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstImpl {
    pub generics: GenericTypes,
    pub target: ParserDataType,
    pub variables: Vec<AstNode>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstType {
    pub identifier: PotentialGenericTypeIdentifier,
    pub object: TypeDefType,
}

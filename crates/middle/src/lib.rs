use std::{collections::HashMap, path::PathBuf};

use calibre_parser::{
    ast::{Node, NodeType, ObjectType, ParserDataType, ParserInnerType, ParserText, VarType},
    lexer::{Location, Span},
};
use miette::Diagnostic;
use thiserror::Error;

use crate::ast::{MiddleNode, MiddleNodeType};
pub mod ast;

#[derive(Debug, Clone, PartialEq)]
pub struct MiddleObject {
    pub object_type: ObjectType<MiddleNode>,
    pub functions: HashMap<String, (MiddleNode, Option<Location>, bool)>,
    pub traits: Vec<String>,
    pub location: Option<Location>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MiddleVariable {
    pub value: MiddleNode,
    pub var_type: VarType,
    pub location: Option<Location>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MiddleEnvironment {
    pub scope_counter: u64,
    pub scopes: HashMap<u64, MiddleScope>,
    pub variables: HashMap<String, MiddleVariable>,
    pub resolved_variables: Vec<String>,
    pub objects: HashMap<String, MiddleObject>,
    pub current_location: Option<Location>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MiddleScope {
    pub id: u64,
    pub parent: Option<u64>,
    pub mappings: HashMap<String, String>,
    pub children: HashMap<String, u64>,
    pub namespace: String,
    pub path: PathBuf,
}

impl MiddleEnvironment {
    pub fn new() -> MiddleEnvironment {
        MiddleEnvironment {
            scope_counter: 0,
            scopes: HashMap::new(),
            resolved_variables: Vec::new(),
            variables: HashMap::new(),
            objects: HashMap::new(),
            current_location: None,
        }
    }

    pub fn get_location(&self, scope: &u64, span: Span) -> Option<Location> {
        Some(Location {
            path: self.scopes.get(scope).unwrap().path.clone(),
            span,
        })
    }

    pub fn resolve_str(&self, scope: &u64, iden: &str) -> Option<&String> {
        let scope = self.scopes.get(scope)?;
        scope.mappings.get(iden)
    }

    pub fn resolve_parser_text(&self, scope: &u64, iden: &ParserText) -> Option<ParserText> {
        let scope = self.scopes.get(scope)?;
        Some(ParserText {
            text: scope.mappings.get(&iden.text)?.to_string(),
            span: iden.span,
        })
    }

    pub fn resolve_type_from_node(&self, scope: &u64, node: &Node) -> Option<ParserDataType> {
        match &node.node_type {
            NodeType::Break
            | NodeType::Continue
            | NodeType::EmptyLine
            | NodeType::VariableDeclaration { .. }
            | NodeType::ImplDeclaration { .. }
            | NodeType::TypeDeclaration { .. }
            | NodeType::Return { .. }
            | NodeType::ImportStatement { .. }
            | NodeType::AssignmentExpression { .. }
            | NodeType::StructLiteral(_)
            | NodeType::LoopDeclaration { .. } => None,
            NodeType::RefStatement { mutability, value } => Some(ParserDataType {
                data_type: ParserInnerType::Ref(
                    Box::new(self.resolve_type_from_node(scope, value)?),
                    mutability.clone(),
                ),
                span: node.span,
            }),
            NodeType::ParenExpression { value } => self.resolve_type_from_node(scope, value),
            NodeType::ScopeDeclaration { body, is_temp: _ } => {
                self.resolve_type_from_node(scope, body.last()?)
            }
            NodeType::IfStatement {
                comparison: _,
                then,
                otherwise,
                special_delim: _,
            } => {
                if let Some(otherwise) = otherwise {
                    let otherwise =
                        if let NodeType::IfStatement { then: then2, .. } = &otherwise.node_type {
                            then2.clone()
                        } else {
                            otherwise.clone()
                        };

                    let ty = self.resolve_type_from_node(scope, then);
                    if ty == self.resolve_type_from_node(scope, &otherwise) {
                        ty
                    } else {
                        None
                    }
                } else {
                    self.resolve_type_from_node(scope, then)
                }
            }
            NodeType::EnumExpression {
                identifier,
                value: _,
                data: _,
            } => Some(ParserDataType {
                data_type: ParserInnerType::Struct(Some(identifier.text.clone())),
                span: identifier.span,
            }),
            NodeType::FunctionDeclaration {
                parameters,
                body,
                return_type,
                is_async,
            } => Some(ParserDataType {
                data_type: ParserInnerType::Function {
                    return_type: if let Some(x) = return_type {
                        Some(Box::new(x.clone()))
                    } else {
                        self.resolve_type_from_node(scope, body)
                            .map(|x| Box::new(x.clone()))
                    },
                    parameters: parameters.iter().map(|x| x.1.clone()).collect(),
                    is_async: *is_async,
                },
                span: node.span,
            }),
            NodeType::MatchDeclaration {
                parameters,
                body: _,
                return_type,
                is_async,
            } => Some(ParserDataType {
                data_type: ParserInnerType::Function {
                    return_type: if let Some(x) = return_type {
                        Some(Box::new(x.clone()))
                    } else {
                        // TODO get type
                        None
                    },
                    parameters: vec![parameters.1.clone()],
                    is_async: *is_async,
                },
                span: node.span,
            }),
            NodeType::BooleanExpression { .. }
            | NodeType::ComparisonExpression { .. }
            | NodeType::NotExpression { .. }
            | NodeType::InDeclaration { .. }
            | NodeType::IsDeclaration { .. } => Some(ParserDataType {
                data_type: ParserInnerType::Bool,
                span: node.span,
            }),
            NodeType::BinaryExpression { left, .. } => self.resolve_type_from_node(scope, left),
            NodeType::ListLiteral(x) => Some(ParserDataType {
                data_type: ParserInnerType::List(
                    self.resolve_type_from_node(scope, x.first()?).map(Box::new),
                ),
                span: node.span,
            }),
            NodeType::IterExpression {
                map,
                loop_type: _,
                conditionals: _,
            } => Some(ParserDataType {
                data_type: ParserInnerType::List(
                    self.resolve_type_from_node(scope, map).map(Box::new),
                ),
                span: node.span,
            }),
            NodeType::NegExpression { value } | NodeType::DebugExpression { value } => {
                self.resolve_type_from_node(scope, value)
            }
            NodeType::AsExpression { value: _, typ } => Some(typ.clone()),
            NodeType::RangeDeclaration { .. } => Some(ParserDataType {
                data_type: ParserInnerType::Range,
                span: node.span,
            }),
            NodeType::IntLiteral(_) => Some(ParserDataType {
                data_type: ParserInnerType::Int,
                span: node.span,
            }),
            NodeType::CharLiteral(_) => Some(ParserDataType {
                data_type: ParserInnerType::Char,
                span: node.span,
            }),
            NodeType::StringLiteral(_) => Some(ParserDataType {
                data_type: ParserInnerType::Str,
                span: node.span,
            }),
            NodeType::FloatLiteral(_) => Some(ParserDataType {
                data_type: ParserInnerType::Float,
                span: node.span,
            }),
            NodeType::Try { value } => match self.resolve_type_from_node(scope, value) {
                Some(ParserDataType {
                    data_type: ParserInnerType::Result(x, _),
                    ..
                })
                | Some(ParserDataType {
                    data_type: ParserInnerType::Option(x),
                    ..
                }) => Some(*x),
                x => x,
            },
            _ => todo!(),
        }
    }

    pub fn get_resolved_node(&mut self, scope: &u64, node: Node) -> Result<MiddleNode, MiddleErr> {
        match node.node_type {
            NodeType::Identifier(x) => Ok(MiddleNode {
                node_type: MiddleNodeType::Identifier(self.resolve_parser_text(scope, &x).unwrap()),
                span: node.span,
            }),
            NodeType::MemberExpression { mut path } => {
                let first = path.remove(0);
                let mut list = vec![(self.get_resolved_node(scope, first.0)?, first.1)];

                for item in path {
                    list.push((self.evaluate(scope, item.0)?, item.1));
                }

                Ok(MiddleNode {
                    node_type: MiddleNodeType::MemberExpression { path: list },
                    span: node.span,
                })
            }
            x => Err(MiddleErr::RefNonVar(x)),
        }
    }

    pub fn evaluate(&mut self, scope: &u64, node: Node) -> Result<MiddleNode, MiddleErr> {
        self.current_location = self.get_location(scope, node.span);

        match node.node_type {
            NodeType::Break => Ok(MiddleNode {
                node_type: MiddleNodeType::Break,
                span: node.span,
            }),
            NodeType::Continue => Ok(MiddleNode {
                node_type: MiddleNodeType::Continue,
                span: node.span,
            }),
            NodeType::EmptyLine => Ok(MiddleNode {
                node_type: MiddleNodeType::EmptyLine,
                span: node.span,
            }),
            NodeType::RefStatement { mutability, value } => Ok(MiddleNode {
                node_type: MiddleNodeType::RefStatement {
                    mutability,
                    value: Box::new(self.get_resolved_node(scope, *value)?),
                },
                span: node.span,
            }),
            NodeType::DerefStatement { value } => Ok(MiddleNode {
                node_type: MiddleNodeType::DerefStatement {
                    value: Box::new(self.get_resolved_node(scope, *value)?),
                },
                span: node.span,
            }),
            NodeType::ParenExpression { value } => self.evaluate(scope, *value),
            NodeType::VariableDeclaration {
                var_type,
                identifier,
                value,
                data_type,
            } => {
                // Lazy
                todo!()
            }
            _ => todo!(),
        }
    }
}

#[derive(Error, Debug, Clone, Diagnostic)]
pub enum MiddleErr {
    #[error("{err}")]
    At {
        err: Box<Self>,
        #[source_code]
        input: String,
        #[label("here")]
        span: Option<(usize, usize)>,
    },
    #[error("Cannot assign a literal value, {0}.")]
    AssignNonVariable(NodeType),
    #[error("Cannot mutably reference a non mutable value, {0:?}.")]
    MutRefNonMut(MiddleNode),
    #[error("Cannot mutably reference a non variable, {0}.")]
    RefNonVar(NodeType),
    #[error("Cannot de-reference a non reference, {0}.")]
    DerefNonRef(NodeType),
    #[error("Cannot index a value that is not a list, {0}.")]
    IndexNonList(NodeType),
    #[error("This AST Node has not been implemented, {0}.")]
    NotImplemented(NodeType),
    #[error("Expected {0} operation.")]
    ExpectedOperation(String),
    #[error("Expected only functions.")]
    ExpectedFunctions,
    #[error("Index out of bounds for list, {0}.")]
    InvalidIndex(i64),
    #[error("Default value name not identifier.")]
    InvalidDefaultFuncArg,
    #[error("Node {0} has an unexpected type.")]
    UnexpectedNode(NodeType),
    #[error("Node {0} needs to be used in the global scope.")]
    UnexpectedNodeInTemp(NodeType),
    #[error("Node {0} can't be used in the global scope.")]
    UnexpectedNodeInGlobal(NodeType),
    #[error("No associated enum item : {1:?} in enum {0:?}")]
    UnexpectedEnumItem(String, String),
    #[error("Setters can only have one argument, {0:?}")]
    SetterArgs(Vec<(NodeType, Option<NodeType>)>),
    #[error("Property not found, {0:?}")]
    PropertyNotFound(String),
    #[error("Unable to import {0:?}")]
    CantImport(String),
}

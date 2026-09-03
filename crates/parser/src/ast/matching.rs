use crate::{
    Span,
    ast::{
        idents::{ParserText, PotentialDollarIdentifier},
        nodes::{AstNode, DestructurePattern, VarType},
        types::ParserDataType,
    },
};
use serde::{Deserialize, Serialize};

#[repr(u8)]
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum MatchTupleItem {
    Rest(Span),
    Wildcard(Span),
    Value(AstNode),
    IsType(ParserDataType),
    In(AstNode),
    At {
        var_type: VarType,
        name: PotentialDollarIdentifier,
        pattern: Box<MatchTupleItem>,
    },
    StringPattern(Vec<MatchStringPatternPart>),
    Enum {
        value: PotentialDollarIdentifier,
        var_type: VarType,
        name: Option<PotentialDollarIdentifier>,
        destructure: Option<DestructurePattern>,
        pattern: Option<Box<MatchArmType>>,
    },
    Binding {
        var_type: VarType,
        name: PotentialDollarIdentifier,
    },
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum MatchStructFieldPattern {
    Value {
        field: String,
        value: AstNode,
    },
    Binding {
        field: String,
        var_type: VarType,
        name: PotentialDollarIdentifier,
    },
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum MatchStringPatternPart {
    Literal(ParserText),
    Binding {
        var_type: VarType,
        name: PotentialDollarIdentifier,
    },
    Wildcard(Span),
}

#[repr(u8)]
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum MatchArmType {
    At {
        var_type: VarType,
        name: PotentialDollarIdentifier,
        pattern: Box<MatchArmType>,
    },
    In(AstNode),
    StringPattern(Vec<MatchStringPatternPart>),
    Enum {
        value: PotentialDollarIdentifier,
        var_type: VarType,
        name: Option<PotentialDollarIdentifier>,
        destructure: Option<DestructurePattern>,
        pattern: Option<Box<MatchArmType>>,
    },
    TuplePattern(Vec<MatchTupleItem>),
    ListPattern(Vec<MatchTupleItem>),
    StructPattern(Vec<MatchStructFieldPattern>),
    Let {
        var_type: VarType,
        name: PotentialDollarIdentifier,
    },
    Value(AstNode),
    IsType(ParserDataType),
    Wildcard(Span),
}

impl MatchArmType {
    fn first_span_from_string_parts(parts: &[MatchStringPatternPart]) -> Option<&Span> {
        let part = parts.first()?;
        match part {
            MatchStringPatternPart::Literal(text) => Some(&text.span),
            MatchStringPatternPart::Binding { name, .. } => Some(name.span()),
            MatchStringPatternPart::Wildcard(span) => Some(span),
        }
    }

    fn first_span_from_tuple_items(items: &[MatchTupleItem]) -> Option<&Span> {
        for item in items {
            match item {
                MatchTupleItem::Rest(sp) | MatchTupleItem::Wildcard(sp) => return Some(sp),
                MatchTupleItem::Value(node) => return Some(&node.span),
                MatchTupleItem::IsType(data_type) => return Some(&data_type.span),
                MatchTupleItem::In(node) => return Some(&node.span),
                MatchTupleItem::At { name, .. } => return Some(name.span()),
                MatchTupleItem::StringPattern(parts) => {
                    if let Some(span) = Self::first_span_from_string_parts(parts) {
                        return Some(span);
                    }
                }
                MatchTupleItem::Enum { value, .. } => return Some(value.span()),
                MatchTupleItem::Binding { name, .. } => return Some(name.span()),
            }
        }
        None
    }

    pub fn into_tuple_item(self) -> Option<MatchTupleItem> {
        match self {
            MatchArmType::At {
                var_type,
                name,
                pattern,
            } => Some(MatchTupleItem::At {
                var_type,
                name,
                pattern: Box::new(pattern.into_tuple_item()?),
            }),
            MatchArmType::Wildcard(sp) => Some(MatchTupleItem::Wildcard(sp)),
            MatchArmType::Let { var_type, name } => {
                Some(MatchTupleItem::Binding { var_type, name })
            }
            MatchArmType::Enum {
                value,
                var_type,
                name,
                destructure,
                pattern,
            } => Some(MatchTupleItem::Enum {
                value,
                var_type,
                name,
                destructure,
                pattern,
            }),
            MatchArmType::Value(node) => Some(MatchTupleItem::Value(node)),
            MatchArmType::IsType(data_type) => Some(MatchTupleItem::IsType(data_type)),
            MatchArmType::In(node) => Some(MatchTupleItem::In(node)),
            MatchArmType::StringPattern(parts) => Some(MatchTupleItem::StringPattern(parts)),
            MatchArmType::TuplePattern(mut inner) => {
                if inner.len() == 1 {
                    Some(inner.remove(0))
                } else {
                    None
                }
            }
            MatchArmType::ListPattern(_) => None,
            MatchArmType::StructPattern(_) => None,
        }
    }

    pub fn into_tuple_items(self) -> Option<Vec<MatchTupleItem>> {
        match self {
            MatchArmType::TuplePattern(inner) => Some(inner),
            other => Some(vec![other.into_tuple_item()?]),
        }
    }

    fn default_span() -> &'static Span {
        static DEFAULT: Span = Span {
            from: crate::Position { line: 0, col: 0 },
            to: crate::Position { line: 0, col: 0 },
        };

        &DEFAULT
    }

    pub fn span(&self) -> &Span {
        match self {
            Self::Enum { value, .. } => value.span(),
            Self::TuplePattern(items) | Self::ListPattern(items) => {
                if let Some(span) = Self::first_span_from_tuple_items(items) {
                    span
                } else {
                    Self::default_span()
                }
            }
            Self::At { name, .. } => name.span(),
            Self::In(x) => &x.span,
            Self::StringPattern(parts) => {
                if let Some(span) = Self::first_span_from_string_parts(parts) {
                    span
                } else {
                    Self::default_span()
                }
            }
            Self::StructPattern(fields) => {
                if let Some(field) = fields.first() {
                    match field {
                        MatchStructFieldPattern::Value { value, .. } => &value.span,
                        MatchStructFieldPattern::Binding { name, .. } => name.span(),
                    }
                } else {
                    Self::default_span()
                }
            }
            Self::Let { var_type: _, name } => name.span(),
            Self::Value(x) => &x.span,
            Self::IsType(x) => &x.span,
            Self::Wildcard(x) => x,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct TryCatch {
    pub name: Option<PotentialDollarIdentifier>,
    pub body: Box<AstNode>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum SelectArmKind {
    Recv,
    Send,
    Default,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SelectArm {
    pub patterns: Vec<(SelectArmKind, Option<AstNode>, Option<AstNode>)>,
    pub conditionals: Vec<AstNode>,
    pub body: AstNode,
}

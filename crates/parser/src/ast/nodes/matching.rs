use crate::{
    Span,
    ast::{
        Substitutable,
        idents::{ParserText, PotentialDollarIdentifier},
        nodes::{AstNode, DestructurePattern, VarType, functions::FunctionHeader},
        types::ParserDataType,
    },
};
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum MatchStructFieldPattern {
    Value {
        field: String,
        value: AstNode,
    },
    AlternativeValues {
        field: String,
        values: Vec<AstNode>,
    },
    Binding {
        field: String,
        var_type: VarType,
        name: PotentialDollarIdentifier,
    },
}

impl MatchStructFieldPattern {
    pub fn field_name(&self) -> Option<&String> {
        match self {
            MatchStructFieldPattern::Value { field, .. } => Some(field),
            MatchStructFieldPattern::AlternativeValues { field, .. } => Some(field),
            MatchStructFieldPattern::Binding { field, .. } => Some(field),
        }
    }
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
    TuplePattern(Vec<MatchArmType>),
    ListPattern(Vec<MatchArmType>),
    StructPattern(Vec<MatchStructFieldPattern>),
    Let {
        var_type: VarType,
        name: PotentialDollarIdentifier,
    },
    Value(AstNode),
    IsType(ParserDataType),
    Wildcard(Span),
    Rest(Span),
}

impl Substitutable for MatchArmType {
    fn substitute(self, subst: &FxHashMap<String, ParserDataType>) -> Self {
        match self {
            Self::At {
                var_type,
                name,
                pattern,
            } => Self::At {
                var_type,
                name,
                pattern: Box::new(pattern.substitute(subst)),
            },
            Self::In(x) => Self::In(x.substitute(subst)),
            Self::Enum {
                value,
                var_type,
                name,
                destructure,
                pattern,
            } => Self::Enum {
                value,
                var_type,
                name,
                destructure,
                pattern: pattern.map(|x| Box::new(x.substitute(subst))),
            },
            x => x,
        }
    }
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

    pub fn has_wildcard(&self) -> bool {
        match self {
            MatchArmType::Wildcard(_) => true,
            MatchArmType::At { pattern: inner, .. } => inner.has_wildcard(),
            MatchArmType::TuplePattern(items) => items.iter().any(|item| item.has_wildcard()),
            MatchArmType::ListPattern(items) => items.iter().any(|item| item.has_wildcard()),
            MatchArmType::Enum { pattern: inner, .. } => {
                inner.as_ref().map(|p| p.has_wildcard()).unwrap_or(false)
            }
            _ => false,
        }
    }

    pub fn is_wildcard(&self) -> bool {
        matches!(self, MatchArmType::Wildcard(_))
    }

    pub fn is_wildcard_or_rest(&self) -> bool {
        matches!(self, MatchArmType::Wildcard(_) | MatchArmType::Rest(_))
    }

    pub fn into_tuple_items(self) -> Option<Vec<MatchArmType>> {
        match self {
            MatchArmType::TuplePattern(inner) => Some(inner),
            other => Some(vec![other]),
        }
    }

    fn default_span() -> &'static Span {
        static DEFAULT: Span = Span { from: 0, to: 0 };

        &DEFAULT
    }

    pub fn span(&self) -> &Span {
        match self {
            Self::Enum { value, .. } => value.span(),
            Self::TuplePattern(items) | Self::ListPattern(items) => {
                if let Some(first) = items.first() {
                    first.span()
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
                        MatchStructFieldPattern::AlternativeValues { values, .. } => {
                            if let Some(v) = values.first() {
                                &v.span
                            } else {
                                Self::default_span()
                            }
                        }
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
            Self::Rest(x) => x,
        }
    }

    pub fn alias_bindings(self) -> (Self, Vec<(VarType, PotentialDollarIdentifier)>) {
        let mut aliases = Vec::new();
        let mut current = self;

        while let MatchArmType::At {
            var_type,
            name,
            pattern: inner,
        } = current
        {
            aliases.push((var_type, name));
            current = *inner;
        }

        (current, aliases)
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct MatchBody {
    pub values: Vec<(MatchArmType, Vec<AstNode>, Box<AstNode>)>,
}

impl MatchBody {
    pub fn substitute(mut self, subst: &FxHashMap<String, ParserDataType>) -> Self {
        self.values = self
            .values
            .into_iter()
            .map(|x| {
                (
                    x.0.substitute(subst),
                    x.1.into_iter().map(|x| x.substitute(subst)).collect(),
                    Box::new(x.2.substitute(subst)),
                )
            })
            .collect();
        self
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstFnMatch {
    pub header: FunctionHeader,
    pub body: MatchBody,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstMatch {
    pub value: Option<Box<AstNode>>,
    pub body: MatchBody,
}

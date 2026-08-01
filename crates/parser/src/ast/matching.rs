use crate::{
    Span,
    ast::{
        idents::{ParserText, PotentialDollarIdentifier},
        nodes::{DestructurePattern, Node, VarType},
        types::ParserDataType,
    },
};

#[repr(u8)]
#[derive(Clone, Debug, PartialEq)]
pub enum MatchTupleItem {
    Rest(Span),
    Wildcard(Span),
    Value(Node),
    IsType(ParserDataType),
    In(Node),
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

#[derive(Clone, Debug, PartialEq)]
pub enum MatchStructFieldPattern {
    Value {
        field: String,
        value: Node,
    },
    Binding {
        field: String,
        var_type: VarType,
        name: PotentialDollarIdentifier,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub enum MatchStringPatternPart {
    Literal(ParserText),
    Binding {
        var_type: VarType,
        name: PotentialDollarIdentifier,
    },
    Wildcard(Span),
}

#[repr(u8)]
#[derive(Clone, Debug, PartialEq)]
pub enum MatchArmType {
    At {
        var_type: VarType,
        name: PotentialDollarIdentifier,
        pattern: Box<MatchArmType>,
    },
    In(Node),
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
    Value(Node),
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

#[derive(Clone, Debug, PartialEq)]
pub struct TryCatch {
    pub name: Option<PotentialDollarIdentifier>,
    pub body: Box<Node>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum SelectArmKind {
    Recv,
    Send,
    Default,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SelectArm {
    pub patterns: Vec<(SelectArmKind, Option<Node>, Option<Node>)>,
    pub conditionals: Vec<Node>,
    pub body: Node,
}

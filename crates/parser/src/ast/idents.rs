use serde::{Deserialize, Serialize};

use crate::{
    COUNTER, ParserError, Span,
    ast::{
        nodes::{Node, NodeType},
        types::PotentialNewType,
    },
};
use std::{
    fmt::Display,
    ops::{Deref, DerefMut},
    str::FromStr,
};

#[derive(Debug, Clone, PartialEq)]
pub enum PotentialGenericTypeIdentifier {
    Identifier(PotentialDollarIdentifier),
    Generic {
        identifier: PotentialDollarIdentifier,
        generic_types: Vec<PotentialNewType>,
    },
}

impl Display for PotentialGenericTypeIdentifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Identifier(x) => write!(f, "{}", x),
            Self::Generic {
                identifier,
                generic_types,
            } => {
                if generic_types.is_empty() {
                    return write!(f, "{}", identifier);
                }
                let mut txt = format!(
                    "{}:<{}",
                    identifier,
                    generic_types
                        .first()
                        .map(|x| x.to_string())
                        .unwrap_or(String::new())
                );

                for typ in generic_types.iter().skip(1) {
                    txt.push_str(&format!(", {}", typ));
                }

                txt.push_str(">");

                write!(f, "{}", txt)
            }
        }
    }
}

impl PotentialGenericTypeIdentifier {
    pub fn new(span: Span, text: impl ToString) -> Self {
        return Self::Identifier(ParserText::new(span, text).into());
    }
}

impl PotentialGenericTypeIdentifier {
    pub fn get_ident(&self) -> &PotentialDollarIdentifier {
        match self {
            Self::Identifier(x) => x,
            Self::Generic {
                identifier,
                generic_types: _,
            } => identifier,
        }
    }
}

impl Into<PotentialDollarIdentifier> for PotentialGenericTypeIdentifier {
    fn into(self) -> PotentialDollarIdentifier {
        match self {
            Self::Identifier(x) => x,
            Self::Generic {
                identifier,
                generic_types: _,
            } => identifier,
        }
    }
}

impl Into<Node> for PotentialGenericTypeIdentifier {
    fn into(self) -> Node {
        Node {
            span: *self.span(),
            node_type: NodeType::Identifier(self),
        }
    }
}

impl From<ParserText> for PotentialGenericTypeIdentifier {
    fn from(value: ParserText) -> Self {
        Self::Identifier(value.into())
    }
}

impl PotentialGenericTypeIdentifier {
    pub fn span(&self) -> &Span {
        match self {
            Self::Identifier(x) => x.span(),
            Self::Generic {
                identifier,
                generic_types: _,
            } => identifier.span(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum PotentialDollarIdentifier {
    DollarIdentifier(ParserText),
    Identifier(ParserText),
}

impl PotentialDollarIdentifier {
    pub fn new(span: Span, text: impl ToString) -> Self {
        Self::DollarIdentifier(ParserText::new(span, text))
    }

    pub fn span(&self) -> &Span {
        match self {
            Self::Identifier(x) => &x.span,
            Self::DollarIdentifier(x) => &x.span,
        }
    }
}

impl Into<PotentialGenericTypeIdentifier> for PotentialDollarIdentifier {
    fn into(self) -> PotentialGenericTypeIdentifier {
        PotentialGenericTypeIdentifier::Identifier(self)
    }
}

impl Into<Node> for PotentialDollarIdentifier {
    fn into(self) -> Node {
        Node {
            span: *self.span(),
            node_type: NodeType::Identifier(PotentialGenericTypeIdentifier::Identifier(self)),
        }
    }
}

impl From<ParserText> for PotentialDollarIdentifier {
    fn from(value: ParserText) -> Self {
        Self::Identifier(value)
    }
}

impl Display for PotentialDollarIdentifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Identifier(x) => write!(f, "{}", x),
            Self::DollarIdentifier(x) => write!(f, "${}", x),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct ParserText {
    pub text: String,
    pub span: Span,
}

impl Deref for ParserText {
    type Target = String;
    fn deref(&self) -> &Self::Target {
        &self.text
    }
}

impl DerefMut for ParserText {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.text
    }
}

impl ParserText {
    pub fn new(span: Span, text: impl ToString) -> Self {
        Self {
            text: text.to_string(),
            span,
        }
    }

    pub fn temp_name(span: Span) -> Self {
        Self::new(
            span,
            format!("{}-{}", span, {
                let mut counter = COUNTER.write().unwrap();
                *counter += 1;
                *counter
            }),
        )
    }

    pub fn temp_name_with_prefix(prefix: impl Display, span: Span) -> Self {
        Self::new(
            span,
            format!("{prefix}-{}-{}", span, {
                let mut counter = COUNTER.write().unwrap();
                *counter += 1;
                *counter
            }),
        )
    }
}

impl From<String> for ParserText {
    fn from(value: String) -> Self {
        Self::new(Span::default(), value)
    }
}

impl FromStr for ParserText {
    type Err = ParserError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self::from(s.to_string()))
    }
}

impl Display for ParserText {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.text)
    }
}

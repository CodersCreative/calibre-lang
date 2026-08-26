use crate::{
    COUNTER, ParserError, Span,
    ast::{
        nodes::{Node, NodeType},
        types::ParserDataType,
    },
};
use serde::{Deserialize, Serialize};
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
        generic_types: Vec<ParserDataType>,
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

impl From<PotentialGenericTypeIdentifier> for PotentialDollarIdentifier {
    fn from(val: PotentialGenericTypeIdentifier) -> PotentialDollarIdentifier {
        match val {
            PotentialGenericTypeIdentifier::Identifier(x) => x,
            PotentialGenericTypeIdentifier::Generic {
                identifier,
                generic_types: _,
            } => identifier,
        }
    }
}

impl From<PotentialGenericTypeIdentifier> for Node {
    fn from(val: PotentialGenericTypeIdentifier) -> Node {
        Node {
            span: *val.span(),
            node_type: NodeType::Identifier(val),
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
        Self::Identifier(ParserText::new(span, text))
    }

    pub fn span(&self) -> &Span {
        match self {
            Self::Identifier(x) => &x.span,
            Self::DollarIdentifier(x) => &x.span,
        }
    }

    pub fn text(&self) -> &String {
        match self {
            Self::Identifier(x) => &x.text,
            Self::DollarIdentifier(x) => &x.text,
        }
    }
}

impl From<PotentialDollarIdentifier> for PotentialGenericTypeIdentifier {
    fn from(val: PotentialDollarIdentifier) -> PotentialGenericTypeIdentifier {
        PotentialGenericTypeIdentifier::Identifier(val)
    }
}

impl From<PotentialDollarIdentifier> for Node {
    fn from(val: PotentialDollarIdentifier) -> Node {
        Node {
            span: *val.span(),
            node_type: NodeType::Identifier(PotentialGenericTypeIdentifier::Identifier(val)),
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum IntLiteralType {
    Int,
    UInt,
    Byte,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ParsedIntLiteral {
    pub value: i64,
    pub int_type: IntLiteralType,
}

impl ParsedIntLiteral {
    pub fn parse(text: impl ToString) -> Option<Self> {
        let text = text.to_string();

        let (number_text, int_suffix) = match text.chars().last() {
            Some(c) if matches!(c, 'u' | 'i' | 'b') => {
                (&text[..text.len().saturating_sub(1)], Some(c))
            }
            _ => (text.as_str(), None),
        };

        let number_text = number_text.replace('_', "");
        let parse_base = |text: &str| {
            if let Some((_, x)) = text.split_once("x") {
                i64::from_str_radix(x, 16)
            } else if let Some((_, x)) = text.split_once("o") {
                i64::from_str_radix(x, 8)
            } else if let Some((_, x)) = text.split_once("b") {
                i64::from_str_radix(x, 2)
            } else {
                text.parse()
            }
        };

        let parsed = if let Some((base, exp)) = number_text.split_once('e') {
            match (parse_base(base).ok(), exp.parse::<u32>().ok()) {
                (Some(base_val), Some(power)) => base_val.checked_mul(10_i64.pow(power)).ok_or(()),
                _ => Err(()),
            }
        } else {
            parse_base(&number_text).map_err(|_| ())
        };

        Some(Self {
            value: parsed.ok()?,
            int_type: match int_suffix {
                Some('u') => IntLiteralType::UInt,
                Some('b') => IntLiteralType::Byte,
                _ => IntLiteralType::Int,
            },
        })
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
            format!("[{}-{}]", span, {
                let mut counter = COUNTER.write().unwrap();
                *counter += 1;
                *counter
            }),
        )
    }

    pub fn temp_name_with_suffix(suffix: impl ToString, span: Span) -> Self {
        let suffix = suffix.to_string();
        if Self::is_temp_name(&suffix) {
            return Self::new(span, suffix);
        }

        Self::new(
            span,
            format!("[{}-{}]{suffix}", span, {
                let mut counter = COUNTER.write().unwrap();
                *counter += 1;
                *counter
            }),
        )
    }

    pub fn is_temp_name(ident: &impl ToString) -> bool {
        ident.to_string().contains("[")
    }

    pub fn get_temp_name_suffix(ident: &impl ToString) -> Option<String> {
        let ident = ident.to_string();
        if !Self::is_temp_name(&ident) {
            return Some(ident);
        }

        Some(ident.split_once(']')?.1.to_string())
    }

    pub fn temp_name_suffix_matches(left: &impl ToString, right: &impl ToString) -> bool {
        let (left, right) = (left.to_string(), right.to_string());
        if left == right {
            return true;
        }

        let (left, right) = (
            Self::get_temp_name_suffix(&left).unwrap_or(left),
            Self::get_temp_name_suffix(&right).unwrap_or(right),
        );
        if left == right {
            return true;
        }

        left.split_once(":<")
            .and_then(|x| Some(x.0 == right.split_once(":<")?.0))
            .unwrap_or_default()
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

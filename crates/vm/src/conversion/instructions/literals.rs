use crate::conversion::Reg;
use serde::{Deserialize, Serialize};
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct VMLoadLiteral {
    pub dst: Reg,
    pub literal: u16,
}

impl Display for VMLoadLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "%r{} = LITERAL {}", self.dst, self.literal)
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct VMRange {
    pub dst: Reg,
    pub from: Reg,
    pub to: Reg,
    pub inclusive: bool,
}

impl Display for VMRange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "%r{} = RANGE %r{} ..{} %r{}",
            self.dst,
            self.from,
            if self.inclusive { "=" } else { "" },
            self.to
        )
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct VMList {
    pub dst: Reg,
    pub items: Vec<Reg>,
}

impl Display for VMList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "%r{} = LIST [{}]",
            self.dst,
            self.items
                .iter()
                .map(|x| format!("%r{x}"))
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct VMAggregate {
    pub dst: Reg,
    pub layout: u16,
    pub fields: Vec<Reg>,
}

impl Display for VMAggregate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "%r{} = STRUCT {} WITH [{}]",
            self.dst,
            self.layout,
            self.fields
                .iter()
                .map(|x| format!("%r{x}"))
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct VMEnum {
    pub dst: Reg,
    pub name: u16,
    pub variant: u16,
    pub payload: Option<Reg>,
}

impl Display for VMEnum {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "%r{} = ENUM {}:{}{}",
            self.dst,
            self.name,
            self.variant,
            self.payload
                .map(|x| format!(" WITH %r{x}"))
                .unwrap_or_default()
        )
    }
}

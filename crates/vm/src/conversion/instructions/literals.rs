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

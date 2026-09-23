use crate::conversion::Reg;
use serde::{Deserialize, Serialize};
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct VMRef {
    pub dst: Reg,
    pub value: Reg,
}

impl Display for VMRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "%r{} = REF %r{}", self.dst, self.value)
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct VMDeref {
    pub dst: Reg,
    pub value: Reg,
}

impl Display for VMDeref {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "%r{} = DEREF %r{}", self.dst, self.value)
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct VMSetRef {
    pub dst: Reg,
    pub target: Reg,
    pub value: Reg,
}

impl Display for VMSetRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "%r{} = SETREF %r{} = %r{}",
            self.dst, self.target, self.value
        )
    }
}

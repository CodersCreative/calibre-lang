use crate::conversion::Reg;
use serde::{Deserialize, Serialize};
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct VMLoadRegRef {
    pub dst: Reg,
    pub src: Reg,
}

impl Display for VMLoadRegRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "%r{} = REGREF %r{}", self.dst, self.src)
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct VMCopy {
    pub dst: Reg,
    pub src: Reg,
}

impl Display for VMCopy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "%r{} = COPY %r{}", self.dst, self.src)
    }
}

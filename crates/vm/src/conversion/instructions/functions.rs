use crate::conversion::Reg;
use serde::{Deserialize, Serialize};
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct VMCall {
    pub dst: Option<Reg>,
    pub callee: Reg,
    pub args: Box<[Reg]>,
}

impl Display for VMCall {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let args = self
            .args
            .iter()
            .map(|x| format!("%r{x}"))
            .collect::<Vec<_>>()
            .join(", ");

        if let Some(dst) = &self.dst {
            write!(f, "%r{} = CALL %r{} ({})", dst, self.callee, args)
        } else {
            write!(f, "%CALL %r{} ({})", self.callee, args)
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct VMCallSelf {
    pub dst: Option<Reg>,
    pub args: Box<[Reg]>,
}

impl Display for VMCallSelf {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let args = self
            .args
            .iter()
            .map(|x| format!("%r{x}"))
            .collect::<Vec<_>>()
            .join(", ");

        if let Some(dst) = &self.dst {
            write!(f, "%r{} = CALLSELF ({})", dst, args)
        } else {
            write!(f, "%CALLSELF ({})", args)
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct VMSpawn {
    pub dst: Reg,
    pub callee: Reg,
}

impl Display for VMSpawn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "r%{} = SPAWN %r{}", self.dst, self.callee)
    }
}

use crate::conversion::Reg;
use serde::{Deserialize, Serialize};
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct VMLoadVar {
    pub dst: Reg,
    pub name: u16,
}

impl Display for VMLoadVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "%r{} = LOAD {}", self.dst, self.name)
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct VMMoveVar {
    pub dst: Reg,
    pub name: u16,
}

impl Display for VMMoveVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "%r{} = MOVE {}", self.dst, self.name)
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct VMDropVar {
    pub name: u16,
}

impl Display for VMDropVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "DROP {}", self.name)
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct VMStoreVar {
    pub dst: Option<Reg>,
    pub name: u16,
    pub src: Reg,
}

impl Display for VMStoreVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.dst {
            Some(dst) => write!(f, "%r{} = STORE {} <- %r{}", dst, self.name, self.src),
            _ => write!(f, "STORE {} <- %r{}", self.name, self.src),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct VMLoadVarRef {
    pub dst: Reg,
    pub name: u16,
}

impl Display for VMLoadVarRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "%r{} = VARREF {}", self.dst, self.name)
    }
}

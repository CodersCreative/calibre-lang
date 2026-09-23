use crate::conversion::Reg;
use serde::{Deserialize, Serialize};
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct VMLoadMember {
    pub dst: Reg,
    pub value: Reg,
    pub member: u16,
}

impl Display for VMLoadMember {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "%r{} = LOADMEMBER %r{}.{}",
            self.dst, self.value, self.member
        )
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct VMSetMember {
    pub dst: Reg,
    pub target: Reg,
    pub member: u16,
    pub value: Reg,
}

impl Display for VMSetMember {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "%r{} = SETMEMBER %r{}.{} = %r{}",
            self.dst, self.target, self.member, self.value
        )
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct VMIndex {
    pub dst: Reg,
    pub value: Reg,
    pub index: Reg,
}

impl Display for VMIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "%r{} = INDEX %r{}[%r{}]",
            self.dst, self.value, self.index
        )
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct VMSetIndex {
    pub dst: Reg,
    pub target: Reg,
    pub index: Reg,
    pub value: Reg,
}

impl Display for VMSetIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "%r{} = SETINDEX %r{}[%r{}] = %r{}",
            self.dst, self.target, self.index, self.value
        )
    }
}

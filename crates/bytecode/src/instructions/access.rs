use crate::Reg;
use serde::{Deserialize, Serialize};
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct VMDiscriminant {
    pub dst: Reg,
    pub value: Reg,
}

impl Display for VMDiscriminant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "%r{} = DISCRIMINANT %r{}", self.dst, self.value,)
    }
}

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



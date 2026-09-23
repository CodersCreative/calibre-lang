use crate::conversion::Reg;
use calibre_lir::ast::BlockId;
use serde::{Deserialize, Serialize};
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct VMJump {
    pub target: BlockId,
}

impl Display for VMJump {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "JMP BLK {}", self.target.0)
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct VMBranch {
    pub cond: Reg,
    pub then_block: BlockId,
    pub else_block: BlockId,
}

impl Display for VMBranch {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "BRANCH JMP BLK {} if %r{} else JMP BLK {}",
            self.then_block.0, self.cond, self.else_block.0
        )
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct VMReturn {
    pub value: Option<Reg>,
}

impl Display for VMReturn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(r) = &self.value {
            write!(f, "RETURN %r{r}")
        } else {
            write!(f, "RETURN")
        }
    }
}

use crate::conversion::{
    Reg,
    instructions::{
        binary::{VMAs, VMBinary, VMBoolean, VMComparison, VMIs},
        functions::{VMCall, VMCallSelf, VMSpawn},
        literals::{VMAggregate, VMEnum, VMList, VMRange},
        registers::{VMCopy, VMLoadRegRef},
        variables::{VMLoadVarRef, VMStoreVar},
    },
};
use calibre_lir::ast::BlockId;
use literals::VMLoadLiteral;
use serde::{Deserialize, Serialize};
use std::fmt::Display;
use variables::{VMDropVar, VMLoadVar, VMMoveVar};

pub mod binary;
pub mod functions;
pub mod literals;
pub mod registers;
pub mod variables;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum VMInstruction {
    // Literals
    LoadLiteral(VMLoadLiteral),
    Range(VMRange),
    List(VMList),
    Aggregate(VMAggregate),
    Enum(VMEnum),
    Noop,

    // Variables
    LoadVar(VMLoadVar),
    MoveVar(VMMoveVar),
    DropVar(VMDropVar),
    StoreVar(VMStoreVar),
    LoadVarRef(VMLoadVarRef),

    // Registers
    LoadRegRef(VMLoadRegRef),
    Copy(VMCopy),

    // Binary
    As(VMAs),
    Is(VMIs),
    Binary(VMBinary),
    Comparison(VMComparison),
    Boolean(VMBoolean),

    // Functions
    Call(VMCall),
    CallSelf(VMCallSelf),
    Spawn(VMSpawn),

    // Access
    LoadMember {
        dst: Reg,
        value: Reg,
        member: u16,
    },
    SetMember {
        dst: Reg,
        target: Reg,
        member: u16,
        value: Reg,
    },
    Index {
        dst: Reg,
        value: Reg,
        index: Reg,
    },
    SetIndex {
        dst: Reg,
        target: Reg,
        index: Reg,
        value: Reg,
    },

    // Memory
    Ref {
        dst: Reg,
        value: Reg,
    },
    Deref {
        dst: Reg,
        value: Reg,
    },
    SetRef {
        dst: Reg,
        target: Reg,
        value: Reg,
    },

    // Termination
    Jump(BlockId),
    Branch {
        cond: Reg,
        then_block: BlockId,
        else_block: BlockId,
    },
    Return {
        value: Option<Reg>,
    },
}

impl Display for VMInstruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            // Literal
            VMInstruction::LoadLiteral(x) => x.fmt(f),
            VMInstruction::Range(x) => x.fmt(f),
            VMInstruction::List(x) => x.fmt(f),
            VMInstruction::Aggregate(x) => x.fmt(f),
            VMInstruction::Enum(x) => x.fmt(f),

            // Variables
            VMInstruction::LoadVar(x) => x.fmt(f),
            VMInstruction::MoveVar(x) => x.fmt(f),
            VMInstruction::DropVar(x) => x.fmt(f),
            VMInstruction::StoreVar(x) => x.fmt(f),
            VMInstruction::LoadVarRef(x) => x.fmt(f),

            // Registers
            VMInstruction::LoadRegRef(x) => x.fmt(f),
            VMInstruction::Copy(x) => x.fmt(f),

            // Binary
            VMInstruction::As(x) => x.fmt(f),
            VMInstruction::Is(x) => x.fmt(f),
            VMInstruction::Binary(x) => x.fmt(f),
            VMInstruction::Comparison(x) => x.fmt(f),
            VMInstruction::Boolean(x) => x.fmt(f),

            VMInstruction::Call(x) => x.fmt(f),
            VMInstruction::CallSelf(x) => x.fmt(f),
            VMInstruction::Spawn(x) => x.fmt(f),

            VMInstruction::LoadMember { dst, value, member } => {
                write!(f, "%r{dst} = LOADMEMBER %r{value}.{member}")
            }
            VMInstruction::SetMember {
                dst,
                target,
                member,
                value,
            } => {
                write!(f, "%r{dst} = SETMEMBER %r{target}.{member} = %r{value}")
            }
            VMInstruction::Index { dst, value, index } => {
                write!(f, "%r{dst} = INDEX %r{value}[%r{index}]")
            }
            VMInstruction::SetIndex {
                dst,
                target,
                index,
                value,
            } => {
                write!(f, "%r{dst} = SETINDEX %r{target}[%r{index}] = %r{value}")
            }
            VMInstruction::Ref { dst, value } => write!(f, "%r{dst} = REF %r{value}"),
            VMInstruction::Deref { dst, value } => write!(f, "%r{dst} = DEREF %r{value}"),
            VMInstruction::SetRef { dst, target, value } => {
                write!(f, "%r{dst} = SETREF %r{target} = %r{value}")
            }
            VMInstruction::Jump(id) => write!(f, "JMP BLK {}", id.0),
            VMInstruction::Branch {
                cond,
                then_block,
                else_block,
            } => {
                write!(
                    f,
                    "BRANCH JMP BLK {} if %r{} else JMP BLK {}",
                    then_block.0, cond, else_block.0
                )
            }
            VMInstruction::Return { value } => {
                if let Some(r) = value {
                    write!(f, "RETURN %r{r}")
                } else {
                    write!(f, "RETURN")
                }
            }
            VMInstruction::Noop => write!(f, "NOOP"),
        }
    }
}

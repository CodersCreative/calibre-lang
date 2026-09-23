use crate::conversion::instructions::{
    access::{VMIndex, VMLoadMember, VMSetIndex, VMSetMember},
    binary::{VMAs, VMBinary, VMBoolean, VMComparison, VMIs},
    functions::{VMCall, VMCallSelf, VMSpawn},
    literals::{VMAggregate, VMEnum, VMList, VMRange},
    memory::{VMDeref, VMRef, VMSetRef},
    registers::{VMCopy, VMLoadRegRef},
    termination::{VMBranch, VMJump, VMReturn},
    variables::{VMLoadVarRef, VMStoreVar},
};
use literals::VMLoadLiteral;
use serde::{Deserialize, Serialize};
use std::fmt::Display;
use variables::{VMDropVar, VMLoadVar, VMMoveVar};

pub mod access;
pub mod binary;
pub mod functions;
pub mod literals;
pub mod memory;
pub mod registers;
pub mod termination;
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
    LoadMember(VMLoadMember),
    SetMember(VMSetMember),
    Index(VMIndex),
    SetIndex(VMSetIndex),

    // Memory
    Ref(VMRef),
    Deref(VMDeref),
    SetRef(VMSetRef),

    // Termination
    Jump(VMJump),
    Branch(VMBranch),
    Return(VMReturn),
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

            // Functions
            VMInstruction::Call(x) => x.fmt(f),
            VMInstruction::CallSelf(x) => x.fmt(f),
            VMInstruction::Spawn(x) => x.fmt(f),

            // Access
            VMInstruction::LoadMember(x) => x.fmt(f),
            VMInstruction::SetMember(x) => x.fmt(f),
            VMInstruction::Index(x) => x.fmt(f),
            VMInstruction::SetIndex(x) => x.fmt(f),

            // Memory
            VMInstruction::Ref(x) => x.fmt(f),
            VMInstruction::Deref(x) => x.fmt(f),
            VMInstruction::SetRef(x) => x.fmt(f),

            // Termination
            VMInstruction::Jump(x) => x.fmt(f),
            VMInstruction::Branch(x) => x.fmt(f),
            VMInstruction::Return(x) => x.fmt(f),
            VMInstruction::Noop => write!(f, "NOOP"),
        }
    }
}

use crate::{
    Reg,
    instructions::{
        access::{VMDiscriminant, VMIndex, VMLoadMember, VMSetIndex, VMSetMember},
        binary::{VMAs, VMBinary, VMBoolean, VMComparison, VMIs},
        functions::{VMCall, VMCallSelf, VMSpawn},
        literals::{VMAggregate, VMEnum, VMList, VMRange},
        memory::{VMDeref, VMRef, VMSetRef},
        registers::{VMCopy, VMLoadRegRef},
        termination::{VMBranch, VMJump, VMReturn},
        variables::{VMLoadVarRef, VMStoreVar},
    },
};
use literals::VMLoadLiteral;
use rustc_hash::FxHashMap;
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
    Discriminant(VMDiscriminant),
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

impl VMInstruction {
    #[inline]
    pub fn get_dst(&self) -> Option<&u16> {
        match self {
        // Registers
        VMInstruction::Copy(VMCopy { dst, .. })
        | VMInstruction::LoadRegRef(VMLoadRegRef { dst, .. })
        // Variables
        | VMInstruction::LoadVar(VMLoadVar { dst, .. })
        | VMInstruction::MoveVar(VMMoveVar { dst, .. })
        | VMInstruction::LoadVarRef(VMLoadVarRef { dst, .. })
        // Binary
        | VMInstruction::As(VMAs { dst, .. })
        | VMInstruction::Is(VMIs { dst, .. })
        | VMInstruction::Binary(VMBinary { dst, .. })
        | VMInstruction::Comparison(VMComparison { dst, .. })
        | VMInstruction::Boolean(VMBoolean { dst, .. })
        // Functions
        | VMInstruction::Spawn(VMSpawn { dst, .. })
        // Access
        | VMInstruction::LoadMember(VMLoadMember { dst, .. })
        | VMInstruction::SetMember(VMSetMember { dst, .. })
        | VMInstruction::Index(VMIndex { dst, .. })
        | VMInstruction::SetIndex(VMSetIndex { dst, .. })
        | VMInstruction::Discriminant(VMDiscriminant { dst, .. })
        // Memory
        | VMInstruction::Ref(VMRef { dst, .. })
        | VMInstruction::Deref(VMDeref { dst, .. })
        | VMInstruction::SetRef(VMSetRef { dst, .. })
        // Literals
        | VMInstruction::LoadLiteral(VMLoadLiteral { dst, .. })
        | VMInstruction::Aggregate(VMAggregate { dst, .. })
        | VMInstruction::Enum(VMEnum { dst, .. })
        | VMInstruction::List(VMList { dst, .. })
        | VMInstruction::Range(VMRange { dst, .. }) => Some(dst),

        VMInstruction::Call(VMCall { dst, .. })
        | VMInstruction::StoreVar(VMStoreVar { dst, .. })
        | VMInstruction::CallSelf(VMCallSelf { dst, .. }) => dst.as_ref(),
        _ => None,
    }
    }

    #[inline]
    pub fn get_dst_mut(&mut self) -> Option<&mut u16> {
        match self {
        // Registers
        VMInstruction::Copy(VMCopy { dst, .. })
        | VMInstruction::LoadRegRef(VMLoadRegRef { dst, .. })
        // Variables
        | VMInstruction::LoadVar(VMLoadVar { dst, .. })
        | VMInstruction::MoveVar(VMMoveVar { dst, .. })
        | VMInstruction::LoadVarRef(VMLoadVarRef { dst, .. })
        // Binary
        | VMInstruction::As(VMAs { dst, .. })
        | VMInstruction::Is(VMIs { dst, .. })
        | VMInstruction::Binary(VMBinary { dst, .. })
        | VMInstruction::Comparison(VMComparison { dst, .. })
        | VMInstruction::Boolean(VMBoolean { dst, .. })
        // Functions
        | VMInstruction::Spawn(VMSpawn { dst, .. })
        // Access
        | VMInstruction::LoadMember(VMLoadMember { dst, .. })
        | VMInstruction::SetMember(VMSetMember { dst, .. })
        | VMInstruction::Index(VMIndex { dst, .. })
        | VMInstruction::SetIndex(VMSetIndex { dst, .. })
        | VMInstruction::Discriminant(VMDiscriminant { dst, .. })
        // Memory
        | VMInstruction::Ref(VMRef { dst, .. })
        | VMInstruction::Deref(VMDeref { dst, .. })
        | VMInstruction::SetRef(VMSetRef { dst, .. })
        // Literals
        | VMInstruction::LoadLiteral(VMLoadLiteral { dst, .. })
        | VMInstruction::Aggregate(VMAggregate { dst, .. })
        | VMInstruction::Enum(VMEnum { dst, .. })
        | VMInstruction::List(VMList { dst, .. })
        | VMInstruction::Range(VMRange { dst, .. }) => Some(dst),

        VMInstruction::Call(VMCall { dst, .. })
        | VMInstruction::StoreVar(VMStoreVar { dst, .. })
        | VMInstruction::CallSelf(VMCallSelf { dst, .. }) => dst.as_mut(),
        _ => None,
    }
    }

    #[inline]
    pub fn has_side_effects(&self) -> bool {
        matches!(
            self,
            VMInstruction::Call(_)
                | VMInstruction::CallSelf(_)
                | VMInstruction::StoreVar(_)
                | VMInstruction::SetMember(_)
                | VMInstruction::SetIndex(_)
                | VMInstruction::SetRef(_)
                | VMInstruction::Branch(_)
                | VMInstruction::Jump(_)
                | VMInstruction::Return(_)
                | VMInstruction::Spawn(_)
        )
    }

    pub fn count_instr_uses(&self, use_map: &mut FxHashMap<Reg, usize>) {
        let mut add_use = |r: Reg| {
            *use_map.entry(r).or_insert(0) += 1;
        };

        match self {
            VMInstruction::Copy(VMCopy { src, .. })
            | VMInstruction::Ref(VMRef { value: src, .. })
            | VMInstruction::Deref(VMDeref { value: src, .. })
            | VMInstruction::Discriminant(VMDiscriminant { value: src, .. })
            | VMInstruction::LoadRegRef(VMLoadRegRef { src, .. })
            | VMInstruction::Spawn(VMSpawn { callee: src, .. })
            | VMInstruction::Is(VMIs { src, .. })
            | VMInstruction::As(VMAs { src, .. }) => add_use(*src),
            VMInstruction::StoreVar(VMStoreVar { src, .. }) => add_use(*src),
            VMInstruction::Call(VMCall { callee, args, .. }) => {
                add_use(*callee);
                for arg in args {
                    add_use(*arg);
                }
            }
            VMInstruction::CallSelf(VMCallSelf { args, .. }) => {
                for arg in args {
                    add_use(*arg);
                }
            }
            VMInstruction::Binary(VMBinary { left, right, .. })
            | VMInstruction::Range(VMRange {
                from: left,
                to: right,
                ..
            })
            | VMInstruction::Comparison(VMComparison { left, right, .. })
            | VMInstruction::Boolean(VMBoolean { left, right, .. }) => {
                add_use(*left);
                add_use(*right);
            }
            VMInstruction::Index(VMIndex { value, index, .. }) => {
                add_use(*value);
                add_use(*index);
            }
            VMInstruction::LoadMember(VMLoadMember { value, .. }) => add_use(*value),
            VMInstruction::SetRef(VMSetRef {
                dst: _,
                target,
                value,
            })
            | VMInstruction::SetMember(VMSetMember { target, value, .. }) => {
                add_use(*target);
                add_use(*value);
            }
            VMInstruction::SetIndex(VMSetIndex {
                target,
                index,
                value,
                ..
            }) => {
                add_use(*target);
                add_use(*index);
                add_use(*value);
            }
            VMInstruction::Branch(VMBranch { cond, .. }) => add_use(*cond),
            VMInstruction::Enum(VMEnum {
                payload: Some(v), ..
            })
            | VMInstruction::Return(VMReturn { value: Some(v) }) => add_use(*v),
            VMInstruction::Aggregate(VMAggregate { fields: items, .. })
            | VMInstruction::List(VMList { items, .. }) => {
                items.iter().for_each(|x| add_use(*x));
            }

            VMInstruction::Noop
            | VMInstruction::Jump(_)
            | VMInstruction::Return(_)
            | VMInstruction::Enum(_)
            | VMInstruction::LoadLiteral(_)
            | VMInstruction::MoveVar(_)
            | VMInstruction::DropVar(_)
            | VMInstruction::LoadVarRef(_)
            | VMInstruction::LoadVar(_) => {}
        }
    }
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
            VMInstruction::Discriminant(x) => x.fmt(f),

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

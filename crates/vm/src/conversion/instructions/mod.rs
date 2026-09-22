use crate::conversion::Reg;
use calibre_lir::ast::BlockId;
use calibre_parser::ast::{
    binary::BinaryOperator,
    comparison::{BooleanOperator, ComparisonOperator},
    nodes::binary::AsFailureMode,
    types::ParserDataType,
};
use literals::VMLoadLiteral;
use serde::{Deserialize, Serialize};
use std::fmt::Display;
use variables::{VMDropVar, VMLoadVar, VMMoveVar};

pub mod literals;
pub mod variables;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum VMInstruction {
    // Literals
    LoadLiteral(VMLoadLiteral),
    Range {
        dst: Reg,
        from: Reg,
        to: Reg,
        inclusive: bool,
    },
    List {
        dst: Reg,
        items: Vec<Reg>,
    },
    Aggregate {
        dst: Reg,
        layout: u16,
        fields: Vec<Reg>,
    },
    Enum {
        dst: Reg,
        name: u16,
        variant: u16,
        payload: Option<Reg>,
    },
    Noop,

    // Variables
    LoadVar(VMLoadVar),
    MoveVar(VMMoveVar),
    DropVar(VMDropVar),
    StoreVar {
        dst: Option<Reg>,
        name: u16,
        src: Reg,
    },
    LoadVarRef {
        dst: Reg,
        name: u16,
    },

    // Registers
    LoadRegRef {
        dst: Reg,
        src: Reg,
    },
    Copy {
        dst: Reg,
        src: Reg,
    },

    // Binary
    As {
        dst: Reg,
        src: Reg,
        data_type: ParserDataType,
        failure_mode: AsFailureMode,
    },
    Is {
        dst: Reg,
        src: Reg,
        data_type: ParserDataType,
    },
    Binary {
        dst: Reg,
        op: BinaryOperator,
        left: Reg,
        right: Reg,
    },
    Comparison {
        dst: Reg,
        op: ComparisonOperator,
        left: Reg,
        right: Reg,
    },
    Boolean {
        dst: Reg,
        op: BooleanOperator,
        left: Reg,
        right: Reg,
    },

    // Functions
    Call {
        dst: Option<Reg>,
        callee: Reg,
        args: Vec<Reg>,
    },
    CallSelf {
        dst: Option<Reg>,
        args: Vec<Reg>,
    },
    Spawn {
        dst: Reg,
        callee: Reg,
    },

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
            VMInstruction::LoadLiteral(x) => x.fmt(f),
            VMInstruction::LoadVar(x) => x.fmt(f),
            VMInstruction::MoveVar(x) => x.fmt(f),
            VMInstruction::DropVar(x) => x.fmt(f),
            VMInstruction::StoreVar {
                name,
                src,
                dst: Some(dst),
            } => write!(f, "%r{dst} = STORE {name} <- %r{src}"),
            VMInstruction::StoreVar { name, src, dst: _ } => write!(f, "STORE {name} <- %r{src}"),
            VMInstruction::LoadVarRef { dst, name } => write!(f, "%r{dst} = VARREF {name}"),
            VMInstruction::LoadRegRef { dst, src } => write!(f, "%r{dst} = REGREF %r{src}"),
            VMInstruction::Copy { dst, src } => write!(f, "%r{dst} = %r{src}"),
            VMInstruction::As {
                dst,
                src,
                data_type,
                failure_mode,
            } => {
                let suffix = match failure_mode {
                    AsFailureMode::Panic => "!",
                    AsFailureMode::Option => "?",
                    AsFailureMode::Result => "",
                };
                write!(f, "%r{dst} = %r{src} AS{} {data_type}", suffix)
            }
            VMInstruction::Is {
                dst,
                src,
                data_type,
            } => write!(f, "%r{dst} = %r{src} IS {data_type}"),
            VMInstruction::Binary {
                dst,
                op,
                left,
                right,
            } => {
                write!(f, "%r{dst} = BINARY %r{left} {op} %r{right}")
            }
            VMInstruction::Comparison {
                dst,
                op,
                left,
                right,
            } => {
                write!(f, "%r{dst} = COMPARE %r{left} {op} %r{right}")
            }
            VMInstruction::Boolean {
                dst,
                op,
                left,
                right,
            } => {
                write!(f, "%r{dst} = BOOLEAN %r{left} {op} %r{right}")
            }
            VMInstruction::Range {
                dst,
                from,
                to,
                inclusive,
            } => {
                let inclusive = if *inclusive { "=" } else { "" };
                write!(f, "%r{dst} = RANGE %r{from} ..{inclusive} %r{to}")
            }
            VMInstruction::List { dst, items } => write!(f, "%r{dst} = LIST {:?}", items),
            VMInstruction::Aggregate { dst, layout, .. } => {
                write!(f, "%r{dst} = STRUCT {layout}")
            }
            VMInstruction::Enum {
                dst, name, variant, ..
            } => {
                write!(f, "%r{dst} = ENUM {name}:{variant}")
            }
            VMInstruction::Call {
                dst: Some(dst),
                callee,
                args,
            } => {
                write!(f, "%r{dst} = CALL %r{callee} {:?}", args)
            }
            VMInstruction::Call {
                dst: _,
                callee,
                args,
            } => {
                write!(f, "CALL %r{callee} {:?}", args)
            }
            VMInstruction::CallSelf { dst: Some(dst), .. } => write!(f, "%r{dst} = CALL_SELF"),
            VMInstruction::CallSelf { .. } => write!(f, "CALL_SELF"),
            VMInstruction::Spawn { dst, callee } => write!(f, "SPAWN %r{dst}, %r{callee}"),
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
                    "BRANCH %r{cond} ? JMP BLK {} : JMP BLK {}",
                    then_block.0, else_block.0
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

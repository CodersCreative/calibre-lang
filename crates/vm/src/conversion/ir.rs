use calibre_lir::{BlockId, LirGlobal, LirLiteral, LirRegistry};
use calibre_parser::Span;
use calibre_parser::ast::{
    AsFailureMode, ParserDataType,
    binary::BinaryOperator,
    comparison::{BooleanOperator, ComparisonOperator},
};
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};
use std::fmt::Display;
use std::sync::Arc;

pub type Reg = u16;

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct VMRegistry {
    #[serde(with = "crate::serialization::serde_fxhashmap_rc")]
    pub functions: FxHashMap<String, Arc<VMFunction>>,
    #[serde(with = "crate::serialization::serde_fxhashmap")]
    pub globals: FxHashMap<String, VMGlobal>,
    #[serde(default)]
    pub dyn_vtables: FxHashMap<String, FxHashMap<String, FxHashMap<String, String>>>,
}

impl Display for VMRegistry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut txt = String::new();

        for val in &self.globals {
            txt.push_str(&format!("{}\n", val.1));
        }

        for func in &self.functions {
            txt.push_str(&format!("{}\n\n", func.1.as_ref()));
        }

        write!(f, "{}", txt)
    }
}

impl From<LirRegistry> for VMRegistry {
    fn from(value: LirRegistry) -> Self {
        let mut functions =
            FxHashMap::with_capacity_and_hasher(value.functions.len(), Default::default());
        for (k, func) in value.functions {
            functions.insert(k, Arc::new(func.into()));
        }

        let mut globals =
            FxHashMap::with_capacity_and_hasher(value.globals.len(), Default::default());
        for (k, v) in value.globals {
            globals.insert(k, v.into());
        }

        Self {
            functions,
            globals,
            dyn_vtables: value.dyn_vtables,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VMGlobal {
    pub name: String,
    pub blocks: Box<[VMBlock]>,
    pub reg_count: Reg,
    pub entry: BlockId,
    #[serde(with = "crate::serialization::serde_fxhashmap")]
    pub block_map: FxHashMap<BlockId, usize>,
}

impl Display for VMGlobal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut txt = format!("CONST {}", self.name);

        for block in &self.blocks {
            txt.push_str(&format!("\n{}", block).replace("\n", "\n\t"));
        }

        write!(f, "{}", txt)
    }
}

impl From<LirGlobal> for VMGlobal {
    fn from(value: LirGlobal) -> Self {
        let func = VMFunction::from_global(value.name.to_string(), value.blocks.into_vec());
        Self {
            name: value.name.to_string(),
            blocks: func.blocks,
            reg_count: func.reg_count,
            entry: func.entry,
            block_map: func.block_map,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VMFunction {
    pub name: String,
    pub params: Box<[String]>,
    pub captures: Box<[String]>,
    pub returns_value: bool,
    pub blocks: Box<[VMBlock]>,
    #[serde(with = "crate::serialization::serde_fxhashmap")]
    pub renamed: FxHashMap<String, String>,
    pub reg_count: Reg,
    pub param_regs: Vec<Reg>,
    pub ret_reg: Reg,
    pub entry: BlockId,
    #[serde(with = "crate::serialization::serde_fxhashmap")]
    pub block_map: FxHashMap<BlockId, usize>,
}

impl VMFunction {
    pub fn rename(mut self, mut declared: FxHashMap<String, String>) -> Self {
        for param in self.params.iter_mut() {
            let new_name = format!("{}->{}", param, fastrand::u32(0..u32::MAX));
            declared.insert(param.to_string(), new_name.clone());
            *param = new_name;
        }

        for block in self.blocks.iter_mut() {
            for instruction in block.instructions.iter() {
                match instruction {
                    VMInstruction::StoreGlobal { name, .. }
                    | VMInstruction::DropGlobal { name }
                    | VMInstruction::LoadGlobal { name, .. }
                    | VMInstruction::MoveGlobal { name, .. }
                    | VMInstruction::LoadGlobalRef { name, .. } => {
                        if let Some(dest) = block.local_strings.get(*name as usize) {
                            if !declared.contains_key(dest) {
                                declared.insert(
                                    dest.to_string(),
                                    format!("{}->{}", dest, fastrand::u32(0..u32::MAX)),
                                );
                            }
                        }
                    }
                    _ => {}
                }
            }

            for string in block.local_strings.iter_mut() {
                if let Some(x) = declared.get(string) {
                    *string = x.to_string();
                }
            }

            for literal in block.local_literals.iter_mut() {
                match literal {
                    VMLiteral::Closure { label, captures: _ } => {
                        if let Some(x) = declared.get(label) {
                            *label = x.to_string();
                        }
                    }
                    _ => {}
                }
            }
        }

        self.renamed = declared;
        self
    }
}

impl Display for VMFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut txt = format!(
            "{} {} (",
            if self.returns_value {
                "FUNCTION"
            } else {
                "PROCEDURE"
            },
            self.name
        );
        for param in &self.params {
            txt.push_str(&format!("{}, ", param));
        }
        txt = txt.trim_end().trim_end_matches(",").to_string();
        txt.push_str(")");
        for block in &self.blocks {
            txt.push_str(&format!("\n{}", block).replace("\n", "\n\t"));
        }
        txt.push_str(&format!(
            "\nEND{}",
            if self.returns_value {
                "FUNCTION"
            } else {
                "PROCEDURE"
            }
        ));
        write!(f, "{}", txt)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VMBlock {
    pub id: BlockId,
    pub instructions: Vec<VMInstruction>,
    pub instruction_spans: Vec<Span>,
    pub local_literals: Vec<VMLiteral>,
    pub local_strings: Vec<String>,
    pub aggregate_layouts: Vec<AggregateLayout>,
    pub phis: Vec<PhiNode>,
}

impl Display for VMBlock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut txt = format!("BLK {}:", self.id.0);
        for instr in &self.instructions {
            txt.push_str(&format!("\n{};", instr));
        }
        write!(f, "{}", txt.replace("\n", "\n\t"))
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PhiNode {
    pub dest: Reg,
    pub sources: Vec<(BlockId, Reg)>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AggregateLayout {
    pub name: Option<String>,
    pub members: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum VMLiteral {
    Int(i64),
    UInt(u64),
    Byte(u8),
    Float(f64),
    Char(char),
    String(String),
    Null,
    Closure {
        label: String,
        captures: Vec<String>,
    },
    ExternFunction {
        abi: String,
        library: String,
        symbol: String,
        parameters: Vec<ParserDataType>,
        return_type: ParserDataType,
    },
}

impl Display for VMLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int(x) => write!(f, "{x}"),
            Self::UInt(x) => write!(f, "{x}u"),
            Self::Byte(x) => write!(f, "{x}b"),
            Self::Float(x) => write!(f, "{x}f"),
            Self::Char(x) => write!(f, "'{x}'"),
            Self::String(x) => write!(f, "{x:?}"),
            Self::Null => write!(f, "null"),
            Self::Closure { label, .. } => write!(f, "CLOSURE {label}"),
            Self::ExternFunction {
                abi,
                library,
                symbol,
                parameters,
                return_type,
            } => {
                let mut txt = format!("EXTERN \"{}\" {}(", abi, symbol);
                for (i, param) in parameters.iter().enumerate() {
                    if i > 0 {
                        txt.push_str(", ");
                    }
                    txt.push_str(&param.to_string());
                }
                txt.push_str(") -> ");
                txt.push_str(&return_type.to_string());
                txt.push_str(&format!(" from {}", library));
                write!(f, "{}", txt)
            }
        }
    }
}

impl From<LirLiteral> for VMLiteral {
    fn from(value: LirLiteral) -> Self {
        match value {
            LirLiteral::Int(x) => Self::Int(x),
            LirLiteral::UInt(x) => Self::UInt(x),
            LirLiteral::Byte(x) => Self::Byte(x),
            LirLiteral::Float(x) => Self::Float(x),
            LirLiteral::Char(x) => Self::Char(x),
            LirLiteral::String(x) => Self::String(x),
            LirLiteral::Null => Self::Null,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum VMInstruction {
    LoadLiteral {
        dst: Reg,
        literal: u16,
    },
    LoadGlobal {
        dst: Reg,
        name: u16,
    },
    MoveGlobal {
        dst: Reg,
        name: u16,
    },
    DropGlobal {
        name: u16,
    },
    StoreGlobal {
        name: u16,
        src: Reg,
    },
    SetLocalName {
        name: u16,
        src: Reg,
    },
    LoadGlobalRef {
        dst: Reg,
        name: u16,
    },
    LoadRegRef {
        dst: Reg,
        src: Reg,
    },
    Copy {
        dst: Reg,
        src: Reg,
    },
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
    AccLoad {
        src: Reg,
    },
    AccStore {
        dst: Reg,
    },
    AccBinary {
        op: BinaryOperator,
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
    Call {
        dst: Reg,
        callee: Reg,
        args: Vec<Reg>,
    },
    CallDirect {
        dst: Reg,
        name: u16,
        args: Vec<Reg>,
    },
    CallSelf {
        dst: Reg,
        args: Vec<Reg>,
    },
    Spawn {
        dst: Reg,
        callee: Reg,
    },
    LoadMember {
        dst: Reg,
        value: Reg,
        member: u16,
    },
    SetMember {
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
        target: Reg,
        index: Reg,
        value: Reg,
    },
    Ref {
        dst: Reg,
        value: Reg,
    },
    Deref {
        dst: Reg,
        value: Reg,
    },
    SetRef {
        target: Reg,
        value: Reg,
    },
    Jump(BlockId),
    Branch {
        cond: Reg,
        then_block: BlockId,
        else_block: BlockId,
    },
    Return {
        value: Option<Reg>,
    },
    Noop,
}

impl Display for VMInstruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VMInstruction::LoadLiteral { dst, literal } => write!(f, "%r{dst} = LITERAL {literal}"),
            VMInstruction::LoadGlobal { dst, name } => write!(f, "%r{dst} = LOAD {name}"),
            VMInstruction::MoveGlobal { dst, name } => write!(f, "%r{dst} = MOVE {name}"),
            VMInstruction::DropGlobal { name } => write!(f, "DROP {name}"),
            VMInstruction::StoreGlobal { name, src } => write!(f, "STORE {name} <- %r{src}"),
            VMInstruction::SetLocalName { name, src } => write!(f, "SET {name} <- %r{src}"),
            VMInstruction::LoadGlobalRef { dst, name } => write!(f, "%r{dst} = REF {name}"),
            VMInstruction::LoadRegRef { dst, src } => write!(f, "%r{dst} = REF %r{src}"),
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
            VMInstruction::AccLoad { src } => write!(f, "ACC = %r{src}"),
            VMInstruction::AccStore { dst } => write!(f, "%r{dst} = ACC"),
            VMInstruction::AccBinary { op, right } => write!(f, "ACC = BINARY ACC {op} %r{right}"),
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
            VMInstruction::Call { dst, callee, .. } => write!(f, "%r{dst} = CALL %r{callee}"),
            VMInstruction::CallDirect { dst, name, .. } => write!(f, "%r{dst} = CALL @{name}"),
            VMInstruction::CallSelf { dst, .. } => write!(f, "%r{dst} = CALL_SELF"),
            VMInstruction::Spawn { dst, callee } => write!(f, "SPAWN %r{dst}, %r{callee}"),
            VMInstruction::LoadMember { dst, value, member } => {
                write!(f, "%r{dst} = LOADMEMBER %r{value}.{member}")
            }
            VMInstruction::SetMember {
                target,
                member,
                value,
            } => {
                write!(f, "SETMEMBER %r{target}.{member} = %r{value}")
            }
            VMInstruction::Index { dst, value, index } => {
                write!(f, "%r{dst} = INDEX %r{value}[%r{index}]")
            }
            VMInstruction::SetIndex {
                target,
                index,
                value,
            } => {
                write!(f, "SETINDEX %r{target}[%r{index}] = %r{value}")
            }
            VMInstruction::Ref { dst, value } => write!(f, "%r{dst} = REF %r{value}"),
            VMInstruction::Deref { dst, value } => write!(f, "%r{dst} = DEREF %r{value}"),
            VMInstruction::SetRef { target, value } => write!(f, "SETREF %r{target} = %r{value}"),
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

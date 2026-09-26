use crate::{
    conversion::instructions::{
        VMInstruction,
        variables::{VMDropVar, VMLoadVar, VMLoadVarRef, VMMoveVar, VMStoreVar},
    },
    value::{BIG_PRECISION, BIG_ROUNDING, RuntimeValue, hashable::HashKey},
};
use astro_float::{BigFloat, Consts};
use calibre_lir::{
    ast::{BlockId, LirLiteral},
    environment::{LirGlobal, LirRegistry},
};
use calibre_parser::Span;
use calibre_parser::ast::{idents::ParserText, types::ParserDataType};
use indextree::NodeId;
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};
use std::fmt::Display;
use std::sync::Arc;
use ustr::{Ustr, UstrMap, UstrSet};

pub type Reg = u16;

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct VMRegistry {
    #[serde(with = "crate::serialization::serde_ustrmap_rc")]
    pub functions: UstrMap<Arc<VMFunction>>,
    pub globals: UstrMap<VMGlobal>,
    pub natives: UstrMap<Ustr>,
    #[serde(default)]
    pub dyn_vtables: UstrMap<UstrMap<UstrMap<Ustr>>>,
    #[serde(default)]
    pub scope_to_file: FxHashMap<NodeId, Ustr>,
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
            UstrMap::with_capacity_and_hasher(value.functions.len(), Default::default());
        for (k, func) in value.functions {
            functions.insert(k, Arc::new(func.into()));
        }

        let mut globals =
            UstrMap::with_capacity_and_hasher(value.globals.len(), Default::default());
        for (k, v) in value.globals {
            globals.insert(k, v.into());
        }

        Self {
            functions,
            globals,
            natives: value.natives,
            dyn_vtables: value.dyn_vtables,
            scope_to_file: value.scope_to_file,
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
        let func = VMFunction::from_global(value.name, value.blocks.into_vec());
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
    pub name: Ustr,
    pub params: Box<[Ustr]>,
    #[serde(skip)]
    pub param_names: UstrSet,
    pub captures: Box<[Ustr]>,
    pub returns_value: bool,
    pub blocks: Box<[VMBlock]>,
    pub renamed: UstrMap<Ustr>,
    pub reg_count: Reg,
    pub param_regs: Vec<Reg>,
    pub ret_reg: Reg,
    pub entry: BlockId,
    #[serde(with = "crate::serialization::serde_fxhashmap")]
    pub block_map: FxHashMap<BlockId, usize>,
    pub pure: bool,
    pub memo: bool,
    pub memo_params: usize,
    pub referenced_params: usize,
}

impl VMFunction {
    pub fn rename(mut self, mut declared: UstrMap<Ustr>) -> Self {
        for param in self.params.iter_mut() {
            let new_name = Ustr::from(&format!("{}->{}", param, fastrand::u32(0..u32::MAX)));
            declared.insert(Ustr::from(param), new_name);
            *param = new_name;
        }

        for block in self.blocks.iter_mut() {
            for instruction in block.instructions.iter() {
                match instruction {
                    VMInstruction::StoreVar(VMStoreVar { name, .. })
                    | VMInstruction::DropVar(VMDropVar { name })
                    | VMInstruction::LoadVar(VMLoadVar { name, .. })
                    | VMInstruction::MoveVar(VMMoveVar { name, .. })
                    | VMInstruction::LoadVarRef(VMLoadVarRef { name, .. }) => {
                        if let Some(dest) = block.local_strings.get(*name as usize)
                            && !declared.contains_key(dest)
                        {
                            declared.insert(
                                *dest,
                                Ustr::from(&format!("{}->{}", dest, fastrand::u32(0..u32::MAX))),
                            );
                        }
                    }
                    _ => {}
                }
            }

            for string in block.local_strings.iter_mut() {
                if let Some(x) = declared.get(string) {
                    *string = *x;
                }
            }

            for literal in block.local_literals.iter_mut() {
                if let VMLiteral::Closure { label, captures: _ } = literal
                    && let Some(x) = declared.get(label)
                {
                    *label = *x;
                }
            }
        }

        self.renamed = declared;
        self
    }

    pub fn memo_key(&self, args: &[RuntimeValue]) -> Option<Vec<HashKey>> {
        args.iter()
            .enumerate()
            .filter(|(index, _)| self.memo_params == 0 || self.memo_params & (1 << index) != 0)
            .map(|(_, arg)| HashKey::try_from(arg.clone()).ok())
            .collect()
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
        txt.push(')');
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
    pub local_strings: Vec<Ustr>,
    pub aggregate_layouts: Vec<AggregateLayout>,
    pub phis: Vec<PhiNode>,
}

impl Display for VMBlock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut txt = format!("BLK {}:", self.id.0);
        if !self.local_literals.is_empty() {
            txt.push_str("\nLITERALS:");
            for (i, literal) in self.local_literals.iter().enumerate() {
                txt.push_str(&format!("\n\t{} : {}", i, literal));
            }
        }

        if !self.local_strings.is_empty() {
            txt.push_str("\nSTRINGS:");
            for (i, string) in self.local_strings.iter().enumerate() {
                txt.push_str(&format!("\n\t{} : {}", i, string));
            }
        }

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
    pub name: Option<Ustr>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AggregateLayout {
    pub name: Option<Ustr>,
    pub members: Vec<Ustr>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum VMLiteral {
    Bool(bool),
    Big(BigFloat),
    Int(i64),
    UInt(u64),
    Byte(u8),
    Float(f64),
    Char(char),
    String(Ustr),
    Null,
    Closure {
        label: Ustr,
        captures: Box<[Ustr]>,
    },
    ExternFunction {
        abi: Ustr,
        library: Ustr,
        symbol: Ustr,
        parameters: Box<[ParserDataType]>,
        return_type: ParserDataType,
        memo_params: usize,
        memo: bool,
        pure: bool,
    },
}

impl Display for VMLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bool(x) => write!(f, "{x}"),
            Self::Int(x) => write!(f, "{x}"),
            Self::UInt(x) => write!(f, "{x}u"),
            Self::Byte(x) => write!(f, "{x}b"),
            Self::Float(x) => write!(f, "{x}f"),
            Self::Char(x) => write!(f, "{}", ParserText::format_char_literal(*x)),
            Self::Big(x) => write!(f, "{x}g"),
            Self::String(x) => write!(f, "{}", ParserText::format_string_value(x)),
            Self::Null => write!(f, "null"),
            Self::Closure { label, .. } => write!(f, "CLOSURE {label}"),
            Self::ExternFunction {
                abi,
                library,
                symbol,
                parameters,
                return_type,
                ..
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

impl VMLiteral {
    pub fn from_lir_literal(value: LirLiteral, cc: &mut Consts) -> Self {
        match value {
            LirLiteral::Bool(x) => Self::Bool(x),
            LirLiteral::Int(x) => Self::Int(x),
            LirLiteral::UInt(x) => Self::UInt(x),
            LirLiteral::Byte(x) => Self::Byte(x),
            LirLiteral::Float(x) => Self::Float(x),
            LirLiteral::Char(x) => Self::Char(x),
            LirLiteral::String(x) => Self::String(x),
            LirLiteral::Big(x) => Self::Big(BigFloat::parse(
                &x,
                astro_float::Radix::Dec,
                BIG_PRECISION,
                BIG_ROUNDING,
                cc,
            )),
            LirLiteral::Null => Self::Null,
        }
    }
}

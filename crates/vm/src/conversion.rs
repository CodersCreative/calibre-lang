use calibre_lir::{
    BlockId, LirBlock, LirFunction, LirGlobal, LirInstr, LirLValue, LirLiteral, LirNodeType,
    LirRegistry, LirTerminator,
};
use calibre_parser::ast::{
    ParserDataType, ParserInnerType, PotentialFfiDataType,
    binary::BinaryOperator,
    comparison::{BooleanOperator, ComparisonOperator},
};
use calibre_parser::lexer::Span;
use rand::random_range;
use rustc_hash::{FxHashMap, FxHashSet};
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
        let mut functions = FxHashMap::default();
        for (k, func) in value.functions {
            functions.insert(k, Arc::new(func.into()));
        }

        let mut globals = FxHashMap::default();
        for (k, v) in value.globals {
            globals.insert(k, v.into());
        }

        Self { functions, globals }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VMGlobal {
    pub name: String,
    pub blocks: Vec<VMBlock>,
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
        let func = VMFunction::from_global(value.name.clone(), value.blocks);
        Self {
            name: value.name,
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
    pub params: Vec<String>,
    pub captures: Vec<String>,
    pub returns_value: bool,
    pub blocks: Vec<VMBlock>,
    #[serde(with = "crate::serialization::serde_fxhashmap")]
    pub renamed: FxHashMap<String, String>,
    pub is_async: bool,
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
            let new_name = format!("{}->{}", param, random_range(0..10000000));
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
                                    format!("{}->{}", dest, random_range(0..10000000)),
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

    fn from_global(name: String, blocks: Vec<LirBlock>) -> Self {
        let func = LirFunction {
            name,
            params: Vec::new(),
            captures: Vec::new(),
            return_type: ParserDataType::new(Span::default(), ParserInnerType::Null),
            blocks,
            is_async: false,
        };
        let mut lower = FunctionLowering::new(func, true);
        lower.build_cfg();
        lower.build_ssa();
        lower.emit_blocks();
        VMFunction {
            name: lower.func.name.clone(),
            params: Vec::new(),
            captures: Vec::new(),
            returns_value: false,
            blocks: lower.blocks,
            renamed: FxHashMap::default(),
            is_async: lower.func.is_async,
            reg_count: lower.reg_count,
            param_regs: lower.param_regs,
            ret_reg: lower.ret_reg,
            entry: lower.entry,
            block_map: lower.block_map,
        }
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

impl From<LirFunction> for VMFunction {
    fn from(value: LirFunction) -> Self {
        FunctionLowering::lower(value)
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
        parameters: Vec<PotentialFfiDataType>,
        return_type: PotentialFfiDataType,
    },
}

impl Display for VMLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int(x) => write!(f, "{x}"),
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
        literal: u8,
    },
    LoadGlobal {
        dst: Reg,
        name: u8,
    },
    MoveGlobal {
        dst: Reg,
        name: u8,
    },
    DropGlobal {
        name: u8,
    },
    StoreGlobal {
        name: u8,
        src: Reg,
    },
    SetLocalName {
        name: u8,
        src: Reg,
    },
    LoadGlobalRef {
        dst: Reg,
        name: u8,
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
        layout: u8,
        fields: Vec<Reg>,
    },
    Enum {
        dst: Reg,
        name: u8,
        variant: u8,
        payload: Option<Reg>,
    },
    Call {
        dst: Reg,
        callee: Reg,
        args: Vec<Reg>,
    },
    LoadMember {
        dst: Reg,
        value: Reg,
        member: u8,
    },
    SetMember {
        target: Reg,
        member: u8,
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
            } => {
                write!(f, "%r{dst} = %r{src} AS {data_type}")
            }
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

#[derive(Clone)]
struct BlockInfo {
    in_map: FxHashMap<String, Reg>,
    out_map: FxHashMap<String, Reg>,
    phi_for: FxHashMap<String, Reg>,
    phis: Vec<PhiNode>,
}

impl BlockInfo {
    fn new() -> Self {
        Self {
            in_map: FxHashMap::default(),
            out_map: FxHashMap::default(),
            phi_for: FxHashMap::default(),
            phis: Vec::new(),
        }
    }
}

struct FunctionLowering {
    func: LirFunction,
    blocks: Vec<VMBlock>,
    block_map: FxHashMap<BlockId, usize>,
    preds: Vec<Vec<BlockId>>,
    infos: Vec<BlockInfo>,
    reg_count: Reg,
    param_regs: Vec<Reg>,
    locals: FxHashSet<String>,
    captures: FxHashSet<String>,
    entry: BlockId,
    null_reg: Reg,
    ret_reg: Reg,
    is_global: bool,
    assign_regs: Vec<Vec<Option<Reg>>>,
}

impl FunctionLowering {
    fn lower(func: LirFunction) -> VMFunction {
        let mut lower = Self::new(func, false);
        lower.build_cfg();
        lower.build_ssa();
        lower.emit_blocks();

        VMFunction {
            name: lower.func.name.clone(),
            params: lower.func.params.iter().map(|(n, _)| n.clone()).collect(),
            captures: lower.func.captures.iter().map(|(n, _)| n.clone()).collect(),
            returns_value: lower.func.return_type
                != ParserDataType::new(Span::default(), ParserInnerType::Null),
            blocks: lower.blocks,
            renamed: FxHashMap::default(),
            is_async: lower.func.is_async,
            reg_count: lower.reg_count,
            param_regs: lower.param_regs,
            ret_reg: lower.ret_reg,
            entry: lower.entry,
            block_map: lower.block_map,
        }
    }

    fn new(func: LirFunction, is_global: bool) -> Self {
        let entry = func.blocks.first().map(|b| b.id).unwrap_or(BlockId(0));
        let mut block_map = FxHashMap::default();
        for (idx, block) in func.blocks.iter().enumerate() {
            block_map.insert(block.id, idx);
        }

        let mut locals = FxHashSet::default();
        if !is_global {
            for (name, _) in &func.params {
                locals.insert(name.clone());
            }
            for block in &func.blocks {
                for instr in &block.instructions {
                    match &instr.node_type {
                        LirNodeType::Declare { dest, .. } => {
                            locals.insert(dest.clone());
                        }
                        LirNodeType::Assign {
                            dest: LirLValue::Var(name),
                            ..
                        } => {
                            locals.insert(name.clone());
                        }
                        _ => {}
                    }
                }
            }
        }

        let captures: FxHashSet<String> = func.captures.iter().map(|(n, _)| n.clone()).collect();

        let mut reg_count: Reg = 0;
        let mut param_regs = Vec::new();
        for _ in &func.params {
            let r = reg_count;
            reg_count += 1;
            param_regs.push(r);
        }
        let null_reg = reg_count;
        reg_count += 1;
        let ret_reg = reg_count;
        reg_count += 1;

        let mut assign_regs: Vec<Vec<Option<Reg>>> = Vec::new();
        for block in &func.blocks {
            let mut regs = vec![None; block.instructions.len()];
            for (idx, instr) in block.instructions.iter().enumerate() {
                if let Some(name) = assigned_local_name(&instr.node_type) {
                    if !captures.contains(name) && locals.contains(name) {
                        let r = reg_count;
                        reg_count += 1;
                        regs[idx] = Some(r);
                    }
                }
            }
            assign_regs.push(regs);
        }

        Self {
            func,
            blocks: Vec::new(),
            block_map,
            preds: Vec::new(),
            infos: Vec::new(),
            reg_count,
            param_regs,
            locals,
            captures,
            entry,
            null_reg,
            ret_reg,
            is_global,
            assign_regs,
        }
    }

    fn build_cfg(&mut self) {
        let block_len = self.func.blocks.len();
        self.preds = vec![Vec::new(); block_len];
        self.infos = vec![BlockInfo::new(); block_len];

        for block in &self.func.blocks {
            let idx = self.block_map[&block.id];
            if let Some(term) = block.terminator.as_ref() {
                match term {
                    LirTerminator::Jump { target, .. } => {
                        if let Some(target_idx) = self.block_map.get(target) {
                            self.preds[*target_idx].push(block.id);
                        }
                    }
                    LirTerminator::Branch {
                        then_block,
                        else_block,
                        ..
                    } => {
                        if let Some(target_idx) = self.block_map.get(then_block) {
                            self.preds[*target_idx].push(block.id);
                        }
                        if let Some(target_idx) = self.block_map.get(else_block) {
                            self.preds[*target_idx].push(block.id);
                        }
                    }
                    LirTerminator::Return { .. } => {}
                }
            }
            if idx == 0 {
                self.preds[idx].push(BlockId(u32::MAX));
            }
        }
    }

    fn build_ssa(&mut self) {
        let mut changed = true;
        while changed {
            changed = false;
            for idx in 0..self.func.blocks.len() {
                let mut phi_for = self.infos[idx].phi_for.clone();
                let mut phis = self.infos[idx].phis.clone();
                let incoming = self.merge_incoming(idx, &mut phi_for, &mut phis);

                if incoming != self.infos[idx].in_map {
                    self.infos[idx].in_map = incoming.clone();
                    changed = true;
                }

                let mut out = incoming;
                let instructions = self.func.blocks[idx].instructions.clone();
                for (instr_idx, instr) in instructions.iter().enumerate() {
                    if let Some(name) = assigned_local_name(&instr.node_type) {
                        if !self.captures.contains(name) {
                            if let Some(reg) = self.assign_regs[idx].get(instr_idx).and_then(|r| *r)
                            {
                                out.insert(name.to_string(), reg);
                            }
                        }
                    }
                }

                if out != self.infos[idx].out_map {
                    self.infos[idx].out_map = out;
                    changed = true;
                }
                self.infos[idx].phi_for = phi_for;
                self.infos[idx].phis = phis;
            }
        }
    }

    fn merge_incoming(
        &mut self,
        idx: usize,
        phi_for: &mut FxHashMap<String, Reg>,
        phis: &mut Vec<PhiNode>,
    ) -> FxHashMap<String, Reg> {
        let mut incoming: FxHashMap<String, Reg> = FxHashMap::default();
        let preds = self.preds.get(idx).cloned().unwrap_or_default();

        if preds.len() == 1 && preds[0].0 == u32::MAX {
            for (name, reg) in self
                .func
                .params
                .iter()
                .map(|(n, _)| n)
                .zip(self.param_regs.iter().copied())
            {
                incoming.insert(name.clone(), reg);
            }
            return incoming;
        }

        let locals: Vec<String> = self.locals.iter().cloned().collect();
        for var in locals.iter() {
            let mut sources: Vec<(BlockId, Reg)> = Vec::new();
            let mut reg_opt: Option<Reg> = None;

            for pred in &preds {
                if pred.0 == u32::MAX {
                    continue;
                }
                let pred_idx = self.block_map[pred];
                let pred_info = &self.infos[pred_idx];
                let reg = pred_info.out_map.get(var).copied().unwrap_or(self.null_reg);
                sources.push((*pred, reg));
                reg_opt = match reg_opt {
                    None => Some(reg),
                    Some(existing) if existing == reg => Some(existing),
                    Some(_) => Some(Reg::MAX),
                };
            }

            if sources.is_empty() {
                continue;
            }

            let reg = match reg_opt {
                Some(r) if r != Reg::MAX => r,
                _ => {
                    if let Some(existing) = phi_for.get(var).copied() {
                        existing
                    } else {
                        let new_reg = self.alloc_reg();
                        phi_for.insert(var.clone(), new_reg);
                        new_reg
                    }
                }
            };

            if let Some(phi_reg) = phi_for.get(var).copied() {
                let phi = PhiNode {
                    dest: phi_reg,
                    sources: sources.clone(),
                };
                let pos = phis.iter().position(|p| p.dest == phi_reg);
                if let Some(i) = pos {
                    phis[i] = phi;
                } else {
                    phis.push(phi);
                }
            }

            incoming.insert(var.clone(), reg);
        }

        incoming
    }

    fn alloc_reg(&mut self) -> Reg {
        let r = self.reg_count;
        self.reg_count += 1;
        r
    }

    fn emit_blocks(&mut self) {
        for block in &self.func.blocks {
            let idx = self.block_map[&block.id];
            let info = self.infos[idx].clone();
            let mut out = VMBlock {
                id: block.id,
                instructions: Vec::new(),
                instruction_spans: Vec::new(),
                local_literals: Vec::new(),
                local_strings: Vec::new(),
                aggregate_layouts: Vec::new(),
                phis: info.phis.clone(),
            };

            let mut ctx = BlockLoweringCtx {
                block: &mut out,
                reg_count: &mut self.reg_count,
                locals: self.locals.clone(),
                captures: self.captures.clone(),
                map: info.in_map.clone(),
                null_reg: self.null_reg,
                ret_reg: self.ret_reg,
                is_global: self.is_global,
                string_map: FxHashMap::default(),
                int_literals: FxHashMap::default(),
                float_literals: FxHashMap::default(),
                char_literals: FxHashMap::default(),
                string_literals: FxHashMap::default(),
            };

            if block.id == self.entry {
                let lit = ctx.add_literal(VMLiteral::Null);
                ctx.emit(
                    VMInstruction::LoadLiteral {
                        dst: self.null_reg,
                        literal: lit,
                    },
                    Span::default(),
                );
                ctx.emit(
                    VMInstruction::Copy {
                        dst: self.ret_reg,
                        src: self.null_reg,
                    },
                    Span::default(),
                );
            }

            let ret_from_body =
                block
                    .instructions
                    .iter()
                    .enumerate()
                    .rev()
                    .find_map(|(i, instr)| {
                        if is_ret_candidate(&instr.node_type) {
                            Some(i)
                        } else {
                            None
                        }
                    });
            let ret_from_body_non_null = ret_from_body.and_then(|i| {
                let node = &block.instructions[i].node_type;
                if is_null_literal(node) { None } else { Some(i) }
            });
            let ret_idx = match block.terminator {
                Some(LirTerminator::Jump { .. }) => ret_from_body,
                Some(LirTerminator::Return { ref value, .. }) => match value {
                    None => ret_from_body_non_null,
                    Some(LirNodeType::Drop(_)) => ret_from_body_non_null,
                    _ => None,
                },
                None => ret_from_body,
                _ => None,
            };

            for (instr_idx, instr) in block.instructions.iter().enumerate() {
                let assigned = self.assign_regs[idx].get(instr_idx).and_then(|r| *r);
                let set_ret = ret_idx == Some(instr_idx);
                ctx.lower_instr(instr.clone(), assigned, set_ret);
            }

            if let Some(term) = block.terminator.clone() {
                ctx.lower_terminator(term);
            }

            self.blocks.push(out);
        }
    }
}

fn assigned_local_name(node: &LirNodeType) -> Option<&str> {
    match node {
        LirNodeType::Declare { dest, .. } => Some(dest.as_str()),
        LirNodeType::Assign {
            dest: LirLValue::Var(name),
            ..
        } => Some(name.as_str()),
        _ => None,
    }
}

fn is_ret_candidate(node: &LirNodeType) -> bool {
    !matches!(
        node,
        LirNodeType::Declare { .. }
            | LirNodeType::Assign { .. }
            | LirNodeType::Drop(_)
            | LirNodeType::Noop
    )
}

fn is_null_literal(node: &LirNodeType) -> bool {
    matches!(node, LirNodeType::Literal(LirLiteral::Null))
}

struct BlockLoweringCtx<'a> {
    block: &'a mut VMBlock,
    reg_count: &'a mut Reg,
    locals: FxHashSet<String>,
    captures: FxHashSet<String>,
    map: FxHashMap<String, Reg>,
    null_reg: Reg,
    ret_reg: Reg,
    is_global: bool,
    string_map: FxHashMap<String, u8>,
    int_literals: FxHashMap<i64, u8>,
    float_literals: FxHashMap<u64, u8>,
    char_literals: FxHashMap<char, u8>,
    string_literals: FxHashMap<String, u8>,
}

impl<'a> BlockLoweringCtx<'a> {
    fn alloc_reg(&mut self) -> Reg {
        let r = *self.reg_count;
        *self.reg_count += 1;
        r
    }

    fn add_literal(&mut self, lit: VMLiteral) -> u8 {
        match &lit {
            VMLiteral::Int(x) => {
                if let Some(idx) = self.int_literals.get(x).copied() {
                    return idx;
                }
            }
            VMLiteral::Float(x) => {
                let bits = x.to_bits();
                if let Some(idx) = self.float_literals.get(&bits).copied() {
                    return idx;
                }
            }
            VMLiteral::Char(x) => {
                if let Some(idx) = self.char_literals.get(x).copied() {
                    return idx;
                }
            }
            VMLiteral::String(x) => {
                if let Some(idx) = self.string_literals.get(x).copied() {
                    return idx;
                }
            }
            _ => {}
        }
        self.block.local_literals.push(lit);
        let idx = (self.block.local_literals.len() - 1) as u8;
        match &self.block.local_literals[idx as usize] {
            VMLiteral::Int(x) => {
                self.int_literals.insert(*x, idx);
            }
            VMLiteral::Float(x) => {
                self.float_literals.insert(x.to_bits(), idx);
            }
            VMLiteral::Char(x) => {
                self.char_literals.insert(*x, idx);
            }
            VMLiteral::String(x) => {
                self.string_literals.insert(x.clone(), idx);
            }
            _ => {}
        }
        idx
    }

    fn add_string(&mut self, text: String) -> u8 {
        if let Some(idx) = self.string_map.get(&text).copied() {
            return idx;
        }
        self.block.local_strings.push(text);
        let idx = (self.block.local_strings.len() - 1) as u8;
        if let Some(text) = self.block.local_strings.get(idx as usize) {
            self.string_map.insert(text.clone(), idx);
        }
        idx
    }

    fn emit(&mut self, instr: VMInstruction, span: Span) {
        self.block.instructions.push(instr);
        self.block.instruction_spans.push(span);
    }

    fn lower_instr(&mut self, node: LirInstr, assigned: Option<Reg>, set_ret: bool) {
        match node.node_type {
            LirNodeType::Noop => {}
            LirNodeType::Declare { dest, value } => {
                let reg = self.lower_node(*value, node.span);
                if !self.is_global && !self.captures.contains(&dest) {
                    let target = assigned.unwrap_or(reg);
                    if target != reg {
                        self.emit(
                            VMInstruction::Copy {
                                dst: target,
                                src: reg,
                            },
                            node.span,
                        );
                    }
                    let name_idx = self.add_string(dest.clone());
                    self.map.insert(dest, target);
                    self.emit(
                        VMInstruction::SetLocalName {
                            name: name_idx,
                            src: target,
                        },
                        node.span,
                    );
                } else {
                    let name = self.add_string(dest);
                    self.emit(VMInstruction::StoreGlobal { name, src: reg }, node.span);
                }
            }
            LirNodeType::Assign { dest, value } => match dest {
                LirLValue::Var(dest) => {
                    let reg = self.lower_node(*value, node.span);
                    if !self.is_global
                        && !self.captures.contains(&dest)
                        && self.locals.contains(&dest)
                    {
                        let target = assigned.unwrap_or(reg);
                        if target != reg {
                            self.emit(
                                VMInstruction::Copy {
                                    dst: target,
                                    src: reg,
                                },
                                node.span,
                            );
                        }
                        let name_idx = self.add_string(dest.clone());
                        self.map.insert(dest, target);
                        self.emit(
                            VMInstruction::SetLocalName {
                                name: name_idx,
                                src: target,
                            },
                            node.span,
                        );
                    } else {
                        let name = self.add_string(dest);
                        self.emit(VMInstruction::StoreGlobal { name, src: reg }, node.span);
                    }
                }
                LirLValue::Ptr(ptr) => {
                    let value_reg = self.lower_node(*value, node.span);
                    match *ptr {
                        LirNodeType::Member(base, member) => {
                            let base_reg = self.lower_node(*base, node.span);
                            let member = self.add_string(member);
                            self.emit(
                                VMInstruction::SetMember {
                                    target: base_reg,
                                    member,
                                    value: value_reg,
                                },
                                node.span,
                            );
                        }
                        LirNodeType::Index(base, index) => {
                            let index_reg = self.lower_node(*index, node.span);
                            let base_reg = self.lower_node(*base, node.span);
                            self.emit(
                                VMInstruction::SetIndex {
                                    target: base_reg,
                                    index: index_reg,
                                    value: value_reg,
                                },
                                node.span,
                            );
                        }
                        other => {
                            let target_reg = self.lower_node(other, node.span);
                            self.emit(
                                VMInstruction::SetRef {
                                    target: target_reg,
                                    value: value_reg,
                                },
                                node.span,
                            );
                        }
                    }
                }
            },
            other => {
                let reg = self.lower_node(other, node.span);
                if set_ret {
                    if reg != self.ret_reg {
                        self.emit(
                            VMInstruction::Copy {
                                dst: self.ret_reg,
                                src: reg,
                            },
                            node.span,
                        );
                    }
                }
            }
        }
    }

    fn lower_node(&mut self, node: LirNodeType, span: Span) -> Reg {
        match node {
            LirNodeType::Noop => self.null_reg,
            LirNodeType::Literal(x) => {
                if matches!(x, LirLiteral::Null) {
                    return self.null_reg;
                }
                let lit = self.add_literal(x.into());
                let dst = self.alloc_reg();
                self.emit(VMInstruction::LoadLiteral { dst, literal: lit }, span);
                dst
            }
            LirNodeType::Call { caller, args } => {
                let mut arg_regs = Vec::with_capacity(args.len());
                for arg in args {
                    arg_regs.push(self.lower_node(arg, span));
                }
                let callee = self.lower_node(*caller, span);
                let dst = self.alloc_reg();
                self.emit(
                    VMInstruction::Call {
                        dst,
                        callee,
                        args: arg_regs,
                    },
                    span,
                );
                dst
            }
            LirNodeType::List { elements, .. } => {
                let mut regs = Vec::with_capacity(elements.len());
                for item in elements {
                    regs.push(self.lower_node(item, span));
                }
                let dst = self.alloc_reg();
                self.emit(VMInstruction::List { dst, items: regs }, span);
                dst
            }
            LirNodeType::Move(name) => {
                if self.captures.contains(&name) || !self.locals.contains(&name) {
                    let idx = self.add_string(name);
                    let dst = self.alloc_reg();
                    self.emit(VMInstruction::MoveGlobal { dst, name: idx }, span);
                    dst
                } else {
                    let reg = self.map.get(&name).copied().unwrap_or(self.null_reg);
                    self.map.insert(name, self.null_reg);
                    reg
                }
            }
            LirNodeType::Drop(name) => {
                if self.captures.contains(&name) || !self.locals.contains(&name) {
                    let idx = self.add_string(name);
                    self.emit(VMInstruction::DropGlobal { name: idx }, span);
                } else {
                    self.map.insert(name, self.null_reg);
                }
                self.null_reg
            }
            LirNodeType::Load(name) => {
                if !self.captures.contains(&name) && self.locals.contains(&name) {
                    self.map.get(&name).copied().unwrap_or(self.null_reg)
                } else {
                    let idx = self.add_string(name);
                    let dst = self.alloc_reg();
                    self.emit(VMInstruction::LoadGlobal { dst, name: idx }, span);
                    dst
                }
            }
            LirNodeType::Aggregate { name, fields } => {
                let mut layout = Vec::new();
                let mut values = Vec::new();
                for (k, item) in fields.0 {
                    values.push(self.lower_node(item, span));
                    layout.push(k.to_string());
                }
                self.block.aggregate_layouts.push(AggregateLayout {
                    name,
                    members: layout,
                });
                let index = self.block.aggregate_layouts.len() - 1;
                let dst = self.alloc_reg();
                self.emit(
                    VMInstruction::Aggregate {
                        dst,
                        layout: index as u8,
                        fields: values,
                    },
                    span,
                );
                dst
            }
            LirNodeType::Boolean {
                left,
                right,
                operator,
            } => {
                let left = self.lower_node(*left, span);
                let right = self.lower_node(*right, span);
                let dst = self.alloc_reg();
                self.emit(
                    VMInstruction::Boolean {
                        dst,
                        op: operator,
                        left,
                        right,
                    },
                    span,
                );
                dst
            }
            LirNodeType::Comparison {
                left,
                right,
                operator,
            } => {
                let left = self.lower_node(*left, span);
                let right = self.lower_node(*right, span);
                let dst = self.alloc_reg();
                self.emit(
                    VMInstruction::Comparison {
                        dst,
                        op: operator,
                        left,
                        right,
                    },
                    span,
                );
                dst
            }
            LirNodeType::Binary {
                left,
                right,
                operator,
            } => {
                let left = self.lower_node(*left, span);
                let right = self.lower_node(*right, span);
                let dst = self.alloc_reg();
                self.emit(VMInstruction::AccLoad { src: left }, span);
                self.emit(
                    VMInstruction::AccBinary {
                        op: operator,
                        right,
                    },
                    span,
                );
                self.emit(VMInstruction::AccStore { dst }, span);
                dst
            }
            LirNodeType::Range {
                from,
                to,
                inclusive,
            } => {
                let from = self.lower_node(*from, span);
                let to = self.lower_node(*to, span);
                let dst = self.alloc_reg();
                self.emit(
                    VMInstruction::Range {
                        dst,
                        from,
                        to,
                        inclusive,
                    },
                    span,
                );
                dst
            }
            LirNodeType::Deref(x) => {
                let value = self.lower_node(*x, span);
                let dst = self.alloc_reg();
                self.emit(VMInstruction::Deref { dst, value }, span);
                dst
            }
            LirNodeType::Ref(x) => match *x {
                LirNodeType::Load(name) => {
                    if !self.captures.contains(&name)
                        && (self.locals.contains(&name) || self.map.contains_key(&name))
                    {
                        let src = self.map.get(&name).copied().unwrap_or(self.null_reg);
                        let dst = self.alloc_reg();
                        self.emit(VMInstruction::LoadRegRef { dst, src }, span);
                        dst
                    } else {
                        let idx = self.add_string(name);
                        let dst = self.alloc_reg();
                        self.emit(VMInstruction::LoadGlobalRef { dst, name: idx }, span);
                        dst
                    }
                }
                other => {
                    let value = self.lower_node(other, span);
                    let dst = self.alloc_reg();
                    self.emit(VMInstruction::Ref { dst, value }, span);
                    dst
                }
            },
            LirNodeType::As(value, data_type) => {
                let src = self.lower_node(*value, span);
                let dst = self.alloc_reg();
                self.emit(
                    VMInstruction::As {
                        dst,
                        src,
                        data_type,
                    },
                    span,
                );
                dst
            }
            LirNodeType::Enum {
                name,
                variant,
                payload,
            } => {
                let payload = payload.map(|v| self.lower_node(*v, span));
                let name = self.add_string(name);
                let dst = self.alloc_reg();
                self.emit(
                    VMInstruction::Enum {
                        dst,
                        name,
                        variant: variant as u8,
                        payload,
                    },
                    span,
                );
                dst
            }
            LirNodeType::Index(value, index) => {
                let index = self.lower_node(*index, span);
                let value = self.lower_node(*value, span);
                let dst = self.alloc_reg();
                self.emit(VMInstruction::Index { dst, value, index }, span);
                dst
            }
            LirNodeType::Member(value, member) => {
                let value = self.lower_node(*value, span);
                let member = self.add_string(member);
                let dst = self.alloc_reg();
                self.emit(VMInstruction::LoadMember { dst, value, member }, span);
                dst
            }
            LirNodeType::Closure { label, captures } => {
                self.block
                    .local_literals
                    .push(VMLiteral::Closure { label, captures });
                let lit = (self.block.local_literals.len() - 1) as u8;
                let dst = self.alloc_reg();
                self.emit(VMInstruction::LoadLiteral { dst, literal: lit }, span);
                dst
            }
            LirNodeType::ExternFunction {
                abi,
                library,
                symbol,
                parameters,
                return_type,
            } => {
                self.block.local_literals.push(VMLiteral::ExternFunction {
                    abi,
                    library,
                    symbol,
                    parameters,
                    return_type,
                });
                let lit = (self.block.local_literals.len() - 1) as u8;
                let dst = self.alloc_reg();
                self.emit(VMInstruction::LoadLiteral { dst, literal: lit }, span);
                dst
            }
            LirNodeType::Assign { .. } | LirNodeType::Declare { .. } => self.null_reg,
        }
    }

    fn lower_terminator(&mut self, node: LirTerminator) {
        match node {
            LirTerminator::Jump { span, target } => {
                self.emit(VMInstruction::Jump(target), span);
            }
            LirTerminator::Branch {
                span,
                condition,
                then_block,
                else_block,
            } => {
                let cond = self.lower_node(condition, span);
                self.emit(
                    VMInstruction::Branch {
                        cond,
                        then_block,
                        else_block,
                    },
                    span,
                );
            }
            LirTerminator::Return { span, value } => {
                let value = match value {
                    Some(LirNodeType::Drop(name)) => {
                        let _ = self.lower_node(LirNodeType::Drop(name), span);
                        Some(self.ret_reg)
                    }
                    Some(v) => Some(self.lower_node(v, span)),
                    None => Some(self.ret_reg),
                };
                self.emit(VMInstruction::Return { value }, span);
            }
        }
    }
}

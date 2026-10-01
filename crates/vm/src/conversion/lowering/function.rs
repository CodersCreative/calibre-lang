use super::ssa::SSABuilder;
use super::*;
use crate::conversion::instructions::{VMInstruction, literals::VMLoadLiteral, registers::VMCopy};
use calibre_lir::{
    MirDataType, VariableKey,
    ast::{LirDeclare, LirLValue},
};
use calibre_parser::Span;
use rustc_hash::FxHashSet;
use tracing::{debug, instrument};
use ustr::UstrMap;

impl VMFunction {
    #[instrument(skip_all, fields(name = %name))]
    pub(crate) fn from_global(name: VariableKey, blocks: Box<[Option<LirBlock>]>) -> Self {
        debug!("lowering global to VM function");
        let func = LirFunction {
            name,
            params: Vec::new().into_boxed_slice(),
            captures: Vec::new().into_boxed_slice(),
            return_type: MirDataType::Null,
            blocks,
            pure: false,
            memo: false,
            referenced_params: 0,
            memo_params: 0,
        };

        let mut lower = FunctionLowering::new(func, true);
        lower.build_cfg();
        lower.build_ssa();
        lower.emit_blocks();
        debug!("global lowering completed");

        lower.blocks =
            FunctionLowering::optimize_blocks(lower.blocks, lower.entry, &lower.block_map);

        VMFunction {
            name: lower.func.name,
            params: Vec::new().into_boxed_slice(),
            captures: Vec::new().into_boxed_slice(),
            returns_value: false,
            blocks: lower.blocks,
            renamed: UstrMap::default(),
            reg_count: lower.reg_count,
            param_regs: lower.param_regs,
            ret_reg: lower.ret_reg,
            entry: lower.entry,
            block_map: lower.block_map,
            param_names: FxHashSet::default(),
            pure: false,
            memo: false,
            memo_params: 0,
            referenced_params: 0,
        }
    }
}

impl From<LirFunction> for VMFunction {
    fn from(value: LirFunction) -> Self {
        FunctionLowering::lower(value)
    }
}

pub(crate) struct FunctionLowering {
    func: LirFunction,
    blocks: Box<[Option<VMBlock>]>,
    block_map: FxHashMap<BlockId, usize>,
    ssa_builder: SSABuilder,
    reg_count: Reg,
    param_regs: Vec<Reg>,
    captures: FxHashSet<VariableKey>,
    entry: BlockId,
    null_reg: Reg,
    ret_reg: Reg,
    is_global: bool,
    referenced_variables: FxHashSet<VariableKey>,
    big_consts: Consts,
}

impl FunctionLowering {
    #[instrument(skip_all, fields(function = %func.name))]
    fn lower(func: LirFunction) -> VMFunction {
        debug!("lowering LIR function to VM function");
        let mut lower = Self::new(func, false);
        lower.build_cfg();
        lower.build_ssa();
        lower.emit_blocks();

        debug!("function lowering completed");

        let param_names: FxHashSet<VariableKey> =
            lower.func.params.iter().map(|(n, _)| n.clone()).collect();

        lower.blocks =
            FunctionLowering::optimize_blocks(lower.blocks, lower.entry, &lower.block_map);

        VMFunction {
            name: lower.func.name,
            params: lower
                .func
                .params
                .iter()
                .map(|(n, _)| n.clone())
                .collect::<Vec<_>>()
                .into_boxed_slice(),
            captures: lower
                .func
                .captures
                .iter()
                .map(|(n, _)| n.clone())
                .collect::<Vec<_>>()
                .into_boxed_slice(),
            returns_value: !lower.func.return_type.is_null(),
            blocks: lower.blocks,
            renamed: UstrMap::default(),
            reg_count: lower.reg_count,
            param_regs: lower.param_regs,
            ret_reg: lower.ret_reg,
            entry: lower.entry,
            block_map: lower.block_map,
            param_names,
            pure: lower.func.pure,
            memo: lower.func.memo,
            memo_params: lower.func.memo_params,
            referenced_params: lower.func.referenced_params,
        }
    }

    fn new(func: LirFunction, is_global: bool) -> Self {
        let entry = func
            .blocks
            .first()
            .and_then(|b| b.as_ref())
            .map(|b| b.id)
            .unwrap_or(BlockId(0));

        let blocks = (0..func.blocks.len()).map(|_| None).collect();
        let block_map: FxHashMap<_, _> = func
            .blocks
            .iter()
            .enumerate()
            .filter_map(|(idx, block)| block.as_ref().map(|x| (x.id, idx)))
            .collect();

        let mut locals = FxHashSet::default();
        if !is_global {
            locals.extend(func.params.iter().map(|(name, _)| name.clone()));

            for instr in func.blocks.iter().flatten().flat_map(|b| &b.instructions) {
                if let LirNodeType::Declare(LirDeclare { dest, .. }) = &instr.node_type {
                    locals.insert(dest.clone());
                }
            }
        }

        let captures: FxHashSet<VariableKey> =
            func.captures.iter().map(|(n, _)| n.clone()).collect();

        let param_regs: Vec<Reg> = (0..func.params.len() as Reg).collect();
        let mut reg_count = param_regs.len() as Reg;

        let null_reg = reg_count;
        reg_count += 1;
        let ret_reg = reg_count;
        reg_count += 1;

        let assign_regs: Vec<Vec<Option<Reg>>> = func
            .blocks
            .iter()
            .filter_map(|block| block.as_ref())
            .map(|block| {
                block
                    .instructions
                    .iter()
                    .map(|instr| {
                        instr
                            .node_type
                            .local_name()
                            .filter(|name| locals.contains(name))
                            .map(|_| {
                                let r = reg_count;
                                reg_count += 1;
                                r
                            })
                    })
                    .collect()
            })
            .collect();

        let referenced_variables: FxHashSet<VariableKey> = func
            .blocks
            .iter()
            .flatten()
            .flat_map(|block| &block.instructions)
            .filter_map(|node| match &node.node_type {
                LirNodeType::Declare(decl) if decl.is_referenced => Some(decl.dest.clone()),
                LirNodeType::Assign(assign) => match &assign.dest {
                    LirLValue::Var(name) => Some(name.clone()),
                    _ => None,
                },
                _ => None,
            })
            .collect();

        let ssa_builder = SSABuilder::new(
            block_map.clone(),
            locals,
            param_regs.clone(),
            null_reg,
            assign_regs.clone(),
            reg_count,
        );

        Self {
            func,
            blocks,
            block_map,
            ssa_builder,
            reg_count,
            param_regs,
            captures,
            entry,
            null_reg,
            ret_reg,
            is_global,
            referenced_variables,
            big_consts: Consts::new().unwrap(),
        }
    }

    fn build_cfg(&mut self) {
        self.ssa_builder.build_cfg(&self.func.blocks, self.entry);
    }

    fn build_ssa(&mut self) {
        self.ssa_builder.build(&self.func.blocks, &self.func.params);
        self.reg_count = self.ssa_builder.reg_count();
    }

    fn emit_blocks(&mut self) {
        for (i, block) in self.func.blocks.iter().enumerate() {
            if let Some(block) = block {
                let idx = self.block_map[&block.id];
                let info = self.ssa_builder.get_block_info(idx).clone();

                let mut out = VMBlock {
                    id: block.id,
                    instructions: Vec::new(),
                    instruction_spans: Vec::new(),
                    local_literals: Vec::new(),
                    local_strings: Vec::new(),
                    local_types: Vec::new(),
                    local_variables: Vec::new(),
                    aggregate_layouts: Vec::new(),
                    edge_copies: Vec::new(),
                    phis: info.phis.clone(),
                };

                let mut map = info.in_map.clone();
                map.retain(|k, _| !self.referenced_variables.contains(k));

                let mut ctx = BlockLoweringCtx {
                    block: &mut out,
                    reg_count: &mut self.reg_count,
                    captures: self.captures.clone(),
                    map,
                    referenced_variables: self.referenced_variables.clone(),
                    null_reg: self.null_reg,
                    ret_reg: self.ret_reg,
                    is_global: self.is_global,
                    string_map: UstrMap::default(),
                    int_literals: FxHashMap::default(),
                    uint_literals: FxHashMap::default(),
                    float_literals: FxHashMap::default(),
                    char_literals: FxHashMap::default(),
                    vars_map: FxHashMap::default(),
                    type_map: FxHashMap::default(),
                    string_literals: UstrMap::default(),
                    current_fn_name: self.func.name.clone(),
                    big_consts: &mut self.big_consts,
                };

                if block.id == self.entry {
                    let lit = ctx.add_literal(VMLiteral::Null);
                    ctx.emit(
                        VMInstruction::LoadLiteral(VMLoadLiteral {
                            dst: self.null_reg,
                            literal: lit,
                        }),
                        Span::default(),
                    );
                }

                let ret_from_body = block
                    .instructions
                    .iter()
                    .rposition(|instr| instr.node_type.is_return_candidate());

                let ret_from_body_non_null =
                    ret_from_body.filter(|&i| !block.instructions[i].node_type.is_null());

                let ret_idx = match &block.terminator {
                    Some(LirTerminator::Jump { .. }) | None => ret_from_body,
                    Some(LirTerminator::Return {
                        value: None | Some(LirNodeType::Drop(_)),
                        ..
                    }) => ret_from_body_non_null,
                    _ => None,
                };

                for (instr_idx, instr) in block.instructions.iter().enumerate() {
                    let assigned = self.ssa_builder.assign_regs()[idx]
                        .get(instr_idx)
                        .copied()
                        .flatten();

                    let set_ret = ret_idx == Some(instr_idx);
                    ctx.lower_instr(instr.clone(), assigned, set_ret);
                }

                if let Some(term) = block.terminator.clone() {
                    ctx.lower_terminator(term);
                }

                self.blocks[i] = Some(out);
            }
        }

        self.lower_edge_copy_plans();
    }

    fn lower_edge_copy_plans(&mut self) {
        let mut edge_moves: FxHashMap<(usize, BlockId), Vec<VMCopy>> = FxHashMap::default();

        for destination in self.blocks.iter().flatten() {
            for phi in &destination.phis {
                for (prev, source) in &phi.sources {
                    if let Some(&idx) = self.block_map.get(prev) {
                        edge_moves
                            .entry((idx, destination.id))
                            .or_default()
                            .push(VMCopy {
                                dst: phi.dest,
                                src: *source,
                            });
                    }
                }
            }
        }

        for ((predecessor_idx, target), copies) in edge_moves {
            let normalized = self.normalize_parallel_copies(copies);

            if let Some(block) = &mut self.blocks[predecessor_idx] {
                block.edge_copies.push(EdgeCopy {
                    target,
                    copies: normalized.into_boxed_slice(),
                });
            }
        }

        for block in self.blocks.iter_mut().flatten() {
            block.edge_copies.sort_unstable_by_key(|plan| plan.target.0);
            block.phis.clear();
        }
    }

    fn normalize_parallel_copies(&mut self, copies: Vec<VMCopy>) -> Vec<VMCopy> {
        let mut pending: Vec<VMCopy> = copies
            .into_iter()
            .filter(|copy| copy.dst != copy.src)
            .collect();

        let mut normalized = Vec::with_capacity(pending.len());

        while !pending.is_empty() {
            if let Some(index) = pending
                .iter()
                .position(|copy| !pending.iter().any(|other| other.src == copy.dst))
            {
                normalized.push(pending.swap_remove(index));
                continue;
            }

            let dst = pending[0].dst;
            let temp = self.reg_count;
            self.reg_count = self.reg_count.saturating_add(1);

            normalized.push(VMCopy {
                dst: temp,
                src: dst,
            });

            for copy in &mut pending {
                if copy.src == dst {
                    copy.src = temp;
                }
            }
        }

        normalized
    }
}

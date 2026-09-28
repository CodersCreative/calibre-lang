use super::super::ir::{PhiNode, Reg};
use calibre_lir::ast::{BlockId, LirBlock, LirTerminator};
use calibre_parser::ast::types::ParserDataType;
use rustc_hash::FxHashMap;
use ustr::{Ustr, UstrMap, UstrSet};

#[derive(Clone, Default)]
pub struct SSABlockInfo {
    // Names to registers at entry
    pub in_map: UstrMap<Reg>,
    // Names to registers at exit
    pub out_map: UstrMap<Reg>,
    // Names to phi nodes
    pub phi_for: UstrMap<Reg>,
    pub phis: Vec<PhiNode>,
}

pub struct SSABuilder {
    block_map: FxHashMap<BlockId, usize>,
    preds: Vec<Vec<BlockId>>,
    infos: Vec<SSABlockInfo>,
    reg_count: Reg,
    locals: UstrSet,
    param_regs: Vec<Reg>,
    null_reg: Reg,
    assign_regs: Vec<Vec<Option<Reg>>>,
}

impl SSABuilder {
    pub fn new(
        block_map: FxHashMap<BlockId, usize>,
        locals: UstrSet,
        param_regs: Vec<Reg>,
        null_reg: Reg,
        assign_regs: Vec<Vec<Option<Reg>>>,
        initial_reg_count: Reg,
    ) -> Self {
        Self {
            block_map,
            preds: Vec::new(),
            infos: Vec::new(),
            reg_count: initial_reg_count,
            locals,
            param_regs,
            null_reg,
            assign_regs,
        }
    }

    pub fn build_cfg(&mut self, blocks: &[LirBlock], entry: BlockId) {
        let block_len = blocks.len();
        self.preds = vec![Vec::new(); block_len];
        self.infos = vec![SSABlockInfo::default(); block_len];

        for block in blocks {
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
            if block.id == entry {
                self.preds[idx].push(BlockId(u32::MAX));
            }
        }
    }

    pub fn build(&mut self, blocks: &[LirBlock], params: &[(Ustr, ParserDataType)]) {
        let mut scratch_in = UstrMap::default();
        let mut scratch_out = UstrMap::default();
        let mut changed = true;

        while changed {
            changed = false;
            for idx in 0..blocks.len() {
                let mut current_info = std::mem::take(&mut self.infos[idx]);
                let preds = std::mem::take(&mut self.preds[idx]);

                scratch_in.clear();

                if preds.len() == 1 && preds[0].0 == u32::MAX {
                    for ((name, _), &reg) in params.iter().zip(self.param_regs.iter()) {
                        scratch_in.insert(*name, reg);
                    }
                } else {
                    let locals = std::mem::take(&mut self.locals);
                    for &var in &locals {
                        let mut sources = Vec::with_capacity(preds.len());
                        let mut all_same = true;
                        let mut first_reg = None;

                        for pred in &preds {
                            if pred.0 == u32::MAX {
                                continue;
                            }
                            let pred_idx = self.block_map[pred];
                            let reg = self.infos[pred_idx]
                                .out_map
                                .get(&var)
                                .copied()
                                .unwrap_or(self.null_reg);

                            sources.push((*pred, reg));

                            if let Some(first) = first_reg {
                                if first != reg {
                                    all_same = false;
                                }
                            } else {
                                first_reg = Some(reg);
                            }
                        }

                        if sources.is_empty() {
                            continue;
                        }

                        let reg = if all_same {
                            first_reg.unwrap()
                        } else {
                            *current_info
                                .phi_for
                                .entry(var)
                                .or_insert_with(|| self.alloc_reg())
                        };

                        if let Some(&phi_reg) = current_info.phi_for.get(&var) {
                            sources.sort_unstable_by_key(|(block, _)| block.0);

                            if let Some(p) =
                                current_info.phis.iter_mut().find(|p| p.dest == phi_reg)
                            {
                                p.sources = sources;
                            } else {
                                current_info.phis.push(PhiNode {
                                    dest: phi_reg,
                                    sources,
                                    name: Some(var),
                                });
                            }
                        }

                        scratch_in.insert(var, reg);
                    }
                    let _ = std::mem::replace(&mut self.locals, locals);
                }

                scratch_out.clone_from(&scratch_in);

                for (instr_idx, instr) in blocks[idx].instructions.iter().enumerate() {
                    if let Some(name) = instr.node_type.local_name() {
                        let reg = if let Some(r) = self.assign_regs[idx][instr_idx] {
                            r
                        } else {
                            let new_reg = self.alloc_reg();
                            self.assign_regs[idx][instr_idx] = Some(new_reg);
                            new_reg
                        };
                        scratch_out.insert(*name, reg);
                    }
                }

                if scratch_in != current_info.in_map {
                    current_info.in_map.clone_from(&scratch_in);
                    changed = true;
                }

                if scratch_out != current_info.out_map {
                    current_info.out_map.clone_from(&scratch_out);
                    changed = true;
                }

                self.infos[idx] = current_info;
                self.preds[idx] = preds;
            }
        }
    }

    fn alloc_reg(&mut self) -> Reg {
        let r = self.reg_count;
        self.reg_count += 1;
        r
    }

    pub fn get_block_info(&self, idx: usize) -> &SSABlockInfo {
        &self.infos[idx]
    }

    pub fn reg_count(&self) -> Reg {
        self.reg_count
    }

    pub fn assign_regs(&self) -> &Vec<Vec<Option<Reg>>> {
        &self.assign_regs
    }
}

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

    pub fn build_cfg(&mut self, blocks: &[LirBlock], _entry: BlockId) {
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
            if idx == 0 {
                self.preds[idx].push(BlockId(u32::MAX));
            }
        }
    }

    pub fn build(&mut self, blocks: &[LirBlock], params: &[(Ustr, ParserDataType)]) {
        let mut changed = true;
        while changed {
            changed = false;
            for idx in 0..blocks.len() {
                let mut phi_for = self.infos[idx].phi_for.clone();
                let mut phis = self.infos[idx].phis.clone();
                let incoming = self.merge_block_inputs(idx, params, &mut phi_for, &mut phis);

                if incoming != self.infos[idx].in_map {
                    self.infos[idx].in_map = incoming.clone();
                    changed = true;
                }

                let out = self.compute_block_liveness(idx, blocks, incoming);
                if out != self.infos[idx].out_map {
                    self.infos[idx].out_map = out;
                    changed = true;
                }
                self.infos[idx].phi_for = phi_for;
                self.infos[idx].phis = phis;
            }
        }
    }

    fn merge_block_inputs(
        &mut self,
        idx: usize,
        params: &[(Ustr, ParserDataType)],
        phi_for: &mut UstrMap<Reg>,
        phis: &mut Vec<PhiNode>,
    ) -> UstrMap<Reg> {
        let mut incoming: UstrMap<Reg> = UstrMap::default();
        let preds = self.preds.get(idx).cloned().unwrap_or_default();

        if preds.len() == 1 && preds[0].0 == u32::MAX {
            for (name, reg) in params
                .iter()
                .map(|(n, _)| n)
                .zip(self.param_regs.iter().copied())
            {
                incoming.insert(*name, reg);
            }
            return incoming;
        }

        for var in self.locals.clone() {
            let sources = self.compute_variable_sources(&var, &preds);
            if sources.is_empty() {
                continue;
            }

            let reg = self.compute_phi_register(var, &sources, phi_for);
            self.update_phi_nodes(&var, reg, &sources, phi_for, phis);
            incoming.insert(var.clone(), reg);
        }

        incoming
    }

    fn compute_variable_sources(&self, var: &Ustr, preds: &[BlockId]) -> Vec<(BlockId, Reg)> {
        let mut sources: Vec<(BlockId, Reg)> = Vec::new();
        for pred in preds {
            if pred.0 == u32::MAX {
                continue;
            }
            let pred_idx = self.block_map[pred];
            let pred_info = &self.infos[pred_idx];
            let reg = pred_info.out_map.get(var).copied().unwrap_or(self.null_reg);
            sources.push((*pred, reg));
        }
        sources
    }

    fn compute_phi_register(
        &mut self,
        var: Ustr,
        sources: &[(BlockId, Reg)],
        phi_for: &mut UstrMap<Reg>,
    ) -> Reg {
        let reg_opt = sources
            .iter()
            .map(|(_, reg)| *reg)
            .reduce(|acc, reg| if acc == reg { acc } else { Reg::MAX });

        match reg_opt {
            Some(r) if r != Reg::MAX => r,
            _ => {
                if let Some(existing) = phi_for.get(&var).copied() {
                    existing
                } else {
                    let new_reg = self.alloc_reg();
                    phi_for.insert(var, new_reg);
                    new_reg
                }
            }
        }
    }

    fn update_phi_nodes(
        &self,
        var: &Ustr,
        _reg: Reg,
        sources: &[(BlockId, Reg)],
        phi_for: &UstrMap<Reg>,
        phis: &mut Vec<PhiNode>,
    ) {
        if let Some(phi_reg) = phi_for.get(var).copied() {
            let phi = PhiNode {
                dest: phi_reg,
                sources: sources.to_vec(),
                name: Some(*var),
            };
            if let Some(i) = phis.iter().position(|p| p.dest == phi_reg) {
                phis[i] = phi;
            } else {
                phis.push(phi);
            }
        }
    }

    fn compute_block_liveness(
        &mut self,
        idx: usize,
        blocks: &[LirBlock],
        mut incoming: UstrMap<Reg>,
    ) -> UstrMap<Reg> {
        let instructions = blocks[idx].instructions.clone();
        for (instr_idx, instr) in instructions.iter().enumerate() {
            if let Some(name) = instr.node_type.local_name() {
                let _reg = match self.assign_regs[idx].get(instr_idx).and_then(|r| *r) {
                    Some(reg) => reg,
                    None => {
                        let reg = self.alloc_reg();
                        if let Some(slot) = self.assign_regs[idx].get_mut(instr_idx) {
                            *slot = Some(reg);
                        }
                        reg
                    }
                };
                incoming.insert(*name, _reg);
            }
        }
        incoming
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

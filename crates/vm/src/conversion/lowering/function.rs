use super::*;

impl VMFunction {
    pub(crate) fn from_global(name: String, blocks: Vec<LirBlock>) -> Self {
        let func = LirFunction {
            name: name.into_boxed_str(),
            params: Vec::new().into_boxed_slice(),
            captures: Vec::new().into_boxed_slice(),
            return_type: ParserDataType::new(Span::default(), ParserInnerType::Null),
            blocks: blocks.into_boxed_slice(),
        };
        let mut lower = FunctionLowering::new(func, true);
        lower.build_cfg();
        lower.build_ssa();
        lower.emit_blocks();
        VMFunction {
            name: lower.func.name.to_string(),
            params: Vec::new().into_boxed_slice(),
            captures: Vec::new().into_boxed_slice(),
            returns_value: false,
            blocks: lower.blocks.into_boxed_slice(),
            renamed: FxHashMap::default(),
            reg_count: lower.reg_count,
            param_regs: lower.param_regs,
            ret_reg: lower.ret_reg,
            entry: lower.entry,
            block_map: lower.block_map,
        }
    }
}

impl From<LirFunction> for VMFunction {
    fn from(value: LirFunction) -> Self {
        FunctionLowering::lower(value)
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
            name: lower.func.name.to_string(),
            params: lower
                .func
                .params
                .iter()
                .map(|(n, _)| n.to_string())
                .collect::<Vec<_>>()
                .into_boxed_slice(),
            captures: lower
                .func
                .captures
                .iter()
                .map(|(n, _)| n.to_string())
                .collect::<Vec<_>>()
                .into_boxed_slice(),
            returns_value: lower.func.return_type
                != ParserDataType::new(Span::default(), ParserInnerType::Null),
            blocks: lower.blocks.into_boxed_slice(),
            renamed: FxHashMap::default(),
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
                locals.insert(name.to_string());
            }
            for block in &func.blocks {
                for instr in &block.instructions {
                    match &instr.node_type {
                        LirNodeType::Declare { dest, .. } => {
                            locals.insert(dest.to_string());
                        }
                        LirNodeType::Assign {
                            dest: LirLValue::Var(name),
                            ..
                        } => {
                            locals.insert(name.to_string());
                        }
                        _ => {}
                    }
                }
            }
        }

        let captures: FxHashSet<String> =
            func.captures.iter().map(|(n, _)| n.to_string()).collect();

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
                    if locals.contains(name) {
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
                        let reg = match self.assign_regs[idx].get(instr_idx).and_then(|r| *r) {
                            Some(reg) => reg,
                            None => {
                                let reg = self.alloc_reg();
                                if let Some(slot) = self.assign_regs[idx].get_mut(instr_idx) {
                                    *slot = Some(reg);
                                }
                                reg
                            }
                        };
                        out.insert(name.to_string(), reg);
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
                incoming.insert(name.to_string(), reg);
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
                uint_literals: FxHashMap::default(),
                float_literals: FxHashMap::default(),
                char_literals: FxHashMap::default(),
                string_literals: FxHashMap::default(),
                current_fn_name: self.func.name.to_string(),
                current_fn_short: self
                    .func
                    .name
                    .rsplit(':')
                    .next()
                    .unwrap_or(self.func.name.as_ref())
                    .to_string(),
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
        LirNodeType::Declare { dest, .. } => Some(dest.as_ref()),
        LirNodeType::Assign {
            dest: LirLValue::Var(name),
            ..
        } => Some(name.as_ref()),
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

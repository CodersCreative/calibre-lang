use super::ssa::SSABuilder;
use super::*;
use calibre_lir::ast::{LirAssign, LirDeclare};
use tracing::{debug, instrument};
use ustr::{Ustr, UstrMap, UstrSet};

impl VMFunction {
    #[instrument(skip_all, fields(name = %name))]
    pub(crate) fn from_global(name: Ustr, blocks: Vec<LirBlock>) -> Self {
        debug!("lowering global to VM function");
        let func = LirFunction {
            name,
            params: Vec::new().into_boxed_slice(),
            captures: Vec::new().into_boxed_slice(),
            return_type: ParserDataType::new(Span::default(), ParserInnerType::Null),
            blocks: blocks.into_boxed_slice(),
        };
        let mut lower = FunctionLowering::new(func, true);
        lower.build_cfg();
        lower.build_ssa();
        lower.emit_blocks();
        debug!("global lowering completed");

        let needs_param_vars = lower.blocks.iter().any(|block| {
            block.instructions.iter().any(|instr| {
                matches!(
                    instr,
                    VMInstruction::LoadVar { .. } | VMInstruction::StoreVar { .. }
                )
            })
        });

        FunctionLowering::optimize_blocks(&mut lower.blocks);

        VMFunction {
            name: lower.func.name,
            params: Vec::new().into_boxed_slice(),
            captures: Vec::new().into_boxed_slice(),
            returns_value: false,
            blocks: lower.blocks.into_boxed_slice(),
            renamed: UstrMap::default(),
            reg_count: lower.reg_count,
            param_regs: lower.param_regs,
            ret_reg: lower.ret_reg,
            entry: lower.entry,
            block_map: lower.block_map,
            needs_param_vars,
            param_names: UstrSet::default(),
        }
    }
}

impl From<LirFunction> for VMFunction {
    fn from(value: LirFunction) -> Self {
        FunctionLowering::lower(value)
    }
}

struct FunctionLowering {
    func: LirFunction,
    blocks: Vec<VMBlock>,
    block_map: FxHashMap<BlockId, usize>,
    ssa_builder: SSABuilder,
    reg_count: Reg,
    param_regs: Vec<Reg>,
    captures: UstrSet,
    entry: BlockId,
    null_reg: Reg,
    ret_reg: Reg,
    is_global: bool,
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
        let needs_param_vars = lower.blocks.iter().any(|block| {
            block.instructions.iter().any(|instr| {
                matches!(
                    instr,
                    VMInstruction::LoadVar { .. } | VMInstruction::StoreVar { .. }
                )
            })
        });

        let param_names: UstrSet = lower.func.params.iter().map(|(n, _)| n.clone()).collect();

        FunctionLowering::optimize_blocks(&mut lower.blocks);

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
            returns_value: lower.func.return_type
                != ParserDataType::new(Span::default(), ParserInnerType::Null),
            blocks: lower.blocks.into_boxed_slice(),
            renamed: UstrMap::default(),
            reg_count: lower.reg_count,
            param_regs: lower.param_regs,
            ret_reg: lower.ret_reg,
            entry: lower.entry,
            block_map: lower.block_map,
            needs_param_vars,
            param_names,
        }
    }

    // So I made this to see if potentially manually removing certain copies could provide a speedup.
    // Results have been inconclusive so I may remove it in the future
    fn optimize_blocks(blocks: &mut Vec<VMBlock>) {
        for block in blocks.iter_mut() {
            let mut i = 0;
            while i < block.instructions.len() {
                // Removes self-copies: %rX = %rX
                if let VMInstruction::Copy { dst, src } = block.instructions[i]
                    && dst == src
                {
                    block.instructions.remove(i);
                    block.instruction_spans.remove(i);
                    continue;
                }

                // Removes LoadLiteral followed by copy
                if i + 1 < block.instructions.len() {
                    if let (
                        VMInstruction::LoadLiteral {
                            dst: dst1,
                            literal: lit1,
                        },
                        VMInstruction::Copy {
                            dst: dst2,
                            src: src1,
                        },
                    ) = (&block.instructions[i], &block.instructions[i + 1])
                        && *dst1 == *src1
                        && *dst1 != *dst2
                    {
                        block.instructions[i] = VMInstruction::LoadLiteral {
                            dst: *dst2,
                            literal: *lit1,
                        };
                        block.instructions.remove(i + 1);
                        block.instruction_spans.remove(i + 1);
                        continue;
                    }
                }

                // Removes copy followed by copy: %r2 = %r1; %r3 = %r2; -> %r3 = %r1;
                if i + 1 < block.instructions.len()
                    && let (
                        VMInstruction::Copy {
                            dst: dst1,
                            src: src1,
                        },
                        VMInstruction::Copy {
                            dst: dst2,
                            src: src2,
                        },
                    ) = (&block.instructions[i], &block.instructions[i + 1])
                    && *dst1 == *src2
                    && *dst1 != *dst2
                {
                    block.instructions[i] = VMInstruction::Copy {
                        dst: *dst2,
                        src: *src1,
                    };
                    block.instructions.remove(i + 1);
                    block.instruction_spans.remove(i + 1);
                    continue;
                }

                i += 1;
            }
        }
    }

    fn new(func: LirFunction, is_global: bool) -> Self {
        let entry = func.blocks.first().map(|b| b.id).unwrap_or(BlockId(0));
        let mut block_map = FxHashMap::default();
        for (idx, block) in func.blocks.iter().enumerate() {
            block_map.insert(block.id, idx);
        }

        let mut locals = UstrSet::default();
        if !is_global {
            for (name, _) in &func.params {
                locals.insert(name.clone());
            }
            for block in &func.blocks {
                for instr in &block.instructions {
                    match &instr.node_type {
                        LirNodeType::Declare(LirDeclare { dest, .. }) => {
                            locals.insert(dest.clone());
                        }
                        LirNodeType::Assign(LirAssign {
                            dest: LirLValue::Var(name),
                            ..
                        }) => {
                            locals.insert(name.clone());
                        }
                        _ => {}
                    }
                }
            }
        }

        let captures: UstrSet = func.captures.iter().map(|(n, _)| n.clone()).collect();

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
                if let Some(name) = instr.node_type.local_name()
                    && locals.contains(name)
                {
                    let r = reg_count;
                    reg_count += 1;
                    regs[idx] = Some(r);
                }
            }
            assign_regs.push(regs);
        }

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
            blocks: Vec::new(),
            block_map,
            ssa_builder,
            reg_count,
            param_regs,
            captures,
            entry,
            null_reg,
            ret_reg,
            is_global,
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
        for block in &self.func.blocks {
            let idx = self.block_map[&block.id];
            let info = self.ssa_builder.get_block_info(idx).clone();
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
                captures: self.captures.clone(),
                map: info.in_map.clone(),
                null_reg: self.null_reg,
                ret_reg: self.ret_reg,
                is_global: self.is_global,
                string_map: UstrMap::default(),
                int_literals: FxHashMap::default(),
                uint_literals: FxHashMap::default(),
                float_literals: FxHashMap::default(),
                char_literals: FxHashMap::default(),
                string_literals: UstrMap::default(),
                current_fn_name: self.func.name.clone(),
                big_consts: &mut self.big_consts,
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
            }

            let ret_from_body =
                block
                    .instructions
                    .iter()
                    .enumerate()
                    .rev()
                    .find_map(|(i, instr)| {
                        if instr.node_type.is_return_candidate() {
                            Some(i)
                        } else {
                            None
                        }
                    });
            let ret_from_body_non_null = ret_from_body.and_then(|i| {
                let node = &block.instructions[i].node_type;
                if node.is_null() { None } else { Some(i) }
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
                let assigned = self.ssa_builder.assign_regs()[idx]
                    .get(instr_idx)
                    .and_then(|r| *r);
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

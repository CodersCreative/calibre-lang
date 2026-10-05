use crate::conversion::{
    Reg, VMBlock,
    instructions::{
        VMInstruction::{self},
        access::{VMLoadMember, VMSetMember},
        literals::{VMAggregate, VMEnum, VMLoadLiteral},
        registers::VMCopy,
        termination::{VMBranch, VMJump},
        variables::{VMDropVar, VMLoadVar, VMLoadVarRef, VMMoveVar, VMStoreVar},
    },
    lowering::function::FunctionLowering,
};
use calibre_lir::ast::BlockId;
use calibre_parser::Span;
use rustc_hash::{FxHashMap, FxHashSet};
use tracing::instrument;

impl FunctionLowering {
    #[instrument(skip_all)]
    pub(crate) fn optimize_blocks(
        mut blocks: Box<[Option<VMBlock>]>,
        entry: BlockId,
        block_map: &FxHashMap<BlockId, usize>,
    ) -> Box<[Option<VMBlock>]> {
        let mut block_subst: FxHashMap<BlockId, BlockId> = FxHashMap::default();
        let mut reg_use_count: FxHashMap<Reg, usize> = FxHashMap::default();
        let mut prev_count: FxHashMap<BlockId, usize> = FxHashMap::default();
        let mut referenced: FxHashSet<BlockId> = FxHashSet::default();
        let mut worklist: Vec<BlockId> = Vec::new();

        let mut changed = true;
        let mut iterations = 0;

        while changed && iterations < 512 {
            changed = false;
            iterations += 1;

            // Get jump onlys
            block_subst.clear();
            for block in blocks.iter().flatten() {
                if block.instructions.len() == 1
                    && let Some(VMInstruction::Jump(VMJump { target })) = block.instructions.first()
                {
                    block_subst.insert(block.id, *target);
                }
            }

            // Get reachability
            referenced.clear();
            worklist.clear();
            referenced.insert(entry);
            worklist.push(entry);

            while let Some(curr_id) = worklist.pop() {
                if let Some(&idx) = block_map.get(&curr_id)
                    && let Some(block) = &blocks[idx]
                {
                    for instr in &block.instructions {
                        match instr {
                            VMInstruction::Jump(VMJump { target }) => {
                                let target = resolve_target(*target, &block_subst);
                                if referenced.insert(target) {
                                    worklist.push(target);
                                }
                            }
                            VMInstruction::Branch(VMBranch { then_block, .. }) => {
                                let then_b = resolve_target(*then_block, &block_subst);
                                if referenced.insert(then_b) {
                                    worklist.push(then_b);
                                }
                            }
                            _ => {}
                        }
                    }
                }
            }

            // Remove unused blocks
            for block_opt in blocks.iter_mut() {
                if let Some(block) = block_opt
                    && !referenced.contains(&block.id)
                {
                    *block_opt = None;
                    changed = true;
                }
            }

            // Count register uses and prevs for reachable blocks
            reg_use_count.clear();
            prev_count.clear();

            for block in blocks.iter().flatten() {
                for instr in &block.instructions {
                    instr.count_instr_uses(&mut reg_use_count);
                }

                for edge_copy in &block.edge_copies {
                    for copy in &edge_copy.copies {
                        *reg_use_count.entry(copy.src).or_insert(0) += 1;
                    }
                }

                for instr in &block.instructions {
                    match instr {
                        VMInstruction::Jump(VMJump { target }) => {
                            let target = resolve_target(*target, &block_subst);
                            *prev_count.entry(target).or_insert(0) += 1;
                        }
                        VMInstruction::Branch(VMBranch { then_block, .. }) => {
                            let then_b = resolve_target(*then_block, &block_subst);
                            *prev_count.entry(then_b).or_insert(0) += 1;
                        }
                        _ => {}
                    }
                }
            }

            for block in blocks.iter_mut().flatten() {
                let old_instrs = std::mem::take(&mut block.instructions);
                let old_spans = std::mem::take(&mut block.instruction_spans);

                block.instructions.reserve(old_instrs.len());
                block.instruction_spans.reserve(old_spans.len());

                let mut iter = old_instrs.into_iter().zip(old_spans.into_iter()).peekable();

                while let Some((mut instr, span)) = iter.next() {
                    let mut remove_current = false;
                    let mut remove_next = false;

                    // Self copies (%rX = %rX)
                    if let VMInstruction::Copy(VMCopy { dst, src }) = &instr
                        && dst == src
                    {
                        remove_current = true;
                    }

                    // Dead code removal
                    if !remove_current
                        && !instr.has_side_effects()
                        && let Some(dst) = instr.get_dst()
                        && reg_use_count.get(dst).copied().unwrap_or(0) == 0
                    {
                        remove_current = true;
                    }

                    // Simplifications
                    if !remove_current {
                        match &mut instr {
                            VMInstruction::Jump(VMJump { target }) => {
                                let new_target = resolve_target(*target, &block_subst);
                                if new_target != *target {
                                    *target = new_target;
                                    changed = true;
                                }
                            }
                            VMInstruction::Branch(VMBranch {
                                cond: _,
                                then_block,
                            }) => {
                                let new_then = resolve_target(*then_block, &block_subst);
                                if *then_block != new_then {
                                    *then_block = new_then;
                                    changed = true;
                                }
                            }
                            _ => {}
                        }
                    }

                    // 2 instruction optimizations
                    if !remove_current && let Some((next_instr, _next_span)) = iter.peek() {
                        // Producer + Copy
                        if let VMInstruction::Copy(VMCopy {
                            dst: dst2,
                            src: src1,
                        }) = next_instr
                            && let Some(dst) = instr.get_dst_mut()
                            && dst == src1
                            && dst != dst2
                            && reg_use_count.get(dst).copied().unwrap_or(0) == 1
                        {
                            *dst = *dst2;
                            remove_next = true;
                        }

                        // Copy + StoreVar
                        if !remove_next
                            && let VMInstruction::Copy(VMCopy {
                                dst: copy_dst,
                                src: copy_src,
                            }) = &instr
                            && let VMInstruction::StoreVar(VMStoreVar {
                                dst: store_dst,
                                name,
                                src: store_src,
                            }) = next_instr
                            && copy_dst == store_src
                            && reg_use_count.get(copy_dst).copied().unwrap_or(0) == 1
                        {
                            instr = VMInstruction::StoreVar(VMStoreVar {
                                dst: *store_dst,
                                name: *name,
                                src: *copy_src,
                            });
                            remove_current = false;
                            remove_next = true;
                        }
                    }

                    if remove_current {
                        changed = true;
                    } else if remove_next {
                        changed = true;
                        block.instructions.push(instr);
                        block.instruction_spans.push(span);
                        iter.next();
                    } else {
                        block.instructions.push(instr);
                        block.instruction_spans.push(span);
                    }
                }
            }

            // Merge blocks
            let mut merges: Vec<(usize, usize, BlockId)> = Vec::new();
            for (i, block_opt) in blocks.iter().enumerate() {
                if let Some(block) = block_opt
                    && let Some(VMInstruction::Jump(VMJump { target })) = block.instructions.last()
                {
                    let target = resolve_target(*target, &block_subst);
                    if target != block.id
                        && target != entry
                        && prev_count.get(&target).copied().unwrap_or(0) <= 1
                        && let Some(&target_idx) = block_map.get(&target)
                    {
                        merges.push((i, target_idx, target));
                    }
                }
            }

            if !merges.is_empty() {
                changed = true;
            }

            for (i, target_idx, target_id) in merges {
                if blocks[i].is_some() && blocks[target_idx].is_some() {
                    let target_block = blocks[target_idx].take().unwrap();
                    let block = blocks[i].as_mut().unwrap();

                    block.instructions.pop();
                    let last_span = block.instruction_spans.pop().unwrap_or_default();

                    block.merge(target_block, last_span);
                    block_subst.insert(target_id, block.id);
                }
            }
        }
        blocks
    }
}

impl VMBlock {
    pub fn merge(&mut self, mut target_block: VMBlock, copy_span: Span) {
        let mut inline_copies = Vec::new();
        self.edge_copies.retain_mut(|edge_copy| {
            if edge_copy.target == target_block.id {
                for copy in std::mem::take(&mut edge_copy.copies) {
                    inline_copies.push(VMInstruction::Copy(copy));
                }
                false
            } else {
                true
            }
        });

        for copy_instr in inline_copies {
            self.instructions.push(copy_instr);
            self.instruction_spans.push(copy_span);
        }

        let start_literals_index = self.local_literals.len() as u16;
        self.local_literals.append(&mut target_block.local_literals);

        let start_strings_index = self.local_strings.len() as u16;
        self.local_strings.append(&mut target_block.local_strings);

        let start_variables_index = self.local_variables.len() as u16;
        self.local_variables
            .append(&mut target_block.local_variables);

        let start_types_index = self.local_types.len() as u16;
        self.local_types.append(&mut target_block.local_types);

        let start_aggregate_layout = self.aggregate_layouts.len() as u16;
        self.aggregate_layouts
            .append(&mut target_block.aggregate_layouts);

        self.instructions
            .extend(target_block.instructions.into_iter().map(|mut x| {
                match &mut x {
                    VMInstruction::Aggregate(VMAggregate { layout, .. }) => {
                        *layout += start_aggregate_layout
                    }

                    VMInstruction::LoadLiteral(VMLoadLiteral { literal, .. }) => {
                        *literal += start_literals_index
                    }

                    VMInstruction::Enum(VMEnum { name, .. }) => *name += start_types_index,

                    VMInstruction::LoadMember(VMLoadMember { member: name, .. })
                    | VMInstruction::SetMember(VMSetMember { member: name, .. }) => {
                        *name += start_strings_index
                    }

                    VMInstruction::DropVar(VMDropVar { name, .. })
                    | VMInstruction::MoveVar(VMMoveVar { name, .. })
                    | VMInstruction::StoreVar(VMStoreVar { name, .. })
                    | VMInstruction::LoadVarRef(VMLoadVarRef { name, .. })
                    | VMInstruction::LoadVar(VMLoadVar { name, .. }) => {
                        *name += start_variables_index
                    }

                    _ => {}
                }
                x
            }));

        self.instruction_spans
            .append(&mut target_block.instruction_spans);

        self.edge_copies.append(&mut target_block.edge_copies);
    }
}

#[inline]
fn resolve_target(mut target: BlockId, subst: &FxHashMap<BlockId, BlockId>) -> BlockId {
    let mut fuel = 64;
    while let Some(&next) = subst.get(&target) {
        if next == target || fuel == 0 {
            break;
        }
        target = next;
        fuel -= 1;
    }
    target
}

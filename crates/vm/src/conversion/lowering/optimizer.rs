use crate::conversion::{
    Reg, VMBlock,
    instructions::{
        VMInstruction::{self},
        access::{VMIndex, VMLoadMember, VMSetIndex, VMSetMember},
        binary::{VMAs, VMBinary, VMBoolean, VMComparison, VMIs},
        functions::{VMCall, VMCallSelf, VMSpawn},
        literals::{VMAggregate, VMEnum, VMList, VMLoadLiteral, VMRange},
        memory::{VMDeref, VMRef, VMSetRef},
        registers::{VMCopy, VMLoadRegRef},
        termination::{VMBranch, VMJump, VMReturn},
        variables::{VMDropVar, VMLoadVar, VMLoadVarRef, VMMoveVar, VMStoreVar},
    },
    lowering::function::FunctionLowering,
};
use calibre_lir::ast::BlockId;
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
                    count_instr_uses(instr, &mut reg_use_count);
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
                let mut instr_idx = 0;

                while instr_idx < block.instructions.len() {
                    let mut remove_current = false;
                    let mut remove_next = false;

                    let instr = &block.instructions[instr_idx];

                    // Self copies (%rX = %rX)
                    if let VMInstruction::Copy(VMCopy { dst, src }) = instr
                        && dst == src
                    {
                        remove_current = true;
                    }

                    // Dead code removal
                    if !remove_current
                        && !has_side_effects(instr)
                        && let Some(dst) = defined_reg(instr)
                        && reg_use_count.get(&dst).copied().unwrap_or(0) == 0
                    {
                        remove_current = true;
                    }

                    // Simplifications
                    if !remove_current {
                        match &mut block.instructions[instr_idx] {
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
                    if !remove_current && instr_idx + 1 < block.instructions.len() {
                        // Producer + Copy
                        let next = match &block.instructions[instr_idx + 1] {
                            VMInstruction::Copy(x) => Some(x.clone()),
                            _ => None,
                        };

                        if let Some(VMCopy {
                            dst: dst2,
                            src: src1,
                        }) = next
                            && let Some(dst1) = defined_reg(&block.instructions[instr_idx])
                            && dst1 == src1
                            && dst1 != dst2
                            && reg_use_count.get(&dst1).copied().unwrap_or(0) == 1
                            && try_replace_dst(&mut block.instructions[instr_idx], dst2)
                        {
                            remove_next = true;
                        }

                        // Copy + StoreVar
                        if !remove_next
                            && let (
                                VMInstruction::Copy(VMCopy {
                                    dst: copy_dst,
                                    src: copy_src,
                                }),
                                VMInstruction::StoreVar(VMStoreVar {
                                    dst: store_dst,
                                    name,
                                    src: store_src,
                                }),
                            ) = (
                                &block.instructions[instr_idx],
                                &block.instructions[instr_idx + 1],
                            )
                            && copy_dst == store_src
                            && reg_use_count.get(copy_dst).copied().unwrap_or(0) == 1
                        {
                            block.instructions[instr_idx + 1] =
                                VMInstruction::StoreVar(VMStoreVar {
                                    dst: *store_dst,
                                    name: *name,
                                    src: *copy_src,
                                });
                            remove_current = true;
                        }
                    }

                    if remove_current {
                        block.instructions.remove(instr_idx);
                        block.instruction_spans.remove(instr_idx);
                        changed = true;
                    } else if remove_next {
                        block.instructions.remove(instr_idx + 1);
                        block.instruction_spans.remove(instr_idx + 1);
                        changed = true;
                    } else {
                        instr_idx += 1;
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

            for (i, target_idx, target_id) in merges {
                if blocks[i].is_some() && blocks[target_idx].is_some() {
                    let mut target_block = blocks[target_idx].take().unwrap();
                    let block = blocks[i].as_mut().unwrap();

                    block.instructions.pop();
                    let last_span = block.instruction_spans.pop().unwrap_or_default();

                    let mut inline_copies = Vec::new();
                    block.edge_copies.retain_mut(|edge_copy| {
                        if edge_copy.target == target_id {
                            for copy in std::mem::take(&mut edge_copy.copies) {
                                inline_copies.push(VMInstruction::Copy(copy));
                            }
                            false
                        } else {
                            true
                        }
                    });

                    for copy_instr in inline_copies {
                        block.instructions.push(copy_instr);
                        block.instruction_spans.push(last_span);
                    }

                    let start_literals_index = block.local_literals.len() as u16;
                    block
                        .local_literals
                        .append(&mut target_block.local_literals);

                    let start_strings_index = block.local_strings.len() as u16;
                    block.local_strings.append(&mut target_block.local_strings);

                    let start_aggregate_layout = block.aggregate_layouts.len() as u16;
                    block
                        .aggregate_layouts
                        .append(&mut target_block.aggregate_layouts);

                    block
                        .instructions
                        .extend(target_block.instructions.into_iter().map(|x| match x {
                            VMInstruction::LoadLiteral(VMLoadLiteral { dst, literal }) => {
                                VMInstruction::LoadLiteral(VMLoadLiteral {
                                    dst,
                                    literal: literal + start_literals_index,
                                })
                            }
                            VMInstruction::DropVar(VMDropVar { name }) => {
                                VMInstruction::DropVar(VMDropVar {
                                    name: name + start_strings_index,
                                })
                            }
                            VMInstruction::MoveVar(VMMoveVar { dst, name }) => {
                                VMInstruction::MoveVar(VMMoveVar {
                                    dst,
                                    name: name + start_strings_index,
                                })
                            }
                            VMInstruction::StoreVar(VMStoreVar { dst, name, src }) => {
                                VMInstruction::StoreVar(VMStoreVar {
                                    dst,
                                    name: name + start_strings_index,
                                    src,
                                })
                            }
                            VMInstruction::LoadVar(VMLoadVar { dst, name }) => {
                                VMInstruction::LoadVar(VMLoadVar {
                                    dst,
                                    name: name + start_strings_index,
                                })
                            }

                            VMInstruction::LoadVarRef(VMLoadVarRef { dst, name }) => {
                                VMInstruction::LoadVarRef(VMLoadVarRef {
                                    dst,
                                    name: name + start_strings_index,
                                })
                            }
                            VMInstruction::LoadMember(VMLoadMember { dst, value, member }) => {
                                VMInstruction::LoadMember(VMLoadMember {
                                    dst,
                                    value,
                                    member: member + start_strings_index,
                                })
                            }
                            VMInstruction::SetMember(VMSetMember {
                                dst,
                                target,
                                member,
                                value,
                            }) => VMInstruction::SetMember(VMSetMember {
                                dst,
                                target,
                                member: member + start_strings_index,
                                value,
                            }),
                            VMInstruction::Enum(VMEnum {
                                dst,
                                name,
                                variant,
                                payload,
                            }) => VMInstruction::Enum(VMEnum {
                                dst,
                                name: name + start_strings_index,
                                variant,
                                payload,
                            }),
                            VMInstruction::Aggregate(VMAggregate {
                                dst,
                                layout,
                                fields,
                            }) => VMInstruction::Aggregate(VMAggregate {
                                dst,
                                layout: layout + start_aggregate_layout,
                                fields,
                            }),

                            x => x,
                        }));

                    block
                        .instruction_spans
                        .append(&mut target_block.instruction_spans);

                    block.edge_copies.append(&mut target_block.edge_copies);

                    block_subst.insert(target_id, block.id);
                    changed = true;
                }
            }
        }

        blocks
    }
}

#[inline]
fn resolve_target(mut target: BlockId, subst: &FxHashMap<BlockId, BlockId>) -> BlockId {
    while let Some(&next) = subst.get(&target) {
        if next == target {
            break;
        }
        target = next;
    }
    target
}

#[inline]
fn defined_reg(instr: &VMInstruction) -> Option<Reg> {
    match instr {
        // Registers
        VMInstruction::Copy(VMCopy { dst, .. })
        | VMInstruction::LoadRegRef(VMLoadRegRef { dst, .. })
        // Variables
        | VMInstruction::LoadVar(VMLoadVar { dst, .. })
        | VMInstruction::MoveVar(VMMoveVar { dst, .. })
        | VMInstruction::LoadVarRef(VMLoadVarRef { dst, .. })
        // Binary
        | VMInstruction::As(VMAs { dst, .. })
        | VMInstruction::Is(VMIs { dst, .. })
        | VMInstruction::Binary(VMBinary { dst, .. })
        | VMInstruction::Comparison(VMComparison { dst, .. })
        | VMInstruction::Boolean(VMBoolean { dst, .. })
        // Functions
        | VMInstruction::Spawn(VMSpawn { dst, .. })
        // Access
        | VMInstruction::LoadMember(VMLoadMember { dst, .. })
        | VMInstruction::SetMember(VMSetMember { dst, .. })
        | VMInstruction::Index(VMIndex { dst, .. })
        | VMInstruction::SetIndex(VMSetIndex { dst, .. })
        // Memory
        | VMInstruction::Ref(VMRef { dst, .. })
        | VMInstruction::Deref(VMDeref { dst, .. })
        | VMInstruction::SetRef(VMSetRef { dst, .. })
        // Literals
        | VMInstruction::LoadLiteral(VMLoadLiteral { dst, .. })
        | VMInstruction::Aggregate(VMAggregate { dst, .. })
        | VMInstruction::Enum(VMEnum { dst, .. })
        | VMInstruction::List(VMList { dst, .. })
        | VMInstruction::Range(VMRange { dst, .. }) => Some(*dst),

        VMInstruction::Call(VMCall { dst, .. })
        | VMInstruction::StoreVar(VMStoreVar { dst, .. })
        | VMInstruction::CallSelf(VMCallSelf { dst, .. }) => *dst,
        _ => None,
    }
}

#[inline]
fn try_replace_dst(instr: &mut VMInstruction, new_dst: Reg) -> bool {
    match instr {
        // Registers
        VMInstruction::Copy(VMCopy { dst, .. })
        | VMInstruction::LoadRegRef(VMLoadRegRef { dst, .. })
        // Variables
        | VMInstruction::LoadVar(VMLoadVar { dst, .. })
        | VMInstruction::MoveVar(VMMoveVar { dst, .. })
        | VMInstruction::LoadVarRef(VMLoadVarRef { dst, .. })
        // Binary
        | VMInstruction::As(VMAs { dst, .. })
        | VMInstruction::Is(VMIs { dst, .. })
        | VMInstruction::Binary(VMBinary { dst, .. })
        | VMInstruction::Comparison(VMComparison { dst, .. })
        | VMInstruction::Boolean(VMBoolean { dst, .. })
        // Functions
        | VMInstruction::Spawn(VMSpawn { dst, .. })
        // Access
        | VMInstruction::LoadMember(VMLoadMember { dst, .. })
        | VMInstruction::SetMember(VMSetMember { dst, .. })
        | VMInstruction::Index(VMIndex { dst, .. })
        | VMInstruction::SetIndex(VMSetIndex { dst, .. })
        // Memory
        | VMInstruction::Ref(VMRef { dst, .. })
        | VMInstruction::Deref(VMDeref { dst, .. })
        | VMInstruction::SetRef(VMSetRef { dst, .. })
        // Literals
        | VMInstruction::LoadLiteral(VMLoadLiteral { dst, .. })
        | VMInstruction::Aggregate(VMAggregate { dst, .. })
        | VMInstruction::Enum(VMEnum { dst, .. })
        | VMInstruction::List(VMList { dst, .. })
        | VMInstruction::Range(VMRange { dst, .. }) => {
            *dst = new_dst;
            true
        }
        VMInstruction::CallSelf(VMCallSelf { dst, .. })
        | VMInstruction::StoreVar(VMStoreVar { dst, .. })
        | VMInstruction::Call(VMCall { dst, .. }) => {
            *dst = Some(new_dst);
            true
        }
        _ => false,
    }
}

#[inline]
fn has_side_effects(instr: &VMInstruction) -> bool {
    matches!(
        instr,
        VMInstruction::Call(_)
            | VMInstruction::CallSelf(_)
            | VMInstruction::StoreVar(_)
            | VMInstruction::SetMember(_)
            | VMInstruction::SetIndex(_)
            | VMInstruction::SetRef(_)
            | VMInstruction::Branch(_)
            | VMInstruction::Jump(_)
            | VMInstruction::Return(_)
    )
}

fn count_instr_uses(instr: &VMInstruction, use_map: &mut FxHashMap<Reg, usize>) {
    let mut add_use = |r: Reg| {
        *use_map.entry(r).or_insert(0) += 1;
    };

    match instr {
        VMInstruction::Copy(VMCopy { src, .. })
        | VMInstruction::Ref(VMRef { value: src, .. })
        | VMInstruction::Deref(VMDeref { value: src, .. })
        | VMInstruction::LoadRegRef(VMLoadRegRef { src, .. })
        | VMInstruction::Spawn(VMSpawn { callee: src, .. })
        | VMInstruction::Is(VMIs { src, .. })
        | VMInstruction::As(VMAs { src, .. }) => add_use(*src),
        VMInstruction::StoreVar(VMStoreVar { src, .. }) => add_use(*src),
        VMInstruction::Call(VMCall { callee, args, .. }) => {
            add_use(*callee);
            for arg in args {
                add_use(*arg);
            }
        }
        VMInstruction::CallSelf(VMCallSelf { args, .. }) => {
            for arg in args {
                add_use(*arg);
            }
        }
        VMInstruction::Binary(VMBinary { left, right, .. })
        | VMInstruction::Range(VMRange {
            from: left,
            to: right,
            ..
        })
        | VMInstruction::Comparison(VMComparison { left, right, .. })
        | VMInstruction::Boolean(VMBoolean { left, right, .. }) => {
            add_use(*left);
            add_use(*right);
        }
        VMInstruction::Index(VMIndex { value, index, .. }) => {
            add_use(*value);
            add_use(*index);
        }
        VMInstruction::LoadMember(VMLoadMember { value, .. }) => add_use(*value),
        VMInstruction::SetRef(VMSetRef {
            dst: _,
            target,
            value,
        })
        | VMInstruction::SetMember(VMSetMember { target, value, .. }) => {
            add_use(*target);
            add_use(*value);
        }
        VMInstruction::SetIndex(VMSetIndex {
            target,
            index,
            value,
            ..
        }) => {
            add_use(*target);
            add_use(*index);
            add_use(*value);
        }
        VMInstruction::Branch(VMBranch { cond, .. }) => add_use(*cond),
        VMInstruction::Enum(VMEnum {
            payload: Some(v), ..
        })
        | VMInstruction::Return(VMReturn { value: Some(v) }) => add_use(*v),
        VMInstruction::Aggregate(VMAggregate { fields: items, .. })
        | VMInstruction::List(VMList { items, .. }) => {
            items.iter().for_each(|x| add_use(*x));
        }

        VMInstruction::Noop
        | VMInstruction::Jump(_)
        | VMInstruction::Return(_)
        | VMInstruction::Enum(_)
        | VMInstruction::LoadLiteral(_)
        | VMInstruction::MoveVar(_)
        | VMInstruction::DropVar(_)
        | VMInstruction::LoadVarRef(_)
        | VMInstruction::LoadVar(_) => {}
    }
}

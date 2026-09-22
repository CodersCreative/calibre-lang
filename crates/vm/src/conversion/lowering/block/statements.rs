/*
This file handles :
Declare,
Extern,
Assign
*/

use crate::conversion::{
    Reg, VMLiteral,
    instructions::{VMInstruction, literals::VMLoadLiteral},
    lowering::{BlockLoweringCtx, block::VMLowering},
};
use calibre_lir::ast::{
    LirAssign, LirDeclare, LirExtern, LirIndex, LirLValue, LirLoad, LirMember, LirNodeType,
};
use calibre_parser::Span;

impl VMLowering for LirDeclare {
    #[inline(always)]
    fn lower<'a>(self, env: &mut BlockLoweringCtx<'a>, _span: Span) -> Reg {
        env.null_reg
    }

    #[inline(always)]
    fn lower_instr<'a>(
        self,
        env: &mut BlockLoweringCtx<'a>,
        assigned: Option<Reg>,
        _set_ret: bool,
        span: Span,
    ) where
        Self: Sized,
    {
        let promoted = self.is_referenced || env.referenced_variables.contains(&self.dest);

        if !env.is_global && !promoted {
            let target = assigned.unwrap_or_else(|| env.alloc_reg());

            env.lower_node_to(*self.value, target, span);
            env.map.insert(self.dest, target);
        } else {
            let reg = env.lower_node(*self.value, span);
            let name = env.add_string(self.dest);
            env.emit(
                VMInstruction::StoreVar {
                    dst: None,
                    name,
                    src: reg,
                },
                span,
            );
        }
    }
}

impl VMLowering for LirExtern {
    #[inline(always)]
    fn lower<'a>(self, env: &mut BlockLoweringCtx<'a>, span: Span) -> Reg {
        env.block.local_literals.push(VMLiteral::ExternFunction {
            abi: self.abi,
            library: self.library,
            symbol: self.symbol,
            parameters: self.parameters,
            return_type: self.return_type,
            memo: self.memo,
            pure: self.pure,
            memo_params: self.memo_params,
        });
        let lit = (env.block.local_literals.len() - 1) as u16;
        let dst = env.alloc_reg();
        env.emit(
            VMInstruction::LoadLiteral(VMLoadLiteral { dst, literal: lit }),
            span,
        );
        dst
    }
}

fn lower_assignment<'a>(
    node: LirAssign,
    env: &mut BlockLoweringCtx<'a>,
    assigned: Option<Reg>,
    span: Span,
) -> Reg {
    let dst = env.alloc_reg();
    match node.dest {
        LirLValue::Var(dest) => {
            let name_idx = env.add_string(dest);
            let reg = env.lower_node(*node.value, span);
            env.emit(
                VMInstruction::StoreVar {
                    dst: Some(assigned.unwrap_or(dst)),
                    name: name_idx,
                    src: reg,
                },
                span,
            );
        }
        LirLValue::Ptr(ptr) => {
            let value_reg = env.lower_node(*node.value, span);
            match *ptr {
                LirNodeType::Member(LirMember { base, field }) => {
                    let base_reg = env.lower_node(*base, span);
                    let member = env.add_string(field);
                    env.emit(
                        VMInstruction::SetMember {
                            dst,
                            target: base_reg,
                            member,
                            value: value_reg,
                        },
                        span,
                    );
                }
                LirNodeType::Index(LirIndex { base, index }) => {
                    let index_reg = env.lower_node(*index, span);
                    match *base {
                        LirNodeType::Member(LirMember {
                            base: owner,
                            field: member,
                        }) => {
                            let owner_reg = env.lower_node(*owner, span);
                            let member_idx = env.add_string(member);
                            let member_val_reg = env.alloc_reg();
                            env.emit(
                                VMInstruction::LoadMember {
                                    dst: member_val_reg,
                                    value: owner_reg,
                                    member: member_idx,
                                },
                                span,
                            );
                            env.emit(
                                VMInstruction::SetIndex {
                                    dst,
                                    target: member_val_reg,
                                    index: index_reg,
                                    value: value_reg,
                                },
                                span,
                            );
                            env.emit(
                                VMInstruction::SetMember {
                                    dst,
                                    target: owner_reg,
                                    member: member_idx,
                                    value: member_val_reg,
                                },
                                span,
                            );
                        }
                        LirNodeType::Load(LirLoad { value }) => {
                            let base_reg = env.alloc_reg();
                            if let Some(reg) = env.map.get(&value) {
                                env.emit(
                                    VMInstruction::Copy {
                                        dst: base_reg,
                                        src: *reg,
                                    },
                                    span,
                                );
                                env.emit(
                                    VMInstruction::SetIndex {
                                        dst,
                                        target: base_reg,
                                        index: index_reg,
                                        value: value_reg,
                                    },
                                    span,
                                );
                            } else {
                                let idx = env.add_string(value);
                                env.emit(
                                    VMInstruction::LoadVarRef {
                                        dst: base_reg,
                                        name: idx,
                                    },
                                    span,
                                );
                                env.emit(
                                    VMInstruction::SetIndex {
                                        dst,
                                        target: base_reg,
                                        index: index_reg,
                                        value: value_reg,
                                    },
                                    span,
                                );
                            }
                        }
                        other_base => {
                            let base_reg = env.lower_node(other_base, span);
                            env.emit(
                                VMInstruction::SetIndex {
                                    dst,
                                    target: base_reg,
                                    index: index_reg,
                                    value: value_reg,
                                },
                                span,
                            );
                        }
                    }
                }
                other => {
                    let target_reg = env.lower_node(other, span);
                    env.emit(
                        VMInstruction::SetRef {
                            dst,
                            target: target_reg,
                            value: value_reg,
                        },
                        span,
                    );
                }
            }
        }
    }
    dst
}

impl VMLowering for LirAssign {
    #[inline(always)]
    fn lower<'a>(self, env: &mut BlockLoweringCtx<'a>, span: Span) -> Reg {
        lower_assignment(self, env, None, span)
    }

    #[inline(always)]
    fn lower_instr<'a>(
        self,
        env: &mut BlockLoweringCtx<'a>,
        assigned: Option<Reg>,
        _set_ret: bool,
        span: Span,
    ) where
        Self: Sized,
    {
        let _ = lower_assignment(self, env, assigned, span);
    }
}

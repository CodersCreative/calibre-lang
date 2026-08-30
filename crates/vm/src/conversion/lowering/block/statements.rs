/*
This file handles :
Declare,
Extern,
Assign
*/

use crate::conversion::{
    Reg, VMInstruction, VMLiteral,
    lowering::{BlockLoweringCtx, block::VMLowering},
};
use calibre_lir::ast::{
    LirAssign, LirDeclare, LirExtern, LirIndex, LirLValue, LirMember, LirNodeType,
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
        if !env.is_global {
            let target = assigned.unwrap_or_else(|| env.alloc_reg());

            env.lower_node_to(*self.value, target, span);
            let name_idx = env.add_string(self.dest.to_string());
            env.map.insert(self.dest.to_string(), target);

            env.emit(
                VMInstruction::StoreVar {
                    name: name_idx,
                    src: target,
                },
                span,
            );
        } else {
            let reg = env.lower_node(*self.value, span);
            let name = env.add_string(self.dest.to_string());
            env.emit(VMInstruction::StoreVar { name, src: reg }, span);
        }
    }
}

impl VMLowering for LirExtern {
    #[inline(always)]
    fn lower<'a>(self, env: &mut BlockLoweringCtx<'a>, span: Span) -> Reg {
        env.block.local_literals.push(VMLiteral::ExternFunction {
            abi: self.abi.to_string(),
            library: self.library.to_string(),
            symbol: self.symbol.to_string(),
            parameters: self.parameters,
            return_type: self.return_type,
        });
        let lit = (env.block.local_literals.len() - 1) as u16;
        let dst = env.alloc_reg();
        env.emit(VMInstruction::LoadLiteral { dst, literal: lit }, span);
        dst
    }
}

impl VMLowering for LirAssign {
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
        match self.dest {
            LirLValue::Var(dest) => {
                let name_idx = env.add_string(dest.to_string());
                if !env.is_global && env.map.contains_key(dest.as_ref()) {
                    let target = assigned.unwrap_or_else(|| env.alloc_reg());

                    env.lower_node_to(*self.value, target, span);
                    env.map.insert(dest.to_string(), target);

                    env.emit(
                        VMInstruction::StoreVar {
                            name: name_idx,
                            src: target,
                        },
                        span,
                    );
                } else {
                    let reg = env.lower_node(*self.value, span);
                    env.emit(
                        VMInstruction::StoreVar {
                            name: name_idx,
                            src: reg,
                        },
                        span,
                    );
                }
            }
            LirLValue::Ptr(ptr) => {
                let value_reg = env.lower_node(*self.value, span);
                match *ptr {
                    LirNodeType::Member(LirMember { base, field }) => {
                        let base_reg = env.lower_node(*base, span);
                        let member = env.add_string(field.to_string());
                        env.emit(
                            VMInstruction::SetMember {
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
                                let member_idx = env.add_string(member.to_string());
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
                                        target: member_val_reg,
                                        index: index_reg,
                                        value: value_reg,
                                    },
                                    span,
                                );
                                env.emit(
                                    VMInstruction::SetMember {
                                        target: owner_reg,
                                        member: member_idx,
                                        value: member_val_reg,
                                    },
                                    span,
                                );
                            }
                            other_base => {
                                let base_reg = env.lower_node(other_base, span);
                                env.emit(
                                    VMInstruction::SetIndex {
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
                                target: target_reg,
                                value: value_reg,
                            },
                            span,
                        );
                    }
                }
            }
        }
    }
}

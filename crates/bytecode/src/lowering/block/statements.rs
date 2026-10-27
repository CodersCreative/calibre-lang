/*
This file handles :
Declare,
Extern,
Assign
*/

use crate::{
    Reg, VMLiteral,
    instructions::{
        VMInstruction,
        literals::VMLoadLiteral,
        memory::VMSetRef,
        variables::VMStoreVar,
    },
    lowering::{BlockLoweringCtx, block::VMLowering},
};
use calibre_lir::ast::{
    LirAssign, LirDeclare, LirExtern, LirLValue,
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
            let name = env.add_variable(self.dest);

            env.emit(
                VMInstruction::StoreVar(VMStoreVar {
                    dst: None,
                    name,
                    src: reg,
                }),
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
            let name_idx = env.add_variable(dest);
            let reg = env.lower_node(*node.value, span);
            env.emit(
                VMInstruction::StoreVar(VMStoreVar {
                    dst: Some(assigned.unwrap_or(dst)),
                    name: name_idx,
                    src: reg,
                }),
                span,
            );
        }
        LirLValue::Ptr(ptr) => {
            let value = env.lower_node(*node.value, span);
            let target = env.lower_node(*ptr, span);

            env.emit(
                VMInstruction::SetRef(VMSetRef {
                    dst: assigned.unwrap_or(dst),
                    target,
                    value,
                }),
                span,
            );
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

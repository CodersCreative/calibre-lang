/*
This file handles :
Call,
Index,
Member,
Deref,
Ref,
RefLoad
*/

use crate::conversion::{
    Reg, VMInstruction,
    lowering::{BlockLoweringCtx, block::VMLowering},
};
use calibre_lir::ast::{
    LirCall, LirDeref, LirIndex, LirLoad, LirMember, LirMove, LirNodeType, LirRef, LirRefLoad,
};
use calibre_parser::Span;

impl VMLowering for LirCall {
    #[inline(always)]
    fn lower<'a>(self, env: &mut BlockLoweringCtx<'a>, span: Span) -> Reg {
        let args = self
            .args
            .into_iter()
            .map(|arg| env.lower_node(arg, span))
            .collect();
        let dst = env.alloc_reg();

        match *self.caller {
            LirNodeType::Load(LirLoad { value }) | LirNodeType::Move(LirMove { value })
                if value == env.current_fn_name =>
            {
                env.emit(VMInstruction::CallSelf { dst, args }, span);
            }
            other => {
                let callee = env.lower_node(other, span);
                env.emit(VMInstruction::Call { dst, callee, args }, span);
            }
        }

        dst
    }
}

impl VMLowering for LirIndex {
    #[inline(always)]
    fn lower<'a>(self, env: &mut BlockLoweringCtx<'a>, span: Span) -> Reg {
        let index = env.lower_node(*self.index, span);
        let value = env.lower_node(*self.base, span);
        let dst = env.alloc_reg();
        env.emit(VMInstruction::Index { dst, value, index }, span);
        dst
    }
}

impl VMLowering for LirMember {
    #[inline(always)]
    fn lower<'a>(self, env: &mut BlockLoweringCtx<'a>, span: Span) -> Reg {
        let value = env.lower_node(*self.base, span);
        let member = env.add_string(self.field);
        let dst = env.alloc_reg();
        env.emit(VMInstruction::LoadMember { dst, value, member }, span);
        dst
    }
}

impl VMLowering for LirDeref {
    #[inline(always)]
    fn lower<'a>(self, env: &mut BlockLoweringCtx<'a>, span: Span) -> Reg {
        let value = env.lower_node(*self.value, span);
        let dst = env.alloc_reg();
        env.emit(VMInstruction::Deref { dst, value }, span);
        dst
    }
}

impl VMLowering for LirRef {
    #[inline(always)]
    fn lower<'a>(self, env: &mut BlockLoweringCtx<'a>, span: Span) -> Reg {
        match *self.value {
            LirNodeType::Load(LirLoad { value }) => {
                let idx = env.add_string(value);
                let dst = env.alloc_reg();
                env.emit(VMInstruction::LoadVarRef { dst, name: idx }, span);
                dst
            }
            other => {
                let value = env.lower_node(other, span);
                let dst = env.alloc_reg();
                env.emit(VMInstruction::Ref { dst, value }, span);
                dst
            }
        }
    }
}

impl VMLowering for LirRefLoad {
    #[inline(always)]
    fn lower<'a>(self, env: &mut BlockLoweringCtx<'a>, span: Span) -> Reg {
        let idx = env.add_string(self.value);
        let dst = env.alloc_reg();
        env.emit(VMInstruction::LoadVarRef { dst, name: idx }, span);
        dst
    }
}

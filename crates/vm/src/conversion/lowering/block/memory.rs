/*
This file handles :
Load,
Move,
Drop,
Spawn
*/

use crate::conversion::{
    Reg, VMInstruction,
    lowering::{BlockLoweringCtx, block::VMLowering},
};
use calibre_lir::ast::{LirDrop, LirLoad, LirMove, LirSpawn};
use calibre_parser::Span;

impl VMLowering for LirLoad {
    #[inline(always)]
    fn lower<'a>(self, env: &mut BlockLoweringCtx<'a>, span: Span) -> Reg {
        if let Some(reg) = env.map.get(&self.value)
            && reg != &env.null_reg
        {
            *reg
        } else {
            let idx = env.add_string(self.value);
            let dst = env.alloc_reg();
            env.emit(VMInstruction::LoadVar { dst, name: idx }, span);
            dst
        }
    }
}

impl VMLowering for LirMove {
    #[inline(always)]
    fn lower<'a>(self, env: &mut BlockLoweringCtx<'a>, span: Span) -> Reg {
        if env.captures.contains(&self.value) || !env.map.contains_key(&self.value) {
            let idx = env.add_string(self.value);
            let dst = env.alloc_reg();
            env.emit(VMInstruction::MoveVar { dst, name: idx }, span);
            dst
        } else {
            env.map
                .insert(self.value, env.null_reg)
                .unwrap_or(env.null_reg)
        }
    }
}

impl VMLowering for LirDrop {
    #[inline(always)]
    fn lower<'a>(self, env: &mut BlockLoweringCtx<'a>, span: Span) -> Reg {
        if env.captures.contains(&self.value) || !env.map.contains_key(&self.value) {
            let idx = env.add_string(self.value);
            env.emit(VMInstruction::DropVar { name: idx }, span);
        } else {
            env.map.insert(self.value, env.null_reg);
        }
        env.null_reg
    }
}

impl VMLowering for LirSpawn {
    #[inline(always)]
    fn lower<'a>(self, env: &mut BlockLoweringCtx<'a>, span: Span) -> Reg {
        let value = env.lower_node(*self.value, span);
        let dst = env.alloc_reg();
        env.emit(VMInstruction::Spawn { dst, callee: value }, span);
        dst
    }
}

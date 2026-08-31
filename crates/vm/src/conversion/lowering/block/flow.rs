/*
This file handles :
Range,
Closure
*/

use crate::conversion::{
    Reg, VMInstruction, VMLiteral,
    lowering::{BlockLoweringCtx, block::VMLowering},
};
use calibre_lir::ast::{LirClosure, LirRange};
use calibre_parser::Span;

impl VMLowering for LirRange {
    #[inline(always)]
    fn lower<'a>(self, env: &mut BlockLoweringCtx<'a>, span: Span) -> Reg {
        let from = env.lower_node(*self.from, span);
        let to = env.lower_node(*self.to, span);
        let dst = env.alloc_reg();
        env.emit(
            VMInstruction::Range {
                dst,
                from,
                to,
                inclusive: self.inclusive,
            },
            span,
        );
        dst
    }
}

impl VMLowering for LirClosure {
    #[inline(always)]
    fn lower<'a>(self, env: &mut BlockLoweringCtx<'a>, span: Span) -> Reg {
        env.block.local_literals.push(VMLiteral::Closure {
            label: self.label,
            captures: self.captures,
        });
        let lit = (env.block.local_literals.len() - 1) as u16;
        let dst = env.alloc_reg();
        env.emit(VMInstruction::LoadLiteral { dst, literal: lit }, span);
        dst
    }
}

/*
This file handles :
Binary,
Comparison,
Boolean,
As,
Is
*/

use crate::conversion::{
    Reg, VMInstruction,
    lowering::{BlockLoweringCtx, block::VMLowering},
};
use calibre_lir::ast::{LirAs, LirBinary, LirBoolean, LirComparison, LirIs};
use calibre_parser::Span;

impl VMLowering for LirBinary {
    #[inline(always)]
    fn lower<'a>(self, env: &mut BlockLoweringCtx<'a>, span: Span) -> Reg {
        let left = env.lower_node(*self.left, span);
        let right = env.lower_node(*self.right, span);
        let dst = env.alloc_reg();
        env.emit(VMInstruction::AccLoad { src: left }, span);
        env.emit(
            VMInstruction::AccBinary {
                op: self.operator,
                right,
            },
            span,
        );
        env.emit(VMInstruction::AccStore { dst }, span);
        dst
    }
}

impl VMLowering for LirComparison {
    #[inline(always)]
    fn lower<'a>(self, env: &mut BlockLoweringCtx<'a>, span: Span) -> Reg {
        let left = env.lower_node(*self.left, span);
        let right = env.lower_node(*self.right, span);
        let dst = env.alloc_reg();
        env.emit(
            VMInstruction::Comparison {
                dst,
                op: self.operator,
                left,
                right,
            },
            span,
        );
        dst
    }
}

impl VMLowering for LirBoolean {
    #[inline(always)]
    fn lower<'a>(self, env: &mut BlockLoweringCtx<'a>, span: Span) -> Reg {
        let left = env.lower_node(*self.left, span);
        let right = env.lower_node(*self.right, span);
        let dst = env.alloc_reg();

        env.emit(
            VMInstruction::Boolean {
                dst,
                op: self.operator,
                left,
                right,
            },
            span,
        );

        dst
    }
}

impl VMLowering for LirAs {
    #[inline(always)]
    fn lower<'a>(self, env: &mut BlockLoweringCtx<'a>, span: Span) -> Reg {
        let src = env.lower_node(*self.value, span);
        let dst = env.alloc_reg();
        env.emit(
            VMInstruction::As {
                dst,
                src,
                data_type: self.data_type,
                failure_mode: self.failure_mode,
            },
            span,
        );
        dst
    }
}

impl VMLowering for LirIs {
    #[inline(always)]
    fn lower<'a>(self, env: &mut BlockLoweringCtx<'a>, span: Span) -> Reg {
        let src = env.lower_node(*self.value, span);
        let dst = env.alloc_reg();
        env.emit(
            VMInstruction::Is {
                dst,
                src,
                data_type: self.data_type,
            },
            span,
        );
        dst
    }
}

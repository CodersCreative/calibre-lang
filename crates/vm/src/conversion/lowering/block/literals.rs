/*
This file handles :
Literal,
List,
Aggregate,
Enum
*/

use crate::conversion::{
    AggregateLayout, Reg, VMInstruction, VMLiteral,
    lowering::{BlockLoweringCtx, block::VMLowering},
};
use calibre_lir::ast::{LirAggregate, LirEnum, LirList, LirLiteral};
use calibre_parser::Span;

impl VMLowering for LirLiteral {
    #[inline(always)]
    fn lower<'a>(self, env: &mut BlockLoweringCtx<'a>, span: Span) -> Reg {
        if let LirLiteral::Null = self {
            return env.null_reg;
        }

        let lit = VMLiteral::from_lir_literal(self, env.big_consts);
        let lit = env.add_literal(lit);
        let dst = env.alloc_reg();
        env.emit(VMInstruction::LoadLiteral { dst, literal: lit }, span);

        dst
    }
}

impl VMLowering for LirList {
    #[inline(always)]
    fn lower<'a>(self, env: &mut BlockLoweringCtx<'a>, span: Span) -> Reg {
        let mut regs = Vec::with_capacity(self.values.len());

        for item in self.values {
            regs.push(env.lower_node(item, span));
        }

        let dst = env.alloc_reg();
        env.emit(VMInstruction::List { dst, items: regs }, span);
        dst
    }
}

impl VMLowering for LirAggregate {
    #[inline(always)]
    fn lower<'a>(self, env: &mut BlockLoweringCtx<'a>, span: Span) -> Reg {
        let mut layout = Vec::new();
        let mut values = Vec::new();

        for (k, item) in self.fields.0 {
            values.push(env.lower_node(item, span));
            layout.push(k.to_string());
        }

        env.block.aggregate_layouts.push(AggregateLayout {
            name: self.name,
            members: layout,
        });

        let index = env.block.aggregate_layouts.len() - 1;
        let dst = env.alloc_reg();

        env.emit(
            VMInstruction::Aggregate {
                dst,
                layout: index as u16,
                fields: values,
            },
            span,
        );

        dst
    }
}

impl VMLowering for LirEnum {
    #[inline(always)]
    fn lower<'a>(self, env: &mut BlockLoweringCtx<'a>, span: Span) -> Reg {
        let payload = self.payload.map(|v| env.lower_node(*v, span));
        let name = env.add_string(self.name.to_string());
        let dst = env.alloc_reg();
        env.emit(
            VMInstruction::Enum {
                dst,
                name,
                variant: self.variant as u16,
                payload,
            },
            span,
        );
        dst
    }
}

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
use ustr::Ustr;

impl VMLowering for LirLiteral {
    #[inline(always)]
    fn lower<'a>(self, env: &mut BlockLoweringCtx<'a>, span: Span) -> Reg {
        let dst = env.alloc_reg();
        self.lower_to(env, dst, span);
        dst
    }

    #[inline(always)]
    fn lower_to<'a>(self, env: &mut BlockLoweringCtx<'a>, target: Reg, span: Span) {
        if let LirLiteral::Null = self {
            if target != env.null_reg {
                env.emit(
                    VMInstruction::Copy {
                        dst: target,
                        src: env.null_reg,
                    },
                    span,
                );
            }
            return;
        }

        let lit = VMLiteral::from_lir_literal(self, env.big_consts);
        let lit = env.add_literal(lit);
        env.emit(
            VMInstruction::LoadLiteral {
                dst: target,
                literal: lit,
            },
            span,
        );
    }
}

impl VMLowering for LirList {
    #[inline(always)]
    fn lower<'a>(self, env: &mut BlockLoweringCtx<'a>, span: Span) -> Reg {
        let dst = env.alloc_reg();
        self.lower_to(env, dst, span);
        dst
    }

    #[inline(always)]
    fn lower_to<'a>(self, env: &mut BlockLoweringCtx<'a>, target: Reg, span: Span) {
        let regs = self
            .values
            .into_iter()
            .map(|item| env.lower_node(item, span))
            .collect();

        env.emit(
            VMInstruction::List {
                dst: target,
                items: regs,
            },
            span,
        );
    }
}

impl VMLowering for LirAggregate {
    #[inline(always)]
    fn lower<'a>(self, env: &mut BlockLoweringCtx<'a>, span: Span) -> Reg {
        let dst = env.alloc_reg();
        self.lower_to(env, dst, span);
        dst
    }

    #[inline(always)]
    fn lower_to<'a>(self, env: &mut BlockLoweringCtx<'a>, target: Reg, span: Span) {
        let mut layout = Vec::with_capacity(self.fields.0.len());
        let mut values = Vec::with_capacity(self.fields.0.len());

        for (k, item) in self.fields.0 {
            values.push(env.lower_node(item, span));
            layout.push(Ustr::from(&k));
        }

        env.block.aggregate_layouts.push(AggregateLayout {
            name: self.name,
            members: layout,
        });

        let index = env.block.aggregate_layouts.len() - 1;

        env.emit(
            VMInstruction::Aggregate {
                dst: target,
                layout: index as u16,
                fields: values,
            },
            span,
        );
    }
}

impl VMLowering for LirEnum {
    #[inline(always)]
    fn lower<'a>(self, env: &mut BlockLoweringCtx<'a>, span: Span) -> Reg {
        let dst = env.alloc_reg();
        self.lower_to(env, dst, span);
        dst
    }

    #[inline(always)]
    fn lower_to<'a>(self, env: &mut BlockLoweringCtx<'a>, target: Reg, span: Span) {
        let payload = self.payload.map(|v| env.lower_node(*v, span));
        let name = env.add_string(self.name);
        env.emit(
            VMInstruction::Enum {
                dst: target,
                name,
                variant: self.variant as u16,
                payload,
            },
            span,
        );
    }
}

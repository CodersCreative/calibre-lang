/*
This file handles :
Break,
Continue,
Return,
Conditional,
LoopDeclaration,
RangeDeclaration,
Emit
*/

use crate::{
    ast::{LirLoad, LirNodeType, LirRange, LirTerminator},
    environment::LirEnvironment,
    translate::LirLowering,
};
use calibre_mir::ast::{
    MiddleNodeType, MirBreak, MirConditional, MirContinue, MirEmit, MirLoop, MirRange, MirReturn,
};
use calibre_parser::Span;

impl LirLowering for MirBreak {
    #[inline(always)]
    fn lower<'a>(self, env: &mut LirEnvironment<'a>, span: Span) -> LirNodeType {
        env.jump_to_loop_target_if_present(span, self.label.as_ref(), true);
        LirNodeType::null()
    }
}

impl LirLowering for MirContinue {
    #[inline(always)]
    fn lower<'a>(self, env: &mut LirEnvironment<'a>, span: Span) -> LirNodeType {
        env.jump_to_loop_target_if_present(span, self.label.as_ref(), false);
        LirNodeType::null()
    }
}

impl LirLowering for MirReturn {
    #[inline(always)]
    fn lower<'a>(self, env: &mut LirEnvironment<'a>, span: Span) -> LirNodeType {
        if let Some(v) = self.value {
            if let MiddleNodeType::Conditional(MirConditional {
                comparison,
                then,
                otherwise,
            }) = v.node_type
            {
                let then_id = env.create_block();
                let else_id = env.create_block();
                let merge_id = env.create_block();

                let cond = env.lower_node(*comparison);
                env.set_terminator(LirTerminator::Branch {
                    span,
                    condition: cond,
                    then_block: then_id,
                    else_block: else_id,
                });

                env.switch_to(then_id);
                let _ = MirReturn { value: Some(then) }.lower(env, span);

                env.switch_to(else_id);
                let _ = MirReturn { value: otherwise }.lower(env, span);

                env.switch_to(merge_id);
                return LirNodeType::null();
            }

            let value_span = v.span;
            let val = env.lower_node(*v);
            env.emit_return_value(value_span, Some(val));
            LirNodeType::null()
        } else {
            env.emit_return_value(span, None);
            LirNodeType::null()
        }
    }
}

impl LirLowering for MirConditional {
    #[inline(always)]
    fn lower<'a>(self, env: &mut LirEnvironment<'a>, span: Span) -> LirNodeType {
        let then_id = env.create_block();
        let else_id = env.create_block();
        let merge_id = env.create_block();

        let temp = env.get_temp();
        env.declare_temp_null(span, temp);

        let cond = env.lower_node(*self.comparison);
        env.set_terminator(LirTerminator::Branch {
            span,
            condition: cond,
            then_block: then_id,
            else_block: else_id,
        });

        env.switch_to(then_id);
        let then_val = env.lower_node(*self.then);
        if env.current_block_open() {
            env.assign_temp_if_non_null(span, temp, then_val);
            env.jump_if_open(span, merge_id);
        }

        env.switch_to(else_id);
        let else_val = if let Some(alt) = self.otherwise {
            env.lower_node(*alt)
        } else {
            LirNodeType::null()
        };

        if env.current_block_open() {
            env.assign_temp_if_non_null(span, temp, else_val);
            env.jump_if_open(span, merge_id);
        }

        env.switch_to(merge_id);
        LirNodeType::Load(LirLoad { value: temp })
    }
}

impl LirLowering for MirLoop {
    #[inline(always)]
    fn lower<'a>(self, env: &mut LirEnvironment<'a>, span: Span) -> LirNodeType {
        let header_id = env.create_block();
        let body_id = env.create_block();
        let exit_id = env.create_block();

        if let Some(s) = self.state {
            env.lower_and_add_node(*s);
        }

        env.set_terminator(LirTerminator::Jump {
            span,
            target: header_id,
        });

        env.switch_to(header_id);
        env.set_terminator(LirTerminator::Jump {
            span,
            target: body_id,
        });

        env.loop_stack.push((header_id, exit_id, self.label));

        env.switch_to(body_id);
        env.lower_and_add_node(*self.body);
        env.set_terminator(LirTerminator::Jump {
            span,
            target: header_id,
        });

        env.loop_stack.pop();

        env.switch_to(exit_id);
        LirNodeType::null()
    }
}

impl LirLowering for MirRange {
    #[inline(always)]
    fn lower<'a>(self, env: &mut LirEnvironment<'a>, _span: Span) -> LirNodeType {
        let from = env.lower_node(*self.from);
        let to = env.lower_node(*self.to);
        LirNodeType::Range(LirRange {
            from: Box::new(from),
            to: Box::new(to),
            inclusive: self.inclusive,
        })
    }
}

impl LirLowering for MirEmit {
    #[inline(always)]
    fn lower<'a>(self, env: &mut LirEnvironment<'a>, _span: Span) -> LirNodeType {
        // TODO Add emit support
        env.lower_node(*self.value)
    }
}

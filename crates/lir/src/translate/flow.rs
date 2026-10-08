/*
This file handles :
Break,
Continue,
Return,
Conditional,
LoopDeclaration,
RangeDeclaration
*/

use crate::{
    ast::{LirLoad, LirNodeType, LirRange, LirTerminator},
    environment::LirEnvironment,
    translate::LirLowering,
};
use calibre_mir::ast::{
    MiddleNodeType, MirBreak, MirConditional, MirContinue, MirEmit, MirLoop, MirRange, MirReturn, types::MirDataType,
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
                ..
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
                    else_block: Some(else_id),
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
        let then_start = env.create_block();
        let else_start = env.create_block();

        // Create the merge.
        let merge_id = env.create_block();

        let mut temp = None;
        if self.data_type.as_ref().is_none_or(|x| !x.is_null()) {
            let tmp = env.get_temp();
            env.declare_temp_null(span, tmp.clone(), self.data_type.unwrap_or(MirDataType::Null));
            temp = Some(tmp)
        }

        let cond = env.lower_node(*self.comparison);
        env.set_terminator(LirTerminator::Branch {
            span,
            condition: cond,
            then_block: then_start,
            else_block: Some(else_start),
        });

        // Lower the then block
        env.switch_to(then_start);
        let then_val = env.lower_node(*self.then);

        if env.current_block_open() {
            if let Some(temp) = temp.clone() {
                env.assign_temp_if_non_null(span, temp, then_val);
            }
            env.jump_if_open(span, merge_id);
        }

        // Lower the else block
        env.switch_to(else_start);
        let else_val = if let Some(alt) = self.otherwise {
            env.lower_node(*alt)
        } else {
            LirNodeType::null()
        };

        if env.current_block_open() {

            if let Some(temp) = temp.clone() {
                env.assign_temp_if_non_null(span, temp.clone(), else_val);
            }
            env.jump_if_open(span, merge_id);
        }

        env.switch_to(merge_id);

        
            if let Some(temp) = temp.clone() {
        LirNodeType::Load(LirLoad { value: temp })
            }else{
                LirNodeType::null()
            }
    }
}

impl LirLowering for MirLoop {
    #[inline(always)]
    fn lower<'a>(self, env: &mut LirEnvironment<'a>, span: Span) -> LirNodeType {
        let body_id = env.create_block();
        let exit_id = env.create_block();

        env.set_terminator(LirTerminator::Jump {
            span,
            target: body_id,
        });

        env.loop_stack.push((body_id, exit_id, self.label));

        env.switch_to(body_id);
        env.lower_and_add_node(*self.body);
        env.set_terminator(LirTerminator::Jump {
            span,
            target: body_id,
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
        env.lower_node(*self.value)
    }

    fn lower_lvalue<'a>(self, env: &mut LirEnvironment<'a>, _span: Span) -> crate::ast::LirLValue
    where
        Self: Sized,
    {
        env.lower_lvalue(*self.value)
    }
}

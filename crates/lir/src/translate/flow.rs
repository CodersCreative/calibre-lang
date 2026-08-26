/*
This file handles : 
Break,
Continue,
Return,
Conditional,
LoopDeclaration,
RangeDeclaration
*/

use calibre_mir::ast::{MirBreak, MirContinue};
use calibre_parser::Span;
use crate::{ast::LirNodeType, environment::LirEnvironment, translate::LirLowering};

impl LirLowering for MirContinue {
    fn lower<'a>(self, env: &mut LirEnvironment<'a>, span : Span) -> LirNodeType {
        env.jump_to_loop_target_if_present(span, LirEnvironment::loop_label(&self.label), false);
        LirNodeType::null()
    }
}

impl LirLowering for MirBreak {
    fn lower<'a>(self, env: &mut LirEnvironment<'a>, span : Span) -> LirNodeType {
        env.jump_to_loop_target_if_present(span, LirEnvironment::loop_label(&self.label), true);
        LirNodeType::null()
    }
}
/*
This file handles :
Move,
Spawn,
DerefStatement,
RefStatement,
Drop
*/

use crate::{
    ast::{LirLValue, LirNodeType},
    environment::LirEnvironment,
    translate::LirLowering,
};
use calibre_mir::ast::{
    MiddleNodeType, MirDeref, MirDrop, MirIdentifier, MirMove, MirRef, MirSpawn,
};
use calibre_parser::Span;

impl LirLowering for MirMove {
    #[inline(always)]
    fn lower<'a>(self, _env: &mut LirEnvironment<'a>, _span: Span) -> LirNodeType {
        LirNodeType::Move(self.identifier.to_string().into_boxed_str())
    }
}

impl LirLowering for MirSpawn {
    #[inline(always)]
    fn lower<'a>(self, env: &mut LirEnvironment<'a>, _span: Span) -> crate::ast::LirNodeType {
        LirNodeType::Spawn {
            callee: Box::new(env.lower_node(*self.value)),
        }
    }
}

impl LirLowering for MirDeref {
    #[inline(always)]
    fn lower<'a>(self, env: &mut LirEnvironment<'a>, _span: Span) -> LirNodeType {
        LirNodeType::Deref(Box::new(env.lower_node(*self.value)))
    }

    fn lower_lvalue<'a>(self, env: &mut LirEnvironment<'a>, _span: Span) -> LirLValue
    where
        Self: Sized,
    {
        LirLValue::Ptr(Box::new(env.lower_node(*self.value)))
    }
}

impl LirLowering for MirRef {
    #[inline(always)]
    fn lower<'a>(self, env: &mut LirEnvironment<'a>, _span: Span) -> LirNodeType {
        if let MiddleNodeType::Identifier(MirIdentifier { identifier }) = self.value.node_type {
            LirNodeType::RefLoad(identifier.text.into_boxed_str())
        } else {
            LirNodeType::Ref(Box::new(env.lower_node(*self.value)))
        }
    }
}

impl LirLowering for MirDrop {
    #[inline(always)]
    fn lower<'a>(self, _env: &mut LirEnvironment<'a>, _span: Span) -> LirNodeType {
        LirNodeType::Drop(self.identifier.to_string().into_boxed_str())
    }
}

/*
This file handles :
Move,
Spawn,
DerefStatement,
RefStatement,
Drop
*/

use crate::{
    ast::{LirDeref, LirDrop, LirLValue, LirMove, LirNodeType, LirRef, LirRefLoad, LirSpawn},
    environment::{LirEnvironment, LirId},
    translate::LirLowering,
};
use calibre_mir::ast::{
    MiddleNodeType, MirDeref, MirDrop, MirIdentifier, MirMove, MirRef, MirSpawn,
};
use calibre_parser::Span;

impl LirLowering for MirMove {
    #[inline(always)]
    fn lower<'a>(self, env: &mut LirEnvironment<'a>, span: Span) -> LirId {
        env.add(
            LirNodeType::Move(LirMove {
                value: self.identifier.to_string().into_boxed_str(),
            }),
            span,
        )
    }
}

impl LirLowering for MirSpawn {
    #[inline(always)]
    fn lower<'a>(self, env: &mut LirEnvironment<'a>, span: Span) -> LirId {
        let value = env.lower_node(*self.value);

        env.add_with_children(
            LirNodeType::Spawn(LirSpawn { value }),
            std::iter::once(value),
            span,
        )
    }
}

impl LirLowering for MirDeref {
    #[inline(always)]
    fn lower<'a>(self, env: &mut LirEnvironment<'a>, span: Span) -> LirId {
        let value = env.lower_node(*self.value);

        env.add_with_children(
            LirNodeType::Deref(LirDeref { value }),
            std::iter::once(value),
            span,
        )
    }

    fn lower_lvalue<'a>(self, env: &mut LirEnvironment<'a>, _span: Span) -> LirLValue
    where
        Self: Sized,
    {
        LirLValue::Ptr(env.lower_node(*self.value))
    }
}

impl LirLowering for MirRef {
    #[inline(always)]
    fn lower<'a>(self, env: &mut LirEnvironment<'a>, span: Span) -> LirId {
        if let MiddleNodeType::Identifier(MirIdentifier { identifier }) = self.value.node_type {
            env.add(
                LirNodeType::RefLoad(LirRefLoad {
                    value: identifier.text.into_boxed_str(),
                }),
                span,
            )
        } else {
            let value = env.lower_node(*self.value);

            env.add_with_children(
                LirNodeType::Ref(LirRef { value }),
                std::iter::once(value),
                span,
            )
        }
    }
}

impl LirLowering for MirDrop {
    #[inline(always)]
    fn lower<'a>(self, env: &mut LirEnvironment<'a>, span: Span) -> LirId {
        env.add(
            LirNodeType::Drop(LirDrop {
                value: self.identifier.to_string().into_boxed_str(),
            }),
            span,
        )
    }
}

/*
This file handles :
FieldAccess,
ScopeAccess,
IndexAccess,
CallExpression
*/

use crate::{
    ast::{LirCall, LirIndex, LirLValue, LirLoad, LirMember, LirMove, LirNodeType},
    environment::LirEnvironment,
    translate::LirLowering,
};
use calibre_mir::ast::{MirCall, MirField, MirIndex};
use calibre_parser::{Span, ast::types::ParserInnerType};

impl LirLowering for MirField {
    #[inline(always)]
    fn lower<'a>(self, env: &mut LirEnvironment<'a>, _span: Span) -> LirNodeType {
        LirNodeType::Member(LirMember {
            base: Box::new(env.lower_node(*self.base)),
            field: self.field,
        })
    }

    #[inline(always)]
    fn lower_lvalue<'a>(self, env: &mut LirEnvironment<'a>, _span: Span) -> LirLValue
    where
        Self: Sized,
    {
        LirLValue::Ptr(Box::new(LirNodeType::Member(LirMember {
            base: Box::new(env.lower_node(*self.base)),
            field: self.field,
        })))
    }
}

impl LirLowering for MirIndex {
    #[inline(always)]
    fn lower<'a>(self, env: &mut LirEnvironment<'a>, _span: Span) -> LirNodeType {
        LirNodeType::Index(LirIndex {
            base: Box::new(env.lower_node(*self.base)),
            index: Box::new(env.lower_node(*self.index)),
        })
    }

    #[inline(always)]
    fn lower_lvalue<'a>(self, env: &mut LirEnvironment<'a>, _span: Span) -> LirLValue
    where
        Self: Sized,
    {
        LirLValue::Ptr(Box::new(LirNodeType::Index(LirIndex {
            base: Box::new(env.lower_node(*self.base)),
            index: Box::new(env.lower_node(*self.index)),
        })))
    }
}

impl LirLowering for MirCall {
    #[inline(always)]
    fn lower<'a>(self, env: &mut LirEnvironment<'a>, _span: Span) -> LirNodeType {
        let mut returns_value = true;

        let l_caller = env.lower_node(*self.caller);
        let l_args = env.lower_nodes(self.args);

        if let LirNodeType::Load(LirLoad { value }) | LirNodeType::Move(LirMove { value }) =
            &l_caller
            && let Some(var) = env.env.symbols.variables.get(value)
            && let ParserInnerType::Function { return_type, .. }
            | ParserInnerType::NativeFunction { return_type, .. } = &var.data_type.data_type
            && return_type.is_null()
        {
            returns_value = false;
        }

        LirNodeType::Call(LirCall {
            caller: Box::new(l_caller),
            args: l_args,
            returns_value,
        })
    }
}

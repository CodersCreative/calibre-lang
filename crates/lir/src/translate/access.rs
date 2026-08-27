/*
This file handles :
FieldAccess,
ScopeAccess,
IndexAccess,
CallExpression
*/

use crate::{
    ast::{LirCall, LirIndex, LirLValue, LirLoad, LirMember, LirMove, LirNodeType, LirRef},
    environment::LirEnvironment,
    translate::LirLowering,
};
use calibre_mir::ast::{MirCall, MirField, MirIndex, MirScope};
use calibre_parser::{Span, ast::types::ParserInnerType};

impl LirLowering for MirField {
    #[inline(always)]
    fn lower<'a>(self, env: &mut LirEnvironment<'a>, _span: Span) -> LirNodeType {
        LirNodeType::Member(LirMember {
            base: Box::new(env.lower_node(*self.base)),
            field: self.field.text.into_boxed_str(),
        })
    }

    #[inline(always)]
    fn lower_lvalue<'a>(self, env: &mut LirEnvironment<'a>, _span: Span) -> LirLValue
    where
        Self: Sized,
    {
        LirLValue::Ptr(Box::new(LirNodeType::Member(LirMember {
            base: Box::new(env.lower_node(*self.base)),
            field: self.field.text.into_boxed_str(),
        })))
    }
}

impl LirLowering for MirScope {
    #[inline(always)]
    fn lower<'a>(self, env: &mut LirEnvironment<'a>, _span: Span) -> LirNodeType {
        LirNodeType::Member(LirMember {
            base: Box::new(env.lower_node(*self.base)),
            field: self.field.text.into_boxed_str(),
        })
    }

    #[inline(always)]
    fn lower_lvalue<'a>(self, env: &mut LirEnvironment<'a>, _span: Span) -> LirLValue
    where
        Self: Sized,
    {
        LirLValue::Ptr(Box::new(LirNodeType::Member(LirMember {
            base: Box::new(env.lower_node(*self.base)),
            field: self.field.text.into_boxed_str(),
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
        let mut needs_ref_first_arg = false;

        let l_caller = env.lower_node(*self.caller);
        let mut l_args = env.lower_nodes(self.args);

        if let LirNodeType::Load(LirLoad { value }) | LirNodeType::Move(LirMove { value }) =
            &l_caller
            && let Some(var) = env.env.symbols.variables.get(value.as_ref())
            && let ParserInnerType::Function { parameters, .. } = &var.data_type.data_type
        {
            if let Some(first) = parameters.first() {
                needs_ref_first_arg = matches!(first.data_type, ParserInnerType::Ref(_, _))
            }
            let expected = parameters.len();
            while l_args.len() > expected {
                l_args.remove(0);
            }
        }

        if needs_ref_first_arg
            && let Some(first_arg) = l_args.get_mut(0)
            && matches!(first_arg, LirNodeType::Load(_))
        {
            *first_arg = LirNodeType::Ref(LirRef {
                value: Box::new(std::mem::replace(first_arg, LirNodeType::null())),
            });
        }

        LirNodeType::Call(LirCall {
            caller: Box::new(l_caller),
            args: l_args,
        })
    }
}

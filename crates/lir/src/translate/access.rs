/*
This file handles :
FieldAccess,
ScopeAccess,
IndexAccess,
CallExpression
*/

use crate::{
    ast::{LirCall, LirIndex, LirLValue, LirLoad, LirMember, LirMove, LirNodeType, LirRef},
    environment::{LirEnvironment, LirId},
    translate::LirLowering,
};
use calibre_mir::ast::{MirCall, MirField, MirIndex, MirScope};
use calibre_parser::{Span, ast::types::ParserInnerType};

impl LirLowering for MirField {
    #[inline(always)]
    fn lower<'a>(self, env: &mut LirEnvironment<'a>, span: Span) -> LirId {
        let base = env.lower_node(*self.base);

        let node = LirNodeType::Member(LirMember {
            base,
            field: self.field.text.into_boxed_str(),
        });

        env.add_with_children(node, std::iter::once(base), span)
    }

    #[inline(always)]
    fn lower_lvalue<'a>(self, env: &mut LirEnvironment<'a>, span: Span) -> LirLValue
    where
        Self: Sized,
    {
        LirLValue::Ptr(self.lower(env, span))
    }
}

impl LirLowering for MirScope {
    #[inline(always)]
    fn lower<'a>(self, env: &mut LirEnvironment<'a>, span: Span) -> LirId {
        let base = env.lower_node(*self.base);

        let node = LirNodeType::Member(LirMember {
            base,
            field: self.field.text.into_boxed_str(),
        });

        env.add_with_children(node, [base].into_iter(), span)
    }

    #[inline(always)]
    fn lower_lvalue<'a>(self, env: &mut LirEnvironment<'a>, span: Span) -> LirLValue
    where
        Self: Sized,
    {
        LirLValue::Ptr(self.lower(env, span))
    }
}

impl LirLowering for MirIndex {
    #[inline(always)]
    fn lower<'a>(self, env: &mut LirEnvironment<'a>, span: Span) -> LirId {
        let base = env.lower_node(*self.base);
        let index = env.lower_node(*self.index);

        let node = LirNodeType::Index(LirIndex { base, index });

        env.add_with_children(node, [base, index].into_iter(), span)
    }

    #[inline(always)]
    fn lower_lvalue<'a>(self, env: &mut LirEnvironment<'a>, span: Span) -> LirLValue
    where
        Self: Sized,
    {
        LirLValue::Ptr(self.lower(env, span))
    }
}

impl LirLowering for MirCall {
    #[inline(always)]
    fn lower<'a>(self, env: &mut LirEnvironment<'a>, span: Span) -> LirId {
        let mut needs_ref_first_arg = false;

        let l_caller = env.lower_node(*self.caller);
        let mut l_args = env.lower_nodes(self.args);

        if let Some(LirNodeType::Load(LirLoad { value }) | LirNodeType::Move(LirMove { value })) =
            env.registry.nodes.get(l_caller)
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
            && let Some(first_arg) = l_args.get(0)
            && matches!(
                env.registry.nodes.get(*first_arg),
                Some(LirNodeType::Load(_))
            )
        {
            let first_arg_id = *first_arg;
            env.add_with_children(
                LirNodeType::Ref(LirRef {
                    value: first_arg_id,
                }),
                std::iter::once(first_arg_id),
                span,
            );
        }

        env.add_with_children(
            LirNodeType::Call(LirCall {
                caller: l_caller,
                args: l_args.clone(),
            }),
            std::iter::once(l_caller).chain(l_args.into_iter()),
            span,
        )
    }
}

/*
This file handles :
FieldAccess,
ScopeAccess,
IndexAccess,
CallExpression
*/

use crate::{
    ast::{
        LirCall, LirDeref, LirDiscriminant, LirIndex, LirLValue, LirLoad, LirMember, LirMove,
        LirNodeType,
    },
    environment::LirEnvironment,
    translate::LirLowering,
};
use calibre_mir::ast::{MirCall, MirDiscriminant, MirField, MirIndex, types::MirDataType};
use calibre_parser::Span;

impl LirLowering for MirDiscriminant {
    fn lower<'a>(self, env: &mut LirEnvironment<'a>, _span: Span) -> LirNodeType {
        LirNodeType::Discriminant(LirDiscriminant {
            value: Box::new(env.lower_node(*self.value)),
        })
    }
}

impl LirLowering for MirField {
    #[inline(always)]
    fn lower<'a>(self, env: &mut LirEnvironment<'a>, _span: Span) -> LirNodeType {
        LirNodeType::Deref(LirDeref {
            value: Box::new(LirNodeType::Member(LirMember {
                base: Box::new(env.lower_node(*self.base)),
                field: self.field,
            })),
        })
    }

    #[inline(always)]
    fn lower_lvalue<'a>(self, env: &mut LirEnvironment<'a>, _span: Span) -> Option<LirLValue>
    where
        Self: Sized,
    {
        Some(LirLValue::Ptr(Box::new(LirNodeType::Member(LirMember {
            base: Box::new(
                env.lower_lvalue(*self.base.clone())
                    .map(LirNodeType::from)
                    .unwrap_or_else(|| env.lower_node(*self.base)),
            ),
            field: self.field,
        }))))
    }
}

impl LirLowering for MirIndex {
    #[inline(always)]
    fn lower<'a>(self, env: &mut LirEnvironment<'a>, _span: Span) -> LirNodeType {
        LirNodeType::Deref(LirDeref {
            value: Box::new(LirNodeType::Index(LirIndex {
                base: Box::new(env.lower_node(*self.base)),
                index: Box::new(env.lower_node(*self.index)),
                panic: self.panic,
            })),
        })
    }

    #[inline(always)]
    fn lower_lvalue<'a>(self, env: &mut LirEnvironment<'a>, _span: Span) -> Option<LirLValue>
    where
        Self: Sized,
    {
        Some(LirLValue::Ptr(Box::new(LirNodeType::Index(LirIndex {
            base: Box::new(
                env.lower_lvalue(*self.base.clone())
                    .map(LirNodeType::from)
                    .unwrap_or_else(|| env.lower_node(*self.base)),
            ),
            index: Box::new(env.lower_node(*self.index)),
            panic: true,
        }))))
    }
}

impl LirLowering for MirCall {
    #[inline(always)]
    fn lower<'a>(self, env: &mut LirEnvironment<'a>, _span: Span) -> LirNodeType {
        let mut returns_value = true;
        let mut return_type = None;

        let l_caller = env.lower_node(*self.caller);
        let l_args = env.lower_nodes(self.args);

        if let LirNodeType::Load(LirLoad { value }) | LirNodeType::Move(LirMove { value }) =
            &l_caller
            && let Some(var) = env.env.symbols.variables.get(value)
        {
            match &var.data_type {
                MirDataType::Function {
                    return_type: ty, ..
                }
                | MirDataType::NativeFunction {
                    return_type: ty, ..
                } => {
                    if ty.is_null() {
                        returns_value = false;
                    } else {
                        return_type = Some((**ty).clone());
                    }
                }
                _ => {}
            }
        }

        LirNodeType::Call(LirCall {
            caller: Box::new(l_caller),
            args: l_args,
            returns_value,
            return_type,
        })
    }
}

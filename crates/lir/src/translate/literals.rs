/*
This file handles :
Identifier,
StringLiteral,
ListLiteral,
CharLiteral,
FloatLiteral,
IntLiteral,
BigLiteral
*/

use crate::{
    ast::{LirLValue, LirList, LirLiteral, LirLoad, LirNodeType},
    environment::{LirEnvironment, LirId},
    translate::LirLowering,
};
use calibre_mir::ast::{MirBig, MirChar, MirFloat, MirIdentifier, MirInt, MirList, MirString};
use calibre_parser::{Span, ast::idents::IntLiteralType};

impl LirLowering for MirIdentifier {
    #[inline(always)]
    fn lower<'a>(self, env: &mut LirEnvironment<'a>, span: Span) -> LirId {
        env.add(
            LirNodeType::Load(LirLoad {
                value: self.identifier.to_string().into_boxed_str(),
            }),
            span,
        )
    }

    #[inline(always)]
    fn lower_lvalue<'a>(self, _env: &mut LirEnvironment<'a>, _span: Span) -> LirLValue
    where
        Self: Sized,
    {
        LirLValue::Var(self.identifier.to_string().into_boxed_str())
    }
}

impl LirLowering for MirString {
    #[inline(always)]
    fn lower<'a>(self, env: &mut LirEnvironment<'a>, span: Span) -> LirId {
        env.add(
            LirNodeType::Literal(LirLiteral::String(self.value.to_string())),
            span,
        )
    }
}

impl LirLowering for MirList {
    #[inline(always)]
    fn lower<'a>(self, env: &mut LirEnvironment<'a>, span: Span) -> LirId {
        let values = env.lower_nodes(self.values);

        env.add_with_children(
            LirNodeType::List(LirList {
                values: values.clone(),
                data_type: self.data_type,
            }),
            values.into_iter(),
            span,
        )
    }
}

impl LirLowering for MirChar {
    #[inline(always)]
    fn lower<'a>(self, env: &mut LirEnvironment<'a>, span: Span) -> LirId {
        env.add(LirNodeType::Literal(LirLiteral::Char(self.value)), span)
    }
}

impl LirLowering for MirFloat {
    #[inline(always)]
    fn lower<'a>(self, env: &mut LirEnvironment<'a>, span: Span) -> LirId {
        env.add(LirNodeType::Literal(LirLiteral::Float(self.value)), span)
    }
}

impl LirLowering for MirInt {
    #[inline(always)]
    fn lower<'a>(self, env: &mut LirEnvironment<'a>, span: Span) -> LirId {
        let node = match self.value.int_type {
            IntLiteralType::Int => LirNodeType::Literal(LirLiteral::Int(self.value.value)),
            IntLiteralType::UInt => LirNodeType::Literal(LirLiteral::UInt(self.value.value as u64)),
            IntLiteralType::Byte => LirNodeType::Literal(LirLiteral::Byte(self.value.value as u8)),
        };

        env.add(node, span)
    }
}

impl LirLowering for MirBig {
    #[inline(always)]
    fn lower<'a>(self, env: &mut LirEnvironment<'a>, span: Span) -> LirId {
        env.add(LirNodeType::Literal(LirLiteral::Big(self.value.text)), span)
    }
}

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
    ast::{LirLValue, LirLiteral, LirNodeType},
    environment::LirEnvironment,
    translate::LirLowering,
};
use calibre_mir::ast::{MirBig, MirChar, MirFloat, MirIdentifier, MirInt, MirList, MirString};
use calibre_parser::{Span, ast::idents::IntLiteralType};

impl LirLowering for MirIdentifier {
    #[inline(always)]
    fn lower<'a>(self, _env: &mut LirEnvironment<'a>, _span: Span) -> LirNodeType {
        LirNodeType::Load(self.identifier.to_string().into_boxed_str())
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
    fn lower<'a>(self, _env: &mut LirEnvironment<'a>, _span: Span) -> LirNodeType {
        LirNodeType::Literal(LirLiteral::String(self.value.to_string()))
    }
}

impl LirLowering for MirList {
    #[inline(always)]
    fn lower<'a>(self, env: &mut LirEnvironment<'a>, _span: Span) -> LirNodeType {
        LirNodeType::List {
            elements: env.lower_nodes(self.values),
            data_type: self.data_type,
        }
    }
}

impl LirLowering for MirChar {
    #[inline(always)]
    fn lower<'a>(self, _env: &mut LirEnvironment<'a>, _span: Span) -> LirNodeType {
        LirNodeType::Literal(LirLiteral::Char(self.value))
    }
}

impl LirLowering for MirFloat {
    #[inline(always)]
    fn lower<'a>(self, _env: &mut LirEnvironment<'a>, _span: Span) -> LirNodeType {
        LirNodeType::Literal(LirLiteral::Float(self.value))
    }
}

impl LirLowering for MirInt {
    #[inline(always)]
    fn lower<'a>(self, _env: &mut LirEnvironment<'a>, _span: Span) -> LirNodeType {
        match self.value.int_type {
            IntLiteralType::Int => LirNodeType::Literal(LirLiteral::Int(self.value.value)),
            IntLiteralType::UInt => LirNodeType::Literal(LirLiteral::UInt(self.value.value as u64)),
            IntLiteralType::Byte => LirNodeType::Literal(LirLiteral::Byte(self.value.value as u8)),
        }
    }
}

impl LirLowering for MirBig {
    #[inline(always)]
    fn lower<'a>(self, _env: &mut LirEnvironment<'a>, _span: Span) -> LirNodeType {
        LirNodeType::Literal(LirLiteral::Big(self.value.text))
    }
}

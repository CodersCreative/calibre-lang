use crate::{
    ast::{
        MirBig, MirChar, MirFloat, MirIdentifier, MirInt, MirList, MirString, types::MirDataType,
        typing::MirTypable,
    },
    environment::MiddleEnvironment,
    scoping::ScopeId,
};
use calibre_parser::{Span, ast::idents::IntLiteralType};

impl MirTypable for MirIdentifier {
    fn mir_type_of(
        &self,
        env: &mut MiddleEnvironment,
        _scope: ScopeId,
        _span: Span,
    ) -> Option<MirDataType> {
        env.symbols
            .variables
            .get(&self.identifier)
            .map(|x| x.data_type.clone())
    }
}

impl MirTypable for MirString {
    fn mir_type_of(
        &self,
        _env: &mut MiddleEnvironment,
        _scope: ScopeId,
        _span: Span,
    ) -> Option<MirDataType> {
        Some(MirDataType::Str)
    }
}

impl MirTypable for MirList {
    fn mir_type_of(
        &self,
        _env: &mut MiddleEnvironment,
        _scope: ScopeId,
        _span: Span,
    ) -> Option<MirDataType> {
        Some(MirDataType::List(Box::new(self.data_type.clone())))
    }
}

impl MirTypable for MirChar {
    fn mir_type_of(
        &self,
        _env: &mut MiddleEnvironment,
        _scope: ScopeId,
        _span: Span,
    ) -> Option<MirDataType> {
        Some(MirDataType::Char)
    }
}

impl MirTypable for MirFloat {
    fn mir_type_of(
        &self,
        _env: &mut MiddleEnvironment,
        _scope: ScopeId,
        _span: Span,
    ) -> Option<MirDataType> {
        Some(MirDataType::Float)
    }
}

impl MirTypable for MirInt {
    fn mir_type_of(
        &self,
        _env: &mut MiddleEnvironment,
        _scope: ScopeId,
        _span: Span,
    ) -> Option<MirDataType> {
        Some(match self.value.int_type {
            IntLiteralType::Byte => MirDataType::Byte,
            IntLiteralType::UInt => MirDataType::UInt,
            IntLiteralType::Int => MirDataType::Int,
        })
    }
}

impl MirTypable for MirBig {
    fn mir_type_of(
        &self,
        _env: &mut MiddleEnvironment,
        _scope: ScopeId,
        _span: Span,
    ) -> Option<MirDataType> {
        Some(MirDataType::Big)
    }
}

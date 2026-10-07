use crate::{
    ast::{MirAggregate, MirAssignment, MirEnum, types::MirDataType, typing::MirTypable},
    environment::MiddleEnvironment,
    scoping::ScopeId,
};
use calibre_parser::Span;

impl MirTypable for MirAssignment {
    fn mir_type_of(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Option<MirDataType> {
        self.identifier
            .mir_type_of(env, scope, span)
            .or_else(|| self.value.mir_type_of(env, scope, span))
    }
}

impl MirTypable for MirAggregate {
    fn mir_type_of(
        &self,
        _env: &mut MiddleEnvironment,
        _scope: ScopeId,
        _span: Span,
    ) -> Option<MirDataType> {
        self.identifier.as_ref().map(|x| MirDataType::Struct {
            identifier: x.clone(),
            generic_types: Vec::new(),
        })
    }
}

impl MirTypable for MirEnum {
    fn mir_type_of(
        &self,
        _env: &mut MiddleEnvironment,
        _scope: ScopeId,
        _span: Span,
    ) -> Option<MirDataType> {
        self.identifier.as_ref().map(|x| MirDataType::Struct {
            identifier: x.clone(),
            generic_types: Vec::new(),
        })
    }
}

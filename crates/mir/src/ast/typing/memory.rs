use crate::{
    ast::{MirDeref, MirDrop, MirMove, MirRef, MirSpawn, types::MirDataType, typing::MirTypable},
    environment::MiddleEnvironment,
    scoping::ScopeId,
    symbols::resolve::ResolutionOptions,
};
use calibre_parser::Span;

impl MirTypable for MirMove {
    fn mir_type_of(
        &self,
        env: &mut MiddleEnvironment,
        _scope: ScopeId,
        _span: Span,
    ) -> Option<MirDataType> {
        env.symbols
            .variables
            .get(&self.identifier)
            .map(|x| x.data_type.unwrap_all_refs().clone())
    }
}

impl MirTypable for MirSpawn {
    fn mir_type_of(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        _span: Span,
    ) -> Option<MirDataType> {
        Some(MirDataType::Struct {
            identifier: env
                .resolve(scope, &"WaitGroup", ResolutionOptions::typing())
                .ok()?
                .unwrap_typing(),
            generic_types: Vec::new(),
        })
    }
}

impl MirTypable for MirDeref {
    fn mir_type_of(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Option<MirDataType> {
        self.value
            .mir_type_of(env, scope, span)
            .map(|x| x.unwrap_all_refs().clone())
    }
}

impl MirTypable for MirRef {
    fn mir_type_of(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Option<MirDataType> {
        Some(MirDataType::Ref(
            Box::new(
                self.value
                    .mir_type_of(env, scope, span)?
                    .unwrap_all_refs()
                    .clone(),
            ),
            self.mutability,
        ))
    }
}

impl MirTypable for MirDrop {
    fn mir_type_of(
        &self,
        _env: &mut MiddleEnvironment,
        _scope: ScopeId,
        _span: Span,
    ) -> Option<MirDataType> {
        None
    }
}

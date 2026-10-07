use crate::{
    ast::{
        MirBreak, MirConditional, MirContinue, MirEmit, MirLoop, MirRange, MirReturn,
        types::MirDataType, typing::MirTypable,
    },
    environment::MiddleEnvironment,
    scoping::ScopeId,
};
use calibre_parser::Span;

impl MirTypable for MirBreak {
    fn mir_type_of(
        &self,
        _env: &mut MiddleEnvironment,
        _scope: ScopeId,
        _span: Span,
    ) -> Option<MirDataType> {
        None
    }
}

impl MirTypable for MirContinue {
    fn mir_type_of(
        &self,
        _env: &mut MiddleEnvironment,
        _scope: ScopeId,
        _span: Span,
    ) -> Option<MirDataType> {
        None
    }
}

impl MirTypable for MirReturn {
    fn mir_type_of(
        &self,
        _env: &mut MiddleEnvironment,
        _scope: ScopeId,
        _span: Span,
    ) -> Option<MirDataType> {
        None
    }
}

impl MirTypable for MirConditional {
    fn mir_type_of(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Option<MirDataType> {
        if let Some(otherwise) = &self.otherwise {
            let then_ty = self.then.mir_type_of(env, scope, span);
            let else_ty = otherwise.mir_type_of(env, scope, span);

            match (then_ty, else_ty) {
                (Some(a), Some(b)) if a.loose_eq(&b) => Some(a),
                (Some(a), Some(b)) if a.is_null() => Some(b),
                (Some(a), Some(b)) if b.is_null() => Some(a),
                (Some(a), _) | (_, Some(a)) => Some(a),
                _ => None,
            }
        } else {
            self.then.mir_type_of(env, scope, span)
        }
    }
}

impl MirTypable for MirLoop {
    fn mir_type_of(
        &self,
        _env: &mut MiddleEnvironment,
        _scope: ScopeId,
        _span: Span,
    ) -> Option<MirDataType> {
        None
    }
}

impl MirTypable for MirRange {
    fn mir_type_of(
        &self,
        _env: &mut MiddleEnvironment,
        _scope: ScopeId,
        _span: Span,
    ) -> Option<MirDataType> {
        Some(MirDataType::Range)
    }
}

impl MirTypable for MirEmit {
    fn mir_type_of(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Option<MirDataType> {
        self.value.mir_type_of(env, scope, span)
    }
}

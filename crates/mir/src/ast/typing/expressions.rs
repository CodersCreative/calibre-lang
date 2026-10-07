use crate::{
    ast::{
        MirAs, MirBinary, MirBoolean, MirComparison, MirIs, MirNeg, types::MirDataType,
        typing::MirTypable,
    },
    environment::MiddleEnvironment,
    scoping::ScopeId,
};
use calibre_parser::{
    Span,
    ast::{binary::BinaryOperator, nodes::binary::AsFailureMode},
};

impl MirTypable for MirBinary {
    fn mir_type_of(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Option<MirDataType> {
        let left = self.left.mir_type_of(env, scope, span);
        let right = self.right.mir_type_of(env, scope, span);

        #[allow(clippy::single_match)]
        match &self.operator {
            BinaryOperator::BitAnd
                if left.as_ref().is_some_and(|x| x.loose_eq(&MirDataType::Str))
                    || right
                        .as_ref()
                        .is_some_and(|x| x.loose_eq(&MirDataType::Str))
                    || left
                        .as_ref()
                        .is_some_and(|x| x.loose_eq(&MirDataType::Char))
                    || right
                        .as_ref()
                        .is_some_and(|x| x.loose_eq(&MirDataType::Char)) =>
            {
                Some(MirDataType::Str)
            }

            _ => left.or(right),
        }
    }
}

impl MirTypable for MirComparison {
    fn mir_type_of(
        &self,
        _env: &mut MiddleEnvironment,
        _scope: ScopeId,
        _span: Span,
    ) -> Option<MirDataType> {
        Some(MirDataType::Bool)
    }
}

impl MirTypable for MirBoolean {
    fn mir_type_of(
        &self,
        _env: &mut MiddleEnvironment,
        _scope: ScopeId,
        _span: Span,
    ) -> Option<MirDataType> {
        Some(MirDataType::Bool)
    }
}

impl MirTypable for MirNeg {
    fn mir_type_of(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Option<MirDataType> {
        self.value.mir_type_of(env, scope, span)
    }
}

impl MirTypable for MirAs {
    fn mir_type_of(
        &self,
        _env: &mut MiddleEnvironment,
        _scope: ScopeId,
        _span: Span,
    ) -> Option<MirDataType> {
        match &self.failure_mode {
            AsFailureMode::Panic => Some(self.data_type.clone()),
            AsFailureMode::Option => Some(MirDataType::Option(Box::new(self.data_type.clone()))),
            AsFailureMode::Result => Some(MirDataType::Result {
                ok: Box::new(self.data_type.clone()),
                err: Box::new(MirDataType::Dynamic),
            }),
        }
    }
}

impl MirTypable for MirIs {
    fn mir_type_of(
        &self,
        _env: &mut MiddleEnvironment,
        _scope: ScopeId,
        _span: Span,
    ) -> Option<MirDataType> {
        Some(MirDataType::Bool)
    }
}

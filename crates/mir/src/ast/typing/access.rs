use crate::{
    ast::{MirCall, MirDiscriminant, MirField, MirIndex, types::MirDataType, typing::MirTypable},
    environment::MiddleEnvironment,
    scoping::ScopeId,
    translate::MirLowering,
};
use calibre_parser::Span;

impl<T: MirLowering> MirTypable for T {
    fn mir_type_of(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Option<MirDataType> {
        self.type_of(env, scope, span)
    }
}

impl MirTypable for MirField {
    fn mir_type_of(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Option<MirDataType> {
        let base_type = self.base.mir_type_of(env, scope, span).map(|x| match x {
            MirDataType::Option(x) => *x,
            x => x,
        })?;

        env.resolve_member_field_type(&base_type, &self.field)
    }
}

impl MirTypable for MirIndex {
    fn mir_type_of(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Option<MirDataType> {
        let base_type = self.base.mir_type_of(env, scope, span).map(|x| match x {
            MirDataType::Option(x) => *x,
            x => x,
        });

        let index_type = self.index.mir_type_of(env, scope, span);

        let ref_mutability = base_type.as_ref().and_then(|x| match x {
            MirDataType::Ref(_, x) => Some(*x),
            _ => None,
        });

        let data_type = match (base_type, index_type) {
            (Some(base_type), Some(MirDataType::Range)) => {
                Some(match base_type.unwrap_all_refs() {
                    MirDataType::List(_) => base_type,
                    MirDataType::Str => MirDataType::Str,
                    MirDataType::Range => MirDataType::Range,
                    _ => return None,
                })
            }
            (Some(base_type), _) => Some(match base_type.unwrap_all_refs() {
                MirDataType::List(inner) => *inner.clone(),
                MirDataType::Str => MirDataType::Char,
                MirDataType::Range => MirDataType::Int,
                _ => return None,
            }),
            _ => None,
        };

        data_type.map(|data_type| {
            if let Some(ref_mutability) = ref_mutability {
                MirDataType::Option(Box::new(MirDataType::Ref(
                    Box::new(data_type),
                    ref_mutability,
                )))
            } else {
                MirDataType::Option(Box::new(data_type))
            }
        })
    }
}

impl MirTypable for MirDiscriminant {
    fn mir_type_of(
        &self,
        _env: &mut MiddleEnvironment,
        _scope: ScopeId,
        _span: Span,
    ) -> Option<MirDataType> {
        Some(MirDataType::Int)
    }
}

impl MirTypable for MirCall {
    fn mir_type_of(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Option<MirDataType> {
        self.caller.mir_type_of(env, scope, span)?.apply_callable()
    }
}

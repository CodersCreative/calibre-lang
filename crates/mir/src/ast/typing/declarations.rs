use crate::{
    ast::{
        MirExtern, MirFunction, MirScopeDecl, MirVarDecl, types::MirDataType, typing::MirTypable,
    },
    environment::MiddleEnvironment,
    scoping::ScopeId,
};
use calibre_parser::Span;

impl MirTypable for MirVarDecl {
    fn mir_type_of(
        &self,
        _env: &mut MiddleEnvironment,
        _scope: ScopeId,
        _span: Span,
    ) -> Option<MirDataType> {
        None
    }
}

impl MirTypable for MirScopeDecl {
    fn mir_type_of(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        _span: Span,
    ) -> Option<MirDataType> {
        let mut typ = None;

        for node in &self.body {
            typ = env.resolve_emit_type_from_middle_node(scope, node);
            if typ.is_some() {
                break;
            }
        }

        typ
    }
}

impl MirTypable for MirFunction {
    fn mir_type_of(
        &self,
        _env: &mut MiddleEnvironment,
        _scope: ScopeId,
        _span: Span,
    ) -> Option<MirDataType> {
        Some(MirDataType::Function {
            return_type: Box::new(self.return_type.clone()),
            parameters: self.parameters.iter().map(|x| x.1.clone()).collect(),
        })
    }
}

impl MirTypable for MirExtern {
    fn mir_type_of(
        &self,
        _env: &mut MiddleEnvironment,
        _scope: ScopeId,
        _span: Span,
    ) -> Option<MirDataType> {
        None
    }
}

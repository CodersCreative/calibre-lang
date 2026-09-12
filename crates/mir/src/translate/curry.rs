use crate::{
    ast::MiddleNode, environment::MiddleEnvironment, errors::MiddleErr, scoping::ScopeId,
    translate::MirLowering,
};
use calibre_parser::{
    Span,
    ast::{
        idents::{ParserText, PotentialDollarIdentifier},
        nodes::{
            AstNode, AstNodeType,
            functions::{AstCurry, AstFunction, CallArg, FunctionHeader},
        },
        types::{GenericTypes, ParserDataType, ParserInnerType},
    },
};

impl MirLowering for AstCurry {
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Result<MiddleNode, MiddleErr> {
        let value = env.rewrite_curry_call(scope, span, *self.value)?;
        value.lower(env, scope, span)
    }

    fn type_of(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        _span: Span,
    ) -> Option<ParserDataType> {
        env.resolve_curried_type(scope, &self.value)
    }
}

impl MiddleEnvironment {
    #[allow(clippy::type_complexity)]
    fn build_curried_return_type(
        span: Span,
        params: &[(
            PotentialDollarIdentifier,
            Option<ParserDataType>,
            Option<Box<AstNode>>,
        )],
        return_type: ParserDataType,
    ) -> ParserDataType {
        let mut result = return_type;

        for (_, param_type, _) in params.iter().rev() {
            result = ParserDataType::function(
                span,
                vec![
                    param_type
                        .clone()
                        .unwrap_or_else(|| ParserDataType::new(span, ParserInnerType::Dynamic)),
                ],
                result,
            );
        }

        result
    }

    fn build_curried_call(
        span: Span,
        target: AstNode,
        params: Vec<(
            PotentialDollarIdentifier,
            Option<ParserDataType>,
            Option<Box<AstNode>>,
        )>,
        return_type: ParserDataType,
        mut bound: Vec<AstNode>,
    ) -> AstNode {
        let mut params = params.into_iter();

        let Some((first_name, first_type, first_default)) = params.next() else {
            return AstNode::call(
                span,
                target,
                bound.into_iter().map(CallArg::Value).collect::<Vec<_>>(),
            );
        };

        let Some(first_type) = first_type else {
            return AstNode::call(
                span,
                target,
                bound.into_iter().map(CallArg::Value).collect::<Vec<_>>(),
            );
        };

        let params = params.collect::<Vec<_>>();
        bound.push(AstNode::identifier(span, first_name.to_string()));

        AstNode::new(
            span,
            AstNodeType::FunctionDeclaration(AstFunction {
                header: FunctionHeader {
                    generics: GenericTypes::default(),
                    parameters: vec![(first_name, Some(first_type), first_default)],
                    return_type: Self::build_curried_return_type(
                        span,
                        &params,
                        return_type.clone(),
                    ),
                    param_destructures: Vec::new(),
                },
                body: Box::new(AstNode::new_temp_scope(vec![AstNode::ret(
                    Self::build_curried_call(span, target, params, return_type, bound),
                )])),
            }),
        )
    }

    pub fn rewrite_identity_curry(
        &mut self,
        scope: ScopeId,
        span: Span,
        target: AstNode,
    ) -> Result<AstNode, MiddleErr> {
        Ok(AstNode::new(
            span,
            AstNodeType::FunctionDeclaration(AstFunction {
                header: FunctionHeader {
                    generics: GenericTypes::default(),
                    parameters: Vec::new(),
                    return_type: self.resolve_type_from_node(scope, &target).ok_or_else(|| {
                        self.context
                            .err_at_current(MiddleErr::CannotInferCurryTargetType)
                    })?,
                    param_destructures: Vec::new(),
                },
                body: Box::new(AstNode::new_temp_scope(vec![AstNode::ret(target)])),
            }),
        ))
    }

    pub fn rewrite_curry_call(
        &mut self,
        scope: ScopeId,
        span: Span,
        target: AstNode,
    ) -> Result<AstNode, MiddleErr> {
        let ty = self.resolve_type_from_node(scope, &target).ok_or_else(|| {
            self.context
                .err_at_current(MiddleErr::CannotInferCurryTargetType)
        })?;

        match ty.unwrap_all_refs().data_type {
            ParserInnerType::Function {
                return_type,
                parameters,
            }
            | ParserInnerType::NativeFunction {
                return_type,
                parameters,
            } => {
                // TODO Deal with defaults
                let defaults: Box<[&crate::symbols::FunctionParamDefault]> =
                    Vec::new().into_boxed_slice();

                let params = parameters
                    .into_iter()
                    .enumerate()
                    .map(|(index, param)| {
                        (
                            PotentialDollarIdentifier::Identifier(ParserText::new(
                                span,
                                format!("curry_arg_{index}"),
                            )),
                            Some(param),
                            defaults
                                .get(index)
                                .and_then(|d| d.explicit_default.clone())
                                .map(|node| Box::new(node.into())),
                        )
                    })
                    .collect::<Vec<_>>();

                Ok(Self::build_curried_call(
                    span,
                    target,
                    params,
                    *return_type,
                    Vec::new(),
                ))
            }
            _ => self.rewrite_identity_curry(scope, span, target),
        }
    }
}

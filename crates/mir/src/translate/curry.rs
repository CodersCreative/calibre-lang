use crate::{
    environment::MiddleEnvironment,
    errors::MiddleErr,
    scoping::ScopeId,
    symbols::resolve::ResolutionOptions,
};
use calibre_parser::{
    Span,
    ast::{
        idents::{ParserText, PotentialDollarIdentifier},
        nodes::{
            AstNode, AstNodeType, CallArg, FunctionHeader
        },
        types::{GenericTypes, ParserDataType, ParserInnerType},
    },
};
use ustr::Ustr;

impl MiddleEnvironment {
    #[allow(clippy::type_complexity)]
    fn build_curried_return_type(
        span: Span,
        params: &[(PotentialDollarIdentifier, Option<ParserDataType>, Option<Box<AstNode>>) ],
        return_type: ParserDataType,
    ) -> ParserDataType {
        let mut result = return_type;

        for (_, param_type, _) in params.iter().rev() {
            result = ParserDataType::function(span, vec![param_type
                .clone()
                .unwrap_or_else(|| ParserDataType::new(span, ParserInnerType::Dynamic))], result);
        }

        result
    }

    fn build_curried_body(
        span: Span,
        params: Vec<(PotentialDollarIdentifier, Option<ParserDataType>, Option<Box<AstNode>>)>,
        return_type : ParserDataType,
        body: AstNode,
    ) -> AstNode {
        let mut params = params.into_iter();

        let Some((first_name, first_type, first_default)) = params.next() else {
            return body;
        };

        let params = params.collect::<Vec<_>>();

        AstNode::new(
            span,
            AstNodeType::FunctionDeclaration {
                header: FunctionHeader {
                    generics: GenericTypes::default(),
                    parameters: vec![(first_name, first_type, first_default)],
                    return_type: Self::build_curried_return_type(span, &params, return_type.clone()),
                    param_destructures: Vec::new(),
                },
                body: Box::new(AstNode::new_temp_scope(vec![AstNode::ret(Self::build_curried_body(span, params, return_type, body))])),
            },
        )
    }

    fn build_curried_call(
        span: Span,
        target_name: Ustr,
        params: Vec<(PotentialDollarIdentifier, Option<ParserDataType>, Option<Box<AstNode>>)>,
        return_type : ParserDataType,
        mut bound: Vec<AstNode>,
    ) -> AstNode {
        let mut params = params.into_iter();

        let Some((first_name, first_type, first_default)) = params.next() else {
            return AstNode::call(span, AstNode::identifier(span, target_name), bound
                .into_iter()
                .map(CallArg::Value)
                .collect::<Vec<_>>());
        };

        let Some(first_type) = first_type else {
            return AstNode::call(span, AstNode::identifier(span, target_name), bound
                .into_iter()
                .map(CallArg::Value)
                .collect::<Vec<_>>());
        };

        let params = params.collect::<Vec<_>>();
        bound.push(AstNode::identifier(span, first_name.to_string()));

        AstNode::new(
            span,
            AstNodeType::FunctionDeclaration {
                header: FunctionHeader {
                    generics: GenericTypes::default(),
                    parameters: vec![(first_name, Some(first_type), first_default)],
                    return_type: Self::build_curried_return_type(span, &params, return_type.clone()),
                    param_destructures: Vec::new(),
                },
                body: Box::new(AstNode::new_temp_scope(vec![AstNode::ret(Self::build_curried_call(span, target_name, params, return_type, bound))])),
            },
        )
    }

    pub fn rewrite_curry_call(
        &mut self,
        scope: ScopeId,
        span: Span,
        target: AstNode,
    ) -> Result<AstNode, MiddleErr> {
        match target.node_type {
            AstNodeType::FunctionDeclaration { header, body } => Ok(Self::build_curried_body(
                span,
                header.parameters,
                header.return_type,
                *body,
            )),
            AstNodeType::Identifier(ref name) => {
                let resolved = self.resolve(scope, name, ResolutionOptions::idents())?;
                let (data_type, return_type) = self
                    .resolve_type_from_node(scope, &target)
                    .map(|x| x.unwrap_all_refs().data_type)
                    .and_then(|data_type| match data_type {
                        ParserInnerType::Function {
                            return_type,
                            parameters,
                        } => Some((parameters, *return_type)),
                        _ => None,
                    })
                    .ok_or_else(|| self.context.err_at_current(MiddleErr::InferImpossible))?;

                let defaults = self
                    .symbols
                    .function_param_defaults
                    .get(&resolved).map(|x| x.iter().collect::<Box<[_]>>())
                    .unwrap_or_default();

                let params = data_type
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

                Ok(Self::build_curried_call(span, resolved, params, return_type, Vec::new()))
            }
            _ => Err(self.context.err_at_current(MiddleErr::InferImpossible)),
        }
    }
}
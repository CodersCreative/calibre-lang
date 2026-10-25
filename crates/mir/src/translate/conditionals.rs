use crate::{
    ast::{MiddleNode, MiddleNodeType, MirConditional, types::MirDataType},
    environment::MiddleEnvironment,
    errors::MiddleErr,
    scoping::ScopeId,
    symbols::{VariableKey, resolve::ResolutionOptions},
    tags::TagInfo,
    translate::MirLowering,
};
use calibre_parser::{
    Span,
    ast::{
        comparison::BooleanOperator,
        nodes::{
            AstNode, AstNodeType,
            conditionals::{AstIf, AstTernary, IfComparisonType, TernaryType},
            functions::CallArg,
            matching::{AstMatch, MatchArmType, MatchBody},
        },
    },
};
use rustc_hash::FxHashMap;
use tracing::instrument;

#[derive(Debug, Default)]
pub struct TypeNarrowing {
    variables: FxHashMap<VariableKey, MirDataType>,
}

impl From<FxHashMap<VariableKey, MirDataType>> for TypeNarrowing {
    fn from(value: FxHashMap<VariableKey, MirDataType>) -> Self {
        Self { variables: value }
    }
}

impl TypeNarrowing {
    fn analyze(
        &mut self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        node: &AstNode,
        negated: bool,
    ) -> Result<(), MiddleErr> {
        match &node.node_type {
            AstNodeType::IsExpression(is_expr) => {
                if let AstNodeType::Identifier(id) = &is_expr.value.node_type
                    && let Ok(key) = env
                        .resolve(scope, &id.value, ResolutionOptions::idents())
                        .map(|x| x.unwrap_variable())
                {
                    let target_type = env.resolve_data_type(
                        scope,
                        &is_expr.data_type,
                        ResolutionOptions::typing(),
                    )?;

                    if let Some(variable) = env.symbols.variables.get_mut(&key)
                        && !negated
                    {
                        self.variables.insert(key, variable.data_type.clone());
                        variable.data_type = target_type;
                    }
                }
            }
            AstNodeType::ParenExpression(x) => {
                self.analyze(env, scope, &x.value, negated)?;
            }
            AstNodeType::NotExpression(x) => {
                self.analyze(env, scope, &x.value, !negated)?;
            }
            AstNodeType::BooleanExpression(x) => match x.operator {
                BooleanOperator::And => {
                    self.analyze(env, scope, &x.left, negated)?;
                    self.analyze(env, scope, &x.right, negated)?;
                }
                BooleanOperator::Or => {}
            },
            _ => {}
        }

        Ok(())
    }

    pub fn restore(self, env: &mut MiddleEnvironment) {
        for (variable_key, original_type) in self.variables {
            if let Some(variable) = env.symbols.variables.get_mut(&variable_key) {
                variable.data_type = original_type;
            }
        }
    }
}

impl MirLowering for AstIf {
    #[instrument(skip_all)]
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
        mut data_type: Option<MirDataType>,
    ) -> Result<MiddleNode, MiddleErr> {
        if !env.context.type_check && data_type.is_some() {
            let then_type = self.then.type_of(env, scope, span);
            let otherwise_type = self
                .otherwise
                .as_ref()
                .and_then(|x| x.type_of(env, scope, span));

            data_type = Some(env.compare_types(
                then_type,
                otherwise_type,
                Some(&TagInfo::IgnoreInvalidTypeCheck),
                span,
            )?);
        }

        match *self.comparison {
            IfComparisonType::If(x) => {
                let mut narrowing = TypeNarrowing::default();
                narrowing.analyze(env, scope, &x, false)?;

                let comparison = x.lower_or_empty(env, scope, span, Some(MirDataType::Bool));

                match &comparison.node_type {
                    MiddleNodeType::Identifier(x) if x.identifier.name() == "true" => {
                        return self.then.lower(env, scope, span, data_type);
                    }
                    MiddleNodeType::Identifier(x)
                        if x.identifier.name() == "false" && self.otherwise.is_some() =>
                    {
                        return self.otherwise.unwrap().lower(env, scope, span, data_type);
                    }
                    _ => {}
                }

                let then = self
                    .then
                    .lower_or_empty(env, scope, span, data_type.clone());

                narrowing.restore(env);

                Ok(MiddleNode {
                    node_type: MiddleNodeType::Conditional(MirConditional {
                        comparison: Box::new(comparison),
                        then: Box::new(then),
                        otherwise: self.otherwise.map(|x| {
                            Box::new(x.lower_or_empty(env, scope, span, data_type.clone()))
                        }),
                        data_type,
                    }),
                    span,
                })
            }
            IfComparisonType::IfLet { value, pattern } => AstNode {
                node_type: AstNodeType::MatchStatement(AstMatch {
                    value: Some(Box::new(value)),
                    body: {
                        let mut lst: Vec<(MatchArmType, Vec<AstNode>, Box<AstNode>)> = pattern
                            .0
                            .clone()
                            .into_iter()
                            .map(|x| (x, pattern.1.clone(), self.then.clone()))
                            .collect();

                        lst.push((
                            MatchArmType::Wildcard(Span::default()),
                            Vec::new(),
                            self.otherwise.unwrap_or(Box::new(AstNode {
                                node_type: AstNodeType::EmptyLine,
                                span: Span::default(),
                            })),
                        ));

                        MatchBody { values: lst }
                    },
                }),
                span,
            }
            .lower(env, scope, span, data_type),
        }
    }

    fn type_of(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Option<MirDataType> {
        if let Some(otherwise) = &self.otherwise {
            let then_ty = self.then.type_of(env, scope, span);
            let else_ty = otherwise.type_of(env, scope, span);

            match (then_ty, else_ty) {
                (Some(a), Some(b)) if a.loose_eq(&b) => Some(a),
                (Some(a), Some(b)) if a.is_null() => Some(b),
                (Some(a), Some(b)) if b.is_null() => Some(a),
                (Some(a), _) | (_, Some(a)) => Some(a),
                _ => None,
            }
        } else {
            self.then.type_of(env, scope, span)
        }
    }
}

impl MirLowering for AstTernary {
    #[instrument(skip_all)]
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
        data_type: Option<MirDataType>,
    ) -> Result<MiddleNode, MiddleErr> {
        match self.ternary_type {
            TernaryType::Normal => {
                let otherwise = self
                    .otherwise
                    .expect("Otherwise with TernaryType::Normal should be Some");

                AstNode {
                    node_type: AstNodeType::IfStatement(AstIf {
                        comparison: Box::new(IfComparisonType::If(*self.comparison)),
                        then: Box::new(AstNode::new_temp_scope(vec![AstNode::emit(*self.then)])),
                        otherwise: Some(Box::new(AstNode::new_temp_scope(vec![AstNode::emit(
                            *otherwise,
                        )]))),
                    }),
                    span,
                }
                .lower(env, scope, span, data_type)
            }
            TernaryType::Option => AstNode {
                node_type: AstNodeType::IfStatement(AstIf {
                    comparison: Box::new(IfComparisonType::If(*self.comparison)),
                    then: Box::new(AstNode::new_temp_scope(vec![AstNode::emit(AstNode::call(
                        span,
                        AstNode::identifier(span, "some"),
                        vec![CallArg::Value(*self.then)],
                    ))])),
                    otherwise: Some(Box::new(AstNode::new_temp_scope(vec![AstNode::emit(
                        AstNode::identifier(span, "none"),
                    )]))),
                }),
                span,
            }
            .lower(env, scope, span, data_type),
            TernaryType::Result => {
                let otherwise = self
                    .otherwise
                    .expect("Otherwise with TernaryType::Result should be Some");

                AstNode {
                    node_type: AstNodeType::IfStatement(AstIf {
                        comparison: Box::new(IfComparisonType::If(*self.comparison)),
                        then: Box::new(AstNode::new_temp_scope(vec![AstNode::emit(
                            AstNode::call(
                                span,
                                AstNode::identifier(span, "ok"),
                                vec![CallArg::Value(*self.then)],
                            ),
                        )])),
                        otherwise: Some(Box::new(AstNode::new_temp_scope(vec![AstNode::emit(
                            AstNode::call(
                                span,
                                AstNode::identifier(span, "err"),
                                vec![CallArg::Value(*otherwise)],
                            ),
                        )]))),
                    }),
                    span,
                }
                .lower(env, scope, span, data_type)
            }
        }
    }

    fn type_of(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Option<MirDataType> {
        match self.ternary_type {
            TernaryType::Normal => self.then.type_of(env, scope, span),
            TernaryType::Option => Some(MirDataType::Option(Box::new(
                self.then.type_of(env, scope, span)?,
            ))),
            TernaryType::Result => {
                if let Some(otherwise) = &self.otherwise {
                    Some(MirDataType::Result {
                        ok: Box::new(self.then.type_of(env, scope, span)?),
                        err: Box::new(otherwise.type_of(env, scope, span)?),
                    })
                } else {
                    None
                }
            }
        }
    }
}

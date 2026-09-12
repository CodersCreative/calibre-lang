pub mod bindings;
pub mod exhaustiveness;
pub mod guards;
pub mod patterns;
pub mod translator;

pub use bindings::BindingExtractor;
pub use exhaustiveness::{ExhaustivenessCheckerDispatcher, ExhaustivenessReport};
pub use guards::GuardProcessor;
pub use translator::PatternTranslatorDispatcher;

use crate::{
    MiddleNode, environment::MiddleEnvironment, errors::MiddleErr, scoping::ScopeId,
    symbols::resolve::ResolutionOptions, translate::MirLowering, typing::MiddleTypeDefType,
};
use calibre_parser::{
    Span,
    ast::{
        comparison::{BooleanOperator, ComparisonOperator},
        idents::ParserText,
        nodes::{
            AstNode, AstNodeType, VarType,
            access::AstIndex,
            binary::{AstBoolean, AstComparison},
            conditionals::{AstIf, IfComparisonType},
            functions::{AstFunction, CallArg, FunctionHeader},
            matching::{AstFnMatch, AstMatch, MatchArmType},
        },
        types::{ParserDataType, ParserInnerType},
    },
};
use ustr::Ustr;

impl MiddleEnvironment {
    pub fn match_index_access(&self, base: AstNode, index: usize) -> AstNode {
        AstNode::new(
            self.context.current_span(),
            AstNodeType::IndexAccess(AstIndex {
                base: Box::new(base),
                index: Box::new(AstNode::int(self.context.current_span(), index)),
            }),
        )
    }

    fn builtin_enum_variant_index(variant_name: &str) -> Option<i64> {
        match variant_name {
            "Ok" | "Some" => Some(0),
            "Err" | "None" => Some(1),
            _ => None,
        }
    }

    fn enum_key_from_data_type(data_type: &ParserDataType) -> Option<Ustr> {
        match data_type.clone().unwrap_all_refs().data_type {
            ParserInnerType::Struct(name) => Some(Ustr::from(&name)),
            ParserInnerType::StructWithGenerics { identifier, .. } => Some(Ustr::from(&identifier)),
            _ => None,
        }
    }

    fn enum_variant_index_from_data_type(
        &self,
        data_type: &ParserDataType,
        variant_name: &Ustr,
    ) -> Option<i64> {
        if let Some(key) = Self::enum_key_from_data_type(data_type)
            && let Some(obj) = self.typing.objects.get(&key)
            && let MiddleTypeDefType::Enum { variants, .. } = &obj.object_type
            && let Some(index) = variants.iter().position(|x| &x.0 == variant_name)
        {
            return Some(index as i64);
        }
        Self::builtin_enum_variant_index(variant_name)
    }

    pub fn bool_and_nodes(&self, left: AstNode, right: AstNode) -> AstNode {
        AstNode::new(
            self.context.current_span(),
            AstNodeType::BooleanExpression(AstBoolean {
                left: Box::new(left),
                right: Box::new(right),
                operator: BooleanOperator::And,
            }),
        )
    }

    pub fn fold_and_conditions(&self, mut conditions: Vec<AstNode>) -> AstNode {
        if conditions.is_empty() {
            return AstNode::bool(self.context.current_span(), true);
        }
        let first = conditions.remove(0);
        conditions
            .into_iter()
            .fold(first, |acc, node| self.bool_and_nodes(acc, node))
    }

    pub fn discriminant_eq(&self, value: AstNode, index: i64) -> AstNode {
        AstNode::new(
            self.context.current_span(),
            AstNodeType::ComparisonExpression(AstComparison {
                left: Box::new(AstNode::call(
                    self.context.current_span(),
                    AstNode::identifier(self.context.current_span(), "discriminant"),
                    vec![CallArg::Value(value)],
                )),
                right: Box::new(AstNode::int(self.context.current_span(), index)),
                operator: ComparisonOperator::Equal,
            }),
        )
    }

    pub fn enum_variant_index_from_value(
        &mut self,
        scope: ScopeId,
        value_node: &AstNode,
        variant_name: &Ustr,
    ) -> Option<i64> {
        if let Some(dt) = self.resolve_type_from_node(scope, value_node) {
            return self.enum_variant_index_from_data_type(&dt.unwrap_all_refs(), variant_name);
        }
        Self::builtin_enum_variant_index(variant_name)
    }
}

impl MirLowering for AstMatch {
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Result<MiddleNode, MiddleErr> {
        let (decl, value) = if let Some(value) = self.value {
            let tmp_name = ParserText::temp_name_with_suffix("match_tmp", span);
            let resolved = value.type_of(env, scope, span);

            (
                Some(AstNode::new(
                    span,
                    AstNodeType::VariableDeclaration {
                        var_type: VarType::Mutable,
                        identifier: tmp_name.clone().into(),
                        data_type: resolved.unwrap_or_else(|| ParserDataType::auto(span)),
                        value,
                    },
                )),
                Some(AstNode::identifier(span, tmp_name)),
            )
        } else {
            (None, None)
        };

        let mut ifs: Vec<AstNode> = Vec::new();

        for mut pattern in self.body.values {
            // This is just to turn idents that cant be resolved into new let match arms
            if let MatchArmType::Value(AstNode {
                node_type: AstNodeType::Identifier(id),
                ..
            }) = &pattern.0
                && env
                    .resolve(scope, &id.value, ResolutionOptions::idents())
                    .is_err()
            {
                pattern.0 = MatchArmType::Let {
                    var_type: VarType::Immutable,
                    name: id.value.get_ident().clone(),
                };
            }

            let guard_nodes = pattern.1.clone();

            if let Some(value_node) = value.clone() {
                let compilation =
                    PatternTranslatorDispatcher::translate(env, scope, &pattern.0, &value_node)?;

                let mut body_nodes =
                    PatternTranslatorDispatcher::bindings_to_decls(&compilation.bindings, env);

                let guard_bindings: Vec<(Ustr, AstNode)> = compilation
                    .bindings
                    .iter()
                    .map(|b| (b.name, b.value.clone()))
                    .collect();

                let final_cond = if guard_nodes.is_empty() {
                    compilation.condition
                } else {
                    let guard_cond =
                        GuardProcessor::rewrite_guards(env, &guard_nodes, &guard_bindings);
                    env.bool_and_nodes(compilation.condition, guard_cond)
                };

                body_nodes.push(*pattern.2);

                ifs.push(AstNode::new(
                    span,
                    AstNodeType::IfStatement(AstIf {
                        comparison: Box::new(IfComparisonType::If(final_cond)),
                        then: Box::new(AstNode::new_temp_scope(body_nodes)),
                        otherwise: None,
                    }),
                ));
            } else {
                ifs.push(*pattern.2);
            }
        }

        let ifs = if let Some(mut current) = ifs.pop() {
            while let Some(mut prev) = ifs.pop() {
                if let AstNodeType::IfStatement(AstIf { otherwise, .. }) = &mut prev.node_type {
                    *otherwise = Some(Box::new(current));
                }
                current = prev;
            }

            current
        } else {
            AstNode::new(span, AstNodeType::EmptyLine)
        };

        if let Some(decl) = decl {
            AstNode::new_temp_scope(vec![decl, ifs])
        } else {
            ifs
        }
        .lower(env, scope, span)
    }

    fn type_of(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Option<ParserDataType> {
        if let Some((_arm_type, _guards, arm_body)) = self.body.values.first() {
            arm_body.type_of(env, scope, span)
        } else {
            None
        }
    }
}

impl MirLowering for AstFnMatch {
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Result<MiddleNode, MiddleErr> {
        AstNode::new(
            span,
            AstNodeType::FunctionDeclaration(AstFunction {
                body: Box::new(AstNode::new(
                    span,
                    AstNodeType::ScopeDeclaration {
                        body: Some(vec![AstNode::new(
                            span,
                            AstNodeType::MatchStatement(AstMatch {
                                value: Some(Box::new(AstNode::identifier(
                                    span,
                                    self.header.parameters[0].0.clone(),
                                ))),
                                body: self.body,
                            }),
                        )]),
                        named: None,
                        is_temp: true,
                        create_new_scope: Some(true),
                        define: false,
                    },
                )),
                header: FunctionHeader {
                    param_destructures: Vec::new(),
                    ..self.header
                },
            }),
        )
        .lower(env, scope, span)
    }

    fn type_of(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Option<ParserDataType> {
        self.header.type_of(env, scope, span)
    }
}

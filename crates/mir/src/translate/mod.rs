use crate::{
    ast::{MiddleNode, MiddleNodeType, MirAssignment, MirScopeDecl, MirVarDecl},
    environment::MiddleEnvironment,
    errors::MiddleErr,
    scoping::ScopeId,
    symbols::resolve::ResolutionOptions,
    tags::TagInfo,
    typing::{
        MiddleImplMember, MiddleObject, MiddleTrait, MiddleTraitMember, MiddleTypeDefType, Typing,
    },
};
use calibre_parser::{
    Span,
    ast::{
        generics::TraitMemberKind,
        idents::{ParserText, PotentialDollarIdentifier, PotentialGenericTypeIdentifier},
        nodes::{
            AstNode, AstNodeType, TypeDefType, VarType,
            access::{AstField, AstIndex, AstScope},
            conditionals::{AstIf, AstTernary, IfComparisonType},
            functions::{AstFunction, FunctionHeader},
            memory::AstDeref,
        },
        types::{GenericTypes, ParserDataType, ParserInnerType},
    },
};
use tracing::{debug, instrument, trace};
use ustr::{Ustr, UstrMap, UstrSet};

pub mod access;
pub mod binary;
pub mod conditionals;
pub mod curry;
pub mod flow;
pub mod functions;
pub mod iter;
pub mod lists;
pub mod literals;
pub mod loops;
pub mod matching;
pub mod memory;
pub mod scopes;
pub mod spawn;
pub mod statements;
pub mod unary;

pub trait MirLowering {
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Result<MiddleNode, MiddleErr>;

    fn lower_or_empty(self, env: &mut MiddleEnvironment, scope: ScopeId, span: Span) -> MiddleNode
    where
        Self: Sized,
    {
        match self.lower(env, scope, span) {
            Ok(node) => node,
            Err(err) => {
                debug!(error = %err, "evaluation failed, pushing error");
                env.context.push_error(err);
                MiddleNode::new(MiddleNodeType::EmptyLine, span)
            }
        }
    }

    fn type_of(
        &self,
        _env: &mut MiddleEnvironment,
        _scope: ScopeId,
        _span: Span,
    ) -> Option<ParserDataType> {
        None
    }
}

impl MirLowering for AstNode {
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        _span: Span,
    ) -> Result<MiddleNode, MiddleErr> {
        env.evaluate_inner(scope, self)
    }

    fn type_of(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        _span: Span,
    ) -> Option<ParserDataType> {
        env.resolve_type_from_node(scope, self)
    }
}

impl MiddleEnvironment {
    pub fn compare_types(
        &self,
        type1: Option<ParserDataType>,
        type2: Option<ParserDataType>,
        overload_tag: Option<&TagInfo>,
    ) -> Result<ParserDataType, MiddleErr> {
        if !self.context.type_check {
            return match (type1, type2) {
                (Some(x), None) => Ok(x),
                (None, Some(x)) => Ok(x),
                (Some(x), _) => Ok(x),
                (None, None) => {
                    Err(self
                        .context
                        .err_at_current(MiddleErr::CannotInferFromExpression(
                            "type resolution".to_string(),
                        )))
                }
            };
        }

        match (type1, type2) {
            (None, None) => Err(self
                .context
                .err_at_current(MiddleErr::CannotInferFromExpression(
                    "type resolution".to_string(),
                ))),
            (Some(x), None) => Ok(x),
            (None, Some(x)) => Ok(x),
            (Some(x), Some(_))
                if overload_tag.is_some_and(|x| self.tagging.tag_info.contains(x)) =>
            {
                Ok(x)
            }
            (Some(x), Some(y)) => {
                // TODO Handle generics better
                if x.loose_eq(&y)
                    || self
                        .scoping
                        .all_time_generics
                        .contains(&Ustr::from(&y.impl_name()))
                    || self
                        .scoping
                        .all_time_generics
                        .contains(&Ustr::from(&x.impl_name()))
                {
                    Ok(x)
                } else {
                    Err(self.context.err_at_current(MiddleErr::InvalidType {
                        expected: Box::new(x.clone()),
                        found: Box::new(y.clone()),
                    }))
                }
            }
        }
    }

    pub fn compare_types_ref(
        &self,
        type1: Option<&ParserDataType>,
        type2: Option<&ParserDataType>,
        overload_tag: Option<&TagInfo>,
    ) -> Result<(), MiddleErr> {
        if !self.context.type_check {
            return Ok(());
        }

        match (type1, type2) {
            (None, None) => Err(self
                .context
                .err_at_current(MiddleErr::CannotInferFromExpression(
                    "type comparison".to_string(),
                ))),
            (Some(_), None) => Ok(()),
            (None, Some(_)) => Ok(()),
            (Some(_), Some(_))
                if overload_tag.is_some_and(|x| self.tagging.tag_info.contains(x)) =>
            {
                Ok(())
            }
            (Some(x), Some(y)) => {
                // TODO Handle generics better
                if x.loose_eq(y)
                    || self
                        .scoping
                        .all_time_generics
                        .contains(&Ustr::from(&y.impl_name()))
                    || self
                        .scoping
                        .all_time_generics
                        .contains(&Ustr::from(&x.impl_name()))
                {
                    Ok(())
                } else {
                    Err(self.context.err_at_current(MiddleErr::InvalidType {
                        expected: Box::new(x.clone()),
                        found: Box::new(y.clone()),
                    }))
                }
            }
        }
    }

    pub fn evaluate(&mut self, scope: ScopeId, node: AstNode) -> MiddleNode {
        let span = node.span;
        match self.evaluate_inner(scope, node) {
            Ok(node) => node,
            Err(err) => {
                debug!(error = %err, "evaluation failed, pushing error");
                self.context.push_error(err);
                MiddleNode::new(MiddleNodeType::EmptyLine, span)
            }
        }
    }

    #[instrument(skip_all)]
    pub fn evaluate_inner(
        &mut self,
        scope: ScopeId,
        node: AstNode,
    ) -> Result<MiddleNode, MiddleErr> {
        self.context.current_location = self.scoping.get_location(scope, node.span);
        trace!(location = ?self.context.current_location, "evaluating node");

        match node.node_type {
            AstNodeType::DataType { .. } => unreachable!(),
            AstNodeType::Null => Ok(MiddleNode {
                node_type: MiddleNodeType::Null,
                span: node.span,
            }),

            // Flow
            AstNodeType::Break(x) => x.lower(self, scope, node.span),
            AstNodeType::Emit(x) => x.lower(self, scope, node.span),
            AstNodeType::Defer(x) => x.lower(self, scope, node.span),
            AstNodeType::Try(x) => x.lower(self, scope, node.span),
            AstNodeType::Continue(x) => x.lower(self, scope, node.span),
            AstNodeType::Return(x) => x.lower(self, scope, node.span),
            AstNodeType::PipeExpression(x) => x.lower(self, scope, node.span),

            // Literals
            AstNodeType::StructLiteral(x) => x.lower(self, scope, node.span),
            AstNodeType::EnumExpression(x) => x.lower(self, scope, node.span),
            AstNodeType::TupleLiteral(x) => x.lower(self, scope, node.span),
            AstNodeType::StringLiteral(x) => x.lower(self, scope, node.span),
            AstNodeType::RangeDeclaration(x) => x.lower(self, scope, node.span),
            AstNodeType::IntLiteral(x) => x.lower(self, scope, node.span),
            AstNodeType::BigLiteral(x) => x.lower(self, scope, node.span),
            AstNodeType::FloatLiteral(x) => x.lower(self, scope, node.span),
            AstNodeType::CharLiteral(x) => x.lower(self, scope, node.span),

            // Lists
            AstNodeType::ListLiteral(x) => x.lower(self, scope, node.span),
            AstNodeType::ListRepeatLiteral(x) => x.lower(self, scope, node.span),

            // Conditionals
            AstNodeType::Ternary(x) => x.lower(self, scope, node.span),
            AstNodeType::IfStatement(x) => x.lower(self, scope, node.span),

            // Unary
            AstNodeType::NotExpression(x) => x.lower(self, scope, node.span),
            AstNodeType::NegExpression(x) => x.lower(self, scope, node.span),

            // Binary
            AstNodeType::BooleanExpression(x) => x.lower(self, scope, node.span),
            AstNodeType::ComparisonExpression(x) => x.lower(self, scope, node.span),
            AstNodeType::BinaryExpression(x) => x.lower(self, scope, node.span),
            AstNodeType::AsExpression(x) => x.lower(self, scope, node.span),
            AstNodeType::IsExpression(x) => x.lower(self, scope, node.span),
            AstNodeType::InDeclaration(x) => x.lower(self, scope, node.span),

            // Functions
            AstNodeType::CurryExpression(x) => x.lower(self, scope, node.span),
            AstNodeType::FunctionDeclaration(x) => x.lower(self, scope, node.span),
            AstNodeType::ExternFunctionDeclaration(x) => x.lower(self, scope, node.span),
            AstNodeType::CallExpression(x) => x.lower(self, scope, node.span),

            // Access
            AstNodeType::Identifier(x) => x.lower(self, scope, node.span),
            AstNodeType::FieldAccess(x) => x.lower(self, scope, node.span),
            AstNodeType::ScopeAccess(x) => x.lower(self, scope, node.span),
            AstNodeType::IndexAccess(x) => x.lower(self, scope, node.span),

            // Memory
            AstNodeType::RefStatement(x) => x.lower(self, scope, node.span),
            AstNodeType::DerefStatement(x) => x.lower(self, scope, node.span),
            AstNodeType::Drop(x) => x.lower(self, scope, node.span),
            AstNodeType::MoveExpression(x) => x.lower(self, scope, node.span),

            // Matching
            AstNodeType::MatchStatement(x) => x.lower(self, scope, node.span),
            AstNodeType::FnMatchDeclaration(x) => x.lower(self, scope, node.span),

            // Spawn
            AstNodeType::SelectStatement(x) => x.lower(self, scope, node.span),
            AstNodeType::Spawn(x) => x.lower(self, scope, node.span),

            AstNodeType::EmptyLine => Ok(MiddleNode {
                node_type: MiddleNodeType::EmptyLine,
                span: node.span,
            }),
            AstNodeType::ParenExpression { value } => self.evaluate_inner(scope, *value),
            AstNodeType::DestructureDeclaration {
                var_type: _,
                pattern,
                value,
            } => {
                let tmp_ident: PotentialDollarIdentifier =
                    ParserText::temp_name_with_suffix("destructure_tmp", node.span).into();

                let tmp_decl = AstNode::new(
                    node.span,
                    AstNodeType::VariableDeclaration {
                        var_type: VarType::Immutable,
                        identifier: tmp_ident.clone(),
                        data_type: ParserDataType::auto(node.span),
                        value,
                    },
                );

                let mut body = Vec::new();
                body.push(tmp_decl);
                body.extend(
                    self.emit_destructure_statements(&tmp_ident, &pattern, node.span, true),
                );

                self.evaluate_inner(
                    scope,
                    AstNode::new(
                        node.span,
                        AstNodeType::ScopeDeclaration {
                            body: Some(body),
                            named: None,
                            is_temp: true,
                            create_new_scope: Some(false),
                            define: false,
                        },
                    ),
                )
            }
            AstNodeType::DestructureAssignment { pattern, value } => {
                let tmp_ident: PotentialDollarIdentifier =
                    ParserText::temp_name_with_suffix("destructure_tmp", node.span).into();

                let tmp_decl = AstNode::new(
                    node.span,
                    AstNodeType::VariableDeclaration {
                        var_type: VarType::Immutable,
                        identifier: tmp_ident.clone(),
                        data_type: ParserDataType::auto(node.span),
                        value,
                    },
                );

                let mut body = vec![tmp_decl];
                body.extend(
                    self.emit_destructure_statements(&tmp_ident, &pattern, node.span, false),
                );

                self.evaluate_inner(
                    scope,
                    AstNode::new_temp_scope_with_create(body, Some(false)),
                )
            }
            AstNodeType::VariableDeclaration {
                var_type,
                identifier,
                value,
                data_type,
            } => self.evaluate_var_declaration(
                scope, node.span, var_type, identifier, *value, data_type,
            ),
            AstNodeType::TypeDeclaration {
                identifier,
                object,
                overloads,
            } => self.evaluate_type_declaration(scope, node.span, identifier, object, overloads),
            AstNodeType::LoopDeclaration {
                loop_type,
                body,
                until,
                label,
                else_body,
            } => self.evaluate_loop_statement(scope, *loop_type, *body, until, label, else_body),
            AstNodeType::TestDeclaration { identifier, body } => {
                let func_identifier = format!(
                    "test::{}",
                    ParserText::temp_name_with_suffix(identifier.text.trim(), node.span).text
                );
                let file_path = self
                    .scoping
                    .scope_or_err(scope)
                    .map(|s| s.path.clone())
                    .ok();

                self.register_test(
                    Ustr::from(&identifier.text),
                    Ustr::from(&func_identifier),
                    scope,
                    file_path,
                );

                self.evaluate_inner(
                    scope,
                    AstNode::new(
                        node.span,
                        AstNodeType::VariableDeclaration {
                            var_type: VarType::Constant,
                            identifier: PotentialDollarIdentifier::Identifier(ParserText::new(
                                node.span,
                                func_identifier,
                            )),
                            data_type: ParserDataType::auto(node.span),
                            value: Box::new(AstNode::new(
                                node.span,
                                AstNodeType::FunctionDeclaration(AstFunction {
                                    header: FunctionHeader {
                                        generics: GenericTypes::default(),
                                        parameters: Vec::new(),
                                        return_type: ParserDataType::null(node.span),
                                        param_destructures: Vec::new(),
                                    },
                                    body,
                                }),
                            )),
                        },
                    ),
                )
            }
            AstNodeType::IterExpression {
                data_type,
                map,
                spawned,
                loop_type,
                conditionals,
                until,
            } => self.evaluate_iter_expression(
                scope,
                data_type,
                map,
                spawned,
                loop_type,
                conditionals,
                until,
            ),
            AstNodeType::InlineGenerator {
                map,
                data_type,
                loop_type,
                conditionals,
                until,
            } => self.evaluate_inner(
                scope,
                Self::wrap_inline_generator(
                    node.span,
                    *map,
                    *loop_type,
                    conditionals,
                    until,
                    data_type.unwrap_or(ParserDataType::auto(node.span)),
                ),
            ),
            AstNodeType::AssignmentExpression { identifier, value } => {
                if !self.context.type_check {
                    let identifier_type = self.resolve_type_from_node(scope, &identifier);
                    let value_type = self.resolve_type_from_node(scope, &value);
                    self.compare_types_ref(
                        identifier_type.as_ref(),
                        value_type.as_ref(),
                        Some(&TagInfo::IgnoreInvalidTypeCheck),
                    )?;
                }

                match identifier.node_type.clone() {
                    AstNodeType::Ternary(AstTernary {
                        comparison,
                        then,
                        otherwise,
                    }) => self.evaluate_inner(
                        scope,
                        AstNode {
                            node_type: AstNodeType::IfStatement(AstIf {
                                comparison: Box::new(IfComparisonType::If(*comparison)),
                                then: Box::new(AstNode::new(
                                    self.context.current_span(),
                                    AstNodeType::AssignmentExpression {
                                        identifier: then,
                                        value: value.clone(),
                                    },
                                )),
                                otherwise: Some(Box::new(AstNode::new(
                                    self.context.current_span(),
                                    AstNodeType::AssignmentExpression {
                                        identifier: otherwise,
                                        value,
                                    },
                                ))),
                            }),
                            span: node.span,
                        },
                    ),
                    AstNodeType::DerefStatement(AstDeref {
                        value: deref_target,
                    }) => Ok(MiddleNode {
                        node_type: MiddleNodeType::AssignmentExpression(MirAssignment {
                            identifier: Box::new(self.evaluate(
                                scope,
                                AstNode::new(
                                    node.span,
                                    AstNodeType::DerefStatement(AstDeref {
                                        value: deref_target,
                                    }),
                                ),
                            )),
                            value: Box::new(self.evaluate(scope, *value)),
                        }),
                        span: node.span,
                    }),
                    AstNodeType::FieldAccess(AstField { base, field }) => Ok(MiddleNode {
                        node_type: MiddleNodeType::AssignmentExpression(MirAssignment {
                            identifier: Box::new(self.evaluate(
                                scope,
                                AstNode::new(
                                    node.span,
                                    AstNodeType::FieldAccess(AstField { base, field }),
                                ),
                            )),
                            value: Box::new(self.evaluate(scope, *value)),
                        }),
                        span: node.span,
                    }),
                    AstNodeType::ScopeAccess(AstScope { base, field }) => Ok(MiddleNode {
                        node_type: MiddleNodeType::AssignmentExpression(MirAssignment {
                            identifier: Box::new(self.evaluate(
                                scope,
                                AstNode::new(
                                    node.span,
                                    AstNodeType::ScopeAccess(AstScope { base, field }),
                                ),
                            )),
                            value: Box::new(self.evaluate(scope, *value)),
                        }),
                        span: node.span,
                    }),
                    AstNodeType::IndexAccess(AstIndex { base, index }) => {
                        if let Some(overloaded) = self.handle_index_assign_overload(
                            scope,
                            node.span,
                            *base.clone(),
                            *index.clone(),
                            *value.clone(),
                        )? {
                            return Ok(overloaded);
                        }

                        Ok(MiddleNode {
                            node_type: MiddleNodeType::AssignmentExpression(MirAssignment {
                                identifier: Box::new(self.evaluate(
                                    scope,
                                    AstNode::new(
                                        node.span,
                                        AstNodeType::IndexAccess(AstIndex { base, index }),
                                    ),
                                )),
                                value: Box::new(self.evaluate(scope, *value)),
                            }),
                            span: node.span,
                        })
                    }
                    _ => Ok(MiddleNode {
                        node_type: MiddleNodeType::AssignmentExpression(MirAssignment {
                            identifier: Box::new(self.evaluate(scope, *identifier)),
                            value: Box::new(self.evaluate(scope, *value)),
                        }),
                        span: node.span,
                    }),
                }
            }
            AstNodeType::ImplDeclaration {
                generics,
                target,
                variables,
            } => {
                let mut prev_generics = Vec::new();
                if let Ok(scope_ref) = self.scoping.scope_mut_or_err(scope) {
                    for generic in generics.0.iter() {
                        let name = Ustr::from(&generic.identifier.to_string());
                        prev_generics.push((name, scope_ref.mappings.get(&name).cloned()));
                        scope_ref.mappings.insert(name, name);
                    }
                }

                let generic_params: Vec<Ustr> = generics
                    .0
                    .iter()
                    .map(|g| {
                        self.resolve(
                            scope,
                            &g.identifier,
                            ResolutionOptions::default().with_dollar(),
                        )
                        .unwrap_or(Ustr::from(&g.identifier.to_string()))
                    })
                    .collect();

                if !generic_params.is_empty() {
                    self.scoping.push_generic_params(generic_params.clone());
                }

                let resolved = self
                    .resolve_data_type(scope, &target, ResolutionOptions::typing())
                    .unwrap()
                    .unwrap_all_refs();

                let impl_key = Ustr::from(&resolved.impl_name());

                self.typing
                    .get_or_create_impl(impl_key, self.context.current_location.clone());

                {
                    let placeholders = variables
                        .iter()
                        .filter_map(|var| {
                            if let AstNodeType::VariableDeclaration { identifier, .. } =
                                &var.node_type
                            {
                                let identifier = self
                                    .resolve(
                                        scope,
                                        identifier,
                                        ResolutionOptions::default().with_dollar(),
                                    )
                                    .ok()?;
                                let resolved_iden =
                                    Ustr::from(&format!("{}.{}", impl_key, identifier));
                                // TODO Unpack the dollar ident only without resolving
                                Some((identifier, resolved_iden, generic_params.clone()))
                            } else {
                                None
                            }
                        })
                        .collect::<Vec<_>>();

                    let type_defs = variables
                        .iter()
                        .filter_map(|var| {
                            if let AstNodeType::TypeDeclaration {
                                identifier, object, ..
                            } = &var.node_type
                            {
                                let ident = self
                                    .resolve(
                                        scope,
                                        identifier.get_ident(),
                                        ResolutionOptions::default().with_dollar(),
                                    )
                                    .ok()?;
                                if let TypeDefType::NewType(inner) = object {
                                    let resolved_ty = self
                                        .resolve_data_type(
                                            scope,
                                            inner.as_ref(),
                                            ResolutionOptions::typing(),
                                        )
                                        .ok()?
                                        .unwrap_all_refs();
                                    Some((ident, resolved_ty))
                                } else {
                                    None
                                }
                            } else {
                                None
                            }
                        })
                        .collect::<Vec<_>>();

                    let impl_ref = self.typing.impls.get_mut(&impl_key).ok_or_else(|| {
                        MiddleErr::At(
                            node.span,
                            Box::new(MiddleErr::InternalMissingImpl(format!("{impl_key:?}"))),
                        )
                    })?;

                    for var in placeholders {
                        impl_ref.insert_member_placeholder(&var.0, var.1, var.2);
                    }

                    for (ident, ty) in type_defs {
                        impl_ref.assoc_types.insert(ident, ty);
                    }
                }

                let previous_self_type = {
                    let scope = self.scoping.scope_mut_or_err(scope)?;

                    scope
                        .type_mappings
                        .insert(Ustr::from("Self"), resolved.data_type.clone())
                };

                let mut statements = Vec::new();

                fn process_var(
                    env: &mut MiddleEnvironment,
                    scope: ScopeId,
                    resolved: &ParserDataType,
                    generic_params: &[Ustr],
                    var: AstNode,
                ) -> Result<Option<(AstNode, Ustr, bool)>, MiddleErr> {
                    match var.node_type {
                        AstNodeType::VariableDeclaration {
                            var_type,
                            identifier,
                            value,
                            data_type,
                        } => {
                            let identifier = env.resolve(
                                scope,
                                &identifier,
                                ResolutionOptions::default().with_dollar(),
                            )?;
                            let resolved_iden = format!("{}.{}", resolved.impl_name(), identifier);

                            let dependant = match &value.node_type {
                                AstNodeType::FunctionDeclaration(AstFunction {
                                    header, ..
                                }) => {
                                    let param_type = if let Some(Some(param)) =
                                        header.parameters.first().map(|x| &x.1)
                                    {
                                        env.resolve_data_type(
                                            scope,
                                            param,
                                            ResolutionOptions::typing(),
                                        )
                                        .ok()
                                        .map(|x| x.unwrap_all_refs())
                                    } else if let Some(Some(node)) =
                                        header.parameters.first().map(|x| x.2.clone())
                                    {
                                        env.resolve_type_from_node(scope, &node)
                                            .map(|x| x.unwrap_all_refs())
                                    } else {
                                        None
                                    };

                                    if let Some(param_type) = param_type {
                                        resolved.data_type.matches(
                                            &param_type.data_type,
                                            &generic_params
                                                .iter()
                                                .map(|x| x.as_ref())
                                                .collect::<Vec<_>>(),
                                        )
                                    } else {
                                        false
                                    }
                                }
                                _ => false,
                            };

                            Ok(Some((
                                AstNode {
                                    span: var.span,
                                    node_type: AstNodeType::VariableDeclaration {
                                        var_type,
                                        identifier: PotentialDollarIdentifier::Identifier(
                                            ParserText::from(resolved_iden),
                                        ),
                                        value,
                                        data_type,
                                    },
                                },
                                identifier,
                                dependant,
                            )))
                        }
                        AstNodeType::Tag {
                            node,
                            tag,
                            arguments,
                        } => match process_var(env, scope, resolved, generic_params, *node) {
                            Ok(Some(x)) => Ok(Some((
                                AstNode::new(
                                    Span::default(),
                                    AstNodeType::Tag {
                                        node: Box::new(x.0),
                                        tag,
                                        arguments,
                                    },
                                ),
                                x.1,
                                x.2,
                            ))),
                            x => x,
                        },
                        AstNodeType::TypeDeclaration { .. } => Ok(None),
                        _ => Err(MiddleErr::At(
                            var.span,
                            Box::new(MiddleErr::InternalExpectedVariableInImpl),
                        )),
                    }
                }

                for var in variables {
                    let (dec, iden, dependant) =
                        match process_var(self, scope, &resolved, &generic_params, var)? {
                            Some(x) => x,
                            None => continue,
                        };

                    let dec = self.evaluate(scope, dec);

                    let new_name = match &dec.node_type {
                        MiddleNodeType::VariableDeclaration(MirVarDecl { identifier, .. }) => {
                            identifier
                        }
                        _ => {
                            return Err(MiddleErr::At(
                                dec.span,
                                Box::new(MiddleErr::InternalImplBodyNotVariableDeclaration),
                            ));
                        }
                    };

                    self.typing
                        .impls
                        .get_mut(&impl_key)
                        .ok_or_else(|| {
                            MiddleErr::At(
                                dec.span,
                                Box::new(MiddleErr::InternalMissingImpl(format!("{impl_key:?}"))),
                            )
                        })?
                        .insert_member(
                            &iden,
                            MiddleImplMember::new(*new_name, generic_params.clone(), dependant),
                        );

                    statements.push(dec);
                }

                {
                    let scope = self.scoping.scope_mut_or_err(scope)?;

                    if let Some(prev) = previous_self_type {
                        scope.type_mappings.insert(Ustr::from("Self"), prev);
                    }

                    for (name, prev) in prev_generics {
                        if let Some(prev) = prev {
                            scope.mappings.insert(name, prev);
                        } else {
                            scope.mappings.remove(&name);
                        }
                    }

                    if !generic_params.is_empty() {
                        self.scoping.pop_generic_params();
                    }
                }

                Ok(MiddleNode {
                    node_type: MiddleNodeType::ScopeDeclaration(MirScopeDecl {
                        body: statements,
                        create_new_scope: false,
                        is_temp: false,
                        scope_id: scope,
                    }),
                    span: node.span,
                })
            }
            AstNodeType::ImplTraitDeclaration {
                generics,
                trait_ident,
                target,
                variables,
            } => {
                let mut prev_generics = Vec::new();
                if let Ok(scope_ref) = self.scoping.scope_mut_or_err(scope) {
                    for generic in generics.0.iter() {
                        let name = Ustr::from(&generic.identifier.to_string());
                        prev_generics.push((name, scope_ref.mappings.get(&name).cloned()));
                        scope_ref.mappings.insert(name, name);
                    }
                }

                let generic_params: Vec<Ustr> = generics
                    .0
                    .iter()
                    .map(|g| {
                        self.resolve(
                            scope,
                            &g.identifier,
                            ResolutionOptions::default().with_dollar(),
                        )
                        .unwrap_or(Ustr::from(&g.identifier.to_string()))
                    })
                    .collect();

                if !generic_params.is_empty() {
                    self.scoping.push_generic_params(generic_params.clone());
                }

                let resolved_trait =
                    self.resolve(scope, &trait_ident, ResolutionOptions::typing())?;

                let resolved_target = self
                    .resolve_data_type(scope, &target, ResolutionOptions::typing())?
                    .unwrap_all_refs();
                let impl_key = Ustr::from(&resolved_target.impl_name());

                let mut provided = UstrSet::default();
                let mut assoc_types = Vec::new();
                for var in &variables {
                    match &var.node_type {
                        AstNodeType::VariableDeclaration { identifier, .. } => {
                            provided.insert(Ustr::from(&identifier.to_string()));
                        }
                        AstNodeType::TypeDeclaration {
                            identifier, object, ..
                        } => {
                            assoc_types.push((identifier.clone(), object.clone()));
                        }
                        _ => {}
                    }
                }

                let mut all_vars = variables;
                for (name, member) in Typing::collect_trait_default_members(
                    &self.typing.trait_defs,
                    &resolved_trait,
                    &provided,
                ) {
                    if member.default.is_none() {
                        continue;
                    }
                    let default = member.default.unwrap();
                    all_vars.push(AstNode::new(
                        default.span,
                        AstNodeType::VariableDeclaration {
                            var_type: VarType::Constant,
                            identifier: PotentialDollarIdentifier::Identifier(ParserText::from(
                                name,
                            )),
                            data_type: member.data_type.clone(),
                            value: Box::new(default),
                        },
                    ));
                }

                let (previous_self, previous_self_type) = {
                    let scope = self.scoping.scope_mut_or_err(scope)?;

                    (
                        scope.mappings.insert(Ustr::from("Self"), impl_key),
                        scope
                            .type_mappings
                            .insert(Ustr::from("Self"), resolved_target.data_type.clone()),
                    )
                };

                self.typing
                    .get_or_create_impl(impl_key, self.context.current_location.clone());

                for (identifier, object) in assoc_types {
                    if let TypeDefType::NewType(inner) = object {
                        let resolved_ty = self
                            .resolve_data_type(scope, inner.as_ref(), ResolutionOptions::typing())?
                            .unwrap_all_refs();

                        let ident = self.resolve(
                            scope,
                            identifier.get_ident(),
                            ResolutionOptions::default().with_dollar(),
                        )?;

                        let impl_ref = self.typing.impls.get_mut(&impl_key).ok_or_else(|| {
                            MiddleErr::At(
                                node.span,
                                Box::new(MiddleErr::InternalMissingImpl(format!("{impl_key:?}"))),
                            )
                        })?;

                        impl_ref.assoc_types.insert(ident, resolved_ty);
                    }
                }

                {
                    let impl_ref = self.typing.impls.get_mut(&impl_key).ok_or_else(|| {
                        MiddleErr::At(
                            node.span,
                            Box::new(MiddleErr::InternalMissingImpl(format!("{impl_key:?}"))),
                        )
                    })?;
                    for var in &all_vars {
                        if let AstNodeType::VariableDeclaration { identifier, .. } = &var.node_type
                        {
                            let resolved_iden = Ustr::from(&format!("{}.{}", impl_key, identifier));
                            impl_ref.insert_member_placeholder(
                                &identifier.to_string(),
                                resolved_iden,
                                generic_params.clone(),
                            );
                        }
                    }
                }

                let mut statements = Vec::new();

                for var in all_vars {
                    let (dec, iden, dependant) = match var.node_type {
                        AstNodeType::VariableDeclaration {
                            var_type,
                            identifier,
                            value,
                            data_type,
                        } => {
                            // TODO Deal with dollar ident
                            let iden = identifier.to_string();
                            let resolved_iden = format!("{}.{}", impl_key, identifier);

                            let dependant = match &value.node_type {
                                AstNodeType::FunctionDeclaration(AstFunction {
                                    header, ..
                                }) => {
                                    let param_type = if let Some(Some(param)) =
                                        header.parameters.first().map(|x| &x.1)
                                    {
                                        Some(
                                            self.resolve_data_type(
                                                scope,
                                                param,
                                                ResolutionOptions::typing(),
                                            )?
                                            .unwrap_all_refs(),
                                        )
                                    } else if let Some(Some(node)) =
                                        header.parameters.first().map(|x| x.2.clone())
                                    {
                                        self.resolve_type_from_node(scope, &node)
                                            .map(|x| x.unwrap_all_refs())
                                    } else {
                                        None
                                    };

                                    if let Some(param_type) = param_type {
                                        resolved_target.data_type.matches(
                                            &param_type.data_type,
                                            &generic_params
                                                .iter()
                                                .map(|x| x.as_ref())
                                                .collect::<Vec<_>>(),
                                        )
                                    } else {
                                        false
                                    }
                                }
                                _ => false,
                            };

                            (
                                AstNode {
                                    span: var.span,
                                    node_type: AstNodeType::VariableDeclaration {
                                        var_type,
                                        identifier: PotentialDollarIdentifier::Identifier(
                                            ParserText::from(resolved_iden),
                                        ),
                                        value,
                                        data_type,
                                    },
                                },
                                iden,
                                dependant,
                            )
                        }
                        AstNodeType::TypeDeclaration { .. } => {
                            continue;
                        }
                        _ => {
                            return Err(MiddleErr::At(
                                var.span,
                                Box::new(MiddleErr::InternalExpectedVariableInImpl),
                            ));
                        }
                    };

                    let dec = self.evaluate(scope, dec);

                    let new_name = match &dec.node_type {
                        MiddleNodeType::VariableDeclaration(MirVarDecl { identifier, .. }) => {
                            identifier
                        }
                        _ => {
                            return Err(MiddleErr::At(
                                var.span,
                                Box::new(MiddleErr::InternalImplBodyNotVariableDeclaration),
                            ));
                        }
                    };

                    let impl_ref = self.typing.impls.get_mut(&impl_key).ok_or_else(|| {
                        MiddleErr::At(
                            var.span,
                            Box::new(MiddleErr::InternalMissingImpl(format!("{impl_key:?}"))),
                        )
                    })?;

                    impl_ref.insert_member(
                        &iden,
                        MiddleImplMember::new(*new_name, generic_params.clone(), dependant),
                    );
                    if !impl_ref.traits.contains(&resolved_trait) {
                        impl_ref.traits.push(resolved_trait);
                    }

                    if let Some(trait_def) = self.typing.trait_defs.get(&resolved_trait) {
                        for implied in &trait_def.implied_traits {
                            if !impl_ref.traits.contains(implied) {
                                impl_ref.traits.push(*implied);
                            }
                        }
                    }

                    statements.push(dec);
                }

                {
                    let scope = self.scoping.scope_mut_or_err(scope)?;

                    if let Some(prev) = previous_self {
                        scope.mappings.insert(Ustr::from("Self"), prev);
                    }

                    if let Some(prev) = previous_self_type {
                        scope.type_mappings.insert(Ustr::from("Self"), prev);
                    }

                    for (name, prev) in prev_generics {
                        if let Some(prev) = prev {
                            scope.mappings.insert(name, prev);
                        } else {
                            scope.mappings.remove(&name);
                        }
                    }

                    if !generic_params.is_empty() {
                        self.scoping.pop_generic_params();
                    }
                }

                Ok(MiddleNode {
                    node_type: MiddleNodeType::ScopeDeclaration(MirScopeDecl {
                        body: statements,
                        create_new_scope: false,
                        is_temp: false,
                        scope_id: scope,
                    }),
                    span: node.span,
                })
            }
            AstNodeType::TraitDeclaration {
                identifier,
                implied_traits,
                members,
            } => {
                let mut generic_names = Vec::new();
                let base_name = match &identifier {
                    PotentialGenericTypeIdentifier::Identifier(x) => Ustr::from(&x.to_string()),
                    PotentialGenericTypeIdentifier::Generic {
                        identifier,
                        generic_types,
                    } => {
                        for t in generic_types {
                            if let ParserDataType {
                                data_type: ParserInnerType::Struct(s),
                                ..
                            } = t
                            {
                                generic_names.push(Ustr::from(s));
                            }
                        }
                        Ustr::from(&identifier.to_string())
                    }
                };

                let new_name =
                    Ustr::from(&ParserText::temp_name_with_suffix(base_name, node.span).text);

                self.typing.objects.insert(
                    new_name,
                    MiddleObject {
                        object_type: MiddleTypeDefType::Trait,
                        variables: UstrMap::default(),
                        traits: Vec::new(),
                        location: self.context.current_location.clone(),
                    },
                );

                let mut prev_generics = Vec::new();
                if let Ok(scope_ref) = self.scoping.scope_mut_or_err(scope) {
                    scope_ref.mappings.insert(base_name, new_name);

                    for name in &generic_names {
                        prev_generics.push((name, scope_ref.mappings.get(name).cloned()));
                        scope_ref.mappings.insert(*name, *name);
                    }
                }

                let mut trait_members = UstrMap::default();
                let mut assoc_types = UstrMap::default();
                for member in members {
                    match member.kind {
                        TraitMemberKind::Type => {
                            let data_type = self.resolve_data_type(
                                scope,
                                &member.data_type,
                                ResolutionOptions::typing(),
                            )?;
                            assoc_types
                                .insert(Ustr::from(&member.identifier.to_string()), data_type);
                        }
                        TraitMemberKind::Const => {
                            let data_type = self.resolve_data_type(
                                scope,
                                &member.data_type,
                                ResolutionOptions::typing(),
                            )?;
                            trait_members.insert(
                                Ustr::from(&member.identifier.to_string()),
                                MiddleTraitMember {
                                    data_type,
                                    default: member.value.map(|x| *x),
                                },
                            );
                        }
                    }
                }

                let mut implied = Vec::new();
                for imp in implied_traits {
                    let resolved = self
                        .resolve(scope, &imp, ResolutionOptions::default().with_dollar())
                        .unwrap_or_else(|_| Ustr::from(&imp.to_string()));
                    implied.push(resolved);
                }

                self.typing.trait_defs.insert(
                    new_name,
                    MiddleTrait {
                        implied_traits: implied,
                        members: trait_members,
                        assoc_types,
                    },
                );

                if let Ok(scope_ref) = self.scoping.scope_mut_or_err(scope) {
                    for (name, prev) in prev_generics {
                        if let Some(prev) = prev {
                            scope_ref.mappings.insert(*name, prev);
                        } else {
                            scope_ref.mappings.remove(name);
                        }
                    }
                }

                Ok(MiddleNode {
                    node_type: MiddleNodeType::EmptyLine,
                    span: node.span,
                })
            }
            AstNodeType::ScopeAlias {
                identifier,
                value,
                create_new_scope,
            } => self.evaluate_scope_alias(scope, node.span, identifier, value, create_new_scope),
            AstNodeType::ScopeDeclaration {
                body,
                named,
                is_temp,
                create_new_scope,
                define,
            } => self.evaluate_scope_declaration(
                scope,
                body,
                named,
                create_new_scope,
                define,
                is_temp,
            ),
            AstNodeType::Tag {
                node,
                tag,
                arguments,
            } => {
                if let Some(handler) = self
                    .tagging
                    .tag_handlers
                    .get(&Ustr::from(&tag.text))
                    .cloned()
                {
                    let handler_fn = handler.handler.lock().unwrap();
                    handler_fn(self, scope, *node, tag, arguments)
                } else {
                    self.context.push_error(MiddleErr::InvalidTag(tag.text));
                    self.evaluate_inner(scope, *node)
                }
            }
            AstNodeType::ImportStatement {
                module,
                alias,
                values,
            } => {
                let values: Vec<Ustr> = values
                    .into_iter()
                    .map(|val| Ustr::from(&val.to_string()))
                    .collect();
                let module_path: Vec<Ustr> =
                    module.iter().map(|x| Ustr::from(&x.to_string())).collect();

                let alias = if let Some(alias) = alias {
                    self.resolve(scope, &alias, ResolutionOptions::default().with_dollar())
                        .ok()
                } else {
                    None
                };

                let (new_scope, build_node) = if let Some(alias) = alias {
                    if ["super", "root"].contains(&alias.as_str()) {
                        // TODO return err
                        return Ok(MiddleNode {
                            node_type: MiddleNodeType::EmptyLine,
                            span: node.span,
                        });
                    }

                    let (new_scope_id, build_node) = self.import_scope_list(scope, &module_path)?;

                    self.scoping
                        .scope_mut_or_err(scope)?
                        .children
                        .insert(alias, new_scope_id);

                    return Ok(build_node.unwrap_or(MiddleNode {
                        node_type: MiddleNodeType::EmptyLine,
                        span: node.span,
                    }));
                } else if !values.is_empty() {
                    let (new_scope_id, build_node) = self.import_scope_list(scope, &module_path)?;
                    (new_scope_id, build_node)
                } else {
                    let (_, n) = self.import_scope_list(scope, &module_path)?;
                    return Ok(if let Some(x) = n {
                        x
                    } else {
                        MiddleNode {
                            node_type: MiddleNodeType::EmptyLine,
                            span: node.span,
                        }
                    });
                };

                let (ident_map, type_map) = {
                    let scope = self.scoping.scope_or_err(new_scope)?;

                    (scope.mappings.clone(), scope.type_mappings.clone())
                };

                if &values[0] == "*" {
                    let scope = self.scoping.scope_mut_or_err(scope)?;

                    for (key, value) in ident_map {
                        scope.mappings.entry(key).or_insert(value);
                    }

                    for (key, value) in type_map {
                        scope.type_mappings.entry(key).or_insert(value);
                    }
                } else {
                    let scope = self.scoping.scope_mut_or_err(scope)?;

                    for key in values {
                        if let Some(value) = ident_map.get(&key).cloned() {
                            scope.mappings.insert(key, value);
                            continue;
                        }

                        if let Some(value) = type_map.get(&key).cloned() {
                            scope.type_mappings.insert(key, value);
                        } else {
                            return Err(MiddleErr::At(
                                node.span,
                                Box::new(MiddleErr::CantImport(format!("{} at {:?}", key, module))),
                            ));
                        }
                    }
                }

                Ok(build_node.unwrap_or(MiddleNode {
                    node_type: MiddleNodeType::EmptyLine,
                    span: node.span,
                }))
            }
        }
    }
}

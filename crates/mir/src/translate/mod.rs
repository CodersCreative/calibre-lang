use crate::{
    ast::{MiddleNode, MiddleNodeType},
    environment::MiddleEnvironment,
    errors::MiddleErr,
    scoping::ScopeId,
    symbols::resolve::ResolutionOptions,
    tags::TagInfo,
};
use calibre_parser::{
    Span,
    ast::{
        idents::{ParserText, PotentialDollarIdentifier},
        nodes::{
            AstNode, AstNodeType, VarType,
            declaration::AstDeclaration,
            functions::{AstFunction, FunctionHeader},
        },
        types::{GenericTypes, ParserDataType},
    },
};
use tracing::{debug, instrument, trace};
use ustr::Ustr;

pub mod access;
pub mod assignment;
pub mod binary;
pub mod conditionals;
pub mod curry;
pub mod declarations;
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
pub mod types;
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

            // Assignment
            AstNodeType::AssignmentExpression(x) => x.lower(self, scope, node.span),
            AstNodeType::DestructureAssignment(x) => x.lower(self, scope, node.span),

            // Declarations
            AstNodeType::VariableDeclaration(x) => x.lower(self, scope, node.span),
            AstNodeType::DestructureDeclaration(x) => x.lower(self, scope, node.span),

            // Types
            AstNodeType::TypeDeclaration(x) => x.lower(self, scope, node.span),
            AstNodeType::TraitDeclaration(x) => x.lower(self, scope, node.span),
            AstNodeType::ImplDeclaration(x) => x.lower(self, scope, node.span),
            AstNodeType::ImplTraitDeclaration(x) => x.lower(self, scope, node.span),

            AstNodeType::EmptyLine => Ok(MiddleNode {
                node_type: MiddleNodeType::EmptyLine,
                span: node.span,
            }),
            AstNodeType::ParenExpression { value } => self.evaluate_inner(scope, *value),
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
                        AstNodeType::VariableDeclaration(AstDeclaration {
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
                        }),
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

use crate::{
    ast::{MiddleNode, MiddleNodeType, types::MirDataType},
    environment::MiddleEnvironment,
    errors::MiddleErr,
    scoping::ScopeId,
    tags::TagInfo,
};
use calibre_parser::{
    Span,
    ast::nodes::{AstNode, AstNodeType},
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
pub mod generator;
pub mod iter;
pub mod lists;
pub mod literals;
pub mod loops;
pub mod matching;
pub mod memory;
pub mod misc;
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
        data_type: Option<MirDataType>,
    ) -> Result<MiddleNode, MiddleErr>;

    fn lower_or_empty(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
        data_type: Option<MirDataType>,
    ) -> MiddleNode
    where
        Self: Sized,
    {
        match self.lower(env, scope, span, data_type) {
            Ok(node) => node,
            Err(err) => {
                debug!(error = %err, "evaluation failed, pushing error");
                env.context.push_error(env.context.err_at_span(span, err));
                MiddleNode::new(MiddleNodeType::EmptyLine, span)
            }
        }
    }

    fn type_of(
        &self,
        _env: &mut MiddleEnvironment,
        _scope: ScopeId,
        _span: Span,
    ) -> Option<MirDataType> {
        None
    }
}

impl MirLowering for AstNode {
    #[instrument(skip_all)]
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
        data_type: Option<MirDataType>,
    ) -> Result<MiddleNode, MiddleErr> {
        let mut span = if self.span.is_none() { span } else { self.span };

        if !span.is_none() {
            env.context.current_location = env.scoping.get_location(scope, self.span);
            trace!(location = ?env.context.current_location, "evaluating node");
        } else if let Some(location) = &env.context.current_location {
            span = location.span
        }

        match self.node_type {
            AstNodeType::DataType { .. } => unreachable!(),
            AstNodeType::Null => Ok(MiddleNode {
                node_type: MiddleNodeType::Null,
                span,
            }),
            AstNodeType::EmptyLine => Ok(MiddleNode {
                node_type: MiddleNodeType::EmptyLine,
                span,
            }),

            // Flow
            AstNodeType::Break(x) => x.lower(env, scope, span, data_type),
            AstNodeType::Emit(x) => x.lower(env, scope, span, data_type),
            AstNodeType::Defer(x) => x.lower(env, scope, span, data_type),
            AstNodeType::Try(x) => x.lower(env, scope, span, data_type),
            AstNodeType::Continue(x) => x.lower(env, scope, span, data_type),
            AstNodeType::Return(x) => x.lower(env, scope, span, data_type),
            AstNodeType::PipeExpression(x) => x.lower(env, scope, span, data_type),

            // Literals
            AstNodeType::StructLiteral(x) => x.lower(env, scope, span, data_type),
            AstNodeType::EnumExpression(x) => x.lower(env, scope, span, data_type),
            AstNodeType::TupleLiteral(x) => x.lower(env, scope, span, data_type),
            AstNodeType::StringLiteral(x) => x.lower(env, scope, span, data_type),
            AstNodeType::RangeDeclaration(x) => x.lower(env, scope, span, data_type),
            AstNodeType::IntLiteral(x) => x.lower(env, scope, span, data_type),
            AstNodeType::BigLiteral(x) => x.lower(env, scope, span, data_type),
            AstNodeType::FloatLiteral(x) => x.lower(env, scope, span, data_type),
            AstNodeType::CharLiteral(x) => x.lower(env, scope, span, data_type),

            // Lists
            AstNodeType::ListLiteral(x) => x.lower(env, scope, span, data_type),

            // Conditionals
            AstNodeType::Ternary(x) => x.lower(env, scope, span, data_type),
            AstNodeType::IfStatement(x) => x.lower(env, scope, span, data_type),

            // Unary
            AstNodeType::NotExpression(x) => x.lower(env, scope, span, data_type),
            AstNodeType::NegExpression(x) => x.lower(env, scope, span, data_type),

            // Binary
            AstNodeType::BooleanExpression(x) => x.lower(env, scope, span, data_type),
            AstNodeType::ComparisonExpression(x) => x.lower(env, scope, span, data_type),
            AstNodeType::BinaryExpression(x) => x.lower(env, scope, span, data_type),
            AstNodeType::AsExpression(x) => x.lower(env, scope, span, data_type),
            AstNodeType::IsExpression(x) => x.lower(env, scope, span, data_type),
            AstNodeType::InDeclaration(x) => x.lower(env, scope, span, data_type),

            // Functions
            AstNodeType::CurryExpression(x) => x.lower(env, scope, span, data_type),
            AstNodeType::FunctionDeclaration(x) => x.lower(env, scope, span, data_type),
            AstNodeType::ExternFunctionDeclaration(x) => x.lower(env, scope, span, data_type),
            AstNodeType::CallExpression(x) => x.lower(env, scope, span, data_type),

            // Access
            AstNodeType::Identifier(x) => x.lower(env, scope, span, data_type),
            AstNodeType::FieldAccess(x) => x.lower(env, scope, span, data_type),
            AstNodeType::ScopeAccess(x) => x.lower(env, scope, span, data_type),
            AstNodeType::IndexAccess(x) => x.lower(env, scope, span, data_type),

            // Memory
            AstNodeType::RefStatement(x) => x.lower(env, scope, span, data_type),
            AstNodeType::DerefStatement(x) => x.lower(env, scope, span, data_type),
            AstNodeType::Drop(x) => x.lower(env, scope, span, data_type),
            AstNodeType::MoveExpression(x) => x.lower(env, scope, span, data_type),

            // Matching
            AstNodeType::MatchStatement(x) => x.lower(env, scope, span, data_type),
            AstNodeType::FnMatchDeclaration(x) => x.lower(env, scope, span, data_type),

            // Spawn
            AstNodeType::SelectStatement(x) => x.lower(env, scope, span, data_type),
            AstNodeType::Spawn(x) => x.lower(env, scope, span, data_type),

            // Assignment
            AstNodeType::AssignmentExpression(x) => x.lower(env, scope, span, data_type),
            AstNodeType::DestructureAssignment(x) => x.lower(env, scope, span, data_type),

            // Declarations
            AstNodeType::VariableDeclaration(x) => x.lower(env, scope, span, data_type),
            AstNodeType::DestructureDeclaration(x) => x.lower(env, scope, span, data_type),

            // Types
            AstNodeType::TypeDeclaration(x) => x.lower(env, scope, span, data_type),
            AstNodeType::ImplDeclaration(x) => x.lower(env, scope, span, data_type),

            // Lists
            AstNodeType::IterExpression(x) => x.lower(env, scope, span, data_type),
            AstNodeType::LoopDeclaration(x) => x.lower(env, scope, span, data_type),

            // Scopes
            AstNodeType::ScopeAlias(x) => x.lower(env, scope, span, data_type),
            AstNodeType::ScopeDeclaration(x) => x.lower(env, scope, span, data_type),

            // Generator
            AstNodeType::InlineGenerator(x) => x.lower(env, scope, span, data_type),

            // Misc
            AstNodeType::ParenExpression(x) => x.lower(env, scope, span, data_type),
            AstNodeType::TestDeclaration(x) => x.lower(env, scope, span, data_type),
            AstNodeType::Tag(x) => x.lower(env, scope, span, data_type),
            AstNodeType::ImportStatement(x) => x.lower(env, scope, span, data_type),
        }
    }

    fn type_of(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        _span: Span,
    ) -> Option<MirDataType> {
        env.resolve_type_from_node(scope, self)
    }
}

impl MiddleEnvironment {
    pub fn compare_types(
        &self,
        type1: Option<MirDataType>,
        type2: Option<MirDataType>,
        overload_tag: Option<&TagInfo>,
        span: Span,
    ) -> Result<MirDataType, MiddleErr> {
        if !self.context.type_check {
            return match (type1, type2) {
                (Some(x), None) => Ok(x),
                (None, Some(x)) => Ok(x),
                (Some(x), _) => Ok(x),
                (None, None) => Err(self.context.err_at_span(
                    span,
                    MiddleErr::CannotInferFromExpression("type resolution".to_string()),
                )),
            };
        }

        match (type1, type2) {
            (None, None) => Err(self.context.err_at_span(
                span,
                MiddleErr::CannotInferFromExpression("1. type resolution".to_string()),
            )),
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
                    Err(self.context.err_at_span(
                        span,
                        MiddleErr::InvalidType {
                            expected: Box::new(x.clone()),
                            found: Box::new(y.clone()),
                        },
                    ))
                }
            }
        }
    }

    pub fn compare_types_ref(
        &self,
        type1: Option<&MirDataType>,
        type2: Option<&MirDataType>,
        overload_tag: Option<&TagInfo>,
        span: Span,
    ) -> Result<(), MiddleErr> {
        if !self.context.type_check {
            return Ok(());
        }

        match (type1, type2) {
            (None, None) => Err(self.context.err_at_span(
                span,
                MiddleErr::CannotInferFromExpression("type comparison".to_string()),
            )),
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
                    Err(self.context.err_at_span(
                        span,
                        MiddleErr::InvalidType {
                            expected: Box::new(x.clone()),
                            found: Box::new(y.clone()),
                        },
                    ))
                }
            }
        }
    }
}

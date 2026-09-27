use crate::{
    ast::{MiddleNode, MiddleNodeType},
    environment::MiddleEnvironment,
    errors::MiddleErr,
    scoping::ScopeId,
    tags::TagInfo,
};
use calibre_parser::{
    Span,
    ast::{
        nodes::{AstNode, AstNodeType},
        types::ParserDataType,
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
    #[instrument(skip_all)]
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
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
            AstNodeType::Break(x) => x.lower(env, scope, span),
            AstNodeType::Emit(x) => x.lower(env, scope, span),
            AstNodeType::Defer(x) => x.lower(env, scope, span),
            AstNodeType::Try(x) => x.lower(env, scope, span),
            AstNodeType::Continue(x) => x.lower(env, scope, span),
            AstNodeType::Return(x) => x.lower(env, scope, span),
            AstNodeType::PipeExpression(x) => x.lower(env, scope, span),

            // Literals
            AstNodeType::StructLiteral(x) => x.lower(env, scope, span),
            AstNodeType::EnumExpression(x) => x.lower(env, scope, span),
            AstNodeType::TupleLiteral(x) => x.lower(env, scope, span),
            AstNodeType::StringLiteral(x) => x.lower(env, scope, span),
            AstNodeType::RangeDeclaration(x) => x.lower(env, scope, span),
            AstNodeType::IntLiteral(x) => x.lower(env, scope, span),
            AstNodeType::BigLiteral(x) => x.lower(env, scope, span),
            AstNodeType::FloatLiteral(x) => x.lower(env, scope, span),
            AstNodeType::CharLiteral(x) => x.lower(env, scope, span),

            // Lists
            AstNodeType::ListLiteral(x) => x.lower(env, scope, span),

            // Conditionals
            AstNodeType::Ternary(x) => x.lower(env, scope, span),
            AstNodeType::IfStatement(x) => x.lower(env, scope, span),

            // Unary
            AstNodeType::NotExpression(x) => x.lower(env, scope, span),
            AstNodeType::NegExpression(x) => x.lower(env, scope, span),

            // Binary
            AstNodeType::BooleanExpression(x) => x.lower(env, scope, span),
            AstNodeType::ComparisonExpression(x) => x.lower(env, scope, span),
            AstNodeType::BinaryExpression(x) => x.lower(env, scope, span),
            AstNodeType::AsExpression(x) => x.lower(env, scope, span),
            AstNodeType::IsExpression(x) => x.lower(env, scope, span),
            AstNodeType::InDeclaration(x) => x.lower(env, scope, span),

            // Functions
            AstNodeType::CurryExpression(x) => x.lower(env, scope, span),
            AstNodeType::FunctionDeclaration(x) => x.lower(env, scope, span),
            AstNodeType::ExternFunctionDeclaration(x) => x.lower(env, scope, span),
            AstNodeType::CallExpression(x) => x.lower(env, scope, span),

            // Access
            AstNodeType::Identifier(x) => x.lower(env, scope, span),
            AstNodeType::FieldAccess(x) => x.lower(env, scope, span),
            AstNodeType::ScopeAccess(x) => x.lower(env, scope, span),
            AstNodeType::IndexAccess(x) => x.lower(env, scope, span),

            // Memory
            AstNodeType::RefStatement(x) => x.lower(env, scope, span),
            AstNodeType::DerefStatement(x) => x.lower(env, scope, span),
            AstNodeType::Drop(x) => x.lower(env, scope, span),
            AstNodeType::MoveExpression(x) => x.lower(env, scope, span),

            // Matching
            AstNodeType::MatchStatement(x) => x.lower(env, scope, span),
            AstNodeType::FnMatchDeclaration(x) => x.lower(env, scope, span),

            // Spawn
            AstNodeType::SelectStatement(x) => x.lower(env, scope, span),
            AstNodeType::Spawn(x) => x.lower(env, scope, span),

            // Assignment
            AstNodeType::AssignmentExpression(x) => x.lower(env, scope, span),
            AstNodeType::DestructureAssignment(x) => x.lower(env, scope, span),

            // Declarations
            AstNodeType::VariableDeclaration(x) => x.lower(env, scope, span),
            AstNodeType::DestructureDeclaration(x) => x.lower(env, scope, span),

            // Types
            AstNodeType::TypeDeclaration(x) => x.lower(env, scope, span),
            AstNodeType::TraitDeclaration(x) => x.lower(env, scope, span),
            AstNodeType::ImplDeclaration(x) => x.lower(env, scope, span),
            AstNodeType::ImplTraitDeclaration(x) => x.lower(env, scope, span),

            // Lists
            AstNodeType::IterExpression(x) => x.lower(env, scope, span),
            AstNodeType::LoopDeclaration(x) => x.lower(env, scope, span),

            // Scopes
            AstNodeType::ScopeAlias(x) => x.lower(env, scope, span),
            AstNodeType::ScopeDeclaration(x) => x.lower(env, scope, span),

            // Generator
            AstNodeType::InlineGenerator(x) => x.lower(env, scope, span),

            // Misc
            AstNodeType::ParenExpression(x) => x.lower(env, scope, span),
            AstNodeType::TestDeclaration(x) => x.lower(env, scope, span),
            AstNodeType::Tag(x) => x.lower(env, scope, span),
            AstNodeType::ImportStatement(x) => x.lower(env, scope, span),
        }
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
        span: Span,
    ) -> Result<ParserDataType, MiddleErr> {
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
        type1: Option<&ParserDataType>,
        type2: Option<&ParserDataType>,
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

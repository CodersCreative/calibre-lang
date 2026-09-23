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
        _span: Span,
    ) -> Result<MiddleNode, MiddleErr> {
        env.context.current_location = env.scoping.get_location(scope, self.span);
        trace!(location = ?env.context.current_location, "evaluating node");

        match self.node_type {
            AstNodeType::DataType { .. } => unreachable!(),
            AstNodeType::Null => Ok(MiddleNode {
                node_type: MiddleNodeType::Null,
                span: self.span,
            }),
            AstNodeType::EmptyLine => Ok(MiddleNode {
                node_type: MiddleNodeType::EmptyLine,
                span: self.span,
            }),

            // Flow
            AstNodeType::Break(x) => x.lower(env, scope, self.span),
            AstNodeType::Emit(x) => x.lower(env, scope, self.span),
            AstNodeType::Defer(x) => x.lower(env, scope, self.span),
            AstNodeType::Try(x) => x.lower(env, scope, self.span),
            AstNodeType::Continue(x) => x.lower(env, scope, self.span),
            AstNodeType::Return(x) => x.lower(env, scope, self.span),
            AstNodeType::PipeExpression(x) => x.lower(env, scope, self.span),

            // Literals
            AstNodeType::StructLiteral(x) => x.lower(env, scope, self.span),
            AstNodeType::EnumExpression(x) => x.lower(env, scope, self.span),
            AstNodeType::TupleLiteral(x) => x.lower(env, scope, self.span),
            AstNodeType::StringLiteral(x) => x.lower(env, scope, self.span),
            AstNodeType::RangeDeclaration(x) => x.lower(env, scope, self.span),
            AstNodeType::IntLiteral(x) => x.lower(env, scope, self.span),
            AstNodeType::BigLiteral(x) => x.lower(env, scope, self.span),
            AstNodeType::FloatLiteral(x) => x.lower(env, scope, self.span),
            AstNodeType::CharLiteral(x) => x.lower(env, scope, self.span),

            // Lists
            AstNodeType::ListLiteral(x) => x.lower(env, scope, self.span),

            // Conditionals
            AstNodeType::Ternary(x) => x.lower(env, scope, self.span),
            AstNodeType::IfStatement(x) => x.lower(env, scope, self.span),

            // Unary
            AstNodeType::NotExpression(x) => x.lower(env, scope, self.span),
            AstNodeType::NegExpression(x) => x.lower(env, scope, self.span),

            // Binary
            AstNodeType::BooleanExpression(x) => x.lower(env, scope, self.span),
            AstNodeType::ComparisonExpression(x) => x.lower(env, scope, self.span),
            AstNodeType::BinaryExpression(x) => x.lower(env, scope, self.span),
            AstNodeType::AsExpression(x) => x.lower(env, scope, self.span),
            AstNodeType::IsExpression(x) => x.lower(env, scope, self.span),
            AstNodeType::InDeclaration(x) => x.lower(env, scope, self.span),

            // Functions
            AstNodeType::CurryExpression(x) => x.lower(env, scope, self.span),
            AstNodeType::FunctionDeclaration(x) => x.lower(env, scope, self.span),
            AstNodeType::ExternFunctionDeclaration(x) => x.lower(env, scope, self.span),
            AstNodeType::CallExpression(x) => x.lower(env, scope, self.span),

            // Access
            AstNodeType::Identifier(x) => x.lower(env, scope, self.span),
            AstNodeType::FieldAccess(x) => x.lower(env, scope, self.span),
            AstNodeType::ScopeAccess(x) => x.lower(env, scope, self.span),
            AstNodeType::IndexAccess(x) => x.lower(env, scope, self.span),

            // Memory
            AstNodeType::RefStatement(x) => x.lower(env, scope, self.span),
            AstNodeType::DerefStatement(x) => x.lower(env, scope, self.span),
            AstNodeType::Drop(x) => x.lower(env, scope, self.span),
            AstNodeType::MoveExpression(x) => x.lower(env, scope, self.span),

            // Matching
            AstNodeType::MatchStatement(x) => x.lower(env, scope, self.span),
            AstNodeType::FnMatchDeclaration(x) => x.lower(env, scope, self.span),

            // Spawn
            AstNodeType::SelectStatement(x) => x.lower(env, scope, self.span),
            AstNodeType::Spawn(x) => x.lower(env, scope, self.span),

            // Assignment
            AstNodeType::AssignmentExpression(x) => x.lower(env, scope, self.span),
            AstNodeType::DestructureAssignment(x) => x.lower(env, scope, self.span),

            // Declarations
            AstNodeType::VariableDeclaration(x) => x.lower(env, scope, self.span),
            AstNodeType::DestructureDeclaration(x) => x.lower(env, scope, self.span),

            // Types
            AstNodeType::TypeDeclaration(x) => x.lower(env, scope, self.span),
            AstNodeType::TraitDeclaration(x) => x.lower(env, scope, self.span),
            AstNodeType::ImplDeclaration(x) => x.lower(env, scope, self.span),
            AstNodeType::ImplTraitDeclaration(x) => x.lower(env, scope, self.span),

            // Lists
            AstNodeType::IterExpression(x) => x.lower(env, scope, self.span),
            AstNodeType::LoopDeclaration(x) => x.lower(env, scope, self.span),

            // Scopes
            AstNodeType::ScopeAlias(x) => x.lower(env, scope, self.span),
            AstNodeType::ScopeDeclaration(x) => x.lower(env, scope, self.span),

            // Generator
            AstNodeType::InlineGenerator(x) => x.lower(env, scope, self.span),

            // Misc
            AstNodeType::ParenExpression(x) => x.lower(env, scope, self.span),
            AstNodeType::TestDeclaration(x) => x.lower(env, scope, self.span),
            AstNodeType::Tag(x) => x.lower(env, scope, self.span),
            AstNodeType::ImportStatement(x) => x.lower(env, scope, self.span),
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
}

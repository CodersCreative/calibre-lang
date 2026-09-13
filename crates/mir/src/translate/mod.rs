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
            AstNodeType::EmptyLine => Ok(MiddleNode {
                node_type: MiddleNodeType::EmptyLine,
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

            // Lists
            AstNodeType::IterExpression(x) => x.lower(self, scope, node.span),
            AstNodeType::LoopDeclaration(x) => x.lower(self, scope, node.span),

            // Scopes
            AstNodeType::ScopeAlias(x) => x.lower(self, scope, node.span),
            AstNodeType::ScopeDeclaration(x) => x.lower(self, scope, node.span),

            // Generator
            AstNodeType::InlineGenerator(x) => x.lower(self, scope, node.span),

            // Misc
            AstNodeType::ParenExpression(x) => x.lower(self, scope, node.span),
            AstNodeType::TestDeclaration(x) => x.lower(self, scope, node.span),
            AstNodeType::Tag(x) => x.lower(self, scope, node.span),
            AstNodeType::ImportStatement(x) => x.lower(self, scope, node.span),
        }
    }
}

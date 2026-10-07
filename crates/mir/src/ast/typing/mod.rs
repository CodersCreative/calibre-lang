use crate::{
    ast::{MiddleNode, MiddleNodeType, types::MirDataType},
    environment::MiddleEnvironment,
    scoping::ScopeId,
};
use calibre_parser::Span;

pub mod access;
pub mod declarations;
pub mod expressions;
pub mod flow;
pub mod literals;
pub mod memory;
pub mod statements;

pub trait MirTypable {
    fn mir_type_of(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Option<MirDataType>;
}

impl MirTypable for MiddleNode {
    fn mir_type_of(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Option<MirDataType> {
        match &self.node_type {
            MiddleNodeType::Null => Some(MirDataType::Null),
            MiddleNodeType::EmptyLine => None,

            MiddleNodeType::Emit(x) => x.mir_type_of(env, scope, span),
            MiddleNodeType::IntLiteral(x) => x.mir_type_of(env, scope, span),
            MiddleNodeType::FloatLiteral(x) => x.mir_type_of(env, scope, span),
            MiddleNodeType::BigLiteral(x) => x.mir_type_of(env, scope, span),
            MiddleNodeType::CharLiteral(x) => x.mir_type_of(env, scope, span),
            MiddleNodeType::StringLiteral(x) => x.mir_type_of(env, scope, span),
            MiddleNodeType::ListLiteral(x) => x.mir_type_of(env, scope, span),
            MiddleNodeType::AggregateExpression(x) => x.mir_type_of(env, scope, span),
            MiddleNodeType::Spawn(x) => x.mir_type_of(env, scope, span),
            MiddleNodeType::Drop(x) => x.mir_type_of(env, scope, span),
            MiddleNodeType::Move(x) => x.mir_type_of(env, scope, span),
            MiddleNodeType::Identifier(x) => x.mir_type_of(env, scope, span),
            MiddleNodeType::VariableDeclaration(x) => x.mir_type_of(env, scope, span),

            MiddleNodeType::AssignmentExpression(x) => x.mir_type_of(env, scope, span),
            MiddleNodeType::FunctionDeclaration(x) => x.mir_type_of(env, scope, span),
            MiddleNodeType::ExternFunction(x) => x.mir_type_of(env, scope, span),
            MiddleNodeType::EnumExpression(x) => x.mir_type_of(env, scope, span),
            MiddleNodeType::ScopeDeclaration(x) => x.mir_type_of(env, scope, span),
            MiddleNodeType::Conditional(x) => x.mir_type_of(env, scope, span),
            MiddleNodeType::LoopDeclaration(x) => x.mir_type_of(env, scope, span),
            MiddleNodeType::Return(x) => x.mir_type_of(env, scope, span),
            MiddleNodeType::Break(x) => x.mir_type_of(env, scope, span),
            MiddleNodeType::Continue(x) => x.mir_type_of(env, scope, span),
            MiddleNodeType::Discriminant(x) => x.mir_type_of(env, scope, span),
            MiddleNodeType::FieldAccess(x) => x.mir_type_of(env, scope, span),
            MiddleNodeType::IndexAccess(x) => x.mir_type_of(env, scope, span),
            MiddleNodeType::DerefStatement(x) => x.mir_type_of(env, scope, span),
            MiddleNodeType::RefStatement(x) => x.mir_type_of(env, scope, span),
            MiddleNodeType::BinaryExpression(x) => x.mir_type_of(env, scope, span),
            MiddleNodeType::BooleanExpression(x) => x.mir_type_of(env, scope, span),
            MiddleNodeType::ComparisonExpression(x) => x.mir_type_of(env, scope, span),
            MiddleNodeType::CallExpression(x) => x.mir_type_of(env, scope, span),
            MiddleNodeType::AsExpression(x) => x.mir_type_of(env, scope, span),
            MiddleNodeType::IsExpression(x) => x.mir_type_of(env, scope, span),
            MiddleNodeType::NegExpression(x) => x.mir_type_of(env, scope, span),
            MiddleNodeType::RangeDeclaration(x) => x.mir_type_of(env, scope, span),
        }
    }
}

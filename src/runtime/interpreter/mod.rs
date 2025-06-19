pub mod expressions;
pub mod statements;

use core::panic;
use std::{cell::RefCell, rc::Rc};

use thiserror::Error;

use crate::{
    ast::{NodeType, binary::ASTError},
    runtime::{
        interpreter::{expressions::*, statements::*},
        scope::{Scope, ScopeErr},
        values::{RuntimeType, RuntimeValue, ValueErr},
    },
};

#[derive(Error, Debug, Clone)]
pub enum InterpreterErr {
    #[error("{0}")]
    Value(ValueErr),
    #[error("{0}")]
    AST(ASTError),
    #[error("Cannot assign a literal value, {0:?}.")]
    AssignNonVariable(NodeType),
    #[error("Cannot mutably reference a non mutable value, {0:?}.")]
    MutRefNonMut(RuntimeValue),
    #[error("Cannot mutably reference a non variable, {0:?}.")]
    RefNonVar(NodeType),
    #[error("Cannot index a value that is not a list, {0:?}.")]
    IndexNonList(NodeType),
    #[error("This AST Node has not been implemented, {0:?}.")]
    NotImplemented(NodeType),
    #[error("Expected {0:?} operation.")]
    ExpectedOperation(String),
    #[error("Expected only functions.")]
    ExpectedFunctions,
    #[error("{0:?} is not of type {1:?}.")]
    ExpectedType(RuntimeValue, RuntimeType),
    #[error("Variable {0:?} has an unexpected type.")]
    UnexpectedType(RuntimeValue),
    #[error("Setters can only have one argument, {0:?}")]
    SetterArgs(Box<Vec<NodeType>>),
}

impl From<ASTError> for InterpreterErr {
    fn from(value: ASTError) -> Self {
        Self::AST(value)
    }
}

impl From<ValueErr> for InterpreterErr {
    fn from(value: ValueErr) -> Self {
        Self::Value(value)
    }
}

impl From<ScopeErr> for InterpreterErr {
    fn from(value: ScopeErr) -> Self {
        Self::Value(ValueErr::Scope(value))
    }
}

pub fn evaluate(node: NodeType, scope: Rc<RefCell<Scope>>) -> Result<RuntimeValue, InterpreterErr> {
    match node {
        NodeType::FloatLiteral(x) => Ok(RuntimeValue::Float(x)),
        NodeType::IntegerLiteral(x) => Ok(RuntimeValue::Integer(x)),
        NodeType::StringLiteral(x) => Ok(RuntimeValue::Str(x)),
        NodeType::CharLiteral(x) => Ok(RuntimeValue::Char(x)),
        NodeType::BinaryExpression { .. } => evaluate_binary_expression(node, scope),
        NodeType::Program(_) => evaluate_program(node, scope),
        NodeType::Identifier(x) => evaluate_identifier(&x, scope),
        NodeType::StructLiteral(_) => evaluate_struct_expression(node, scope),
        NodeType::ListLiteral(_) => evaluate_list_expression(node, scope),
        NodeType::CallExpression(_, _) => evaluate_call_expression(node, scope),
        NodeType::VariableDeclaration { .. } => evaluate_variable_declaration(node, scope),
        NodeType::StructDeclaration { .. } => evaluate_struct_declaration(node, scope),
        NodeType::AssignmentExpression { .. } => evaluate_assignment_expression(node, scope),
        NodeType::FunctionDeclaration { .. } => evaluate_function_declaration(node, scope),
        NodeType::ComparisonExpression { .. } => evaluate_comparison_expression(node, scope),
        NodeType::BooleanExpression { .. } => evaluate_boolean_expression(node, scope),
        NodeType::IfStatement { .. } => evaluate_if_statement(node, scope),
        NodeType::MemberExpression { .. } => evaluate_member_expression(node, scope),
        NodeType::ImplDeclaration { .. } => evaluate_impl_declaration(node, scope),
        _ => Err(InterpreterErr::NotImplemented(node)),
    }
}

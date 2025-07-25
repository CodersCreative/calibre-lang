pub mod expressions;
pub mod statements;

use std::{cell::RefCell, rc::Rc};

use expressions::lists::evaluate_iter_expression;
use statements::{comparisons::evaluate_in_statement, matching::evaluate_match_statement};
use thiserror::Error;

use crate::{
    ast::{NodeType, binary::ASTError},
    runtime::{
        interpreter::{
            expressions::{
                call::evaluate_call_expression,
                lists::{evaluate_list_expression, evaluate_tuple_expression},
                member::evaluate_member_expression,
                scope::evaluate_scope,
                structs::{evaluate_enum_expression, evaluate_struct_expression},
                *,
            },
            statements::{
                comparisons::evaluate_if_statement,
                import::evaluate_import_statement,
                loops::evaluate_loop_declaration,
                structs::{
                    evaluate_enum_declaration, evaluate_impl_declaration,
                    evaluate_struct_declaration,
                },
                *,
            },
        },
        scope::{
            Scope, ScopeErr,
            variables::{get_global_scope, get_stop},
        },
        values::{RuntimeType, RuntimeValue, ValueErr, helper::StopValue},
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
    #[error("Node {0:?} has an unexpected type.")]
    UnexpectedNode(NodeType),
    #[error("No associated enum item : {1:?} in enum {0:?}")]
    UnexpectedEnumItem(String, String),
    #[error("Setters can only have one argument, {0:?}")]
    SetterArgs(Vec<(NodeType, Option<NodeType>)>),
    #[error("Property not found, {0:?}")]
    PropertyNotFound(String),
    #[error("Out of bounds of a array: {0:?} - Value : {1:?}")]
    OutOfBounds(String, i16),
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

pub fn evaluate(
    node: NodeType,
    scope: &Rc<RefCell<Scope>>,
) -> Result<RuntimeValue, InterpreterErr> {
    match node {
        NodeType::FloatLiteral(x) => Ok(if x > f32::MAX as f64 {
            RuntimeValue::Double(x as f64)
        } else {
            RuntimeValue::Float(x as f32)
        }),
        NodeType::IntLiteral(x) => Ok(if x > i64::MAX as i128 {
            if x.is_negative() {
                RuntimeValue::Long(x)
            } else {
                RuntimeValue::ULong(x as u128)
            }
        } else {
            if x.is_negative() {
                RuntimeValue::Int(x as i64)
            } else {
                RuntimeValue::UInt(x as u64)
            }
        }),
        NodeType::StringLiteral(x) => Ok(RuntimeValue::Str(x)),
        NodeType::CharLiteral(x) => Ok(RuntimeValue::Char(x)),
        NodeType::Try { value } => {
            let mut value = evaluate(*value, scope)?;

            match value {
                RuntimeValue::Result(Err(_), _) | RuntimeValue::Option(None, _) => {
                    get_global_scope(scope).borrow_mut().stop = Some(StopValue::Return);
                }
                RuntimeValue::Result(Ok(x), _) => {
                    value = *x;
                }
                RuntimeValue::Option(Some(x), _) => {
                    value = *x;
                }
                _ => {}
            }

            Ok(value)
        }
        NodeType::Return { value } => {
            get_global_scope(scope).borrow_mut().stop = Some(StopValue::Return);
            evaluate(*value, scope)
        }
        NodeType::Break => {
            if get_stop(scope) != Some(StopValue::Return) {
                get_global_scope(scope).borrow_mut().stop = Some(StopValue::Break);
            }
            Ok(RuntimeValue::Null)
        }
        NodeType::Continue => {
            if get_stop(scope) == None {
                get_global_scope(scope).borrow_mut().stop = Some(StopValue::Continue);
            }
            Ok(RuntimeValue::Null)
        }
        NodeType::BinaryExpression { .. } => evaluate_binary_expression(node, scope),
        NodeType::Program(_) => evaluate_program(node, scope),
        NodeType::Identifier(x) => evaluate_identifier(&x, scope),
        NodeType::StructLiteral(_) => evaluate_struct_expression(node, scope),
        NodeType::ListLiteral(_) => evaluate_list_expression(node, scope),
        NodeType::TupleLiteral(_) => evaluate_tuple_expression(node, scope),
        NodeType::CallExpression(_, _) => evaluate_call_expression(node, scope),
        NodeType::VariableDeclaration { .. } => evaluate_variable_declaration(node, scope),
        NodeType::StructDeclaration { .. } => evaluate_struct_declaration(node, scope),
        NodeType::RangeDeclaration { .. } => evaluate_range_expression(node, scope),
        NodeType::AssignmentExpression { .. } => evaluate_assignment_expression(node, scope),
        NodeType::FunctionDeclaration { .. } => evaluate_function_declaration(node, scope),
        NodeType::ComparisonExpression { .. } => evaluate_comparison_expression(node, scope),
        NodeType::BooleanExpression { .. } => evaluate_boolean_expression(node, scope),
        NodeType::IfStatement { .. } => evaluate_if_statement(node, scope),
        NodeType::ImportStatement { .. } => evaluate_import_statement(node, scope),
        NodeType::MatchDeclaration { .. } => evaluate_match_statement(node, scope),
        NodeType::InDeclaration { .. } => evaluate_in_statement(node, scope),
        NodeType::AsExpression { .. } => evaluate_as_expression(node, scope),
        NodeType::MemberExpression { .. } => evaluate_member_expression(node, scope),
        NodeType::ImplDeclaration { .. } => evaluate_impl_declaration(node, scope),
        NodeType::ScopeDeclaration { .. } => evaluate_scope(node, scope),
        NodeType::NotExpression { .. } => evaluate_not(node, scope),
        NodeType::EnumDeclaration { .. } => evaluate_enum_declaration(node, scope),
        NodeType::EnumExpression { .. } => evaluate_enum_expression(node, scope),
        NodeType::LoopDeclaration { .. } => evaluate_loop_declaration(node, scope),
        NodeType::IterExpression { .. } => evaluate_iter_expression(node, scope),
        _ => Err(InterpreterErr::NotImplemented(node)),
    }
}

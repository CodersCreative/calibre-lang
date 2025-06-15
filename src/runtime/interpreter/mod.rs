pub mod expressions;
pub mod statements;

use core::panic;

use crate::{
    ast::{BinaryOperator, NodeType},
    runtime::{
        interpreter::{expressions::*, statements::*},
        scope::Scope,
        values::RuntimeValue,
    },
};

pub fn evaluate(node: NodeType, scope: &mut Scope) -> RuntimeValue {
    match node {
        NodeType::NumericLiteral(x) => RuntimeValue::Number(x),
        NodeType::BinaryExpression { .. } => evaluate_binary_expression(node, scope),
        NodeType::Program(_) => evaluate_program(node, scope),
        NodeType::Identifier(x) => evaluate_identifier(&x, scope),
        NodeType::VariableDeclaration { .. } => evaluate_variable_declaration(node, scope),
        _ => panic!("This AST Node has not been implemented."),
    }
}

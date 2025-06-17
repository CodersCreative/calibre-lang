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
        NodeType::FloatLiteral(x) => RuntimeValue::Float(x),
        NodeType::IntegerLiteral(x) => RuntimeValue::Integer(x),
        NodeType::StringLiteral(x) => RuntimeValue::Str(x),
        NodeType::CharLiteral(x) => RuntimeValue::Char(x),
        NodeType::BinaryExpression { .. } => evaluate_binary_expression(node, scope),
        NodeType::Program(_) => evaluate_program(node, scope),
        NodeType::Identifier(x) => evaluate_identifier(&x, scope),
        NodeType::MapLiteral(_) => evaluate_object_expression(node, scope),
        NodeType::CallExpression(_, _) => evaluate_call_expression(node, scope),
        NodeType::VariableDeclaration { .. } => evaluate_variable_declaration(node, scope),
        NodeType::StructDeclaration { .. } => evaluate_struct_declaration(node, scope),
        NodeType::AssignmentExpression { .. } => evaluate_assignment_expression(node, scope),
        NodeType::FunctionDeclaration { .. } => evaluate_function_declaration(node, scope),
        _ => panic!("This AST Node has not been implemented. {:?}", node),
    }
}

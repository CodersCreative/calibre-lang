pub mod expressions;
pub mod statements;

use thiserror::Error;

use crate::{
    ast::{NodeType, binary::ASTError},
    runtime::{
        scope::{Environment, ScopeErr},
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

impl Environment {
    pub fn evaluate(
        &mut self,
        scope: &u64,
        node: NodeType,
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
                let mut value = self.evaluate(scope, *value)?;

                match value {
                    RuntimeValue::Result(Err(_), _) | RuntimeValue::Option(None, _) => {
                        self.stop = Some(StopValue::Return);
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
                self.stop = Some(StopValue::Return);
                self.evaluate(scope, *value)
            }
            NodeType::Break => {
                if self.stop != Some(StopValue::Return) {
                    self.stop = Some(StopValue::Break);
                }
                Ok(RuntimeValue::Null)
            }
            NodeType::Continue => {
                if self.stop == None {
                    self.stop = Some(StopValue::Continue);
                }
                Ok(RuntimeValue::Null)
            }
            NodeType::BinaryExpression { .. } => self.evaluate_binary_expression(scope, node),
            NodeType::Program(_) => self.evaluate_program(scope, node),
            NodeType::Identifier(x) => self.evaluate_identifier(scope, &x),
            NodeType::StructLiteral(_) => self.evaluate_struct_expression(scope, node),
            NodeType::ListLiteral(_) => self.evaluate_list_expression(scope, node),
            NodeType::TupleLiteral(_) => self.evaluate_tuple_expression(scope, node),
            NodeType::CallExpression(_, _) => self.evaluate_call_expression(scope, node),
            NodeType::VariableDeclaration { .. } => self.evaluate_variable_declaration(scope, node),
            NodeType::RangeDeclaration { .. } => self.evaluate_range_expression(scope, node),
            NodeType::AssignmentExpression { .. } => {
                self.evaluate_assignment_expression(scope, node)
            }
            NodeType::FunctionDeclaration { .. } => self.evaluate_function_declaration(scope, node),
            NodeType::ComparisonExpression { .. } => {
                self.evaluate_comparison_expression(scope, node)
            }
            NodeType::BooleanExpression { .. } => self.evaluate_boolean_expression(scope, node),
            NodeType::IfStatement { .. } => self.evaluate_if_statement(scope, node),
            NodeType::ImportStatement { .. } => self.evaluate_import_statement(scope, node),
            NodeType::MatchDeclaration { .. } => self.evaluate_match_declaration(scope, node),
            NodeType::InDeclaration { .. } => self.evaluate_in_statement(scope, node),
            NodeType::AsExpression { .. } => self.evaluate_as_expression(scope, node),
            NodeType::MemberExpression { .. } => self.evaluate_member_expression(scope, node),
            NodeType::ImplDeclaration { .. } => self.evaluate_impl_declaration(scope, node),
            NodeType::ScopeDeclaration { .. } => self.evaluate_scope(scope, node),
            NodeType::NotExpression { .. } => self.evaluate_not(scope, node),
            NodeType::TypeDeclaration { .. } => self.evaluate_type_declaration(scope, node),
            NodeType::PipeExpression { .. } => self.evaluate_pipe_expression(scope, node),
            NodeType::EnumExpression { .. } => self.evaluate_enum_expression(scope, node),
            NodeType::LoopDeclaration { .. } => self.evaluate_loop_declaration(scope, node),
            NodeType::IterExpression { .. } => self.evaluate_iter_expression(scope, node),
            _ => Err(InterpreterErr::NotImplemented(node)),
        }
    }
}

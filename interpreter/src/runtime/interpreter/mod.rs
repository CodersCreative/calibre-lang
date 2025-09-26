use crate::{
    operators::binary::ASTError,
    runtime::{
        scope::{Environment, ScopeErr},
        values::{RuntimeType, RuntimeValue, ValueErr, helper::StopValue},
    },
};
use calibre_parser::ast::NodeType;
use thiserror::Error;

pub mod expressions;
pub mod statements;

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
            NodeType::BinaryExpression { left, right, operator } => {
                let (left, right) = (self.evaluate(scope, *left)?, self.evaluate(scope, *right)?);
                self.evaluate_binary_expression(scope, left, right, operator)
            },
            NodeType::Identifier(x) => self.evaluate_identifier(scope, &x),
            NodeType::StructLiteral(obj) => self.evaluate_struct_expression(scope, obj),
            NodeType::ListLiteral(vals) => self.evaluate_list_expression(scope, vals),
            NodeType::TupleLiteral(vals) => self.evaluate_tuple_expression(scope, vals),
            NodeType::CallExpression(caller, args) => self.evaluate_call_expression(scope, *caller, args),
            NodeType::VariableDeclaration { var_type, identifier, value, data_type } => {
                let value = self.evaluate(scope, *value)?;
                self.evaluate_variable_declaration(scope, var_type, identifier, value, match data_type {Some(x) => Some(x.into()), None => None })
            },
            NodeType::RangeDeclaration { from, to, inclusive } => {
                let (from, to) = (self.evaluate(scope, *from)?, self.evaluate(scope, *to)?);
                self.evaluate_range_expression(scope, from, to, inclusive)
            },
            NodeType::IsDeclaration { value, data_type } => {
                let value = self.evaluate(scope, *value)?;
                self.evaluate_is_expression(scope, value, data_type.into())
            },
            NodeType::AssignmentExpression { identifier, value } => {
                let value = self.evaluate(scope, *value)?;
                self.evaluate_assignment_expression(scope, *identifier, value)
            }
            NodeType::FunctionDeclaration { .. } => self.evaluate_function_declaration(scope, node),
            NodeType::ComparisonExpression { left, right, operator } => {
                let (left, right) = (self.evaluate(scope, *left)?, self.evaluate(scope, *right)?);
                self.evaluate_comparison_expression(scope, left, right, operator)
            },
            NodeType::BooleanExpression { left, right, operator } => {
                let (left, right) = (self.evaluate(scope, *left)?, self.evaluate(scope, *right)?);
                self.evaluate_boolean_expression(scope, left, right, operator)
            },
            NodeType::IfStatement { comparison, then, otherwise } => self.evaluate_if_statement(scope, *comparison, *then, match otherwise {
                Some(x) => Some(*x),
                None => None,
            }),
            NodeType::ImportStatement { module, alias, values } => self.evaluate_import_statement(scope, module, alias, values),
            NodeType::MatchDeclaration { .. } => self.evaluate_match_declaration(scope, node),
            NodeType::InDeclaration { identifier, expression } => {
                let (identifier, expression) = (self.evaluate(scope, *identifier)?, self.evaluate(scope, *expression)?);
                self.evaluate_in_statement(scope, identifier, expression)
            },
            NodeType::AsExpression { value, typ } => {
                let value = self.evaluate(scope, *value)?;
                self.evaluate_as_expression(scope, value, typ.into())
            },
            NodeType::MemberExpression { .. } => self.evaluate_member_expression(scope, node),
            NodeType::ImplDeclaration { identifier, functions } => self.evaluate_impl_declaration(scope, identifier, functions),
            NodeType::ScopeDeclaration { body, is_temp } => self.evaluate_scope(scope, body, is_temp),
            NodeType::NotExpression { value } => {
                let value = self.evaluate(scope, *value)?;
                self.evaluate_not(scope, value)
            },
            NodeType::TypeDeclaration { identifier, object } => self.evaluate_type_declaration(scope, identifier, object.into()),
            NodeType::PipeExpression(nodes) => self.evaluate_pipe_expression(scope, nodes),
            NodeType::EnumExpression { identifier, value, data } => self.evaluate_enum_expression(scope, identifier, value, data),
            NodeType::LoopDeclaration { loop_type, body } => self.evaluate_loop_declaration(scope, *loop_type, *body),
            NodeType::IterExpression { map, loop_type, conditionals } => self.evaluate_iter_expression(scope, *map, *loop_type, conditionals),
        }
    }
}

use crate::runtime::{
    interpreter::expressions::member::MembrExprPathRes,
    scope::InterpreterEnvironment,
    values::{RuntimeType, RuntimeValue, helper::StopValue},
};
use calibre_common::environment::InterpreterFrom;
use calibre_common::errors::RuntimeErr;
use calibre_mir_ty::{MiddleNode, MiddleNodeType};
use calibre_parser::ast::{CompStage, VarType};

pub mod expressions;
pub mod statements;

pub type InterpreterErr = RuntimeErr<RuntimeValue, RuntimeType>;

impl InterpreterEnvironment {
    pub fn evaluate(
        &mut self,
        scope: &u64,
        node: MiddleNode,
    ) -> Result<RuntimeValue, InterpreterErr> {
        match node.node_type {
            MiddleNodeType::FloatLiteral(x) => Ok(RuntimeValue::Float(x as f64)),
            MiddleNodeType::IntLiteral(x) => Ok(RuntimeValue::Int(x as i64)),
            MiddleNodeType::StringLiteral(x) => Ok(RuntimeValue::Str(x.to_string())),
            MiddleNodeType::CharLiteral(x) => Ok(RuntimeValue::Char(x)),
            MiddleNodeType::DebugExpression {
                value,
                pretty_printed_str,
            } => {
                let val = self.evaluate(scope, *value)?;

                println!("Node : {}", pretty_printed_str);
                println!("Evaluates to : {}", val.to_string());
                Ok(val)
            }
            MiddleNodeType::DerefStatement { value } => {
                if let RuntimeValue::Ref(pointer, _) = match value.node_type.clone() {
                    MiddleNodeType::Identifier(x) => self.get_var_ref(&x)?,
                    MiddleNodeType::MemberExpression { path } => {
                        let MembrExprPathRes::Path(path) =
                            self.get_member_expression_path(scope, path)?
                        else {
                            return Err(InterpreterErr::DerefNonRef(value.node_type));
                        };

                        self.get_member_ref(&path)?
                    }
                    _ => return Err(InterpreterErr::DerefNonRef(value.node_type)),
                } {
                    let value = self.get_var(&pointer)?.value.clone();

                    if let RuntimeValue::Ref(pointer, _) = value {
                        return Ok(self.get_var(&pointer)?.value.clone());
                    }

                    Ok(value)
                } else {
                    Err(RuntimeErr::DerefNonRef(value.node_type))
                }
            }
            MiddleNodeType::RefStatement {
                mutability: _,
                value,
            } => {
                let value = match value.node_type.clone() {
                    MiddleNodeType::Identifier(x) => self.get_var_ref(&x)?,
                    MiddleNodeType::MemberExpression { path } => {
                        let MembrExprPathRes::Path(path) =
                            self.get_member_expression_path(scope, path)?
                        else {
                            return Err(InterpreterErr::RefNonVar(value.node_type));
                        };

                        self.get_member_ref(&path)?
                    }
                    _ => return Err(InterpreterErr::RefNonVar(value.node_type)),
                };

                Ok(value)
            }
            MiddleNodeType::Return { value } => {
                self.stop = Some(StopValue::Return);
                if let Some(value) = value {
                    self.evaluate(scope, *value)
                } else {
                    Ok(RuntimeValue::Null)
                }
            }
            MiddleNodeType::Break => {
                if self.stop != Some(StopValue::Return) {
                    self.stop = Some(StopValue::Break);
                }
                Ok(RuntimeValue::Null)
            }
            MiddleNodeType::Continue => {
                if self.stop == None {
                    self.stop = Some(StopValue::Continue);
                }
                Ok(RuntimeValue::Null)
            }
            MiddleNodeType::BinaryExpression {
                left,
                right,
                operator,
            } => {
                let (left, right) = (self.evaluate(scope, *left)?, self.evaluate(scope, *right)?);
                self.evaluate_binary_expression(scope, left, right, operator)
            }
            MiddleNodeType::Identifier(x) => self.evaluate_identifier(&x),
            MiddleNodeType::AggregateExpression { identifier, value } => {
                self.evaluate_aggregate_expression(scope, identifier.map(|x| x.text), value)
            }
            MiddleNodeType::ListLiteral(typ, vals) => {
                self.evaluate_list_expression(scope, typ, vals)
            }
            MiddleNodeType::CallExpression(caller, args) => {
                self.evaluate_call_expression(scope, *caller, args)
            }
            MiddleNodeType::VariableDeclaration {
                var_type,
                identifier,
                value,
                data_type,
            } => {
                let value = self.evaluate(scope, *value)?;
                self.evaluate_variable_declaration(
                    scope,
                    var_type,
                    identifier.to_string(),
                    value,
                    Some(RuntimeType::interpreter_from(self, scope, data_type)?),
                )
            }
            MiddleNodeType::RangeDeclaration {
                from,
                to,
                inclusive,
            } => {
                let (from, to) = (self.evaluate(scope, *from)?, self.evaluate(scope, *to)?);
                self.evaluate_range_expression(scope, from, to, inclusive)
            }
            MiddleNodeType::IsDeclaration { value, data_type } => {
                let value = self.evaluate(scope, *value)?;
                self.evaluate_is_expression(
                    scope,
                    value,
                    RuntimeType::interpreter_from(self, scope, data_type)?,
                )
            }
            MiddleNodeType::AssignmentExpression { identifier, value } => {
                let value = self.evaluate(scope, *value)?;
                self.evaluate_assignment_expression(scope, *identifier, value)
            }
            MiddleNodeType::FunctionDeclaration { .. } => {
                self.evaluate_function_declaration(scope, node)
            }
            MiddleNodeType::ComparisonExpression {
                left,
                right,
                operator,
            } => {
                let (left, right) = (self.evaluate(scope, *left)?, self.evaluate(scope, *right)?);
                self.evaluate_comparison_expression(scope, left, right, operator)
            }
            MiddleNodeType::BooleanExpression {
                left,
                right,
                operator,
            } => {
                let (left, right) = (self.evaluate(scope, *left)?, self.evaluate(scope, *right)?);
                self.evaluate_boolean_expression(scope, left, right, operator)
            }
            MiddleNodeType::IfStatement {
                comparison,
                then,
                otherwise,
                ..
            } => self.evaluate_if_statement(
                scope,
                *comparison,
                *then,
                match otherwise {
                    Some(x) => Some(*x),
                    None => None,
                },
            ),
            MiddleNodeType::InDeclaration { identifier, value } => {
                let (identifier, value) = (
                    self.evaluate(scope, *identifier)?,
                    self.evaluate(scope, *value)?,
                );
                self.evaluate_in_statement(scope, identifier, value)
            }
            MiddleNodeType::AsExpression { value, data_type } => {
                let value = self.evaluate(scope, *value)?;
                self.evaluate_as_expression(
                    scope,
                    value,
                    RuntimeType::interpreter_from(self, scope, data_type)?,
                )
            }
            MiddleNodeType::MemberExpression { .. } => self.evaluate_member_expression(scope, node),
            MiddleNodeType::ScopeDeclaration {
                body,
                is_temp,
                create_new_scope,
            } => self.evaluate_scope(scope, body, is_temp, create_new_scope),
            MiddleNodeType::NegExpression { value } => {
                let value = self.evaluate(scope, *value)?;
                self.evaluate_neg(scope, value)
            }
            MiddleNodeType::NotExpression { value } => {
                let value = self.evaluate(scope, *value)?;
                self.evaluate_not(scope, value)
            }
            MiddleNodeType::EnumExpression {
                identifier,
                value,
                data,
            } => self.evaluate_enum_expression(
                scope,
                identifier.to_string(),
                value.to_string(),
                data,
            ),
            MiddleNodeType::LoopDeclaration { body, state } => {
                self.evaluate_loop_declaration(scope, state.map(|x| *x), *body)
            }
            MiddleNodeType::DataType { data_type } => Ok(RuntimeValue::Type(data_type)),
            MiddleNodeType::Comp {
                stage: CompStage::Wildcard,
                body,
            } => self.evaluate(scope, *body),
            MiddleNodeType::EmptyLine => Ok(RuntimeValue::Null),
            _ => Err(InterpreterErr::UnexpectedNodeInTemp(node.node_type)),
        }
    }

    pub fn evaluate_global(
        &mut self,
        scope: &u64,
        node: MiddleNode,
    ) -> Result<RuntimeValue, InterpreterErr> {
        match node.node_type {
            MiddleNodeType::Comp {
                stage: CompStage::Wildcard,
                body,
            } => self.evaluate_global(scope, *body),
            MiddleNodeType::ScopeDeclaration {
                body,
                is_temp,
                create_new_scope,
            } => self.evaluate_scope(scope, body, is_temp, create_new_scope),
            MiddleNodeType::VariableDeclaration {
                var_type: VarType::Constant,
                identifier,
                value,
                data_type,
            } => {
                let value = self.evaluate(scope, *value)?;
                self.evaluate_variable_declaration(
                    scope,
                    VarType::Constant,
                    identifier.to_string(),
                    value,
                    Some(RuntimeType::interpreter_from(self, scope, data_type)?),
                )
            }
            _ => Err(InterpreterErr::UnexpectedNodeInGlobal(node.node_type)),
        }
    }

    pub fn handle_conditionals(
        &mut self,
        scope: &u64,
        conditionals: Vec<MiddleNode>,
    ) -> Result<bool, InterpreterErr> {
        let mut result = true;

        for condition in conditionals.into_iter() {
            if let RuntimeValue::Bool(value) = self.evaluate(scope, condition)? {
                result = result && value;
            }
        }

        Ok(result)
    }
}

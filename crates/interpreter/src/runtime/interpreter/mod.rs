use crate::runtime::{
    interpreter::expressions::member::MembrExprPathRes,
    scope::InterpreterEnvironment,
    values::{RuntimeType, RuntimeValue, helper::StopValue},
};
use calibre_common::errors::RuntimeErr;
use calibre_parser::ast::{Node, NodeType, VarType};

pub mod expressions;
pub mod statements;

pub type InterpreterErr = RuntimeErr<RuntimeValue, RuntimeType>;

impl InterpreterEnvironment {
    pub fn evaluate(&mut self, scope: &u64, node: Node) -> Result<RuntimeValue, InterpreterErr> {
        self.current_location = self.get_location(scope, node.span);

        match node.node_type {
            NodeType::FloatLiteral(x) => Ok(RuntimeValue::Float(x as f64)),
            NodeType::IntLiteral(x) => Ok(RuntimeValue::Int(x as i64)),
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
            NodeType::DebugExpression { value } => {
                let val = self.evaluate(scope, *value.clone())?;
                println!("Debug at span : {:?}", self.current_location);
                println!("Node : {:?}", value);
                println!("Evaluates to : {}", val.to_string());
                Ok(val)
            }
            NodeType::DerefStatement { value } => {
                if let RuntimeValue::Ref(pointer, _) = match value.node_type.clone() {
                    NodeType::Identifier(x) => self.get_var_ref(scope, &x)?,
                    NodeType::MemberExpression { path } => {
                        let MembrExprPathRes::Path(path) =
                            self.get_member_expression_path(scope, path)?
                        else {
                            return Err(InterpreterErr::DerefNonRef(value.node_type));
                        };

                        self.get_member_ref(scope, &path)?
                    }
                    _ => return Err(InterpreterErr::DerefNonRef(value.node_type)),
                } {
                    let value = self.get_value_from_ref_pointer(&pointer)?.value;

                    Ok(value)
                } else {
                    Err(RuntimeErr::DerefNonRef(value.node_type))
                }
            }
            NodeType::RefStatement {
                mutability: _,
                value,
            } => {
                let value = match value.node_type.clone() {
                    NodeType::Identifier(x) => self.get_var_ref(scope, &x)?,
                    NodeType::MemberExpression { path } => {
                        let MembrExprPathRes::Path(path) =
                            self.get_member_expression_path(scope, path)?
                        else {
                            return Err(InterpreterErr::RefNonVar(value.node_type));
                        };

                        self.get_member_ref(scope, &path)?
                    }
                    _ => return Err(InterpreterErr::RefNonVar(value.node_type)),
                };

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
            NodeType::BinaryExpression {
                left,
                right,
                operator,
            } => {
                let (left, right) = (self.evaluate(scope, *left)?, self.evaluate(scope, *right)?);
                self.evaluate_binary_expression(scope, left, right, operator)
            }
            NodeType::Identifier(x) => self.evaluate_identifier(scope, &x),
            NodeType::StructLiteral(obj) => self.evaluate_struct_expression(scope, obj),
            NodeType::ListLiteral(vals) => self.evaluate_list_expression(scope, vals),
            NodeType::CallExpression(caller, args) => {
                self.evaluate_call_expression(scope, *caller, args)
            }
            NodeType::VariableDeclaration {
                var_type,
                identifier,
                value,
                data_type,
            } => {
                let value = self.evaluate(scope, *value)?;
                self.evaluate_variable_declaration(
                    scope,
                    var_type,
                    identifier,
                    value,
                    match data_type {
                        Some(x) => Some(x.into()),
                        None => None,
                    },
                )
            }
            NodeType::RangeDeclaration {
                from,
                to,
                inclusive,
            } => {
                let (from, to) = (self.evaluate(scope, *from)?, self.evaluate(scope, *to)?);
                self.evaluate_range_expression(scope, from, to, inclusive)
            }
            NodeType::IsDeclaration { value, data_type } => {
                let value = self.evaluate(scope, *value)?;
                self.evaluate_is_expression(scope, value, data_type.into())
            }
            NodeType::AssignmentExpression { identifier, value } => {
                let value = self.evaluate(scope, *value)?;
                self.evaluate_assignment_expression(scope, *identifier, value)
            }
            NodeType::FunctionDeclaration { .. } => self.evaluate_function_declaration(scope, node),
            NodeType::ComparisonExpression {
                left,
                right,
                operator,
            } => {
                let (left, right) = (self.evaluate(scope, *left)?, self.evaluate(scope, *right)?);
                self.evaluate_comparison_expression(scope, left, right, operator)
            }
            NodeType::BooleanExpression {
                left,
                right,
                operator,
            } => {
                let (left, right) = (self.evaluate(scope, *left)?, self.evaluate(scope, *right)?);
                self.evaluate_boolean_expression(scope, left, right, operator)
            }
            NodeType::IfStatement {
                comparison,
                then,
                otherwise,
            } => self.evaluate_if_statement(
                scope,
                *comparison,
                *then,
                match otherwise {
                    Some(x) => Some(*x),
                    None => None,
                },
            ),
            NodeType::MatchDeclaration { .. } => self.evaluate_match_declaration(scope, node),
            NodeType::InDeclaration {
                identifier,
                expression,
            } => {
                let (identifier, expression) = (
                    self.evaluate(scope, *identifier)?,
                    self.evaluate(scope, *expression)?,
                );
                self.evaluate_in_statement(scope, identifier, expression)
            }
            NodeType::AsExpression { value, typ } => {
                let value = self.evaluate(scope, *value)?;
                self.evaluate_as_expression(scope, value, typ.into())
            }
            NodeType::MemberExpression { .. } => self.evaluate_member_expression(scope, node),
            NodeType::ScopeMemberExpression { path } => {
                self.evaluate_scope_member_expression(scope, path)
            }
            NodeType::ScopeDeclaration { body, is_temp } => {
                self.evaluate_scope(scope, body, is_temp)
            }
            NodeType::NotExpression { value } => {
                let value = self.evaluate(scope, *value)?;
                self.evaluate_not(scope, value)
            }
            NodeType::PipeExpression(nodes) => self.evaluate_pipe_expression(scope, nodes),
            NodeType::EnumExpression {
                identifier,
                value,
                data,
            } => self.evaluate_enum_expression(scope, identifier, value, data),
            NodeType::LoopDeclaration { loop_type, body } => {
                self.evaluate_loop_declaration(scope, *loop_type, *body)
            }
            NodeType::IterExpression {
                map,
                loop_type,
                conditionals,
            } => self.evaluate_iter_expression(scope, *map, *loop_type, conditionals),
            _ => Err(InterpreterErr::UnexpectedNodeInTemp(node.node_type)),
        }
    }

    pub fn evaluate_global(
        &mut self,
        scope: &u64,
        node: Node,
    ) -> Result<RuntimeValue, InterpreterErr> {
        self.current_location = self.get_location(scope, node.span);

        match node.node_type {
            NodeType::VariableDeclaration {
                var_type: VarType::Constant,
                identifier,
                value,
                data_type,
            } => {
                let value = self.evaluate(scope, *value)?;
                self.evaluate_variable_declaration(
                    scope,
                    VarType::Constant,
                    identifier,
                    value,
                    match data_type {
                        Some(x) => Some(x.into()),
                        None => None,
                    },
                )
            }
            NodeType::ImportStatement {
                module,
                alias,
                values,
            } => self.evaluate_import_statement(scope, module, alias, values),
            NodeType::ImplDeclaration {
                identifier,
                functions,
            } => self.evaluate_impl_declaration(scope, identifier, functions),
            NodeType::TypeDeclaration { identifier, object } => {
                self.evaluate_type_declaration(scope, identifier, object.into())
            }
            _ => Err(InterpreterErr::UnexpectedNodeInGlobal(node.node_type)),
        }
    }
}

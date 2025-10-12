use calibre_common::{
    environment::{Location, RuntimeValue},
    errors::RuntimeErr,
};
use calibre_parser::ast::{Node, NodeType};

use crate::runtime::{
    interpreter::expressions::member::MembrExprPathRes, scope::CheckerEnvironment,
    values::RuntimeType,
};

pub mod expressions;
pub mod statements;

pub type InterpreterErr = RuntimeErr<RuntimeType, RuntimeType>;

impl CheckerEnvironment {
    pub fn evaluate(&mut self, scope: &u64, node: Node) -> Result<RuntimeType, InterpreterErr> {
        self.current_location = self.get_location(scope, node.span);

        match node.node_type {
            NodeType::FloatLiteral(_) => Ok(RuntimeType::Float),
            NodeType::IntLiteral(_) => Ok(RuntimeType::Int),
            NodeType::StringLiteral(_) => Ok(RuntimeType::Str),
            NodeType::CharLiteral(_) => Ok(RuntimeType::Char),
            NodeType::Try { value } => self.evaluate(scope, *value),
            NodeType::Return { value } => self.evaluate(scope, *value),
            NodeType::DebugExpression { value } => self.evaluate(scope, *value),
            NodeType::Break => Ok(RuntimeType::Null),
            NodeType::Continue => Ok(RuntimeType::Null),
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
            NodeType::DerefStatement { value } => {
                if let RuntimeType::Ref(t) = match value.node_type.clone() {
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
                } {
                    Ok(*t.clone())
                } else {
                    panic!()
                }
            }
            NodeType::RefStatement { mutability, value } => {
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
            NodeType::IsDeclaration { value, data_type } => Ok(RuntimeType::Bool),
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
            NodeType::ImportStatement {
                module,
                alias,
                values,
            } => self.evaluate_import_statement(scope, module, alias, values),
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
            NodeType::ImplDeclaration {
                identifier,
                functions,
            } => self.evaluate_impl_declaration(scope, identifier, functions),
            NodeType::ScopeDeclaration { body, is_temp } => {
                self.evaluate_scope(scope, body, is_temp)
            }
            NodeType::NotExpression { value } => {
                let value = self.evaluate(scope, *value)?;
                self.evaluate_not(scope, value)
            }
            NodeType::TypeDeclaration { identifier, object } => {
                self.evaluate_type_declaration(scope, identifier, object.into())
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
        }
    }
}

use std::{cell::RefCell, rc::Rc};

use crate::{
    ast::NodeType,
    runtime::{
        interpreter::{InterpreterErr, evaluate, expressions::member::assign_member_expression},
        scope::{Scope, variables::get_var},
        values::{RuntimeType, RuntimeValue},
    },
};

pub mod call;
pub mod lists;
pub mod member;
pub mod scope;
pub mod structs;

pub fn evaluate_identifier(
    identifier: &str,
    scope: Rc<RefCell<Scope>>,
) -> Result<RuntimeValue, InterpreterErr> {
    Ok(get_var(scope, identifier)?.0)
}

pub fn evaluate_not<'a>(
    exp: NodeType,
    scope: Rc<RefCell<Scope>>,
) -> Result<RuntimeValue, InterpreterErr> {
    if let NodeType::NotExpression { value } = exp {
        let value = evaluate(*value, scope.clone())?;

        match value {
            RuntimeValue::Bool(x) => Ok(RuntimeValue::Bool(!x)),
            RuntimeValue::Integer(x) => Ok(RuntimeValue::Integer(-x)),
            RuntimeValue::Float(x) => Ok(RuntimeValue::Float(-x)),
            RuntimeValue::Range(f, t) => Ok(RuntimeValue::Range(t, f)),
            RuntimeValue::List {
                mut data,
                data_type,
            } => {
                data.reverse();
                Ok(RuntimeValue::List { data, data_type })
            }
            _ => Err(InterpreterErr::UnexpectedType(value)),
        }
    } else {
        Err(InterpreterErr::NotImplemented(exp))
    }
}

pub fn evaluate_binary_expression(
    exp: NodeType,
    scope: Rc<RefCell<Scope>>,
) -> Result<RuntimeValue, InterpreterErr> {
    if let NodeType::BinaryExpression {
        left,
        right,
        operator,
    } = exp
    {
        let left = evaluate(*left, scope.clone())?;
        let right = evaluate(*right, scope.clone())?;

        Ok(operator.handle(left, right)?)
    } else {
        Err(InterpreterErr::NotImplemented(exp))
    }
}

pub fn evaluate_range_expression(
    exp: NodeType,
    scope: Rc<RefCell<Scope>>,
) -> Result<RuntimeValue, InterpreterErr> {
    if let NodeType::RangeDeclaration {
        from,
        to,
        inclusive,
    } = exp
    {
        if let RuntimeValue::Integer(from) = evaluate(*from.clone(), scope.clone())?
            .into_type(scope.clone(), RuntimeType::Integer)?
        {
            if let RuntimeValue::Integer(to) = evaluate(*to.clone(), scope.clone())?
                .into_type(scope.clone(), RuntimeType::Integer)?
            {
                let to = if inclusive { to + 1 } else { to };

                Ok(RuntimeValue::Range(from as i32, to as i32))
            } else {
                Err(InterpreterErr::NotImplemented(*to))
            }
        } else {
            Err(InterpreterErr::NotImplemented(*from))
        }
    } else {
        Err(InterpreterErr::NotImplemented(exp))
    }
}

pub fn evaluate_boolean_expression(
    exp: NodeType,
    scope: Rc<RefCell<Scope>>,
) -> Result<RuntimeValue, InterpreterErr> {
    if let NodeType::BooleanExpression {
        left,
        right,
        operator,
    } = exp
    {
        let left = evaluate(*left, scope.clone())?;
        let right = evaluate(*right, scope.clone())?;

        Ok(operator.handle(left, right)?)
    } else {
        Err(InterpreterErr::NotImplemented(exp))
    }
}

pub fn evaluate_comparison_expression(
    exp: NodeType,
    scope: Rc<RefCell<Scope>>,
) -> Result<RuntimeValue, InterpreterErr> {
    if let NodeType::ComparisonExpression {
        left,
        right,
        operator,
    } = exp
    {
        let left = evaluate(*left, scope.clone())?;
        let right = evaluate(*right, scope.clone())?;

        Ok(operator.handle(left, right))
    } else {
        Err(InterpreterErr::NotImplemented(exp))
    }
}
pub fn evaluate_assignment_expression(
    node: NodeType,
    scope: Rc<RefCell<Scope>>,
) -> Result<RuntimeValue, InterpreterErr> {
    if let NodeType::AssignmentExpression { identifier, value } = node {
        if let NodeType::Identifier(identifier) = *identifier {
            let value = evaluate(*value, scope.clone())?;
            let _ = scope.borrow_mut().assign_var(&identifier, value.clone())?;
            return Ok(value);
        }
        if let NodeType::MemberExpression { .. } = *identifier {
            let value = evaluate(*value, scope.clone())?;
            let _ = assign_member_expression(*identifier, value.clone(), scope)?;
            return Ok(value);
        } else {
            Err(InterpreterErr::AssignNonVariable(*identifier))
        }
    } else {
        Err(InterpreterErr::NotImplemented(node))
    }
}

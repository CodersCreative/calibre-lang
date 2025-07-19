use std::{cell::RefCell, rc::Rc};

use crate::{
    ast::NodeType,
    runtime::{
        interpreter::{InterpreterErr, evaluate},
        scope::Scope,
        values::{
            RuntimeValue,
            helper::{Block, VarType},
        },
    },
};

pub mod comparisons;
pub mod loops;
pub mod matching;
pub mod structs;

pub fn evaluate_program(
    exp: NodeType,
    scope: &Rc<RefCell<Scope>>,
) -> Result<RuntimeValue, InterpreterErr> {
    let mut last = RuntimeValue::Null;

    if let NodeType::Program(body) = exp {
        for statement in body.into_iter() {
            last = evaluate(statement, scope)?;
        }
    } else {
        return Err(InterpreterErr::NotImplemented(exp));
    }

    Ok(last)
}

pub fn evaluate_function_declaration(
    declaration: NodeType,
    scope: &Rc<RefCell<Scope>>,
) -> Result<RuntimeValue, InterpreterErr> {
    if let NodeType::FunctionDeclaration {
        identifier,
        parameters,
        body,
        return_type,
        is_async,
    } = declaration
    {
        let mut params = Vec::new();

        for p in parameters.into_iter() {
            let default = if let Some(node) = p.3 {
                Some(evaluate(node, scope)?)
            } else {
                None
            };

            params.push((p.0, p.1, p.2, default));
        }

        let value = RuntimeValue::Function {
            identifier: identifier.clone(),
            parameters: params,
            body: Block(body),
            return_type,
            is_async,
        };

        let _ = scope
            .borrow_mut()
            .push_var(identifier, value.clone(), VarType::Immutable(None))?;
        Ok(value)
    } else {
        Err(InterpreterErr::NotImplemented(declaration))
    }
}

pub fn evaluate_variable_declaration(
    declaration: NodeType,
    scope: &Rc<RefCell<Scope>>,
) -> Result<RuntimeValue, InterpreterErr> {
    if let NodeType::VariableDeclaration {
        var_type,
        identifier,
        value,
        data_type,
    } = declaration
    {
        let mut value = match value {
            Some(x) => evaluate(*x, scope)?,
            None => RuntimeValue::Null,
        };

        if let Some(t) = data_type {
            value = value.into_type(scope, &t)?;
        }

        let _ = scope
            .borrow_mut()
            .push_var(identifier, value.clone(), var_type)?;

        Ok(value)
    } else {
        Err(InterpreterErr::NotImplemented(declaration))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{LoopType, NodeType, RefMutability};
    use crate::runtime::scope::Scope;
    use crate::runtime::values::{
        RuntimeValue,
        helper::{ObjectType, VarType},
    };
    use std::cell::RefCell;
    use std::rc::Rc;

    fn new_scope() -> Rc<RefCell<Scope>> {
        Rc::new(RefCell::new(Scope::new(None)))
    }

    #[test]
    fn test_evaluate_variable_declaration() {
        let scope = new_scope();
        let node = NodeType::VariableDeclaration {
            var_type: VarType::Mutable(None),
            identifier: "x".to_string(),
            value: Some(Box::new(NodeType::IntLiteral(42))),
            data_type: None,
        };
        let result = evaluate_variable_declaration(node, scope.clone()).unwrap();
        assert_eq!(result, RuntimeValue::Int(42));
        assert_eq!(
            scope.borrow().variables.get("x").unwrap().0,
            RuntimeValue::Int(42)
        );
    }

    #[test]
    fn test_evaluate_function_declaration() {
        let scope = new_scope();
        let node = NodeType::FunctionDeclaration {
            identifier: "foo".to_string(),
            parameters: vec![],
            body: vec![NodeType::Return {
                value: Box::new(NodeType::IntLiteral(1)),
            }],
            return_type: None,
            is_async: false,
        };
        let result = evaluate_function_declaration(node, scope.clone()).unwrap();
        match result {
            RuntimeValue::Function { identifier, .. } => assert_eq!(identifier, "foo"),
            _ => panic!("Expected RuntimeValue::Function"),
        }
        assert!(scope.borrow().variables.contains_key("foo"));
    }
}

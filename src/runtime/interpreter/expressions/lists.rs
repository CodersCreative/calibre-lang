use std::{cell::RefCell, mem::discriminant, rc::Rc};

use crate::{
    ast::NodeType,
    runtime::{
        interpreter::{evaluate, InterpreterErr},
        scope::Scope,
        values::RuntimeValue,
    },
};

pub fn evaluate_tuple_expression(
    obj: NodeType,
    scope: Rc<RefCell<Scope>>,
) -> Result<RuntimeValue, InterpreterErr> {
    let mut values = Vec::new();

    if let NodeType::TupleLiteral(vals) = obj {
        for val in vals.iter() {
            values.push(evaluate(val.clone(), scope.clone())?);
        }
    }

    // let types: Vec<RuntimeType> = values.into_iter().map(|x| x.into()).collect();

    Ok(RuntimeValue::Tuple(values))
}

pub fn evaluate_list_expression(
    obj: NodeType,
    scope: Rc<RefCell<Scope>>,
) -> Result<RuntimeValue, InterpreterErr> {
    let mut values = Vec::new();

    if let NodeType::ListLiteral(vals) = obj {
        for val in vals.iter() {
            values.push(evaluate(val.clone(), scope.clone())?);
        }
    }

    let t = if values.len() > 0 {
        let t = discriminant(&values[0]);
        let filtered: Vec<&RuntimeValue> =
            values.iter().filter(|x| discriminant(*x) == t).collect();
        if values.len() == filtered.len() {
            Some(values[0].clone().into())
        } else {
            None
        }
    } else {
        None
    };

    Ok(RuntimeValue::List {
        data: values,
        data_type: Box::new(t),
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::NodeType;
    use crate::runtime::scope::Scope;
    use crate::runtime::values::RuntimeValue;
    use std::cell::RefCell;
    use std::rc::Rc;

    fn new_scope() -> Rc<RefCell<Scope>> {
        Rc::new(RefCell::new(Scope::new(None)))
    }

    #[test]
    fn test_evaluate_tuple_expression_basic() {
        let scope = new_scope();
        let node = NodeType::TupleLiteral(Box::new(vec![
            NodeType::IntegerLiteral(1),
            NodeType::FloatLiteral(2.0),
        ]));
        let result = evaluate_tuple_expression(node, scope).unwrap();
        assert_eq!(
            result,
            RuntimeValue::Tuple(vec![RuntimeValue::Integer(1), RuntimeValue::Float(2.0)])
        );
    }

    #[test]
    fn test_evaluate_list_expression_homogeneous() {
        let scope = new_scope();
        let node = NodeType::ListLiteral(Box::new(vec![
            NodeType::IntegerLiteral(1),
            NodeType::IntegerLiteral(2),
            NodeType::IntegerLiteral(3),
        ]));
        let result = evaluate_list_expression(node, scope).unwrap();
        match result {
            RuntimeValue::List { data, data_type } => {
                assert_eq!(
                    data,
                    vec![
                        RuntimeValue::Integer(1),
                        RuntimeValue::Integer(2),
                        RuntimeValue::Integer(3)
                    ]
                );
                assert!(data_type.is_some());
            }
            _ => panic!("Expected RuntimeValue::List"),
        }
    }

    #[test]
    fn test_evaluate_list_expression_heterogeneous() {
        let scope = new_scope();
        let node = NodeType::ListLiteral(Box::new(vec![
            NodeType::IntegerLiteral(1),
            NodeType::FloatLiteral(2.0),
        ]));
        let result = evaluate_list_expression(node, scope).unwrap();
        match result {
            RuntimeValue::List { data, data_type } => {
                assert_eq!(
                    data,
                    vec![RuntimeValue::Integer(1), RuntimeValue::Float(2.0)]
                );
                assert!(data_type.is_none());
            }
            _ => panic!("Expected RuntimeValue::List"),
        }
    }

    #[test]
    fn test_evaluate_list_expression_empty() {
        let scope = new_scope();
        let node = NodeType::ListLiteral(Box::new(vec![]));
        let result = evaluate_list_expression(node, scope).unwrap();
        match result {
            RuntimeValue::List { data, data_type } => {
                assert!(data.is_empty());
                assert!(data_type.is_none());
            }
            _ => panic!("Expected RuntimeValue::List"),
        }
    }
}

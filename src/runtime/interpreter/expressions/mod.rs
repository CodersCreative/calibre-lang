use crate::{
    ast::NodeType,
    runtime::{
        interpreter::InterpreterErr,
        scope::Environment,
        values::{RuntimeType, RuntimeValue},
    },
};

pub mod call;
pub mod lists;
pub mod member;
pub mod scope;
pub mod structs;

impl Environment {
    pub fn evaluate_identifier(
        &mut self,
        scope: &u64,
        identifier: &str,
    ) -> Result<RuntimeValue, InterpreterErr> {
        Ok(self.get_var_ref(scope, identifier)?.value.clone())
    }

    pub fn evaluate_not<'a>(
        &mut self,
        scope: &u64,
        exp: NodeType,
    ) -> Result<RuntimeValue, InterpreterErr> {
        if let NodeType::NotExpression { value } = exp {
            match *value {
                NodeType::RangeDeclaration {
                    from,
                    to,
                    inclusive,
                } => {
                    return self.evaluate(
                        scope,
                        NodeType::RangeDeclaration {
                            from: Box::new(NodeType::NotExpression { value: from }),
                            to,
                            inclusive,
                        },
                    );
                }
                _ => {}
            }
            let value = self.evaluate(scope, *value)?;

            match value {
                RuntimeValue::Bool(x) => Ok(RuntimeValue::Bool(!x)),
                RuntimeValue::Int(x) => Ok(RuntimeValue::Int(-x)),
                RuntimeValue::UInt(x) => Ok(RuntimeValue::Int(-(x as i64))),
                RuntimeValue::Float(x) => Ok(RuntimeValue::Float(-x)),
                RuntimeValue::Double(x) => Ok(RuntimeValue::Double(-x)),
                RuntimeValue::ULong(x) => Ok(RuntimeValue::Long(-(x as i128))),
                RuntimeValue::Long(x) => Ok(RuntimeValue::Long(-x)),
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

    pub fn evaluate_as_expression(
        &mut self,
        scope: &u64,
        exp: NodeType,
    ) -> Result<RuntimeValue, InterpreterErr> {
        if let NodeType::AsExpression { value, typ } = exp {
            let value = self.evaluate(scope, *value)?;
            Ok(match value.into_type(self, scope, &typ) {
                Ok(x) => RuntimeValue::Result(
                    Ok(Box::new(x.clone())),
                    RuntimeType::Result(Box::new(RuntimeType::Dynamic), Box::new((&x).into())),
                ),
                Err(e) => RuntimeValue::Result(
                    Err(Box::new(RuntimeValue::Str(String::from(e.to_string())))),
                    RuntimeType::Result(Box::new(RuntimeType::Str), Box::new(RuntimeType::Dynamic)),
                ),
            })
        } else {
            Err(InterpreterErr::NotImplemented(exp))
        }
    }

    pub fn evaluate_binary_expression(
        &mut self,
        scope: &u64,
        exp: NodeType,
    ) -> Result<RuntimeValue, InterpreterErr> {
        if let NodeType::BinaryExpression {
            left,
            right,
            operator,
        } = exp
        {
            let left = self.evaluate(scope, *left)?;
            let right = self.evaluate(scope, *right)?;

            operator.handle(self, scope, left, right)
        } else {
            Err(InterpreterErr::NotImplemented(exp))
        }
    }

    pub fn evaluate_range_expression(
        &mut self,
        scope: &u64,
        exp: NodeType,
    ) -> Result<RuntimeValue, InterpreterErr> {
        if let NodeType::RangeDeclaration {
            from,
            to,
            inclusive,
        } = exp
        {
            if let RuntimeValue::Int(from) =
                self.evaluate(scope, *from.clone())?
                    .into_type(self, scope, &RuntimeType::Int)?
            {
                if let RuntimeValue::Int(to) =
                    self.evaluate(scope, *to.clone())?
                        .into_type(self, scope, &RuntimeType::Int)?
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
        &mut self,
        scope: &u64,
        exp: NodeType,
    ) -> Result<RuntimeValue, InterpreterErr> {
        if let NodeType::BooleanExpression {
            left,
            right,
            operator,
        } = exp
        {
            let left = self
                .evaluate(scope, *left)?
                .into_type(self, scope, &RuntimeType::Bool)?;
            let right = match self.evaluate(scope, *right) {
                Ok(x) => x
                    .into_type(self, scope, &RuntimeType::Bool)
                    .unwrap_or(RuntimeValue::Null),
                _ => RuntimeValue::Null,
            };

            Ok(operator.handle(&left, &right)?)
        } else {
            Err(InterpreterErr::NotImplemented(exp))
        }
    }

    pub fn evaluate_comparison_expression(
        &mut self,
        scope: &u64,
        exp: NodeType,
    ) -> Result<RuntimeValue, InterpreterErr> {
        if let NodeType::ComparisonExpression {
            left,
            right,
            operator,
        } = exp
        {
            let left = self.evaluate(scope, *left)?;
            let right = self.evaluate(scope, *right)?;
            operator.handle(self, scope, left, right)
        } else {
            Err(InterpreterErr::NotImplemented(exp))
        }
    }
    pub fn evaluate_assignment_expression(
        &mut self,
        scope: &u64,
        node: NodeType,
    ) -> Result<RuntimeValue, InterpreterErr> {
        if let NodeType::AssignmentExpression { identifier, value } = node {
            let value = self.evaluate(scope, *value)?;
            if let NodeType::Identifier(identifier) = *identifier {
                let _ = self.assign_var(scope, &identifier, value.clone())?;
                return Ok(value);
            } else if let NodeType::MemberExpression { .. } = *identifier {
                let _ = self.assign_member_expression(scope, *identifier, value.clone())?;
                return Ok(value);
            } else {
                Err(InterpreterErr::AssignNonVariable(*identifier))
            }
        } else {
            Err(InterpreterErr::NotImplemented(node))
        }
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{NodeType, binary::BinaryOperator};
    use crate::runtime::scope::Scope;
    use crate::runtime::values::helper::VarType;
    use crate::runtime::values::{RuntimeType, RuntimeValue};
    use std::cell::RefCell;
    use std::rc::Rc;

    fn new_scope() -> Rc<RefCell<Scope>> {
        Rc::new(RefCell::new(Scope::new(None)))
    }

    #[test]
    fn test_evaluate_identifier() {
        let scope = new_scope();
        scope
            .borrow_mut()
            .push_var(
                "x".to_string(),
                RuntimeValue::Int(5),
                VarType::Mutable(None),
            )
            .unwrap();
        let result = evaluate_identifier("x", scope).unwrap();
        assert_eq!(result, RuntimeValue::Int(5));
    }

    #[test]
    fn test_evaluate_not_bool() {
        let scope = new_scope();
        let node = NodeType::NotExpression {
            value: Box::new(NodeType::Identifier(String::from("true"))),
        };
        let result = evaluate_not(node, scope).unwrap();
        assert_eq!(result, RuntimeValue::Bool(false));
    }

    #[test]
    fn test_evaluate_not_Int() {
        let scope = new_scope();
        let node = NodeType::NotExpression {
            value: Box::new(NodeType::IntLiteral(7)),
        };
        let result = evaluate_not(node, scope).unwrap();
        assert_eq!(result, RuntimeValue::Int(-7));
    }

    #[test]
    fn test_evaluate_not_float() {
        let scope = new_scope();
        let node = NodeType::NotExpression {
            value: Box::new(NodeType::FloatLiteral(3.5)),
        };
        let result = evaluate_not(node, scope).unwrap();
        assert_eq!(result, RuntimeValue::Float(-3.5));
    }

    #[test]
    fn test_evaluate_not_range() {
        let scope = new_scope();
        let node = NodeType::NotExpression {
            value: Box::new(NodeType::RangeDeclaration {
                from: Box::new(NodeType::IntLiteral(1)),
                to: Box::new(NodeType::IntLiteral(3)),
                inclusive: false,
            }),
        };
        let result = evaluate_not(node, scope).unwrap();
        assert_eq!(result, RuntimeValue::Range(3, 1));
    }

    #[test]
    fn test_evaluate_not_list() {
        let scope = new_scope();
        let node = NodeType::NotExpression {
            value: Box::new(NodeType::ListLiteral(vec![
                NodeType::IntLiteral(1),
                NodeType::IntLiteral(2),
                NodeType::IntLiteral(3),
            ])),
        };
        let result = evaluate_not(node, scope).unwrap();
        match result {
            RuntimeValue::List { data, .. } => assert_eq!(
                data,
                vec![
                    RuntimeValue::Int(3),
                    RuntimeValue::Int(2),
                    RuntimeValue::Int(1)
                ]
            ),
            _ => panic!("Expected List"),
        }
    }

    #[test]
    fn test_evaluate_binary_expression_add() {
        let scope = new_scope();
        let node = NodeType::BinaryExpression {
            left: Box::new(NodeType::IntLiteral(2)),
            right: Box::new(NodeType::IntLiteral(3)),
            operator: BinaryOperator::Add,
        };
        let result = evaluate_binary_expression(node, scope).unwrap();
        assert_eq!(result, RuntimeValue::Int(5));
    }

    #[test]
    fn test_evaluate_range_expression_inclusive() {
        let scope = new_scope();
        let node = NodeType::RangeDeclaration {
            from: Box::new(NodeType::IntLiteral(1)),
            to: Box::new(NodeType::IntLiteral(3)),
            inclusive: true,
        };
        let result = evaluate_range_expression(node, scope).unwrap();
        assert_eq!(result, RuntimeValue::Range(1, 4));
    }

    #[test]
    fn test_evaluate_range_expression_exclusive() {
        let scope = new_scope();
        let node = NodeType::RangeDeclaration {
            from: Box::new(NodeType::IntLiteral(1)),
            to: Box::new(NodeType::IntLiteral(3)),
            inclusive: false,
        };
        let result = evaluate_range_expression(node, scope).unwrap();
        assert_eq!(result, RuntimeValue::Range(1, 3));
    }

    #[test]
    fn test_evaluate_boolean_expression_and() {
        let scope = new_scope();
        let node = NodeType::BooleanExpression {
            left: Box::new(NodeType::Identifier(String::from("true"))),
            right: Box::new(NodeType::Identifier(String::from("false"))),
            operator: crate::ast::comparison::BooleanOperation::And,
        };
        let result = evaluate_boolean_expression(node, scope).unwrap();
        assert_eq!(result, RuntimeValue::Bool(false));
    }

    #[test]
    fn test_evaluate_comparison_expression_lesser() {
        let scope = new_scope();
        let node = NodeType::ComparisonExpression {
            left: Box::new(NodeType::IntLiteral(3)),
            right: Box::new(NodeType::IntLiteral(2)),
            operator: crate::ast::comparison::Comparison::Lesser,
        };
        let result = evaluate_comparison_expression(node, scope).unwrap();
        assert_eq!(result, RuntimeValue::Bool(true));
    }

    #[test]
    fn test_evaluate_assignment_expression_identifier() {
        let scope = new_scope();
        scope
            .borrow_mut()
            .push_var(
                "x".to_string(),
                RuntimeValue::Int(0),
                VarType::Mutable(None),
            )
            .unwrap();
        let node = NodeType::AssignmentExpression {
            identifier: Box::new(NodeType::Identifier("x".to_string())),
            value: Box::new(NodeType::IntLiteral(42)),
        };
        let result = evaluate_assignment_expression(node, scope.clone()).unwrap();
        assert_eq!(result, RuntimeValue::Int(42));
        assert_eq!(
            scope.borrow().variables.get("x").unwrap().0,
            RuntimeValue::Int(42)
        );
    }
}

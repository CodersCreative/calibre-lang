use crate::{
    ast::NodeType,
    runtime::{
        interpreter::InterpreterErr,
        scope::{Environment, Variable},
        values::{
            FunctionType, RuntimeValue,
            helper::{Block, MatchBlock},
        },
    },
};

pub mod comparisons;
pub mod import;
pub mod loops;
pub mod matching;
pub mod structs;

impl Environment {
    pub fn evaluate_program(
        &mut self,
        scope: &u64,
        exp: NodeType,
    ) -> Result<RuntimeValue, InterpreterErr> {
        let mut last = RuntimeValue::Null;

        if let NodeType::Program(body) = exp {
            for statement in body.into_iter() {
                last = self.evaluate(scope, statement)?;
            }
        } else {
            return Err(InterpreterErr::NotImplemented(exp));
        }

        Ok(last)
    }

    pub fn evaluate_match_declaration(
        &mut self,
        scope: &u64,
        declaration: NodeType,
    ) -> Result<RuntimeValue, InterpreterErr> {
        if let NodeType::MatchDeclaration {
            parameters,
            body,
            return_type,
            is_async,
        } = declaration
        {
            let mut params = Vec::new();

            let default = if let Some(node) = parameters.3 {
                Some(self.evaluate(scope, *node)?)
            } else {
                None
            };

            params.push((parameters.0, parameters.1, parameters.2, default));

            Ok(RuntimeValue::Function {
                parameters: params,
                body: FunctionType::Match(MatchBlock(body)),
                return_type,
                is_async,
            })
        } else {
            Err(InterpreterErr::NotImplemented(declaration))
        }
    }

    pub fn evaluate_function_declaration(
        &mut self,
        scope: &u64,
        declaration: NodeType,
    ) -> Result<RuntimeValue, InterpreterErr> {
        if let NodeType::FunctionDeclaration {
            parameters,
            body,
            return_type,
            is_async,
        } = declaration
        {
            let mut params = Vec::new();

            for p in parameters.into_iter() {
                let default = if let Some(node) = p.3 {
                    Some(self.evaluate(scope, node)?)
                } else {
                    None
                };

                params.push((p.0, p.1, p.2, default));
            }

            Ok(RuntimeValue::Function {
                parameters: params,
                body: FunctionType::Regular(Block(body)),
                return_type,
                is_async,
            })
        } else {
            Err(InterpreterErr::NotImplemented(declaration))
        }
    }

    pub fn evaluate_variable_declaration(
        &mut self,
        scope: &u64,
        declaration: NodeType,
    ) -> Result<RuntimeValue, InterpreterErr> {
        if let NodeType::VariableDeclaration {
            var_type,
            identifier,
            value,
            data_type,
        } = declaration
        {
            let mut value = self.evaluate(scope, *value)?;

            if let Some(t) = data_type {
                value = value.unwrap(self, scope)?.into_type(self, scope, &t)?;
            }

            Ok(self.push_var(scope, identifier, Variable { value, var_type })?)
        } else {
            Err(InterpreterErr::NotImplemented(declaration))
        }
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

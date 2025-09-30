use calibre_common::environment::Variable;
use calibre_parser::ast::{LoopType, NodeType, RefMutability, VarType};

use crate::runtime::{
    interpreter::InterpreterErr, scope::CheckerEnvironment, values::RuntimeType
};

impl CheckerEnvironment {
    pub fn evaluate_loop_declaration(
        &mut self,
        scope: &u64,
        loop_type : LoopType,
        body : NodeType,
    ) -> Result<RuntimeType, InterpreterErr> {
            let mut result = RuntimeType::Null;

            if let LoopType::For(identifier, range) = loop_type {
                let range = self.evaluate(scope, range)?;
                if let RuntimeType::List(x) = range {
                    let new_scope = self.get_new_scope(scope, Vec::new(), Vec::new())?;
                    let _ = self.push_var(
                        &new_scope,
                        identifier.clone(),
                        Variable {
                            value: match *x {
                                Some(x) => x,
                                _ => RuntimeType::Dynamic,
                            },
                            var_type: VarType::Immutable,
                        },
                    );
                    result = self.evaluate(&new_scope, body.clone())?;
                    self.remove_scope(&new_scope);
                } else if let RuntimeType::Range =
                    range.into_type(self, scope, &RuntimeType::Range)?
                {
                    let new_scope = self.get_new_scope(
                        scope,
                        vec![(
                            identifier.clone(),
                            RuntimeType::Int,
                            RefMutability::Value,
                            None,
                        )],
                        vec![(NodeType::IntLiteral(0 as i128), None)],
                    )?;
                    result = self.evaluate(&new_scope, body.clone())?;
                    self.remove_scope(&new_scope);
                }
            } else if let LoopType::ForEach(identifier, (loop_name, mutability)) = loop_type {
                let var = self.get_var(scope, &loop_name)?.clone();
                if let RuntimeType::List(x) = &var.value {
                    let new_scope = self.get_new_scope(scope, Vec::new(), Vec::new())?;

                    let _ = self.push_var(
                        &new_scope,
                        identifier.clone(),
                        Variable {
                            value: x.clone().unwrap_or(RuntimeType::Dynamic),
                            var_type: match mutability {
                                RefMutability::MutRef | RefMutability::MutValue => {
                                    VarType::Mutable
                                }
                                _ => VarType::Immutable,
                            },
                        },
                    );

                    result = self.evaluate(&new_scope, body.clone())?;
                    self.remove_scope(&new_scope);
                }
            } else if let LoopType::While(condition) = loop_type {
                match self.evaluate(scope, condition.clone())? {
                    RuntimeType::Bool => {
                        let new_scope = self.get_new_scope(scope, Vec::new(), Vec::new())?;
                        result = self.evaluate(&new_scope, body.clone())?;
                        self.remove_scope(&new_scope);
                    }
                    RuntimeType::Range => {
                        return self.evaluate_loop_declaration(
                            scope,
                                LoopType::For(
                                    String::from("hidden_index"),
                                    NodeType::RangeDeclaration {
                                        from: Box::new(NodeType::IntLiteral(0 as i128)),
                                        to: Box::new(NodeType::IntLiteral(0 as i128)),
                                        inclusive: false,
                                    },
                                ),
                                body,
                        );
                    }
                    RuntimeType::Range => {
                        return self.evaluate_loop_declaration(
                            scope,
                                LoopType::For(
                                    String::from("hidden_index"),
                                    NodeType::IntLiteral(0 as i128),
                                ),
                                body,
                        );
                    }
                    RuntimeType::Float => {
                        return self.evaluate_loop_declaration(
                            scope,
                            LoopType::For(
                                    String::from("hidden_index"),
                                    NodeType::FloatLiteral(0 as f64),
                                ),
                                body,
                            
                        );
                    }
                    _ => {}
                }
            }

            Ok(result)
    }
}

#[cfg(test)]
mod tests {
    use std::{path::PathBuf, str::FromStr};

    use calibre_parser::ast::{LoopType, NodeType, VarType};

    use crate::runtime::{scope::Environment, values::RuntimeValue};

    fn get_new_env() -> (Environment, u64) {
        let mut env = Environment::new();
        let scope = env.new_scope_with_stdlib(None, PathBuf::from_str("./main.cl").unwrap(), None);
        (env, scope)
    }

    #[test]
    fn test_evaluate_loop_declaration_for_range() {
        let (mut env, scope) = get_new_env();

        let loop_type = Box::new(LoopType::For(
            "i".to_string(),
            NodeType::RangeDeclaration {
                from: Box::new(NodeType::IntLiteral(0)),
                to: Box::new(NodeType::IntLiteral(3)),
                inclusive: false,
            },
        ));

        let body = Box::new(NodeType::VariableDeclaration {
            var_type: VarType::Mutable,
            identifier: "x".to_string(),
            value: Box::new(NodeType::Identifier("i".to_string())),
            data_type: None,
        });

        let node = NodeType::LoopDeclaration { loop_type, body };
        let result = env
            .evaluate(&scope, node)
            .unwrap()
            .unwrap_val(&env, &scope)
            .unwrap();

        assert_eq!(result, RuntimeValue::Int(2));
    }
}

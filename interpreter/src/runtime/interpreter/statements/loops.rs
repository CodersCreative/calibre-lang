use calibre_common::environment::Variable;
use calibre_parser::ast::{LoopType, Node, NodeType, RefMutability, VarType};

use crate::runtime::{
    interpreter::InterpreterErr, scope::InterpreterEnvironment, values::{helper::StopValue, RuntimeType, RuntimeValue}
};

impl InterpreterEnvironment {
    pub fn evaluate_loop_declaration(
        &mut self,
        scope: &u64,
        loop_type : LoopType,
        body : Node,
    ) -> Result<RuntimeValue, InterpreterErr> {
            let mut result = RuntimeValue::Null;

            if let LoopType::For(identifier, range_node) = loop_type {
                let range = self.evaluate(scope, range_node.clone())?;
                if let RuntimeValue::List { data, data_type: _ } = range {
                    for d in data.into_iter() {
                        let new_scope = self.get_new_scope(scope, Vec::new(), Vec::new())?;
                        let location = self.get_location(scope, range_node.line, range_node.col);
                        let _ = self.push_var(
                            &new_scope,
                            identifier.clone(),
                            Variable {
                                value: d.clone(),
                                var_type: VarType::Immutable,
                                location
                            },
                        );
                        result = self.evaluate(&new_scope, body.clone())?.unwrap_links_val(
                            self,
                            &new_scope,
                            Some(new_scope),
                        )?;
                        self.remove_scope(&new_scope);

                        match self.stop {
                            Some(StopValue::Break) => break,
                            Some(StopValue::Return) => {
                                self.stop = Some(StopValue::Return);
                                break;
                            }
                            _ => {}
                        };
                    }
                } else if let RuntimeValue::Range(from, to) =
                    range.unwrap_val(self, scope)?
                {
                    for i in from..to {
                        let new_scope = self.get_new_scope(
                            scope,
                            vec![(
                                identifier.clone(),
                                RuntimeType::Int,
                                RefMutability::Value,
                                None,
                            )],
                            vec![(Node::new(NodeType::IntLiteral(i as i128), range_node.line, range_node.col), None)],
                        )?;
                        result = self.evaluate(&new_scope, body.clone())?.unwrap_links_val(
                            self,
                            &new_scope,
                            Some(new_scope),
                        )?;
                        self.remove_scope(&new_scope);

                        match self.stop {
                            Some(StopValue::Break) => break,
                            Some(StopValue::Return) => {
                                self.stop = Some(StopValue::Return);
                                break;
                            }
                            _ => {}
                        };
                    }
                }
            } else if let LoopType::ForEach(identifier, (loop_name, mutability)) = loop_type {
                let var = self.get_var(scope, &loop_name)?.clone();
                if let RuntimeValue::List { data, .. } = &var.value {
                    for (i, d) in data.iter().enumerate() {
                        let new_scope = self.get_new_scope(scope, Vec::new(), Vec::new())?;

                        let _ = self.push_var(
                            &new_scope,
                            identifier.clone(),
                            Variable {
                                value: RuntimeValue::Link(
                                    *scope,
                                    vec![loop_name.clone(), i.to_string()],
                                    d.into(),
                                ),
                                var_type: match mutability {
                                    RefMutability::MutRef | RefMutability::MutValue => {
                                        VarType::Mutable
                                    }
                                    _ => VarType::Immutable,
                                },
                                location : var.location.clone(),
                            },
                        );

                        result = self.evaluate(&new_scope, body.clone())?.unwrap_links_val(
                            self,
                            &new_scope,
                            Some(new_scope),
                        )?;
                        self.remove_scope(&new_scope);

                        match self.stop {
                            Some(StopValue::Break) => break,
                            Some(StopValue::Return) => {
                                self.stop = Some(StopValue::Return);
                                break;
                            }
                            _ => {}
                        };
                    }
                }
            } else if let LoopType::While(condition) = loop_type {
                match self.evaluate(scope, condition.clone())? {
                    RuntimeValue::Bool(_) => {
                        while let RuntimeValue::Bool(x) = self.evaluate(scope, condition.clone())? {
                            if !x {
                                break;
                            }
                            let new_scope = self.get_new_scope(scope, Vec::new(), Vec::new())?;
                            result = self.evaluate(&new_scope, body.clone())?.unwrap_links_val(
                                self,
                                &new_scope,
                                Some(new_scope),
                            )?;
                            self.remove_scope(&new_scope);

                            match self.stop {
                                Some(StopValue::Break) => break,
                                Some(StopValue::Return) => {
                                    self.stop = Some(StopValue::Return);
                                    break;
                                }
                                _ => {}
                            };
                        }
                    }
                    RuntimeValue::Range(from, to) => {
                        return self.evaluate_loop_declaration(
                            scope,
                                LoopType::For(
                                    String::from("hidden_index"),
                                    Node::new(NodeType::RangeDeclaration {
                                        from: Box::new(Node::new(NodeType::IntLiteral(from as i128), condition.line, condition.col)),
                                        to: Box::new(Node::new(NodeType::IntLiteral(to as i128), condition.line, condition.col)),
                                        inclusive: false,
                                    }, condition.line, condition.col),
                                ),
                                body,
                        );
                    }
                    RuntimeValue::Int(x) => {
                        return self.evaluate_loop_declaration(
                            scope,
                                LoopType::For(
                                    String::from("hidden_index"),
                                    Node::new(NodeType::IntLiteral(x as i128), condition.line, condition.col),
                                ),
                                body,
                        );
                    }
                    RuntimeValue::Float(x) => {
                        return self.evaluate_loop_declaration(
                            scope,
                            LoopType::For(
                                    String::from("hidden_index"),
                                    Node::new(NodeType::FloatLiteral(x as f64), condition.line, condition.col),
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

use calibre_common::environment::{Location, Variable};
use calibre_parser::ast::{Node, NodeType, RefMutability, VarType};

use crate::runtime::{
    interpreter::{expressions::member::MembrExprPathRes, InterpreterErr}, scope::InterpreterEnvironment, values::{RuntimeType, RuntimeValue}
};

impl InterpreterEnvironment {
    pub fn get_new_scope_with_values(
        &mut self,
        scope: &u64,
        arguments: Vec<(String, RuntimeValue, RefMutability, Option<Location>)>,
    ) -> Result<u64, InterpreterErr> {
        let new_scope = self.new_scope_from_parent_shallow(scope.clone());
        for (k, v, m, location) in arguments.into_iter() {
            // if m == RefMutability::MutRef || m == RefMutability::Ref {
            //     todo!()
            // } else {
            let _ = self.push_var(
                &new_scope,
                k.to_string(),
                Variable {
                    value: v.clone(),
                    var_type: match m {
                        RefMutability::MutRef | RefMutability::MutValue => VarType::Mutable,
                        _ => VarType::Immutable,
                    },
                    location,
                },
            )?;
            // }
        }

        Ok(new_scope)
    }

    pub fn get_new_scope(
        &mut self,
        scope: &u64,
        parameters: Vec<(String, RuntimeType, RefMutability, Option<RuntimeValue>)>,
        arguments: Vec<(Node, Option<Node>)>,
    ) -> Result<u64, InterpreterErr> {
        let new_scope = self.new_scope_from_parent_shallow(scope.clone());

        for (i, (k, v, m, d)) in parameters.iter().enumerate() {
            if m == &RefMutability::MutRef || m == &RefMutability::Ref {
                if let Some(arg) = &arguments.get(i) {
                    if let None = arg.1 {
                        let location = self.get_location(scope, arg.0.line, arg.0.col);
                        if let NodeType::Identifier(x) = &arg.0.node_type {
                            let (typ, _) = {
                                let var = self.get_var(scope, x)?;

                                match var.var_type {
                                    VarType::Mutable => {}
                                    _ if m == &RefMutability::MutRef => {
                                        return Err(InterpreterErr::MutRefNonMut(
                                            var.value.clone(),
                                        ));
                                    }
                                    _ => {}
                                }

                                if !var.value.is_type(self, &scope, &v) {
                                    return Err(InterpreterErr::UnexpectedType(var.value.clone()));
                                }

                                ((&var.value).into(), var.var_type.clone())
                            };

                            let _ = self.push_var(
                                &new_scope,
                                k.to_string(),
                                Variable {
                                    value: RuntimeValue::Link(
                                        scope.clone(),
                                        vec![x.to_string()],
                                        typ,
                                    ),
                                    var_type: match m {
                                        RefMutability::MutRef | RefMutability::MutValue => {
                                            VarType::Mutable
                                        }
                                        _ => VarType::Immutable,
                                    },
                                    location
                                },
                            )?;
                            continue;
                        } else if let NodeType::MemberExpression { path } = &arg.0.node_type {
                            let path = match self.get_member_expression_path(scope, path.clone())? {
                                MembrExprPathRes::Path(x) => x,
                                _ => {
                                    return Err(InterpreterErr::RefNonVar(arguments[0].0.node_type.clone()));
                                }
                            };

                            let typ = {
                                let var = self.get_link_path(scope, &path)?.unwrap(self, scope)?;

                                if !var.is_type(self, &scope, &v) {
                                    return Err(InterpreterErr::UnexpectedType(var.clone()));
                                }

                                (var).into()
                            };

                            let _ = self.push_var(
                                &new_scope,
                                k.to_string(),
                                Variable {
                                    value: RuntimeValue::Link(scope.clone(), path, typ),
                                    var_type: match m {
                                        RefMutability::MutRef | RefMutability::MutValue => {
                                            VarType::Mutable
                                        }
                                        _ => VarType::Immutable,
                                    },
                                    location
                                },
                            )?;
                            continue;
                        } else {
                            return Err(InterpreterErr::RefNonVar(arguments[0].0.node_type.clone()));
                        }
                    }
                }
            }
            if let Some(arg) = arguments.get(i) {
                if let None = arg.1 {
                    let arg = self
                        .evaluate(&new_scope, arg.0.clone())?;
                    self.push_var(
                        &new_scope,
                        k.to_string(),
                        Variable {
                            value: arg,
                            var_type: match m {
                                RefMutability::MutRef | RefMutability::MutValue => VarType::Mutable,
                                _ => VarType::Immutable,
                            },
                            location: self.current_location.clone(),
                        },
                    )?;
                    continue;
                }
            }
            if let Some(d) = arguments.iter().find(|x| {
                if let NodeType::Identifier(key) = &x.0.node_type {
                    key == k && x.1.is_some()
                } else {
                    false
                }
            }) {
                let value = self.evaluate(scope, d.1.clone().unwrap())?;
                self.push_var(
                    &new_scope,
                    k.to_string(),
                    Variable {
                        value,
                        var_type: match m {
                            RefMutability::MutRef | RefMutability::MutValue => VarType::Mutable,
                            _ => VarType::Immutable,
                        },
                        location: self.current_location.clone(),
                    },
                )?;

                continue;
            }

            if let Some(d) = d {
                self.push_var(
                    &new_scope,
                    k.to_string(),
                    Variable {
                        value: d.clone(),
                        var_type: match m {
                            RefMutability::MutRef | RefMutability::MutValue => VarType::Mutable,
                            _ => VarType::Immutable,
                        },
                        location : self.current_location.clone(),
                    },
                )?;

                continue;
            }

            return Err(InterpreterErr::RefNonVar(arguments[0].0.node_type.clone()));
        }

        Ok(new_scope)
    }

    pub fn evaluate_scope(
        &mut self,
        scope: &u64,
        body : Vec<Node>,
        is_temp : bool,
    ) -> Result<RuntimeValue, InterpreterErr> {
            let new_scope = if is_temp {
                self.get_new_scope(scope, Vec::new(), Vec::new())?
            } else {
                scope.clone()
            };

            let mut result: RuntimeValue = RuntimeValue::Null;
            for statement in body.into_iter() {
                match self.stop {
                    Some(_) if is_temp => return Ok(result),
                    _ => result = self.evaluate(&new_scope, statement)?,
                }
            }

            if is_temp {
                result = result.unwrap_links_val(self, &new_scope, Some(new_scope))?;
                self.remove_scope(&new_scope);
            }

            Ok(result)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::values::{RuntimeType, RuntimeValue};
    use calibre_parser::ast::{NodeType, RefMutability};
    use std::path::PathBuf;
    use std::str::FromStr;

    fn get_new_env() -> (Environment, u64) {
        let mut env = Environment::new();
        let scope = env.new_scope_with_stdlib(None, PathBuf::from_str("./main.cl").unwrap(), None);
        (env, scope)
    }

    #[test]
    fn test_get_new_scope_basic_value() {
        let (mut env, scope) = get_new_env();

        let params = vec![(
            "x".to_string(),
            RuntimeType::Int,
            RefMutability::Value,
            None,
        )];

        let args = vec![(NodeType::IntLiteral(5), None)];
        let scope = env.get_new_scope(&scope, params, args).unwrap();
        let val = env.get_var(&scope, "x").unwrap().value.clone();
        assert_eq!(val, RuntimeValue::Int(5));
    }

    #[test]
    fn test_get_new_scope_default_value() {
        let (mut env, scope) = get_new_env();

        let params = vec![(
            "x".to_string(),
            RuntimeType::Int,
            RefMutability::Value,
            Some(RuntimeValue::Int(10)),
        )];

        let args = vec![];
        let scope = env.get_new_scope(&scope, params, args).unwrap();
        let val = env.get_var(&scope, "x").unwrap().value.clone();
        assert_eq!(val, RuntimeValue::Int(10));
    }

    #[test]
    fn test_get_new_scope_named_argument() {
        let (mut env, scope) = get_new_env();

        let params = vec![(
            "x".to_string(),
            RuntimeType::UInt,
            RefMutability::Value,
            Some(RuntimeValue::UInt(1)),
        )];
        let args = vec![(
            NodeType::Identifier("x".to_string()),
            Some(NodeType::IntLiteral(99)),
        )];

        let scope = env.get_new_scope(&scope, params, args).unwrap();
        let val = env.get_var(&scope, "x").unwrap().value.clone();
        assert_eq!(val, RuntimeValue::UInt(99));
    }

    #[test]
    fn test_get_new_scope_mut_ref_error() {
        let (mut env, scope) = get_new_env();

        let params = vec![(
            "a".to_string(),
            RuntimeType::UInt,
            RefMutability::MutRef,
            None,
        )];

        let args = vec![(NodeType::IntLiteral(5), None)];
        let result = env.get_new_scope(&scope, params, args);
        assert!(result.is_err());
    }

    #[test]
    fn test_evaluate_scope_return() {
        let (mut env, scope) = get_new_env();
        let node = NodeType::ScopeDeclaration {
            body: vec![NodeType::Return {
                value: Box::new(NodeType::IntLiteral(42)),
            }],
            is_temp : true,
        };
        let result = env.evaluate(&scope, node).unwrap();
        assert_eq!(result, RuntimeValue::UInt(42));
    }

    #[test]
    fn test_evaluate_scope_no_return() {
        let (mut env, scope) = get_new_env();
        let node = NodeType::ScopeDeclaration {
            body: vec![NodeType::IntLiteral(7)],
            is_temp : true,
        };
        let result = env.evaluate(&scope, node).unwrap();
        assert_eq!(result, RuntimeValue::UInt(7));
    }
}

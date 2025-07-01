use std::{cell::RefCell, rc::Rc};

use crate::{ast::{NodeType, RefMutability}, runtime::{interpreter::{evaluate, expressions::{call::evaluate_function, scope::{evaluate_scope, get_new_scope_with_values}}, InterpreterErr}, scope::{objects::get_object, Object, Scope, ScopeErr}, values::{helper::{ObjectType, StopValue}, RuntimeType, RuntimeValue}}};


pub fn evaluate_match_statement(
    declaration: NodeType,
    scope: Rc<RefCell<Scope>>,
) -> Result<RuntimeValue, InterpreterErr> {
    if let NodeType::MatchDeclaration { value, patterns } = declaration
    {
        let value = evaluate(*value, scope.clone())?;
        
        for (pattern, conditionals, body) in patterns{
            match evaluate(pattern.clone(), scope.clone()){
                Ok(x) if x == value && handle_conditionals(scope.clone(), conditionals.clone())? => {
                    return evaluate_scope(NodeType::ScopeDeclaration { body }, scope);
                },
                Err(e) => {
                    match pattern{
                        NodeType::Identifier(x) if x.trim() == "_" => {
                            return evaluate_scope(NodeType::ScopeDeclaration { body }, scope);
                        }
                        NodeType::EnumExpression { identifier, data , value: enm_value } => {
                            if let RuntimeValue::Enum(iden, val, dat) = value.clone() {
                                let Object::Enum(enm) = get_object(scope.clone(), &iden)? else {continue};
                                let Some(index) = enm.iter().position(|x| x.0 == enm_value) else {continue};

                                if index != val {
                                    continue;
                                }

                                let Some(data) = data else {
                                    if !handle_conditionals(scope.clone(), conditionals.clone())? {continue;}
                                    return evaluate_scope(NodeType::ScopeDeclaration { body }, scope);
                                };
                                
                                let Some(dat) = dat else {
                                    if !handle_conditionals(scope.clone(), conditionals.clone())? {continue;}
                                    return evaluate_scope(NodeType::ScopeDeclaration { body }, scope);
                                };

                                if identifier != iden{continue};
                                let mut new_scope = None;
                                match data{
                                    ObjectType::Map(map) => {
                                        if let ObjectType::Map(m) = dat {
                                            new_scope = Some(get_new_scope_with_values(scope.clone(), m.into_iter().map(|(k, v)| {
                                                let Some(NodeType::Identifier(k)) =  map.get(&k).unwrap() else {panic!()};
                                                (k.to_string(), v, RefMutability::MutValue)
                                            }).collect())?);
                                        }
                                    },
                                    ObjectType::Tuple(list) => {
                                        if let ObjectType::Tuple(lst) = dat {
                                            let list : Vec<String>= list.into_iter().map(|x| {
                                                let NodeType::Identifier(y) = x.unwrap() else {panic!()};
                                                y
                                            }).collect();
                                            new_scope = Some(get_new_scope_with_values(scope.clone(), lst.into_iter().enumerate().map(|(i, v)| (list[i].clone(), v, RefMutability::MutValue)).collect())?);
                                        }else if let ObjectType::Map(map) = dat {
                                            let list : Vec<String>= list.into_iter().map(|x| {
                                                let NodeType::Identifier(y) = x.unwrap() else {panic!()};
                                                y
                                            }).collect();
                                            
                                            new_scope = Some(get_new_scope_with_values(scope.clone(), map.into_iter().filter(|x| list.contains(&x.0)).map(|(k, v)| {
                                                (k, v, RefMutability::MutValue)
                                            }).collect())?);

                                        } 
                                    },
                                }
                                if let Some(new_scope) = new_scope{
                                    if !handle_conditionals(new_scope.clone(), conditionals.clone())? {continue;}
                                    let mut result: RuntimeValue = RuntimeValue::Null;
                                    for statement in &body {
                                        if let Some(StopValue::Return) = new_scope.borrow().stop {
                                            break;
                                        } else if let NodeType::Return { value } = statement {
                                            result = evaluate(*value.clone(), new_scope.clone())?;
                                            break;
                                        } else {
                                            result = evaluate(statement.clone(), new_scope.clone())?;
                                        }
                                    }

                                    return Ok(result);
                                }
                            }
                        },
                        _ => {},
                    }
                }
                _ => {}
            }
        }
        Ok(RuntimeValue::Null)
    } else {
        Err(InterpreterErr::NotImplemented(declaration))
    }
}


pub fn handle_conditionals(scope: Rc<RefCell<Scope>>, conditionals : Vec<NodeType>) -> Result<bool, InterpreterErr> {
    let mut result = true;

    for condition in conditionals.iter() {
        if let RuntimeValue::Bool(value) = evaluate(condition.clone(), scope.clone())?{
            result = result && value;
        }
    }

    Ok(result)
}

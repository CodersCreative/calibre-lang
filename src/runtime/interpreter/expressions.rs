use core::panic;
use std::{cell::RefCell, collections::HashMap, mem::discriminant, rc::Rc};

use rand::random_range;

use crate::{
    ast::{NodeType, RefMutability},
    runtime::{
        interpreter::{InterpreterErr, evaluate, statements::evaluate_if_statement},
        scope::{
            Scope, ScopeErr,
            structs::{get_struct_function, resolve_struct_function},
            variables::{get_var, resolve_var},
        },
        values::{RuntimeType, RuntimeValue, ValueErr, helper::Map},
    },
};

pub fn evaluate_identifier(
    identifier: &str,
    scope: Rc<RefCell<Scope>>,
) -> Result<RuntimeValue, InterpreterErr> {
    Ok(get_var(scope, identifier).clone()?)
}

pub fn assign_member_expression(
    member: NodeType,
    value: RuntimeValue,
    scope: Rc<RefCell<Scope>>,
) -> Result<(), InterpreterErr> {
    if let NodeType::MemberExpression {
        object: old_object,
        mut property,
        is_computed,
    } = member
    {
        let mut object = evaluate(*old_object.clone(), scope.clone())?;
        let mut path = Vec::new();

        if let RuntimeValue::Struct(map, t) = object.clone() {
            let mut latest = (map.clone(), t.clone());

            while let NodeType::MemberExpression {
                property: prop,
                object,
                ..
            } = *property
            {
                if let NodeType::Identifier(name) = *object {
                    if let Some(RuntimeValue::Struct(map, t)) = latest.0.0.get(&name) {
                        latest = (map.clone(), t.clone());
                        path.push(name);
                    }
                }
                property = prop;
            }

            if let NodeType::Identifier(prop) = *property {
                let mut main = get_nested_mut(&mut object, &path);

                if let Some(RuntimeValue::Struct(m, _)) = &mut main {
                    if let Some(x) = m.0.get_mut(&prop) {
                        *x = value;
                    } else {
                        return Err(InterpreterErr::Value(ValueErr::Scope(ScopeErr::Variable(
                            prop,
                        ))));
                    }
                }
                let main = object;

                if let NodeType::Identifier(x) = *old_object {
                    let _ = scope.borrow_mut().assign_var(&x, main);

                    return Ok(());
                }
            } else {
                return Err(InterpreterErr::NotImplemented(*property));
            }

            panic!()
        } else {
            Err(InterpreterErr::UnexpectedType(object))
        }
    } else {
        Err(InterpreterErr::NotImplemented(member))
    }
}

pub fn evaluate_member_expression(
    exp: NodeType,
    scope: Rc<RefCell<Scope>>,
) -> Result<RuntimeValue, InterpreterErr> {
    if let NodeType::MemberExpression {
        object: old_object,
        mut property,
        is_computed,
    } = exp
    {
        let mut object = match evaluate(*old_object.clone(), scope.clone()) {
            Ok(x) => x,
            Err(x) => {
                if let InterpreterErr::Value(ValueErr::Scope(ScopeErr::Variable(struct_name))) = x {
                    if let NodeType::CallExpression(caller, args) = *property {
                        if let NodeType::Identifier(caller) = *caller {
                            if let Ok(struct_fn) =
                                get_struct_function(scope.clone(), &struct_name, &caller)
                            {
                                return Ok(evaluate_function(scope.clone(), struct_fn.0, *args)?);
                            }
                        }
                    }

                    return Err(InterpreterErr::Value(ValueErr::Scope(ScopeErr::Struct(
                        struct_name,
                    ))));
                } else {
                    return Err(x);
                }
            }
        };

        let mut path = Vec::new();

        if let RuntimeValue::Struct(map, t) = object.clone() {
            let mut latest = (map.clone(), t.clone());

            while let NodeType::MemberExpression {
                property: prop,
                object,
                ..
            } = *property
            {
                if let NodeType::Identifier(name) = *object {
                    if let Some(RuntimeValue::Struct(map, t)) = latest.0.0.get(&name) {
                        latest = (map.clone(), t.clone());
                        path.push(name);
                    }
                }
                property = prop;
            }

            if let NodeType::Identifier(prop) = *property {
                if let Some(x) = latest.0.0.get(&prop) {
                    return Ok(x.clone());
                } else {
                    return Err(InterpreterErr::Value(ValueErr::Scope(ScopeErr::Variable(
                        prop,
                    ))));
                }
            } else if let NodeType::CallExpression(caller, mut args) = *property {
                let mut main = get_nested_mut(&mut object, &path).unwrap();
                if let NodeType::Identifier(caller) = *caller {
                    if let Ok(val) = get_struct_function(scope.clone(), &latest.1.unwrap(), &caller)
                    {
                        let mut name = None;

                        if let RuntimeValue::Function { parameters, .. } = val.0.clone() {
                            let mut arguments = Vec::new();
                            if parameters.len() > 0 {
                                if parameters[0].0 == "self" {
                                    name = Some(
                                        [random_range(0..1000).to_string(), path.join("")].join(""),
                                    );
                                    let _ = scope.borrow_mut().push_var(
                                        name.clone().unwrap(),
                                        &main.clone(),
                                        true,
                                    )?;
                                    arguments.push(NodeType::Identifier(name.clone().unwrap()));
                                }
                            }

                            arguments.append(&mut args);

                            let value = evaluate_function(scope.clone(), val.0, arguments)?;

                            if let Some(name) = name {
                                if let RuntimeValue::Struct(m, _) = &mut main {
                                    if let RuntimeValue::Struct(x, _) =
                                        get_var(scope.clone(), &name)?
                                    {
                                        *m = x;
                                    }
                                }

                                if let NodeType::Identifier(x) = *old_object {
                                    let _ = scope.borrow_mut().assign_var(&x, object)?;
                                }

                                scope.borrow_mut().variables.remove(&name);
                                scope.borrow_mut().constants.remove(&name);
                            }

                            return Ok(value);
                        }
                    }
                }
            } else {
                return Err(InterpreterErr::NotImplemented(*property));
            }
            panic!("2. {:?}", old_object);
        } else {
            Err(InterpreterErr::UnexpectedType(object))
        }
    } else {
        Err(InterpreterErr::NotImplemented(exp))
    }
}

fn get_nested_mut<'a>(
    root: &'a mut RuntimeValue,
    path: &Vec<String>,
) -> Option<&'a mut RuntimeValue> {
    let mut current_val = root;
    for key in path {
        if let RuntimeValue::Struct(obj, _) = current_val {
            current_val = obj.0.get_mut(key)?;
        } else {
            return None;
        }
    }
    Some(current_val)
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

                Ok(RuntimeValue::Range(from as i8, to as i8))
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

pub fn evaluate_struct_expression(
    obj: NodeType,
    scope: Rc<RefCell<Scope>>,
) -> Result<RuntimeValue, InterpreterErr> {
    let mut properties = HashMap::new();

    if let NodeType::StructLiteral(props) = obj {
        for (k, v) in props {
            let value = if let Some(value) = v {
                evaluate(value, scope.clone())?
            } else {
                get_var(scope.clone(), &k)?
            };

            properties.insert(k, value);
        }
    }

    Ok(RuntimeValue::Struct(Map(properties), None))
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

pub fn get_new_scope(
    scope: Rc<RefCell<Scope>>,
    parameters: Vec<(String, RuntimeType, RefMutability)>,
    arguments: Vec<NodeType>,
) -> Result<Rc<RefCell<Scope>>, InterpreterErr> {
    let new_scope = Rc::new(RefCell::new(Scope::new(Some(scope.clone()))));

    for (i, (k, v, m)) in parameters.iter().enumerate() {
        match m {
            RefMutability::MutRef | RefMutability::Ref => {
                if let NodeType::Identifier(x) = &arguments[i] {
                    let (env, name) = resolve_var(new_scope.clone(), x)?;

                    if m == &RefMutability::MutRef && env.borrow().constants.contains_key(&name) {
                        return Err(InterpreterErr::MutRefNonMut(
                            env.borrow().constants.get(&name).unwrap().clone(),
                        ));
                    }
                    let var = get_var(env, &name)?;
                    let x = name.clone();
                    if var.is_type(scope.clone(), v.clone()) {
                        new_scope.borrow_mut().alias.insert(k.to_string(), x);
                        let _ = new_scope.borrow_mut().push_var(
                            k.to_string(),
                            &RuntimeValue::Null,
                            match m {
                                RefMutability::MutRef | RefMutability::MutValue => true,
                                _ => false,
                            },
                        )?;
                    } else {
                        return Err(InterpreterErr::UnexpectedType(var));
                    }
                } else {
                    return Err(InterpreterErr::RefNonVar(arguments[0].clone()));
                }
            }
            _ => {
                let arg = evaluate(arguments[i].clone(), new_scope.clone())?
                    .into_type(new_scope.clone(), v.clone())?;
                new_scope.borrow_mut().push_var(
                    k.to_string(),
                    &arg,
                    match m {
                        RefMutability::MutRef | RefMutability::MutValue => true,
                        _ => false,
                    },
                )?;
            }
        }
    }

    Ok(new_scope)
}

pub fn evaluate_function(
    scope: Rc<RefCell<Scope>>,
    func: RuntimeValue,
    arguments: Vec<NodeType>,
) -> Result<RuntimeValue, InterpreterErr> {
    if let RuntimeValue::Function {
        identifier,
        parameters,
        body,
        return_type,
        is_async,
    } = func
    {
        let new_scope = get_new_scope(scope, parameters, arguments)?;

        let mut result: RuntimeValue = RuntimeValue::Null;
        for statement in &body.0 {
            if let NodeType::IfStatement { .. } = statement {
                let value = evaluate_if_statement(statement.clone(), new_scope.clone())?;
                result = value.0;

                if value.1 {
                    return Ok(result);
                }
            } else if let NodeType::Return { value } = statement {
                return evaluate(*value.clone(), new_scope);
            } else {
                result = evaluate(statement.clone(), new_scope.clone())?;
            }
        }

        if let Some(t) = return_type {
            return Ok(result.into_type(new_scope, t.clone())?);
        } else {
            return Ok(RuntimeValue::Null);
        }
    } else {
        panic!()
    }
}

pub fn evaluate_call_expression(
    exp: NodeType,
    scope: Rc<RefCell<Scope>>,
) -> Result<RuntimeValue, InterpreterErr> {
    if let NodeType::CallExpression(caller, arguments) = exp {
        let func = evaluate(*caller.clone(), scope.clone())?;

        match func {
            RuntimeValue::Function { .. } => {
                return evaluate_function(scope, func, *arguments);
            }
            RuntimeValue::List { data, data_type } if arguments.len() == 1 => {
                match evaluate(arguments[0].clone(), scope)? {
                    RuntimeValue::Integer(i) if arguments.len() == 1 => {
                        return Ok(data
                            .get(i as usize)
                            .expect("Tried to get index that is larger than list size")
                            .clone());
                    }
                    _ => return Err(InterpreterErr::IndexNonList(arguments[0].clone())),
                }
            }
            RuntimeValue::NativeFunction(_) => {
                let mut evaluated_arguments = Vec::new();

                for arg in arguments.iter() {
                    evaluated_arguments.push(evaluate(arg.clone(), scope.clone())?);
                }

                return Ok(func.call_native(evaluated_arguments, scope));
            }
            _ => {}
        }

        if let NodeType::Identifier(caller) = *caller.clone() {
            if let Ok(scope_b) = resolve_var(scope, &caller) {
                if scope_b.0.borrow().variables.contains_key(&scope_b.1) {
                    if arguments.len() <= 0 {
                        return Ok(get_var(scope_b.0, &scope_b.1)?);
                    } else if arguments.len() == 1 {
                        let _ = scope_b.0.borrow_mut().assign_var(
                            &caller,
                            evaluate(arguments[0].clone(), scope_b.0.clone())?,
                        )?;
                        return Ok(RuntimeValue::Null);
                    } else {
                        return Err(InterpreterErr::SetterArgs(arguments));
                    }
                } else if let Some(var) = scope_b.0.borrow().constants.get(&scope_b.1) {
                    match var {
                        NativeFunctions => {}
                        _ => {
                            if arguments.len() <= 0 {
                                return Ok(get_var(scope_b.0.clone(), &scope_b.1)?);
                            } else {
                                return Err(InterpreterErr::Value(ValueErr::Scope(
                                    ScopeErr::AssignConstant(scope_b.1),
                                )));
                            }
                        }
                    }
                }
            }
        }
        panic!("Cannot call non-variable or function value, {:?}", func);
    } else {
        Err(InterpreterErr::NotImplemented(exp))
    }
}

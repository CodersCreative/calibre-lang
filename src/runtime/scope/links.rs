use crate::{
    ast::NodeType,
    runtime::{
        interpreter::InterpreterErr,
        scope::{Environment, ScopeErr},
        values::{RuntimeValue, helper::ObjectType},
    },
};
use core::panic;

pub fn progress<'a>(
    mut current: &'a RuntimeValue,
    key: &str,
) -> Result<&'a RuntimeValue, InterpreterErr> {
    match current {
        RuntimeValue::Struct(_, _, ObjectType::Map(map)) => current = map.get(key).unwrap(),
        RuntimeValue::Enum(_, _, _, Some(ObjectType::Map(map))) => current = map.get(key).unwrap(),
        RuntimeValue::Result(Err(x), _) if key == "Err" => {
            current = x;
        }
        RuntimeValue::Result(Ok(x), _) if key == "Ok" => {
            current = x;
        }
        RuntimeValue::Option(Some(x), _) if key == "Some" => {
            current = x;
        }
        RuntimeValue::Struct(_, _, ObjectType::Tuple(tuple)) => {
            let idx = key
                .parse::<usize>()
                .map_err(|_| InterpreterErr::IndexNonList(NodeType::Identifier(key.to_string())))?;
            current = tuple.get(idx).unwrap()
        }
        RuntimeValue::Enum(_, _, _, Some(ObjectType::Tuple(tuple))) => {
            let idx = key
                .parse::<usize>()
                .map_err(|_| InterpreterErr::IndexNonList(NodeType::Identifier(key.to_string())))?;
            current = tuple.get(idx).unwrap()
        }
        RuntimeValue::Tuple(tuple) => {
            let idx = key
                .parse::<usize>()
                .map_err(|_| InterpreterErr::IndexNonList(NodeType::Identifier(key.to_string())))?;
            current = tuple.get(idx).unwrap()
        }
        RuntimeValue::List { data, .. } => {
            let idx = key
                .parse::<usize>()
                .map_err(|_| InterpreterErr::IndexNonList(NodeType::Identifier(key.to_string())))?;
            current = data.get(idx).unwrap()
        }
        _ => {
            panic!()
        }
    }

    Ok(current)
}
pub fn progress_mut<'a>(
    mut current: &'a mut RuntimeValue,
    key: &str,
) -> Result<&'a mut RuntimeValue, InterpreterErr> {
    match current {
        RuntimeValue::Struct(_, _, ObjectType::Map(map)) => current = map.get_mut(key).unwrap(),
        RuntimeValue::Enum(_, _, _, Some(ObjectType::Map(map))) => {
            current = map.get_mut(key).unwrap()
        }
        RuntimeValue::Result(Err(x), _) if key == "Err" => {
            current = x;
        }
        RuntimeValue::Result(Ok(x), _) if key == "Ok" => {
            current = x;
        }
        RuntimeValue::Option(Some(x), _) if key == "Some" => {
            current = x;
        }
        RuntimeValue::Struct(_, _, ObjectType::Tuple(tuple)) => {
            let idx = key
                .parse::<usize>()
                .map_err(|_| InterpreterErr::IndexNonList(NodeType::Identifier(key.to_string())))?;
            current = tuple.get_mut(idx).unwrap()
        }
        RuntimeValue::Enum(_, _, _, Some(ObjectType::Tuple(tuple))) => {
            let idx = key
                .parse::<usize>()
                .map_err(|_| InterpreterErr::IndexNonList(NodeType::Identifier(key.to_string())))?;
            current = tuple.get_mut(idx).unwrap()
        }
        RuntimeValue::Tuple(tuple) => {
            let idx = key
                .parse::<usize>()
                .map_err(|_| InterpreterErr::IndexNonList(NodeType::Identifier(key.to_string())))?;
            current = tuple.get_mut(idx).unwrap()
        }
        RuntimeValue::List { data, .. } => {
            let idx = key
                .parse::<usize>()
                .map_err(|_| InterpreterErr::IndexNonList(NodeType::Identifier(key.to_string())))?;
            current = data.get_mut(idx).unwrap()
        }
        _ => {
            return Err(InterpreterErr::Value(
                crate::runtime::values::ValueErr::Scope(ScopeErr::Variable(key.to_string())),
            ));
        }
    }

    Ok(current)
}
impl Environment {
    pub fn get_link_path(
        &self,
        scope: &u64,
        path: &[String],
    ) -> Result<&RuntimeValue, InterpreterErr> {
        if let Some(vars) = self.variables.get(scope) {
            if let Some(var) = vars.get(&path[0]) {
                if let RuntimeValue::Link(scope, p, _) = var.value.clone() {
                    return self.get_link_path(&scope, &[p.as_slice(), &path[1..]].concat());
                }
                let mut var = &var.value;
                for key in path.iter().skip(1) {
                    var = progress(var, key)?;
                }
                return Ok(var);
            }
        }
        return Err(ScopeErr::Variable(path[0].to_string()).into());
    }

    pub fn get_link(&self, link: &RuntimeValue) -> Result<&RuntimeValue, InterpreterErr> {
        if let RuntimeValue::Link(scope, path, _) = link {
            self.get_link_path(scope, &path)
        } else {
            panic!()
        }
    }

    pub fn update_link<F>(&mut self, link: &RuntimeValue, f: F) -> Result<(), InterpreterErr>
    where
        F: FnMut(&mut RuntimeValue) -> Result<(), InterpreterErr>,
    {
        if let RuntimeValue::Link(scope, path, _) = link {
            self.update_link_path(scope, &path, f)
        } else {
            panic!()
        }
    }

    pub fn update_link_path<F>(
        &mut self,
        scope: &u64,
        path: &[String],
        mut f: F,
    ) -> Result<(), InterpreterErr>
    where
        F: FnMut(&mut RuntimeValue) -> Result<(), InterpreterErr>,
    {
        if let Some(vars) = self.variables.get_mut(scope) {
            if let Some(var) = vars.get_mut(&path[0]) {
                if let RuntimeValue::Link(scope, p, _) = var.value.clone() {
                    return self.update_link_path(&scope, &[p.as_slice(), &path[1..]].concat(), f);
                }

                let mut var = Ok(&mut var.value);

                for key in path.iter().skip(1) {
                    if let Ok(x) = var {
                        var = progress_mut(x, key)
                    } else {
                        break;
                    }
                }

                if let Ok(x) = var {
                    return f(x);
                }
            }
        }

        if let RuntimeValue::Link(scope, path, _) = &self.get_var(scope, &path[0])?.value.clone() {
            return self.update_link_path(&scope, &path, f);
        }

        return Err(ScopeErr::Variable(path[0].to_string()).into());
    }

    pub fn get_link_parent(&self, link: &RuntimeValue) -> Result<&RuntimeValue, ScopeErr> {
        if let RuntimeValue::Link(scope, path, _) = link {
            Ok(self.get_link_path(scope, &[path[0].clone()]).unwrap())
        } else {
            panic!()
        }
    }
}

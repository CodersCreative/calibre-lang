use calibre_common::errors::{ScopeErr, ValueErr};
use calibre_parser::ast::{NodeType, ObjectType};

use crate::runtime::{
    interpreter::InterpreterErr, scope::CheckerEnvironment, values::RuntimeType
};

pub fn progress(
    current: RuntimeType,
    key: &str,
) -> Result<RuntimeType, InterpreterErr> {
    Ok(match current.clone() {
        RuntimeType::Struct(_, _, ObjectType::Map(map)) => {
            match map.get(key) {
                Some(x) => x.clone(),
                None => return Err(InterpreterErr::Value(ValueErr::ProgressErr)),
            }
        }
        RuntimeType::Enum(_, _, Some(ObjectType::Map(map))) => {
            match map.get(key) {
                Some(x) => x.clone(),
                None => return Err(InterpreterErr::Value(ValueErr::ProgressErr)),
            }
        }
        RuntimeType::Result(x, _) if key == "Err" => {
            *x
        }
        RuntimeType::Result(_, x) if key == "Ok" => {
            *x
        }
        RuntimeType::Option(x) if key == "Some" => {
            *x
        }
        RuntimeType::Struct(_, _, ObjectType::Tuple(tuple)) => {
            let idx = key
                .parse::<usize>()
                .map_err(|_| InterpreterErr::IndexNonList(NodeType::Identifier(key.to_string())))?;
            match tuple.get(idx) {
                Some(x) => x.clone(),
                None => return Err(InterpreterErr::Value(ValueErr::ProgressErr)),
            }
        }
        RuntimeType::Enum(_, _, Some(ObjectType::Tuple(tuple))) => {
            let idx = key
                .parse::<usize>()
                .map_err(|_| InterpreterErr::IndexNonList(NodeType::Identifier(key.to_string())))?;
            match tuple.get(idx) {
                Some(x) => x.clone(),
                None => return Err(InterpreterErr::Value(ValueErr::ProgressErr)),
            }
        }
        RuntimeType::Tuple(tuple) => {
            let idx = key
                .parse::<usize>()
                .map_err(|_| InterpreterErr::IndexNonList(NodeType::Identifier(key.to_string())))?;
            match tuple.get(idx) {
                Some(x) => x.clone(),
                None => return Err(InterpreterErr::Value(ValueErr::ProgressErr)),
            }
        }
        RuntimeType::List(data) => {
            match *data {
                Some(x) => x.clone(),
                _ => RuntimeType::Dynamic
            }
        }
        _ => return Err(InterpreterErr::Value(ValueErr::ProgressErr)),
    })
}
impl CheckerEnvironment {
    pub fn get_link_path(
        &self,
        scope: &u64,
        path: &[String],
    ) -> Result<RuntimeType, InterpreterErr> {
        if let Some(vars) = self.variables.get(scope) {
            if let Some(var) = vars.get(&path[0]) {
                let mut var = var.value.clone();
                for key in path.iter().skip(1) {
                    var = progress(var, key)?;
                }
                return Ok(var);
            }
        }

        if let Some(scope) = self.scopes.get(scope).unwrap().parent {
            return self.get_link_path(&scope, path);
        }

        return Err(ScopeErr::Variable(path[0].to_string()).into());
    }
}

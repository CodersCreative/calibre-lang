pub mod children;
pub mod links;
pub mod objects;
pub mod variables;

use std::{
    cell::RefCell,
    collections::HashMap,
    f32::{self, consts::PI},
    fs, i64,
    path::PathBuf,
    rc::Rc,
};

use thiserror::Error;

use crate::{
    parser,
    runtime::{
        interpreter::evaluate,
        scope::children::get_next_scope,
        values::{
            RuntimeValue,
            helper::{ObjectType, StopValue, VarType},
            native::NativeFunctions,
        },
    },
    utils::get_path,
};

// static mut scopes: HashMap<&str, Rc<RefCell<Scope>>> = HashMap::new();

use super::values::RuntimeType;

#[derive(Error, Debug, Clone)]
pub enum ScopeErr {
    #[error("Unable to resolve variable : {0}.")]
    Variable(String),
    #[error("Unable to assign immutable variable : {0}.")]
    AssignConstant(String),
    #[error("Unable to shadow immutable variable : {0}.")]
    ShadowConstant(String),
    #[error("Variable types dont match : {0:?} and {1:?}.")]
    TypeMismatch(RuntimeValue, RuntimeValue),
    #[error("Unable to resolve object : {0}.")]
    Object(String),
    #[error("Unable to resolve static function : {0}.")]
    Function(String),
    #[error("Unable to resolve scope : {0}.")]
    Scope(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    Enum(Vec<(String, Option<ObjectType<RuntimeType>>)>),
    Struct(ObjectType<RuntimeType>),
    NewType(RuntimeType),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Scope {
    pub parent: Option<Rc<RefCell<Self>>>,
    pub children: HashMap<String, Rc<RefCell<Self>>>,
    pub variables: HashMap<String, (RuntimeValue, VarType)>,
    pub objects: HashMap<String, Object>,
    pub path: PathBuf,
    pub functions: HashMap<String, HashMap<String, (RuntimeValue, bool)>>,
    pub stop: Option<StopValue>,
}

fn get_global_variables() -> HashMap<String, (RuntimeValue, VarType)> {
    let vars = vec![
        (String::from("PI"), RuntimeValue::Float(PI)),
        (String::from("FLOAT_MAX"), RuntimeValue::Float(f32::MAX)),
        (String::from("DOUBLE_MAX"), RuntimeValue::Double(f64::MAX)),
        (String::from("INT_MAX"), RuntimeValue::Int(i64::MAX)),
        (String::from("LONG_MAX"), RuntimeValue::Long(i128::MAX)),
        (String::from("FLOAT_MIN"), RuntimeValue::Float(f32::MIN)),
        (String::from("DOUBLE_MIN"), RuntimeValue::Double(f64::MIN)),
        (String::from("INT_MIN"), RuntimeValue::Int(i64::MIN)),
        (String::from("LONG_MIN"), RuntimeValue::Long(i128::MIN)),
        (String::from("true"), RuntimeValue::Bool(true)),
        (String::from("false"), RuntimeValue::Bool(false)),
        (
            String::from("none"),
            RuntimeValue::Option(None, RuntimeType::Str),
        ),
        (
            String::from("some"),
            RuntimeValue::NativeFunction(NativeFunctions::Some),
        ),
        (
            String::from("ok"),
            RuntimeValue::NativeFunction(NativeFunctions::Ok),
        ),
        (
            String::from("err"),
            RuntimeValue::NativeFunction(NativeFunctions::Err),
        ),
        (
            String::from("input"),
            RuntimeValue::NativeFunction(NativeFunctions::Input),
        ),
        (
            String::from("trim"),
            RuntimeValue::NativeFunction(NativeFunctions::Trim),
        ),
        (
            String::from("print"),
            RuntimeValue::NativeFunction(NativeFunctions::Print),
        ),
        (
            String::from("clear"),
            RuntimeValue::NativeFunction(NativeFunctions::Clear),
        ),
        (
            String::from("wait"),
            RuntimeValue::NativeFunction(NativeFunctions::Wait),
        ),
        (
            String::from("range"),
            RuntimeValue::NativeFunction(NativeFunctions::Range),
        ),
        (String::from("INFINITY"), RuntimeValue::Float(f32::INFINITY)),
        (
            String::from("NEG_INFINITY"),
            RuntimeValue::Float(f32::NEG_INFINITY),
        ),
    ];

    let mut var_hashmap = HashMap::new();

    for var in vars {
        var_hashmap.insert(var.0, (var.1, VarType::Constant));
    }

    var_hashmap
}

impl Scope {
    pub fn new_with_stdlib(
        parent: Option<Rc<RefCell<Self>>>,
        path: PathBuf,
        namespace: Option<String>,
    ) -> (Rc<RefCell<Self>>, parser::Parser) {
        let mut parser = parser::Parser::default();
        let scope = Self {
            variables: get_global_variables(),
            children: HashMap::new(),
            objects: HashMap::new(),
            stop: None,
            functions: HashMap::new(),
            parent: parent.clone(),
            path,
        };

        let scope = Rc::new(RefCell::new(scope));
        let program = parser
            .produce_ast(fs::read_to_string(get_path("stdlib/main.cl".to_string())).unwrap())
            .unwrap();

        if let (Some(name), Some(parent)) = (namespace, parent) {
            if let Ok(scope) = get_next_scope(&parent, &name) {
                panic!()
            } else {
                let _ = parent.borrow_mut().push_child(name, scope.clone());
            }
        }

        let _ = evaluate(program, &scope).unwrap();

        (scope, parser)
    }

    pub fn new_from_parent_shallow(parent: Rc<RefCell<Self>>) -> Rc<RefCell<Self>> {
        let path = parent.borrow().path.clone();
        Self::new(Some(parent), path, None)
    }

    pub fn new_from_parent(parent: Rc<RefCell<Self>>, namespace: String) -> Rc<RefCell<Self>> {
        if let Some(scope) = parent.borrow().children.get(&namespace) {
            return scope.clone();
        }

        let path = parent.borrow().path.clone();
        let parent_name = path.file_name().unwrap();
        let folder = path.parent().unwrap().to_path_buf();

        let extra = if parent_name == "main.cl" {
            String::new()
        } else {
            format!(
                "{}/",
                parent_name.to_str().unwrap().split(".").nth(0).unwrap()
            )
        };

        let mut path1 = folder.clone();
        path1 = path1.join(format!("{extra}{namespace}.cl"));

        let mut path2 = folder.clone();
        path2 = path2.join(format!("{extra}{namespace}/main.cl"));

        if path1.exists() {
            Self::new(Some(parent), path1, Some(namespace))
        } else if path2.exists() {
            Self::new(Some(parent), path2, Some(namespace))
        } else {
            panic!("Tried:\n{path1:?}\n{path2:?}")
        }
    }

    pub fn new(
        parent: Option<Rc<RefCell<Self>>>,
        path: PathBuf,
        namespace: Option<String>,
    ) -> Rc<RefCell<Self>> {
        let scope = if parent.is_some() {
            Rc::new(RefCell::new(Self {
                variables: HashMap::new(),
                objects: HashMap::new(),
                stop: None,
                functions: HashMap::new(),
                parent: parent.clone(),
                children: HashMap::new(),
                path,
            }))
        } else {
            Self::new_with_stdlib(None, path, namespace.clone()).0
        };

        if let (Some(name), Some(parent)) = (namespace, parent) {
            if get_next_scope(&parent, &name).is_ok() {
                panic!()
            } else {
                let _ = parent.borrow_mut().push_child(name, scope.clone());
            }
        }

        scope
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::scope::objects::{get_function, get_object};
    use crate::runtime::values::{RuntimeValue, helper::VarType};
    use std::cell::RefCell;
    use std::rc::Rc;
    use std::str::FromStr;

    #[test]
    fn test_scope_global_variables() {
        let (scope_rc, _) =
            Scope::new_with_stdlib(None, PathBuf::from_str("./main.cl").unwrap(), None);
        let scope = scope_rc.borrow();
        assert!(scope.variables.contains_key("PI"));
        assert!(scope.variables.contains_key("true"));
        assert!(scope.variables.contains_key("print"));
        assert_eq!(scope.variables.get("PI").unwrap().1, VarType::Constant);
    }

    #[test]
    fn test_scope_push_and_get_var() {
        let (scope_rc, _) = Scope::new_with_stdlib(None, None);
        let mut scope = scope_rc.borrow_mut();
        scope
            .push_var(
                "x".to_string(),
                RuntimeValue::Int(42),
                VarType::Mutable(None),
            )
            .unwrap();
        assert_eq!(scope.variables.get("x").unwrap().0, RuntimeValue::Int(42));
    }

    #[test]
    fn test_scope_assign_var_and_type_mismatch() {
        let (scope_rc, _) = Scope::new_with_stdlib(None);
        let mut scope = scope_rc.borrow_mut();
        scope
            .push_var(
                "x".to_string(),
                RuntimeValue::Int(1),
                VarType::Mutable(None),
            )
            .unwrap();
        assert!(scope.assign_var("x", RuntimeValue::Int(2)).is_ok());
        let err = scope.assign_var("x", RuntimeValue::Bool(true)).unwrap_err();
        match err {
            ScopeErr::TypeMismatch(RuntimeValue::Int(_), RuntimeValue::Bool(_)) => {}
            _ => panic!("Expected TypeMismatch"),
        }
    }

    #[test]
    fn test_scope_assign_const_var_error() {
        let (scope_rc, _) = Scope::new_with_stdlib(None);
        let mut scope = scope_rc.borrow_mut();
        scope
            .push_var("y".to_string(), RuntimeValue::Int(1), VarType::Constant)
            .unwrap();
        let err = scope.assign_var("y", RuntimeValue::Int(2)).unwrap_err();
        match err {
            ScopeErr::AssignConstant(ref name) if name == "y" => {}
            _ => panic!("Expected AssignConstant"),
        }
    }

    #[test]
    fn test_scope_variable_shadowing() {
        let (parent, _) = Scope::new_with_stdlib(None);
        parent
            .borrow_mut()
            .push_var("z".to_string(), RuntimeValue::Int(1), VarType::Constant)
            .unwrap();
        let child = Scope::new(Some(parent.clone()));
        child
            .borrow_mut()
            .push_var(
                "z".to_string(),
                RuntimeValue::Int(2),
                VarType::Mutable(None),
            )
            .unwrap();
        assert_eq!(
            child.borrow().variables.get("z").unwrap().0,
            RuntimeValue::Int(2)
        );
    }

    #[test]
    fn test_scope_get_var_and_resolve_var() {
        let parent = Scope::new(None);
        parent
            .borrow_mut()
            .push_var(
                "a".to_string(),
                RuntimeValue::Int(10),
                VarType::Mutable(None),
            )
            .unwrap();
        let child = Scope::new(Some(parent.clone()));
        let (val, vtype) = crate::runtime::scope::variables::get_var(&child, "a").unwrap();
        assert_eq!(val, RuntimeValue::Int(10));
        assert_eq!(vtype, VarType::Mutable(None));
    }

    #[test]
    fn test_scope_push_and_get_object() {
        let mut scope = Scope::new(None);
        let obj = Object::NewType(RuntimeType::Int);
        scope
            .push_object("MyType".to_string(), obj.clone())
            .unwrap();
        let rc_scope = Rc::new(RefCell::new(scope));
        let fetched = get_object(rc_scope.clone(), "MyType").unwrap();
        assert_eq!(fetched, obj);
    }

    #[test]
    fn test_scope_push_and_get_function() {
        let mut scope = Scope::new(None);
        let obj = Object::NewType(RuntimeType::Int);
        scope.push_object("MyStruct".to_string(), obj).unwrap();
        scope
            .push_function(
                "MyStruct".to_string(),
                ("foo".to_string(), RuntimeValue::Int(1), false),
            )
            .unwrap();
        let rc_scope = Rc::new(RefCell::new(scope));
        let (val, is_static) = get_function(rc_scope.clone(), "MyStruct", "foo").unwrap();
        assert_eq!(val, RuntimeValue::Int(1));
        assert!(!is_static);
    }

    #[test]
    fn test_scope_get_object_error() {
        let scope = Rc::new(RefCell::new(Scope::new(None)));
        let err = get_object(scope.clone(), "DoesNotExist").unwrap_err();
        match err {
            ScopeErr::Object(ref name) if name == "DoesNotExist" => {}
            _ => panic!("Expected Object error"),
        }
    }

    #[test]
    fn test_scope_get_function_error() {
        let mut scope = Scope::new(None);
        let obj = Object::Struct(ObjectType::Tuple(vec![RuntimeType::Int]));
        scope.push_object("MyStruct".to_string(), obj).unwrap();
        let rc_scope = Rc::new(RefCell::new(scope));
        let err = get_function(rc_scope.clone(), "MyStruct", "does_not_exist");
        match err {
            Err(_) => {}
            _ => panic!("Expected Function error"),
        }
    }
}

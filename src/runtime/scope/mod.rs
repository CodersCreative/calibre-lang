pub mod objects;
pub mod variables;

use std::{
    cell::RefCell,
    collections::HashMap,
    f64::{self, consts::PI},
    i64,
    rc::Rc,
};

use thiserror::Error;

use crate::runtime::values::{
    NativeFunctions, RuntimeValue,
    helper::{ObjectType, StopValue, VarType},
};

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
    pub variables: HashMap<String, (RuntimeValue, VarType)>,
    pub objects: HashMap<String, Object>,
    pub functions: HashMap<String, HashMap<String, (RuntimeValue, bool)>>,
    pub stop: Option<StopValue>,
}

fn get_global_variables() -> HashMap<String, (RuntimeValue, VarType)> {
    let vars = vec![
        (String::from("PI"), RuntimeValue::Float(PI)),
        (String::from("FLOAT_MAX"), RuntimeValue::Float(f64::MAX)),
        (String::from("INT_MAX"), RuntimeValue::Integer(i64::MAX)),
        (String::from("FLOAT_MIN"), RuntimeValue::Float(f64::MIN)),
        (String::from("INT_MIN"), RuntimeValue::Integer(i64::MIN)),
        (String::from("true"), RuntimeValue::Bool(true)),
        (String::from("false"), RuntimeValue::Bool(false)),
        (String::from("null"), RuntimeValue::Null),
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
            String::from("range"),
            RuntimeValue::NativeFunction(NativeFunctions::Range),
        ),
        (String::from("INFINITY"), RuntimeValue::Float(f64::INFINITY)),
        (
            String::from("NEG_INFINITY"),
            RuntimeValue::Float(f64::NEG_INFINITY),
        ),
    ];

    let mut var_hashmap = HashMap::new();

    for var in vars {
        var_hashmap.insert(var.0, (var.1, VarType::Constant));
    }

    var_hashmap
}

impl Scope {
    pub fn new(parent: Option<Rc<RefCell<Self>>>) -> Self {
        Self {
            variables: if let None = parent {
                get_global_variables()
            } else {
                HashMap::new()
            },
            objects: HashMap::new(),
            stop: None,
            functions: HashMap::new(),
            parent,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::values::{RuntimeValue, helper::{VarType}};
    use crate::runtime::scope::objects::{get_object, get_function};
    use std::rc::Rc;
    use std::cell::RefCell;

    #[test]
    fn test_scope_global_variables() {
        let scope = Scope::new(None);
        assert!(scope.variables.contains_key("PI"));
        assert!(scope.variables.contains_key("true"));
        assert!(scope.variables.contains_key("print"));
        assert_eq!(scope.variables.get("PI").unwrap().1, VarType::Constant);
    }

    #[test]
    fn test_scope_push_and_get_var() {
        let mut scope = Scope::new(None);
        scope.push_var("x".to_string(), RuntimeValue::Integer(42), VarType::Mutable(None)).unwrap();
        assert_eq!(scope.variables.get("x").unwrap().0, RuntimeValue::Integer(42));
    }

    #[test]
    fn test_scope_assign_var_and_type_mismatch() {
        let mut scope = Scope::new(None);
        scope.push_var("x".to_string(), RuntimeValue::Integer(1), VarType::Mutable(None)).unwrap();
        assert!(scope.assign_var("x", RuntimeValue::Integer(2)).is_ok());
        let err = scope.assign_var("x", RuntimeValue::Bool(true)).unwrap_err();
        match err {
            ScopeErr::TypeMismatch(RuntimeValue::Integer(_), RuntimeValue::Bool(_)) => {}
            _ => panic!("Expected TypeMismatch"),
        }
    }

    #[test]
    fn test_scope_assign_const_var_error() {
        let mut scope = Scope::new(None);
        scope.push_var("y".to_string(), RuntimeValue::Integer(1), VarType::Constant).unwrap();
        let err = scope.assign_var("y", RuntimeValue::Integer(2)).unwrap_err();
        match err {
            ScopeErr::AssignConstant(ref name) if name == "y" => {}
            _ => panic!("Expected AssignConstant"),
        }
    }

    #[test]
    fn test_scope_variable_shadowing() {
        let parent = Rc::new(RefCell::new(Scope::new(None)));
        parent.borrow_mut().push_var("z".to_string(), RuntimeValue::Integer(1), VarType::Constant).unwrap();
        let child = Rc::new(RefCell::new(Scope::new(Some(parent.clone()))));
        child.borrow_mut().push_var("z".to_string(), RuntimeValue::Integer(2), VarType::Mutable(None)).unwrap();
        assert_eq!(child.borrow().variables.get("z").unwrap().0, RuntimeValue::Integer(2));
    }

    #[test]
    fn test_scope_get_var_and_resolve_var() {
        let parent = Rc::new(RefCell::new(Scope::new(None)));
        parent.borrow_mut().push_var("a".to_string(), RuntimeValue::Integer(10), VarType::Mutable(None)).unwrap();
        let child = Rc::new(RefCell::new(Scope::new(Some(parent.clone()))));
        let (val, vtype) = crate::runtime::scope::variables::get_var(child.clone(), "a").unwrap();
        assert_eq!(val, RuntimeValue::Integer(10));
        assert_eq!(vtype, VarType::Mutable(None));
    }

    #[test]
    fn test_scope_push_and_get_object() {
        let mut scope = Scope::new(None);
        let obj = Object::NewType(RuntimeType::Integer);
        scope.push_object("MyType".to_string(), obj.clone()).unwrap();
        let rc_scope = Rc::new(RefCell::new(scope));
        let fetched = get_object(rc_scope.clone(), "MyType").unwrap();
        assert_eq!(fetched, obj);
    }

    #[test]
    fn test_scope_push_and_get_function() {
        let mut scope = Scope::new(None);
        let obj = Object::NewType(RuntimeType::Integer);
        scope.push_object("MyStruct".to_string(), obj).unwrap();
        scope.push_function("MyStruct".to_string(), ("foo".to_string(), RuntimeValue::Integer(1), false)).unwrap();
        let rc_scope = Rc::new(RefCell::new(scope));
        let (val, is_static) = get_function(rc_scope.clone(), "MyStruct", "foo").unwrap();
        assert_eq!(val, RuntimeValue::Integer(1));
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
        let obj = Object::Struct(ObjectType::Tuple(vec![RuntimeType::Integer]));
        scope.push_object("MyStruct".to_string(), obj).unwrap();
        let rc_scope = Rc::new(RefCell::new(scope));
        let err = get_function(rc_scope.clone(), "MyStruct", "does_not_exist");
        match err {
            Err(_) => {}
            _ => panic!("Expected Function error"),
        }
    }
}

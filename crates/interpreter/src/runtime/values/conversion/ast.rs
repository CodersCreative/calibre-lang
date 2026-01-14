use std::collections::HashMap;

use calibre_common::environment::InterpreterFrom;
use calibre_mir::environment::MiddleTypeDefType;
use calibre_mir_ty::{MiddleNode, MiddleNodeType};
use calibre_parser::{
    ast::{ObjectMap, ParserDataType, ParserInnerType, ParserText, RefMutability},
    lexer::Span,
};

use crate::runtime::{
    scope::InterpreterEnvironment,
    values::{RuntimeType, RuntimeValue},
};

impl InterpreterFrom<RuntimeValue> for MiddleNode {
    type Interpreter = InterpreterEnvironment;
    fn interpreter_from(
        env: &Self::Interpreter,
        scope: &u64,
        value: RuntimeValue,
    ) -> Result<Self, calibre_common::errors::ScopeErr> {
        Ok(MiddleNode::new_from_type(MiddleNodeType::interpreter_from(
            env, scope, value,
        )?))
    }
}

impl InterpreterFrom<RuntimeValue> for MiddleNodeType {
    type Interpreter = InterpreterEnvironment;
    fn interpreter_from(
        env: &Self::Interpreter,
        scope: &u64,
        value: RuntimeValue,
    ) -> Result<Self, calibre_common::errors::ScopeErr> {
        match value {
            RuntimeValue::Null => Ok(MiddleNodeType::EmptyLine),
            RuntimeValue::Float(x) => Ok(MiddleNodeType::FloatLiteral(x)),
            RuntimeValue::Type(x) => Ok(MiddleNodeType::DataType { data_type: x }),
            RuntimeValue::Int(x) => Ok(MiddleNodeType::IntLiteral(x)),
            RuntimeValue::Range(from, to) => Ok(MiddleNodeType::RangeDeclaration {
                from: Box::new(MiddleNode::new_from_type(MiddleNodeType::IntLiteral(from))),
                to: Box::new(MiddleNode::new_from_type(MiddleNodeType::IntLiteral(to))),
                inclusive: false,
            }),
            RuntimeValue::Bool(x) => Ok(match x {
                true => MiddleNodeType::Identifier(ParserText::from(String::from("true"))),
                _ => MiddleNodeType::Identifier(ParserText::from(String::from("false"))),
            }),
            RuntimeValue::Str(x) => Ok(MiddleNodeType::StringLiteral(ParserText::from(x))),
            RuntimeValue::Char(x) => Ok(MiddleNodeType::CharLiteral(x)),
            RuntimeValue::Aggregate(x, y) => Ok(MiddleNodeType::AggregateExpression {
                identifier: x.map(ParserText::from),
                value: {
                    let mut map = HashMap::new();

                    for (k, v) in y.0 {
                        map.insert(k, MiddleNode::interpreter_from(env, scope, v)?);
                    }

                    ObjectMap(map)
                },
            }),
            RuntimeValue::Enum(x, y, z) => Ok(MiddleNodeType::EnumExpression {
                identifier: ParserText::from(x.clone()),
                value: {
                    let MiddleTypeDefType::Enum(enm) = env.get_object_type(&x).unwrap() else {
                        unreachable!()
                    };

                    enm[y].0.clone()
                },
                data: if let Some(z) = z {
                    Some(Box::new(MiddleNode::interpreter_from(env, scope, *z)?))
                } else {
                    None
                },
            }),
            RuntimeValue::Ref(x, _) => {
                let value = env.get_var(&x).unwrap().value.clone();
                MiddleNodeType::interpreter_from(env, scope, value)
            }
            RuntimeValue::List { data, data_type } => {
                let mut lst = Vec::new();

                for val in data {
                    let val = MiddleNode::interpreter_from(env, scope, val)?;
                    lst.push(val);
                }

                Ok(MiddleNodeType::ListLiteral(
                    ParserDataType::interpreter_from(env, scope, *data_type)?,
                    lst,
                ))
            }
            RuntimeValue::Option(Some(value), _) => Ok(MiddleNodeType::CallExpression(
                Box::new(MiddleNode::new_from_type(MiddleNodeType::Identifier(
                    ParserText::from(String::from("some")),
                ))),
                vec![(MiddleNode::interpreter_from(env, scope, *value)?, None)],
            )),
            RuntimeValue::Option(None, _) => Ok(MiddleNodeType::CallExpression(
                Box::new(MiddleNode::new_from_type(MiddleNodeType::Identifier(
                    ParserText::from(String::from("none")),
                ))),
                Vec::new(),
            )),
            RuntimeValue::Result(Ok(value), _) => Ok(MiddleNodeType::CallExpression(
                Box::new(MiddleNode::new_from_type(MiddleNodeType::Identifier(
                    ParserText::from(String::from("ok")),
                ))),
                vec![(MiddleNode::interpreter_from(env, scope, *value)?, None)],
            )),
            RuntimeValue::Result(Err(value), _) => Ok(MiddleNodeType::CallExpression(
                Box::new(MiddleNode::new_from_type(MiddleNodeType::Identifier(
                    ParserText::from(String::from("err")),
                ))),
                vec![(MiddleNode::interpreter_from(env, scope, *value)?, None)],
            )),
            RuntimeValue::NativeFunction(x) => Ok(MiddleNodeType::Identifier(ParserText::from(
                x.get_resolved_name(env),
            ))),
            RuntimeValue::Function {
                parameters,
                body,
                return_type,
                is_async,
            } => Ok(MiddleNodeType::FunctionDeclaration {
                parameters: {
                    let mut params = Vec::new();

                    for param in parameters {
                        params.push((
                            ParserText::from(param.0),
                            ParserDataType::interpreter_from(env, scope, param.1)?,
                            if let Some(def) = param.2 {
                                Some(MiddleNode::interpreter_from(env, scope, def)?)
                            } else {
                                None
                            },
                        ));
                    }

                    params
                },
                body: body.0,
                return_type: ParserDataType::interpreter_from(env, scope, return_type)?,
                is_async,
            }),
        }
    }
}

impl InterpreterFrom<RuntimeType> for ParserDataType {
    type Interpreter = InterpreterEnvironment;
    fn interpreter_from(
        env: &Self::Interpreter,
        scope: &u64,
        value: RuntimeType,
    ) -> Result<Self, calibre_common::errors::ScopeErr> {
        Ok(ParserDataType {
            data_type: ParserInnerType::interpreter_from(env, scope, value)?,
            span: Span::default(),
        })
    }
}

impl InterpreterFrom<RuntimeType> for ParserInnerType {
    type Interpreter = InterpreterEnvironment;
    fn interpreter_from(
        env: &Self::Interpreter,
        scope: &u64,
        value: RuntimeType,
    ) -> Result<Self, calibre_common::errors::ScopeErr> {
        Ok(match value {
            RuntimeType::Float => ParserInnerType::Float,
            RuntimeType::Int => ParserInnerType::Int,
            RuntimeType::Dynamic => ParserInnerType::Dynamic,
            RuntimeType::Bool => ParserInnerType::Bool,
            RuntimeType::Str => ParserInnerType::Str,
            RuntimeType::Char => ParserInnerType::Char,
            RuntimeType::Tuple(_x) => {
                let lst = Vec::new();

                ParserInnerType::Tuple(lst)
            }
            RuntimeType::List(value) => {
                let val = ParserDataType::interpreter_from(env, scope, *value)?;
                ParserInnerType::List(Box::new(val))
            }
            RuntimeType::Range => ParserInnerType::Range,
            RuntimeType::Option(value) => {
                let val = ParserDataType::interpreter_from(env, scope, *value)?;
                ParserInnerType::Option(Box::new(val))
            }
            RuntimeType::NativeFn(value) => {
                let val = ParserDataType::interpreter_from(env, scope, *value)?;
                ParserInnerType::Option(Box::new(val))
            }
            RuntimeType::Enum(value) => ParserInnerType::Struct(value),
            RuntimeType::Struct(value) => ParserInnerType::Struct(value),
            RuntimeType::Ref(value) => {
                let val = ParserDataType::interpreter_from(env, scope, *value)?;
                ParserInnerType::Ref(Box::new(val), RefMutability::Ref)
            }
            RuntimeType::Null => ParserInnerType::Null,
            RuntimeType::Result { ok, err } => {
                let ok = ParserDataType::interpreter_from(env, scope, *ok)?;
                let err = ParserDataType::interpreter_from(env, scope, *err)?;
                ParserInnerType::Result {
                    ok: Box::new(ok),
                    err: Box::new(err),
                }
            }
            RuntimeType::Function {
                return_type,
                parameters,
                is_async,
            } => {
                let return_type = ParserDataType::interpreter_from(env, scope, *return_type)?;
                let mut params = Vec::new();

                for param in parameters {
                    let param = ParserDataType::interpreter_from(env, scope, param)?;
                    params.push(param);
                }

                ParserInnerType::Function {
                    return_type: Box::new(return_type),
                    parameters: params,
                    is_async,
                }
            }
            RuntimeType::Type => unreachable!(),
        })
    }
}

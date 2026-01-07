use std::collections::HashMap;

use calibre_parser::{
    ast::{
        IfComparisonType, LoopType, Node, NodeType, ObjectType, ParserDataType, ParserInnerType,
        ParserText, VarType,
    },
    lexer::Span,
};

use crate::{
    ast::{MiddleNode, MiddleNodeType},
    errors::MiddleErr,
};
use environment::*;
pub mod ast;
pub mod environment;
pub mod errors;
pub mod identifiers;

impl MiddleEnvironment {
    pub fn evaluate(&mut self, scope: &u64, node: Node) -> Result<MiddleNode, MiddleErr> {
        self.current_location = self.get_location(scope, node.span);

        match node.node_type {
            NodeType::Identifier(x) => Ok(MiddleNode {
                node_type: MiddleNodeType::Identifier(self.resolve_parser_text(scope, &x).unwrap()),
                span: node.span,
            }),
            NodeType::IntLiteral(x) => Ok(MiddleNode {
                node_type: MiddleNodeType::IntLiteral(x),
                span: node.span,
            }),
            NodeType::FloatLiteral(x) => Ok(MiddleNode {
                node_type: MiddleNodeType::FloatLiteral(x),
                span: node.span,
            }),
            NodeType::StringLiteral(x) => Ok(MiddleNode {
                node_type: MiddleNodeType::StringLiteral(x),
                span: node.span,
            }),
            NodeType::CharLiteral(x) => Ok(MiddleNode {
                node_type: MiddleNodeType::CharLiteral(x),
                span: node.span,
            }),
            NodeType::RangeDeclaration {
                from,
                to,
                inclusive,
            } => Ok(MiddleNode {
                node_type: MiddleNodeType::RangeDeclaration {
                    from: Box::new(self.evaluate(scope, *from)?),
                    to: Box::new(self.evaluate(scope, *to)?),
                    inclusive,
                },
                span: node.span,
            }),
            NodeType::MemberExpression { mut path } => {
                let first = path.remove(0);
                let mut list = vec![(self.evaluate(scope, first.0)?, first.1)];

                for item in path {
                    list.push((
                        match item.0.node_type {
                            NodeType::Identifier(x) => MiddleNode {
                                node_type: MiddleNodeType::Identifier(x),
                                span: node.span,
                            },
                            _ => self.evaluate(scope, item.0)?,
                        },
                        item.1,
                    ));
                }

                Ok(MiddleNode {
                    node_type: MiddleNodeType::MemberExpression { path: list },
                    span: node.span,
                })
            }
            NodeType::IfStatement {
                comparison,
                then,
                otherwise,
                special_delim: _,
            } => match *comparison {
                IfComparisonType::If(x) => Ok(MiddleNode {
                    node_type: MiddleNodeType::IfStatement {
                        comparison: Box::new(self.evaluate(scope, x)?),
                        then: Box::new(self.evaluate(scope, *then)?),
                        otherwise: match otherwise {
                            Some(x) => Some(Box::new(self.evaluate(scope, *x)?)),
                            _ => None,
                        },
                    },
                    span: node.span,
                }),
                IfComparisonType::IfLet { value, pattern } => Ok(MiddleNode {
                    node_type: MiddleNodeType::CallExpression(
                        Box::new(self.evaluate(
                            scope,
                            Node {
                                node_type: NodeType::MatchDeclaration {
                                    parameters: (
                                        ParserText::new(
                                            String::from("input_value"),
                                            Span::default(),
                                        ),
                                        self.resolve_type_from_node(scope, &value).unwrap_or(
                                            ParserDataType::new(
                                                ParserInnerType::Dynamic,
                                                Span::default(),
                                            ),
                                        ),
                                        None,
                                    ),
                                    return_type: self.resolve_type_from_node(scope, &then),
                                    body: vec![
                                        (pattern.0, pattern.1, then),
                                        (
                                            Node {
                                                node_type: NodeType::Identifier(ParserText::new(
                                                    String::from("_"),
                                                    Span::default(),
                                                )),
                                                span: Span::default(),
                                            },
                                            Vec::new(),
                                            otherwise.unwrap_or(Box::new(Node {
                                                node_type: NodeType::EmptyLine,
                                                span: Span::default(),
                                            })),
                                        ),
                                    ],
                                    is_async: false,
                                },
                                span: node.span,
                            },
                        )?),
                        vec![(self.evaluate(scope, value)?, None)],
                    ),
                    span: node.span,
                }),
            },
            NodeType::Break => Ok(MiddleNode {
                node_type: MiddleNodeType::Break,
                span: node.span,
            }),
            NodeType::Continue => Ok(MiddleNode {
                node_type: MiddleNodeType::Continue,
                span: node.span,
            }),
            NodeType::EmptyLine => Ok(MiddleNode {
                node_type: MiddleNodeType::EmptyLine,
                span: node.span,
            }),
            NodeType::Return { value } => Ok(MiddleNode {
                node_type: MiddleNodeType::Return {
                    value: Box::new(self.evaluate(scope, *value)?),
                },
                span: node.span,
            }),
            NodeType::RefStatement { mutability, value } => Ok(MiddleNode {
                node_type: MiddleNodeType::RefStatement {
                    mutability,
                    value: Box::new(self.evaluate(scope, *value)?),
                },
                span: node.span,
            }),
            NodeType::DerefStatement { value } => Ok(MiddleNode {
                node_type: MiddleNodeType::DerefStatement {
                    value: Box::new(self.evaluate(scope, *value)?),
                },
                span: node.span,
            }),
            NodeType::ParenExpression { value } => self.evaluate(scope, *value),
            NodeType::VariableDeclaration {
                var_type,
                identifier,
                value,
                data_type,
            } => {
                let new_name =
                    get_disamubiguous_name(scope, Some(identifier.text.trim()), Some(&var_type));
                let data_type = if let Some(x) = data_type {
                    x
                } else if let Some(x) = self.resolve_type_from_node(scope, &value) {
                    x
                } else {
                    ParserDataType {
                        data_type: ParserInnerType::Dynamic,
                        span: identifier.span,
                    }
                };
                let value = self.evaluate(scope, *value)?;

                self.variables.insert(
                    new_name.clone(),
                    MiddleVariable {
                        data_type: data_type.clone(),
                        var_type: var_type.clone(),
                        location: self.current_location.clone(),
                    },
                );

                self.scopes
                    .get_mut(scope)
                    .unwrap()
                    .mappings
                    .insert(identifier.text, new_name.clone());

                Ok(MiddleNode {
                    node_type: MiddleNodeType::VariableDeclaration {
                        var_type,
                        identifier: ParserText {
                            text: new_name,
                            span: identifier.span,
                        },
                        value: Box::new(value),
                        data_type,
                    },
                    span: node.span,
                })
            }
            NodeType::TypeDeclaration { identifier, object } => {
                let new_name = get_disamubiguous_name(scope, Some(identifier.text.trim()), None);

                self.objects.insert(
                    new_name.clone(),
                    MiddleObject {
                        object_type: object,
                        functions: HashMap::new(),
                        traits: Vec::new(),
                        location: self.current_location.clone(),
                    },
                );

                self.scopes
                    .get_mut(scope)
                    .unwrap()
                    .mappings
                    .insert(identifier.text, new_name);

                Ok(MiddleNode {
                    node_type: MiddleNodeType::EmptyLine,
                    span: node.span,
                })
            }
            NodeType::BooleanExpression {
                left,
                right,
                operator,
            } => Ok(MiddleNode {
                node_type: MiddleNodeType::BooleanExpression {
                    left: Box::new(self.evaluate(scope, *left)?),
                    right: Box::new(self.evaluate(scope, *right)?),
                    operator,
                },
                span: node.span,
            }),
            NodeType::ComparisonExpression {
                left,
                right,
                operator,
            } => Ok(MiddleNode {
                node_type: MiddleNodeType::ComparisonExpression {
                    left: Box::new(self.evaluate(scope, *left)?),
                    right: Box::new(self.evaluate(scope, *right)?),
                    operator,
                },
                span: node.span,
            }),
            NodeType::BinaryExpression {
                left,
                right,
                operator,
            } => Ok(MiddleNode {
                node_type: MiddleNodeType::BinaryExpression {
                    left: Box::new(self.evaluate(scope, *left)?),
                    right: Box::new(self.evaluate(scope, *right)?),
                    operator,
                },
                span: node.span,
            }),
            NodeType::NotExpression { value } => Ok(MiddleNode {
                node_type: MiddleNodeType::NotExpression {
                    value: Box::new(self.evaluate(scope, *value)?),
                },
                span: node.span,
            }),
            NodeType::NegExpression { value } => Ok(MiddleNode {
                node_type: MiddleNodeType::NegExpression {
                    value: Box::new(self.evaluate(scope, *value)?),
                },
                span: node.span,
            }),
            NodeType::AsExpression { value, typ } => Ok(MiddleNode {
                node_type: MiddleNodeType::AsExpression {
                    value: Box::new(self.evaluate(scope, *value)?),
                    typ,
                },
                span: node.span,
            }),
            NodeType::IsDeclaration { value, data_type } => Ok(MiddleNode {
                node_type: MiddleNodeType::IsDeclaration {
                    value: Box::new(self.evaluate(scope, *value)?),
                    data_type,
                },
                span: node.span,
            }),
            NodeType::InDeclaration {
                identifier,
                expression,
            } => Ok(MiddleNode {
                node_type: MiddleNodeType::InDeclaration {
                    identifier: Box::new(self.evaluate(scope, *identifier)?),
                    expression: Box::new(self.evaluate(scope, *expression)?),
                },
                span: node.span,
            }),
            NodeType::DebugExpression { value } => Ok(MiddleNode {
                node_type: MiddleNodeType::DebugExpression {
                    pretty_printed_str: value.to_string(),
                    value: Box::new(self.evaluate(scope, *value)?),
                },
                span: node.span,
            }),
            NodeType::ListLiteral(x) => {
                let mut lst = Vec::new();

                for item in x {
                    lst.push(self.evaluate(scope, item)?);
                }

                Ok(MiddleNode {
                    node_type: MiddleNodeType::ListLiteral(lst),
                    span: node.span,
                })
            }
            NodeType::Try { value } => Ok(MiddleNode {
                node_type: MiddleNodeType::CallExpression(
                    Box::new(self.evaluate(
                        scope,
                        Node {
                            node_type: NodeType::MatchDeclaration {
                                parameters: (
                                    ParserText::new(
                                        String::from("input_value"),
                                        Span::default(),
                                    ),
                                    self.resolve_type_from_node(scope, &value).unwrap_or(
                                        ParserDataType::new(
                                            ParserInnerType::Dynamic,
                                            Span::default(),
                                        ),
                                    ),
                                    None,
                                ),
                                body: vec![
                                    (
                                        Node {
                                            node_type: NodeType::CallExpression(Box::new(Node::new_from_type(NodeType::Identifier(ParserText::from(String::from("Ok"))))), vec![(Node::new_from_type(NodeType::Identifier(ParserText::from(String::from("anon_ok_value")))), None)]),
                                            span: Span::default(),
                                        },
                                        Vec::new(),
                                        Box::new(Node {
                                            node_type: NodeType::Identifier(ParserText::from(String::from("anon_ok_value"))),
                                            span: Span::default(),
                                        }),
                                    ),
                                    (
                                        Node {
                                            node_type: NodeType::CallExpression(Box::new(Node::new_from_type(NodeType::Identifier(ParserText::from(String::from("Err"))))), vec![(Node::new_from_type(NodeType::Identifier(ParserText::from(String::from("anon_err_value")))), None)]),
                                            span: Span::default(),
                                        },
                                        Vec::new(),
                                        Box::new(Node {
                                            node_type: NodeType::Return { value: Box::new(Node::new_from_type(NodeType::CallExpression(Box::new(Node::new_from_type(NodeType::Identifier(ParserText::from(String::from("err"))))), vec![(Node::new_from_type(NodeType::Identifier(ParserText::from(String::from("anon_err_value")))), None)]))) },
                                            span: Span::default(),
                                        }),
                                    ),
                                ],
                                return_type:  match self.resolve_type_from_node(scope, &value) {
                                    Some(ParserDataType { data_type: ParserInnerType::Result(x, _), span: _ }) | Some(ParserDataType { data_type: ParserInnerType::Option(x), span: _ }) => Some(*x),
                                    Some(x) => Some(x),
                                    _ => None
                                },
                                is_async: false,
                            },
                            span: node.span,
                        },
                    )?),
                    vec![(self.evaluate(scope, *value)?, None)],
                ),
                span: node.span,
            }),
            NodeType::LoopDeclaration { loop_type, body } => Ok(MiddleNode {
                node_type: MiddleNodeType::LoopDeclaration {
                    body: Box::new(self.evaluate(
                        scope,
                        Node::new_from_type(NodeType::ScopeDeclaration {
                            body: {
                                let mut lst = vec![match *loop_type {
                                    LoopType::While(x) => Node::new_from_type(NodeType::IfStatement {
                                        comparison: Box::new(IfComparisonType::If(
                                            Node::new_from_type(NodeType::NegExpression {
                                                value: Box::new(x),
                                            }),
                                        )),
                                        then: Box::new(Node::new_from_type(
                                            NodeType::Break,
                                        )),
                                        otherwise: None,
                                        special_delim: true,
                                    }),
                                    _ => todo!(),
                                }];

                                match body.node_type {
                                    NodeType::ScopeDeclaration { mut body, is_temp: _ } => lst.append(&mut body),
                                    _ => lst.push(*body),
                                }

                                lst
                            },
                            is_temp: true,
                        }),
                    )?),
                },
                span: node.span,
            }),
            NodeType::IterExpression {
                map,
                loop_type,
                conditionals,
            } => self.evaluate(
                scope,
                Node {
                    node_type: NodeType::ScopeDeclaration {
                        body: vec![
                            Node::new_from_type(NodeType::VariableDeclaration {
                                var_type: VarType::Mutable,
                                identifier: ParserText::from(String::from("anon_iter_list")),
                                value: Box::new(Node::new_from_type(NodeType::ListLiteral(
                                    Vec::new(),
                                ))),
                                data_type: self.resolve_type_from_node(scope, &map),
                            }),
                            Node::new_from_type(NodeType::LoopDeclaration {
                                loop_type,
                                body: Box::new(Node::new_from_type(NodeType::ScopeDeclaration {
                                    body: {
                                        let mut lst = Vec::new();

                                        for condition in conditionals {
                                            lst.push(Node::new_from_type(NodeType::IfStatement {
                                                comparison: Box::new(IfComparisonType::If(
                                                    Node::new_from_type(NodeType::NegExpression {
                                                        value: Box::new(condition),
                                                    }),
                                                )),
                                                then: Box::new(Node::new_from_type(
                                                    NodeType::Continue,
                                                )),
                                                otherwise: None,
                                                special_delim: true,
                                            }));
                                        }

                                        lst.push(Node::new_from_type(
                                            NodeType::AssignmentExpression {
                                                identifier: Box::new(Node::new_from_type(
                                                    NodeType::Identifier(ParserText::from(
                                                        String::from("anon_iter_list"),
                                                    )),
                                                )),
                                                value: Box::new(Node::new_from_type(NodeType::BinaryExpression { left: Box::new(Node::new_from_type(
                                                    NodeType::Identifier(ParserText::from(
                                                        String::from("anon_iter_list"),
                                                    )),
                                                )), right: map, operator: calibre_parser::ast::binary::BinaryOperator::Shl })),
                                            },
                                        ));

                                        lst
                                    },
                                    is_temp: true,
                                })),
                            }),
                        ],
                        is_temp: true,
                    },
                    span: node.span,
                },
            ),
            NodeType::AssignmentExpression { identifier, value } => Ok(MiddleNode {
                node_type: MiddleNodeType::AssignmentExpression {
                    identifier: Box::new(self.evaluate(scope, *identifier)?),
                    value: Box::new(self.evaluate(scope, *value)?),
                },
                span: node.span,
            }),
            NodeType::ImplDeclaration {
                identifier,
                functions,
            } => {
                let resolved = self.resolve_parser_text(scope, &identifier).unwrap();
                for function in functions {
                    if let NodeType::VariableDeclaration {
                        identifier: iden,
                        value,
                        ..
                    } = function.0.node_type
                    {
                        let func = self.evaluate(scope, *value)?;
                        let location = self.get_location(scope, function.0.span);
                        self.objects
                            .get_mut(&resolved.text)
                            .unwrap()
                            .functions
                            .insert(iden.to_string(), (func, location, function.1));
                    }
                }

                Ok(MiddleNode {
                    node_type: MiddleNodeType::EmptyLine,
                    span: node.span,
                })
            }
            NodeType::ScopeDeclaration { body, is_temp } => {
                let _new_scope = if is_temp {
                    self.new_scope_from_parent_shallow(*scope)
                } else {
                    scope.clone()
                };

                let mut stmts = Vec::new();
                for statement in body.into_iter() {
                    stmts.push(self.evaluate(scope, statement)?);
                }

                Ok(MiddleNode {
                    node_type: MiddleNodeType::ScopeDeclaration {
                        body: stmts,
                        is_temp,
                    },
                    span: node.span,
                })
            }
            NodeType::StructLiteral(data) =>  Ok(MiddleNode {
                node_type: MiddleNodeType::StructLiteral(
                    match data {
                        ObjectType::Map(x) => {
                            let mut map = HashMap::new();

                            for itm in x {
                                map.insert(
                                    itm.0,
                                    if let Some(x) = itm.1 {
                                        Some(self.evaluate(scope, x)?)
                                    } else {
                                        None
                                    },
                                );
                            }

                            ObjectType::Map(map)
                        }
                        ObjectType::Tuple(x) => {
                            let mut map = Vec::new();

                            for itm in x {
                                map.push(if let Some(x) = itm {
                                    Some(self.evaluate(scope, x)?)
                                } else {
                                    None
                                });
                            }

                            ObjectType::Tuple(map)
                        }
                    }
                ),
                span: node.span,
            }),
            NodeType::EnumExpression {
                identifier,
                value,
                data,
            } => {
                let identifier = self.resolve_parser_text(scope, &identifier).unwrap();
                Ok(MiddleNode {
                node_type: MiddleNodeType::EnumExpression {
                    identifier: identifier.clone(),
                    value,
                    data: if let Some(data) = data {
                        match data {
                            ObjectType::Map(x) => {
                                let mut map = HashMap::new();

                                for itm in x {
                                    map.insert(
                                        itm.0,
                                        if let Some(x) = itm.1 {
                                            Some(self.evaluate(scope, x)?)
                                        } else {
                                            None
                                        },
                                    );
                                }

                                Some(ObjectType::Map(map))
                            }
                            ObjectType::Tuple(data) => {
                                let mut map = Vec::new();

                                for itm in data {
                                    map.push(if let Some(x) = itm {
                                        Some(self.evaluate(scope, x)?)
                                    } else {
                                        None
                                    });
                                }

                                if let Some(x) = self.variables.get(&identifier.to_string()) {
                                    // TODO deal with references
                                    let mut args = vec![(MiddleNode::new_from_type(MiddleNodeType::Identifier(identifier)), None)];
                                    args.append(&mut map.into_iter().map(|x| (x.unwrap(), None)).collect());
                                    return Ok(MiddleNode::new_from_type(MiddleNodeType::CallExpression(Box::new(MiddleNode::new_from_type(MiddleNodeType::Identifier(ParserText::from(x.data_type.to_string())))), args)));
                                }else if self.objects.contains_key(&identifier.to_string()) {
                                    return Ok(MiddleNode::new_from_type(MiddleNodeType::CallExpression(Box::new(MiddleNode::new_from_type(MiddleNodeType::Identifier(identifier))), map.into_iter().map(|x| (x.unwrap(), None)).collect())));
                                }else {
                                    Some(ObjectType::Tuple(map))
                                }
                            }
                        }
                    } else {
                        None
                    },
                },
                span: node.span,
            })
            },
            NodeType::MatchDeclaration { parameters, body, return_type, is_async } => Ok(MiddleNode {
                node_type: MiddleNodeType::MatchDeclaration {
                    parameters: (parameters.0, parameters.1, if let Some(x) = parameters.2 {Some(Box::new(self.evaluate(scope, *x)?))} else {None}),
                    body: {
                        // TODO make sure to create a new scope and add variables that are created in the match arms
                        todo!()
                    },
                    return_type,
                    is_async
                },
                span: node.span,
            }),
            NodeType::FunctionDeclaration { parameters, body, return_type, is_async } => {
                let mut params = Vec::with_capacity(parameters.len());
                let new_scope = self.new_scope_from_parent_shallow(*scope);
                for param in parameters {
                    let new_name = get_disamubiguous_name(scope, Some(param.0.trim()), Some(&VarType::Mutable));
                    self.variables.insert(
                        new_name.clone(),
                        MiddleVariable {
                            data_type: param.1.clone(),
                            var_type: VarType::Mutable,
                            location: self.current_location.clone(),
                        },
                    );

                    self.scopes
                        .get_mut(&new_scope)
                        .unwrap()
                        .mappings
                        .insert(param.0.text.clone(), new_name.clone());

                    params.push((
                        param.0,
                        param.1,
                        if let Some(x) = param.2 {
                            Some(self.evaluate(scope, x)?)
                        }else{
                            None
                        }
                    ));
                }


                Ok(MiddleNode {
                    node_type: MiddleNodeType::FunctionDeclaration { parameters: params, body: Box::new(self.evaluate(scope, *body)?), return_type, is_async },
                    span: node.span,
                })
            },
            NodeType::CallExpression(caller, args) => {
                if let NodeType::Identifier(caller) = &caller.node_type {
                    if "tuple" == &caller.text {
                        let mut lst = Vec::new();

                        for arg in args {
                            lst.push(self.evaluate(scope, arg.0)?);
                        }
                        
                        return Ok(MiddleNode {
                            node_type: MiddleNodeType::TupleLiteral(lst),
                            span: node.span,
                        });
                    }
                }
                Ok(MiddleNode {
                node_type: MiddleNodeType::CallExpression(Box::new(self.evaluate(scope, *caller)?), {
                    let mut lst = Vec::new();

                    for arg in args {
                        lst.push((
                            self.evaluate(scope, arg.0)?,
                            if let Some(a) = arg.1 {
                                Some(self.evaluate(scope, a)?)
                            }else{
                                None
                            }
                        ));
                    }

                    lst
                }),
                span: node.span,
            })},
            NodeType::ImportStatement { module, alias, values } => {
                let new_scope = if let Some(alias) = alias {
                    if ["super", "root"].contains(&alias.as_str()) {
                        return Ok(MiddleNode {
                            node_type: MiddleNodeType::EmptyLine,
                            span: node.span,
                        });
                    }
                    let new_scope_id = self.get_scope_list(*scope, module.into_iter().map(|x| x.to_string()).collect())?;
                    self.scopes
                        .get_mut(scope)
                        .unwrap()
                        .children
                        .insert(alias.to_string(), new_scope_id);

                    return Ok(MiddleNode {
                        node_type: MiddleNodeType::EmptyLine,
                        span: node.span,
                    });
                } else if !values.is_empty() {
                    self.get_scope_list(*scope, module.iter().map(|x| x.to_string()).collect())?
                } else {
                    let (_, n) = self.import_scope_list(*scope, module.into_iter().map(|x| x.to_string()).collect())?;
                    return Ok(if let Some(x) = n{x} else {
                        MiddleNode {
                            node_type: MiddleNodeType::EmptyLine,
                            span: node.span,
                        }
                    });
                };

                if &values[0].text == "*" {
                    let vars: Vec<(String, String)> = self
                        .scopes
                        .get(&new_scope)
                        .unwrap()
                        .mappings
                        .iter()
                        .map(|x| (x.0.clone(), x.1.clone()))
                        .collect();

                    for (key, value) in vars {
                        if !key.starts_with("__") {
                            let scope = self.scopes
                                .get_mut(scope)
                                .unwrap();
                            if !scope.mappings.contains_key(&key) {
                                scope
                                .mappings
                                .insert(key, value);
                            }
                        }
                    }
                } else {
                    for key in values {
                        if let Some(value) = self
                            .scopes
                            .get(&new_scope)
                            .unwrap()
                            .mappings
                            .get(&key.text)
                            .map(|x| x.clone())
                        {
                            self.scopes
                                .get_mut(scope)
                                .unwrap()
                                .mappings
                                .insert(key.to_string(), value);
                        } else {
                            return Err(MiddleErr::CantImport(format!("{} at {:?}", key, module)));
                        }
                    }
                }

                Ok(MiddleNode {
                    node_type: MiddleNodeType::EmptyLine,
                    span: node.span,
                })
            },
            NodeType::ScopeMemberExpression { path } => self.evaluate_scope_member_expression(scope, path),
            NodeType::PipeExpression(_) => todo!(),
        }
    }
}

impl MiddleEnvironment {
    pub fn get_scope_member_scope_path(
        &mut self,
        scope: &u64,
        mut path: Vec<Node>,
    ) -> Result<(u64, Node), MiddleErr> {
        if let NodeType::Identifier(x) = &path[0].node_type {
            if let Ok(s) = self.get_next_scope(*scope, &x) {
                if path.len() <= 2 {
                    return Ok((s, path.remove(1)));
                }

                match self.get_scope_member_scope_path(&s, path[1..].to_vec()) {
                    Ok(x) => return Ok(x),
                    _ => {}
                }
            }
        }

        Err(MiddleErr::Scope(format!("{:?}", path[0].node_type)).into())
    }

    pub fn evaluate_scope_member_expression(
        &mut self,
        scope: &u64,
        path: Vec<Node>,
    ) -> Result<MiddleNode, MiddleErr> {
        let (s, node) = self.get_scope_member_scope_path(scope, path)?;
        self.evaluate(&s, node)
    }
}

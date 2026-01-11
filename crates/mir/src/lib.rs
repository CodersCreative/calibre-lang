use std::collections::HashMap;

use calibre_parser::{
    ast::{
        IfComparisonType, LoopType, MatchArmType, Node, NodeType, ObjectMap, ObjectType, ParserDataType, ParserInnerType, ParserText, RefMutability, VarType, binary::BinaryOperator, comparison::Comparison
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
pub mod native;

impl MiddleEnvironment {
    pub fn evaluate(&mut self, scope: &u64, node: Node) -> Result<MiddleNode, MiddleErr> {
        self.current_location = self.get_location(scope, node.span);

        match node.node_type {
            NodeType::Identifier(x) => Ok(MiddleNode {
                node_type: MiddleNodeType::Identifier(if let Some(x) = self.resolve_parser_text(scope, &x){
                    x
                }else {
                    return Err(MiddleErr::Variable(x.text));
                }),
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
                if path.is_empty() {
                    return Ok(MiddleNode {
                        node_type: MiddleNodeType::EmptyLine,
                        span: node.span,
                    });
                }
                
                if let NodeType::Identifier(x) = &path[0].0.node_type {
                    if let Some(Some(object)) = self.resolve_str(scope, x).map(|x| self.objects.get(x)) {
                        match object.object_type {
                            MiddleTypeDefType::Enum(_) if path.len() == 2 => {
                                match &path[1].0.node_type {
                                    NodeType::Identifier(y) => return self.evaluate(scope, Node::new_from_type(NodeType::EnumExpression { identifier: ParserText::from(x.to_string()), value: ParserText::from(y.to_string()), data: None })),
                                    _ => {}
                                }
                                
                            },
                            _ => {}
                        }
                    }
                }
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
            NodeType::Ternary { comparison, then, otherwise } => self.evaluate(scope, Node {
                node_type: NodeType::IfStatement { comparison: Box::new(IfComparisonType::If(*comparison)), then, otherwise: Some(otherwise), special_delim: true },
                span: node.span
            }),
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
                                    return_type: self.resolve_type_from_node(scope, &then).unwrap_or(ParserDataType::from(ParserInnerType::Null)),
                                    body: {
                                        let mut lst : Vec<(calibre_parser::ast::MatchArmType, Vec<Node>, Box<Node>)> = pattern.0.clone().into_iter().map(|x| (x, pattern.1.clone(), then.clone())).collect();
                                        
                                        lst.push((
                                            MatchArmType::Wildcard(Span::default()),
                                            Vec::new(),
                                            otherwise.unwrap_or(Box::new(Node {
                                                node_type: NodeType::EmptyLine,
                                                span: Span::default(),
                                            })),
                                        ));

                                        lst
                                    },
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
                    self.resolve_data_type(scope, x)
                } else if let Some(x) = self.resolve_type_from_node(scope, &value) {
                    x
                } else {
                    ParserDataType {
                        data_type: ParserInnerType::Dynamic,
                        span: identifier.span,
                    }
                };

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


                let value = self.evaluate(scope, *value)?;

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
                        object_type: object.into(),
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
            NodeType::ListLiteral(typ, x) => {
                let typ = if let Some(typ) = typ {
                    self.resolve_data_type(scope, typ)
                } else if x.is_empty() {
                    panic!()
                } else if let Some(typ) = self.resolve_type_from_node(scope, &x[0]) {
                    typ
                } else {
                    panic!()
                };
                
                let mut lst = Vec::new();

                for item in x {
                    lst.push(self.evaluate(scope, item)?);
                }

                Ok(MiddleNode {
                    node_type: MiddleNodeType::ListLiteral(typ, lst),
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
                                        MatchArmType::Enum { var_type : VarType::Immutable, value: ParserText::from(String::from("Ok")), name: Some(ParserText::from(String::from("anon_ok_value"))) },
                                        Vec::new(),
                                        Box::new(Node {
                                            node_type: NodeType::Identifier(ParserText::from(String::from("anon_ok_value"))),
                                            span: Span::default(),
                                        }),
                                    ),
                                    (
                                        MatchArmType::Enum { var_type : VarType::Immutable, value: ParserText::from(String::from("Err")), name: Some(ParserText::from(String::from("anon_err_value"))) },
                                        Vec::new(),
                                        Box::new(Node {
                                            node_type: NodeType::Return { value: Box::new(Node::new_from_type(NodeType::CallExpression(Box::new(Node::new_from_type(NodeType::Identifier(ParserText::from(String::from("err"))))), vec![(Node::new_from_type(NodeType::Identifier(ParserText::from(String::from("anon_err_value")))), None)]))) },
                                            span: Span::default(),
                                        }),
                                    ),
                                ],
                                return_type:  match self.resolve_type_from_node(scope, &value) {
                                    Some(ParserDataType { data_type: ParserInnerType::Result { ok: x, err: _ }, span: _ }) | Some(ParserDataType { data_type: ParserInnerType::Option(x), span: _ }) => *x,
                                    Some(x) => x,
                                    _ => ParserDataType::from(ParserInnerType::Null),
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
            NodeType::LoopDeclaration { loop_type, body } => {
                let scope = self.new_scope_from_parent_shallow(*scope);
                match *loop_type {
                LoopType::Loop => Ok(MiddleNode {
                    node_type: MiddleNodeType::LoopDeclaration {
                        state: None,
                        body: Box::new(self.evaluate(
                            &scope,
                            *body,
                        )?),
                    },
                    span: node.span,
                }),
                LoopType::While(condition) => Ok(MiddleNode {
                    node_type: MiddleNodeType::LoopDeclaration {
                        state: None,
                        body: Box::new(self.evaluate(
                            &scope,
                            Node::new_from_type(NodeType::ScopeDeclaration {
                                body: {
                                    let mut lst = vec![
                                        match self.resolve_type_from_node(&scope, &condition) {
                                            Some(x) if x.data_type != ParserInnerType::Bool && x.data_type != ParserInnerType::Dynamic => return self.evaluate(&scope, Node{
                                                node_type: NodeType::LoopDeclaration { loop_type: Box::new(LoopType::For(ParserText::from("anon_loop_counter".to_string()), condition)), body },
                                                span: node.span,
                                            }),
                                            _ => Node::new_from_type(NodeType::IfStatement {
                                                comparison: Box::new(IfComparisonType::If(
                                                    Node::new_from_type(NodeType::NotExpression {
                                                        value: Box::new(condition),
                                                    }),
                                                )),
                                                then: Box::new(Node::new_from_type(
                                                    NodeType::Break,
                                                )),
                                                otherwise: None,
                                                special_delim: true,
                                            })
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
                LoopType::Let { value, pattern } => Ok(MiddleNode {
                    node_type: MiddleNodeType::LoopDeclaration {
                        state: None,
                        body: Box::new(self.evaluate(
                            &scope,
                            Node::new_from_type(NodeType::IfStatement {
                                comparison: Box::new(IfComparisonType::IfLet { value, pattern }),
                                then: body,
                                otherwise: Some(Box::new(Node::new_from_type(
                                    NodeType::Break,
                                ))),
                                special_delim: true,
                            }),
                        )?),
                    },
                    span: node.span,
                }),                
                LoopType::For(name, range) => Ok(MiddleNode {
                    node_type: MiddleNodeType::LoopDeclaration {
                        state: Some(Box::new(self.evaluate(&scope, Node::new_from_type(NodeType::VariableDeclaration { var_type: VarType::Mutable, identifier: ParserText::from("anon_loop_index".to_string()), value: Box::new(Node::new_from_type(NodeType::IntLiteral(0))), data_type: Some(ParserDataType::from(ParserInnerType::Int)) }))?)),
                        body: Box::new(self.evaluate(
                            &scope,
                            Node::new_from_type(NodeType::ScopeDeclaration {
                                body: {
                                    let range_data_type = self.resolve_type_from_node(&scope, &range);
                                    let mut lst = vec![
                                        Node::new_from_type(NodeType::VariableDeclaration  { identifier: name.clone(), var_type: VarType::Constant, data_type: None, value: match range_data_type.map(|x| x.data_type) {
                                            Some(ParserInnerType::List(_)) => Box::new(Node::new_from_type(NodeType::MemberExpression { path: vec![(range.clone(), false), (Node::new_from_type(NodeType::Identifier(ParserText::from("anon_loop_index".to_string()))), true)] })),
                                            Some(ParserInnerType::Tuple(_)) => Box::new(Node::new_from_type(NodeType::MemberExpression { path: vec![(range.clone(), false), (Node::new_from_type(NodeType::Identifier(ParserText::from("anon_loop_index".to_string()))), false)] })),
                                            _ => Box::new(Node::new_from_type(NodeType::Identifier(ParserText::from("anon_loop_index".to_string())))),
                                        } }),
                                        Node::new_from_type(NodeType::IfStatement {
                                                comparison: Box::new(IfComparisonType::If(
                                                    Node::new_from_type(NodeType::ComparisonExpression {
                                                        left: Box::new(Node::new_from_type(NodeType::Identifier(ParserText::from("anon_loop_index".to_string())))),
                                                        right: Box::new(Node::new_from_type(NodeType::CallExpression(Box::new(Node::new_from_type(NodeType::Identifier(ParserText::from("len".to_string())))), vec![(range, None)]))),
                                                        operator: calibre_parser::ast::comparison::Comparison::Greater
                                                    }),
                                                )),
                                                then: Box::new(Node::new_from_type(
                                                    NodeType::AssignmentExpression { identifier: Box::new(Node::new_from_type(NodeType::Identifier(ParserText::from("anon_loop_index".to_string())))), value: Box::new(Node::new_from_type(NodeType::BinaryExpression { left: Box::new(Node::new_from_type(NodeType::Identifier(ParserText::from("anon_loop_index".to_string())))), right: Box::new(Node::new_from_type(NodeType::IntLiteral(1))), operator: BinaryOperator::Add })) },
                                                )),
                                                otherwise: Some(Box::new(Node::new_from_type(
                                                    NodeType::Break,
                                                ))),
                                                special_delim: false,
                                            })
                                    ];

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
            }},
            NodeType::IterExpression {
                data_type,
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
                                    data_type,
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
            NodeType::AssignmentExpression { identifier, value } => {
                match identifier.node_type.clone().unwrap() {
                    NodeType::Ternary { comparison, then, otherwise } => self.evaluate(scope, Node {
                        node_type: NodeType::IfStatement {
                            comparison : Box::new(IfComparisonType::If(*comparison)),
                            then: Box::new(Node::new_from_type(NodeType::AssignmentExpression { identifier: then, value: value.clone() })),
                            otherwise: Some(Box::new(Node::new_from_type(NodeType::AssignmentExpression { identifier: otherwise, value }))),
                            special_delim: false
                        },
                        span: node.span
                    }),
                    _ => Ok(MiddleNode {
                        node_type: MiddleNodeType::AssignmentExpression {
                            identifier: Box::new(self.evaluate(scope, *identifier)?),
                            value: Box::new(self.evaluate(scope, *value)?),
                        },
                        span: node.span,
                    })
                }
            },
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
                let mut stmts = Vec::new();

                if is_temp {
                    let scope = self.new_scope_from_parent_shallow(*scope);

                    for statement in body.into_iter() {
                        stmts.push(self.evaluate(&scope, statement)?);
                    }
                } else {
                    let mut errored = Vec::new();

                    for statement in body.into_iter() {
                        if let Ok(x) = self.evaluate(scope, statement.clone()) {
                            stmts.push(x);
                        }else {
                            errored.push(statement);
                        }
                    }

                    let mut last_len = 0;
                    while !errored.is_empty() && last_len != errored.len() {
                        last_len = errored.len();

                        for elem in errored.clone() {
                            if let Ok(x) = self.evaluate(scope, elem.clone()) {
                                stmts.push(x);
                                errored.retain(|x| x != &elem);
                            }
                        }
                    }

                    if !errored.is_empty() {
                        let _ = self.evaluate(scope, errored[0].clone())?;
                    }
                }

                Ok(MiddleNode {
                    node_type: MiddleNodeType::ScopeDeclaration {
                        body: stmts.into_iter().filter(|x| x.node_type != MiddleNodeType::EmptyLine).collect(),
                        is_temp,
                    },
                    span: node.span,
                })
            }
            NodeType::StructLiteral { identifier, value } =>  Ok(MiddleNode {
                node_type: MiddleNodeType::AggregateExpression{
                    identifier : Some(self.resolve_parser_text(scope, &identifier).unwrap()),
                    value : ObjectMap(match value {
                        ObjectType::Map(x) => {
                            let mut map = HashMap::new();

                            for itm in x {
                                map.insert(
                                    itm.0,
                                    self.evaluate(scope, itm.1)?,
                                );
                            }

                            map
                        }
                        ObjectType::Tuple(x) => {
                            let mut map = HashMap::new();

                            for itm in x.into_iter().enumerate() {
                                map.insert(
                                    itm.0.to_string(),
                                    self.evaluate(scope, itm.1)?
                                );
                            }

                            map
                        }
                    })
                },
                span: node.span,
            }),
            NodeType::EnumExpression {
                identifier,
                value,
                data,
            } => {
                let identifier = if let Some(x) = self.resolve_parser_text(scope, &identifier) {
                    x
                }else {
                    return Err(MiddleErr::Object(identifier.text))
                };
                Ok(MiddleNode {
                node_type: MiddleNodeType::EnumExpression {
                    identifier: identifier.clone(),
                    value,
                    data: if let Some(data) = data {
                        Some(Box::new(self.evaluate(scope, *data)?))
                    } else {
                        None
                    },
                },
                span: node.span,
            })
            },
            NodeType::MatchDeclaration { parameters, body, return_type, is_async } => self.evaluate(scope, Node::new_from_type(NodeType::FunctionDeclaration{
                    parameters: vec![(parameters.0.clone(), parameters.1.clone(), parameters.2.map(|x| *x))],
                    return_type,
                    is_async,
                    body: {
                        let mut lst = Vec::new();
                        let resolved_data_type = self.resolve_data_type(scope, parameters.1.clone()).data_type;
                        let mut reference = None;
                        let enum_object = if let Some(x) = self.objects.get(resolved_data_type.to_string().replace("mut ", "").replace("&", "").trim()) {
                            match (resolved_data_type.to_string().contains("mut "), resolved_data_type.to_string().contains("&")) {
                                (true, true) => reference = Some(RefMutability::MutRef),
                                (true, false) => reference = Some(RefMutability::MutValue),
                                (false, true) => reference = Some(RefMutability::Ref),
                                (false, false) => reference = Some(RefMutability::Value),
                            }
                            match &x.object_type {
                                MiddleTypeDefType::Enum(x) => Some(x),
                                _ => None
                            }
                        }else{
                            None
                        };
                            
                        for pattern in body {
                            let main = if pattern.1.is_empty() {
                                Node::new_from_type(NodeType::Return { value: pattern.2 })
                            }else {
                                todo!()  
                            };
                            
                            match pattern.0 {
                                MatchArmType::Wildcard(_) => lst.push(main),
                                MatchArmType::Value(x) => lst.push(Node::new_from_type(NodeType::IfStatement {
                                    comparison: Box::new(IfComparisonType::If(Node::new_from_type(NodeType::ComparisonExpression { left: Box::new(Node::new_from_type(NodeType::Identifier(parameters.0.clone()))), right: Box::new(x), operator: Comparison::Equal }))),
                                    then: Box::new(main),
                                    otherwise: None,
                                    special_delim: true
                                })),
                                MatchArmType::Let { var_type, name } => {
                                    lst.push(Node::new_from_type(NodeType::ScopeDeclaration {
                                        body: vec![
                                            Node::new_from_type(NodeType::VariableDeclaration {
                                                var_type,
                                                identifier: name,
                                                value: Box::new(Node::new_from_type(NodeType::Identifier(parameters.0.clone()))),
                                                data_type: Some(parameters.1.clone())
                                            }),
                                            main,
                                        ],
                                        is_temp: true
                                    }));
                                }
                                MatchArmType::Enum { value, var_type, name }  => {
                                    let index : i64 = match value.text.trim() {
                                        _ if enum_object.is_some() => {
                                            let Some(object) = enum_object else {return Err(MiddleErr::CantMatch(parameters.1));};
                                            let Some(index) = object.iter().position(|x| x.0.text == value.text) else {return Err(MiddleErr::EnumVariant(value.text));};
                                            index as i64
                                        }
                                        "Ok" => 0,
                                        "Err" => 1,
                                        "Some" => 0,
                                        "None" => 1,
                                        _ => return Err(MiddleErr::CantMatch(parameters.1)),  
                                    };

                                    lst.push(Node::new_from_type(NodeType::IfStatement {
                                        comparison: Box::new(IfComparisonType::If(Node::new_from_type(NodeType::ComparisonExpression {
                                            left: Box::new(Node::new_from_type(NodeType::CallExpression(Box::new(Node::new_from_type(NodeType::Identifier(ParserText::from(String::from("discriminant"))))), vec![(Node::new_from_type(NodeType::Identifier(parameters.0.clone())), None)]))),
                                            right: Box::new(Node::new_from_type(NodeType::IntLiteral(index))),
                                            operator: Comparison::Equal
                                        }))),
                                        then: {Box::new(if let Some(name) = name {
                                            Node::new_from_type(NodeType::ScopeDeclaration {
                                                body: vec![
                                                    Node::new_from_type(NodeType::VariableDeclaration {
                                                        var_type,
                                                        identifier: name,
                                                        value: if reference.is_some() && reference != Some(RefMutability::Value) {
                                                            Box::new(Node::new_from_type(NodeType::RefStatement {
                                                                mutability: reference.clone().unwrap(),
                                                                value: Box::new(Node::new_from_type(NodeType::MemberExpression {
                                                                    path: vec![
                                                                        (Node::new_from_type(NodeType::Identifier(parameters.0.clone())), false),
                                                                        (Node::new_from_type(NodeType::Identifier(ParserText::from(String::from("next")))), false)
                                                                    ]
                                                                }))
                                                            }))
                                                        }else{
                                                            Box::new(Node::new_from_type(NodeType::MemberExpression {
                                                                path: vec![
                                                                    (Node::new_from_type(NodeType::Identifier(parameters.0.clone())), false),
                                                                    (Node::new_from_type(NodeType::Identifier(ParserText::from(String::from("next")))), false)
                                                                ]
                                                            }))
                                                        },
                                                        data_type: None
                                                    }),
                                                    main,
                                                ],
                                                is_temp: true
                                            })}else{
                                              main  
                                            })
                                        },
                                        otherwise: None,
                                        special_delim: true
                                    }));
                                }
                                
                            }    
                        }

                        
                        Box::new(Node::new_from_type(NodeType::ScopeDeclaration { body: lst, is_temp: true }))
                    },
                })),
            NodeType::FunctionDeclaration { parameters, body, return_type, is_async } => {
                let mut params = Vec::with_capacity(parameters.len());
                let new_scope = self.new_scope_from_parent_shallow(*scope);
                for param in parameters {
                    let new_name = get_disamubiguous_name(scope, Some(param.0.trim()), Some(&VarType::Mutable));
                    let data_type = self.resolve_data_type(scope, param.1);
                    self.variables.insert(
                        new_name.clone(),
                        MiddleVariable {
                            data_type: data_type.clone(),
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
                        ParserText::from(new_name),
                        data_type,
                        if let Some(x) = param.2 {
                            Some(self.evaluate(scope, x)?)
                        }else{
                            None
                        }
                    ));
                }


                Ok(MiddleNode {
                    node_type: MiddleNodeType::FunctionDeclaration { parameters: params, body: Box::new(self.evaluate(&new_scope, *body)?), return_type, is_async },
                    span: node.span,
                })
            },
            NodeType::CallExpression(caller, args) => {
                if let NodeType::Identifier(caller) = &caller.node_type {
                    if "tuple" == &caller.text {
                        let mut map = HashMap::new();

                        for (i, arg) in args.into_iter().enumerate() {
                            map.insert(i.to_string(), self.evaluate(scope, arg.0)?);
                        }

                        return Ok(MiddleNode {
                            node_type: MiddleNodeType::AggregateExpression {identifier: None, value: map.into() },
                            span: node.span,
                        });
                    }

                    if let Some(caller) = self.resolve_str(scope, caller).map(|x| x.clone()) {
                        if self.objects.contains_key(&caller) {
                            let mut map = HashMap::new();

                            for (i, arg) in args.into_iter().enumerate() {
                                map.insert(i.to_string(), self.evaluate(scope, arg.0)?);
                            }

                            return Ok(MiddleNode {
                                node_type: MiddleNodeType::AggregateExpression {identifier: Some(ParserText::from(caller)), value: map.into() },
                                span: node.span,
                            });
                        }
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

use calibre_mir_ty::{MiddleNode, MiddleNodeType};
use calibre_parser::{
    ast::{
        IfComparisonType, LoopType, MatchArmType, Node, NodeType, ObjectMap, ObjectType,
        ParserDataType, ParserInnerType, ParserText, PotentialDollarIdentifier, RefMutability,
        VarType,
        binary::BinaryOperator,
        comparison::{BooleanOperation, Comparison},
    },
    lexer::Span,
};
use std::collections::HashMap;

use crate::errors::MiddleErr;
use environment::*;

pub mod environment;
pub mod errors;
pub mod native;

impl MiddleEnvironment {
    pub fn evaluate(&mut self, scope: &u64, node: Node) -> Result<MiddleNode, MiddleErr> {
        self.current_location = self.get_location(scope, node.span);

        match node.node_type {
            NodeType::Identifier(x) => Ok(MiddleNode {
                node_type: MiddleNodeType::Identifier(
                    if let Some(x) = self.resolve_potential_dollar_ident(scope, &x) {
                        x
                    } else if let PotentialDollarIdentifier::DollarIdentifier(x) = x {
                        let val = self.resolve_macro_arg(scope, &x).unwrap().clone();
                        return self.evaluate(scope, val);
                    } else {
                        // x.to_string().into()
                        return Err(MiddleErr::Variable(x.to_string()));
                    },
                ),
                span: node.span,
            }),
            NodeType::Comp { stage, body } => Ok(MiddleNode {
                node_type: MiddleNodeType::Comp {
                    stage,
                    body: Box::new(self.evaluate(scope, *body)?),
                },
                span: node.span,
            }),
            NodeType::DataType { data_type } => Ok(MiddleNode {
                node_type: MiddleNodeType::DataType {
                    data_type: self.resolve_potential_new_type(scope, data_type),
                },
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
                    if let Some(Some(object)) = self
                        .resolve_potential_dollar_ident(scope, x)
                        .map(|x| self.objects.get(&x.text))
                    {
                        match object.object_type {
                            MiddleTypeDefType::Enum(_) if path.len() == 2 => {
                                match &path[1].0.node_type {
                                    NodeType::Identifier(y) => {
                                        return self.evaluate(
                                            scope,
                                            Node::new_from_type(NodeType::EnumExpression {
                                                identifier: x.clone(),
                                                value: y.clone(),
                                                data: None,
                                            }),
                                        );
                                    }
                                    _ => {}
                                }
                            }
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
                                node_type: MiddleNodeType::Identifier(
                                    self.resolve_dollar_ident_only(scope, &x).unwrap(),
                                ),
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
            NodeType::Ternary {
                comparison,
                then,
                otherwise,
            } => self.evaluate(
                scope,
                Node {
                    node_type: NodeType::IfStatement {
                        comparison: Box::new(IfComparisonType::If(*comparison)),
                        then,
                        otherwise: Some(otherwise),
                    },
                    span: node.span,
                },
            ),
            NodeType::IfStatement {
                comparison,
                then,
                otherwise,
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
                IfComparisonType::IfLet { value, pattern } => self.evaluate(
                    scope,
                    Node {
                        node_type: NodeType::MatchStatement {
                            value: Box::new(value),
                            body: {
                                let mut lst: Vec<(
                                    calibre_parser::ast::MatchArmType,
                                    Vec<Node>,
                                    Box<Node>,
                                )> = pattern
                                    .0
                                    .clone()
                                    .into_iter()
                                    .map(|x| (x, pattern.1.clone(), then.clone()))
                                    .collect();

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
                        },
                        span: node.span,
                    },
                ),
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
                    value: if let Some(value) = value {
                        Some(Box::new(self.evaluate(scope, *value)?))
                    } else {
                        None
                    },
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
                let identifier = self.resolve_dollar_ident_only(scope, &identifier).unwrap();
                let new_name =
                    get_disamubiguous_name(scope, Some(identifier.text.trim()), Some(&var_type));

                let data_type = if let Some(x) = data_type {
                    self.resolve_potential_new_type(scope, x)
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
            NodeType::TypeDeclaration {
                identifier,
                object,
                overloads,
            } => {
                let identifier = self.resolve_dollar_ident_only(scope, &identifier).unwrap();
                let object = self.type_def_type_into(scope, object);
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
            NodeType::AsExpression { value, data_type } => Ok(MiddleNode {
                node_type: MiddleNodeType::AsExpression {
                    value: Box::new(self.evaluate(scope, *value)?),
                    data_type: self.resolve_potential_new_type(scope, data_type),
                },
                span: node.span,
            }),
            NodeType::IsDeclaration { value, data_type } => Ok(MiddleNode {
                node_type: MiddleNodeType::IsDeclaration {
                    value: Box::new(self.evaluate(scope, *value)?),
                    data_type: self.resolve_potential_new_type(scope, data_type),
                },
                span: node.span,
            }),
            NodeType::InDeclaration { identifier, value } => Ok(MiddleNode {
                node_type: MiddleNodeType::InDeclaration {
                    identifier: Box::new(self.evaluate(scope, *identifier)?),
                    value: Box::new(self.evaluate(scope, *value)?),
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
                    self.resolve_potential_new_type(scope, typ)
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
            NodeType::Try { value, catch } => {
                let resolved_type = self.resolve_type_from_node(scope, &value);

                self.evaluate(
                    scope,
                    Node {
                        node_type: NodeType::MatchStatement  {
                            value,
                            body: match resolved_type {
                                Some(ParserDataType { data_type: ParserInnerType::Option(_), span: _ }) => vec![
                                    (
                                        MatchArmType::Enum { var_type : VarType::Immutable, value: ParserText::from(String::from("Some")).into(), name: Some(ParserText::from(String::from("anon_ok_value")).into()) },
                                        Vec::new(),
                                        Box::new(Node {
                                            node_type: NodeType::Identifier(ParserText::from(String::from("anon_ok_value")).into()),
                                            span: Span::default(),
                                        }),
                                    ),
                                    if let Some(catch) = catch {
                                        (
                                            MatchArmType::Enum { var_type : VarType::Immutable, value: ParserText::from(String::from("None")).into(), name: catch.name },
                                            Vec::new(),
                                            catch.body,
                                        )
                                    }else {
                                        (
                                            MatchArmType::Enum { var_type : VarType::Immutable, value: ParserText::from(String::from("None")).into(), name: None },
                                            Vec::new(),
                                            Box::new(Node {
                                                node_type: NodeType::Return { value: Some(Box::new(Node::new_from_type(NodeType::CallExpression(Box::new(Node::new_from_type(NodeType::Identifier(ParserText::from(String::from("none")).into()))), Vec::new())))) },
                                                span: Span::default(),
                                            }),
                                        )
                                    },
                                ],
                                _ => vec![
                                    (
                                        MatchArmType::Enum { var_type : VarType::Immutable, value: ParserText::from(String::from("Ok")).into(), name: Some(ParserText::from(String::from("anon_ok_value")).into()) },
                                        Vec::new(),
                                        Box::new(Node {
                                            node_type: NodeType::Identifier(ParserText::from(String::from("anon_ok_value")).into()),
                                            span: Span::default(),
                                        }),
                                    ),
                                    if let Some(catch) = catch {
                                        (
                                            MatchArmType::Enum { var_type : VarType::Immutable, value: ParserText::from(String::from("Err")).into(), name: catch.name },
                                            Vec::new(),
                                            catch.body,
                                        )
                                    }else {
                                        (
                                            MatchArmType::Enum { var_type : VarType::Immutable, value: ParserText::from(String::from("Err")).into(), name: Some(ParserText::from(String::from("anon_err_value")).into()) },
                                            Vec::new(),
                                            Box::new(Node {
                                                node_type: NodeType::Return { value: Some(Box::new(Node::new_from_type(NodeType::CallExpression(Box::new(Node::new_from_type(NodeType::Identifier(ParserText::from(String::from("err")).into()))), vec![(Node::new_from_type(NodeType::Identifier(ParserText::from(String::from("anon_err_value")).into())), None)])))) },
                                                span: Span::default(),
                                            }),
                                        )
                                    },
                                ]
                            },
                        },
                        span: node.span,
                    },
                )
            }
            NodeType::LoopDeclaration { loop_type, body } => {
                let scope = self.new_scope_from_parent_shallow(*scope);
                match *loop_type {
                    LoopType::Loop => Ok(MiddleNode {
                        node_type: MiddleNodeType::LoopDeclaration {
                            state: None,
                            body: Box::new(self.evaluate(&scope, *body)?),
                        },
                        span: node.span,
                    }),
                    LoopType::While(condition) => {
                        let body = {
                            let mut lst =
                                vec![match self.resolve_type_from_node(&scope, &condition) {
                                    Some(x)
                                        if x.data_type != ParserInnerType::Bool
                                            && x.data_type != ParserInnerType::Dynamic =>
                                    {
                                        return self.evaluate(
                                            &scope,
                                            Node {
                                                node_type: NodeType::LoopDeclaration {
                                                    loop_type: Box::new(LoopType::For(
                                                        ParserText::from(
                                                            "anon_loop_counter".to_string(),
                                                        )
                                                        .into(),
                                                        condition,
                                                    )),
                                                    body,
                                                },
                                                span: node.span,
                                            },
                                        );
                                    }
                                    _ => Node::new_from_type(NodeType::IfStatement {
                                        comparison: Box::new(IfComparisonType::If(
                                            Node::new_from_type(NodeType::NotExpression {
                                                value: Box::new(condition),
                                            }),
                                        )),
                                        then: Box::new(Node::new_from_type(NodeType::Break)),
                                        otherwise: None,
                                    }),
                                }];

                            match body.node_type {
                                NodeType::ScopeDeclaration {
                                    body: Some(mut body),
                                    ..
                                } => lst.append(&mut body),
                                _ => lst.push(*body),
                            }

                            Some(lst)
                        };

                        Ok(MiddleNode {
                            node_type: MiddleNodeType::LoopDeclaration {
                                state: None,
                                body: Box::new(self.evaluate(
                                    &scope,
                                    Node::new_from_type(NodeType::ScopeDeclaration {
                                        body,
                                        is_temp: true,
                                        create_new_scope: Some(true),
                                        define: false,
                                        named: None,
                                    }),
                                )?),
                            },
                            span: node.span,
                        })
                    }
                    LoopType::Let { value, pattern } => Ok(MiddleNode {
                        node_type: MiddleNodeType::LoopDeclaration {
                            state: None,
                            body: Box::new(self.evaluate(
                                &scope,
                                Node::new_from_type(NodeType::IfStatement {
                                    comparison: Box::new(IfComparisonType::IfLet {
                                        value,
                                        pattern,
                                    }),
                                    then: body,
                                    otherwise: Some(Box::new(Node::new_from_type(NodeType::Break))),
                                }),
                            )?),
                        },
                        span: node.span,
                    }),
                    LoopType::For(name, range) => {
                        let range_data_type = self.resolve_type_from_node(&scope, &range);

                        Ok(MiddleNode {
                    node_type: MiddleNodeType::LoopDeclaration {
                        state: Some(Box::new(self.evaluate(&scope, Node::new_from_type(NodeType::VariableDeclaration { var_type: VarType::Mutable, identifier: ParserText::from("anon_loop_index".to_string()).into(), value: Box::new(Node::new_from_type(NodeType::IntLiteral(0))), data_type: Some(ParserDataType::from(ParserInnerType::Int).into()) }))?)),
                        body: Box::new(self.evaluate(
                            &scope,
                            Node::new_from_type(NodeType::ScopeDeclaration {
                                body: {
                                    let mut lst = vec![
                                        Node::new_from_type(NodeType::VariableDeclaration  { identifier: name.clone(), var_type: VarType::Constant, data_type: None, value: match range_data_type.map(|x| x.data_type) {
                                            Some(ParserInnerType::List(_)) => Box::new(Node::new_from_type(NodeType::MemberExpression { path: vec![(range.clone(), false), (Node::new_from_type(NodeType::Identifier(ParserText::from("anon_loop_index".to_string()).into())), true)] })),
                                            Some(ParserInnerType::Tuple(_)) => Box::new(Node::new_from_type(NodeType::MemberExpression { path: vec![(range.clone(), false), (Node::new_from_type(NodeType::Identifier(ParserText::from("anon_loop_index".to_string()).into())), false)] })),
                                            _ => Box::new(Node::new_from_type(NodeType::Identifier(ParserText::from("anon_loop_index".to_string()).into()))),
                                        } }),
                                        Node::new_from_type(NodeType::IfStatement {
                                                comparison: Box::new(IfComparisonType::If(
                                                    Node::new_from_type(NodeType::ComparisonExpression {
                                                        left: Box::new(Node::new_from_type(NodeType::Identifier(ParserText::from("anon_loop_index".to_string()).into()))),
                                                        right: Box::new(Node::new_from_type(NodeType::CallExpression(Box::new(Node::new_from_type(NodeType::Identifier(ParserText::from("len".to_string()).into()))), vec![(range, None)]))),
                                                        operator: calibre_parser::ast::comparison::Comparison::Greater
                                                    }),
                                                )),
                                                then: Box::new(Node::new_from_type(
                                                    NodeType::AssignmentExpression { identifier: Box::new(Node::new_from_type(NodeType::Identifier(ParserText::from("anon_loop_index".to_string()).into()))), value: Box::new(Node::new_from_type(NodeType::BinaryExpression { left: Box::new(Node::new_from_type(NodeType::Identifier(ParserText::from("anon_loop_index".to_string()).into()))), right: Box::new(Node::new_from_type(NodeType::IntLiteral(1))), operator: BinaryOperator::Add })) },
                                                )),
                                                otherwise: Some(Box::new(Node::new_from_type(
                                                    NodeType::Break,
                                                ))),
                                            })
                                    ];

                                    match body.node_type {
                                        NodeType::ScopeDeclaration { body : Some(mut body), .. } => lst.append(&mut body),
                                        _ => lst.push(*body),
                                    }

                                    Some(lst)
                                },
                                is_temp: true,
                                create_new_scope: Some(true),
                                define : false,
                                named: None,
                            }),
                        )?),
                    },
                    span: node.span,
                })
                    }
                }
            }
            NodeType::IterExpression {
                data_type,
                map,
                loop_type,
                conditionals,
            } => {
                let resolved_data_type = if let Some(data_type) = data_type.clone() {
                    Some(self.resolve_potential_new_type(scope, data_type))
                } else {
                    self.resolve_type_from_node(scope, &map)
                };
                self.evaluate(
                    scope,
                    Node {
                        node_type: NodeType::ScopeDeclaration {
                            body: Some(vec![
                                Node::new_from_type(NodeType::VariableDeclaration {
                                    var_type: VarType::Mutable,
                                    identifier: ParserText::from(String::from("anon_iter_list")).into(),
                                    value: Box::new(Node::new_from_type(NodeType::ListLiteral(
                                        data_type.clone(),
                                        Vec::new(),
                                    ))),
                                    data_type: if let Some(typ) = resolved_data_type {
                                        Some(calibre_mir_ty::middle_data_type_to_new_type(ParserDataType::from(ParserInnerType::List(Box::new(typ)))))
                                    }else{
                                        None
                                    },
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
                                                }));
                                            }

                                            lst.push(Node::new_from_type(
                                                NodeType::AssignmentExpression {
                                                    identifier: Box::new(Node::new_from_type(
                                                        NodeType::Identifier(ParserText::from(
                                                            String::from("anon_iter_list"),
                                                        ).into()),
                                                    )),
                                                    value: Box::new(Node::new_from_type(NodeType::BinaryExpression { left: Box::new(Node::new_from_type(
                                                        NodeType::Identifier(ParserText::from(
                                                            String::from("anon_iter_list"),
                                                        ).into()),
                                                    )), right: map, operator: calibre_parser::ast::binary::BinaryOperator::Shl })),
                                                },
                                            ));

                                            Some(lst)
                                        },
                                        create_new_scope: Some(true),
                                        define : false,
                                        named: None,
                                        is_temp: true,
                                    })),
                                }),
                                Node::new_from_type(NodeType::Identifier(ParserText::from(
                                    String::from("anon_iter_list"),
                                ).into())),
                            ]),
                            create_new_scope: Some(true),
                            define : false,
                            named: None,
                            is_temp: true,
                        },
                        span: node.span,
                    },
                )
            }
            NodeType::AssignmentExpression { identifier, value } => {
                match identifier.node_type.clone().unwrap() {
                    NodeType::Ternary {
                        comparison,
                        then,
                        otherwise,
                    } => self.evaluate(
                        scope,
                        Node {
                            node_type: NodeType::IfStatement {
                                comparison: Box::new(IfComparisonType::If(*comparison)),
                                then: Box::new(Node::new_from_type(
                                    NodeType::AssignmentExpression {
                                        identifier: then,
                                        value: value.clone(),
                                    },
                                )),
                                otherwise: Some(Box::new(Node::new_from_type(
                                    NodeType::AssignmentExpression {
                                        identifier: otherwise,
                                        value,
                                    },
                                ))),
                            },
                            span: node.span,
                        },
                    ),
                    _ => Ok(MiddleNode {
                        node_type: MiddleNodeType::AssignmentExpression {
                            identifier: Box::new(self.evaluate(scope, *identifier)?),
                            value: Box::new(self.evaluate(scope, *value)?),
                        },
                        span: node.span,
                    }),
                }
            }
            NodeType::ImplDeclaration {
                identifier,
                functions,
            } => {
                let resolved = self
                    .resolve_potential_dollar_ident(scope, &identifier)
                    .unwrap();
                for function in functions {
                    if let NodeType::VariableDeclaration {
                        identifier: iden,
                        value,
                        ..
                    } = function.node_type
                    {
                        let func = self.evaluate(scope, *value)?;
                        let location = self.get_location(scope, function.span);
                        let mut dependant = false;

                        if let MiddleNodeType::FunctionDeclaration { parameters, .. } =
                            &func.node_type
                        {
                            if parameters.len() > 0 {
                                let new_data_type = self.resolve_data_type(
                                    scope,
                                    calibre_mir_ty::middle_data_type_to_node(
                                        parameters[0].1.clone(),
                                    ),
                                );

                                if let ParserInnerType::Struct(obj) = new_data_type.data_type {
                                    if obj == resolved.text {
                                        dependant = true;
                                    }
                                }
                            }
                        };

                        self.objects
                            .get_mut(&resolved.text)
                            .unwrap()
                            .functions
                            .insert(iden.to_string(), (func, location, dependant));
                    }
                }

                Ok(MiddleNode {
                    node_type: MiddleNodeType::EmptyLine,
                    span: node.span,
                })
            }
            NodeType::ScopeAlias {
                identifier,
                value,
                create_new_scope,
            } => {
                let identifer = self
                    .resolve_dollar_ident_only(scope, &identifier)
                    .unwrap()
                    .text;
                let name = self
                    .resolve_dollar_ident_only(scope, &value.name)
                    .unwrap()
                    .text;

                let scope_macro = self.resolve_macro(scope, &name).unwrap().clone();
                let mut args = Vec::new();

                let mut added = Vec::new();

                for arg in value.args {
                    let arg_text = self.resolve_dollar_ident_only(scope, &arg.0).unwrap();
                    added.push(arg_text.text.clone());
                    args.push(arg);
                }

                for arg in scope_macro.args {
                    let arg_text = self.resolve_dollar_ident_only(scope, &arg.0).unwrap();
                    if !added.contains(&arg_text) {
                        added.push(arg_text.text.clone());
                        args.push(arg);
                    }
                }

                let scope_macro = ScopeMacro {
                    name: name.clone(),
                    args,
                    create_new_scope: create_new_scope.unwrap_or(scope_macro.create_new_scope),
                    ..scope_macro
                };

                self.scopes
                    .get_mut(scope)
                    .unwrap()
                    .macros
                    .insert(identifer, scope_macro);

                Ok(MiddleNode {
                    node_type: MiddleNodeType::EmptyLine,
                    span: node.span,
                })
            }
            NodeType::ScopeDeclaration {
                mut body,
                named,
                is_temp,
                create_new_scope,
                define,
            } => {
                let mut stmts = Vec::new();
                let mut og_create_new_scope = create_new_scope.clone();
                let create_new_scope = create_new_scope.unwrap_or(true) || named.is_some();

                let new_scope = if create_new_scope && !define {
                    self.new_scope_from_parent_shallow(*scope)
                } else {
                    *scope
                };

                if let (Some(named), false) =
                    (named.clone(), body.clone().unwrap_or_default().is_empty())
                {
                    let body = body.clone().unwrap();
                    let name = self
                        .resolve_dollar_ident_only(scope, &named.name)
                        .unwrap()
                        .text;

                    let scope_macro = ScopeMacro {
                        name: name.clone(),
                        args: named.args.clone(),
                        body,
                        create_new_scope: og_create_new_scope.unwrap(),
                    };

                    self.scopes
                        .get_mut(scope)
                        .unwrap()
                        .macros
                        .insert(name, scope_macro);

                    if define {
                        return Ok(MiddleNode {
                            node_type: MiddleNodeType::EmptyLine,
                            span: node.span,
                        });
                    } else {
                        for arg in named.args.clone() {
                            let arg_text = self.resolve_dollar_ident_only(scope, &arg.0).unwrap();
                            self.scopes
                                .get_mut(&new_scope)
                                .unwrap()
                                .macro_args
                                .insert(arg_text.text, arg.1);
                        }
                    }
                } else if let Some(named) = named {
                    let name = self
                        .resolve_dollar_ident_only(scope, &named.name)
                        .unwrap()
                        .text;
                    let mut added = Vec::new();

                    let scope_macro_args: Vec<(PotentialDollarIdentifier, Node)> = {
                        let scope_macro = self.resolve_macro(scope, &name).unwrap();
                        if og_create_new_scope.is_none() {
                            og_create_new_scope = Some(scope_macro.create_new_scope);
                        }
                        body = Some(scope_macro.body.clone());
                        scope_macro.args.clone()
                    };

                    for arg in named.args {
                        let arg_text = self.resolve_dollar_ident_only(scope, &arg.0).unwrap();
                        added.push(arg_text.text.clone());
                        self.scopes
                            .get_mut(&new_scope)
                            .unwrap()
                            .macro_args
                            .insert(arg_text.text, arg.1);
                    }

                    for arg in scope_macro_args {
                        let arg_text = self.resolve_dollar_ident_only(scope, &arg.0).unwrap();
                        if !added.contains(&arg_text) {
                            added.push(arg_text.text.clone());
                            self.scopes
                                .get_mut(&new_scope)
                                .unwrap()
                                .macro_args
                                .insert(arg_text.text, arg.1);
                        }
                    }
                }

                if let Some(body) = body {
                    if is_temp {
                        for statement in body.into_iter() {
                            stmts.push(self.evaluate(&new_scope, statement)?);
                        }
                    } else {
                        let mut errored = Vec::new();

                        for statement in body.into_iter() {
                            if let Ok(x) = self.evaluate(&new_scope, statement.clone()) {
                                stmts.push(x);
                            } else {
                                errored.push(statement);
                            }
                        }

                        let mut last_len = 0;
                        while !errored.is_empty() && last_len != errored.len() {
                            last_len = errored.len();

                            for elem in errored.clone() {
                                if let Ok(x) = self.evaluate(&new_scope, elem.clone()) {
                                    stmts.push(x);
                                    errored.retain(|x| x != &elem);
                                }
                            }
                        }

                        if !errored.is_empty() {
                            let _ = self.evaluate(&new_scope, errored[0].clone())?;
                        }
                    }
                }

                if &new_scope != scope && !og_create_new_scope.clone().unwrap() {
                    let (mappings, macros) = {
                        let scope = self.scopes.get(&new_scope).unwrap();
                        (scope.mappings.clone(), scope.macros.clone())
                    };

                    for mapping in mappings {
                        self.scopes
                            .get_mut(scope)
                            .unwrap()
                            .mappings
                            .insert(mapping.0, mapping.1);
                    }

                    for scope_macro in macros {
                        self.scopes
                            .get_mut(scope)
                            .unwrap()
                            .macros
                            .insert(scope_macro.0, scope_macro.1);
                    }
                }

                Ok(MiddleNode {
                    node_type: MiddleNodeType::ScopeDeclaration {
                        body: stmts
                            .into_iter()
                            .filter(|x| x.node_type != MiddleNodeType::EmptyLine)
                            .collect(),
                        is_temp,
                        create_new_scope: og_create_new_scope.unwrap(),
                    },
                    span: node.span,
                })
            }
            NodeType::StructLiteral { identifier, value } => Ok(MiddleNode {
                node_type: MiddleNodeType::AggregateExpression {
                    identifier: Some(
                        self.resolve_potential_dollar_ident(scope, &identifier)
                            .unwrap(),
                    ),
                    value: ObjectMap(match value {
                        ObjectType::Map(x) => {
                            let mut map = HashMap::new();

                            for itm in x {
                                map.insert(itm.0, self.evaluate(scope, itm.1)?);
                            }

                            map
                        }
                        ObjectType::Tuple(x) => {
                            let mut map = HashMap::new();

                            for itm in x.into_iter().enumerate() {
                                map.insert(itm.0.to_string(), self.evaluate(scope, itm.1)?);
                            }

                            map
                        }
                    }),
                },
                span: node.span,
            }),
            NodeType::EnumExpression {
                identifier,
                value,
                data,
            } => {
                let identifier =
                    if let Some(x) = self.resolve_potential_dollar_ident(scope, &identifier) {
                        x
                    } else {
                        return Err(MiddleErr::Object(identifier.to_string()));
                    };
                Ok(MiddleNode {
                    node_type: MiddleNodeType::EnumExpression {
                        identifier: identifier.clone(),
                        value: self.resolve_dollar_ident_only(scope, &value).unwrap(),
                        data: if let Some(data) = data {
                            Some(Box::new(self.evaluate(scope, *data)?))
                        } else {
                            None
                        },
                    },
                    span: node.span,
                })
            }
            NodeType::MatchStatement { value, body } => {
                let resolved_data_type = self.resolve_type_from_node(scope, &value).unwrap();
                let resolved_node_type =
                    calibre_mir_ty::middle_data_type_to_node(resolved_data_type.clone());

                let mut ifs: Vec<Node> = Vec::new();
                let mut reference = None;
                let enum_object = if let Some(x) = self.objects.get(
                    resolved_node_type
                        .to_string()
                        .replace("mut ", "")
                        .replace("&", "")
                        .trim(),
                ) {
                    match (
                        resolved_node_type.to_string().contains("mut "),
                        resolved_node_type.to_string().contains("&"),
                    ) {
                        (true, true) => reference = Some(RefMutability::MutRef),
                        (true, false) => reference = Some(RefMutability::MutValue),
                        (false, true) => reference = Some(RefMutability::Ref),
                        (false, false) => reference = Some(RefMutability::Value),
                    }
                    match &x.object_type {
                        MiddleTypeDefType::Enum(x) => Some(x),
                        _ => None,
                    }
                } else {
                    None
                };
                for mut pattern in body {
                    let mut conditionals = if pattern.1.is_empty() {
                        Node::new_from_type(NodeType::Identifier(
                            ParserText::from("true".to_string()).into(),
                        ))
                    } else {
                        pattern.1.remove(0)
                    };

                    for condition in pattern.1 {
                        conditionals = Node::new_from_type(NodeType::BooleanExpression {
                            left: Box::new(conditionals),
                            right: Box::new(condition),
                            operator: BooleanOperation::And,
                        });
                    }

                    match pattern.0 {
                        MatchArmType::Wildcard(_) => {
                            ifs.push(Node::new_from_type(NodeType::IfStatement {
                                comparison: Box::new(IfComparisonType::If(conditionals)),
                                then: pattern.2,
                                otherwise: None,
                            }))
                        }
                        MatchArmType::Value(x) => {
                            ifs.push(Node::new_from_type(NodeType::IfStatement {
                                comparison: Box::new(IfComparisonType::If(Node::new_from_type(
                                    NodeType::BooleanExpression {
                                        left: Box::new(Node::new_from_type(
                                            NodeType::ComparisonExpression {
                                                left: value.clone(),
                                                right: Box::new(x),
                                                operator: Comparison::Equal,
                                            },
                                        )),
                                        right: Box::new(conditionals),
                                        operator: BooleanOperation::And,
                                    },
                                ))),
                                then: pattern.2,
                                otherwise: None,
                            }))
                        }
                        MatchArmType::Let { var_type, name } => {
                            ifs.push(Node::new_from_type(NodeType::IfStatement {
                                comparison: Box::new(IfComparisonType::If(conditionals)),
                                then: Box::new(Node::new_from_type(NodeType::ScopeDeclaration {
                                    body: Some(vec![
                                        Node::new_from_type(NodeType::VariableDeclaration {
                                            var_type,
                                            identifier: name,
                                            value: value.clone(),
                                            data_type: Some(resolved_node_type.clone().into()),
                                        }),
                                        *pattern.2,
                                    ]),
                                    create_new_scope: Some(true),
                                    define: false,
                                    named: None,
                                    is_temp: true,
                                })),
                                otherwise: None,
                            }))
                        }
                        MatchArmType::Enum {
                            value: val,
                            var_type,
                            name,
                        } => {
                            let val = self.resolve_dollar_ident_only(scope, &val).unwrap();
                            let index: i64 = match val.text.trim() {
                                _ if enum_object.is_some() => {
                                    let Some(object) = enum_object else {
                                        return Err(MiddleErr::CantMatch(
                                            calibre_mir_ty::middle_data_type_to_node(
                                                resolved_data_type,
                                            )
                                            .to_string(),
                                        ));
                                    };
                                    let Some(index) =
                                        object.iter().position(|x| x.0.text == val.text)
                                    else {
                                        return Err(MiddleErr::EnumVariant(val.text));
                                    };
                                    index as i64
                                }
                                "Ok" => 0,
                                "Err" => 1,
                                "Some" => 0,
                                "None" => 1,
                                _ => {
                                    return Err(MiddleErr::CantMatch(
                                        calibre_mir_ty::middle_data_type_to_node(
                                            resolved_data_type,
                                        )
                                        .to_string(),
                                    ));
                                }
                            };

                            ifs.push(Node::new_from_type(NodeType::IfStatement {
                                comparison: Box::new(IfComparisonType::If(Node::new_from_type(NodeType::BooleanExpression{
                                    left : Box::new(Node::new_from_type(NodeType::ComparisonExpression {
                                        left: Box::new(Node::new_from_type(NodeType::CallExpression(Box::new(Node::new_from_type(NodeType::Identifier(ParserText::from(String::from("discriminant")).into()))), vec![(*value.clone(), None)]))),
                                        right: Box::new(Node::new_from_type(NodeType::IntLiteral(index))),
                                        operator: Comparison::Equal
                                    })),
                                    right : Box::new(conditionals),
                                    operator: BooleanOperation::And,
                                }))),
                                then: {Box::new(if let Some(name) = name {
                                    Node::new_from_type(NodeType::ScopeDeclaration {
                                        body: Some(vec![
                                            Node::new_from_type(NodeType::VariableDeclaration {
                                                var_type,
                                                identifier: name,
                                                value: if reference.is_some() && reference != Some(RefMutability::Value) {
                                                    Box::new(Node::new_from_type(NodeType::RefStatement {
                                                        mutability: reference.clone().unwrap(),
                                                        value: Box::new(Node::new_from_type(NodeType::MemberExpression {
                                                            path: vec![
                                                                (*value.clone(), false),
                                                                (Node::new_from_type(NodeType::Identifier(ParserText::from(String::from("next")).into())), false)
                                                            ]
                                                        }))
                                                    }))
                                                }else{
                                                    Box::new(Node::new_from_type(NodeType::MemberExpression {
                                                        path: vec![
                                                            (*value.clone(), false),
                                                            (Node::new_from_type(NodeType::Identifier(ParserText::from(String::from("next")).into())), false)
                                                        ]
                                                    }))
                                                },
                                                data_type: None
                                            }),
                                            *pattern.2,
                                        ]),
                                        is_temp: true,
                                        create_new_scope: Some(true),
                                        named: None,
                                        define: false
                                    })}else{
                                        *pattern.2
                                    })
                                },
                                otherwise: None,
                            }));
                        }
                    }
                }
                let ifs = if ifs.is_empty() {
                    Node::new_from_type(NodeType::EmptyLine)
                } else {
                    let mut cur_if = ifs.remove(0);
                    let mut cur_if_ref = &mut cur_if;

                    for new_if in ifs {
                        match &mut cur_if_ref.node_type {
                            NodeType::IfStatement { otherwise, .. } => {
                                *otherwise = Some(Box::new(new_if));
                                cur_if_ref = otherwise.as_mut().unwrap()
                            }
                            _ => unreachable!(),
                        }
                    }

                    cur_if
                };

                self.evaluate(scope, ifs)
            }
            NodeType::FnMatchDeclaration {
                parameters,
                body,
                return_type,
                is_async,
            } => {
                let resolved_data_type =
                    self.resolve_potential_new_type(scope, parameters.1.clone());
                let resolved_node_type =
                    calibre_mir_ty::middle_data_type_to_node(resolved_data_type.clone());

                self.evaluate(
                    scope,
                    Node::new_from_type(NodeType::FunctionDeclaration {
                        parameters: vec![(
                            parameters.0.clone(),
                            parameters.1.clone(),
                            parameters.2.map(|x| *x),
                        )],
                        return_type,
                        is_async,
                        body: Box::new(Node::new_from_type(NodeType::ScopeDeclaration {
                            body: Some(vec![Node::new_from_type(NodeType::MatchStatement {
                                value: Box::new(Node::new_from_type(NodeType::Identifier(
                                    parameters.0,
                                ))),
                                body,
                            })]),
                            named: None,
                            is_temp: true,
                            create_new_scope: Some(false),
                            define: false,
                        })),
                    }),
                )
            }
            NodeType::FunctionDeclaration {
                parameters,
                body,
                return_type,
                is_async,
            } => {
                let mut params = Vec::with_capacity(parameters.len());
                let new_scope = self.new_scope_from_parent_shallow(*scope);
                for param in parameters {
                    let og_name = self.resolve_dollar_ident_only(scope, &param.0).unwrap();
                    let new_name = get_disamubiguous_name(
                        scope,
                        Some(og_name.trim()),
                        Some(&VarType::Mutable),
                    );
                    let data_type = self.resolve_potential_new_type(scope, param.1);
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
                        .insert(og_name.text.clone(), new_name.clone());

                    params.push((
                        ParserText::from(new_name),
                        data_type,
                        if let Some(x) = param.2 {
                            Some(self.evaluate(scope, x)?)
                        } else {
                            None
                        },
                    ));
                }

                let return_type = self.resolve_potential_new_type(scope, return_type);

                Ok(MiddleNode {
                    node_type: MiddleNodeType::FunctionDeclaration {
                        parameters: params,
                        body: Box::new(self.evaluate(&new_scope, *body)?),
                        return_type,
                        is_async,
                    },
                    span: node.span,
                })
            }
            NodeType::CallExpression(caller, args) => {
                if let NodeType::Identifier(caller) = &caller.node_type {
                    if "tuple" == &caller.to_string() {
                        let mut map = HashMap::new();

                        for (i, arg) in args.into_iter().enumerate() {
                            map.insert(i.to_string(), self.evaluate(scope, arg.0)?);
                        }

                        return Ok(MiddleNode {
                            node_type: MiddleNodeType::AggregateExpression {
                                identifier: None,
                                value: map.into(),
                            },
                            span: node.span,
                        });
                    }

                    if let Some(caller) = self
                        .resolve_potential_dollar_ident(scope, caller)
                        .map(|x| x.clone())
                    {
                        if self.objects.contains_key(&caller.text) {
                            let mut map = HashMap::new();

                            for (i, arg) in args.into_iter().enumerate() {
                                map.insert(i.to_string(), self.evaluate(scope, arg.0)?);
                            }

                            return Ok(MiddleNode {
                                node_type: MiddleNodeType::AggregateExpression {
                                    identifier: Some(ParserText::from(caller)),
                                    value: map.into(),
                                },
                                span: node.span,
                            });
                        }
                    }
                }

                Ok(MiddleNode {
                    node_type: MiddleNodeType::CallExpression(
                        Box::new(self.evaluate(scope, *caller)?),
                        {
                            let mut lst = Vec::new();

                            for arg in args {
                                lst.push((
                                    self.evaluate(scope, arg.0)?,
                                    if let Some(a) = arg.1 {
                                        Some(self.evaluate(scope, a)?)
                                    } else {
                                        None
                                    },
                                ));
                            }

                            lst
                        },
                    ),
                    span: node.span,
                })
            }
            NodeType::ImportStatement {
                module,
                alias,
                values,
            } => {
                let values = {
                    let mut lst = Vec::new();

                    for val in values {
                        lst.push(self.resolve_potential_dollar_ident(scope, &val).unwrap());
                    }

                    lst
                };

                let alias = if let Some(alias) = alias {
                    self.resolve_potential_dollar_ident(scope, &alias)
                } else {
                    None
                };

                let new_scope = if let Some(alias) = alias {
                    if ["super", "root"].contains(&alias.as_str()) {
                        return Ok(MiddleNode {
                            node_type: MiddleNodeType::EmptyLine,
                            span: node.span,
                        });
                    }
                    let new_scope_id = self.get_scope_list(
                        *scope,
                        module.into_iter().map(|x| x.to_string()).collect(),
                    )?;
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
                    let (_, n) = self.import_scope_list(
                        *scope,
                        module.into_iter().map(|x| x.to_string()).collect(),
                    )?;
                    return Ok(if let Some(x) = n {
                        x
                    } else {
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
                            let scope = self.scopes.get_mut(scope).unwrap();
                            if !scope.mappings.contains_key(&key) {
                                scope.mappings.insert(key, value);
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
            }
            NodeType::ScopeMemberExpression { path } => {
                self.evaluate_scope_member_expression(scope, path)
            }
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
            let x = self.resolve_potential_dollar_ident(scope, x).unwrap();
            if let Ok(s) = self.get_next_scope(*scope, &x.text) {
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

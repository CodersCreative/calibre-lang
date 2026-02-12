use crate::{
    ast::{MiddleNode, MiddleNodeType},
    environment::{
        MiddleEnvironment, MiddleObject, MiddleOverload, MiddleTrait, MiddleTraitMember,
        MiddleTypeDefType, Operator, get_disamubiguous_name,
    },
    errors::MiddleErr,
};
use calibre_parser::{
    ast::{
        CallArg, FunctionHeader, IfComparisonType, MatchArmType, Node, NodeType, ObjectMap,
        ObjectType, ParserDataType, ParserInnerType, ParserText, PotentialDollarIdentifier,
        PotentialGenericTypeIdentifier, PotentialNewType, VarType, comparison::ComparisonOperator,
    },
    lexer::Span,
};
use rustc_hash::FxHashMap;
use std::str::FromStr;

pub mod functions;
pub mod loops;
pub mod matches;
pub mod member;
pub mod scopes;
pub mod statements;

impl MiddleEnvironment {
    pub fn evaluate(&mut self, scope: &u64, node: Node) -> MiddleNode {
        let span = node.span;
        match self.evaluate_inner(scope, node) {
            Ok(node) => node,
            Err(err) => {
                self.push_error(err);
                MiddleNode::new(MiddleNodeType::EmptyLine, span)
            }
        }
    }

    pub fn evaluate_inner(&mut self, scope: &u64, node: Node) -> Result<MiddleNode, MiddleErr> {
        self.current_location = self.get_location(scope, node.span);

        match node.node_type {
            NodeType::DataType { .. } => unreachable!(),
            NodeType::Null => Ok(MiddleNode {
                node_type: MiddleNodeType::Null,
                span: node.span,
            }),
            NodeType::Defer { value, function } => {
                if function {
                    self.func_defers.push(*value);
                } else {
                    self.scopes.get_mut(scope).unwrap().defers.push(*value);
                }
                Ok(MiddleNode {
                    node_type: MiddleNodeType::EmptyLine,
                    span: node.span,
                })
            }
            NodeType::Identifier(x) => Ok(MiddleNode {
                node_type: MiddleNodeType::Identifier(
                    if let Some(x) = self.resolve_potential_generic_ident(scope, &x) {
                        x
                    } else if let PotentialDollarIdentifier::DollarIdentifier(x) = x.get_ident() {
                        let val = self.resolve_macro_arg(scope, x).unwrap().clone();
                        return self.evaluate_inner(scope, val);
                    } else {
                        return Err(MiddleErr::At(
                            node.span,
                            Box::new(MiddleErr::Variable(x.to_string())),
                        ));
                    },
                ),
                span: node.span,
            }),
            NodeType::IntLiteral(number) => {
                let number = if let Some((_, x)) = number.split_once("x") {
                    i64::from_str_radix(x, 16).unwrap()
                } else if let Some((_, x)) = number.split_once("o") {
                    i64::from_str_radix(x, 8).unwrap()
                } else if let Some((_, x)) = number.split_once("b") {
                    i64::from_str_radix(x, 2).unwrap()
                } else {
                    number.parse().unwrap()
                };

                Ok(MiddleNode {
                    node_type: MiddleNodeType::IntLiteral(number),
                    span: node.span,
                })
            }
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
                    from: Box::new(self.evaluate(scope, *from)),
                    to: Box::new(self.evaluate(scope, *to)),
                    inclusive,
                },
                span: node.span,
            }),
            NodeType::MemberExpression { path } => {
                self.evaluate_member_expression(scope, node.span, path)
            }
            NodeType::Ternary {
                comparison,
                then,
                otherwise,
            } => self.evaluate_inner(
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
            NodeType::MoveExpression { value } => {
                if let NodeType::MemberExpression { mut path } = value.node_type.clone()
                    && let Some((
                        Node {
                            node_type: NodeType::Identifier(base),
                            ..
                        },
                        _,
                    )) = path.first().cloned()
                {
                    let tmp_name =
                        format!("__move_tmp_{}_{}", node.span.from.line, node.span.from.col);

                    let tmp_ident =
                        PotentialDollarIdentifier::Identifier(ParserText::from(tmp_name.clone()));

                    let tmp_decl = Node::new(
                        node.span,
                        NodeType::VariableDeclaration {
                            var_type: VarType::Immutable,
                            identifier: tmp_ident.clone(),
                            data_type: PotentialNewType::DataType(ParserDataType::from(
                                ParserInnerType::Auto(None),
                            )),
                            value: Box::new(Node::new(node.span, NodeType::Move(base.into()))),
                        },
                    );

                    path[0].0 = Node::new(
                        node.span,
                        NodeType::Identifier(PotentialGenericTypeIdentifier::Identifier(tmp_ident)),
                    );
                    let member = Node::new(node.span, NodeType::MemberExpression { path });

                    return self.evaluate_inner(
                        scope,
                        Node::new(
                            node.span,
                            NodeType::ScopeDeclaration {
                                body: Some(vec![tmp_decl, member]),
                                is_temp: true,
                                create_new_scope: Some(true),
                                define: false,
                                named: None,
                            },
                        ),
                    );
                }

                self.evaluate_inner(scope, *value)
            }
            NodeType::TupleLiteral { values } => {
                let span = node.span;
                let tuple_call = Node::new(
                    span,
                    NodeType::CallExpression {
                        string_fn: None,
                        generic_types: Vec::new(),
                        caller: Box::new(Node::new(
                            span,
                            NodeType::Identifier(PotentialGenericTypeIdentifier::Identifier(
                                ParserText::from(String::from("tuple")).into(),
                            )),
                        )),
                        args: values.into_iter().map(CallArg::Value).collect(),
                        reverse_args: Vec::new(),
                    },
                );
                self.evaluate_inner(scope, tuple_call)
            }
            NodeType::Move(x) => Ok(MiddleNode {
                node_type: MiddleNodeType::Move(
                    self.resolve_potential_dollar_ident(scope, &x).unwrap(),
                ),
                span: node.span,
            }),
            NodeType::Drop(x) => Ok(MiddleNode {
                node_type: MiddleNodeType::Drop(
                    self.resolve_potential_dollar_ident(scope, &x).unwrap(),
                ),
                span: node.span,
            }),
            NodeType::IfStatement {
                comparison,
                then,
                otherwise,
            } => match *comparison {
                IfComparisonType::If(x) => Ok(MiddleNode {
                    node_type: MiddleNodeType::IfStatement {
                        comparison: Box::new(self.evaluate(scope, x)),
                        then: Box::new(self.evaluate(scope, *then)),
                        otherwise: otherwise.map(|x| Box::new(self.evaluate(scope, *x))),
                    },
                    span: node.span,
                }),
                IfComparisonType::IfLet { value, pattern } => self.evaluate_inner(
                    scope,
                    Node {
                        node_type: NodeType::MatchStatement {
                            value: Some(Box::new(value)),
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
            NodeType::Until { condition } => self.evaluate_inner(
                scope,
                Node {
                    node_type: NodeType::IfStatement {
                        comparison: Box::new(IfComparisonType::If(*condition)),
                        then: Box::new(Node {
                            node_type: NodeType::Break,
                            span: node.span,
                        }),
                        otherwise: None,
                    },
                    span: node.span,
                },
            ),
            NodeType::Break => Ok(MiddleNode {
                node_type: {
                    let mut lst = Vec::new();

                    for x in self.collect_defers_chain(scope) {
                        lst.push(self.evaluate(scope, x));
                    }

                    for value in self.scopes.get(scope).unwrap().defined.clone() {
                        lst.push(MiddleNode::new(
                            MiddleNodeType::Drop(value.into()),
                            self.current_span(),
                        ))
                    }

                    if lst.is_empty() {
                        return Ok(MiddleNode::new(MiddleNodeType::Break, node.span));
                    }

                    lst.push(MiddleNode::new(MiddleNodeType::Break, self.current_span()));

                    MiddleNodeType::ScopeDeclaration {
                        body: lst,
                        create_new_scope: false,
                        is_temp: true,
                        scope_id: *scope,
                    }
                },
                span: node.span,
            }),
            NodeType::Continue => Ok(MiddleNode {
                node_type: {
                    let mut lst = Vec::new();

                    for x in self.collect_defers_chain(scope) {
                        lst.push(self.evaluate(scope, x));
                    }

                    for value in self.scopes.get(scope).unwrap().defined.clone() {
                        lst.push(MiddleNode::new(
                            MiddleNodeType::Drop(value.into()),
                            self.current_span(),
                        ))
                    }

                    if lst.is_empty() {
                        return Ok(MiddleNode::new(MiddleNodeType::Continue, node.span));
                    }

                    lst.push(MiddleNode::new(
                        MiddleNodeType::Continue,
                        self.current_span(),
                    ));

                    MiddleNodeType::ScopeDeclaration {
                        body: lst,
                        create_new_scope: false,
                        is_temp: true,
                        scope_id: *scope,
                    }
                },
                span: node.span,
            }),
            NodeType::EmptyLine => Ok(MiddleNode {
                node_type: MiddleNodeType::EmptyLine,
                span: node.span,
            }),
            NodeType::Return { value } => Ok(MiddleNode {
                node_type: MiddleNodeType::Return {
                    value: {
                        let mut lst = Vec::new();

                        let mut in_return = Vec::new();

                        let value = if let Some(x) = value {
                            let x = self.evaluate(scope, *x);
                            in_return = x
                                .identifiers_used()
                                .into_iter()
                                .map(|x| x.to_string())
                                .collect();
                            Some(x)
                        } else {
                            None
                        };

                        let chain_defers = self.collect_defers_chain(scope);
                        for x in chain_defers {
                            lst.push(self.evaluate(scope, x));
                        }

                        for x in self.func_defers.clone() {
                            lst.push(self.evaluate(scope, x));
                        }

                        for value in self
                            .scopes
                            .get(scope)
                            .unwrap()
                            .defined
                            .clone()
                            .into_iter()
                            .filter(|x| !in_return.contains(x))
                        {
                            lst.push(MiddleNode::new(
                                MiddleNodeType::Drop(value.into()),
                                self.current_span(),
                            ))
                        }

                        if lst.is_empty() {
                            value.map(Box::new)
                        } else {
                            if let Some(x) = value {
                                lst.push(x);
                            }

                            Some(Box::new(MiddleNode::new(
                                MiddleNodeType::ScopeDeclaration {
                                    body: lst,
                                    create_new_scope: false,
                                    is_temp: true,
                                    scope_id: *scope,
                                },
                                self.current_span(),
                            )))
                        }
                    },
                },
                span: node.span,
            }),
            NodeType::RefStatement { mutability, value } => Ok(MiddleNode {
                node_type: MiddleNodeType::RefStatement {
                    mutability,
                    value: Box::new(self.evaluate_inner(scope, *value)?),
                },
                span: node.span,
            }),
            NodeType::DerefStatement { value } => Ok(MiddleNode {
                node_type: MiddleNodeType::DerefStatement {
                    value: Box::new(self.evaluate_inner(scope, *value)?),
                },
                span: node.span,
            }),
            NodeType::ParenExpression { value } => self.evaluate_inner(scope, *value),
            NodeType::DestructureDeclaration {
                var_type: _,
                pattern,
                value,
            } => {
                let tmp_name = format!(
                    "__destructure_tmp_{}_{}",
                    node.span.from.line, node.span.from.col
                );

                let tmp_ident =
                    PotentialDollarIdentifier::Identifier(ParserText::from(tmp_name.clone()));

                let tmp_decl = Node::new(
                    node.span,
                    NodeType::VariableDeclaration {
                        var_type: VarType::Immutable,
                        identifier: tmp_ident.clone(),
                        data_type: PotentialNewType::DataType(ParserDataType::from(
                            ParserInnerType::Auto(None),
                        )),
                        value,
                    },
                );

                let mut body = Vec::new();
                body.push(tmp_decl);
                body.extend(
                    self.emit_destructure_statements(&tmp_ident, &pattern, node.span, true),
                );

                self.evaluate_inner(
                    scope,
                    Node::new(
                        node.span,
                        NodeType::ScopeDeclaration {
                            body: Some(body),
                            named: None,
                            is_temp: true,
                            create_new_scope: Some(false),
                            define: false,
                        },
                    ),
                )
            }
            NodeType::DestructureAssignment { pattern, value } => {
                let tmp_name = format!(
                    "__destructure_tmp_{}_{}",
                    node.span.from.line, node.span.from.col
                );
                let tmp_ident =
                    PotentialDollarIdentifier::Identifier(ParserText::from(tmp_name.clone()));

                let tmp_decl = Node::new(
                    node.span,
                    NodeType::VariableDeclaration {
                        var_type: VarType::Immutable,
                        identifier: tmp_ident.clone(),
                        data_type: PotentialNewType::DataType(ParserDataType::from(
                            ParserInnerType::Auto(None),
                        )),
                        value,
                    },
                );

                let mut body = Vec::new();
                body.push(tmp_decl);
                body.extend(
                    self.emit_destructure_statements(&tmp_ident, &pattern, node.span, false),
                );

                self.evaluate_inner(
                    scope,
                    Node::new(
                        node.span,
                        NodeType::ScopeDeclaration {
                            body: Some(body),
                            named: None,
                            is_temp: true,
                            create_new_scope: Some(false),
                            define: false,
                        },
                    ),
                )
            }
            NodeType::VariableDeclaration {
                var_type,
                identifier,
                value,
                data_type,
            } => self.evaluate_var_declaration(
                scope, node.span, var_type, identifier, *value, data_type,
            ),
            NodeType::TypeDeclaration {
                identifier,
                object,
                overloads,
            } => {
                if let calibre_parser::ast::TypeDefType::NewType(inner) = &object {
                    let identifier = self
                        .resolve_dollar_ident_potential_generic_only(scope, &identifier)
                        .unwrap();
                    let resolved = self.resolve_potential_new_type(scope, *inner.clone());
                    self.type_aliases
                        .insert(identifier.text.clone(), resolved.clone());
                    self.scopes
                        .get_mut(scope)
                        .unwrap()
                        .mappings
                        .insert(identifier.text.clone(), identifier.text.clone());

                    return Ok(MiddleNode {
                        node_type: MiddleNodeType::EmptyLine,
                        span: node.span,
                    });
                }
                if let calibre_parser::ast::PotentialGenericTypeIdentifier::Generic {
                    identifier: base_ident,
                    generic_types,
                } = identifier.clone()
                {
                    let base_ident = self.resolve_dollar_ident_only(scope, &base_ident).unwrap();
                    let template_params: Vec<String> = generic_types
                        .iter()
                        .filter_map(|t| match t {
                            PotentialNewType::DataType(ParserDataType {
                                data_type: ParserInnerType::Struct(s),
                                ..
                            }) => Some(s.clone()),
                            _ => None,
                        })
                        .collect();

                    self.generic_type_templates
                        .entry(base_ident.text.clone())
                        .or_insert((template_params, object.clone(), overloads.clone()));

                    self.scopes
                        .get_mut(scope)
                        .unwrap()
                        .mappings
                        .insert(base_ident.text.clone(), base_ident.text.clone());

                    return Ok(MiddleNode {
                        node_type: MiddleNodeType::EmptyLine,
                        span: node.span,
                    });
                }

                let identifier = self
                    .resolve_dollar_ident_potential_generic_only(scope, &identifier)
                    .unwrap();
                let object = self.type_def_type_into(scope, object);
                let new_name = if identifier.text.contains("__") {
                    identifier.text.clone()
                } else {
                    get_disamubiguous_name(scope, Some(identifier.text.trim()), None)
                };

                self.objects.insert(
                    new_name.clone(),
                    MiddleObject {
                        object_type: object,
                        variables: FxHashMap::default(),
                        traits: Vec::new(),
                        location: self.current_location.clone(),
                    },
                );

                self.scopes
                    .get_mut(scope)
                    .unwrap()
                    .mappings
                    .insert(identifier.text, new_name.clone());

                let previous_self = self
                    .scopes
                    .get_mut(scope)
                    .unwrap()
                    .mappings
                    .insert(String::from("Self"), new_name.clone());

                for overload in overloads {
                    let overload = MiddleOverload {
                        operator: Operator::from_str(&overload.operator.text)?,
                        return_type: self
                            .resolve_potential_new_type(scope, overload.header.return_type.clone()),
                        parameters: {
                            let mut params = Vec::new();
                            let mut contains = false;

                            for param in overload.header.parameters.iter() {
                                let ty = self.resolve_potential_new_type(scope, param.1.clone());

                                if let ParserInnerType::Struct(x) =
                                    ty.data_type.clone().unwrap_all_refs()
                                    && x == new_name
                                {
                                    contains = true;
                                }

                                params.push(ty);
                            }

                            if !contains {
                                continue;
                            }

                            params
                        },
                        func: overload.into(),
                    };

                    self.overloads.push(overload);
                }

                if let Some(prev) = previous_self {
                    self.scopes
                        .get_mut(scope)
                        .unwrap()
                        .mappings
                        .insert(String::from("Self"), prev);
                }

                Ok(MiddleNode {
                    node_type: MiddleNodeType::EmptyLine,
                    span: node.span,
                })
            }
            NodeType::BooleanExpression {
                left,
                right,
                operator,
            } => {
                if let Some(x) = self.handle_operator_overloads(
                    scope,
                    node.span,
                    *left.clone(),
                    *right.clone(),
                    Operator::Boolean(operator.clone()),
                )? {
                    return Ok(x);
                }

                Ok(MiddleNode {
                    node_type: MiddleNodeType::BooleanExpression {
                        left: Box::new(self.evaluate(scope, *left)),
                        right: Box::new(self.evaluate(scope, *right)),
                        operator,
                    },
                    span: node.span,
                })
            }
            NodeType::ComparisonExpression {
                left,
                right,
                operator,
            } => {
                if let Some(x) = self.handle_operator_overloads(
                    scope,
                    node.span,
                    *left.clone(),
                    *right.clone(),
                    Operator::Comparison(operator.clone()),
                )? {
                    return Ok(x);
                }
                Ok(MiddleNode {
                    node_type: MiddleNodeType::ComparisonExpression {
                        left: Box::new(self.evaluate(scope, *left)),
                        right: Box::new(self.evaluate(scope, *right)),
                        operator,
                    },
                    span: node.span,
                })
            }
            NodeType::BinaryExpression {
                left,
                right,
                operator,
            } => {
                if let Some(x) = self.handle_operator_overloads(
                    scope,
                    node.span,
                    *left.clone(),
                    *right.clone(),
                    Operator::Binary(operator.clone()),
                )? {
                    return Ok(x);
                }
                Ok(MiddleNode {
                    node_type: MiddleNodeType::BinaryExpression {
                        left: Box::new(self.evaluate(scope, *left)),
                        right: Box::new(self.evaluate(scope, *right)),
                        operator,
                    },
                    span: node.span,
                })
            }
            NodeType::NotExpression { value } => self.evaluate_inner(
                scope,
                Node {
                    node_type: NodeType::ComparisonExpression {
                        left: value,
                        right: Box::new(Node::new(
                            self.current_span(),
                            NodeType::Identifier(ParserText::from("false".to_string()).into()),
                        )),
                        operator: ComparisonOperator::Equal,
                    },
                    span: node.span,
                },
            ),
            NodeType::NegExpression { value } => Ok(MiddleNode {
                node_type: MiddleNodeType::NegExpression {
                    value: Box::new(self.evaluate_inner(scope, *value)?),
                },
                span: node.span,
            }),
            NodeType::AsExpression { value, data_type } => Ok(MiddleNode {
                node_type: MiddleNodeType::AsExpression {
                    value: Box::new(self.evaluate_inner(scope, *value)?),
                    data_type: self.resolve_potential_new_type(scope, data_type),
                },
                span: node.span,
            }),
            NodeType::InDeclaration { identifier, value } => self.evaluate_inner(
                scope,
                Node::new(
                    self.current_span(),
                    NodeType::CallExpression {
                        string_fn: None,
                        caller: Box::new(Node::new(
                            self.current_span(),
                            NodeType::Identifier(ParserText::from("contains".to_string()).into()),
                        )),
                        generic_types: Vec::new(),
                        args: vec![CallArg::Value(*value), CallArg::Value(*identifier)],
                        reverse_args: Vec::new(),
                    },
                ),
            ),
            NodeType::DebugExpression { value } => Ok(MiddleNode {
                node_type: MiddleNodeType::DebugExpression {
                    pretty_printed_str: value.to_string(),
                    value: Box::new(self.evaluate_inner(scope, *value)?),
                },
                span: node.span,
            }),
            NodeType::ListLiteral(data_type, x) => {
                let data_type = if data_type.is_auto() && !x.is_empty() {
                    self.resolve_type_from_node(scope, x.first().unwrap())
                        .unwrap_or(self.resolve_potential_new_type(scope, data_type))
                } else {
                    self.resolve_potential_new_type(scope, data_type)
                };

                let mut lst = Vec::new();

                for item in x {
                    lst.push(self.evaluate(scope, item));
                }

                Ok(MiddleNode {
                    node_type: MiddleNodeType::ListLiteral(data_type, lst),
                    span: node.span,
                })
            }
            NodeType::Try { value, catch } => {
                let resolved_type = self.resolve_type_from_node(scope, &value);

                self.evaluate_inner(
                    scope,
                    Node {
                        node_type: NodeType::MatchStatement  {
                                        value: Some(value),
                                        body: match resolved_type {
                                            Some(ParserDataType { data_type: ParserInnerType::Option(_), span: _ }) => vec![
                                                (
                                                    MatchArmType::Enum { var_type : VarType::Immutable, value: ParserText::from(String::from("Some")).into(), name: Some(ParserText::from(String::from("anon_ok_value")).into()), destructure: None },
                                                    Vec::new(),
                                                    Box::new(Node {
                                                        node_type: NodeType::Identifier(ParserText::from(String::from("anon_ok_value")).into()),
                                                        span: Span::default(),
                                                    }),
                                                ),
                                                if let Some(catch) = catch {
                                                    (
                                                        MatchArmType::Enum { var_type : VarType::Immutable, value: ParserText::from(String::from("None")).into(), name: catch.name, destructure: None },
                                                        Vec::new(),
                                                        catch.body,
                                                    )
                                                }else {
                                                    (
                                                        MatchArmType::Enum { var_type : VarType::Immutable, value: ParserText::from(String::from("None")).into(), name: None, destructure: None },
                                                        Vec::new(),
                                                        Box::new(Node {
                                                            node_type: NodeType::Return { value: Some(Box::new(Node::new(self.current_span(), NodeType::CallExpression{string_fn : None,generic_types: Vec::new(),caller : Box::new(Node::new(self.current_span(), NodeType::Identifier(ParserText::from(String::from("none")).into()))), args : Vec::new(), reverse_args: Vec::new(),}))) },
                                                            span: Span::default(),
                                                        }),
                                                    )
                                                },
                                            ],
                                            _ => vec![
                                                (
                                                    MatchArmType::Enum { var_type : VarType::Immutable, value: ParserText::from(String::from("Ok")).into(), name: Some(ParserText::from(String::from("anon_ok_value")).into()), destructure: None },
                                                    Vec::new(),
                                                    Box::new(Node {
                                                        node_type: NodeType::Identifier(ParserText::from(String::from("anon_ok_value")).into()),
                                                        span: Span::default(),
                                                    }),
                                                ),
                                                if let Some(catch) = catch {
                                                    (
                                                        MatchArmType::Enum { var_type : VarType::Immutable, value: ParserText::from(String::from("Err")).into(), name: catch.name, destructure: None },
                                                        Vec::new(),
                                                        catch.body,
                                                    )
                                                }else {
                                                    (
                                                        MatchArmType::Enum { var_type : VarType::Immutable, value: ParserText::from(String::from("Err")).into(), name: Some(ParserText::from(String::from("anon_err_value")).into()), destructure: None },
                                                        Vec::new(),
                                                        Box::new(Node {
                                                            node_type: NodeType::Return { value: Some(Box::new(Node::new(self.current_span(), NodeType::CallExpression{string_fn : None,generic_types: Vec::new(), caller : Box::new(Node::new(self.current_span(), NodeType::Identifier(ParserText::from(String::from("err")).into()))), args : vec![CallArg::Value(Node::new(self.current_span(), NodeType::Identifier(ParserText::from(String::from("anon_err_value")).into())))], reverse_args: Vec::new(),}))) },
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
            NodeType::LoopDeclaration {
                loop_type,
                body,
                until,
            } => self.evaluate_loop_statement(scope, node.span, *loop_type, *body, until),
            NodeType::IterExpression {
                data_type,
                map,
                loop_type,
                conditionals,
                until,
            } => self.evaluate_iter_expression(
                scope,
                node.span,
                data_type,
                map,
                loop_type,
                conditionals,
                until,
            ),
            NodeType::AssignmentExpression { identifier, value } => {
                match identifier.node_type.clone().unwrap() {
                    NodeType::Ternary {
                        comparison,
                        then,
                        otherwise,
                    } => self.evaluate_inner(
                        scope,
                        Node {
                            node_type: NodeType::IfStatement {
                                comparison: Box::new(IfComparisonType::If(*comparison)),
                                then: Box::new(Node::new(
                                    self.current_span(),
                                    NodeType::AssignmentExpression {
                                        identifier: then,
                                        value: value.clone(),
                                    },
                                )),
                                otherwise: Some(Box::new(Node::new(
                                    self.current_span(),
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
                            identifier: Box::new(self.evaluate(scope, *identifier)),
                            value: Box::new(self.evaluate(scope, *value)),
                        },
                        span: node.span,
                    }),
                }
            }
            NodeType::ImplDeclaration {
                generics,
                target,
                variables,
            } => {
                let resolved = self
                    .resolve_potential_new_type(scope, target)
                    .unwrap_all_refs();
                let target_key = self.type_key(&resolved);

                let mut prev_generics = Vec::new();
                if let Some(scope_ref) = self.scopes.get_mut(scope) {
                    for generic in generics.0.iter() {
                        let name = generic.identifier.to_string();
                        prev_generics.push((name.clone(), scope_ref.mappings.get(&name).cloned()));
                        scope_ref.mappings.insert(name.clone(), name.clone());
                    }
                }

                let generic_params: Vec<String> = generics
                    .0
                    .iter()
                    .map(|g| g.identifier.to_string())
                    .collect();
                let impl_key = self.get_or_create_impl(resolved.clone(), generic_params);

                let previous_self = self
                    .scopes
                    .get_mut(scope)
                    .unwrap()
                    .mappings
                    .insert(String::from("Self"), target_key.clone());

                let mut statements = Vec::new();

                for var in variables {
                    #[allow(unused_assignments)]
                    let (mut iden, mut dependant) = (String::new(), false);

                    let dec = Node {
                        span: var.span,
                        node_type: match var.node_type {
                            NodeType::VariableDeclaration {
                                var_type,
                                identifier,
                                value,
                                data_type,
                            } => {
                                iden = identifier.to_string();
                                let resolved_iden = format!("{}::{}", target_key, identifier);

                                dependant = match &value.node_type {
                                    NodeType::FunctionDeclaration {
                                        header, body: _, ..
                                    } => {
                                        if let Some(PotentialNewType::DataType(param)) =
                                            header.parameters.first().map(|x| x.1.clone())
                                        {
                                            let param_type = self
                                                .resolve_data_type(scope, param)
                                                .unwrap_all_refs();
                                            self.impl_type_matches(
                                                &resolved.data_type,
                                                &param_type.data_type,
                                                &self.impls.get(&impl_key).unwrap().generic_params,
                                            )
                                        } else {
                                            false
                                        }
                                    }
                                    _ => false,
                                };

                                NodeType::VariableDeclaration {
                                    var_type,
                                    identifier: PotentialDollarIdentifier::Identifier(
                                        ParserText::from(resolved_iden),
                                    ),
                                    value,
                                    data_type,
                                }
                            }
                            NodeType::TypeDeclaration { .. } => {
                                continue;
                            }
                            _ => unreachable!(),
                        },
                    };

                    let dec = self.evaluate(scope, dec);

                    let new_name = match &dec.node_type {
                        MiddleNodeType::VariableDeclaration { identifier, .. } => {
                            identifier.text.clone()
                        }
                        _ => unreachable!(),
                    };

                    self.impls
                        .get_mut(&impl_key)
                        .unwrap()
                        .variables
                        .insert(iden, (new_name, dependant));

                    statements.push(dec);
                }

                if let Some(prev) = previous_self {
                    self.scopes
                        .get_mut(scope)
                        .unwrap()
                        .mappings
                        .insert(String::from("Self"), prev);
                }
                if let Some(scope_ref) = self.scopes.get_mut(scope) {
                    for (name, prev) in prev_generics {
                        if let Some(prev) = prev {
                            scope_ref.mappings.insert(name, prev);
                        } else {
                            scope_ref.mappings.remove(&name);
                        }
                    }
                }

                Ok(MiddleNode {
                    node_type: MiddleNodeType::ScopeDeclaration {
                        body: statements,
                        create_new_scope: false,
                        is_temp: false,
                        scope_id: *scope,
                    },
                    span: node.span,
                })
            }
            NodeType::ImplTraitDeclaration {
                generics,
                trait_ident,
                target,
                variables,
            } => {
                let mut prev_generics = Vec::new();
                if let Some(scope_ref) = self.scopes.get_mut(scope) {
                    for generic in generics.0.iter() {
                        let name = generic.identifier.to_string();
                        prev_generics.push((name.clone(), scope_ref.mappings.get(&name).cloned()));
                        scope_ref.mappings.insert(name.clone(), name.clone());
                    }
                }

                let resolved_trait = self
                    .resolve_potential_generic_ident(scope, &trait_ident)
                    .ok_or_else(|| {
                        self.err_at_current(MiddleErr::Scope(trait_ident.to_string()))
                    })?;

                let resolved_target = self
                    .resolve_potential_new_type(scope, target)
                    .unwrap_all_refs();
                let target_key = self.type_key(&resolved_target);

                let trait_def = self.trait_defs.get(&resolved_trait.text).cloned();

                let mut provided = std::collections::HashSet::new();
                let mut assoc_types = Vec::new();
                for var in &variables {
                    match &var.node_type {
                        NodeType::VariableDeclaration { identifier, .. } => {
                            provided.insert(identifier.to_string());
                        }
                        NodeType::TypeDeclaration {
                            identifier, object, ..
                        } => {
                            assoc_types.push((identifier.clone(), object.clone()));
                        }
                        _ => {}
                    }
                }

                let mut all_vars = variables;
                if let Some(trait_def) = trait_def {
                    for (name, member) in trait_def.members {
                        if member.default.is_none() || provided.contains(&name) {
                            continue;
                        }

                        let default = member.default.unwrap();
                        all_vars.push(Node::new(
                            default.span,
                            NodeType::VariableDeclaration {
                                var_type: VarType::Constant,
                                identifier: PotentialDollarIdentifier::Identifier(
                                    ParserText::from(name.clone()),
                                ),
                                data_type: PotentialNewType::DataType(member.data_type.clone()),
                                value: Box::new(default),
                            },
                        ));
                    }
                }

                let previous_self = self
                    .scopes
                    .get_mut(scope)
                    .unwrap()
                    .mappings
                    .insert(String::from("Self"), target_key.clone());

                let generic_params: Vec<String> = generics
                    .0
                    .iter()
                    .map(|g| g.identifier.to_string())
                    .collect();
                let impl_key = self.get_or_create_impl(resolved_target.clone(), generic_params);

                for (identifier, object) in assoc_types {
                    if let calibre_parser::ast::TypeDefType::NewType(inner) = object {
                        let resolved_ty = self
                            .resolve_potential_new_type(scope, *inner)
                            .unwrap_all_refs();
                        self.impls
                            .get_mut(&impl_key)
                            .unwrap()
                            .assoc_types
                            .insert(identifier.to_string(), resolved_ty);
                    }
                }

                {
                    let impl_ref = self.impls.get_mut(&impl_key).unwrap();
                    for var in &all_vars {
                        if let NodeType::VariableDeclaration { identifier, .. } = &var.node_type {
                            let resolved_iden = format!("{}::{}", target_key, identifier);
                            impl_ref
                                .variables
                                .entry(identifier.to_string())
                                .or_insert((resolved_iden, false));
                        }
                    }
                }

                let mut statements = Vec::new();

                for var in all_vars {
                    #[allow(unused_assignments)]
                    let (mut iden, mut dependant) = (String::new(), false);

                    let dec = Node {
                        span: var.span,
                        node_type: match var.node_type {
                            NodeType::VariableDeclaration {
                                var_type,
                                identifier,
                                value,
                                data_type,
                            } => {
                                iden = identifier.to_string();
                                let resolved_iden = format!("{}::{}", target_key, identifier);

                                dependant = match &value.node_type {
                                    NodeType::FunctionDeclaration {
                                        header, body: _, ..
                                    } => {
                                        if let Some(PotentialNewType::DataType(param)) =
                                            header.parameters.first().map(|x| x.1.clone())
                                        {
                                            let param_type = self
                                                .resolve_data_type(scope, param)
                                                .unwrap_all_refs();
                                            self.impl_type_matches(
                                                &resolved_target.data_type,
                                                &param_type.data_type,
                                                &self.impls.get(&impl_key).unwrap().generic_params,
                                            )
                                        } else {
                                            false
                                        }
                                    }
                                    _ => false,
                                };

                                NodeType::VariableDeclaration {
                                    var_type,
                                    identifier: PotentialDollarIdentifier::Identifier(
                                        ParserText::from(resolved_iden),
                                    ),
                                    value,
                                    data_type,
                                }
                            }
                            NodeType::TypeDeclaration { .. } => {
                                continue;
                            }
                            _ => unreachable!(),
                        },
                    };

                    let dec = self.evaluate(scope, dec);

                    let new_name = match &dec.node_type {
                        MiddleNodeType::VariableDeclaration { identifier, .. } => {
                            identifier.text.clone()
                        }
                        _ => unreachable!(),
                    };

                    let impl_ref = self.impls.get_mut(&impl_key).unwrap();
                    impl_ref.variables.insert(iden, (new_name, dependant));
                    if !impl_ref.traits.contains(&resolved_trait.text) {
                        impl_ref.traits.push(resolved_trait.text.clone());
                    }
                    if let Some(trait_def) = self.trait_defs.get(&resolved_trait.text) {
                        for implied in &trait_def.implied_traits {
                            if !impl_ref.traits.contains(implied) {
                                impl_ref.traits.push(implied.clone());
                            }
                        }
                    }

                    statements.push(dec);
                }

                if let Some(prev) = previous_self {
                    self.scopes
                        .get_mut(scope)
                        .unwrap()
                        .mappings
                        .insert(String::from("Self"), prev);
                }
                if let Some(scope_ref) = self.scopes.get_mut(scope) {
                    for (name, prev) in prev_generics {
                        if let Some(prev) = prev {
                            scope_ref.mappings.insert(name, prev);
                        } else {
                            scope_ref.mappings.remove(&name);
                        }
                    }
                }

                Ok(MiddleNode {
                    node_type: MiddleNodeType::ScopeDeclaration {
                        body: statements,
                        create_new_scope: false,
                        is_temp: false,
                        scope_id: *scope,
                    },
                    span: node.span,
                })
            }
            NodeType::TraitDeclaration {
                identifier,
                implied_traits,
                members,
            } => {
                let mut generic_names = Vec::new();
                let base_name = match &identifier {
                    PotentialGenericTypeIdentifier::Identifier(x) => x.to_string(),
                    PotentialGenericTypeIdentifier::Generic {
                        identifier,
                        generic_types,
                    } => {
                        for t in generic_types {
                            if let PotentialNewType::DataType(ParserDataType {
                                data_type: ParserInnerType::Struct(s),
                                ..
                            }) = t
                            {
                                generic_names.push(s.clone());
                            }
                        }
                        identifier.to_string()
                    }
                };

                let new_name = get_disamubiguous_name(scope, Some(base_name.as_str()), None);

                self.objects.insert(
                    new_name.clone(),
                    MiddleObject {
                        object_type: MiddleTypeDefType::Trait,
                        variables: FxHashMap::default(),
                        traits: Vec::new(),
                        location: self.current_location.clone(),
                    },
                );

                if let Some(scope_ref) = self.scopes.get_mut(scope) {
                    scope_ref.mappings.insert(base_name, new_name.clone());
                }

                let mut prev_generics = Vec::new();
                if let Some(scope_ref) = self.scopes.get_mut(scope) {
                    for name in &generic_names {
                        prev_generics.push((name.clone(), scope_ref.mappings.get(name).cloned()));
                        scope_ref.mappings.insert(name.clone(), name.clone());
                    }
                }

                let mut trait_members = FxHashMap::default();
                let mut assoc_types = FxHashMap::default();
                for member in members {
                    match member.kind {
                        calibre_parser::ast::TraitMemberKind::Type => {
                            let data_type =
                                self.resolve_potential_new_type(scope, member.data_type);
                            assoc_types.insert(member.identifier.to_string(), data_type);
                        }
                        calibre_parser::ast::TraitMemberKind::Const => {
                            let data_type =
                                self.resolve_potential_new_type(scope, member.data_type);
                            trait_members.insert(
                                member.identifier.to_string(),
                                MiddleTraitMember {
                                    data_type,
                                    default: member.value.map(|x| *x),
                                },
                            );
                        }
                    }
                }

                let mut implied = Vec::new();
                for imp in implied_traits {
                    let resolved = self
                        .resolve_dollar_ident_only(scope, &imp)
                        .map(|x| x.text)
                        .unwrap_or_else(|| imp.to_string());
                    implied.push(resolved);
                }

                self.trait_defs.insert(
                    new_name.clone(),
                    MiddleTrait {
                        implied_traits: implied,
                        members: trait_members,
                        assoc_types,
                    },
                );

                if let Some(scope_ref) = self.scopes.get_mut(scope) {
                    for (name, prev) in prev_generics {
                        if let Some(prev) = prev {
                            scope_ref.mappings.insert(name, prev);
                        } else {
                            scope_ref.mappings.remove(&name);
                        }
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
            } => self.evaluate_scope_alias(scope, node.span, identifier, value, create_new_scope),
            NodeType::ScopeDeclaration {
                body,
                named,
                is_temp,
                create_new_scope,
                define,
            } => self.evaluate_scope_declaration(
                scope,
                node.span,
                body,
                named,
                create_new_scope,
                define,
                is_temp,
            ),
            NodeType::StructLiteral { identifier, value } => Ok(MiddleNode {
                node_type: MiddleNodeType::AggregateExpression {
                    identifier: Some({
                        match &identifier {
                            calibre_parser::ast::PotentialGenericTypeIdentifier::Generic {
                                identifier: base,
                                generic_types,
                            } => {
                                let base = self.resolve_dollar_ident_only(scope, base).unwrap();
                                let concrete: Vec<ParserDataType> = generic_types
                                    .iter()
                                    .map(|g| self.resolve_potential_new_type(scope, g.clone()))
                                    .collect();
                                if let Some((tpl_params, _, _)) =
                                    self.generic_type_templates.get(&base.text)
                                {
                                    let tpl_params = tpl_params.clone();
                                    if let Some(spec) = self.ensure_specialized_type(
                                        scope,
                                        &base.text,
                                        &tpl_params,
                                        &concrete,
                                    ) {
                                        ParserText::from(spec)
                                    } else {
                                        ParserText::from(base.text)
                                    }
                                } else {
                                    ParserText::from(base.text)
                                }
                            }
                            _ => self
                                .resolve_potential_generic_ident(scope, &identifier)
                                .unwrap(),
                        }
                    }),
                    value: ObjectMap(match value {
                        ObjectType::Map(x) => {
                            let mut map = Vec::new();

                            for itm in x {
                                map.push((itm.0, self.evaluate(scope, itm.1)));
                            }

                            map
                        }
                        ObjectType::Tuple(x) => {
                            let mut map = Vec::new();

                            for itm in x.into_iter().enumerate() {
                                map.push((itm.0.to_string(), self.evaluate(scope, itm.1)));
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
                    if let Some(x) = self.resolve_potential_generic_ident(scope, &identifier) {
                        x
                    } else {
                        return Err(MiddleErr::At(
                            node.span,
                            Box::new(MiddleErr::Object(identifier.to_string())),
                        ));
                    };
                let value = self.resolve_dollar_ident_only(scope, &value).unwrap();

                if let Some(object) = self.objects.get(&identifier.text) {
                    let var = object.variables.get(&value.text).map(|x| x.0.clone());

                    if let Some(var) = var {
                        return self.evaluate_inner(
                            scope,
                            Node::new(
                                self.current_span(),
                                NodeType::Identifier(PotentialGenericTypeIdentifier::Identifier(
                                    ParserText::from(var).into(),
                                )),
                            ),
                        );
                    }
                }

                Ok(MiddleNode {
                    node_type: MiddleNodeType::EnumExpression {
                        identifier,
                        value,
                        data: if let Some(data) = data {
                            Some(Box::new(self.evaluate_inner(scope, *data)?))
                        } else {
                            None
                        },
                    },
                    span: node.span,
                })
            }
            NodeType::MatchStatement { value, body } => {
                self.evaluate_match_statement(scope, node.span, value, body)
            }
            NodeType::FnMatchDeclaration { header, body } => self.evaluate_inner(
                scope,
                Node::new(
                    self.current_span(),
                    NodeType::FunctionDeclaration {
                        body: Box::new(Node::new(
                            self.current_span(),
                            NodeType::ScopeDeclaration {
                                body: Some(vec![Node::new(
                                    self.current_span(),
                                    NodeType::MatchStatement {
                                        value: Some(Box::new(Node::new(
                                            self.current_span(),
                                            NodeType::Identifier(
                                                PotentialGenericTypeIdentifier::Identifier(
                                                    header.parameters[0].0.clone(),
                                                ),
                                            ),
                                        ))),
                                        body,
                                    },
                                )]),
                                named: None,
                                is_temp: true,
                                create_new_scope: Some(false),
                                define: false,
                            },
                        )),
                        header: FunctionHeader {
                            param_destructures: Vec::new(),
                            ..header
                        },
                    },
                ),
            ),
            NodeType::FunctionDeclaration { header, body } => {
                self.evaluate_function_declaration(scope, node.span, header, *body)
            }
            NodeType::ExternFunctionDeclaration {
                abi,
                identifier,
                parameters,
                return_type,
                library,
                symbol,
            } => self.evaluate_extern_function(
                scope,
                node.span,
                abi,
                identifier,
                parameters,
                return_type,
                library,
                symbol,
            ),
            NodeType::CallExpression {
                string_fn: _,
                caller,
                generic_types,
                args,
                reverse_args,
            } => self.evaluate_call_expression(
                scope,
                node.span,
                *caller,
                generic_types,
                args,
                reverse_args,
            ),
            NodeType::ImportStatement {
                module,
                alias,
                values,
            } => {
                let values = {
                    let mut lst = Vec::new();

                    for val in values {
                        if val.to_string() == "*" {
                            lst.push(ParserText::new(*val.span(), val.to_string()));
                        } else {
                            lst.push(self.resolve_potential_dollar_ident(scope, &val).unwrap());
                        }
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
                            return Err(MiddleErr::At(
                                key.span,
                                Box::new(MiddleErr::CantImport(format!("{} at {:?}", key, module))),
                            ));
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
            NodeType::PipeExpression(mut path) if !path.is_empty() => {
                let mut value = path.remove(0).into();
                let mut prior_mappings = FxHashMap::default();

                prior_mappings.insert(
                    "$".to_string(),
                    self.scopes
                        .get(scope)
                        .unwrap()
                        .mappings
                        .get("$")
                        .map(|x| x.to_string()),
                );

                for point in path.into_iter() {
                    let resolved_type = self
                        .resolve_type_from_node(scope, point.get_node())
                        .map(|x| x.unwrap_all_refs().data_type);

                    match resolved_type {
                        Some(ParserInnerType::Function { .. })
                        | Some(ParserInnerType::NativeFunction(_))
                            if !point.is_named() && !point.get_node().node_type.is_call() =>
                        {
                            value = Node::new(
                                self.current_span(),
                                NodeType::CallExpression {
                                    string_fn: None,
                                    caller: Box::new(point.into()),
                                    generic_types: Vec::new(),
                                    args: vec![CallArg::Value(value)],
                                    reverse_args: Vec::new(),
                                },
                            )
                        }
                        _ => {
                            let keep_scope = point.is_named();
                            let var_dec = match &point {
                                calibre_parser::ast::PipeSegment::Named { identifier, .. } => {
                                    let ident =
                                        self.resolve_dollar_ident_only(scope, identifier).unwrap();

                                    prior_mappings.insert(
                                        ident.text.clone(),
                                        self.scopes
                                            .get(scope)
                                            .unwrap()
                                            .mappings
                                            .get(&ident.text)
                                            .map(|x| x.to_string()),
                                    );

                                    Node::new(
                                        self.current_span(),
                                        NodeType::VariableDeclaration {
                                            var_type: VarType::Mutable,
                                            identifier: ident.into(),
                                            value: Box::new(value),
                                            data_type: PotentialNewType::DataType(
                                                ParserDataType::from(ParserInnerType::Auto(None)),
                                            ),
                                        },
                                    )
                                }
                                _ => Node::new(
                                    self.current_span(),
                                    NodeType::VariableDeclaration {
                                        var_type: VarType::Mutable,
                                        identifier: ParserText::from(String::from("$")).into(),
                                        value: Box::new(value),
                                        data_type: PotentialNewType::DataType(
                                            ParserDataType::from(ParserInnerType::Auto(None)),
                                        ),
                                    },
                                ),
                            };

                            let point: Node = point.into();
                            value = match point.node_type {
                                NodeType::ScopeDeclaration {
                                    body: Some(mut body),
                                    named: None,
                                    is_temp,
                                    create_new_scope: _,
                                    define,
                                } => {
                                    body.insert(0, var_dec);

                                    Node {
                                        node_type: NodeType::ScopeDeclaration {
                                            body: Some(body),
                                            named: None,
                                            is_temp,
                                            create_new_scope: Some(!keep_scope),
                                            define,
                                        },
                                        ..point
                                    }
                                }
                                _ => Node::new(
                                    self.current_span(),
                                    NodeType::ScopeDeclaration {
                                        body: Some(vec![var_dec, point]),
                                        named: None,
                                        is_temp: true,
                                        create_new_scope: Some(!keep_scope),
                                        define: false,
                                    },
                                ),
                            }
                        }
                    }
                }

                for (k, v) in prior_mappings {
                    if let Some(v) = v {
                        self.scopes.get_mut(scope).unwrap().mappings.insert(k, v);
                    } else {
                        self.scopes.get_mut(scope).unwrap().mappings.remove(&k);
                    }
                }

                self.evaluate_inner(scope, value)
            }
            NodeType::PipeExpression(_) => Ok(MiddleNode::new(
                MiddleNodeType::EmptyLine,
                self.current_span(),
            )),
        }
    }
}

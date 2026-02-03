use ast::{
    MiddleNode, MiddleNodeType,
    hm::{self, Type, TypeGenerator, TypeScheme},
};
use calibre_parser::{
    ast::{
        CallArg, FunctionHeader, GenericTypes, IfComparisonType, LoopType, MatchArmType, Node,
        NodeType, ObjectMap, ObjectType, ParserDataType, ParserInnerType, ParserText,
        PotentialDollarIdentifier, PotentialGenericTypeIdentifier, PotentialNewType, RefMutability,
        VarType,
        binary::BinaryOperator,
        comparison::{BooleanOperator, ComparisonOperator},
    },
    lexer::Span,
};
use rustc_hash::FxHashMap;
use std::str::FromStr;

use crate::{errors::MiddleErr, infer::infer_node_hm};
use environment::*;

pub mod environment;
pub mod errors;
pub mod infer;
pub mod ast;
pub mod native;

impl MiddleEnvironment {
    pub fn evaluate(&mut self, scope: &u64, node: Node) -> Result<MiddleNode, MiddleErr> {
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
                        let val = self.resolve_macro_arg(scope, &x).unwrap().clone();
                        return self.evaluate(scope, val);
                    } else {
                        return Err(MiddleErr::At(
                            node.span,
                            Box::new(MiddleErr::Variable(x.to_string())),
                        ));
                    },
                ),
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
                        .resolve_potential_generic_ident(scope, x)
                        .map(|x| self.objects.get(&x.text))
                    {
                        match (&object.object_type, &path[1].0.node_type) {
                            (MiddleTypeDefType::Enum(_), NodeType::Identifier(y))
                                if path.len() == 2 =>
                            {
                                return self.evaluate(
                                    scope,
                                    Node::new(self.current_span(), NodeType::EnumExpression {
                                        identifier: x.clone(),
                                        value: y.clone().into(),
                                        data: None,
                                    }),
                                );
                            }
                            (
                                _,
                                NodeType::CallExpression {
                                    string_fn,
                                    caller,
                                    generic_types,
                                    args,
                                    reverse_args,
                                },
                            ) => {
                                let static_fn =
                                    if let NodeType::Identifier(second) = &caller.node_type {
                                        object
                                            .variables
                                            .get(&second.to_string())
                                            .map(|x| x.0.clone())
                                    } else {
                                        None
                                    };

                                if let Some(static_fn) = static_fn {
                                    return self.evaluate(
                                        scope,
                                        Node::new(self.current_span(), NodeType::CallExpression {
                                            string_fn: string_fn.clone(),
                                            caller: Box::new(Node::new(self.current_span(), 
                                                NodeType::Identifier(
                                                    PotentialGenericTypeIdentifier::Identifier(
                                                        ParserText::from(static_fn).into(),
                                                    ),
                                                ),
                                            )),
                                            generic_types: generic_types.clone(),
                                            args: args.clone(),
                                            reverse_args: reverse_args.clone(),
                                        }),
                                    );
                                }
                            }
                            (_, NodeType::Identifier(ident)) => {
                                let ident =
                                    self.resolve_dollar_ident_potential_generic_only(scope, ident);

                                if let Some(ident) = ident {
                                    let var =
                                        object.variables.get(&ident.text).map(|x| x.0.clone());

                                    if let Some(var) = var {
                                        return self.evaluate(
                                            scope,
                                            Node::new(self.current_span(), NodeType::Identifier(
                                                PotentialGenericTypeIdentifier::Identifier(
                                                    ParserText::from(var).into(),
                                                ),
                                            )),
                                        );
                                    }
                                }
                            }
                            _ => {}
                        }
                    }
                }
                let first = path.remove(0);
                let mut list = vec![(self.evaluate(scope, first.0)?, first.1)];

                for (i, item) in path.into_iter().enumerate() {
                    list.push((
                        match item.0.node_type {
                            NodeType::Identifier(_) if item.1 => self.evaluate(scope, item.0)?,
                            NodeType::Identifier(x) if i == 0 => {
                                let first = list.first().unwrap().clone();

                                let x = self
                                    .resolve_dollar_ident_potential_generic_only(scope, &x)
                                    .unwrap();

                                let struct_name =
                                    if let MiddleNodeType::Identifier(x) = first.0.node_type {
                                        if let Some(ParserInnerType::Struct(x)) =
                                            self.variables.get(&x.text).map(|x| {
                                                x.data_type.clone().unwrap_all_refs().data_type
                                            })
                                        {
                                            Some(x.to_string())
                                        } else {
                                            Some(x.text)
                                        }
                                    } else {
                                        None
                                    };

                                if let Some(obj) = &struct_name {
                                    let static_var = if let Some(s) = self.objects.get(obj) {
                                        s.variables.get(&x.text).map(|x| x.0.clone())
                                    } else {
                                        None
                                    };

                                    if let Some(static_var) = static_var {
                                        return self.evaluate(
                                            scope,
                                            Node::new(self.current_span(), NodeType::Identifier(
                                                PotentialGenericTypeIdentifier::Identifier(
                                                    ParserText::from(static_var).into(),
                                                ),
                                            )),
                                        );
                                    }
                                }

                                MiddleNode {
                                    node_type: MiddleNodeType::Identifier(x),
                                    span: node.span,
                                }
                            }
                            NodeType::Identifier(x) => MiddleNode {
                                node_type: MiddleNodeType::Identifier(
                                    self.resolve_dollar_ident_potential_generic_only(scope, &x)
                                        .unwrap(),
                                ),
                                span: node.span,
                            },
                            NodeType::CallExpression {
                                string_fn: _,
                                caller,
                                generic_types,
                                mut args,
                                reverse_args,
                            } if !item.1 => {
                                let first = list.first().unwrap().clone();
                                let node =
                                    MiddleNode::new(
                                        MiddleNodeType::MemberExpression { path: list },
                                        self.current_span(),
                                    );

                                let struct_name = if let Some(ParserInnerType::Struct(x)) = self
                                    .resolve_type_from_node(scope, &node.clone().into())
                                    .map(|x| x.unwrap_all_refs().data_type)
                                {
                                    Some(x.to_string())
                                } else if let MiddleNodeType::Identifier(x) = first.0.node_type {
                                    if let Some(ParserInnerType::Struct(x)) = self
                                        .variables
                                        .get(&x.text)
                                        .map(|x| x.data_type.clone().unwrap_all_refs().data_type)
                                    {
                                        Some(x.to_string())
                                    } else {
                                        Some(x.text)
                                    }
                                } else {
                                    None
                                };

                                if let Some(x) = &struct_name {
                                    let static_fn = if let Some(s) = self.objects.get(x) {
                                        if let NodeType::Identifier(second) = &caller.node_type {
                                            s.variables
                                                .get(&second.to_string())
                                                .map(|x| x.0.clone())
                                        } else {
                                            None
                                        }
                                    } else {
                                        None
                                    };
                                    args.insert(0, CallArg::Value(node.into()));

                                    if let Some(static_fn) = static_fn {
                                        return self.evaluate(
                                            scope,
                                            Node::new(self.current_span(), NodeType::CallExpression {
                                                string_fn: None,
                                                caller: Box::new(Node::new(self.current_span(), 
                                                    NodeType::Identifier(
                                                        PotentialGenericTypeIdentifier::Identifier(
                                                            ParserText::from(static_fn).into(),
                                                        ),
                                                    ),
                                                )),
                                                generic_types,
                                                args,
                                                reverse_args,
                                            }),
                                        );
                                    }
                                }

                                panic!("{:?}", struct_name)
                            }
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
            NodeType::Until { condition } => self.evaluate(
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
                node_type: if self.scopes.get(scope).unwrap().defers.is_empty() {
                    // TODO find a way to get all defers since the last loop was created
                    let mut lst = Vec::new();

                    for x in self.scopes.get(scope).unwrap().defers.clone() {
                        lst.push(self.evaluate(scope, x)?);
                    }

                    for value in self.scopes.get(scope).unwrap().defined.clone() {
                        lst.push(MiddleNode::new(
                            MiddleNodeType::Drop(value.into()),
                            self.current_span(),
                        ))
                    }

                    lst.push(MiddleNode::new(MiddleNodeType::Break, self.current_span()));

                    MiddleNodeType::ScopeDeclaration {
                        body: lst,
                        create_new_scope: false,
                        is_temp: true,
                    }
                } else {
                    MiddleNodeType::Break
                },
                span: node.span,
            }),
            NodeType::Continue => Ok(MiddleNode {
                node_type: if self.scopes.get(scope).unwrap().defers.is_empty() {
                    let mut lst = Vec::new();

                    for x in self.scopes.get(scope).unwrap().defers.clone() {
                        lst.push(self.evaluate(scope, x)?);
                    }

                    for value in self.scopes.get(scope).unwrap().defined.clone() {
                        lst.push(MiddleNode::new(
                            MiddleNodeType::Drop(value.into()),
                            self.current_span(),
                        ))
                    }

                    lst.push(MiddleNode::new(MiddleNodeType::Continue, self.current_span()));

                    MiddleNodeType::ScopeDeclaration {
                        body: lst,
                        create_new_scope: false,
                        is_temp: true,
                    }
                } else {
                    MiddleNodeType::Continue
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
                        // TODO get all defers in the function body
                        if self.scopes.get(scope).unwrap().defers.is_empty() {
                            if let Some(value) = value {
                                Some(Box::new(self.evaluate(scope, *value)?))
                            } else {
                                None
                            }
                        } else {
                            let mut lst = Vec::new();

                            let mut in_return = Vec::new();

                            let value = if let Some(x) = value {
                                let x = self.evaluate(scope, *x)?;
                                in_return = x
                                    .identifiers_used()
                                    .into_iter()
                                    .map(|x| x.to_string())
                                    .collect();
                                Some(x)
                            } else {
                                None
                            };

                            for x in self.scopes.get(scope).unwrap().defers.clone() {
                                lst.push(self.evaluate(scope, x)?);
                            }

                            for x in self.func_defers.clone() {
                                lst.push(self.evaluate(scope, x)?);
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

                            if let Some(x) = value {
                                lst.push(x);
                            }

                            Some(Box::new(MiddleNode::new(
                                MiddleNodeType::ScopeDeclaration {
                                    body: lst,
                                    create_new_scope: false,
                                    is_temp: true,
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
                let new_name = if identifier.text.contains("->") {
                    identifier.text.clone()
                } else {
                    self.resolve_str(scope, &identifier.text)
                        .unwrap_or_else(|| {
                            get_disamubiguous_name(
                                scope,
                                Some(identifier.text.trim()),
                                Some(&var_type),
                            )
                        })
                };

                let original_value_node = *value.clone();

                if let NodeType::FunctionDeclaration {
                    ref header,
                    ref body,
                } = original_value_node.node_type
                {
                    if !header.generics.0.is_empty() {
                        let base_name = new_name.clone();
                        let template_params: Vec<String> = header
                            .generics
                            .0
                            .iter()
                            .map(|g| g.identifier.to_string())
                            .collect();
                        self.generic_fn_templates.entry(base_name).or_insert((
                            template_params,
                            header.clone(),
                            (**body).clone(),
                        ));
                    }
                }

                let mut data_type = if data_type.is_auto() {
                    self.resolve_type_from_node(scope, &value)
                        .unwrap_or(self.resolve_potential_new_type(scope, data_type))
                } else {
                    self.resolve_potential_new_type(scope, data_type)
                };

                let val = if self.resolve_str(scope, &identifier.text).is_some() {
                    Some(self.evaluate(scope, *value.clone())?)
                } else {
                    None
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

                if let NodeType::FunctionDeclaration { ref header, .. } =
                    original_value_node.node_type
                {
                    let mut tg = TypeGenerator::default();

                    let ret_pd = self.resolve_potential_new_type(scope, header.return_type.clone());
                    let mut ret_hm = if matches!(ret_pd.data_type, ParserInnerType::Auto(_)) {
                        tg.fresh()
                    } else {
                        hm::from_parser_data_type(&ret_pd, &mut tg)
                    };

                    for param in header.parameters.iter().rev() {
                        let p_pd = self.resolve_potential_new_type(scope, param.1.clone());
                        let p_hm = if matches!(p_pd.data_type, ParserInnerType::Auto(_)) {
                            tg.fresh()
                        } else {
                            hm::from_parser_data_type(&p_pd, &mut tg)
                        };

                        ret_hm = Type::TArrow(Box::new(p_hm), Box::new(ret_hm));
                    }

                    if !self.hm_env.contains_key(&new_name) {
                        self.hm_env
                            .insert(new_name.clone(), TypeScheme::new(Vec::new(), ret_hm));
                    }
                }

                let mut value = if let Some(x) = val {
                    x
                } else {
                    if let NodeType::FunctionDeclaration { ref header, .. } =
                        original_value_node.node_type
                    {
                        let new_scope = self.new_scope_from_parent_shallow(*scope);

                        for param in header.parameters.iter() {
                            let og_name = self.resolve_dollar_ident_only(scope, &param.0).unwrap();
                            let new_name = get_disamubiguous_name(
                                scope,
                                Some(og_name.trim()),
                                Some(&VarType::Mutable),
                            );
                            let data_type = self.resolve_potential_new_type(scope, param.1.clone());

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

                            self.scopes
                                .get_mut(&new_scope)
                                .unwrap()
                                .defined
                                .push(new_name.clone());
                        }

                        self.evaluate(&new_scope, *value)?
                    } else {
                        self.evaluate(scope, *value)?
                    }
                };

                fn contains_auto(pd: &ParserDataType) -> bool {
                    match &pd.data_type {
                        ParserInnerType::Auto(_) => true,
                        ParserInnerType::Tuple(xs) => xs.iter().any(|x| contains_auto(x)),
                        ParserInnerType::List(x) => contains_auto(x),
                        ParserInnerType::Option(x) => contains_auto(x),
                        ParserInnerType::Result { ok, err } => {
                            contains_auto(ok) || contains_auto(err)
                        }
                        ParserInnerType::Function {
                            return_type,
                            parameters,
                            ..
                        } => {
                            contains_auto(return_type)
                                || parameters.iter().any(|p| contains_auto(p))
                        }
                        ParserInnerType::Ref(x, _) => contains_auto(x),
                        ParserInnerType::StructWithGenerics { generic_types, .. } => {
                            generic_types.iter().any(|g| contains_auto(g))
                        }
                        ParserInnerType::Scope(xs) => xs.iter().any(|x| contains_auto(x)),
                        _ => false,
                    }
                }

                if data_type.is_auto() || contains_auto(&data_type) {
                    if let Some((hm_t, subst)) = infer_node_hm(self, scope, &original_value_node) {
                        let t_applied = hm::apply_subst(&subst, &hm_t);

                        let mut tenv: FxHashMap<String, hm::TypeScheme> = FxHashMap::default();
                        for (k, s) in self.hm_env.iter() {
                            tenv.insert(k.clone(), s.clone());
                        }

                        let parser_ty = hm::to_parser_data_type(&t_applied);
                        let scheme = match &parser_ty.data_type {
                            calibre_parser::ast::ParserInnerType::Function { .. }
                                if contains_auto(&parser_ty) =>
                            {
                                hm::TypeScheme::new(Vec::new(), t_applied.clone())
                            }
                            _ => hm::generalize(&tenv, &t_applied),
                        };
                        self.hm_env.insert(new_name.clone(), scheme);

                        if let Some(v) = self.variables.get_mut(&new_name) {
                            v.data_type = parser_ty.clone();
                            data_type = parser_ty.clone();

                            if let MiddleNodeType::FunctionDeclaration {
                                parameters: ref mut params,
                                return_type: ref mut ret_type,
                                ..
                            } = value.node_type
                            {
                                if let ParserInnerType::Function {
                                    return_type: inferred_ret,
                                    parameters: inferred_params,
                                    is_async: _,
                                } = parser_ty.data_type
                                {
                                    for (i, (_name, p_ty)) in params.iter_mut().enumerate() {
                                        if i < inferred_params.len() {
                                            *p_ty = inferred_params[i].clone();
                                        }
                                    }

                                    *ret_type = *inferred_ret.clone();
                                }
                            }
                        }
                    }
                }

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
                                {
                                    if x == new_name {
                                        contains = true;
                                    }
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
                        left: Box::new(self.evaluate(scope, *left)?),
                        right: Box::new(self.evaluate(scope, *right)?),
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
                        left: Box::new(self.evaluate(scope, *left)?),
                        right: Box::new(self.evaluate(scope, *right)?),
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
                        left: Box::new(self.evaluate(scope, *left)?),
                        right: Box::new(self.evaluate(scope, *right)?),
                        operator,
                    },
                    span: node.span,
                })
            }
            NodeType::NotExpression { value } => self.evaluate(
                scope,
                Node {
                    node_type: NodeType::ComparisonExpression {
                        left: value,
                        right: Box::new(Node::new(self.current_span(), NodeType::Identifier(
                            ParserText::from("false".to_string()).into(),
                        ))),
                        operator: ComparisonOperator::Equal,
                    },
                    span: node.span,
                },
            ),
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
            NodeType::InDeclaration { identifier, value } => self.evaluate(
                scope,
                Node::new(self.current_span(), NodeType::CallExpression {
                    string_fn: None,
                    caller: Box::new(Node::new(self.current_span(), NodeType::Identifier(
                        ParserText::from("contains".to_string()).into(),
                    ))),
                    generic_types: Vec::new(),
                    args: vec![CallArg::Value(*value), CallArg::Value(*identifier)],
                    reverse_args: Vec::new(),
                }),
            ),
            NodeType::DebugExpression { value } => Ok(MiddleNode {
                node_type: MiddleNodeType::DebugExpression {
                    pretty_printed_str: value.to_string(),
                    value: Box::new(self.evaluate(scope, *value)?),
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
                    lst.push(self.evaluate(scope, item)?);
                }

                Ok(MiddleNode {
                    node_type: MiddleNodeType::ListLiteral(data_type, lst),
                    span: node.span,
                })
            }
            NodeType::Try { value, catch } => {
                let resolved_type = self.resolve_type_from_node(scope, &value);

                self.evaluate(
                    scope,
                    Node {
                        node_type: NodeType::MatchStatement  {
                            value: Some(value),
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
                                                node_type: NodeType::Return { value: Some(Box::new(Node::new(self.current_span(), NodeType::CallExpression{string_fn : None,generic_types: Vec::new(),caller : Box::new(Node::new(self.current_span(), NodeType::Identifier(ParserText::from(String::from("none")).into()))), args : Vec::new(), reverse_args: Vec::new(),}))) },
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
                mut body,
                until,
            } => {
                let scope = self.new_scope_from_parent_shallow(*scope);

                let wrap_body = |target_body: Box<Node>, injection: Node, at_start: bool| {
                    let mut instructions = match target_body.node_type {
                        NodeType::ScopeDeclaration { body: Some(b), .. } => b,
                        _ => vec![*target_body],
                    };
                    if at_start {
                        instructions.insert(0, injection);
                    } else {
                        instructions.push(injection);
                    }

                    Box::new(Node::new(self.current_span(), NodeType::ScopeDeclaration {
                        body: Some(instructions),
                        is_temp: true,
                        create_new_scope: Some(true),
                        define: false,
                        named: None,
                    }))
                };

                if let Some(cond) = until {
                    let until_node = Node::new(self.current_span(), NodeType::Until { condition: cond });
                    body = wrap_body(body, until_node, false);
                }

                match *loop_type {
                    LoopType::Loop => Ok(MiddleNode {
                        node_type: MiddleNodeType::LoopDeclaration {
                            state: None,
                            body: Box::new(self.evaluate(&scope, *body)?),
                        },
                        span: node.span,
                    }),

                    LoopType::While(condition) => {
                        let break_if_not = Node::new(self.current_span(), NodeType::IfStatement {
                            comparison: Box::new(IfComparisonType::If(Node::new(self.current_span(), 
                                NodeType::NotExpression {
                                    value: Box::new(condition),
                                },
                            ))),
                            then: Box::new(Node::new(self.current_span(), NodeType::Break)),
                            otherwise: None,
                        });

                        Ok(MiddleNode {
                            node_type: MiddleNodeType::LoopDeclaration {
                                state: None,
                                body: Box::new(
                                    self.evaluate(&scope, *wrap_body(body, break_if_not, true))?,
                                ),
                            },
                            span: node.span,
                        })
                    }

                    LoopType::Let { value, pattern } => Ok(MiddleNode {
                        node_type: MiddleNodeType::LoopDeclaration {
                            state: None,
                            body: Box::new(self.evaluate(
                                &scope,
                                Node::new(self.current_span(), NodeType::IfStatement {
                                    comparison: Box::new(IfComparisonType::IfLet {
                                        value,
                                        pattern,
                                    }),
                                    then: body,
                                    otherwise: Some(Box::new(Node::new(self.current_span(), NodeType::Break))),
                                }),
                            )?),
                        },
                        span: node.span,
                    }),

                    LoopType::For(name, range) => {
                        let range_dt = self.resolve_type_from_node(&scope, &range);
                        let idx_id: PotentialDollarIdentifier =
                            ParserText::from("anon_loop_index".to_string()).into();

                        let state = Some(Box::new(self.evaluate(
                            &scope,
                            Node::new(self.current_span(), NodeType::VariableDeclaration {
                                var_type: VarType::Mutable,
                                identifier: idx_id.clone(),
                                value: Box::new(Node::new(self.current_span(), NodeType::CallExpression {
                                    string_fn: None,
                                    caller: Box::new(Node::new(self.current_span(), NodeType::Identifier(
                                        ParserText::from("min_or_zero".to_string()).into(),
                                    ))),
                                    generic_types: vec![],
                                    args: vec![CallArg::Value(range.clone())],
                                    reverse_args: vec![],
                                })),
                                data_type: ParserDataType::from(ParserInnerType::Int).into(),
                            }),
                        )?));

                        let break_node = Node::new(self.current_span(), NodeType::IfStatement {
                            comparison: Box::new(IfComparisonType::If(Node::new(self.current_span(), 
                                NodeType::ComparisonExpression {
                                    left: Box::new(Node::new(self.current_span(), NodeType::Identifier(idx_id.clone().into()))),
                                    right: Box::new(Node::new(self.current_span(), NodeType::CallExpression {
                                        string_fn: None,
                                        caller: Box::new(Node::new(self.current_span(), NodeType::Identifier(ParserText::from("len".to_string()).into()))),
                                        generic_types: vec![],
                                        args: vec![CallArg::Value(range.clone())],
                                        reverse_args: vec![],
                                    })),
                                    operator: calibre_parser::ast::comparison::ComparisonOperator::GreaterEqual,
                                }
                            ))),
                            then: Box::new(Node::new(self.current_span(), NodeType::Break)),
                            otherwise: None,
                        });

                        let var_name_node = Node::new(self.current_span(), NodeType::VariableDeclaration {
                            identifier: name,
                            var_type: VarType::Mutable,
                            data_type: PotentialNewType::DataType(ParserDataType::from(
                                ParserInnerType::Auto(None),
                            )),
                            value: match range_dt.map(|x| x.data_type) {
                                Some(ParserInnerType::List(_)) => {
                                    Box::new(Node::new(self.current_span(), NodeType::MemberExpression {
                                        path: vec![
                                            (range.clone(), false),
                                            (
                                                Node::new(self.current_span(), NodeType::Identifier(
                                                    idx_id.clone().into(),
                                                )),
                                                true,
                                            ),
                                        ],
                                    }))
                                }
                                Some(ParserInnerType::Tuple(_)) => {
                                    Box::new(Node::new(self.current_span(), NodeType::MemberExpression {
                                        path: vec![
                                            (range.clone(), false),
                                            (
                                                Node::new(self.current_span(), NodeType::Identifier(
                                                    idx_id.clone().into(),
                                                )),
                                                false,
                                            ),
                                        ],
                                    }))
                                }
                                _ => Box::new(Node::new(self.current_span(), NodeType::Identifier(
                                    idx_id.clone().into(),
                                ))),
                            },
                        });

                        let increment_node = Node::new(self.current_span(), NodeType::AssignmentExpression {
                            identifier: Box::new(Node::new(self.current_span(), NodeType::Identifier(
                                idx_id.clone().into(),
                            ))),
                            value: Box::new(Node::new(self.current_span(), NodeType::BinaryExpression {
                                left: Box::new(Node::new(self.current_span(), NodeType::Identifier(
                                    idx_id.into(),
                                ))),
                                right: Box::new(Node::new(self.current_span(), NodeType::IntLiteral(1))),
                                operator: BinaryOperator::Add,
                            })),
                        });

                        let mut instructions = match body.node_type {
                            NodeType::ScopeDeclaration { body: Some(b), .. } => b,
                            _ => vec![*body],
                        };

                        instructions.insert(0, var_name_node);
                        instructions.insert(0, break_node);
                        instructions.push(increment_node);

                        let final_body = Node::new(self.current_span(), NodeType::ScopeDeclaration {
                            body: Some(instructions),
                            is_temp: true,
                            create_new_scope: Some(true),
                            define: false,
                            named: None,
                        });

                        Ok(MiddleNode {
                            node_type: MiddleNodeType::LoopDeclaration {
                                state,
                                body: Box::new(self.evaluate(&scope, final_body)?),
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
                until,
            } => {
                let resolved_data_type = if data_type.is_auto() {
                    self.resolve_type_from_node(scope, &map)
                        .unwrap_or(self.resolve_potential_new_type(scope, data_type.clone()))
                } else {
                    self.resolve_potential_new_type(scope, data_type.clone())
                };

                let node = Node {
                    node_type: NodeType::ScopeDeclaration {
                        body: Some(vec![
                            Node::new(self.current_span(), NodeType::VariableDeclaration {
                                var_type: VarType::Mutable,
                                identifier: ParserText::from(String::from("anon_iter_list")).into(),
                                value: Box::new(Node::new(self.current_span(), NodeType::ListLiteral(
                                    data_type.clone(),
                                    Vec::new(),
                                ))),
                                data_type: ParserDataType::from(ParserInnerType::List(Box::new(
                                    resolved_data_type,
                                )))
                                .into(),
                            }),
                            Node::new(self.current_span(), NodeType::LoopDeclaration {
                                loop_type,
                                until,
                                body: Box::new(Node::new(self.current_span(), NodeType::ScopeDeclaration {
                                    body: {
                                        let mut lst = Vec::new();

                                        for condition in conditionals {
                                            lst.push(Node::new(self.current_span(), NodeType::IfStatement {
                                                comparison: Box::new(IfComparisonType::If(
                                                    Node::new(self.current_span(), NodeType::NegExpression {
                                                        value: Box::new(condition),
                                                    }),
                                                )),
                                                then: Box::new(Node::new(self.current_span(), 
                                                    NodeType::Continue,
                                                )),
                                                otherwise: None,
                                            }));
                                        }

                                        lst.push(Node::new(self.current_span(), 
                                                NodeType::AssignmentExpression {
                                                    identifier: Box::new(Node::new(self.current_span(), 
                                                        NodeType::Identifier(ParserText::from(
                                                            String::from("anon_iter_list"),
                                                        ).into()),
                                                    )),
                                                    value: Box::new(Node::new(self.current_span(), NodeType::BinaryExpression { left: Box::new(Node::new(self.current_span(), 
                                                        NodeType::Identifier(ParserText::from(
                                                            String::from("anon_iter_list"),
                                                        ).into()),
                                                    )), right: map, operator: calibre_parser::ast::binary::BinaryOperator::Shl })),
                                                },
                                            ));

                                        Some(lst)
                                    },
                                    create_new_scope: Some(true),
                                    define: false,
                                    named: None,
                                    is_temp: true,
                                })),
                            }),
                            Node::new(self.current_span(), NodeType::Identifier(
                                ParserText::from(String::from("anon_iter_list")).into(),
                            )),
                        ]),
                        create_new_scope: Some(true),
                        define: false,
                        named: None,
                        is_temp: true,
                    },
                    span: node.span,
                };

                self.evaluate(scope, node)
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
                                then: Box::new(Node::new(self.current_span(), 
                                    NodeType::AssignmentExpression {
                                        identifier: then,
                                        value: value.clone(),
                                    },
                                )),
                                otherwise: Some(Box::new(Node::new(self.current_span(), 
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
                variables,
            } => {
                let resolved = self
                    .resolve_potential_generic_ident(scope, &identifier)
                    .unwrap();

                let previous_self = self
                    .scopes
                    .get_mut(scope)
                    .unwrap()
                    .mappings
                    .insert(String::from("Self"), resolved.text.clone());

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
                                let resolved_iden =
                                    format!("{}::{}", resolved, identifier);

                                dependant = match &value.node_type {
                                    NodeType::FunctionDeclaration { header, body: _ } => {
                                        if let Some(PotentialNewType::DataType(param)) =
                                            header.parameters.first().map(|x| x.1.clone())
                                        {
                                            if let ParserInnerType::Struct(x) = self
                                                .resolve_data_type(scope, param)
                                                .unwrap_all_refs()
                                                .data_type
                                            {
                                                x.trim() == resolved.trim()
                                            } else {
                                                false
                                            }
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
                            _ => unreachable!(),
                        },
                    };

                    let dec = self.evaluate(scope, dec)?;
                    let new_name = match &dec.node_type {
                        MiddleNodeType::VariableDeclaration { identifier, .. } => {
                            identifier.text.clone()
                        }
                        _ => unreachable!(),
                    };
                    self.objects
                        .get_mut(&resolved.text)
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

                Ok(MiddleNode {
                    node_type: MiddleNodeType::ScopeDeclaration {
                        body: statements,
                        create_new_scope: false,
                        is_temp: false,
                    },
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

                if let Some(mut body) = body {
                    if is_temp {
                        let last = body.pop();
                        for statement in body.into_iter() {
                            stmts.push(self.evaluate(&new_scope, statement)?);
                        }

                        let mut in_return = Vec::new();

                        let last = if let Some(x) = last {
                            let x = self.evaluate(&new_scope, x)?;
                            in_return = x
                                .identifiers_used()
                                .into_iter()
                                .map(|x| x.to_string())
                                .collect();
                            Some(x)
                        } else {
                            None
                        };

                        for x in self.scopes.get(&new_scope).unwrap().defers.clone() {
                            stmts.push(self.evaluate(&new_scope, x)?);
                        }

                        for value in self
                            .scopes
                            .get(&new_scope)
                            .unwrap()
                            .defined
                            .clone()
                            .into_iter()
                            .filter(|x| !in_return.contains(&&x))
                        {
                            stmts.push(MiddleNode::new(
                                MiddleNodeType::Drop(value.into()),
                                self.current_span(),
                            ))
                        }

                        if let Some(last) = last {
                            stmts.push(last);
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
                        {
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
                        }
                        .into(),
                    ),
                    value: ObjectMap(match value {
                        ObjectType::Map(x) => {
                            let mut map = Vec::new();

                            for itm in x {
                                map.push((itm.0, self.evaluate(scope, itm.1)?));
                            }

                            map
                        }
                        ObjectType::Tuple(x) => {
                            let mut map = Vec::new();

                            for itm in x.into_iter().enumerate() {
                                map.push((itm.0.to_string(), self.evaluate(scope, itm.1)?));
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
                        return self.evaluate(
                            scope,
                            Node::new(self.current_span(), NodeType::Identifier(
                                PotentialGenericTypeIdentifier::Identifier(
                                    ParserText::from(var).into(),
                                ),
                            )),
                        );
                    }
                }

                Ok(MiddleNode {
                    node_type: MiddleNodeType::EnumExpression {
                        identifier,
                        value,
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
                let resolved_data_type = if let Some(value) = value.as_ref() {
                    self.resolve_type_from_node(scope, value)
                } else {
                    None
                };

                let mut ifs: Vec<Node> = Vec::new();
                let mut reference = None;
                let enum_object = if let Some(resolved_data_type) = &resolved_data_type {
                    if let Some(x) = self.objects.get(
                        resolved_data_type
                            .to_string()
                            .replace("mut ", "")
                            .replace("&", "")
                            .trim(),
                    ) {
                        match (
                            resolved_data_type.to_string().contains("mut "),
                            resolved_data_type.to_string().contains("&"),
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
                    }
                } else {
                    None
                };
                for mut pattern in body {
                    let mut conditionals = if pattern.1.is_empty() {
                        Node::new(self.current_span(), NodeType::Identifier(
                            ParserText::from("true".to_string()).into(),
                        ))
                    } else {
                        pattern.1.remove(0)
                    };

                    for condition in pattern.1 {
                        conditionals = Node::new(self.current_span(), NodeType::BooleanExpression {
                            left: Box::new(conditionals),
                            right: Box::new(condition),
                            operator: BooleanOperator::And,
                        });
                    }

                    let (Some(value), Some(resolved_data_type)) =
                        (value.clone(), resolved_data_type.as_ref())
                    else {
                        match pattern.0 {
                            MatchArmType::Wildcard(_) => {
                                ifs.push(Node::new(self.current_span(), NodeType::IfStatement {
                                    comparison: Box::new(IfComparisonType::If(conditionals)),
                                    then: pattern.2,
                                    otherwise: None,
                                }))
                            }
                            MatchArmType::Value(x) => {
                                conditionals = Node::new(self.current_span(), NodeType::BooleanExpression {
                                    left: Box::new(x),
                                    right: Box::new(conditionals),
                                    operator: BooleanOperator::And,
                                });

                                ifs.push(Node::new(self.current_span(), NodeType::IfStatement {
                                    comparison: Box::new(IfComparisonType::If(conditionals)),
                                    then: pattern.2,
                                    otherwise: None,
                                }))
                            }
                            _ => unreachable!(),
                        }

                        continue;
                    };

                    match pattern.0 {
                        MatchArmType::Wildcard(_) => {
                            ifs.push(Node::new(self.current_span(), NodeType::IfStatement {
                                comparison: Box::new(IfComparisonType::If(conditionals)),
                                then: pattern.2,
                                otherwise: None,
                            }))
                        }
                        MatchArmType::Value(x) => {
                            ifs.push(Node::new(self.current_span(), NodeType::IfStatement {
                                comparison: Box::new(IfComparisonType::If(Node::new(self.current_span(), 
                                    NodeType::BooleanExpression {
                                        left: Box::new(Node::new(self.current_span(), 
                                            NodeType::ComparisonExpression {
                                                left: value.clone(),
                                                right: Box::new(x),
                                                operator: ComparisonOperator::Equal,
                                            },
                                        )),
                                        right: Box::new(conditionals),
                                        operator: BooleanOperator::And,
                                    },
                                ))),
                                then: pattern.2,
                                otherwise: None,
                            }))
                        }
                        MatchArmType::Let { var_type, name } => {
                            ifs.push(Node::new(self.current_span(), NodeType::IfStatement {
                                comparison: Box::new(IfComparisonType::If(conditionals)),
                                then: Box::new(Node::new(self.current_span(), NodeType::ScopeDeclaration {
                                    body: Some(vec![
                                        Node::new(self.current_span(), NodeType::VariableDeclaration {
                                            var_type,
                                            identifier: name,
                                            value: value.clone(),
                                            data_type: resolved_data_type.clone().into(),
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
                                        return Err(MiddleErr::At(
                                            val.span,
                                            Box::new(MiddleErr::CantMatch(
                                                resolved_data_type.clone(),
                                            )),
                                        ));
                                    };
                                    let Some(index) =
                                        object.iter().position(|x| x.0.text == val.text)
                                    else {
                                        return Err(MiddleErr::At(
                                            val.span,
                                            Box::new(MiddleErr::EnumVariant(val.text)),
                                        ));
                                    };
                                    index as i64
                                }
                                "Ok" => 0,
                                "Err" => 1,
                                "Some" => 0,
                                "None" => 1,
                                _ => {
                                    return Err(MiddleErr::At(
                                        val.span,
                                        Box::new(MiddleErr::CantMatch(
                                            resolved_data_type.clone(),
                                        )),
                                    ));
                                }
                            };

                            ifs.push(Node::new(self.current_span(), NodeType::IfStatement {
                                comparison: Box::new(IfComparisonType::If(Node::new(self.current_span(), NodeType::BooleanExpression{
                                    left : Box::new(Node::new(self.current_span(), NodeType::ComparisonExpression {
                                        left: Box::new(Node::new(self.current_span(), NodeType::CallExpression{string_fn : None, generic_types: Vec::new(), caller : Box::new(Node::new(self.current_span(), NodeType::Identifier(ParserText::from(String::from("discriminant")).into()))), args : vec![CallArg::Value(*value.clone())], reverse_args: Vec::new(),})),
                                        right: Box::new(Node::new(self.current_span(), NodeType::IntLiteral(index))),
                                        operator: ComparisonOperator::Equal
                                    })),
                                    right : Box::new(conditionals),
                                    operator: BooleanOperator::And,
                                }))),
                                then: {Box::new(if let Some(name) = name {
                                    Node::new(self.current_span(), NodeType::ScopeDeclaration {
                                        body: Some(vec![
                                            Node::new(self.current_span(), NodeType::VariableDeclaration {
                                                var_type,
                                                identifier: name,
                                                value: if reference.is_some() && reference != Some(RefMutability::Value) {
                                                    Box::new(Node::new(self.current_span(), NodeType::RefStatement {
                                                        mutability: reference.clone().unwrap(),
                                                        value: Box::new(Node::new(self.current_span(), NodeType::MemberExpression {
                                                            path: vec![
                                                                (*value.clone(), false),
                                                                (Node::new(self.current_span(), NodeType::Identifier(ParserText::from(String::from("next")).into())), false)
                                                            ]
                                                        }))
                                                    }))
                                                }else{
                                                    Box::new(Node::new(self.current_span(), NodeType::MemberExpression {
                                                        path: vec![
                                                            (*value.clone(), false),
                                                            (Node::new(self.current_span(), NodeType::Identifier(ParserText::from(String::from("next")).into())), false)
                                                        ]
                                                    }))
                                                },
                                                data_type: PotentialNewType::DataType(ParserDataType::from(
                                                    ParserInnerType::Auto(None),
                                                )),
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
                    Node::new(self.current_span(), NodeType::EmptyLine)
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
            NodeType::FnMatchDeclaration { header, body } => self.evaluate(
                scope,
                Node::new(self.current_span(), NodeType::FunctionDeclaration {
                    body: Box::new(Node::new(self.current_span(), NodeType::ScopeDeclaration {
                        body: Some(vec![Node::new(self.current_span(), NodeType::MatchStatement {
                            value: Some(Box::new(Node::new(self.current_span(), NodeType::Identifier(
                                PotentialGenericTypeIdentifier::Identifier(
                                    header.parameters[0].0.clone(),
                                ),
                            )))),
                            body,
                        })]),
                        named: None,
                        is_temp: true,
                        create_new_scope: Some(false),
                        define: false,
                    })),
                    header,
                }),
            ),
            NodeType::FunctionDeclaration { header, body } => {
                // TODO Handle generics
                let mut params = Vec::with_capacity(header.parameters.len());
                let mut old_func_defers = Vec::new();
                old_func_defers.append(&mut self.func_defers);
                let new_scope = self.new_scope_from_parent_shallow(*scope);
                for param in header.parameters {
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

                    self.scopes
                        .get_mut(&new_scope)
                        .unwrap()
                        .defined
                        .push(new_name.clone());
                    params.push((ParserText::from(new_name), data_type));
                }

                let return_type = self.resolve_potential_new_type(&new_scope, header.return_type);

                let body = self.evaluate(&new_scope, *body)?;
                let mut func_defers = Vec::new();
                func_defers.append(&mut self.func_defers);

                let body = if let MiddleNodeType::ScopeDeclaration {
                    body: mut scope_body,
                    create_new_scope,
                    is_temp,
                } = body.node_type
                {
                    let last = scope_body.pop();
                    for defer in func_defers {
                        scope_body.push(self.evaluate(scope, defer)?);
                    }

                    if let Some(last) = last {
                        scope_body.push(last);
                    }

                    MiddleNode {
                        span: body.span,
                        node_type: MiddleNodeType::ScopeDeclaration {
                            body: scope_body,
                            create_new_scope,
                            is_temp,
                        },
                    }
                } else {
                    body
                };
                self.func_defers.append(&mut old_func_defers);

                let mut fn_node = MiddleNode {
                    node_type: MiddleNodeType::FunctionDeclaration {
                        parameters: params.clone(),
                        body: Box::new(body.clone()),
                        return_type: return_type.clone(),
                        is_async: header.is_async,
                    },
                    span: node.span,
                };

                let ast_node: Node = fn_node.clone().into();
                for (p_name, _p_ty) in params.iter() {
                    let full = p_name.text.clone();
                    if let Some(idx) = full.rfind(':') {
                        let short = full[idx + 1..].to_string();
                        self.scopes
                            .get_mut(&new_scope)
                            .unwrap()
                            .mappings
                            .insert(short, full.clone());
                    }
                }

                if let Some((hm_t, subst)) = infer_node_hm(self, &new_scope, &ast_node) {
                    let t_applied = hm::apply_subst(&subst, &hm_t);
                    let parser_ty = hm::to_parser_data_type(&t_applied);

                    if let MiddleNodeType::FunctionDeclaration {
                        parameters: ref mut params2,
                        return_type: ref mut ret2,
                        ..
                    } = fn_node.node_type
                    {
                        if let calibre_parser::ast::ParserInnerType::Function {
                            return_type: inferred_ret,
                            parameters: inferred_params,
                            is_async: _,
                        } = parser_ty.data_type
                        {
                            for (i, (name, p_ty)) in params2.iter_mut().enumerate() {
                                if i < inferred_params.len() {
                                    *p_ty = inferred_params[i].clone();
                                    if let Some(var) = self.variables.get_mut(&name.text) {
                                        var.data_type = inferred_params[i].clone();
                                    }
                                }
                            }

                            *ret2 = *inferred_ret.clone();
                        }
                    }
                }

                Ok(fn_node)
            }
            NodeType::CallExpression {
                string_fn,
                caller,
                generic_types,
                mut args,
                mut reverse_args,
            } => {
                if let NodeType::Identifier(caller_ident) = &caller.node_type {
                    if let Some(resolved_caller) =
                        self.resolve_potential_generic_ident(scope, caller_ident)
                    {
                        let base_name = resolved_caller.text.clone();

                        if let Some((tpl_params, header, _body)) =
                            self.generic_fn_templates.get(&base_name).cloned()
                        {
                            let explicit_args: Vec<ParserDataType> = generic_types
                                .iter()
                                .map(|g| self.resolve_potential_new_type(scope, g.clone()))
                                .collect();

                            let concrete_args: Option<Vec<ParserDataType>> =
                                if !explicit_args.is_empty() {
                                    Some(explicit_args)
                                } else {
                                    // Infer from call-site argument types.
                                    let mut all_args: Vec<Node> =
                                        args.iter().cloned().map(|a| a.into()).collect();
                                    all_args.append(&mut reverse_args.clone());
                                    let arg_types: Vec<ParserDataType> = all_args
                                        .iter()
                                        .filter_map(|a| self.resolve_type_from_node(scope, a))
                                        .collect();

                                    let param_types: Vec<ParserDataType> = header
                                        .parameters
                                        .iter()
                                        .filter_map(|(_n, p)| match p {
                                            PotentialNewType::DataType(dt) => Some(dt.clone()),
                                            _ => None,
                                        })
                                        .collect();

                                    if param_types.len() == arg_types.len() {
                                        self.infer_generic_args_from_call(
                                            &tpl_params,
                                            &param_types,
                                            &arg_types,
                                        )
                                    } else {
                                        None
                                    }
                                };

                            if let Some(concrete_args) = concrete_args {
                                if let Some(spec) = self.ensure_specialized_function(
                                    scope,
                                    &base_name,
                                    &tpl_params,
                                    &concrete_args,
                                ) {
                                    return self.evaluate(
                                        scope,
                                        Node::new(self.current_span(), NodeType::CallExpression {
                                            string_fn,
                                            caller: Box::new(Node::new(self.current_span(), 
                                                NodeType::Identifier(ParserText::from(spec).into()),
                                            )),
                                            generic_types: Vec::new(),
                                            args,
                                            reverse_args,
                                        }),
                                    );
                                }
                            }
                        }
                    }
                }

                if let NodeType::Identifier(caller) = &caller.node_type {
                    if "tuple" == &caller.to_string() {
                        let mut args: Vec<Node> = args.into_iter().map(|x| x.into()).collect();
                        args.append(&mut reverse_args);

                        let mut map = Vec::new();

                        for (i, arg) in args.into_iter().enumerate() {
                            map.push((i.to_string(), self.evaluate(scope, arg)?));
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
                        .resolve_potential_generic_ident(scope, &caller)
                        .map(|x| x.clone())
                    {
                        if self.objects.contains_key(&caller.text) {
                            let mut map = Vec::new();
                            let mut args: Vec<Node> = args.into_iter().map(|x| x.into()).collect();
                            args.append(&mut reverse_args);

                            for (i, arg) in args.into_iter().enumerate() {
                                map.push((i.to_string(), self.evaluate(scope, arg)?));
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

                let data_type = self
                    .resolve_type_from_node(scope, &caller)
                    .map(|x| x.unwrap_all_refs().data_type);

                Ok(MiddleNode {
                    node_type: MiddleNodeType::CallExpression {
                        args: match data_type {
                            Some(ParserInnerType::Function {
                                return_type,
                                parameters,
                                is_async,
                            }) if (parameters.len() < args.len() + reverse_args.len()
                                || parameters.len() == args.len() + reverse_args.len() + 1)
                                && parameters
                                    .get(parameters.len() - reverse_args.len() - 1)
                                    .unwrap()
                                    .is_list() =>
                            {
                                let mut lst = Vec::new();
                                for _ in 0..(parameters.len() - 1 - reverse_args.len()) {
                                    let arg = args.remove(0);
                                    lst.push(self.evaluate(scope, arg.into())?);
                                }

                                lst.push(
                                    self.evaluate(
                                        scope,
                                        Node::new(self.current_span(), NodeType::ListLiteral(
                                            match parameters
                                                .last()
                                                .unwrap()
                                                .clone()
                                                .unwrap_all_refs()
                                                .data_type
                                            {
                                                ParserInnerType::List(x) => (*x).into(),
                                                _ => PotentialNewType::DataType(
                                                    ParserDataType::from(ParserInnerType::Auto(
                                                        None,
                                                    )),
                                                ),
                                            },
                                            args.into_iter().map(|x| x.into()).collect(),
                                        )),
                                    )?,
                                );

                                for _ in 0..reverse_args.len() {
                                    lst.push(self.evaluate(scope, reverse_args.remove(0))?);
                                }

                                lst
                            }
                            Some(ParserInnerType::Function {
                                return_type,
                                parameters,
                                is_async,
                            }) if parameters.len() > args.len() + reverse_args.len() => {
                                let mut converted_missing_param_types: Vec<PotentialNewType> =
                                    parameters
                                        .iter()
                                        .enumerate()
                                        .filter(|(i, _)| i >= &(args.len() + reverse_args.len()))
                                        .map(|x| x.1.clone().into())
                                        .collect();

                                let mut missing_param_names: Vec<String> = (0
                                    ..converted_missing_param_types.len())
                                    .map(|i| format!("$-curry-{i}"))
                                    .collect();

                                for name in missing_param_names.clone() {
                                    args.push(CallArg::Value(Node::new(self.current_span(), 
                                        NodeType::Identifier(ParserText::from(name).into()),
                                    )));
                                }

                                for _ in 0..reverse_args.len() {
                                    args.push(CallArg::Value(reverse_args.remove(0)));
                                }

                                return self.evaluate(
                                    scope,
                                    Node {
                                        node_type: NodeType::FunctionDeclaration {
                                            header: FunctionHeader {
                                                generics: GenericTypes::default(),
                                                parameters: (0..converted_missing_param_types
                                                    .len())
                                                    .map(|_| {
                                                        (
                                                            ParserText::from(
                                                                missing_param_names.remove(0),
                                                            )
                                                            .into(),
                                                            converted_missing_param_types.remove(0),
                                                        )
                                                    })
                                                    .collect(),
                                                return_type: (*return_type).into(),
                                                is_async,
                                            },
                                            body: Box::new(Node::new(self.current_span(), 
                                                NodeType::CallExpression {
                                                    string_fn,
                                                    caller,
                                                    generic_types,
                                                    args,
                                                    reverse_args: Vec::new(),
                                                },
                                            )),
                                        },
                                        span: node.span,
                                    },
                                );
                            }
                            _ => {
                                let mut lst = Vec::new();

                                for arg in args {
                                    lst.push(self.evaluate(scope, arg.into())?);
                                }

                                for _ in 0..reverse_args.len() {
                                    lst.push(self.evaluate(scope, reverse_args.remove(0))?);
                                }

                                lst
                            }
                        },
                        caller: Box::new(self.evaluate(scope, *caller)?),
                    },
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
                            return Err(MiddleErr::At(
                                key.span,
                                Box::new(MiddleErr::CantImport(format!(
                                    "{} at {:?}",
                                    key, module
                                ))),
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
                            value = Node::new(self.current_span(), NodeType::CallExpression {
                                string_fn: None,
                                caller: Box::new(point.into()),
                                generic_types: Vec::new(),
                                args: vec![CallArg::Value(value)],
                                reverse_args: Vec::new(),
                            })
                        }
                        _ => {
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

                                    Node::new(self.current_span(), NodeType::VariableDeclaration {
                                        var_type: VarType::Mutable,
                                        identifier: ident.into(),
                                        value: Box::new(value),
                                        data_type: PotentialNewType::DataType(
                                            ParserDataType::from(ParserInnerType::Auto(None)),
                                        ),
                                    })
                                }
                                _ => Node::new(self.current_span(), NodeType::VariableDeclaration {
                                    var_type: VarType::Mutable,
                                    identifier: ParserText::from(String::from("$")).into(),
                                    value: Box::new(value),
                                    data_type: PotentialNewType::DataType(ParserDataType::from(
                                        ParserInnerType::Auto(None),
                                    )),
                                }),
                            };

                            let _ = self.evaluate(scope, var_dec.clone());

                            let point: Node = point.into();
                            value = match point.node_type {
                                NodeType::ScopeDeclaration {
                                    body: Some(mut body),
                                    named: None,
                                    is_temp,
                                    create_new_scope,
                                    define,
                                } => {
                                    body.insert(0, var_dec);

                                    Node {
                                        node_type: NodeType::ScopeDeclaration {
                                            body: Some(body),
                                            named: None,
                                            is_temp,
                                            create_new_scope,
                                            define,
                                        },
                                        ..point
                                    }
                                }
                                _ => Node::new(self.current_span(), NodeType::ScopeDeclaration {
                                    body: Some(vec![var_dec, point]),
                                    named: None,
                                    is_temp: true,
                                    create_new_scope: Some(true),
                                    define: false,
                                }),
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

                self.evaluate(scope, value)
            }
            NodeType::PipeExpression(_) => {
                Ok(MiddleNode::new(MiddleNodeType::EmptyLine, self.current_span()))
            }
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
            let x = self
                .resolve_potential_generic_ident(scope, x)
                .unwrap_or(x.to_string().into());
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

        Err(MiddleErr::At(
            path[0].span,
            Box::new(MiddleErr::Scope(format!("{:?}", path[0].node_type))),
        )
        .into())
    }

    pub fn handle_operator_overloads(
        &mut self,
        scope: &u64,
        span: Span,
        left: Node,
        right: Node,
        operator: Operator,
    ) -> Result<Option<MiddleNode>, MiddleErr> {
        if let (Some(left_ty), Some(right_ty)) = (
            self.resolve_type_from_node(scope, &left),
            self.resolve_type_from_node(scope, &right),
        ) {
            if let Some(overload) = self
                .overloads
                .iter()
                .filter(|x| x.parameters.len() == 2 && x.operator == operator)
                .find(|x| {
                    x.parameters[0].data_type == left_ty.data_type
                        && x.parameters[1].data_type == right_ty.data_type
                })
                .map(|x| x.clone())
            {
                return Ok(Some(MiddleNode {
                    node_type: MiddleNodeType::CallExpression {
                        caller: Box::new(self.evaluate(scope, overload.func.clone())?),
                        args: vec![self.evaluate(scope, left)?, self.evaluate(scope, right)?],
                    },
                    span,
                }));
            }
        }

        Ok(None)
    }

    pub fn get_operator_overload(
        &mut self,
        scope: &u64,
        left: &Node,
        right: &Node,
        operator: &Operator,
    ) -> Option<&MiddleOverload> {
        if let (Some(left_ty), Some(right_ty)) = (
            self.resolve_type_from_node(scope, left),
            self.resolve_type_from_node(scope, right),
        ) {
            if let Some(overload) = self
                .overloads
                .iter()
                .filter(|x| x.parameters.len() == 2 && &x.operator == operator)
                .find(|x| {
                    x.parameters[0].data_type == left_ty.data_type
                        && x.parameters[1].data_type == right_ty.data_type
                })
            {
                return Some(overload);
            }
        }

        None
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

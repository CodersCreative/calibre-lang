use crate::{
    environment::MiddleEnvironment, symbols::resolve::ResolutionOptions, typing::MiddleTypeDefType,
};
use calibre_parser::ast::{
    Operator,
    idents::ParsedIntLiteral,
    nodes::{AsFailureMode, EmitType, Node, NodeType},
    types::{ParserDataType, ParserInnerType},
};

impl MiddleEnvironment {
    pub fn resolve_emit_type_from_node(
        &mut self,
        scope: &u64,
        node: &Node,
    ) -> Option<ParserDataType> {
        let typ = match &node.node_type {
            NodeType::IfStatement { .. } | NodeType::MatchStatement { .. } => {
                self.resolve_type_from_node(scope, node)
            }
            NodeType::Emit(EmitType::Scope(x)) => self.resolve_type_from_node(scope, x),
            _ => None,
        };

        typ.and_then(|typ| {
            self.resolve_data_type(scope, &typ, ResolutionOptions::typing())
                .ok()
        })
    }

    pub fn resolve_type_from_node(&mut self, scope: &u64, node: &Node) -> Option<ParserDataType> {
        let typ = match &node.node_type {
            NodeType::Break { .. }
            | NodeType::Continue { .. }
            | NodeType::VariableDeclaration { .. }
            | NodeType::ImplDeclaration { .. }
            | NodeType::ImplTraitDeclaration { .. }
            | NodeType::TraitDeclaration { .. }
            | NodeType::TypeDeclaration { .. }
            | NodeType::ExternFunctionDeclaration { .. }
            | NodeType::Return { .. }
            | NodeType::ImportStatement { .. }
            | NodeType::AssignmentExpression { .. }
            | NodeType::DestructureDeclaration { .. }
            | NodeType::DestructureAssignment { .. }
            | NodeType::LoopDeclaration {
                else_body: None, ..
            }
            | NodeType::TestDeclaration { .. }
            | NodeType::ScopeDeclaration { define: true, .. }
            | NodeType::ScopeAlias { .. }
            | NodeType::DataType { .. }
            | NodeType::Until { .. }
            | NodeType::SelectStatement { .. }
            | NodeType::Emit(EmitType::Scope(_)) => None,
            NodeType::Emit(_) => Some(ParserDataType::new(node.span, ParserInnerType::Bool)),
            NodeType::Spawn { auto_wait, .. } => Some(ParserDataType::new(
                node.span,
                if *auto_wait {
                    ParserInnerType::Null
                } else {
                    ParserInnerType::Struct(String::from("WaitGroup"))
                },
            )),
            NodeType::InlineGenerator { map, data_type, .. } => {
                let elem = match data_type {
                    Some(dt) => dt.clone(),
                    _ => self
                        .resolve_type_from_node(scope, map)
                        .unwrap_or(ParserDataType::new(node.span, ParserInnerType::Auto(None))),
                };

                Some(ParserDataType::new(
                    node.span,
                    ParserInnerType::Gen(Box::new(elem)),
                ))
            }
            NodeType::Null | NodeType::Defer { .. } | NodeType::Drop(_) | NodeType::EmptyLine => {
                Some(ParserDataType::new(node.span, ParserInnerType::Null))
            }
            NodeType::MoveExpression { value } | NodeType::ParenExpression { value } => self
                .resolve_type_from_node(scope, value)
                .map(|x| x.unwrap_all_refs()),
            NodeType::TupleLiteral { values } => {
                let mut types = Vec::new();
                for value in values {
                    types.push(
                        self.resolve_type_from_node(scope, value)
                            .unwrap_or(ParserDataType::new(
                                value.span,
                                ParserInnerType::Auto(None),
                            )),
                    );
                }
                Some(ParserDataType::new(
                    node.span,
                    ParserInnerType::Tuple(types),
                ))
            }
            NodeType::RefStatement { mutability, value } => Some(ParserDataType {
                data_type: ParserInnerType::Ref(
                    Box::new(self.resolve_type_from_node(scope, value)?.unwrap_all_refs()),
                    *mutability,
                ),
                span: node.span,
            }),
            NodeType::ScopeDeclaration {
                body: Some(body), ..
            } => {
                let mut typ = None;

                for node in body {
                    typ = self.resolve_emit_type_from_node(scope, node);
                    if typ.is_some() {
                        break;
                    }
                }

                typ
            }
            NodeType::ScopeDeclaration {
                named: Some(named), ..
            } => {
                let name = self
                    .resolve(
                        scope,
                        &named.name,
                        ResolutionOptions::default().with_dollar(),
                    )
                    .ok()?;
                let resolved = self
                    .scoping
                    .resolve_macro(scope, &name)?
                    .body
                    .last()?
                    .clone();
                self.resolve_type_from_node(scope, &resolved)
            }
            NodeType::IfStatement {
                comparison: _,
                then,
                otherwise,
            } => {
                if let Some(otherwise) = otherwise {
                    let otherwise =
                        if let NodeType::IfStatement { then: then2, .. } = &otherwise.node_type {
                            then2.clone()
                        } else {
                            otherwise.clone()
                        };

                    let then_ty = self.resolve_type_from_node(scope, then);
                    let else_ty = self.resolve_type_from_node(scope, &otherwise);
                    match (then_ty, else_ty) {
                        (Some(a), Some(b)) if a.data_type == b.data_type => Some(a),
                        (Some(a), Some(b)) if a.data_type == ParserInnerType::Null => Some(b),
                        (Some(a), Some(b)) if b.data_type == ParserInnerType::Null => Some(a),
                        _ => None,
                    }
                } else {
                    Some(ParserDataType::new(node.span, ParserInnerType::Null))
                }
            }

            NodeType::LoopDeclaration {
                else_body: Some(body),
                ..
            } => self.resolve_type_from_node(scope, &body),
            NodeType::MatchStatement { value: _, body } => {
                if let Some((_arm_type, _guards, arm_body)) = body.first() {
                    self.resolve_type_from_node(scope, arm_body)
                } else {
                    None
                }
            }
            NodeType::EnumExpression { identifier, .. }
            | NodeType::StructLiteral { identifier, .. } => {
                self.resolve_to_data_type(scope, identifier).ok()
            }
            NodeType::FunctionDeclaration { header, .. }
            | NodeType::FnMatchDeclaration { header, .. } => {
                let generic_params: Vec<String> = header
                    .generics
                    .0
                    .iter()
                    .map(|g| g.identifier.to_string())
                    .collect();

                if !generic_params.is_empty() {
                    self.scoping.push_generic_params(generic_params.clone());
                }

                let return_type = self
                    .resolve_data_type(scope, &header.return_type, ResolutionOptions::typing())
                    .ok()?;

                Some(ParserDataType {
                    data_type: ParserInnerType::Function {
                        return_type: Box::new(return_type),
                        parameters: {
                            let mut params = Vec::new();

                            for param in header.parameters.clone() {
                                let data_type = if let Some(x) = param.1 {
                                    self.resolve_data_type(scope, &x, ResolutionOptions::typing())
                                        .ok()?
                                } else if let Some(node) = &param.2 {
                                    self.resolve_type_from_node(scope, node)?
                                } else {
                                    return None;
                                };
                                params.push(data_type);
                            }

                            self.scoping.pop_generic_params();

                            params
                        },
                    },
                    span: node.span,
                })
            }

            NodeType::NotExpression { .. } => Some(ParserDataType {
                data_type: ParserInnerType::Bool,
                span: node.span,
            }),
            NodeType::InDeclaration { identifier, value } => {
                self.resolve_operator_or_bool(scope, identifier, value, Operator::In, node.span)
            }
            NodeType::ComparisonExpression {
                left,
                right,
                operator,
            } => self.resolve_operator_or_bool(
                scope,
                left,
                right,
                Operator::Comparison(operator.clone()),
                node.span,
            ),
            NodeType::BooleanExpression {
                left,
                right,
                operator,
            } => self.resolve_operator_or_bool(
                scope,
                left,
                right,
                Operator::Boolean(operator.clone()),
                node.span,
            ),
            NodeType::BinaryExpression {
                left,
                right,
                operator,
            } => {
                if let Some(x) = self.get_operator_overload(
                    scope,
                    left,
                    right,
                    &Operator::Binary(operator.clone()),
                ) {
                    Some(x.return_type.clone())
                } else {
                    self.resolve_type_from_node(scope, left)
                        .or_else(|| self.resolve_type_from_node(scope, right))
                }
            }
            NodeType::IterExpression {
                data_type, spawned, ..
            } => {
                let list_type = ParserDataType {
                    data_type: ParserInnerType::List(Box::new(
                        self.resolve_data_type(scope, data_type, ResolutionOptions::typing())
                            .ok()?,
                    )),
                    span: node.span,
                };
                if *spawned {
                    Some(ParserDataType {
                        data_type: ParserInnerType::StructWithGenerics {
                            identifier: String::from("Mutex"),
                            generic_types: vec![list_type],
                        },
                        span: node.span,
                    })
                } else {
                    Some(list_type)
                }
            }
            NodeType::ListLiteral(data_type, _) | NodeType::ListRepeatLiteral { data_type, .. } => {
                Some(ParserDataType {
                    data_type: ParserInnerType::List(Box::new(
                        self.resolve_data_type(scope, data_type, ResolutionOptions::typing())
                            .ok()?,
                    )),
                    span: node.span,
                })
            }
            NodeType::NegExpression { value }
            | NodeType::DebugExpression { value }
            | NodeType::Ternary { then: value, .. } => self.resolve_type_from_node(scope, value),
            NodeType::AsExpression {
                value: _,
                data_type,
                failure_mode,
            } => {
                let ok = self
                    .resolve_data_type(scope, data_type, ResolutionOptions::typing())
                    .ok()?;
                match failure_mode {
                    AsFailureMode::Panic => Some(ok),
                    AsFailureMode::Option => Some(ParserDataType {
                        data_type: ParserInnerType::Option(Box::new(ok)),
                        span: node.span,
                    }),
                    AsFailureMode::Result => Some(ParserDataType {
                        data_type: ParserInnerType::Result {
                            ok: Box::new(ok),
                            err: Box::new(ParserDataType::new(node.span, ParserInnerType::Dynamic)),
                        },
                        span: node.span,
                    }),
                }
            }
            NodeType::IsExpression { .. } => Some(ParserDataType {
                data_type: ParserInnerType::Bool,
                span: node.span,
            }),
            NodeType::RangeDeclaration { .. } => Some(ParserDataType {
                data_type: ParserInnerType::Range,
                span: node.span,
            }),
            NodeType::IntLiteral(number) => Some(ParserDataType {
                data_type: if number.ends_with('b') {
                    ParserInnerType::Byte
                } else if number.ends_with('u') {
                    ParserInnerType::UInt
                } else {
                    ParserInnerType::Int
                },
                span: node.span,
            }),
            NodeType::CharLiteral(_) => Some(ParserDataType {
                data_type: ParserInnerType::Char,
                span: node.span,
            }),
            NodeType::BigLiteral(_) => Some(ParserDataType {
                data_type: ParserInnerType::Big,
                span: node.span,
            }),
            NodeType::StringLiteral(_) => Some(ParserDataType {
                data_type: ParserInnerType::Str,
                span: node.span,
            }),
            NodeType::FloatLiteral(_) => Some(ParserDataType {
                data_type: ParserInnerType::Float,
                span: node.span,
            }),
            NodeType::Try { value, .. } => match self.resolve_type_from_node(scope, value) {
                Some(ParserDataType {
                    data_type: ParserInnerType::Result { ok: x, err: _ },
                    ..
                })
                | Some(ParserDataType {
                    data_type: ParserInnerType::Option(x),
                    ..
                }) => Some(*x),
                x => x,
            },
            NodeType::CallExpression {
                caller,
                generic_types: _generic_types,
                args,
                ..
            } => {
                if let NodeType::FieldAccess { base, field } = &caller.node_type {
                    let member_name = self
                        .resolve(scope, field, ResolutionOptions::default().with_dollar())
                        .unwrap_or(field.text().clone());

                    if !member_name.is_empty() {
                        if let Some(ty) = &self.resolve_type_from_node(scope, base).or_else(|| {
                            if let NodeType::Identifier(id) = &base.node_type {
                                self.resolve_to_data_type(scope, id).ok()
                            } else {
                                None
                            }
                        }) && let Some(method_ty) =
                            self.resolve_member_fn_type(&ty, &member_name)
                        {
                            return method_ty.apply_callable();
                        }

                        return Some(ParserDataType::new(base.span, ParserInnerType::Dynamic));
                    }
                }

                let mut caller_type = None;
                if let NodeType::Identifier(caller) = &caller.node_type {
                    if &caller.to_string() == "tuple" {
                        let mut lst = Vec::new();

                        for arg in args {
                            let ty = self.resolve_type_from_node(scope, &arg.clone().into())?;
                            lst.push(ty);
                        }
                        return Some(ParserDataType {
                            data_type: ParserInnerType::Tuple(lst),
                            span: node.span,
                        });
                    }

                    if let Ok(caller_ty) = self.resolve_to_data_type(scope, caller) {
                        match &caller_ty.data_type {
                            ParserInnerType::Struct(name) => {
                                if self.typing.objects.contains_key(name) {
                                    return Some(ParserDataType {
                                        data_type: ParserInnerType::Struct(name.clone()),
                                        span: node.span,
                                    });
                                }
                            }
                            ParserInnerType::StructWithGenerics {
                                identifier,
                                generic_types,
                            } => {
                                if self.typing.objects.contains_key(identifier) {
                                    return Some(ParserDataType {
                                        data_type: ParserInnerType::StructWithGenerics {
                                            identifier: identifier.clone(),
                                            generic_types: generic_types.clone(),
                                        },
                                        span: node.span,
                                    });
                                }
                            }
                            _ => {}
                        }
                    }
                }

                caller_type = caller_type.or_else(|| self.resolve_type_from_node(scope, &caller));

                caller_type?.data_type.apply_callable()
            }
            NodeType::Identifier(x) => {
                if let Ok(iden) = self.resolve(scope, x, ResolutionOptions::all())
                    && let Some(x) = self.symbols.variables.get(&iden)
                {
                    Some(x.data_type.clone())
                } else {
                    None
                }
            }
            NodeType::FieldAccess { base, field } => {
                let ty = self.resolve_type_from_node(scope, base).or_else(|| {
                    if let NodeType::Identifier(id) = &base.node_type {
                        self.resolve_to_data_type(scope, id).ok()
                    } else {
                        None
                    }
                })?;

                let member = self
                    .resolve(scope, field, ResolutionOptions::default().with_dollar())
                    .unwrap_or_else(|_| field.text().clone());

                if let Some(member_type) = self.resolve_member_fn_type(&ty, &member) {
                    return Some(member_type);
                }

                if let Some(MiddleTypeDefType::Enum { .. }) = self
                    .typing
                    .find_object_for_struct_name(&ty.impl_name())
                    .map(|x| &x.object_type)
                {
                    return Some(ty);
                }

                self.resolve_member_field_type(scope, &ty, &member, node.span)
            }
            NodeType::ScopeAccess { base, field } => {
                let mut module_path = Vec::new();
                if base.scope_access_path(&mut module_path) {
                    let member = self
                        .resolve(scope, field, ResolutionOptions::default().with_dollar())
                        .unwrap_or_else(|_| field.text().clone());

                    if let Ok(member_scope) = self
                        .get_scope_list(*scope, module_path.clone())
                        .or_else(|_| self.import_scope_list(*scope, module_path).map(|x| x.0))
                    {
                        let resolved = self
                            .resolve(&member_scope, field, ResolutionOptions::all())
                            .unwrap_or(member);
                        return self
                            .symbols
                            .variables
                            .get(&resolved)
                            .map(|x| x.data_type.clone());
                    }
                }

                None
            }
            NodeType::IndexAccess { base, index } => {
                let base_type = self.resolve_type_from_node(scope, base).or_else(|| {
                    if let NodeType::Identifier(id) = &base.node_type {
                        self.resolve_to_data_type(scope, id).ok()
                    } else {
                        None
                    }
                });

                if let Some(base_type) = base_type {
                    let resolved_type = match self.resolve_data_type(
                        scope,
                        &base_type,
                        ResolutionOptions::typing(),
                    ) {
                        Ok(ty) => ty.unwrap_all_refs(),
                        Err(_) => return Some(ParserDataType::auto(node.span)),
                    };

                    let index_type = match resolved_type.data_type {
                        ParserInnerType::List(inner)
                        | ParserInnerType::Option(inner)
                        | ParserInnerType::Ptr(inner) => *inner,
                        ParserInnerType::Tuple(values) => match &index.node_type {
                            NodeType::IntLiteral(i) => ParsedIntLiteral::parse(i)
                                .and_then(|idx| values.get(idx.value as usize).cloned())
                                .unwrap_or_else(|| {
                                    ParserDataType::new(node.span, ParserInnerType::Auto(None))
                                }),
                            _ => ParserDataType::new(node.span, ParserInnerType::Auto(None)),
                        },
                        ParserInnerType::Result { ok, err } => {
                            if ok.data_type == err.data_type {
                                *ok
                            } else {
                                ParserDataType::new(node.span, ParserInnerType::Dynamic)
                            }
                        }
                        _ => ParserDataType::new(node.span, ParserInnerType::Auto(None)),
                    };
                    Some(index_type)
                } else {
                    Some(ParserDataType::auto(node.span))
                }
            }
            NodeType::PipeExpression(path) => {
                let mut iter = path.iter();
                let first = iter.next()?;
                let mut current = self.resolve_type_from_node(scope, first.get_node())?;

                let mut idx = 1usize;
                while idx < path.len() {
                    let point = &path[idx];
                    let point_ty = self.resolve_type_from_node(scope, point.get_node());
                    let point_callable = point_ty.as_ref().is_some_and(|ty| {
                        ty.is_callable()
                            && !point.is_named()
                            && !point.get_node().node_type.is_call()
                    });

                    if !point_callable && let Some(next) = path.get(idx + 1) {
                        let next_ty = self.resolve_type_from_node(scope, next.get_node());
                        let next_callable = next_ty.as_ref().is_some_and(|ty| {
                            ty.is_callable()
                                && !next.is_named()
                                && !next.get_node().node_type.is_call()
                        });

                        if next_callable {
                            current = next_ty
                                .and_then(|x| x.apply_callable())
                                .unwrap_or(ParserDataType::auto(node.span));
                            idx += 2;
                            continue;
                        }
                    }

                    current = if point_callable {
                        point_ty
                            .and_then(|x| x.apply_callable())
                            .unwrap_or(ParserDataType::auto(node.span))
                    } else {
                        point_ty
                            .unwrap_or(ParserDataType::new(node.span, ParserInnerType::Auto(None)))
                    };
                    idx += 1;
                }

                Some(current)
            }
            NodeType::DerefStatement { value } => self
                .resolve_type_from_node(scope, &value)
                .map(|x| x.unwrap_all_refs()),
            NodeType::ScopeDeclaration { .. } => unreachable!(),
            NodeType::Tag { .. } => {
                Some(ParserDataType::new(node.span, ParserInnerType::Auto(None)))
            }
        };

        typ.and_then(|typ| {
            self.resolve_data_type(scope, &typ, ResolutionOptions::typing())
                .ok()
        })
    }
}

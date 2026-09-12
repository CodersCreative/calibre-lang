use crate::{
    environment::MiddleEnvironment,
    errors::MiddleErr,
    scoping::ScopeId,
    symbols::resolve::{ResolutionOptions, StrOrAstNode},
    translate::MirLowering,
    typing::MiddleTypeDefType,
};
use calibre_parser::ast::{
    idents::ParsedIntLiteral,
    nodes::{AstNode, AstNodeType, flow::AstEmit},
    types::{ParserDataType, ParserInnerType},
};
use ustr::Ustr;

impl MiddleEnvironment {
    pub fn resolve_emit_type_from_node(
        &mut self,
        scope: ScopeId,
        node: &AstNode,
    ) -> Option<ParserDataType> {
        let typ = match &node.node_type {
            AstNodeType::IfStatement { .. } | AstNodeType::MatchStatement { .. } => {
                self.resolve_type_from_node(scope, node)
            }
            AstNodeType::Emit(AstEmit::Scope(x)) => self.resolve_type_from_node(scope, x),
            _ => None,
        };

        typ.and_then(|typ| {
            self.resolve_data_type(scope, &typ, ResolutionOptions::typing())
                .ok()
        })
    }

    fn resolve_curried_type(&mut self, scope: ScopeId, value: &AstNode) -> Option<ParserDataType> {
        match self.resolve_type_from_node(scope, value)?.data_type {
            ParserInnerType::Function {
                return_type,
                parameters,
            }
            | ParserInnerType::NativeFunction {
                return_type,
                parameters,
            } => {
                if parameters.is_empty() {
                    return Some(ParserDataType::function(
                        value.span,
                        parameters,
                        *return_type,
                    ));
                }

                let mut result = *return_type;
                for parameter in parameters.iter().rev() {
                    result = ParserDataType::function(value.span, vec![parameter.clone()], result);
                }

                Some(result)
            }
            other => Some(ParserDataType::function(
                value.span,
                Vec::new(),
                ParserDataType::new(value.span, other),
            )),
        }
    }

    pub fn resolve_type_from_node(
        &mut self,
        scope: ScopeId,
        node: &AstNode,
    ) -> Option<ParserDataType> {
        let typ = match &node.node_type {
            // Flow
            AstNodeType::Emit(x) => x.type_of(self, scope, node.span),
            AstNodeType::Try(x) => x.type_of(self, scope, node.span),
            AstNodeType::PipeExpression(x) => x.type_of(self, scope, node.span),

            // Literals
            AstNodeType::StructLiteral(x) => x.type_of(self, scope, node.span),
            AstNodeType::EnumExpression(x) => x.type_of(self, scope, node.span),
            AstNodeType::TupleLiteral(x) => x.type_of(self, scope, node.span),
            AstNodeType::StringLiteral(x) => x.type_of(self, scope, node.span),
            AstNodeType::RangeDeclaration(x) => x.type_of(self, scope, node.span),
            AstNodeType::IntLiteral(x) => x.type_of(self, scope, node.span),
            AstNodeType::CharLiteral(x) => x.type_of(self, scope, node.span),
            AstNodeType::BigLiteral(x) => x.type_of(self, scope, node.span),
            AstNodeType::FloatLiteral(x) => x.type_of(self, scope, node.span),

            // Lists
            AstNodeType::ListLiteral(x) => x.type_of(self, scope, node.span),
            AstNodeType::ListRepeatLiteral(x) => x.type_of(self, scope, node.span),

            // Conditionals
            AstNodeType::Ternary(x) => x.type_of(self, scope, node.span),
            AstNodeType::IfStatement(x) => x.type_of(self, scope, node.span),

            // Unary
            AstNodeType::NegExpression(x) => x.type_of(self, scope, node.span),
            AstNodeType::NotExpression(x) => x.type_of(self, scope, node.span),

            // Binary
            AstNodeType::InDeclaration(x) => x.type_of(self, scope, node.span),
            AstNodeType::ComparisonExpression(x) => x.type_of(self, scope, node.span),
            AstNodeType::BooleanExpression(x) => x.type_of(self, scope, node.span),
            AstNodeType::BinaryExpression(x) => x.type_of(self, scope, node.span),
            AstNodeType::AsExpression(x) => x.type_of(self, scope, node.span),
            AstNodeType::IsExpression(x) => x.type_of(self, scope, node.span),

            // TODO
            AstNodeType::Break { .. }
            | AstNodeType::Continue { .. }
            | AstNodeType::VariableDeclaration { .. }
            | AstNodeType::ImplDeclaration { .. }
            | AstNodeType::ImplTraitDeclaration { .. }
            | AstNodeType::TraitDeclaration { .. }
            | AstNodeType::TypeDeclaration { .. }
            | AstNodeType::ExternFunctionDeclaration { .. }
            | AstNodeType::Return { .. }
            | AstNodeType::ImportStatement { .. }
            | AstNodeType::AssignmentExpression { .. }
            | AstNodeType::DestructureDeclaration { .. }
            | AstNodeType::DestructureAssignment { .. }
            | AstNodeType::LoopDeclaration {
                else_body: None, ..
            }
            | AstNodeType::TestDeclaration { .. }
            | AstNodeType::ScopeDeclaration { define: true, .. }
            | AstNodeType::ScopeAlias { .. }
            | AstNodeType::DataType { .. }
            | AstNodeType::SelectStatement { .. } => None,
            AstNodeType::Spawn { auto_wait, .. } => Some(ParserDataType::new(
                node.span,
                if *auto_wait {
                    ParserInnerType::Null
                } else {
                    ParserInnerType::Struct(String::from("WaitGroup"))
                },
            )),
            AstNodeType::InlineGenerator { map, data_type, .. } => {
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
            AstNodeType::Null
            | AstNodeType::Defer { .. }
            | AstNodeType::Drop(_)
            | AstNodeType::EmptyLine => Some(ParserDataType::new(node.span, ParserInnerType::Null)),
            AstNodeType::MoveExpression { value } | AstNodeType::ParenExpression { value } => self
                .resolve_type_from_node(scope, value)
                .map(|x| x.unwrap_all_refs()),
            AstNodeType::RefStatement { mutability, value } => Some(ParserDataType {
                data_type: ParserInnerType::Ref(
                    Box::new(self.resolve_type_from_node(scope, value)?.unwrap_all_refs()),
                    *mutability,
                ),
                span: node.span,
            }),
            AstNodeType::ScopeDeclaration {
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
            AstNodeType::ScopeDeclaration {
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

            AstNodeType::LoopDeclaration {
                else_body: Some(body),
                ..
            } => self.resolve_type_from_node(scope, body),
            AstNodeType::MatchStatement { value: _, body } => {
                if let Some((_arm_type, _guards, arm_body)) = body.first() {
                    self.resolve_type_from_node(scope, arm_body)
                } else {
                    None
                }
            }
            AstNodeType::FunctionDeclaration { header, .. }
            | AstNodeType::FnMatchDeclaration { header, .. } => {
                let generic_params: Vec<Ustr> = header
                    .generics
                    .0
                    .iter()
                    .map(|g| {
                        self.resolve(
                            scope,
                            &g.identifier,
                            ResolutionOptions::default().with_dollar(),
                        )
                    })
                    .collect::<Result<Vec<Ustr>, MiddleErr>>()
                    .ok()?;

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
                            let mut params = Vec::with_capacity(header.parameters.len());

                            for param in &header.parameters {
                                let data_type = if let Some(x) = &param.1 {
                                    self.resolve_data_type(scope, x, ResolutionOptions::typing())
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
            AstNodeType::IterExpression {
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
            AstNodeType::CurryExpression { value } => self.resolve_curried_type(scope, value),

            AstNodeType::CallExpression {
                caller,
                generic_types: _generic_types,
                args,
                reverse_args,
                ..
            } => {
                if let AstNodeType::FieldAccess { base, field } = &caller.node_type {
                    let member_name = self
                        .resolve(scope, field, ResolutionOptions::default().with_dollar())
                        .unwrap_or(Ustr::from(field.text()));

                    if !member_name.is_empty() {
                        if let Some(ty) = &self.resolve_type_from_node(scope, base).or_else(|| {
                            if let AstNodeType::Identifier(id) = &base.node_type {
                                self.resolve_to_data_type(scope, id).ok()
                            } else {
                                None
                            }
                        }) && let Some(method_ty) = self.resolve_member_fn_type(ty, &member_name)
                        {
                            return method_ty.apply_callable();
                        }

                        return Some(ParserDataType::new(base.span, ParserInnerType::Dynamic));
                    }
                }

                let mut caller_type = None;
                if let AstNodeType::Identifier(caller) = &caller.node_type {
                    match caller.to_string().as_str() {
                        "tuple" => {
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
                        "curry" if args.len() == 1 && reverse_args.is_empty() => {
                            return self.resolve_curried_type(scope, &args[0].clone().into());
                        }
                        _ => {}
                    }

                    if let Ok(caller_ty) = self.resolve_to_data_type(scope, caller) {
                        match &caller_ty.data_type {
                            ParserInnerType::Struct(name)
                                if self.typing.objects.contains_key(&Ustr::from(name)) =>
                            {
                                return Some(ParserDataType {
                                    data_type: ParserInnerType::Struct(name.clone()),
                                    span: node.span,
                                });
                            }
                            ParserInnerType::StructWithGenerics {
                                identifier,
                                generic_types,
                            } if self.typing.objects.contains_key(&Ustr::from(identifier)) => {
                                return Some(ParserDataType {
                                    data_type: ParserInnerType::StructWithGenerics {
                                        identifier: identifier.clone(),
                                        generic_types: generic_types.clone(),
                                    },
                                    span: node.span,
                                });
                            }
                            _ => {}
                        }
                    }
                }

                caller_type = caller_type.or_else(|| self.resolve_type_from_node(scope, caller));

                caller_type?.data_type.apply_callable()
            }
            AstNodeType::Identifier(x) => {
                match self
                    .resolve_potential_node(scope, x, ResolutionOptions::idents())
                    .ok()?
                {
                    StrOrAstNode::Str(iden) => self
                        .symbols
                        .variables
                        .get(&iden)
                        .map(|x| x.data_type.clone()),
                    StrOrAstNode::Node(x) => return self.resolve_type_from_node(scope, &x),
                }
            }
            AstNodeType::FieldAccess { base, field } => {
                let ty = self.resolve_type_from_node(scope, base).or_else(|| {
                    if let AstNodeType::Identifier(id) = &base.node_type {
                        self.resolve_to_data_type(scope, id).ok()
                    } else {
                        None
                    }
                })?;

                let member = self
                    .resolve(scope, field, ResolutionOptions::default().with_dollar())
                    .unwrap_or_else(|_| Ustr::from(field.text()));

                if let Some(member_type) = self.resolve_member_fn_type(&ty, &member) {
                    return Some(member_type);
                }

                if let Some(MiddleTypeDefType::Enum { .. }) = self
                    .typing
                    .find_object_for_struct_name(&Ustr::from(&ty.impl_name()))
                    .map(|x| &x.object_type)
                {
                    return Some(ty);
                }

                self.resolve_member_field_type(scope, &ty, &member, node.span)
            }
            AstNodeType::ScopeAccess { base, field } => {
                let mut module_path = Vec::new();
                if base.scope_access_path(&mut module_path) {
                    let member = self
                        .resolve(scope, field, ResolutionOptions::default().with_dollar())
                        .unwrap_or_else(|_| Ustr::from(field.text()));

                    if let Ok(member_scope) = self
                        .get_scope_list(scope, &module_path.clone())
                        .or_else(|_| self.import_scope_list(scope, &module_path).map(|x| x.0))
                    {
                        let resolved = self
                            .resolve(member_scope, field, ResolutionOptions::idents())
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
            AstNodeType::IndexAccess { base, index } => {
                let base_type = self.resolve_type_from_node(scope, base).or_else(|| {
                    if let AstNodeType::Identifier(id) = &base.node_type {
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
                            AstNodeType::IntLiteral(i) => ParsedIntLiteral::parse(&i.value)
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
            AstNodeType::DerefStatement { value } => self
                .resolve_type_from_node(scope, value)
                .map(|x| x.unwrap_all_refs()),
            AstNodeType::ScopeDeclaration { .. } => unreachable!(),
            AstNodeType::Tag { .. } => {
                Some(ParserDataType::new(node.span, ParserInnerType::Auto(None)))
            }
        };

        typ.and_then(|typ| {
            self.resolve_data_type(scope, &typ, ResolutionOptions::typing())
                .ok()
        })
    }
}

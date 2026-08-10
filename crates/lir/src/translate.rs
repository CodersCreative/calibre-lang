use calibre_mir::{
    ast::{MiddleNode, MiddleNodeType},
    environment::MiddleEnvironment,
    typing::{MiddleImpl, MiddleTrait, MiddleTypeDefType},
};
use calibre_parser::{
    Span,
    ast::{
        ObjectMap,
        binary::BinaryOperator,
        comparison::BooleanOperator,
        idents::{IntLiteralType, ParsedIntLiteral, ParserText},
        types::{ParserDataType, ParserInnerType},
    },
};
use rustc_hash::{FxHashMap, FxHashSet};

use crate::{
    ast::{BlockId, LirLValue, LirLiteral, LirNode, LirNodeType, LirTerminator},
    environment::{LirEnvironment, LirFunction, LirGlobal, LirRegistry},
};

impl<'a> LirEnvironment<'a> {
    fn lower_nodes(&mut self, nodes: Vec<MiddleNode>) -> Vec<LirNodeType> {
        nodes
            .into_iter()
            .map(|node| self.lower_node(node))
            .collect()
    }

    fn assign_var(&mut self, span: Span, name: &str, value: LirNodeType) {
        self.add_instr(LirNode::new(
            span,
            LirNodeType::Assign {
                dest: LirLValue::Var(name.to_string().into_boxed_str()),
                value: Box::new(value),
            },
        ));
    }

    fn declare_temp_null(&mut self, span: Span, temp: &str) {
        self.add_instr(LirNode::new(
            span,
            LirNodeType::Declare {
                dest: temp.to_string().into_boxed_str(),
                value: Box::new(LirNodeType::null()),
            },
        ));
    }

    fn jump_if_open(&mut self, span: Span, target: BlockId) {
        if self.current_block_open() {
            self.set_terminator(LirTerminator::Jump { span, target });
        }
    }

    #[inline]
    fn assign_temp_if_non_null(&mut self, span: Span, temp: &str, value: LirNodeType) {
        if !value.is_null() {
            self.assign_var(span, temp, value);
        }
    }

    #[inline]
    fn jump_to_loop_target_if_present(&mut self, span: Span, label: Option<&str>, use_exit: bool) {
        if let Some(target) = self.find_loop_target(label, use_exit) {
            self.set_terminator(LirTerminator::Jump { span, target });
        }
    }

    #[inline]
    fn emit_return_value(&mut self, span: Span, value: Option<LirNodeType>) {
        self.set_terminator(LirTerminator::Return { span, value });
    }

    #[inline]
    fn lower_scope_items(&mut self, body: Vec<MiddleNode>) {
        for stmt in body {
            if !self.current_block_open() {
                break;
            }
            self.lower_and_add_node(stmt);
        }
    }

    #[inline]
    fn loop_label(label: &Option<ParserText>) -> Option<&str> {
        label.as_ref().map(|x| x.text.as_str())
    }

    #[inline]
    fn next_function_label(&mut self) -> String {
        if let Some(name) = self.last_ident.take()
            && !name.contains("curry_capture")
        {
            return name;
        }
        self.get_temp()
    }

    fn collect_trait_methods(
        imp: &MiddleImpl,
        trait_def: Option<&MiddleTrait>,
    ) -> FxHashMap<String, String> {
        let mut methods: FxHashMap<String, String> = FxHashMap::default();
        if let Some(trait_def) = trait_def {
            for member in trait_def.members.keys() {
                if let Some((mapped, _)) = imp.variables.get(member) {
                    methods.insert(member.clone(), mapped.clone());
                }
            }
        } else {
            for (member, (mapped, _)) in &imp.variables {
                methods.insert(member.clone(), mapped.clone());
            }
        }
        methods
    }

    pub fn build_dyn_vtables(
        env: &MiddleEnvironment,
    ) -> FxHashMap<String, FxHashMap<String, FxHashMap<String, String>>> {
        let mut out: FxHashMap<String, FxHashMap<String, FxHashMap<String, String>>> =
            FxHashMap::default();

        for imp in env.typing.impls.values() {
            let concrete = imp.data_type.clone().unwrap_all_refs().to_string();
            let trait_map = out.entry(concrete).or_default();

            for trait_name in &imp.traits {
                let methods =
                    Self::collect_trait_methods(imp, env.typing.trait_defs.get(trait_name));
                if !methods.is_empty() {
                    trait_map.insert(trait_name.clone(), methods);
                }
            }
        }

        out
    }

    pub fn lower(env: &'a MiddleEnvironment, node: MiddleNode) -> LirRegistry {
        let mut this = Self::new(env);
        this.lower_and_add_node(node);
        this.registry
    }

    pub fn lower_with_root(
        env: &'a MiddleEnvironment,
        node: MiddleNode,
        root_name: String,
    ) -> LirRegistry {
        let mut this = Self::new(env);
        this.lower_and_add_node(node);
        if !this.blocks.is_empty() {
            let blocks = std::mem::take(&mut this.blocks).into_boxed_slice();
            this.registry.globals.insert(
                root_name.clone(),
                LirGlobal {
                    name: root_name.into_boxed_str(),
                    data_type: ParserDataType::new(Span::default(), ParserInnerType::Dynamic),
                    blocks,
                },
            );
        }
        this.registry
    }

    fn lower_member_lvalue(&mut self, path: Vec<(MiddleNode, bool)>) -> LirNodeType {
        self.lower_member_path(path, true)
    }

    fn lower_member_path(
        &mut self,
        path: Vec<(MiddleNode, bool)>,
        ref_identifier_base: bool,
    ) -> LirNodeType {
        let mut path = path.into_iter();

        let Some((base_node, _)) = path.next() else {
            return LirNodeType::null();
        };

        let mut current = match (ref_identifier_base, &base_node.node_type) {
            (true, MiddleNodeType::Identifier(name)) => LirNodeType::Ref(Box::new(
                LirNodeType::Load(name.text.clone().into_boxed_str()),
            )),
            _ => self.lower_node(base_node),
        };

        for (step, is_dynamic) in path {
            if is_dynamic {
                current = LirNodeType::Index(Box::new(current), Box::new(self.lower_node(step)));
            } else {
                current = LirNodeType::Member(Box::new(current), step.member_field());
            }
        }

        current
    }

    pub fn lower_and_add_node(&mut self, node: MiddleNode) {
        if !self.current_block_open() {
            return;
        }

        if matches!(node.node_type, MiddleNodeType::Return { .. }) {
            let _ = self.lower_node(node);
            return;
        }

        let span = node.span;
        let value = self.lower_node(node);

        if value.is_noop() || value.is_null() {
            return;
        }

        self.add_instr(LirNode::new(span, value));
    }

    pub fn lower_node(&mut self, node: MiddleNode) -> LirNodeType {
        let span = node.span;
        match node.node_type {
            MiddleNodeType::Emit { value } => {
                // TODO Add emit support
                self.lower_node(*value)
            }
            MiddleNodeType::IntLiteral(ParsedIntLiteral { value, int_type }) => match int_type {
                IntLiteralType::Int => LirNodeType::Literal(LirLiteral::Int(value)),
                IntLiteralType::UInt => LirNodeType::Literal(LirLiteral::UInt(value as u64)),
                IntLiteralType::Byte => LirNodeType::Literal(LirLiteral::Byte(value as u8)),
            },
            MiddleNodeType::FloatLiteral(f) => LirNodeType::Literal(LirLiteral::Float(f)),
            MiddleNodeType::CharLiteral(c) => LirNodeType::Literal(LirLiteral::Char(c)),
            MiddleNodeType::Null => LirNodeType::null(),
            MiddleNodeType::StringLiteral(s) => {
                LirNodeType::Literal(LirLiteral::String(s.to_string()))
            }
            MiddleNodeType::ListLiteral(data_type, elements) => LirNodeType::List {
                elements: self.lower_nodes(elements),
                data_type,
            },
            MiddleNodeType::AggregateExpression { identifier, value } => LirNodeType::Aggregate {
                name: identifier.map(|i| i.to_string()),
                fields: ObjectMap(
                    value
                        .0
                        .into_iter()
                        .map(|(field_name, field_node)| {
                            (field_name.to_string(), self.lower_node(field_node))
                        })
                        .collect(),
                ),
            },
            MiddleNodeType::EmptyLine => LirNodeType::noop(),
            MiddleNodeType::Spawn { value } => LirNodeType::Spawn {
                callee: Box::new(self.lower_node(*value)),
            },
            MiddleNodeType::Drop(name) => LirNodeType::Drop(name.to_string().into_boxed_str()),
            MiddleNodeType::Move(name) => LirNodeType::Move(name.to_string().into_boxed_str()),
            MiddleNodeType::Identifier(name) => {
                LirNodeType::Load(name.to_string().into_boxed_str())
            }
            MiddleNodeType::VariableDeclaration {
                identifier, value, ..
            } => {
                if let MiddleNodeType::FunctionDeclaration { .. } = value.node_type {
                    self.last_ident = Some(identifier.to_string());
                } else {
                    self.last_ident = None;
                }

                let val = self.lower_node(*value);

                self.add_instr(LirNode::new(
                    identifier.span,
                    LirNodeType::Declare {
                        dest: identifier.to_string().into_boxed_str(),
                        value: Box::new(val),
                    },
                ));

                LirNodeType::null()
            }

            MiddleNodeType::AssignmentExpression { identifier, value } => {
                let rhs = self.lower_node(*value);
                let ident_span = identifier.span;

                let (lhs, old_expr) = match identifier.node_type {
                    MiddleNodeType::Identifier(name) => {
                        let name = name.to_string().into_boxed_str();
                        (
                            Some(LirLValue::Var(name.clone())),
                            Some(LirNodeType::Load(name)),
                        )
                    }
                    MiddleNodeType::DerefStatement { value } => {
                        let ptr_expr = self.lower_node(*value);
                        let ptr_tmp = self.get_temp();
                        self.add_instr(LirNode::new(
                            ident_span,
                            LirNodeType::Declare {
                                dest: ptr_tmp.clone().into_boxed_str(),
                                value: Box::new(ptr_expr),
                            },
                        ));
                        let ptr_load = LirNodeType::Load(ptr_tmp.into_boxed_str());
                        (
                            Some(LirLValue::Ptr(Box::new(ptr_load.clone()))),
                            Some(LirNodeType::Deref(Box::new(ptr_load))),
                        )
                    }
                    MiddleNodeType::MemberExpression { path } => {
                        if path.is_empty() {
                            (
                                Some(LirLValue::Var(Box::<str>::from("<invalid>"))),
                                Some(LirNodeType::null()),
                            )
                        } else {
                            let mut base_path = path.clone();
                            let (last_node, last_is_index) = base_path.pop().unwrap_or_else(|| {
                                (
                                    MiddleNode::new(MiddleNodeType::EmptyLine, ident_span),
                                    false,
                                )
                            });

                            let base_expr = if base_path.is_empty() {
                                self.lower_node(last_node.clone())
                            } else {
                                self.lower_member_path(base_path, true)
                            };

                            let base_tmp = self.get_temp();
                            self.add_instr(LirNode::new(
                                ident_span,
                                LirNodeType::Declare {
                                    dest: base_tmp.clone().into_boxed_str(),
                                    value: Box::new(base_expr),
                                },
                            ));
                            let base_load = LirNodeType::Load(base_tmp.into_boxed_str());

                            if last_is_index {
                                let index_expr = self.lower_node(last_node);
                                let index_tmp = self.get_temp();
                                self.add_instr(LirNode::new(
                                    ident_span,
                                    LirNodeType::Declare {
                                        dest: index_tmp.clone().into_boxed_str(),
                                        value: Box::new(index_expr),
                                    },
                                ));

                                let index_load = LirNodeType::Load(index_tmp.into_boxed_str());

                                (
                                    Some(LirLValue::Ptr(Box::new(LirNodeType::Index(
                                        Box::new(base_load.clone()),
                                        Box::new(index_load.clone()),
                                    )))),
                                    Some(LirNodeType::Index(
                                        Box::new(base_load),
                                        Box::new(index_load),
                                    )),
                                )
                            } else {
                                let field = last_node.member_field();
                                (
                                    Some(LirLValue::Ptr(Box::new(LirNodeType::Member(
                                        Box::new(base_load.clone()),
                                        field.clone(),
                                    )))),
                                    Some(LirNodeType::Member(Box::new(base_load), field)),
                                )
                            }
                        }
                    }
                    other => (
                        Some(self.lower_lvalue(MiddleNode::new(other, ident_span))),
                        Some(LirNodeType::null()),
                    ),
                };

                let old_expr = old_expr.unwrap_or_else(LirNodeType::null);
                let temp = self.get_temp();
                self.add_instr(LirNode::new(
                    ident_span,
                    LirNodeType::Declare {
                        dest: temp.clone().into_boxed_str(),
                        value: Box::new(old_expr),
                    },
                ));

                if let Some(lhs) = lhs {
                    self.add_instr(LirNode::new(
                        ident_span,
                        LirNodeType::Assign {
                            dest: lhs,
                            value: Box::new(rhs),
                        },
                    ));
                }

                LirNodeType::Load(temp.into_boxed_str())
            }
            MiddleNodeType::FunctionDeclaration {
                parameters,
                body,
                return_type,
                ..
            } => {
                let param_names: FxHashSet<String> = parameters
                    .iter()
                    .map(|(name, _, _)| name.text.clone())
                    .collect();

                let captures: Vec<(String, ParserDataType)> = body
                    .captured()
                    .into_iter()
                    .filter(|x| !param_names.contains(x.as_str()))
                    .map(|cap| {
                        (
                            cap.clone(),
                            self.env
                                .symbols
                                .variables
                                .get(cap)
                                .map(|v| v.data_type.clone())
                                .unwrap_or_else(|| {
                                    ParserDataType::new(Span::default(), ParserInnerType::Dynamic)
                                        .into()
                                }),
                        )
                    })
                    .collect();

                let internal_name = self.next_function_label();
                let mut sub_lowerer = LirEnvironment::new_with_hoist(self.env, false);

                let body_span = body.span;
                let is_temp_body = matches!(
                    body.node_type,
                    MiddleNodeType::ScopeDeclaration { is_temp: true, .. }
                );
                let fallback_expr = match &body.node_type {
                    MiddleNodeType::ScopeDeclaration { body, .. } => body.last().cloned(),
                    _ => None,
                };

                let (mut has_body_value, mut body_val) =
                    if let MiddleNodeType::Conditional { .. } = &body.node_type {
                        let ret = MiddleNode::new(
                            MiddleNodeType::Return {
                                value: Some(body.clone()),
                            },
                            body_span,
                        );
                        let _ = sub_lowerer.lower_node(ret);
                        (false, LirNodeType::null())
                    } else {
                        let body = sub_lowerer.lower_node(*body);
                        if is_temp_body {
                            (false, LirNodeType::null())
                        } else {
                            (!body.is_null(), body)
                        }
                    };

                if !has_body_value {
                    if let Some(expr) = fallback_expr {
                        if expr.node_type.is_simple_function_fallback() {
                            body_val = sub_lowerer.lower_node(expr);
                            has_body_value = true;
                        } else if is_temp_body {
                            sub_lowerer.lower_and_add_node(expr);
                        }
                    }
                }

                if sub_lowerer
                    .blocks
                    .last()
                    .map(|b| b.terminator.is_none())
                    .unwrap_or(false)
                    && has_body_value
                {
                    sub_lowerer.emit_return_value(body_span, Some(body_val));
                }

                self.registry.append(sub_lowerer.registry);

                let mut capture_names = Vec::with_capacity(captures.len());
                let mut captures_for_func = Vec::with_capacity(captures.len());

                for (n, t) in captures.into_iter() {
                    capture_names.push(n.clone().into_boxed_str());
                    captures_for_func.push((n.into_boxed_str(), t));
                }

                self.registry.functions.insert(
                    internal_name.clone(),
                    LirFunction {
                        name: internal_name.clone().into_boxed_str(),
                        params: parameters
                            .into_iter()
                            .map(|x| (x.0.text.into_boxed_str(), x.1))
                            .collect::<Vec<_>>()
                            .into_boxed_slice(),
                        captures: captures_for_func.into_boxed_slice(),
                        return_type,
                        blocks: sub_lowerer.blocks.into_boxed_slice(),
                    },
                );

                LirNodeType::Closure {
                    label: internal_name.into_boxed_str(),
                    captures: capture_names,
                }
            }
            MiddleNodeType::ExternFunction {
                abi,
                library,
                symbol,
                parameters,
                return_type,
            } => LirNodeType::ExternFunction {
                abi: abi.into_boxed_str(),
                library: library.into_boxed_str(),
                symbol: symbol.into_boxed_str(),
                parameters,
                return_type,
            },
            MiddleNodeType::EnumExpression {
                identifier,
                value,
                data,
            } => LirNodeType::Enum {
                variant: if let Some(obj) = self.env.typing.objects.get(&identifier.to_string())
                    && let MiddleTypeDefType::Enum { variants, .. } = &obj.object_type
                {
                    variants
                        .iter()
                        .position(|(name, _)| name.text == value.to_string())
                        .unwrap_or(0) as u32
                } else {
                    0
                },
                name: identifier.text.into_boxed_str(),
                payload: data.map(|d| Box::new(self.lower_node(*d))),
            },
            MiddleNodeType::ScopeDeclaration {
                body,
                is_temp: false,
                ..
            } => {
                if !self.allow_global_hoist {
                    self.lower_scope_items(body);
                    return LirNodeType::null();
                }

                for stmt in body {
                    if let MiddleNodeType::VariableDeclaration {
                        identifier,
                        data_type,
                        ..
                    } = &stmt.node_type
                    {
                        let global_name = identifier.to_string();
                        let global_type = data_type.clone();
                        let mut sub_lowerer = LirEnvironment::new_with_hoist(self.env, false);

                        let _ = sub_lowerer.lower_node(stmt);

                        self.registry.append(sub_lowerer.registry);

                        self.registry.globals.insert(
                            global_name.clone(),
                            LirGlobal {
                                name: global_name.into_boxed_str(),
                                data_type: global_type,
                                blocks: sub_lowerer.blocks.into_boxed_slice(),
                            },
                        );
                    } else {
                        self.lower_and_add_node(stmt);
                    }
                }

                LirNodeType::null()
            }
            MiddleNodeType::ScopeDeclaration {
                mut body, is_temp, ..
            } => {
                let last = body.pop();
                self.lower_scope_items(body);

                let Some(last) = last else {
                    return LirNodeType::null();
                };

                if is_temp {
                    let temp = self.get_temp();
                    let lowered = self.lower_node(last.clone());

                    if lowered.is_null() {
                        self.lower_and_add_node(last);
                        return LirNodeType::null();
                    }

                    self.add_instr(LirNode::new(
                        span,
                        LirNodeType::Declare {
                            dest: temp.clone().into_boxed_str(),
                            value: Box::new(lowered),
                        },
                    ));

                    LirNodeType::Load(temp.into_boxed_str())
                } else {
                    self.lower_and_add_node(last);
                    LirNodeType::null()
                }
            }
            MiddleNodeType::Conditional {
                comparison,
                then,
                otherwise,
            } => {
                let then_id = self.create_block();
                let else_id = self.create_block();
                let merge_id = self.create_block();

                let temp = self.get_temp();
                self.declare_temp_null(span, temp.as_str());

                let cond = self.lower_node(*comparison);
                self.set_terminator(LirTerminator::Branch {
                    span,
                    condition: cond,
                    then_block: then_id,
                    else_block: else_id,
                });

                self.switch_to(then_id);
                let then_val = self.lower_node(*then);
                if self.current_block_open() {
                    self.assign_temp_if_non_null(span, temp.as_str(), then_val);
                    self.jump_if_open(span, merge_id);
                }

                self.switch_to(else_id);
                let else_val = if let Some(alt) = otherwise {
                    self.lower_node(*alt)
                } else {
                    LirNodeType::null()
                };

                if self.current_block_open() {
                    self.assign_temp_if_non_null(span, temp.as_str(), else_val);
                    self.jump_if_open(span, merge_id);
                }

                self.switch_to(merge_id);
                LirNodeType::Load(temp.into_boxed_str())
            }
            MiddleNodeType::LoopDeclaration {
                state, body, label, ..
            } => {
                let header_id = self.create_block();
                let body_id = self.create_block();
                let exit_id = self.create_block();

                if let Some(s) = state {
                    self.lower_and_add_node(*s);
                }

                self.set_terminator(LirTerminator::Jump {
                    span,
                    target: header_id,
                });

                self.switch_to(header_id);
                self.set_terminator(LirTerminator::Jump {
                    span,
                    target: body_id,
                });

                self.loop_stack
                    .push((header_id, exit_id, label.map(|l| l.text)));

                self.switch_to(body_id);
                self.lower_and_add_node(*body);
                self.set_terminator(LirTerminator::Jump {
                    span,
                    target: header_id,
                });

                self.loop_stack.pop();

                self.switch_to(exit_id);
                LirNodeType::null()
            }
            MiddleNodeType::Return { value: None } => {
                self.emit_return_value(span, None);
                LirNodeType::null()
            }
            MiddleNodeType::Return { value: Some(v) } => {
                if let MiddleNodeType::Conditional {
                    comparison,
                    then,
                    otherwise,
                } = v.node_type
                {
                    let then_id = self.create_block();
                    let else_id = self.create_block();
                    let merge_id = self.create_block();

                    let cond = self.lower_node(*comparison);
                    self.set_terminator(LirTerminator::Branch {
                        span,
                        condition: cond,
                        then_block: then_id,
                        else_block: else_id,
                    });

                    self.switch_to(then_id);
                    let then_return =
                        MiddleNode::new(MiddleNodeType::Return { value: Some(then) }, span);
                    let _ = self.lower_node(then_return);

                    self.switch_to(else_id);
                    let else_return = MiddleNode::new(
                        MiddleNodeType::Return {
                            value: otherwise.map(|o| o),
                        },
                        span,
                    );
                    let _ = self.lower_node(else_return);

                    self.switch_to(merge_id);
                    return LirNodeType::null();
                }

                let value_span = v.span;
                let val = self.lower_node(*v);
                self.emit_return_value(value_span, Some(val));
                LirNodeType::null()
            }
            MiddleNodeType::Break { label, .. } => {
                self.jump_to_loop_target_if_present(span, Self::loop_label(&label), true);
                LirNodeType::null()
            }
            MiddleNodeType::Continue { label } => {
                self.jump_to_loop_target_if_present(span, Self::loop_label(&label), false);
                LirNodeType::null()
            }
            MiddleNodeType::MemberExpression { path } => self.lower_member_path(path, false),
            MiddleNodeType::DerefStatement { value } => {
                LirNodeType::Deref(Box::new(self.lower_node(*value)))
            }
            MiddleNodeType::RefStatement { value, .. } => {
                if let MiddleNodeType::Identifier(name) = value.node_type {
                    LirNodeType::RefLoad(name.text.into_boxed_str())
                } else {
                    LirNodeType::Ref(Box::new(self.lower_node(*value)))
                }
            }
            MiddleNodeType::BinaryExpression {
                left,
                right,
                operator,
            } => LirNodeType::Binary {
                left: Box::new(self.lower_node(*left)),
                right: Box::new(self.lower_node(*right)),
                operator,
            },
            MiddleNodeType::BooleanExpression {
                left,
                right,
                operator,
            } => {
                let then_id = self.create_block();
                let else_id = self.create_block();
                let merge_id = self.create_block();

                let temp = self.get_temp();
                self.declare_temp_null(span, temp.as_str());

                let cond = self.lower_node(*left);
                self.set_terminator(LirTerminator::Branch {
                    span,
                    condition: cond,
                    then_block: then_id,
                    else_block: else_id,
                });

                match operator {
                    BooleanOperator::And => {
                        self.switch_to(then_id);
                        let right_val = self.lower_node(*right);
                        let checked = LirNodeType::Boolean {
                            left: Box::new(right_val),
                            right: Box::new(LirNodeType::bool(true)),
                            operator,
                        };
                        if self.current_block_open() {
                            self.assign_var(span, temp.as_str(), checked);
                            self.jump_if_open(span, merge_id);
                        }

                        self.switch_to(else_id);
                        if self.current_block_open() {
                            self.assign_var(span, temp.as_str(), LirNodeType::bool(false));
                            self.jump_if_open(span, merge_id);
                        }
                    }
                    BooleanOperator::Or => {
                        self.switch_to(then_id);
                        if self.current_block_open() {
                            self.assign_var(span, temp.as_str(), LirNodeType::bool(true));
                            self.jump_if_open(span, merge_id);
                        }

                        self.switch_to(else_id);
                        let right_val = self.lower_node(*right);
                        let checked = LirNodeType::Boolean {
                            left: Box::new(right_val),
                            right: Box::new(LirNodeType::bool(false)),
                            operator,
                        };
                        if self.current_block_open() {
                            self.assign_var(span, temp.as_str(), checked);
                            self.jump_if_open(span, merge_id);
                        }
                    }
                }

                self.switch_to(merge_id);
                LirNodeType::Load(temp.into_boxed_str())
            }
            MiddleNodeType::ComparisonExpression {
                left,
                right,
                operator,
            } => LirNodeType::Comparison {
                left: Box::new(self.lower_node(*left)),
                right: Box::new(self.lower_node(*right)),
                operator,
            },
            MiddleNodeType::CallExpression { caller, args } => {
                let caller_node = *caller;

                let needs_ref_first_arg = if let MiddleNodeType::Identifier(name) =
                    &caller_node.node_type
                    && let Some(var) = self.env.symbols.variables.get(&name.text)
                    && let ParserInnerType::Function { parameters, .. } = &var.data_type.data_type
                    && let Some(first) = parameters.first()
                {
                    matches!(first.data_type, ParserInnerType::Ref(_, _))
                } else {
                    false
                };

                let l_caller = self.lower_node(caller_node);
                let mut l_args = self.lower_nodes(args);

                if let LirNodeType::Load(name) | LirNodeType::Move(name) = &l_caller
                    && let Some(var) = self.env.symbols.variables.get(name.as_ref())
                    && let ParserInnerType::Function { parameters, .. } = &var.data_type.data_type
                {
                    let expected = parameters.len();
                    while l_args.len() > expected {
                        if let Some(pos) = l_args
                            .iter()
                            .position(LirNodeType::is_invalid_member_placeholder)
                        {
                            l_args.remove(pos);
                        } else {
                            l_args.remove(0);
                        }
                    }
                }

                if needs_ref_first_arg
                    && let Some(first_arg) = l_args.get_mut(0)
                    && matches!(first_arg, LirNodeType::Load(_))
                {
                    *first_arg = LirNodeType::Ref(Box::new(std::mem::replace(
                        first_arg,
                        LirNodeType::null(),
                    )));
                }

                LirNodeType::Call {
                    caller: Box::new(l_caller),
                    args: l_args,
                }
            }
            MiddleNodeType::AsExpression {
                value,
                data_type,
                failure_mode,
            } => LirNodeType::As(Box::new(self.lower_node(*value)), data_type, failure_mode),
            MiddleNodeType::IsExpression { value, data_type } => {
                LirNodeType::Is(Box::new(self.lower_node(*value)), data_type)
            }
            MiddleNodeType::DebugExpression { value, .. } => self.lower_node(*value),
            MiddleNodeType::NegExpression { value } => {
                let val = self.lower_node(*value);
                LirNodeType::Binary {
                    left: Box::new(LirNodeType::Literal(LirLiteral::Int(0))),
                    right: Box::new(val),
                    operator: BinaryOperator::Sub,
                }
            }

            MiddleNodeType::RangeDeclaration {
                from,
                to,
                inclusive,
            } => {
                let from = self.lower_node(*from);
                let to = self.lower_node(*to);
                LirNodeType::Range {
                    from: Box::new(from),
                    to: Box::new(to),
                    inclusive,
                }
            }
        }
    }

    pub fn lower_lvalue(&mut self, node: MiddleNode) -> LirLValue {
        match node.node_type {
            MiddleNodeType::Identifier(name) => LirLValue::Var(name.to_string().into_boxed_str()),
            MiddleNodeType::DerefStatement { value } => {
                LirLValue::Ptr(Box::new(self.lower_node(*value)))
            }
            MiddleNodeType::MemberExpression { path } => {
                LirLValue::Ptr(Box::new(self.lower_member_lvalue(path)))
            }
            _ => LirLValue::Var(Box::<str>::from("<invalid>")),
        }
    }
}

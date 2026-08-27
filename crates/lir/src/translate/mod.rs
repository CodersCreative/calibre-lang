use crate::{
    ast::{BlockId, LirLValue, LirNode, LirNodeType, LirTerminator},
    environment::{LirEnvironment, LirFunction, LirGlobal, LirRegistry},
};
use calibre_mir::{
    ast::{MiddleNode, MiddleNodeType, MirDeref, MirField, MirIdentifier, MirIndex, MirReturn},
    environment::MiddleEnvironment,
    typing::{MiddleImpl, MiddleTrait, MiddleTypeDefType},
};
use calibre_parser::{
    Span,
    ast::{
        ObjectMap,
        idents::ParserText,
        types::{ParserDataType, ParserInnerType},
    },
};
use rustc_hash::{FxHashMap, FxHashSet};
use tracing::{debug, info, instrument, trace};

pub mod access;
pub mod declarations;
pub mod expressions;
pub mod flow;
pub mod literals;
pub mod memory;
pub mod statements;

pub trait LirLowering {
    fn lower<'a>(self, env: &mut LirEnvironment<'a>, span: Span) -> LirNodeType;

    #[inline(always)]
    fn lower_lvalue<'a>(self, _env: &mut LirEnvironment<'a>, _span: Span) -> LirLValue
    where
        Self: Sized,
    {
        unreachable!()
    }
}

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
                data_type: ParserDataType::null(span),
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
        trait_name: &str,
    ) -> FxHashMap<String, String> {
        let mut methods: FxHashMap<String, String> = FxHashMap::default();
        if let Some(trait_def) = trait_def {
            for member in trait_def.members.keys() {
                if let Some(mapped) = imp.get_member(member, &[]) {
                    methods.insert(member.clone(), mapped.symbol_name.clone());
                } else if let Some(trait_member) = trait_def.members.get(member)
                    && trait_member.default.is_some()
                {
                    let symbol_name = format!("{}.{}", trait_name, member);
                    methods.insert(member.clone(), symbol_name);
                }
            }
        } else {
            for (member, mapped) in imp.get_all_members() {
                methods.insert(member.clone(), mapped.symbol_name.clone());
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
            let trait_map = out.entry(concrete.clone()).or_default();

            for trait_name in &imp.traits {
                let methods = Self::collect_trait_methods(
                    imp,
                    env.typing.trait_defs.get(trait_name),
                    trait_name,
                );
                if !methods.is_empty() {
                    trait_map.insert(trait_name.clone(), methods);
                }
            }
        }

        out
    }

    #[instrument(skip_all)]
    pub fn lower(env: &'a MiddleEnvironment, node: MiddleNode) -> LirRegistry {
        let mut this = Self::new(env);
        this.lower_and_add_node(node);

        info!(
            functions = this.registry.functions.len(),
            "LIR lowering completed"
        );

        this.registry
    }

    #[instrument(skip_all, fields(root_name = %root_name))]
    pub fn lower_with_root(
        env: &'a MiddleEnvironment,
        node: MiddleNode,
        root_name: String,
    ) -> LirRegistry {
        debug!("lowering with root");
        let mut this = Self::new(env);
        this.lower_and_add_node(node);
        if !this.blocks.is_empty() {
            debug!("creating global for root");
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

    #[instrument(skip_all)]
    pub fn lower_node(&mut self, node: MiddleNode) -> LirNodeType {
        let span = node.span;
        trace!("lowering MIR node to LIR");
        match node.node_type {
            MiddleNodeType::Null => LirNodeType::null(),
            MiddleNodeType::EmptyLine => LirNodeType::noop(),

            MiddleNodeType::Emit(x) => x.lower(self, span),
            MiddleNodeType::IntLiteral(x) => x.lower(self, span),
            MiddleNodeType::FloatLiteral(x) => x.lower(self, span),
            MiddleNodeType::BigLiteral(x) => x.lower(self, span),
            MiddleNodeType::CharLiteral(x) => x.lower(self, span),
            MiddleNodeType::StringLiteral(x) => x.lower(self, span),
            MiddleNodeType::ListLiteral(x) => x.lower(self, span),
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

            MiddleNodeType::Spawn(x) => x.lower(self, span),
            MiddleNodeType::Drop(x) => x.lower(self, span),
            MiddleNodeType::Move(x) => x.lower(self, span),
            MiddleNodeType::Identifier(x) => x.lower(self, span),
            MiddleNodeType::VariableDeclaration {
                identifier,
                value,
                data_type,
                ..
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
                        data_type,
                        value: Box::new(val),
                    },
                ));

                LirNodeType::null()
            }

            MiddleNodeType::AssignmentExpression { identifier, value } => {
                let rhs = self.lower_node(*value);
                let ident_span = identifier.span;

                let (lhs, old_expr) = match identifier.node_type {
                    MiddleNodeType::Identifier(MirIdentifier { identifier }) => {
                        let name = identifier.to_string().into_boxed_str();
                        (
                            Some(LirLValue::Var(name.clone())),
                            Some(LirNodeType::Load(name)),
                        )
                    }
                    MiddleNodeType::DerefStatement(MirDeref { value }) => {
                        let ptr_expr = self.lower_node(*value);
                        let ptr_tmp = self.get_temp();
                        self.add_instr(LirNode::new(
                            ident_span,
                            LirNodeType::Declare {
                                dest: ptr_tmp.clone().into_boxed_str(),
                                data_type: ParserDataType::auto(ident_span),
                                value: Box::new(ptr_expr),
                            },
                        ));
                        let ptr_load = LirNodeType::Load(ptr_tmp.into_boxed_str());
                        (
                            Some(LirLValue::Ptr(Box::new(ptr_load.clone()))),
                            Some(LirNodeType::Deref(Box::new(ptr_load))),
                        )
                    }
                    MiddleNodeType::FieldAccess(MirField { base, field }) => {
                        let base_expr = self.lower_node(*base);
                        let base_tmp = self.get_temp();
                        self.add_instr(LirNode::new(
                            ident_span,
                            LirNodeType::Declare {
                                dest: base_tmp.clone().into_boxed_str(),
                                data_type: ParserDataType::auto(ident_span),
                                value: Box::new(base_expr),
                            },
                        ));
                        let base_load = LirNodeType::Load(base_tmp.into_boxed_str());

                        (
                            Some(LirLValue::Ptr(Box::new(LirNodeType::Member(
                                Box::new(base_load.clone()),
                                field.text.clone().into_boxed_str(),
                            )))),
                            Some(LirNodeType::Member(
                                Box::new(base_load),
                                field.text.into_boxed_str(),
                            )),
                        )
                    }
                    MiddleNodeType::IndexAccess(MirIndex { base, index }) => {
                        let base_expr = self.lower_node(*base);
                        let base_tmp = self.get_temp();
                        self.add_instr(LirNode::new(
                            ident_span,
                            LirNodeType::Declare {
                                dest: base_tmp.clone().into_boxed_str(),
                                data_type: ParserDataType::auto(ident_span),
                                value: Box::new(base_expr),
                            },
                        ));
                        let base_load = LirNodeType::Load(base_tmp.into_boxed_str());

                        let index_expr = self.lower_node(*index);
                        let index_tmp = self.get_temp();
                        self.add_instr(LirNode::new(
                            ident_span,
                            LirNodeType::Declare {
                                dest: index_tmp.clone().into_boxed_str(),
                                data_type: ParserDataType::auto(ident_span),
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
                        data_type: ParserDataType::auto(ident_span),
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
                        let _ = MirReturn {
                            value: Some(body.clone()),
                        }
                        .lower(self, span);
                        (false, LirNodeType::null())
                    } else {
                        let body = sub_lowerer.lower_node(*body);
                        if is_temp_body {
                            (false, LirNodeType::null())
                        } else {
                            (!body.is_null(), body)
                        }
                    };

                if !has_body_value && let Some(expr) = fallback_expr {
                    if expr.node_type.is_simple_function_fallback() {
                        body_val = sub_lowerer.lower_node(expr);
                        has_body_value = true;
                    } else if is_temp_body {
                        sub_lowerer.lower_and_add_node(expr);
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
                            data_type: ParserDataType::auto(span),
                            value: Box::new(lowered),
                        },
                    ));

                    LirNodeType::Load(temp.into_boxed_str())
                } else {
                    self.lower_and_add_node(last);
                    LirNodeType::null()
                }
            }
            MiddleNodeType::Conditional(x) => x.lower(self, span),
            MiddleNodeType::LoopDeclaration(x) => x.lower(self, span),
            MiddleNodeType::Return(x) => x.lower(self, span),
            MiddleNodeType::Break(x) => x.lower(self, span),
            MiddleNodeType::Continue(x) => x.lower(self, span),
            MiddleNodeType::FieldAccess(x) => x.lower(self, span),
            MiddleNodeType::ScopeAccess(x) => x.lower(self, span),
            MiddleNodeType::IndexAccess(x) => x.lower(self, span),
            MiddleNodeType::DerefStatement(x) => x.lower(self, span),
            MiddleNodeType::RefStatement(x) => x.lower(self, span),
            MiddleNodeType::BinaryExpression(x) => x.lower(self, span),
            MiddleNodeType::BooleanExpression(x) => x.lower(self, span),
            MiddleNodeType::ComparisonExpression(x) => x.lower(self, span),
            MiddleNodeType::CallExpression(x) => x.lower(self, span),
            MiddleNodeType::AsExpression(x) => x.lower(self, span),
            MiddleNodeType::IsExpression(x) => x.lower(self, span),
            MiddleNodeType::NegExpression(x) => x.lower(self, span),
            MiddleNodeType::RangeDeclaration(x) => x.lower(self, span),
            MiddleNodeType::DebugExpression { value, .. } => self.lower_node(*value),
        }
    }

    pub fn lower_lvalue(&mut self, node: MiddleNode) -> LirLValue {
        match node.node_type {
            MiddleNodeType::Identifier(x) => x.lower_lvalue(self, node.span),
            MiddleNodeType::DerefStatement(x) => x.lower_lvalue(self, node.span),
            MiddleNodeType::FieldAccess(x) => x.lower_lvalue(self, node.span),
            MiddleNodeType::ScopeAccess(x) => x.lower_lvalue(self, node.span),
            MiddleNodeType::IndexAccess(x) => x.lower_lvalue(self, node.span),
            _ => unreachable!(),
        }
    }
}

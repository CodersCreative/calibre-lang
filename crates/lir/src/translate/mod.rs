use crate::{
    ast::{BlockId, LirLValue, LirNode, LirNodeType, LirTerminator},
    environment::{LirEnvironment, LirGlobal, LirRegistry},
};
use calibre_mir::{
    ast::{MiddleNode, MiddleNodeType},
    environment::MiddleEnvironment,
    typing::{MiddleImpl, MiddleTrait},
};
use calibre_parser::{
    Span,
    ast::{
        idents::ParserText,
        types::{ParserDataType, ParserInnerType},
    },
};
use rustc_hash::FxHashMap;
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
            MiddleNodeType::AggregateExpression(x) => x.lower(self, span),
            MiddleNodeType::Spawn(x) => x.lower(self, span),
            MiddleNodeType::Drop(x) => x.lower(self, span),
            MiddleNodeType::Move(x) => x.lower(self, span),
            MiddleNodeType::Identifier(x) => x.lower(self, span),
            MiddleNodeType::VariableDeclaration(x) => x.lower(self, span),

            MiddleNodeType::AssignmentExpression(x) => x.lower(self, span),
            MiddleNodeType::FunctionDeclaration(x) => x.lower(self, span),
            MiddleNodeType::ExternFunction(x) => x.lower(self, span),
            MiddleNodeType::EnumExpression(x) => x.lower(self, span),
            MiddleNodeType::ScopeDeclaration(x) => x.lower(self, span),
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
            MiddleNodeType::DebugExpression(x) => x.lower(self, span),
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

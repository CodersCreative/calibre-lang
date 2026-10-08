use crate::{
    ast::{BlockId, LirAssign, LirDeclare, LirLValue, LirNode, LirNodeType, LirTerminator},
    environment::{LirEnvironment, LirGlobal, LirRegistry},
};
use calibre_mir::{
    ast::{MiddleNode, MiddleNodeType, types::MirDataType},
    environment::MiddleEnvironment,
    symbols::VariableKey,
};
use calibre_parser::Span;
use tracing::{debug, info, instrument, trace};
use ustr::Ustr;

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
    fn lower_nodes<I>(&mut self, nodes: I) -> Box<[LirNodeType]>
    where
        I: IntoIterator<Item = MiddleNode>,
    {
        nodes
            .into_iter()
            .map(|node| self.lower_node(node))
            .collect()
    }

    fn assign_var(&mut self, span: Span, name: VariableKey, value: LirNodeType) {
        self.add_instr(LirNode::new(
            span,
            LirNodeType::Assign(LirAssign {
                dest: LirLValue::Var(name),
                value: Box::new(value),
            }),
        ));
    }

    fn declare_temp_null(&mut self, span: Span, dest: VariableKey, data_type: MirDataType) {
        self.add_instr(LirNode::new(
            span,
            LirNodeType::Declare(LirDeclare {
                dest,
                data_type,
                value: Box::new(LirNodeType::null()),
                is_referenced: false,
            }),
        ));
    }

    fn jump_if_open(&mut self, span: Span, target: BlockId) {
        if self.current_block_open() {
            self.set_terminator(LirTerminator::Jump { span, target });
        }
    }

    #[inline]
    fn assign_temp_if_non_null(&mut self, span: Span, temp: VariableKey, value: LirNodeType) {
        if !value.is_null() {
            self.assign_var(span, temp, value);
        }
    }

    #[inline]
    fn jump_to_loop_target_if_present(&mut self, span: Span, label: Option<&Ustr>, use_exit: bool) {
        if let Some(target) = self.find_loop_target(label, use_exit) {
            self.set_terminator(LirTerminator::Jump { span, target });
        }
    }

    #[inline]
    fn emit_return_value(&mut self, span: Span, value: Option<LirNodeType>) {
        self.set_terminator(LirTerminator::Return { span, value });
    }

    #[inline]
    fn lower_scope_items<I>(&mut self, body: I)
    where
        I: IntoIterator<Item = MiddleNode>,
    {
        for stmt in body {
            if !self.current_block_open() {
                break;
            }
            self.lower_and_add_node(stmt);
        }
    }

    #[inline]
    fn next_function_label(&mut self) -> VariableKey {
        if let Some(name) = self.last_ident.take()
            && !name.name().contains("curry_capture")
        {
            return name;
        }

        self.get_temp()
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
        root_name: VariableKey,
    ) -> LirRegistry {
        debug!("lowering with root");
        let mut this = Self::new(env);
        this.lower_and_add_node(node);
        if !this.blocks.is_empty() {
            debug!("creating global for root");
            let blocks = std::mem::take(&mut this.blocks)
                .into_iter()
                .map(Some)
                .collect();

            this.registry.globals.insert(
                root_name.clone(),
                LirGlobal {
                    name: root_name,
                    data_type: MirDataType::Dynamic,
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
            MiddleNodeType::Discriminant(x) => x.lower(self, span),
            MiddleNodeType::FieldAccess(x) => x.lower(self, span),
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
        }
    }

    pub fn lower_lvalue(&mut self, node: MiddleNode) -> LirLValue {
        match node.node_type {
            MiddleNodeType::Identifier(x) => x.lower_lvalue(self, node.span),
            MiddleNodeType::DerefStatement(x) => x.lower_lvalue(self, node.span),
            MiddleNodeType::FieldAccess(x) => x.lower_lvalue(self, node.span),
            MiddleNodeType::IndexAccess(x) => x.lower_lvalue(self, node.span),
            _ => unreachable!(),
        }
    }
}

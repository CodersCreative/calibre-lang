use calibre_mir::{
    ast::{MiddleNode, MiddleNodeType},
    environment::{MiddleEnvironment, MiddleTypeDefType},
};
use calibre_parser::Span;
use calibre_parser::ast::{
    AsFailureMode, ObjectMap, ParserDataType, ParserInnerType,
    binary::BinaryOperator,
    comparison::{BooleanOperator, ComparisonOperator},
};
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};
use std::fmt::Display;
use std::sync::atomic::{AtomicU64, Ordering};

static GLOBAL_TEMP_ID: AtomicU64 = AtomicU64::new(0);

#[derive(Debug, Clone, Default)]
pub struct LirRegistry {
    pub functions: FxHashMap<String, LirFunction>,
    pub globals: FxHashMap<String, LirGlobal>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LirInstr {
    pub span: Span,
    pub node_type: LirNodeType,
}

impl LirInstr {
    pub fn new(span: Span, node_type: LirNodeType) -> Self {
        Self { span, node_type }
    }
}

impl Display for LirInstr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.node_type)
    }
}

impl LirRegistry {
    pub fn append(&mut self, other: LirRegistry) {
        for func in other.functions {
            self.functions.insert(func.0, func.1);
        }
    }
}

impl Display for LirRegistry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for val in &self.globals {
            writeln!(f, "{}", val.1)?;
        }

        for func in &self.functions {
            writeln!(f, "{}\n", func.1)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct LirGlobal {
    pub name: Box<str>,
    pub data_type: ParserDataType,
    pub blocks: Box<[LirBlock]>,
}

impl Display for LirGlobal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut txt = format!("const {} : {} =", self.name, self.data_type);

        for block in &self.blocks {
            txt.push_str(&format!("\n{}", block).replace("\n", "\n\t"));
        }

        write!(f, "{}", txt)
    }
}

#[derive(Debug, Clone)]
pub struct LirFunction {
    pub name: Box<str>,
    pub params: Box<[(Box<str>, ParserDataType)]>,
    pub captures: Box<[(Box<str>, ParserDataType)]>,
    pub return_type: ParserDataType,
    pub blocks: Box<[LirBlock]>,
}

impl Display for LirFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut txt = format!("const {} = fn(", self.name);
        for param in &self.params {
            txt.push_str(&format!("{} : {}, ", param.0, param.1));
        }

        txt = txt.trim_end().trim_end_matches(",").to_string();
        txt.push_str(&format!(") -> {}:", self.return_type));

        for block in &self.blocks {
            txt.push_str(&format!("\n{}", block).replace("\n", "\n\t"));
        }

        write!(f, "{}", txt)
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct BlockId(pub u32);

#[repr(u8)]
#[derive(Debug, Clone, PartialEq)]
pub enum LirLiteral {
    Int(i64),
    UInt(u64),
    Float(f64),
    Char(char),
    String(String),
    Null,
}

impl Display for LirLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int(x) => write!(f, "{x}"),
            Self::UInt(x) => write!(f, "{x}u"),
            Self::Float(x) => write!(f, "{x}f"),
            Self::Char(x) => write!(f, "'{x}'"),
            Self::String(x) => write!(f, "{x:?}"),
            Self::Null => write!(f, "null"),
        }
    }
}

#[repr(u8)]
#[derive(Debug, Clone, PartialEq)]
pub enum LirNodeType {
    Noop,
    Spawn {
        callee: Box<LirNodeType>,
    },
    Closure {
        label: Box<str>,
        captures: Vec<Box<str>>,
    },
    List {
        elements: Vec<LirNodeType>,
        data_type: ParserDataType,
    },
    Aggregate {
        name: Option<String>,
        fields: ObjectMap<LirNodeType>,
    },
    Range {
        from: Box<LirNodeType>,
        to: Box<LirNodeType>,
        inclusive: bool,
    },
    Literal(LirLiteral),
    Load(Box<str>),
    Boolean {
        left: Box<LirNodeType>,
        right: Box<LirNodeType>,
        operator: BooleanOperator,
    },
    Move(Box<str>),
    Drop(Box<str>),
    Binary {
        left: Box<LirNodeType>,
        right: Box<LirNodeType>,
        operator: BinaryOperator,
    },
    Comparison {
        left: Box<LirNodeType>,
        right: Box<LirNodeType>,
        operator: ComparisonOperator,
    },
    Call {
        caller: Box<LirNodeType>,
        args: Vec<LirNodeType>,
    },
    Deref(Box<LirNodeType>),
    Ref(Box<LirNodeType>),
    Index(Box<LirNodeType>, Box<LirNodeType>),
    Member(Box<LirNodeType>, Box<str>),
    Enum {
        name: Box<str>,
        variant: u32,
        payload: Option<Box<LirNodeType>>,
    },
    As(Box<LirNodeType>, ParserDataType, AsFailureMode),
    Assign {
        dest: LirLValue,
        value: Box<LirNodeType>,
    },
    Declare {
        dest: Box<str>,
        value: Box<LirNodeType>,
    },
    ExternFunction {
        abi: Box<str>,
        library: Box<str>,
        symbol: Box<str>,
        parameters: Vec<ParserDataType>,
        return_type: ParserDataType,
    },
}

impl Display for LirNodeType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Noop => "noop".to_string(),
                Self::Spawn { callee } => format!("spawn {}", callee),
                Self::List {
                    elements,
                    data_type,
                } => {
                    let mut txt = format!("list:<{}>[", data_type);
                    for element in elements {
                        txt.push_str(&format!("{}, ", element));
                    }
                    txt = txt.trim_end().trim_end_matches(",").to_string();
                    txt.push(']');
                    txt
                }
                Self::Closure { label, captures } => {
                    let mut txt = format!("let {} = fn[", label);
                    for capture in captures {
                        txt.push_str(&format!("{}, ", capture));
                    }
                    txt = txt.trim_end().trim_end_matches(",").to_string();
                    txt.push_str("]");
                    txt
                }
                Self::Aggregate { name, fields: _ } => {
                    let txt = if let Some(name) = name {
                        name.to_string()
                    } else {
                        String::new()
                    };

                    txt
                }
                Self::Literal(x) => x.to_string(),
                Self::As(node, data_type, failure_mode) => {
                    let suffix = match failure_mode {
                        AsFailureMode::Panic => "!",
                        AsFailureMode::Option => "?",
                        AsFailureMode::Result => "",
                    };
                    format!("{} as{} {}", node, suffix, data_type)
                }
                Self::Declare { dest, value } => format!("let {} = {}", dest, value),
                Self::Assign { dest, value } => format!("{} = {}", dest, value),
                Self::ExternFunction {
                    abi,
                    library,
                    symbol,
                    parameters,
                    return_type,
                } => {
                    let mut txt = format!("extern \"{}\" {}(", abi, symbol);
                    for (i, param) in parameters.iter().enumerate() {
                        if i > 0 {
                            txt.push_str(", ");
                        }
                        txt.push_str(&param.to_string());
                    }
                    txt.push_str(") -> ");
                    txt.push_str(&return_type.to_string());
                    txt.push_str(&format!(" from {}", library));
                    txt
                }
                Self::Range {
                    from,
                    to,
                    inclusive,
                } => format!("{}..{}{}", from, if *inclusive { "=" } else { "" }, to),
                Self::Boolean {
                    left,
                    right,
                    operator,
                } => format!("{} {} {}", left, operator, right),
                Self::Comparison {
                    left,
                    right,
                    operator,
                } => format!("{} {} {}", left, operator, right),
                Self::Binary {
                    left,
                    right,
                    operator,
                } => format!("{} {} {}", left, operator, right),
                Self::Load(x) => format!("{}", x),
                Self::Call { caller, args } => {
                    let mut txt = format!("{}(", caller);
                    for arg in args {
                        txt.push_str(&format!("{}, ", arg));
                    }
                    txt = txt.trim_end().trim_end_matches(",").to_string();
                    txt.push_str(&format!(")"));
                    txt
                }
                Self::Ref(x) => format!("&{}", x),
                Self::Deref(x) => format!("*{}", x),
                Self::Drop(x) => format!("drop {}", x),
                Self::Move(x) => format!("move {}", x),
                Self::Enum {
                    name,
                    variant,
                    payload,
                } => format!(
                    "{}.{}{}",
                    name,
                    variant,
                    match payload {
                        Some(x) => format!(" : {}", x),
                        None => String::new(),
                    }
                ),
                Self::Index(x, i) => format!("{}[{}]", x, i),
                Self::Member(x, i) => format!("{}.{}", x, i),
            }
        )
    }
}

#[repr(u8)]
#[derive(Debug, Clone, PartialEq)]
pub enum LirLValue {
    Var(Box<str>),
    Ptr(Box<LirNodeType>),
}

impl Display for LirLValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Var(x) => write!(f, "{}", x),
            Self::Ptr(x) => write!(f, "{}", x),
        }
    }
}

#[repr(u8)]
#[derive(Debug, Clone)]
pub enum LirTerminator {
    Jump {
        span: Span,
        target: BlockId,
    },
    Branch {
        span: Span,
        condition: LirNodeType,
        then_block: BlockId,
        else_block: BlockId,
    },
    Return {
        span: Span,
        value: Option<LirNodeType>,
    },
}

impl Display for LirTerminator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Jump { target, .. } => write!(f, "jmp blk {}", target.0),
            Self::Branch {
                condition,
                then_block,
                else_block,
                ..
            } => write!(
                f,
                "{} ? jmp blk {} : jmp blk {}",
                condition, then_block.0, else_block.0
            ),
            Self::Return { value, .. } => write!(
                f,
                "return{}",
                if let Some(x) = value.as_ref() {
                    format!(" {}", x)
                } else {
                    String::new()
                }
            ),
        }
    }
}

#[derive(Debug, Clone)]
pub struct LirBlock {
    pub id: BlockId,
    pub instructions: Vec<LirInstr>,
    pub terminator: Option<LirTerminator>,
}

impl Display for LirBlock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut txt = format!("blk {}:", self.id.0);
        for instr in &self.instructions {
            txt.push_str(&format!("\n{};", instr));
        }

        if let Some(t) = self.terminator.as_ref() {
            txt.push_str(&format!("\n{};", t));
        }

        write!(f, "{}", txt.replace("\n", "\n\t"))
    }
}

#[derive(Debug, Clone)]
pub struct LirEnvironment<'a> {
    pub env: &'a MiddleEnvironment,
    pub last_ident: Option<String>,
    pub registry: LirRegistry,
    pub blocks: Vec<LirBlock>,
    pub current_block: BlockId,
    pub loop_stack: Vec<(BlockId, BlockId, Option<String>)>,
    allow_global_hoist: bool,
}

impl<'a> LirEnvironment<'a> {
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

    pub fn new(env: &'a MiddleEnvironment) -> Self {
        Self::new_with_hoist(env, true)
    }

    pub fn new_with_hoist(env: &'a MiddleEnvironment, allow_global_hoist: bool) -> Self {
        let entry_id = BlockId(0);
        Self {
            env,
            last_ident: None,
            registry: LirRegistry::default(),
            blocks: vec![LirBlock {
                id: entry_id,
                instructions: vec![],
                terminator: None,
            }],
            current_block: entry_id,
            loop_stack: vec![],
            allow_global_hoist,
        }
    }

    fn get_temp(&mut self) -> String {
        let id = GLOBAL_TEMP_ID.fetch_add(1, Ordering::Relaxed);
        format!("tmp_{}", id)
    }

    fn member_field(step: MiddleNode) -> Box<str> {
        match step.node_type {
            MiddleNodeType::Identifier(name) => name.text.into_boxed_str(),
            MiddleNodeType::IntLiteral { value, signed } => {
                if signed {
                    value.to_string().into_boxed_str()
                } else {
                    format!("{value}u").into_boxed_str()
                }
            }
            MiddleNodeType::FloatLiteral(x) => x.to_string().into_boxed_str(),
            _ => "<invalid>".to_string().into_boxed_str(),
        }
    }

    fn lower_member_lvalue(&mut self, path: Vec<(MiddleNode, bool)>) -> LirNodeType {
        let mut iter = path.into_iter();
        let Some((base_node, _)) = iter.next() else {
            return LirNodeType::Literal(LirLiteral::Null);
        };
        let mut current = match base_node.node_type {
            MiddleNodeType::Identifier(name) => {
                LirNodeType::Ref(Box::new(LirNodeType::Load(name.text.into_boxed_str())))
            }
            _ => self.lower_node(base_node),
        };

        for (step, is_dynamic) in iter {
            if is_dynamic {
                let idx = self.lower_node(step);
                current = LirNodeType::Index(Box::new(current), Box::new(idx));
            } else {
                current = LirNodeType::Member(Box::new(current), Self::member_field(step));
            }
        }

        current
    }

    fn add_instr(&mut self, instr: LirInstr) {
        let idx = self.current_block.0 as usize;
        self.blocks[idx].instructions.push(instr);
    }

    fn set_terminator(&mut self, term: LirTerminator) {
        let idx = self.current_block.0 as usize;
        if self.blocks[idx].terminator.is_none() {
            self.blocks[idx].terminator = Some(term);
        }
    }

    fn create_block(&mut self) -> BlockId {
        let id = BlockId(self.blocks.len() as u32);
        self.blocks.push(LirBlock {
            id,
            instructions: vec![],
            terminator: None,
        });
        id
    }

    fn switch_to(&mut self, id: BlockId) {
        self.current_block = id;
    }

    pub fn lower_and_add_node(&mut self, node: MiddleNode) {
        if matches!(node.node_type, MiddleNodeType::Return { .. }) {
            let _ = self.lower_node(node);
            return;
        }
        let span = node.span;
        let value = self.lower_node(node);
        if matches!(
            value,
            LirNodeType::Noop | LirNodeType::Literal(LirLiteral::Null)
        ) {
            return;
        }
        self.add_instr(LirInstr::new(span, value));
    }

    pub fn lower_node(&mut self, node: MiddleNode) -> LirNodeType {
        let span = node.span;
        match node.node_type {
            MiddleNodeType::IntLiteral { value, signed } => {
                if signed {
                    LirNodeType::Literal(LirLiteral::Int(value))
                } else {
                    LirNodeType::Literal(LirLiteral::UInt(value as u64))
                }
            }
            MiddleNodeType::FloatLiteral(f) => LirNodeType::Literal(LirLiteral::Float(f)),
            MiddleNodeType::CharLiteral(c) => LirNodeType::Literal(LirLiteral::Char(c)),
            MiddleNodeType::Null => LirNodeType::Literal(LirLiteral::Null),
            MiddleNodeType::StringLiteral(s) => {
                LirNodeType::Literal(LirLiteral::String(s.to_string()))
            }
            MiddleNodeType::ListLiteral(data_type, elements) => {
                let mut lowered_elements = Vec::with_capacity(elements.len());
                for element in elements {
                    lowered_elements.push(self.lower_node(element));
                }

                LirNodeType::List {
                    elements: lowered_elements,
                    data_type,
                }
            }
            MiddleNodeType::AggregateExpression { identifier, value } => {
                let mut lowered_fields = Vec::with_capacity(value.0.len());
                for (field_name, field_node) in value.0 {
                    lowered_fields.push((field_name.to_string(), self.lower_node(field_node)));
                }

                let name = identifier.map(|i| i.to_string());
                LirNodeType::Aggregate {
                    name,
                    fields: ObjectMap(lowered_fields),
                }
            }
            MiddleNodeType::EmptyLine => LirNodeType::Noop,
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
                let is_fn_literal =
                    matches!(value.node_type, MiddleNodeType::FunctionDeclaration { .. });
                if is_fn_literal {
                    self.last_ident = Some(identifier.to_string());
                } else {
                    self.last_ident = None;
                }
                let val = self.lower_node(*value);
                self.add_instr(LirInstr::new(
                    identifier.span,
                    LirNodeType::Declare {
                        dest: identifier.to_string().into_boxed_str(),
                        value: Box::new(val),
                    },
                ));
                LirNodeType::Literal(LirLiteral::Null)
            }

            MiddleNodeType::AssignmentExpression { identifier, value } => {
                let rhs = self.lower_node(*value);
                let ident_span = identifier.span;
                let lhs = self.lower_lvalue(*identifier);
                self.add_instr(LirInstr::new(
                    ident_span,
                    LirNodeType::Assign {
                        dest: lhs,
                        value: Box::new(rhs),
                    },
                ));
                LirNodeType::Literal(LirLiteral::Null)
            }
            MiddleNodeType::FunctionDeclaration {
                parameters,
                body,
                return_type,
                ..
            } => {
                let mut captures = Vec::new();
                let param_names: rustc_hash::FxHashSet<String> = parameters
                    .iter()
                    .map(|(name, _)| name.text.clone())
                    .collect();

                for cap in body.captured() {
                    if param_names.contains(cap) {
                        continue;
                    }
                    let cap_type = self
                        .env
                        .variables
                        .get(cap)
                        .map(|v| v.data_type.clone())
                        .unwrap_or_else(|| {
                            ParserDataType::new(Span::default(), ParserInnerType::Dynamic).into()
                        });

                    captures.push((cap.clone(), cap_type));
                }

                let internal_name = if let Some(x) = self.last_ident.take() {
                    if x.contains("__curry_capture_") {
                        self.get_temp()
                    } else {
                        x
                    }
                } else {
                    self.get_temp()
                };
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
                let mut body_val;
                let mut has_body_value;
                if let MiddleNodeType::Conditional { .. } = &body.node_type {
                    let ret = MiddleNode::new(
                        MiddleNodeType::Return {
                            value: Some(body.clone()),
                        },
                        body_span,
                    );
                    let _ = sub_lowerer.lower_node(ret);
                    body_val = LirNodeType::Literal(LirLiteral::Null);
                    has_body_value = false;
                } else {
                    body_val = sub_lowerer.lower_node(*body);
                    has_body_value = !matches!(body_val, LirNodeType::Literal(LirLiteral::Null));
                }
                if is_temp_body {
                    has_body_value = false;
                    body_val = LirNodeType::Literal(LirLiteral::Null);
                }

                if !has_body_value {
                    if let Some(expr) = fallback_expr {
                        let simple_fallback = matches!(
                            expr.node_type,
                            MiddleNodeType::Identifier(_)
                                | MiddleNodeType::IntLiteral { .. }
                                | MiddleNodeType::FloatLiteral(_)
                                | MiddleNodeType::StringLiteral(_)
                                | MiddleNodeType::CharLiteral(_)
                                | MiddleNodeType::Null
                                | MiddleNodeType::MemberExpression { .. }
                                | MiddleNodeType::AggregateExpression { .. }
                                | MiddleNodeType::ListLiteral(_, _)
                                | MiddleNodeType::RangeDeclaration { .. }
                        );
                        if simple_fallback {
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
                    sub_lowerer.set_terminator(LirTerminator::Return {
                        span: body_span,
                        value: Some(body_val),
                    });
                }

                self.registry.append(sub_lowerer.registry);

                let mut capture_names = Vec::with_capacity(captures.len());
                let mut captures_for_func = Vec::with_capacity(captures.len());
                for (n, t) in captures.into_iter() {
                    capture_names.push(n.clone().into_boxed_str());
                    captures_for_func.push((n.into_boxed_str(), t));
                }

                let lir_func = LirFunction {
                    name: internal_name.clone().into_boxed_str(),
                    params: parameters
                        .into_iter()
                        .map(|x| (x.0.text.into_boxed_str(), x.1))
                        .collect::<Vec<_>>()
                        .into_boxed_slice(),
                    captures: captures_for_func.into_boxed_slice(),
                    return_type,
                    blocks: sub_lowerer.blocks.into_boxed_slice(),
                };

                self.registry
                    .functions
                    .insert(internal_name.clone(), lir_func);

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
            } => {
                let variant =
                    self.resolve_enum_variant(&identifier.to_string(), &value.to_string());
                let payload = data.map(|d| Box::new(self.lower_node(*d)));
                LirNodeType::Enum {
                    name: identifier.text.into_boxed_str(),
                    variant,
                    payload,
                }
            }
            MiddleNodeType::ScopeDeclaration {
                body,
                is_temp: false,
                ..
            } => {
                if !self.allow_global_hoist {
                    for stmt in body {
                        self.lower_and_add_node(stmt);
                    }
                    return LirNodeType::Literal(LirLiteral::Null);
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

                        let _body_val = sub_lowerer.lower_node(stmt);

                        self.registry.append(sub_lowerer.registry);

                        self.registry.globals.insert(
                            global_name.clone(),
                            LirGlobal {
                                name: global_name.into_boxed_str(),
                                data_type: global_type,
                                blocks: sub_lowerer.blocks.into_boxed_slice(),
                            },
                        );
                        continue;
                    }
                    self.lower_and_add_node(stmt);
                }

                LirNodeType::Literal(LirLiteral::Null)
            }
            MiddleNodeType::ScopeDeclaration {
                mut body, is_temp, ..
            } => {
                let last = body.pop();
                for stmt in body {
                    self.lower_and_add_node(stmt);
                }
                let Some(last) = last else {
                    return LirNodeType::Literal(LirLiteral::Null);
                };
                if is_temp {
                    let temp = self.get_temp();
                    let lowered = self.lower_node(last);
                    if matches!(lowered, LirNodeType::Literal(LirLiteral::Null)) {
                        return LirNodeType::Literal(LirLiteral::Null);
                    }
                    self.add_instr(LirInstr::new(
                        span,
                        LirNodeType::Declare {
                            dest: temp.clone().into_boxed_str(),
                            value: Box::new(lowered),
                        },
                    ));
                    LirNodeType::Load(temp.into_boxed_str())
                } else {
                    self.lower_and_add_node(last);
                    LirNodeType::Literal(LirLiteral::Null)
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
                self.add_instr(LirInstr::new(
                    span,
                    LirNodeType::Declare {
                        dest: temp.clone().into_boxed_str(),
                        value: Box::new(LirNodeType::Literal(LirLiteral::Null)),
                    },
                ));

                let cond = self.lower_node(*comparison);
                self.set_terminator(LirTerminator::Branch {
                    span,
                    condition: cond,
                    then_block: then_id,
                    else_block: else_id,
                });

                self.switch_to(then_id);
                let then_val = self.lower_node(*then);
                if self.blocks[self.current_block.0 as usize]
                    .terminator
                    .is_none()
                {
                    if !matches!(then_val, LirNodeType::Literal(LirLiteral::Null)) {
                        self.add_instr(LirInstr::new(
                            span,
                            LirNodeType::Assign {
                                dest: LirLValue::Var(temp.clone().into_boxed_str()),
                                value: Box::new(then_val),
                            },
                        ));
                    }
                    self.set_terminator(LirTerminator::Jump {
                        span,
                        target: merge_id,
                    });
                }

                self.switch_to(else_id);
                let else_val = if let Some(alt) = otherwise {
                    self.lower_node(*alt)
                } else {
                    LirNodeType::Literal(LirLiteral::Null)
                };
                if self.blocks[self.current_block.0 as usize]
                    .terminator
                    .is_none()
                {
                    if !matches!(else_val, LirNodeType::Literal(LirLiteral::Null)) {
                        self.add_instr(LirInstr::new(
                            span,
                            LirNodeType::Assign {
                                dest: LirLValue::Var(temp.clone().into_boxed_str()),
                                value: Box::new(else_val),
                            },
                        ));
                    }
                    self.set_terminator(LirTerminator::Jump {
                        span,
                        target: merge_id,
                    });
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
                LirNodeType::Literal(LirLiteral::Null)
            }
            MiddleNodeType::Return { value } => {
                if let Some(v) = value {
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
                        return LirNodeType::Literal(LirLiteral::Null);
                    }

                    let value_span = v.span;
                    let val = self.lower_node(*v);
                    self.set_terminator(LirTerminator::Return {
                        span: value_span,
                        value: Some(val),
                    });
                    LirNodeType::Literal(LirLiteral::Null)
                } else {
                    self.set_terminator(LirTerminator::Return { span, value: None });
                    LirNodeType::Literal(LirLiteral::Null)
                }
            }
            MiddleNodeType::Break { label, .. } => {
                let target = if let Some(label) = label.as_ref() {
                    self.loop_stack
                        .iter()
                        .rev()
                        .find(|(_, _, l)| l.as_deref() == Some(label.text.as_str()))
                        .map(|(_, exit, _)| *exit)
                        .or_else(|| self.loop_stack.last().map(|(_, exit, _)| *exit))
                } else {
                    self.loop_stack.last().map(|(_, exit, _)| *exit)
                };
                if let Some(target) = target {
                    self.set_terminator(LirTerminator::Jump { span, target });
                }
                LirNodeType::Literal(LirLiteral::Null)
            }
            MiddleNodeType::Continue { label } => {
                let target = if let Some(label) = label.as_ref() {
                    self.loop_stack
                        .iter()
                        .rev()
                        .find(|(_, _, l)| l.as_deref() == Some(label.text.as_str()))
                        .map(|(header, _, _)| *header)
                        .or_else(|| self.loop_stack.last().map(|(header, _, _)| *header))
                } else {
                    self.loop_stack.last().map(|(header, _, _)| *header)
                };
                if let Some(target) = target {
                    self.set_terminator(LirTerminator::Jump { span, target });
                }
                LirNodeType::Literal(LirLiteral::Null)
            }
            MiddleNodeType::MemberExpression { path } => {
                let mut iter = path.into_iter();
                let Some((base_node, _)) = iter.next() else {
                    return LirNodeType::Literal(LirLiteral::Null);
                };
                let mut current = self.lower_node(base_node);

                for (step, is_dynamic) in iter {
                    if is_dynamic {
                        let idx = self.lower_node(step);
                        current = LirNodeType::Index(Box::new(current), Box::new(idx));
                    } else {
                        current = LirNodeType::Member(Box::new(current), Self::member_field(step));
                    }
                }
                current
            }
            MiddleNodeType::DerefStatement { value } => {
                LirNodeType::Deref(Box::new(self.lower_node(*value)))
            }
            MiddleNodeType::RefStatement { value, .. } => {
                LirNodeType::Ref(Box::new(self.lower_node(*value)))
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
            } => LirNodeType::Boolean {
                left: Box::new(self.lower_node(*left)),
                right: Box::new(self.lower_node(*right)),
                operator,
            },
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
                    && let Some(var) = self.env.variables.get(&name.text)
                    && let ParserInnerType::Function { parameters, .. } = &var.data_type.data_type
                    && let Some(first) = parameters.first()
                {
                    matches!(first.data_type, ParserInnerType::Ref(_, _))
                } else {
                    false
                };
                let l_caller = self.lower_node(caller_node);
                let mut l_args: Vec<LirNodeType> = Vec::with_capacity(args.len());
                for a in args {
                    l_args.push(self.lower_node(a));
                }

                if needs_ref_first_arg
                    && let Some(first_arg) = l_args.get_mut(0)
                    && matches!(first_arg, LirNodeType::Load(_))
                {
                    *first_arg = LirNodeType::Ref(Box::new(std::mem::replace(
                        first_arg,
                        LirNodeType::Literal(LirLiteral::Null),
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

    fn resolve_enum_variant(&self, enum_name: &str, variant_name: &str) -> u32 {
        if let Some(obj) = self.env.objects.get(enum_name) {
            if let MiddleTypeDefType::Enum(variants) = &obj.object_type {
                return variants
                    .iter()
                    .position(|(name, _)| name.text == variant_name)
                    .unwrap_or(0) as u32;
            }
        }
        0
    }
}

use std::{collections::HashMap, fmt::Display};

use calibre_mir::environment::{MiddleEnvironment, MiddleTypeDefType};
use calibre_mir_ty::{MiddleNode, MiddleNodeType};
use calibre_parser::ast::{
    ObjectMap, ParserDataType, ParserInnerType, ParserText,
    binary::BinaryOperator,
    comparison::{BooleanOperator, ComparisonOperator},
};

#[derive(Debug, Clone, Default)]
pub struct LirRegistry {
    pub functions: HashMap<String, LirFunction>,
    pub globals: HashMap<String, LirGlobal>,
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
        let mut txt = String::new();

        for val in &self.globals {
            txt.push_str(&format!("{}\n", val.1));
        }

        for func in &self.functions {
            txt.push_str(&format!("{}\n\n", func.1));
        }

        write!(f, "{}", txt)
    }
}

#[derive(Debug, Clone)]
pub struct LirGlobal {
    pub name: String,
    pub data_type: ParserDataType,
    pub initial_value: LirNodeType,
}

impl Display for LirGlobal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "const {} : {} = {};",
            self.name, self.data_type, self.initial_value
        )
    }
}

#[derive(Debug, Clone)]
pub struct LirFunction {
    pub name: String,
    pub params: Vec<(String, ParserDataType)>,
    pub captures: Vec<(String, ParserDataType)>,
    pub return_type: ParserDataType,
    pub blocks: Vec<LirBlock>,
    pub is_async: bool,
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

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct BlockId(pub u32);

#[derive(Debug, Clone, PartialEq)]
pub enum LirLiteral {
    Int(i64),
    Float(f64),
    Char(char),
    String(String),
    Null,
}

impl Display for LirLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int(x) => write!(f, "{x}"),
            Self::Float(x) => write!(f, "{x}f"),
            Self::Char(x) => write!(f, "'{x}'"),
            Self::String(x) => write!(f, "{x:?}"),
            Self::Null => write!(f, "null"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum LirNodeType {
    Closure {
        label: String,
        captures: Vec<String>,
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
    Load(String),
    Boolean {
        left: Box<LirNodeType>,
        right: Box<LirNodeType>,
        operator: BooleanOperator,
    },
    Move(String),
    Drop(String),
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
    Member(Box<LirNodeType>, String),
    Enum {
        name: String,
        variant: u32,
        payload: Option<Box<LirNodeType>>,
    },
    As(Box<LirNodeType>, ParserDataType),
    Assign {
        dest: LirLValue,
        value: Box<LirNodeType>,
    },
    Declare {
        dest: String,
        value: Box<LirNodeType>,
    },
}

impl Display for LirNodeType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
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
                Self::Aggregate { name, fields } => {
                    let mut txt = if let Some(name) = name {
                        name.to_string()
                    } else {
                        String::new()
                    };

                    txt
                }
                Self::Literal(x) => x.to_string(),
                Self::As(node, data_type) => format!("{} as {}", node, data_type),
                Self::Declare { dest, value } => format!("let {} = {}", dest, value),
                Self::Assign { dest, value } => format!("{} = {}", dest, value),
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

#[derive(Debug, Clone, PartialEq)]
pub enum LirLValue {
    Var(String),
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

#[derive(Debug, Clone)]
pub enum LirTerminator {
    Jump(BlockId),
    Branch {
        condition: LirNodeType,
        then_block: BlockId,
        else_block: BlockId,
    },
    Return(Option<LirNodeType>),
}

impl Display for LirTerminator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Jump(x) => write!(f, "jmp blk {}", x.0),
            Self::Branch {
                condition,
                then_block,
                else_block,
            } => write!(
                f,
                "{} ? jmp blk {} : jmp blk {}",
                condition, then_block.0, else_block.0
            ),
            Self::Return(x) => write!(
                f,
                "return{}",
                if let Some(x) = x.as_ref() {
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
    pub instructions: Vec<LirNodeType>,
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
    pub temp_count: usize,
    pub loop_stack: Vec<(BlockId, BlockId)>,
}

impl<'a> LirEnvironment<'a> {
    pub fn lower(env: &'a MiddleEnvironment, node: MiddleNode) -> LirRegistry {
        let mut this = Self::new(env);
        let node = this.lower_node(node);
        this.registry
    }

    pub fn new(env: &'a MiddleEnvironment) -> Self {
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
            temp_count: 0,
            loop_stack: vec![],
        }
    }

    fn get_temp(&mut self) -> String {
        let name = format!("tmp_{}", self.temp_count);
        self.temp_count += 1;
        name
    }

    fn add_instr(&mut self, instr: LirNodeType) {
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
        let value = self.lower_node(node);
        self.add_instr(value);
    }

    pub fn lower_node(&mut self, node: MiddleNode) -> LirNodeType {
        match node.node_type {
            MiddleNodeType::IntLiteral(i) => LirNodeType::Literal(LirLiteral::Int(i)),
            MiddleNodeType::FloatLiteral(f) => LirNodeType::Literal(LirLiteral::Float(f)),
            MiddleNodeType::CharLiteral(c) => LirNodeType::Literal(LirLiteral::Char(c)),
            MiddleNodeType::StringLiteral(s) => {
                LirNodeType::Literal(LirLiteral::String(s.to_string()))
            }
            MiddleNodeType::ListLiteral(data_type, elements) => {
                let lowered_elements = elements.into_iter().map(|e| self.lower_node(e)).collect();

                LirNodeType::List {
                    elements: lowered_elements,
                    data_type,
                }
            }
            MiddleNodeType::AggregateExpression { identifier, value } => {
                let mut lowered_fields = Vec::new();
                for (field_name, field_node) in value.0 {
                    lowered_fields.push((field_name.to_string(), self.lower_node(field_node)));
                }

                LirNodeType::Aggregate {
                    name: identifier.map(|i| i.to_string()),
                    fields: ObjectMap(lowered_fields),
                }
            }
            MiddleNodeType::EmptyLine => LirNodeType::Literal(LirLiteral::Null),
            MiddleNodeType::Drop(name) => LirNodeType::Drop(name.to_string()),
            MiddleNodeType::Move(name) => LirNodeType::Move(name.to_string()),
            MiddleNodeType::Identifier(name) => LirNodeType::Load(name.to_string()),
            MiddleNodeType::VariableDeclaration {
                identifier, value, ..
            } => {
                self.last_ident = Some(identifier.to_string());
                let val = self.lower_node(*value);
                self.add_instr(LirNodeType::Declare {
                    dest: identifier.to_string(),
                    value: Box::new(val),
                });
                LirNodeType::Literal(LirLiteral::Null)
            }

            MiddleNodeType::AssignmentExpression { identifier, value } => {
                let rhs = self.lower_node(*value);
                let lhs = self.lower_lvalue(*identifier);
                self.add_instr(LirNodeType::Assign {
                    dest: lhs,
                    value: Box::new(rhs),
                });
                LirNodeType::Literal(LirLiteral::Null)
            }
            MiddleNodeType::FunctionDeclaration {
                parameters,
                body,
                return_type,
                is_async,
            } => {
                let mut captures = Vec::new();

                for cap in body.captured() {
                    let cap_type = self
                        .env
                        .variables
                        .get(cap)
                        .map(|v| v.data_type.clone())
                        .unwrap_or_else(|| ParserDataType::from(ParserInnerType::Dynamic).into());

                    captures.push((cap.clone(), cap_type));
                }

                let internal_name = if let Some(x) = &self.last_ident {
                    x.clone()
                } else {
                    self.get_temp()
                };
                let mut sub_lowerer = LirEnvironment::new(self.env);

                let body_val = sub_lowerer.lower_node(*body);

                if sub_lowerer.blocks.last().unwrap().terminator.is_none() {
                    sub_lowerer.set_terminator(LirTerminator::Return(Some(body_val)));
                }

                self.registry.append(sub_lowerer.registry);

                let capture_names = captures.iter().map(|x| x.0.clone()).collect();

                let lir_func = LirFunction {
                    name: internal_name.clone(),
                    params: parameters.into_iter().map(|x| (x.0.text, x.1)).collect(),
                    captures,
                    return_type,
                    blocks: sub_lowerer.blocks,
                    is_async,
                };

                self.registry
                    .functions
                    .insert(internal_name.clone(), lir_func);

                LirNodeType::Closure {
                    label: internal_name,
                    captures: capture_names,
                }
            }

            MiddleNodeType::EnumExpression {
                identifier,
                value,
                data,
            } => {
                let variant =
                    self.resolve_enum_variant(&identifier.to_string(), &value.to_string());
                let payload = data.map(|d| Box::new(self.lower_node(*d)));
                LirNodeType::Enum {
                    name: identifier.text,
                    variant,
                    payload,
                }
            }
            MiddleNodeType::ScopeDeclaration {
                body,
                is_temp: false,
                ..
            } => {
                for stmt in body {
                    if let MiddleNodeType::VariableDeclaration {
                        var_type,
                        identifier,
                        value,
                        data_type,
                    } = stmt.node_type.clone()
                    {
                        match value.node_type {
                            MiddleNodeType::FunctionDeclaration {
                                parameters,
                                body,
                                return_type,
                                is_async,
                            } => {}
                            _ => {
                                let val = self.lower_node(*value);
                                self.registry.globals.insert(
                                    identifier.to_string(),
                                    LirGlobal {
                                        name: identifier.to_string(),
                                        data_type: data_type,
                                        initial_value: val,
                                    },
                                );
                                continue;
                            }
                        }
                    }
                    let val = self.lower_node(stmt);
                    self.add_instr(val);
                }

                LirNodeType::Literal(LirLiteral::Null)
            }
            MiddleNodeType::ScopeDeclaration { mut body, .. } => {
                let mut last = body.pop();
                for stmt in body {
                    self.lower_and_add_node(stmt);
                }
                last.map(|x| self.lower_node(x))
                    .unwrap_or(LirNodeType::Literal(LirLiteral::Null))
            }
            MiddleNodeType::IfStatement {
                comparison,
                then,
                otherwise,
            } => {
                let then_id = self.create_block();
                let else_id = self.create_block();
                let merge_id = self.create_block();

                let cond = self.lower_node(*comparison);
                self.set_terminator(LirTerminator::Branch {
                    condition: cond,
                    then_block: then_id,
                    else_block: else_id,
                });

                self.switch_to(then_id);
                self.lower_and_add_node(*then);
                self.set_terminator(LirTerminator::Jump(merge_id));

                self.switch_to(else_id);
                if let Some(alt) = otherwise {
                    self.lower_and_add_node(*alt);
                }
                self.set_terminator(LirTerminator::Jump(merge_id));

                self.switch_to(merge_id);
                LirNodeType::Literal(LirLiteral::Null)
            }
            MiddleNodeType::LoopDeclaration { state, body } => {
                let header_id = self.create_block();
                let body_id = self.create_block();
                let exit_id = self.create_block();

                if let Some(s) = state {
                    self.lower_and_add_node(*s);
                }
                self.set_terminator(LirTerminator::Jump(header_id));

                self.switch_to(header_id);
                self.set_terminator(LirTerminator::Jump(body_id));

                self.loop_stack.push((header_id, exit_id));
                self.switch_to(body_id);
                self.lower_and_add_node(*body);
                self.set_terminator(LirTerminator::Jump(header_id));
                self.loop_stack.pop();

                self.switch_to(exit_id);
                LirNodeType::Literal(LirLiteral::Null)
            }
            MiddleNodeType::Return { value } => {
                let val = value.map(|v| self.lower_node(*v));
                self.set_terminator(LirTerminator::Return(val));
                LirNodeType::Literal(LirLiteral::Null)
            }
            MiddleNodeType::Break => {
                let (_, exit) = *self.loop_stack.last().expect("Break outside loop");
                self.set_terminator(LirTerminator::Jump(exit));
                LirNodeType::Literal(LirLiteral::Null)
            }
            MiddleNodeType::Continue => {
                let (header, _) = *self.loop_stack.last().expect("Continue outside loop");
                self.set_terminator(LirTerminator::Jump(header));
                LirNodeType::Literal(LirLiteral::Null)
            }
            MiddleNodeType::MemberExpression { path } => {
                let (base_node, _) = &path[0];
                let mut current = self.lower_node(base_node.clone());

                for (step, is_dynamic) in path.iter().skip(1) {
                    if *is_dynamic {
                        let idx = self.lower_node(step.clone());
                        current = LirNodeType::Index(Box::new(current), Box::new(idx));
                    } else {
                        let field = match &step.node_type {
                            MiddleNodeType::Identifier(name) => name.to_string(),
                            MiddleNodeType::IntLiteral(x) => x.to_string(),
                            MiddleNodeType::FloatLiteral(x) => x.to_string(),
                            x => panic!("Invalid field identifier : {}", x),
                        };
                        current = LirNodeType::Member(Box::new(current), field);
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
                let l_caller = self.lower_node(*caller);
                let mut l_args: Vec<LirNodeType> =
                    args.into_iter().map(|a| self.lower_node(a)).collect();

                LirNodeType::Call {
                    caller: Box::new(l_caller),
                    args: l_args,
                }
            }
            MiddleNodeType::AsExpression { value, data_type } => {
                LirNodeType::As(Box::new(self.lower_node(*value)), data_type)
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
            MiddleNodeType::Identifier(name) => LirLValue::Var(name.to_string()),
            MiddleNodeType::DerefStatement { value } => {
                LirLValue::Ptr(Box::new(self.lower_node(*value)))
            }
            MiddleNodeType::MemberExpression { .. } => {
                if let LirNodeType::Deref(addr) = self.lower_node(node) {
                    LirLValue::Ptr(addr)
                } else {
                    panic!("Member expression must resolve to a pointer address")
                }
            }
            _ => panic!("Invalid L-Value: Assignment target must be a variable or memory location"),
        }
    }

    fn resolve_enum_variant(&self, enum_name: &str, variant_name: &str) -> u32 {
        if let Some(obj) = self.env.objects.get(enum_name) {
            if let MiddleTypeDefType::Enum(variants) = &obj.object_type {
                return variants
                    .iter()
                    .position(|(name, _)| name.to_string() == variant_name)
                    .unwrap_or(0) as u32;
            }
        }
        0
    }
}

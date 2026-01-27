use calibre_lir::{
    BlockId, LirBlock, LirFunction, LirGlobal, LirLValue, LirLiteral, LirNodeType, LirRegistry,
    LirTerminator,
};
use calibre_mir_ty::MiddleNode;
use calibre_parser::ast::{
    ParserDataType, ParserInnerType,
    binary::BinaryOperator,
    comparison::{BooleanOperator, ComparisonOperator},
};
use std::collections::HashMap;

#[derive(Debug, Clone, Default)]
pub struct VMRegistry {
    pub functions: HashMap<String, VMFunction>,
    pub globals: HashMap<String, VMGlobal>,
}

impl From<LirRegistry> for VMRegistry {
    fn from(value: LirRegistry) -> Self {
        let mut functions = HashMap::new();

        for (k, func) in value.functions {
            functions.insert(k, func.into());
        }

        let mut globals = HashMap::new();

        for (k, v) in value.globals {
            globals.insert(k, v.into());
        }

        Self { functions, globals }
    }
}

#[derive(Debug, Clone)]
pub struct VMGlobal {
    pub name: String,
    pub initial_value: LirNodeType,
}

impl From<LirGlobal> for VMGlobal {
    fn from(value: LirGlobal) -> Self {
        Self {
            name: value.name,
            initial_value: value.initial_value,
        }
    }
}

#[derive(Debug, Clone)]
pub struct VMFunction {
    pub name: String,
    pub params: Vec<String>,
    pub captures: Vec<String>,
    pub returns_value: bool,
    pub blocks: Vec<VMBlock>,
    pub is_async: bool,
}

impl From<LirFunction> for VMFunction {
    fn from(value: LirFunction) -> Self {
        Self {
            name: value.name,
            params: value.params.into_iter().map(|x| x.0).collect(),
            captures: value.captures,
            returns_value: value.return_type.data_type != ParserInnerType::Null,
            blocks: value.blocks.into_iter().map(|x| x.into()).collect(),
            is_async: value.is_async,
        }
    }
}

#[derive(Debug, Clone)]
pub struct VMBlock {
    pub id: BlockId,
    pub instructions: Vec<VMInstruction>,
    pub local_literals: Vec<VMLiteral>,
    pub local_strings: Vec<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum VMLiteral {
    Int(i64),
    Float(f64),
    Char(char),
    String(String),
    Closure {
        label: String,
        captures: Vec<String>,
    },
    Null,
}

impl From<LirLiteral> for VMLiteral {
    fn from(value: LirLiteral) -> Self {
        match value {
            LirLiteral::Int(x) => Self::Int(x),
            LirLiteral::Float(x) => Self::Float(x),
            LirLiteral::Char(x) => Self::Char(x),
            LirLiteral::String(x) => Self::String(x),
            LirLiteral::Null => Self::Null,
        }
    }
}

pub type Count = u8;
pub type StringIndex = u8;

#[derive(Debug, Clone, PartialEq)]
pub enum VMInstruction {
    Jump(BlockId),
    Branch(BlockId, BlockId),
    Return(bool),
    LoadLiteral(u8),
    LoadVar(StringIndex),
    LoadVarRef(StringIndex),
    Deref,
    SetVar(StringIndex),
    Binary(BinaryOperator),
    Comparison(ComparisonOperator),
    Boolean(BooleanOperator),
    List(Count),
    Call(Count),
    Aggregate {
        name: Option<StringIndex>,
        count: Count,
    },
    Range(bool),
    Index,
    LoadMember(StringIndex),
    SetMember(StringIndex),
    Enum {
        name: StringIndex,
        variant: u8,
        has_payload: bool,
    },
    As(ParserDataType<MiddleNode>),
}

impl From<LirBlock> for VMBlock {
    fn from(value: LirBlock) -> Self {
        let mut block = Self {
            id: value.id,
            instructions: Vec::with_capacity(value.instructions.len()),
            local_literals: Vec::new(),
            local_strings: Vec::new(),
        };

        for node in value.instructions {
            block.translate(node);
        }

        if let Some(terminator) = value.terminator {
            block.translate_terminator(terminator);
        }

        block
    }
}

impl VMBlock {
    pub fn add_string(&mut self, text: String) -> StringIndex {
        self.local_strings.push(text);
        self.local_literals.len() as u8
    }

    pub fn translate(&mut self, node: LirNodeType) {
        match node {
            LirNodeType::Literal(x) => {
                self.local_literals.push(x.into());
                self.instructions
                    .push(VMInstruction::LoadLiteral(self.local_literals.len() as u8));
            }
            LirNodeType::Call { caller, args } => {
                let count = args.len();
                for arg in args {
                    self.translate(arg);
                }
                self.translate(*caller);
                self.instructions.push(VMInstruction::Call(count as u8));
            }
            LirNodeType::List {
                elements,
                data_type,
            } => {
                let count = elements.len();
                for item in elements {
                    self.translate(item);
                }
                self.instructions.push(VMInstruction::List(count as u8));
            }
            LirNodeType::Load(x) => {
                let name = self.add_string(x);
                self.instructions.push(VMInstruction::LoadVar(name));
            }
            LirNodeType::Aggregate { name, fields } => {
                let name = if let Some(name) = name {
                    Some(self.add_string(name))
                } else {
                    None
                };

                let count = fields.0.len();
                for (_, item) in fields.0 {
                    self.translate(item);
                }
                self.instructions.push(VMInstruction::Aggregate {
                    name,
                    count: count as u8,
                });
            }
            LirNodeType::Boolean {
                left,
                right,
                operator,
            } => {
                self.translate(*left);
                self.translate(*right);
                self.instructions.push(VMInstruction::Boolean(operator));
            }
            LirNodeType::Comparison {
                left,
                right,
                operator,
            } => {
                self.translate(*left);
                self.translate(*right);
                self.instructions.push(VMInstruction::Comparison(operator));
            }
            LirNodeType::Binary {
                left,
                right,
                operator,
            } => {
                self.translate(*left);
                self.translate(*right);
                self.instructions.push(VMInstruction::Binary(operator));
            }
            LirNodeType::Range {
                from,
                to,
                inclusive,
            } => {
                self.translate(*from);
                self.translate(*to);
                self.instructions.push(VMInstruction::Range(inclusive));
            }
            LirNodeType::Deref(x) => {
                self.translate(*x);
                self.instructions.push(VMInstruction::Deref);
            }
            LirNodeType::Ref(x) => {
                todo!()
            }
            LirNodeType::As(value, data_type) => {
                self.translate(*value);
                self.instructions.push(VMInstruction::As(data_type));
            }
            LirNodeType::Enum {
                name,
                variant,
                payload,
            } => {
                let name = self.add_string(name);
                let has_payload = payload.is_some();

                if let Some(payload) = payload {
                    self.translate(*payload);
                }

                self.instructions.push(VMInstruction::Enum {
                    name,
                    variant: variant as u8,
                    has_payload,
                });
            }
            LirNodeType::Assign {
                dest: LirLValue::Var(dest),
                value,
            }
            | LirNodeType::Declare { dest, value } => {
                self.translate(*value);
                let dest = self.add_string(dest);
                self.instructions.push(VMInstruction::SetVar(dest));
            }
            LirNodeType::Assign { .. } => todo!(),
            LirNodeType::Index(value, index) => {
                self.translate(*value);
                self.translate(*index);
                self.instructions.push(VMInstruction::Index);
            }
            LirNodeType::Member(value, member) => {
                self.translate(*value);
                let member = self.add_string(member);
                self.instructions.push(VMInstruction::LoadMember(member));
            }
            LirNodeType::Closure { label, captures } => {
                self.local_literals
                    .push(VMLiteral::Closure { label, captures });
                self.instructions
                    .push(VMInstruction::LoadLiteral(self.local_literals.len() as u8));
            }
        }
    }

    pub fn translate_terminator(&mut self, node: LirTerminator) {
        match node {
            LirTerminator::Jump(x) => self.instructions.push(VMInstruction::Jump(x)),
            LirTerminator::Branch {
                condition,
                then_block,
                else_block,
            } => {
                self.translate(condition);
                self.instructions
                    .push(VMInstruction::Branch(then_block, else_block));
            }
            LirTerminator::Return(x) => {
                let has_value = x.is_some();
                if let Some(v) = x {
                    self.translate(v);
                }
                self.instructions.push(VMInstruction::Return(has_value));
            }
        }
    }
}

use std::collections::HashMap;

use calibre_parser::{
    ast::{
        ObjectMap, ParserDataType, ParserText, RefMutability, VarType,
        binary::BinaryOperator,
        comparison::{BooleanOperation, Comparison},
    },
    lexer::Span,
};

#[derive(Debug, Clone, PartialEq)]
pub struct MiddleNode {
    pub node_type: MiddleNodeType,
    pub span: Span,
}

impl MiddleNode {
    pub fn new(node_type: MiddleNodeType, span: Span) -> Self {
        Self { node_type, span }
    }

    pub fn new_from_type(node_type: MiddleNodeType) -> Self {
        Self {
            node_type,
            span: Span::default(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum MiddleNodeType {
    Break,
    Continue,
    EmptyLine,
    RefStatement {
        mutability: RefMutability,
        value: Box<MiddleNode>,
    },
    DerefStatement {
        value: Box<MiddleNode>,
    },
    VariableDeclaration {
        var_type: VarType,
        identifier: ParserText,
        value: Box<MiddleNode>,
        data_type: ParserDataType,
    },
    EnumExpression {
        identifier: ParserText,
        value: ParserText,
        data: Option<ObjectMap<MiddleNode>>,
    },
    ScopeDeclaration {
        body: Vec<MiddleNode>,
        is_temp: bool,
    },
    MatchDeclaration {
        parameters: (ParserText, ParserDataType, Option<Box<MiddleNode>>),
        body: Vec<(MiddleNode, Vec<MiddleNode>, Box<MiddleNode>)>,
        return_type: ParserDataType,
        is_async: bool,
    },
    FunctionDeclaration {
        parameters: Vec<(ParserText, ParserDataType, Option<MiddleNode>)>,
        body: Box<MiddleNode>,
        return_type: ParserDataType,
        is_async: bool,
    },
    AssignmentExpression {
        identifier: Box<MiddleNode>,
        value: Box<MiddleNode>,
    },
    NotExpression {
        value: Box<MiddleNode>,
    },
    DebugExpression {
        pretty_printed_str: String,
        value: Box<MiddleNode>,
    },
    NegExpression {
        value: Box<MiddleNode>,
    },
    AsExpression {
        value: Box<MiddleNode>,
        typ: ParserDataType,
    },
    InDeclaration {
        identifier: Box<MiddleNode>,
        expression: Box<MiddleNode>,
    },
    IsDeclaration {
        value: Box<MiddleNode>,
        data_type: ParserDataType,
    },
    RangeDeclaration {
        from: Box<MiddleNode>,
        to: Box<MiddleNode>,
        inclusive: bool,
    },
    LoopDeclaration {
        state: Option<Box<MiddleNode>>,
        body: Box<MiddleNode>,
    },
    Return {
        value: Box<MiddleNode>,
    },
    Identifier(ParserText),
    StringLiteral(ParserText),
    ListLiteral(Vec<MiddleNode>),
    CharLiteral(char),
    FloatLiteral(f64),
    IntLiteral(i64),
    MemberExpression {
        path: Vec<(MiddleNode, bool)>,
    },
    CallExpression(Box<MiddleNode>, Vec<(MiddleNode, Option<MiddleNode>)>),
    BinaryExpression {
        left: Box<MiddleNode>,
        right: Box<MiddleNode>,
        operator: BinaryOperator,
    },
    ComparisonExpression {
        left: Box<MiddleNode>,
        right: Box<MiddleNode>,
        operator: Comparison,
    },
    BooleanExpression {
        left: Box<MiddleNode>,
        right: Box<MiddleNode>,
        operator: BooleanOperation,
    },
    AggregateExpression {
        identifier: Option<ParserText>,
        value: ObjectMap<MiddleNode>,
    },
    IfStatement {
        comparison: Box<MiddleNode>,
        then: Box<MiddleNode>,
        otherwise: Option<Box<MiddleNode>>,
    },
}

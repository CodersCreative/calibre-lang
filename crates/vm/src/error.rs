use crate::value::RuntimeValue;
use calibre_parser::Span;
use calibre_parser::ast::{
    ParserInnerType,
    binary::BinaryOperator,
    comparison::{BooleanOperator, ComparisonOperator},
};
use std::num::{ParseFloatError, ParseIntError};

#[derive(Debug)]
pub enum RuntimeError {
    At(Span, Box<RuntimeError>),
    Boolean(RuntimeValue, RuntimeValue, BooleanOperator),
    Comparison(RuntimeValue, RuntimeValue, ComparisonOperator),
    Binary(RuntimeValue, RuntimeValue, BinaryOperator),
    UnexpectedType(RuntimeValue),
    MissingMember {
        target: RuntimeValue,
        member: String,
    },
    ParseFloat(ParseFloatError),
    ParseInt(ParseIntError),
    CantConvert(RuntimeValue, ParserInnerType),
    StackUnderflow,
    FunctionNotFound(String),
    InvalidFunctionCall,
    Ffi(String),
    DanglingRef(String),
    InvalidBytecode(String),
    Io(String),
    Panic(Option<String>),
}

impl From<ParseFloatError> for RuntimeError {
    fn from(value: ParseFloatError) -> Self {
        Self::ParseFloat(value)
    }
}

impl From<ParseIntError> for RuntimeError {
    fn from(value: ParseIntError) -> Self {
        Self::ParseInt(value)
    }
}

impl std::fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::At(_, inner) => write!(f, "{}", inner),
            RuntimeError::Boolean(left, right, op) => {
                write!(f, "Invalid boolean operation: {left} {op} {right}")
            }
            RuntimeError::Comparison(left, right, op) => {
                write!(f, "Invalid comparison: {left} {op} {right}")
            }
            RuntimeError::Binary(left, right, op) => {
                write!(f, "Invalid binary operation: {left} {op} {right}")
            }
            RuntimeError::UnexpectedType(value) => write!(f, "Unexpected value type: {value:?}"),
            RuntimeError::MissingMember { target, member } => {
                write!(f, "Missing member \"{member}\" on {target:?}")
            }
            RuntimeError::ParseFloat(x) => write!(f, "{x}"),
            RuntimeError::ParseInt(x) => write!(f, "{x}"),
            RuntimeError::CantConvert(value, ty) => write!(f, "Cannot convert {value:?} to {ty:?}"),
            RuntimeError::StackUnderflow => write!(f, "Internal runtime error: stack underflow"),
            RuntimeError::FunctionNotFound(name) => write!(f, "Function not found: {name}"),
            RuntimeError::InvalidFunctionCall => write!(f, "Invalid function call"),
            RuntimeError::Ffi(msg) => write!(f, "FFI error: {msg}"),
            RuntimeError::DanglingRef(name) => write!(f, "Dangling reference: {name}"),
            RuntimeError::InvalidBytecode(msg) => write!(f, "Invalid bytecode: {msg}"),
            RuntimeError::Io(msg) => write!(f, "I/O error: {msg}"),
            RuntimeError::Panic(Some(msg)) => write!(f, "panic: {msg}"),
            RuntimeError::Panic(None) => write!(f, "panic"),
        }
    }
}

impl RuntimeError {
    pub fn at(span: Span, err: RuntimeError) -> RuntimeError {
        if span == Span::default() {
            err
        } else {
            RuntimeError::At(span, Box::new(err))
        }
    }

    pub fn innermost(&self) -> (Option<Span>, &RuntimeError) {
        let mut span = None;
        let mut current = self;

        while let RuntimeError::At(inner_span, inner) = current {
            if *inner_span != Span::default() {
                span = Some(*inner_span);
            }
            current = inner.as_ref();
        }

        (span, current)
    }

    pub fn help(&self) -> Option<String> {
        match self {
            RuntimeError::At(_, inner) => inner.help(),
            RuntimeError::Boolean(_, _, _) => Some(
                "Ensure both operands are booleans (true/false) when using boolean operators."
                    .to_string(),
            ),
            RuntimeError::Comparison(_, _, _) => {
                Some("Check that both sides of the comparison are compatible types.".to_string())
            }
            RuntimeError::Binary(_, _, _) => {
                Some("Check that both operands support this arithmetic operator.".to_string())
            }
            RuntimeError::UnexpectedType(_) => Some(
                "Verify the value you're using matches the expected type in this context."
                    .to_string(),
            ),
            RuntimeError::MissingMember { .. } => {
                Some("Check the field or method name is correct for this value's type.".to_string())
            }
            RuntimeError::CantConvert(_, _) => Some(
                "Use an explicit conversion or adjust the value to a compatible type.".to_string(),
            ),
            RuntimeError::StackUnderflow => Some(
                "This is likely a compiler/runtime bug. Please report this with a repro."
                    .to_string(),
            ),
            RuntimeError::FunctionNotFound(_) => Some(
                "Make sure the function is defined, imported, and spelled correctly.".to_string(),
            ),
            RuntimeError::InvalidFunctionCall => Some(
                "Check that you are calling a function value and passing the right arguments."
                    .to_string(),
            ),
            RuntimeError::ParseFloat(x) => Some(x.to_string()),
            RuntimeError::ParseInt(x) => Some(x.to_string()),
            RuntimeError::Ffi(_) => Some(
                "Verify the library path, symbol name, and FFI types match the external function."
                    .to_string(),
            ),
            RuntimeError::DanglingRef(_) => {
                Some("This value was freed or went out of scope before use.".to_string())
            }
            RuntimeError::InvalidBytecode(_) => Some(
                "This is likely a compiler/runtime bug. Please report this with a repro."
                    .to_string(),
            ),
            RuntimeError::Io(_) => Some(
                "Check file permissions, terminal availability, or input/output state.".to_string(),
            ),
            RuntimeError::Panic(_) => Some(
                "A panic was triggered. If this is unexpected, inspect the call stack.".to_string(),
            ),
        }
    }
}

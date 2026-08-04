use crate::value::RuntimeValue;
use calibre_parser::{CalibreError, Span};
use calibre_parser::ast::{
    binary::BinaryOperator,
    comparison::{BooleanOperator, ComparisonOperator},
    types::ParserInnerType,
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
    InvalidFunctionCallValue(RuntimeValue),
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
            RuntimeError::InvalidFunctionCallValue(value) => {
                write!(f, "Invalid function call: {value:?}")
            }
            RuntimeError::Ffi(msg) => write!(f, "FFI error: {msg}"),
            RuntimeError::DanglingRef(name) => write!(f, "Dangling reference: {name}"),
            RuntimeError::InvalidBytecode(msg) => write!(f, "Invalid bytecode: {msg}"),
            RuntimeError::Io(msg) => write!(f, "I/O error: {msg}"),
            RuntimeError::Panic(Some(msg)) => write!(f, "panic: {msg}"),
            RuntimeError::Panic(None) => write!(f, "panic"),
        }
    }
}

impl CalibreError for RuntimeError {
    fn code(&self) -> usize {
        match self {
            Self::At(_, inner) => inner.code(),
            Self::Boolean(_, _, _) => 401,
            Self::Comparison(_, _, _) => 402,
            Self::Binary(_, _, _) => 403,
            Self::UnexpectedType(_) => 404,
            Self::MissingMember { .. } => 405,
            Self::ParseFloat(_) => 406,
            Self::ParseInt(_) => 407,
            Self::CantConvert(_, _) => 408,
            Self::StackUnderflow => 409,
            Self::FunctionNotFound(_) => 410,
            Self::InvalidFunctionCall => 411,
            Self::InvalidFunctionCallValue(_) => 412,
            Self::Ffi(_) => 413,
            Self::DanglingRef(_) => 414,
            Self::InvalidBytecode(_) => 415,
            Self::Io(_) => 416,
            Self::Panic(_) => 417,
        }
    }

    fn hint(&self) -> Option<String> {
        match self {
            Self::At(_, inner) => inner.hint(),
            Self::Boolean(_, _, _) => Some(
                "Ensure both operands are booleans (true/false) when using boolean operators."
                    .to_string(),
            ),
            Self::Comparison(_, _, _) => {
                Some("Check that both sides of the comparison are compatible types.".to_string())
            }
            Self::Binary(_, _, _) => {
                Some("Check that both operands support this arithmetic operator.".to_string())
            }
            Self::UnexpectedType(_) => Some(
                "Verify the value you're using matches the expected type in this context."
                    .to_string(),
            ),
            Self::MissingMember { .. } => {
                Some("Check the field or method name is correct for this value's type.".to_string())
            }
            Self::CantConvert(_, _) => Some(
                "Use an explicit conversion or adjust the value to a compatible type.".to_string(),
            ),
            Self::StackUnderflow => Some(
                "This is likely a compiler/runtime bug. Please report this with a repro."
                    .to_string(),
            ),
            Self::FunctionNotFound(_) => Some(
                "Make sure the function is defined, imported, and spelled correctly.".to_string(),
            ),
            Self::InvalidFunctionCall => Some(
                "Check that you are calling a function value and passing the right arguments."
                    .to_string(),
            ),
            Self::InvalidFunctionCallValue(_) => Some(
                "Ensure the callee is a function, native function, or bound method.".to_string(),
            ),
            Self::ParseFloat(x) => Some(x.to_string()),
            Self::ParseInt(x) => Some(x.to_string()),
            Self::Ffi(_) => Some(
                "Verify the library path, symbol name, and FFI types match the external function."
                    .to_string(),
            ),
            Self::DanglingRef(_) => {
                Some("This value was freed or went out of scope before use.".to_string())
            }
            Self::InvalidBytecode(_) => Some(
                "This is likely a compiler/runtime bug. Please report this with a repro."
                    .to_string(),
            ),
            Self::Io(_) => Some(
                "Check file permissions, terminal availability, or input/output state.".to_string(),
            ),
            Self::Panic(_) => Some(
                "A panic was triggered. If this is unexpected, inspect the call stack.".to_string(),
            ),
        }
    }

    fn step(&self) -> &'static str {
        "VM"
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

    pub fn span(&self) -> Span {
        match self {
            Self::At(span, _) => *span,
            _ => Span::default(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum RuntimeValue {
    Null,
    Number(f64),
    Bool(bool),
}

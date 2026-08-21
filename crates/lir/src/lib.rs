use std::sync::atomic::AtomicU64;

pub mod ast;
pub mod dead_code;
pub mod environment;
pub mod translate;

pub static COUNTER: AtomicU64 = AtomicU64::new(0);

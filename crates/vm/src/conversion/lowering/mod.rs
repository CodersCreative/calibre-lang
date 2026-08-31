use super::ir::*;
use astro_float::Consts;
use calibre_lir::{
    ast::{BlockId, LirBlock, LirLValue, LirNode, LirNodeType, LirTerminator},
    environment::LirFunction,
};
use calibre_parser::Span;
use calibre_parser::ast::types::{ParserDataType, ParserInnerType};
use rustc_hash::FxHashMap;
use ustr::{Ustr, UstrMap, UstrSet};

mod block;
mod function;
mod ssa;

struct BlockLoweringCtx<'a> {
    block: &'a mut VMBlock,
    reg_count: &'a mut Reg,
    captures: UstrSet,
    map: UstrMap<Reg>,
    null_reg: Reg,
    ret_reg: Reg,
    is_global: bool,
    string_map: UstrMap<u16>,
    int_literals: FxHashMap<i64, u16>,
    uint_literals: FxHashMap<u64, u16>,
    float_literals: FxHashMap<u64, u16>,
    char_literals: FxHashMap<char, u16>,
    string_literals: UstrMap<u16>,
    current_fn_name: Ustr,
    big_consts: &'a mut Consts,
}

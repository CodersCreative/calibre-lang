use super::ir::*;
use calibre_lir::{
    BlockId, LirBlock, LirFunction, LirInstr, LirLValue, LirLiteral, LirNodeType, LirTerminator,
};
use calibre_parser::Span;
use calibre_parser::ast::{ParserDataType, ParserInnerType};
use rustc_hash::{FxHashMap, FxHashSet};

mod block;
mod function;

struct BlockLoweringCtx<'a> {
    block: &'a mut VMBlock,
    reg_count: &'a mut Reg,
    locals: FxHashSet<String>,
    captures: FxHashSet<String>,
    map: FxHashMap<String, Reg>,
    null_reg: Reg,
    ret_reg: Reg,
    is_global: bool,
    string_map: FxHashMap<String, u16>,
    int_literals: FxHashMap<i64, u16>,
    uint_literals: FxHashMap<u64, u16>,
    float_literals: FxHashMap<u64, u16>,
    char_literals: FxHashMap<char, u16>,
    string_literals: FxHashMap<String, u16>,
    current_fn_name: String,
    current_fn_short: String,
}

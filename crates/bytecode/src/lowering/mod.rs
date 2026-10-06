use crate::{Reg, VMBlock};
use calibre_lir::{
    TypeKey, VariableKey,
    ast::{LirNode, LirNodeType, LirTerminator},
};
use calibre_parser::Span;
use rustc_hash::{FxHashMap, FxHashSet};
use ustr::{Ustr, UstrMap};

mod block;
mod function;
mod optimizer;
mod ssa;

struct BlockLoweringCtx<'a> {
    block: &'a mut VMBlock,
    reg_count: &'a mut Reg,
    captures: FxHashSet<VariableKey>,
    map: FxHashMap<VariableKey, Reg>,
    referenced_variables: FxHashSet<VariableKey>,
    null_reg: Reg,
    ret_reg: Reg,
    is_global: bool,
    string_map: UstrMap<u16>,
    vars_map: FxHashMap<VariableKey, u16>,
    type_map: FxHashMap<TypeKey, u16>,
    int_literals: FxHashMap<i64, u16>,
    uint_literals: FxHashMap<u64, u16>,
    float_literals: FxHashMap<u64, u16>,
    char_literals: FxHashMap<char, u16>,
    string_literals: UstrMap<u16>,
    current_fn_name: VariableKey,
}

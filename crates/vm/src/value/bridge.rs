use super::*;

#[derive(Debug)]
pub enum TerminateValue {
    None,
    Jump(BlockId),
    Return(RuntimeValue),
    Yield {
        block: BlockId,
        ip: usize,
        prev_block: Option<BlockId>,
        yielded: Option<RuntimeValue>,
    },
}

use std::path::PathBuf;

#[derive(Clone, Default, Debug)]
pub struct VMConfig {
    pub gc_interval: Option<u64>,
    pub async_max_per_thread: Option<usize>,
    pub async_quantum: Option<usize>,
    pub profiling: bool,
    pub profiling_output: Option<PathBuf>,
    pub backtrace: bool,
}

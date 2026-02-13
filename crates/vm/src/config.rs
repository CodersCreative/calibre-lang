#[derive(Clone, Default, Debug)]
pub struct VMConfig {
    pub gc_interval: Option<u64>,
    pub async_max_per_thread: Option<usize>,
    pub async_quantum: Option<usize>,
}

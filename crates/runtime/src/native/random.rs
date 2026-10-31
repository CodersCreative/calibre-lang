#[unsafe(no_mangle)]
pub extern "C" fn calibre_random_rand(value: i64) -> f64 {
    let from = value as i32 as f64;
    let to = (value >> 32) as i32 as f64;

    from + fastrand::f64() * (to - from)
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_random_seed(value: u64) {
    fastrand::seed(value);
}

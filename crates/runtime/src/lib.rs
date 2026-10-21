#[derive(Debug, Clone, Copy)]
pub struct RuntimeSymbol {
    pub name: &'static str,
    pub ptr: *const u8,
}

unsafe impl Send for RuntimeSymbol {}
unsafe impl Sync for RuntimeSymbol {}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_assert_ints(actual: i64, expected: i64) -> i64 {
    println!("{} == {}", actual, expected);
    assert_eq!(actual, expected, "assert failed");
    1
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_print_int(value: i64) -> i64 {
    println!("{}", value);
    value
}

pub fn get_runtime_symbols() -> &'static [RuntimeSymbol] {
    &[
        RuntimeSymbol {
            name: "calibre_assert_ints",
            ptr: calibre_assert_ints as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_print_int",
            ptr: calibre_print_int as *const u8,
        },
    ]
}

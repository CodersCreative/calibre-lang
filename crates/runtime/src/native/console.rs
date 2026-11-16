use std::io::{self, BufReader};

use crate::values::reader::GcReader;

#[unsafe(no_mangle)]
pub extern "C" fn calibre_console_input_stream() -> *mut GcReader {
    let reader = BufReader::new(io::stdin());
    GcReader(Box::new(reader)).alloc()
}

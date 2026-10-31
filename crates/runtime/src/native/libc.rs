extern crate errno;
use errno::{Errno, errno, set_errno};

use crate::values::string::GcString;

#[unsafe(no_mangle)]
pub extern "C" fn calibre_libc_get_c_errno() -> i64 {
    let e = errno();
    e.0 as i64
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_libc_get_c_errno_description() -> *mut GcString {
    let e = errno();

    GcString {
        data: e.to_string(),
    }
    .alloc()
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_libc_set_c_errno(code: i64) {
    set_errno(Errno(code as i32));
}

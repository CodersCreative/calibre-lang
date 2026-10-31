use crate::values::string::GcString;
use blake3::Hasher as Blake3;
use sha2::{Digest, Sha256, Sha512};

#[unsafe(no_mangle)]
pub extern "C" fn calibre_crypto_sha256(text: *mut GcString) -> *mut GcString {
    if text.is_null() {
        return text;
    }

    let mut hasher = Sha256::new();
    hasher.update(unsafe { (*text).data.as_bytes() });
    let out = hasher.finalize();

    GcString {
        data: hex::encode(out),
    }
    .alloc()
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_crypto_sha512(text: *mut GcString) -> *mut GcString {
    if text.is_null() {
        return text;
    }

    let mut hasher = Sha512::new();
    hasher.update(unsafe { (*text).data.as_bytes() });
    let out = hasher.finalize();

    GcString {
        data: hex::encode(out),
    }
    .alloc()
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_crypto_blake3(text: *mut GcString) -> *mut GcString {
    if text.is_null() {
        return text;
    }

    let mut hasher = Blake3::new();
    hasher.update(unsafe { (*text).data.as_bytes() });
    let out = hasher.finalize();

    GcString {
        data: out.to_hex().to_string(),
    }
    .alloc()
}

use crate::values::{
    Value, aggregate::GcAggregate, enums::calibre_option_none, host::GcHost, list::GcList,
    string::GcString,
};
use std::{
    fs::{DirEntry, File, FileType, Metadata, OpenOptions},
    hash::{Hash, Hasher},
    io::{Read, Write},
    path::PathBuf,
    time::Duration,
};

#[unsafe(no_mangle)]
pub extern "C" fn calibre_fs_path_new(path: *mut GcString) -> *mut GcHost {
    if path.is_null() {
        return std::ptr::null_mut();
    }

    let path = PathBuf::from(unsafe { (*path).data.as_str() });

    GcHost {
        data: Box::new(path),
    }
    .alloc()
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_fs_path_as_str(path: *mut GcHost) -> *mut GcString {
    if path.is_null() {
        return std::ptr::null_mut();
    }

    let path = unsafe {
        (*path)
            .data
            .as_any()
            .downcast_ref::<PathBuf>()
            .expect("Invalid type when downcasting host")
    }
    .to_string_lossy();

    GcString {
        data: path.to_string(),
    }
    .alloc()
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_fs_path_exists(path: *mut GcHost) -> bool {
    if path.is_null() {
        return false;
    }

    unsafe {
        (*path)
            .data
            .as_any()
            .downcast_ref::<PathBuf>()
            .expect("Invalid type when downcasting host")
    }
    .exists()
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_fs_path_is_file(path: *mut GcHost) -> bool {
    if path.is_null() {
        return false;
    }

    unsafe {
        (*path)
            .data
            .as_any()
            .downcast_ref::<PathBuf>()
            .expect("Invalid type when downcasting host")
    }
    .is_file()
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_fs_path_is_dir(path: *mut GcHost) -> bool {
    if path.is_null() {
        return false;
    }

    unsafe {
        (*path)
            .data
            .as_any()
            .downcast_ref::<PathBuf>()
            .expect("Invalid type when downcasting host")
    }
    .is_dir()
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_fs_path_canonicalize(path: *mut GcHost) -> *mut GcAggregate {
    if path.is_null() {
        return Value::String(
            GcString {
                data: String::from("null ptr"),
            }
            .alloc(),
        )
        .err();
    }

    match unsafe {
        (*path)
            .data
            .as_any()
            .downcast_ref::<PathBuf>()
            .expect("Invalid type when downcasting host")
    }
    .canonicalize()
    {
        Ok(path) => Value::Host(
            GcHost {
                data: Box::new(path),
            }
            .alloc(),
        )
        .ok(),
        Err(e) => Value::String(
            GcString {
                data: e.to_string(),
            }
            .alloc(),
        )
        .err(),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_fs_path_parent(path: *mut GcHost) -> *mut GcAggregate {
    if path.is_null() {
        return calibre_option_none();
    }

    match unsafe {
        (*path)
            .data
            .as_any()
            .downcast_ref::<PathBuf>()
            .expect("Invalid type when downcasting host")
    }
    .parent()
    {
        Some(path) => Value::Host(
            GcHost {
                data: Box::new(path.to_path_buf()),
            }
            .alloc(),
        )
        .some(),
        None => calibre_option_none(),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_fs_path_file_name(path: *mut GcHost) -> *mut GcAggregate {
    if path.is_null() {
        return calibre_option_none();
    }

    match unsafe {
        (*path)
            .data
            .as_any()
            .downcast_ref::<PathBuf>()
            .expect("Invalid type when downcasting host")
    }
    .file_name()
    {
        Some(name) => Value::String(
            GcString {
                data: name.to_string_lossy().to_string(),
            }
            .alloc(),
        )
        .some(),
        None => calibre_option_none(),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_fs_path_extension(path: *mut GcHost) -> *mut GcAggregate {
    if path.is_null() {
        return calibre_option_none();
    }

    match unsafe {
        (*path)
            .data
            .as_any()
            .downcast_ref::<PathBuf>()
            .expect("Invalid type when downcasting host")
    }
    .extension()
    {
        Some(name) => Value::String(
            GcString {
                data: name.to_string_lossy().to_string(),
            }
            .alloc(),
        )
        .some(),
        None => calibre_option_none(),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_fs_path_stem(path: *mut GcHost) -> *mut GcAggregate {
    if path.is_null() {
        return calibre_option_none();
    }

    match unsafe {
        (*path)
            .data
            .as_any()
            .downcast_ref::<PathBuf>()
            .expect("Invalid type when downcasting host")
    }
    .file_stem()
    {
        Some(name) => Value::String(
            GcString {
                data: name.to_string_lossy().to_string(),
            }
            .alloc(),
        )
        .some(),
        None => calibre_option_none(),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_fs_path_join(path: *mut GcHost, other: *mut GcString) {
    if path.is_null() || other.is_null() {
        return;
    }

    unsafe {
        (*path)
            .data
            .as_any_mut()
            .downcast_mut::<PathBuf>()
            .expect("Invalid type when downcasting host")
    }
    .push(unsafe { (*other).data.as_str() });
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_fs_path_with_extension(
    path: *mut GcHost,
    other: *mut GcString,
) -> *mut GcHost {
    if path.is_null() || other.is_null() {
        return std::ptr::null_mut();
    }

    GcHost {
        data: Box::new(
            unsafe {
                (*path)
                    .data
                    .as_any_mut()
                    .downcast_mut::<PathBuf>()
                    .expect("Invalid type when downcasting host")
            }
            .with_extension(unsafe { (*other).data.as_str() }),
        ),
    }
    .alloc()
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_fs_path_with_file_name(
    path: *mut GcHost,
    other: *mut GcString,
) -> *mut GcHost {
    if path.is_null() || other.is_null() {
        return std::ptr::null_mut();
    }

    GcHost {
        data: Box::new(
            unsafe {
                (*path)
                    .data
                    .as_any_mut()
                    .downcast_mut::<PathBuf>()
                    .expect("Invalid type when downcasting host")
            }
            .with_file_name(unsafe { (*other).data.as_str() }),
        ),
    }
    .alloc()
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_fs_path_read_dir(path: *mut GcHost) -> *mut GcAggregate {
    if path.is_null() {
        return Value::String(
            GcString {
                data: String::from("null ptr"),
            }
            .alloc(),
        )
        .err();
    }

    match std::fs::read_dir(unsafe {
        (*path)
            .data
            .as_any()
            .downcast_ref::<PathBuf>()
            .expect("Invalid type when downcasting host")
    }) {
        Ok(entries) => {
            let mut out = Vec::new();
            for entry in entries {
                match entry {
                    Ok(entry) => {
                        out.push(Value::Host(
                            GcHost {
                                data: Box::new(HostDirEntry::from(entry)),
                            }
                            .alloc(),
                        ));
                    }
                    Err(e) => {
                        return Value::String(
                            GcString {
                                data: e.to_string(),
                            }
                            .alloc(),
                        )
                        .err();
                    }
                }
            }

            Value::List(GcList { data: out }.alloc()).ok()
        }
        Err(e) => Value::String(
            GcString {
                data: e.to_string(),
            }
            .alloc(),
        )
        .err(),
    }
}

#[derive(Debug)]
pub struct HostDirEntry(pub DirEntry);

impl Hash for HostDirEntry {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.path().hash(state);
    }
}

impl From<DirEntry> for HostDirEntry {
    fn from(value: DirEntry) -> Self {
        Self(value)
    }
}

#[derive(Debug)]
pub struct HostMetadata(pub Metadata);

impl Hash for HostMetadata {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.len().hash(state);
    }
}

impl From<Metadata> for HostMetadata {
    fn from(value: Metadata) -> Self {
        Self(value)
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_fs_direntry_path(entry: *mut GcHost) -> *mut GcHost {
    if entry.is_null() {
        return std::ptr::null_mut();
    }

    GcHost {
        data: Box::new(
            unsafe {
                (*entry)
                    .data
                    .as_any()
                    .downcast_ref::<HostDirEntry>()
                    .expect("Invalid type when downcasting host")
            }
            .0
            .path(),
        ),
    }
    .alloc()
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_fs_direntry_file_name(entry: *mut GcHost) -> *mut GcString {
    if entry.is_null() {
        return std::ptr::null_mut();
    }

    GcString {
        data: unsafe {
            (*entry)
                .data
                .as_any()
                .downcast_ref::<HostDirEntry>()
                .expect("Invalid type when downcasting host")
        }
        .0
        .file_name()
        .to_string_lossy()
        .to_string(),
    }
    .alloc()
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_fs_direntry_file_type(entry: *mut GcHost) -> *mut GcAggregate {
    if entry.is_null() {
        return Value::String(
            GcString {
                data: String::from("null ptr"),
            }
            .alloc(),
        )
        .err();
    }

    match unsafe {
        (*entry)
            .data
            .as_any()
            .downcast_ref::<HostDirEntry>()
            .expect("Invalid type when downcasting host")
    }
    .0
    .file_type()
    {
        Ok(ft) => Value::Host(GcHost { data: Box::new(ft) }.alloc()).ok(),
        Err(e) => Value::String(
            GcString {
                data: e.to_string(),
            }
            .alloc(),
        )
        .err(),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_fs_direntry_metadata(entry: *mut GcHost) -> *mut GcAggregate {
    if entry.is_null() {
        return Value::String(
            GcString {
                data: String::from("null ptr"),
            }
            .alloc(),
        )
        .err();
    }

    match unsafe {
        (*entry)
            .data
            .as_any()
            .downcast_ref::<HostDirEntry>()
            .expect("Invalid type when downcasting host")
    }
    .0
    .metadata()
    {
        Ok(meta) => Value::Host(
            GcHost {
                data: Box::new(HostMetadata::from(meta)),
            }
            .alloc(),
        )
        .ok(),
        Err(e) => Value::String(
            GcString {
                data: e.to_string(),
            }
            .alloc(),
        )
        .err(),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_fs_filetype_is_file(ft: *mut GcHost) -> bool {
    if ft.is_null() {
        return false;
    }

    unsafe {
        (*ft)
            .data
            .as_any()
            .downcast_ref::<FileType>()
            .expect("Invalid type when downcasting host")
    }
    .is_file()
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_fs_filetype_is_dir(ft: *mut GcHost) -> bool {
    if ft.is_null() {
        return false;
    }

    unsafe {
        (*ft)
            .data
            .as_any()
            .downcast_ref::<FileType>()
            .expect("Invalid type when downcasting host")
    }
    .is_dir()
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_fs_filetype_is_symlink(ft: *mut GcHost) -> bool {
    if ft.is_null() {
        return false;
    }

    unsafe {
        (*ft)
            .data
            .as_any()
            .downcast_ref::<FileType>()
            .expect("Invalid type when downcasting host")
    }
    .is_symlink()
}

// Metadata

#[unsafe(no_mangle)]
pub extern "C" fn calibre_fs_metadata_is_file(meta: *mut GcHost) -> bool {
    if meta.is_null() {
        return false;
    }

    unsafe {
        (*meta)
            .data
            .as_any()
            .downcast_ref::<HostMetadata>()
            .expect("Invalid type when downcasting host")
    }
    .0
    .is_file()
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_fs_metadata_is_dir(meta: *mut GcHost) -> bool {
    if meta.is_null() {
        return false;
    }

    unsafe {
        (*meta)
            .data
            .as_any()
            .downcast_ref::<HostMetadata>()
            .expect("Invalid type when downcasting host")
    }
    .0
    .is_dir()
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_fs_metadata_len(meta: *mut GcHost) -> u64 {
    if meta.is_null() {
        return 0;
    }

    unsafe {
        (*meta)
            .data
            .as_any()
            .downcast_ref::<HostMetadata>()
            .expect("Invalid type when downcasting host")
    }
    .0
    .len()
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_fs_metadata_modified(meta: *mut GcHost) -> *mut GcAggregate {
    if meta.is_null() {
        return Value::String(
            GcString {
                data: String::from("null ptr"),
            }
            .alloc(),
        )
        .err();
    }

    match unsafe {
        (*meta)
            .data
            .as_any()
            .downcast_ref::<HostMetadata>()
            .expect("Invalid type when downcasting host")
    }
    .0
    .modified()
    {
        Ok(time) => {
            let duration: Duration = time
                .duration_since(std::time::SystemTime::UNIX_EPOCH)
                .unwrap_or_default();

            Value::UInt(duration.as_secs()).ok()
        }
        Err(e) => Value::String(
            GcString {
                data: e.to_string(),
            }
            .alloc(),
        )
        .err(),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_fs_metadata_created(meta: *mut GcHost) -> *mut GcAggregate {
    if meta.is_null() {
        return Value::String(
            GcString {
                data: String::from("null ptr"),
            }
            .alloc(),
        )
        .err();
    }

    match unsafe {
        (*meta)
            .data
            .as_any()
            .downcast_ref::<HostMetadata>()
            .expect("Invalid type when downcasting host")
    }
    .0
    .created()
    {
        Ok(time) => {
            let duration: Duration = time
                .duration_since(std::time::SystemTime::UNIX_EPOCH)
                .unwrap_or_default();

            Value::UInt(duration.as_secs()).ok()
        }
        Err(e) => Value::String(
            GcString {
                data: e.to_string(),
            }
            .alloc(),
        )
        .err(),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_fs_metadata_accessed(meta: *mut GcHost) -> *mut GcAggregate {
    if meta.is_null() {
        return Value::String(
            GcString {
                data: String::from("null ptr"),
            }
            .alloc(),
        )
        .err();
    }

    match unsafe {
        (*meta)
            .data
            .as_any()
            .downcast_ref::<HostMetadata>()
            .expect("Invalid type when downcasting host")
    }
    .0
    .accessed()
    {
        Ok(time) => {
            let duration: Duration = time
                .duration_since(std::time::SystemTime::UNIX_EPOCH)
                .unwrap_or_default();

            Value::UInt(duration.as_secs()).ok()
        }
        Err(e) => Value::String(
            GcString {
                data: e.to_string(),
            }
            .alloc(),
        )
        .err(),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_fs_metadata_is_readonly(meta: *mut GcHost) -> bool {
    if meta.is_null() {
        return false;
    }

    unsafe {
        (*meta)
            .data
            .as_any()
            .downcast_ref::<HostMetadata>()
            .expect("Invalid type when downcasting host")
    }
    .0
    .permissions()
    .readonly()
}

// File

#[derive(Debug)]
pub struct HostFile(pub File);

impl Hash for HostFile {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0
            .metadata()
            .expect("Invalid type when downcasting host")
            .len()
            .hash(state);
    }
}

impl From<File> for HostFile {
    fn from(value: File) -> Self {
        Self(value)
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_fs_file_open(path: *mut GcHost, mode: *mut GcString) -> *mut GcAggregate {
    if path.is_null() || mode.is_null() {
        return Value::String(
            GcString {
                data: String::from("null ptr"),
            }
            .alloc(),
        )
        .err();
    }

    let mut options = OpenOptions::new();
    match unsafe { (*mode).data.as_str() } {
        "r" => options.read(true),
        "w" => options.write(true).create(true).truncate(true),
        "a" => options.write(true).create(true).append(true),
        "r+" | "rw" | "wr" => options.read(true).write(true),
        "w+" => options.read(true).write(true).create(true).truncate(true),
        "a+" => options.read(true).write(true).create(true).append(true),
        _ => {
            return Value::String(
                GcString {
                    data: String::from("invalid mode"),
                }
                .alloc(),
            )
            .err();
        }
    };

    match options.open(unsafe {
        (*path)
            .data
            .as_any()
            .downcast_ref::<PathBuf>()
            .expect("Invalid type when downcasting host")
    }) {
        Ok(file) => Value::Host(
            GcHost {
                data: Box::new(HostFile::from(file)),
            }
            .alloc(),
        )
        .ok(),
        Err(e) => Value::String(
            GcString {
                data: e.to_string(),
            }
            .alloc(),
        )
        .err(),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_fs_file_close(file: *mut GcHost) -> *mut GcAggregate {
    if file.is_null() {
        return Value::String(
            GcString {
                data: String::from("null ptr"),
            }
            .alloc(),
        )
        .err();
    }

    match unsafe {
        (*file)
            .data
            .as_any_mut()
            .downcast_mut::<HostFile>()
            .expect("Invalid type when downcasting host")
    }
    .0
    .flush()
    {
        Ok(_) => Value::Null.ok(),
        Err(e) => Value::String(
            GcString {
                data: e.to_string(),
            }
            .alloc(),
        )
        .err(),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_fs_file_write(
    file: *mut GcHost,
    content: *mut GcString,
) -> *mut GcAggregate {
    if file.is_null() {
        return Value::String(
            GcString {
                data: String::from("null ptr"),
            }
            .alloc(),
        )
        .err();
    }

    match unsafe {
        (*file)
            .data
            .as_any_mut()
            .downcast_mut::<HostFile>()
            .expect("Invalid type when downcasting host")
    }
    .0
    .write_all(unsafe { (*content).data.as_bytes() })
    {
        Ok(_) => Value::Null.ok(),
        Err(e) => Value::String(
            GcString {
                data: e.to_string(),
            }
            .alloc(),
        )
        .err(),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_fs_file_write_line(
    file: *mut GcHost,
    content: *mut GcString,
) -> *mut GcAggregate {
    if file.is_null() {
        return Value::String(
            GcString {
                data: String::from("null ptr"),
            }
            .alloc(),
        )
        .err();
    }

    match writeln!(
        unsafe {
            (*file)
                .data
                .as_any_mut()
                .downcast_mut::<HostFile>()
                .expect("Invalid type when downcasting host")
        }
        .0,
        "{}",
        unsafe { (*content).data.as_str() }
    ) {
        Ok(_) => Value::Null.ok(),
        Err(e) => Value::String(
            GcString {
                data: e.to_string(),
            }
            .alloc(),
        )
        .err(),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_fs_file_read_all(file: *mut GcHost) -> *mut GcAggregate {
    if file.is_null() {
        return Value::String(
            GcString {
                data: String::from("null ptr"),
            }
            .alloc(),
        )
        .err();
    }

    let mut content = String::new();

    match unsafe {
        (*file)
            .data
            .as_any_mut()
            .downcast_mut::<HostFile>()
            .expect("Invalid type when downcasting host")
    }
    .0
    .read_to_string(&mut content)
    {
        Ok(_) => Value::String(GcString { data: content }.alloc()).ok(),
        Err(e) => Value::String(
            GcString {
                data: e.to_string(),
            }
            .alloc(),
        )
        .err(),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_fs_file_flush(file: *mut GcHost) -> *mut GcAggregate {
    if file.is_null() {
        return Value::String(
            GcString {
                data: String::from("null ptr"),
            }
            .alloc(),
        )
        .err();
    }

    match unsafe {
        (*file)
            .data
            .as_any_mut()
            .downcast_mut::<HostFile>()
            .expect("Invalid type when downcasting host")
    }
    .0
    .flush()
    {
        Ok(_) => Value::Null.ok(),
        Err(e) => Value::String(
            GcString {
                data: e.to_string(),
            }
            .alloc(),
        )
        .err(),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_fs_dir_create(path: *mut GcString) -> *mut GcAggregate {
    if path.is_null() {
        return Value::String(
            GcString {
                data: String::from("null ptr"),
            }
            .alloc(),
        )
        .err();
    }

    match std::fs::create_dir(unsafe { (*path).data.as_str() }) {
        Ok(_) => Value::Null.ok(),
        Err(e) => Value::String(
            GcString {
                data: e.to_string(),
            }
            .alloc(),
        )
        .err(),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_fs_dir_create_all(path: *mut GcString) -> *mut GcAggregate {
    if path.is_null() {
        return Value::String(
            GcString {
                data: String::from("null ptr"),
            }
            .alloc(),
        )
        .err();
    }

    match std::fs::create_dir_all(unsafe { (*path).data.as_str() }) {
        Ok(_) => Value::Null.ok(),
        Err(e) => Value::String(
            GcString {
                data: e.to_string(),
            }
            .alloc(),
        )
        .err(),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_fs_dir_remove(path: *mut GcString) -> *mut GcAggregate {
    if path.is_null() {
        return Value::String(
            GcString {
                data: String::from("null ptr"),
            }
            .alloc(),
        )
        .err();
    }

    match std::fs::remove_dir(unsafe { (*path).data.as_str() }) {
        Ok(_) => Value::Null.ok(),
        Err(e) => Value::String(
            GcString {
                data: e.to_string(),
            }
            .alloc(),
        )
        .err(),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_fs_dir_remove_all(path: *mut GcString) -> *mut GcAggregate {
    if path.is_null() {
        return Value::String(
            GcString {
                data: String::from("null ptr"),
            }
            .alloc(),
        )
        .err();
    }

    match std::fs::remove_dir_all(unsafe { (*path).data.as_str() }) {
        Ok(_) => Value::Null.ok(),
        Err(e) => Value::String(
            GcString {
                data: e.to_string(),
            }
            .alloc(),
        )
        .err(),
    }
}

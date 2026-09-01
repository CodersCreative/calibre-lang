use crate::{
    VM,
    error::RuntimeError,
    native::{
        NativeFunction,
        utils::{expect_num_args, pop_or_null, resolve_host, resolve_str},
    },
    value::{GcVec, RuntimeValue},
};
use dumpster::sync::Gc;
use std::io::{Read, Write};
use std::path::PathBuf;
use std::sync::Arc;
use std::{
    fs::{DirEntry, File, FileType, Metadata, OpenOptions},
    time::Duration,
};
use ustr::Ustr;
use wasm_sync::Mutex;

// Path

pub struct FsPathNew;

impl NativeFunction for FsPathNew {
    fn name(&self) -> String {
        String::from("fs.path_new")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        let path = resolve_str(env, &pop_or_null(&mut args))?;

        let path_buf = PathBuf::from(path.as_str());
        Ok(RuntimeValue::Host(Arc::new(Mutex::new(path_buf))))
    }
}

pub struct FsPathAsStr;

impl NativeFunction for FsPathAsStr {
    fn name(&self) -> String {
        String::from("fs.path_as_str")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        let path = resolve_host(env, &pop_or_null(&mut args))?;

        Ok(RuntimeValue::Str(Ustr::from(
            &path
                .lock()
                .unwrap()
                .downcast_ref::<PathBuf>()
                .ok_or_else(|| RuntimeError::UnexpectedType(Box::new(RuntimeValue::Null)))?
                .to_string_lossy(),
        )))
    }
}

pub struct FsPathExists;

impl NativeFunction for FsPathExists {
    fn name(&self) -> String {
        String::from("fs.path_exists")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        let path = resolve_host(env, &pop_or_null(&mut args))?;

        Ok(RuntimeValue::Bool(
            path.lock()
                .unwrap()
                .downcast_ref::<PathBuf>()
                .ok_or_else(|| RuntimeError::UnexpectedType(Box::new(RuntimeValue::Null)))?
                .exists(),
        ))
    }
}

pub struct FsPathIsFile;

impl NativeFunction for FsPathIsFile {
    fn name(&self) -> String {
        String::from("fs.path_is_file")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        let path = resolve_host(env, &pop_or_null(&mut args))?;

        Ok(RuntimeValue::Bool(
            path.lock()
                .unwrap()
                .downcast_ref::<PathBuf>()
                .ok_or_else(|| RuntimeError::UnexpectedType(Box::new(RuntimeValue::Null)))?
                .is_file(),
        ))
    }
}

pub struct FsPathIsDir;

impl NativeFunction for FsPathIsDir {
    fn name(&self) -> String {
        String::from("fs.path_is_dir")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        let path = resolve_host(env, &pop_or_null(&mut args))?;

        Ok(RuntimeValue::Bool(
            path.lock()
                .unwrap()
                .downcast_ref::<PathBuf>()
                .ok_or_else(|| RuntimeError::UnexpectedType(Box::new(RuntimeValue::Null)))?
                .is_dir(),
        ))
    }
}

pub struct FsPathCanonicalize;

impl NativeFunction for FsPathCanonicalize {
    fn name(&self) -> String {
        String::from("fs.path_canonicalize")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        let path = resolve_host(env, &pop_or_null(&mut args))?;

        match path
            .lock()
            .unwrap()
            .downcast_ref::<PathBuf>()
            .ok_or_else(|| RuntimeError::UnexpectedType(Box::new(RuntimeValue::Null)))?
            .canonicalize()
        {
            Ok(canonical) => Ok(RuntimeValue::Result(Ok(Gc::new(RuntimeValue::Host(
                Arc::new(Mutex::new(canonical)),
            ))))),
            Err(e) => Ok(RuntimeValue::Result(Err(Gc::new(RuntimeValue::Str(
                Ustr::from(&e.to_string()),
            ))))),
        }
    }
}

pub struct FsPathParent;

impl NativeFunction for FsPathParent {
    fn name(&self) -> String {
        String::from("fs.path_parent")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        let path = resolve_host(env, &pop_or_null(&mut args))?;

        match path
            .lock()
            .unwrap()
            .downcast_ref::<PathBuf>()
            .ok_or_else(|| RuntimeError::UnexpectedType(Box::new(RuntimeValue::Null)))?
            .parent()
        {
            Some(parent) => Ok(RuntimeValue::Host(Arc::new(Mutex::new(
                parent.to_path_buf(),
            )))),
            None => Ok(RuntimeValue::Null),
        }
    }
}

pub struct FsPathFileName;

impl NativeFunction for FsPathFileName {
    fn name(&self) -> String {
        String::from("fs.path_file_name")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        let path = resolve_host(env, &pop_or_null(&mut args))?;

        match path
            .lock()
            .unwrap()
            .downcast_ref::<PathBuf>()
            .ok_or_else(|| RuntimeError::UnexpectedType(Box::new(RuntimeValue::Null)))?
            .file_name()
        {
            Some(name) => Ok(RuntimeValue::Str(Ustr::from(&name.to_string_lossy()))),
            None => Ok(RuntimeValue::Null),
        }
    }
}

pub struct FsPathExtension;

impl NativeFunction for FsPathExtension {
    fn name(&self) -> String {
        String::from("fs.path_extension")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        let path = resolve_host(env, &pop_or_null(&mut args))?;

        match path
            .lock()
            .unwrap()
            .downcast_ref::<PathBuf>()
            .ok_or_else(|| RuntimeError::UnexpectedType(Box::new(RuntimeValue::Null)))?
            .extension()
        {
            Some(ext) => Ok(RuntimeValue::Str(Ustr::from(&ext.to_string_lossy()))),
            None => Ok(RuntimeValue::Null),
        }
    }
}

pub struct FsPathStem;

impl NativeFunction for FsPathStem {
    fn name(&self) -> String {
        String::from("fs.path_stem")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        let path = resolve_host(env, &pop_or_null(&mut args))?;

        match path
            .lock()
            .unwrap()
            .downcast_ref::<PathBuf>()
            .ok_or_else(|| RuntimeError::UnexpectedType(Box::new(RuntimeValue::Null)))?
            .file_stem()
        {
            Some(stem) => Ok(RuntimeValue::Str(Ustr::from(&stem.to_string_lossy()))),
            None => Ok(RuntimeValue::Null),
        }
    }
}

pub struct FsPathJoin;

impl NativeFunction for FsPathJoin {
    fn name(&self) -> String {
        String::from("fs.path_join")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[2])?;

        let other = resolve_str(env, &pop_or_null(&mut args))?;
        let path = resolve_host(env, &pop_or_null(&mut args))?;

        path.lock()
            .unwrap()
            .downcast_mut::<PathBuf>()
            .ok_or_else(|| RuntimeError::UnexpectedType(Box::new(RuntimeValue::Null)))?
            .push(other.as_str());
        Ok(RuntimeValue::Null)
    }
}

pub struct FsPathWithExtension;

impl NativeFunction for FsPathWithExtension {
    fn name(&self) -> String {
        String::from("fs.path_with_extension")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[2])?;

        let other = resolve_str(env, &pop_or_null(&mut args))?;
        let path = resolve_host(env, &pop_or_null(&mut args))?;

        Ok(RuntimeValue::Host(Arc::new(Mutex::new(
            path.lock()
                .unwrap()
                .downcast_ref::<PathBuf>()
                .ok_or_else(|| RuntimeError::UnexpectedType(Box::new(RuntimeValue::Null)))?
                .with_extension(other.as_str()),
        ))))
    }
}

pub struct FsPathWithFileName;

impl NativeFunction for FsPathWithFileName {
    fn name(&self) -> String {
        String::from("fs.path_with_file_name")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[2])?;

        let other = resolve_str(env, &pop_or_null(&mut args))?;
        let path = resolve_host(env, &pop_or_null(&mut args))?;

        Ok(RuntimeValue::Host(Arc::new(Mutex::new(
            path.lock()
                .unwrap()
                .downcast_ref::<PathBuf>()
                .ok_or_else(|| RuntimeError::UnexpectedType(Box::new(RuntimeValue::Null)))?
                .with_file_name(other.as_str()),
        ))))
    }
}

pub struct FsPathReadDir;

impl NativeFunction for FsPathReadDir {
    fn name(&self) -> String {
        String::from("fs.path_read_dir")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        let path = resolve_host(env, &pop_or_null(&mut args))?;

        match std::fs::read_dir(
            path.lock()
                .unwrap()
                .downcast_ref::<PathBuf>()
                .ok_or_else(|| RuntimeError::UnexpectedType(Box::new(RuntimeValue::Null)))?,
        ) {
            Ok(entries) => {
                let mut out = Vec::new();
                for entry in entries {
                    match entry {
                        Ok(entry) => {
                            out.push(RuntimeValue::Host(Arc::new(Mutex::new(entry))));
                        }
                        Err(err) => {
                            return Ok(RuntimeValue::Result(Err(Gc::new(RuntimeValue::Str(
                                Ustr::from(&err.to_string()),
                            )))));
                        }
                    }
                }
                Ok(RuntimeValue::Result(Ok(Gc::new(RuntimeValue::List(
                    Gc::new(GcVec(out)),
                )))))
            }
            Err(err) => Ok(RuntimeValue::Result(Err(Gc::new(RuntimeValue::Str(
                Ustr::from(&err.to_string()),
            ))))),
        }
    }
}

// DirEntry

pub struct FsDirEntryPath;

impl NativeFunction for FsDirEntryPath {
    fn name(&self) -> String {
        String::from("fs.direntry_path")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        let entry = resolve_host(env, &pop_or_null(&mut args))?;

        Ok(RuntimeValue::Host(Arc::new(Mutex::new(
            entry
                .lock()
                .unwrap()
                .downcast_ref::<DirEntry>()
                .ok_or_else(|| RuntimeError::UnexpectedType(Box::new(RuntimeValue::Null)))?
                .path(),
        ))))
    }
}

pub struct FsDirEntryFileName;

impl NativeFunction for FsDirEntryFileName {
    fn name(&self) -> String {
        String::from("fs.direntry_file_name")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        let entry = resolve_host(env, &pop_or_null(&mut args))?;

        Ok(RuntimeValue::Str(Ustr::from(
            &entry
                .lock()
                .unwrap()
                .downcast_ref::<DirEntry>()
                .ok_or_else(|| RuntimeError::UnexpectedType(Box::new(RuntimeValue::Null)))?
                .file_name()
                .to_string_lossy(),
        )))
    }
}

pub struct FsDirEntryFileType;

impl NativeFunction for FsDirEntryFileType {
    fn name(&self) -> String {
        String::from("fs.direntry_file_type")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        let entry = resolve_host(env, &pop_or_null(&mut args))?;

        match entry
            .lock()
            .unwrap()
            .downcast_ref::<DirEntry>()
            .ok_or_else(|| RuntimeError::UnexpectedType(Box::new(RuntimeValue::Null)))?
            .file_type()
        {
            Ok(ft) => Ok(RuntimeValue::Result(Ok(Gc::new(RuntimeValue::Host(
                Arc::new(Mutex::new(ft)),
            ))))),
            Err(e) => Ok(RuntimeValue::Result(Err(Gc::new(RuntimeValue::Str(
                Ustr::from(&e.to_string()),
            ))))),
        }
    }
}

pub struct FsDirEntryMetadata;

impl NativeFunction for FsDirEntryMetadata {
    fn name(&self) -> String {
        String::from("fs.direntry_metadata")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        let entry = resolve_host(env, &pop_or_null(&mut args))?;

        match entry
            .lock()
            .unwrap()
            .downcast_ref::<DirEntry>()
            .ok_or_else(|| RuntimeError::UnexpectedType(Box::new(RuntimeValue::Null)))?
            .metadata()
        {
            Ok(meta) => Ok(RuntimeValue::Result(Ok(Gc::new(RuntimeValue::Host(
                Arc::new(Mutex::new(meta)),
            ))))),
            Err(e) => Ok(RuntimeValue::Result(Err(Gc::new(RuntimeValue::Str(
                Ustr::from(&e.to_string()),
            ))))),
        }
    }
}

// FileType

pub struct FsFileTypeIsFile;

impl NativeFunction for FsFileTypeIsFile {
    fn name(&self) -> String {
        String::from("fs.filetype_is_file")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        let ft = resolve_host(env, &pop_or_null(&mut args))?;

        Ok(RuntimeValue::Bool(
            ft.lock()
                .unwrap()
                .downcast_ref::<FileType>()
                .ok_or_else(|| RuntimeError::UnexpectedType(Box::new(RuntimeValue::Null)))?
                .is_file(),
        ))
    }
}

pub struct FsFileTypeIsDir;

impl NativeFunction for FsFileTypeIsDir {
    fn name(&self) -> String {
        String::from("fs.filetype_is_dir")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        let ft = resolve_host(env, &pop_or_null(&mut args))?;

        Ok(RuntimeValue::Bool(
            ft.lock()
                .unwrap()
                .downcast_ref::<FileType>()
                .ok_or_else(|| RuntimeError::UnexpectedType(Box::new(RuntimeValue::Null)))?
                .is_dir(),
        ))
    }
}

pub struct FsFileTypeIsSymlink;

impl NativeFunction for FsFileTypeIsSymlink {
    fn name(&self) -> String {
        String::from("fs.filetype_is_symlink")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        let ft = resolve_host(env, &pop_or_null(&mut args))?;

        Ok(RuntimeValue::Bool(
            ft.lock()
                .unwrap()
                .downcast_ref::<FileType>()
                .ok_or_else(|| RuntimeError::UnexpectedType(Box::new(RuntimeValue::Null)))?
                .is_symlink(),
        ))
    }
}

// Metadata

pub struct FsMetadataIsFile;

impl NativeFunction for FsMetadataIsFile {
    fn name(&self) -> String {
        String::from("fs.metadata_is_file")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        let meta = resolve_host(env, &pop_or_null(&mut args))?;

        Ok(RuntimeValue::Bool(
            meta.lock()
                .unwrap()
                .downcast_ref::<Metadata>()
                .ok_or_else(|| RuntimeError::UnexpectedType(Box::new(RuntimeValue::Null)))?
                .is_file(),
        ))
    }
}

pub struct FsMetadataIsDir;

impl NativeFunction for FsMetadataIsDir {
    fn name(&self) -> String {
        String::from("fs.metadata_is_dir")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        let meta = resolve_host(env, &pop_or_null(&mut args))?;

        Ok(RuntimeValue::Bool(
            meta.lock()
                .unwrap()
                .downcast_ref::<Metadata>()
                .ok_or_else(|| RuntimeError::UnexpectedType(Box::new(RuntimeValue::Null)))?
                .is_dir(),
        ))
    }
}

pub struct FsMetadataLen;

impl NativeFunction for FsMetadataLen {
    fn name(&self) -> String {
        String::from("fs.metadata_len")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        let meta = resolve_host(env, &pop_or_null(&mut args))?;

        Ok(RuntimeValue::UInt(
            meta.lock()
                .unwrap()
                .downcast_ref::<Metadata>()
                .ok_or_else(|| RuntimeError::UnexpectedType(Box::new(RuntimeValue::Null)))?
                .len(),
        ))
    }
}

pub struct FsMetadataModified;

impl NativeFunction for FsMetadataModified {
    fn name(&self) -> String {
        String::from("fs.metadata_modified")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        let meta = resolve_host(env, &pop_or_null(&mut args))?;

        match meta
            .lock()
            .unwrap()
            .downcast_ref::<Metadata>()
            .ok_or_else(|| RuntimeError::UnexpectedType(Box::new(RuntimeValue::Null)))?
            .modified()
        {
            Ok(time) => {
                let duration: Duration = time
                    .duration_since(std::time::SystemTime::UNIX_EPOCH)
                    .unwrap_or_default();
                Ok(RuntimeValue::Result(Ok(Gc::new(RuntimeValue::UInt(
                    duration.as_secs(),
                )))))
            }
            Err(e) => Ok(RuntimeValue::Result(Err(Gc::new(RuntimeValue::Str(
                Ustr::from(&e.to_string()),
            ))))),
        }
    }
}

pub struct FsMetadataCreated;

impl NativeFunction for FsMetadataCreated {
    fn name(&self) -> String {
        String::from("fs.metadata_created")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        let meta = resolve_host(env, &pop_or_null(&mut args))?;

        match meta
            .lock()
            .unwrap()
            .downcast_ref::<Metadata>()
            .ok_or_else(|| RuntimeError::UnexpectedType(Box::new(RuntimeValue::Null)))?
            .created()
        {
            Ok(time) => {
                let duration: Duration = time
                    .duration_since(std::time::SystemTime::UNIX_EPOCH)
                    .unwrap_or_default();
                Ok(RuntimeValue::Result(Ok(Gc::new(RuntimeValue::UInt(
                    duration.as_secs(),
                )))))
            }
            Err(e) => Ok(RuntimeValue::Result(Err(Gc::new(RuntimeValue::Str(
                Ustr::from(&e.to_string()),
            ))))),
        }
    }
}

pub struct FsMetadataAccessed;

impl NativeFunction for FsMetadataAccessed {
    fn name(&self) -> String {
        String::from("fs.metadata_accessed")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        let meta = resolve_host(env, &pop_or_null(&mut args))?;

        match meta
            .lock()
            .unwrap()
            .downcast_ref::<Metadata>()
            .ok_or_else(|| RuntimeError::UnexpectedType(Box::new(RuntimeValue::Null)))?
            .accessed()
        {
            Ok(time) => {
                let duration: Duration = time
                    .duration_since(std::time::SystemTime::UNIX_EPOCH)
                    .unwrap_or_default();
                Ok(RuntimeValue::Result(Ok(Gc::new(RuntimeValue::UInt(
                    duration.as_secs(),
                )))))
            }
            Err(e) => Ok(RuntimeValue::Result(Err(Gc::new(RuntimeValue::Str(
                Ustr::from(&e.to_string()),
            ))))),
        }
    }
}

pub struct FsMetadataIsReadOnly;

impl NativeFunction for FsMetadataIsReadOnly {
    fn name(&self) -> String {
        String::from("fs.metadata_is_readonly")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        let meta = resolve_host(env, &pop_or_null(&mut args))?;

        Ok(RuntimeValue::Bool(
            meta.lock()
                .unwrap()
                .downcast_ref::<Metadata>()
                .ok_or_else(|| RuntimeError::UnexpectedType(Box::new(RuntimeValue::Null)))?
                .permissions()
                .readonly(),
        ))
    }
}

// File

pub struct FsFileOpen;

impl NativeFunction for FsFileOpen {
    fn name(&self) -> String {
        String::from("fs.file_open")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[2])?;

        let mode = resolve_str(env, &pop_or_null(&mut args))?;
        let path = resolve_host(env, &pop_or_null(&mut args))?;

        let mut options = OpenOptions::new();
        match mode.as_str() {
            "r" => options.read(true),
            "w" => options.write(true).create(true).truncate(true),
            "a" => options.write(true).create(true).append(true),
            "r+" | "rw" | "wr" => options.read(true).write(true),
            "w+" => options.read(true).write(true).create(true).truncate(true),
            "a+" => options.read(true).write(true).create(true).append(true),
            _ => {
                return Ok(RuntimeValue::Result(Err(Gc::new(RuntimeValue::Str(
                    Ustr::from("invalid mode"),
                )))));
            }
        };

        match options.open(
            path.lock()
                .unwrap()
                .downcast_ref::<PathBuf>()
                .ok_or_else(|| RuntimeError::UnexpectedType(Box::new(RuntimeValue::Null)))?,
        ) {
            Ok(file) => Ok(RuntimeValue::Result(Ok(Gc::new(RuntimeValue::Host(
                Arc::new(Mutex::new(file)),
            ))))),
            Err(e) => Ok(RuntimeValue::Result(Err(Gc::new(RuntimeValue::Str(
                Ustr::from(&e.to_string()),
            ))))),
        }
    }
}

pub struct FsFileClose;

impl NativeFunction for FsFileClose {
    fn name(&self) -> String {
        String::from("fs.file_close")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        let file = resolve_host(env, &pop_or_null(&mut args))?;

        match file
            .lock()
            .unwrap()
            .downcast_mut::<File>()
            .ok_or_else(|| RuntimeError::UnexpectedType(Box::new(RuntimeValue::Null)))?
            .flush()
        {
            Ok(_) => (),
            Err(e) => {
                return Ok(RuntimeValue::Result(Err(Gc::new(RuntimeValue::Str(
                    Ustr::from(&e.to_string()),
                )))));
            }
        }
        Ok(RuntimeValue::Result(Ok(Gc::new(RuntimeValue::Null))))
    }
}

pub struct FsFileWrite;

impl NativeFunction for FsFileWrite {
    fn name(&self) -> String {
        String::from("fs.file_write")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[2])?;

        let content = resolve_str(env, &pop_or_null(&mut args))?;
        let file = resolve_host(env, &pop_or_null(&mut args))?;

        match file
            .lock()
            .unwrap()
            .downcast_mut::<File>()
            .ok_or_else(|| RuntimeError::UnexpectedType(Box::new(RuntimeValue::Null)))?
            .write_all(content.as_bytes())
        {
            Ok(_) => Ok(RuntimeValue::Result(Ok(Gc::new(RuntimeValue::Null)))),
            Err(e) => Ok(RuntimeValue::Result(Err(Gc::new(RuntimeValue::Str(
                Ustr::from(&e.to_string()),
            ))))),
        }
    }
}

pub struct FsFileWriteLine;

impl NativeFunction for FsFileWriteLine {
    fn name(&self) -> String {
        String::from("fs.file_write_line")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[2])?;

        let content = resolve_str(env, &pop_or_null(&mut args))?;
        let file = resolve_host(env, &pop_or_null(&mut args))?;

        match writeln!(
            file.lock()
                .unwrap()
                .downcast_mut::<File>()
                .ok_or_else(|| RuntimeError::UnexpectedType(Box::new(RuntimeValue::Null)))?,
            "{}",
            content
        ) {
            Ok(_) => Ok(RuntimeValue::Result(Ok(Gc::new(RuntimeValue::Null)))),
            Err(e) => Ok(RuntimeValue::Result(Err(Gc::new(RuntimeValue::Str(
                Ustr::from(&e.to_string()),
            ))))),
        }
    }
}

pub struct FsFileReadAll;

impl NativeFunction for FsFileReadAll {
    fn name(&self) -> String {
        String::from("fs.file_read_all")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        let file = resolve_host(env, &pop_or_null(&mut args))?;

        let mut content = String::new();

        match file
            .lock()
            .unwrap()
            .downcast_mut::<File>()
            .ok_or_else(|| RuntimeError::UnexpectedType(Box::new(RuntimeValue::Null)))?
            .read_to_string(&mut content)
        {
            Ok(_) => Ok(RuntimeValue::Result(Ok(Gc::new(RuntimeValue::Str(
                Ustr::from(&content),
            ))))),
            Err(e) => Ok(RuntimeValue::Result(Err(Gc::new(RuntimeValue::Str(
                Ustr::from(&e.to_string()),
            ))))),
        }
    }
}

pub struct FsFileFlush;

impl NativeFunction for FsFileFlush {
    fn name(&self) -> String {
        String::from("fs.file_flush")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        let file = resolve_host(env, &pop_or_null(&mut args))?;

        match file
            .lock()
            .unwrap()
            .downcast_mut::<File>()
            .ok_or_else(|| RuntimeError::UnexpectedType(Box::new(RuntimeValue::Null)))?
            .flush()
        {
            Ok(_) => Ok(RuntimeValue::Result(Ok(Gc::new(RuntimeValue::Null)))),
            Err(e) => Ok(RuntimeValue::Result(Err(Gc::new(RuntimeValue::Str(
                Ustr::from(&e.to_string()),
            ))))),
        }
    }
}

// Dir

pub struct FsDirCreate;

impl NativeFunction for FsDirCreate {
    fn name(&self) -> String {
        String::from("fs.dir_create")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        let path = resolve_str(env, &pop_or_null(&mut args))?;

        match std::fs::create_dir(path.as_str()) {
            Ok(_) => Ok(RuntimeValue::Result(Ok(Gc::new(RuntimeValue::Null)))),
            Err(e) => Ok(RuntimeValue::Result(Err(Gc::new(RuntimeValue::Str(
                Ustr::from(&e.to_string()),
            ))))),
        }
    }
}

pub struct FsDirCreateAll;

impl NativeFunction for FsDirCreateAll {
    fn name(&self) -> String {
        String::from("fs.dir_create_all")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        let path = resolve_str(env, &pop_or_null(&mut args))?;

        match std::fs::create_dir_all(path.as_str()) {
            Ok(_) => Ok(RuntimeValue::Result(Ok(Gc::new(RuntimeValue::Null)))),
            Err(e) => Ok(RuntimeValue::Result(Err(Gc::new(RuntimeValue::Str(
                Ustr::from(&e.to_string()),
            ))))),
        }
    }
}

pub struct FsDirRemove;

impl NativeFunction for FsDirRemove {
    fn name(&self) -> String {
        String::from("fs.dir_remove")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        let path = resolve_str(env, &pop_or_null(&mut args))?;

        match std::fs::remove_dir(path.as_str()) {
            Ok(_) => Ok(RuntimeValue::Result(Ok(Gc::new(RuntimeValue::Null)))),
            Err(e) => Ok(RuntimeValue::Result(Err(Gc::new(RuntimeValue::Str(
                Ustr::from(&e.to_string()),
            ))))),
        }
    }
}

pub struct FsDirRemoveAll;

impl NativeFunction for FsDirRemoveAll {
    fn name(&self) -> String {
        String::from("fs.dir_remove_all")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        let path = resolve_str(env, &pop_or_null(&mut args))?;

        match std::fs::remove_dir_all(path.as_str()) {
            Ok(_) => Ok(RuntimeValue::Result(Ok(Gc::new(RuntimeValue::Null)))),
            Err(e) => Ok(RuntimeValue::Result(Err(Gc::new(RuntimeValue::Str(
                Ustr::from(&e.to_string()),
            ))))),
        }
    }
}

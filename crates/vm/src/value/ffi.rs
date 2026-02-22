use super::*;

impl ExternFunction {
    fn resolve_value(env: &mut VM, value: RuntimeValue) -> RuntimeValue {
        match value {
            RuntimeValue::Ref(name) => env
                .variables
                .get(&name)
                .cloned()
                .unwrap_or(RuntimeValue::Ref(name)),
            RuntimeValue::VarRef(id) => env
                .variables
                .get_by_id(id)
                .unwrap_or(RuntimeValue::VarRef(id)),
            RuntimeValue::RegRef { frame, reg } => env.get_reg_value_in_frame(frame, reg),
            RuntimeValue::Ptr(id) => env
                .ptr_heap
                .get(&id)
                .cloned()
                .unwrap_or(RuntimeValue::Ptr(id)),
            RuntimeValue::MutexGuard(guard) => guard.get_clone(),
            other => other,
        }
    }
    fn type_to_libffi_type(typ: &ParserDataType) -> Type {
        match &typ.data_type {
            ParserInnerType::Int => Type::i64(),
            ParserInnerType::UInt => Type::u64(),
            ParserInnerType::Float => Type::f64(),
            ParserInnerType::Bool => Type::u8(),
            ParserInnerType::Char => Type::u8(),
            ParserInnerType::Str => Type::pointer(),
            ParserInnerType::Ptr(_) => Type::pointer(),
            ParserInnerType::Null => Type::void(),
            ParserInnerType::FfiType(x) => match x {
                ParserFfiInnerType::F32 => Type::f32(),
                ParserFfiInnerType::F64 | ParserFfiInnerType::LongDouble => Type::f64(),
                ParserFfiInnerType::U8 | ParserFfiInnerType::UChar => Type::u8(),
                ParserFfiInnerType::I8 | ParserFfiInnerType::SChar => Type::i8(),
                ParserFfiInnerType::U16 | ParserFfiInnerType::UShort => Type::u16(),
                ParserFfiInnerType::I16 | ParserFfiInnerType::Short => Type::i16(),
                ParserFfiInnerType::U32 | ParserFfiInnerType::UInt => Type::u32(),
                ParserFfiInnerType::I32 | ParserFfiInnerType::Int => Type::i32(),
                ParserFfiInnerType::U64
                | ParserFfiInnerType::ULong
                | ParserFfiInnerType::ULongLong => Type::u64(),
                ParserFfiInnerType::I64
                | ParserFfiInnerType::Long
                | ParserFfiInnerType::LongLong => Type::i64(),
                ParserFfiInnerType::USize => Type::u64(),
                ParserFfiInnerType::ISize => Type::i64(),
            },
            _ => Type::pointer(),
        }
    }

    fn push_arg(ffi_args: &mut Vec<FfiArg>, libffi_args: &mut Vec<Arg>, arg: FfiArg) {
        ffi_args.push(arg);
        let Some(last) = ffi_args.last() else {
            return;
        };
        match last {
            FfiArg::U8(x) => libffi_args.push(Arg::new(x)),
            FfiArg::I8(x) => libffi_args.push(Arg::new(x)),
            FfiArg::U16(x) => libffi_args.push(Arg::new(x)),
            FfiArg::I16(x) => libffi_args.push(Arg::new(x)),
            FfiArg::U32(x) => libffi_args.push(Arg::new(x)),
            FfiArg::I32(x) => libffi_args.push(Arg::new(x)),
            FfiArg::U64(x) => libffi_args.push(Arg::new(x)),
            FfiArg::I64(x) => libffi_args.push(Arg::new(x)),
            FfiArg::F32(x) => libffi_args.push(Arg::new(x)),
            FfiArg::F64(x) => libffi_args.push(Arg::new(x)),
            FfiArg::Bool(x) => libffi_args.push(Arg::new(x)),
            FfiArg::Char(x) => libffi_args.push(Arg::new(x)),
            FfiArg::Ptr(x) => libffi_args.push(Arg::new(x)),
            FfiArg::CString { value, ptr } => {
                let _ = value.as_bytes();
                libffi_args.push(Arg::new(ptr));
            }
            FfiArg::Bytes { value, ptr } => {
                let _ = value.as_slice();
                libffi_args.push(Arg::new(ptr));
            }
            FfiArg::Struct { backing } => {
                let _ = backing.as_slice();
                if let Some(first) = backing.first() {
                    libffi_args.push(Arg::new(first));
                }
            }
        }
    }

    fn struct_field_to_bytes(value: RuntimeValue) -> Option<(Vec<u8>, Type, usize, usize)> {
        match value {
            RuntimeValue::UInt(x) => {
                let size = std::mem::size_of::<u64>();
                Some((x.to_le_bytes().to_vec(), Type::u64(), size, size))
            }
            RuntimeValue::Int(x) => {
                let size = std::mem::size_of::<i64>();
                Some((x.to_le_bytes().to_vec(), Type::i64(), size, size))
            }
            RuntimeValue::Float(x) => {
                let size = std::mem::size_of::<f64>();
                Some((x.to_le_bytes().to_vec(), Type::f64(), size, size))
            }
            RuntimeValue::Bool(x) => Some((vec![x as u8], Type::u8(), 1, 1)),
            RuntimeValue::Char(x) => Some((vec![x as u8], Type::u8(), 1, 1)),
            RuntimeValue::Ptr(id) => {
                let size = std::mem::size_of::<*const c_void>();
                let bytes = (id as usize).to_le_bytes().to_vec();
                Some((bytes, Type::pointer(), size, size))
            }
            _ => None,
        }
    }

    fn pack_struct_arg(env: &mut VM, value: RuntimeValue) -> Option<(Vec<u8>, Type)> {
        match value {
            RuntimeValue::Aggregate(_, data) => {
                let field_count = data.as_ref().0.0.len();
                let mut bytes = Vec::with_capacity(field_count.saturating_mul(8));
                let mut fields = Vec::with_capacity(field_count);
                let mut offset = 0usize;
                let mut max_align = 1usize;

                for (_, field) in data.as_ref().0.0.iter() {
                    let resolved = Self::resolve_value(env, field.clone());
                    let (field_bytes, field_ty, size, align) =
                        Self::struct_field_to_bytes(resolved)?;
                    max_align = max_align.max(align);
                    let padding = (align - (offset % align)) % align;
                    if padding > 0 {
                        bytes.extend(std::iter::repeat(0u8).take(padding));
                        offset += padding;
                    }
                    bytes.extend_from_slice(&field_bytes);
                    offset += size;
                    fields.push(field_ty);
                }

                let tail_padding = (max_align - (offset % max_align)) % max_align;
                if tail_padding > 0 {
                    bytes.extend(std::iter::repeat(0u8).take(tail_padding));
                }

                Some((bytes, Type::structure(fields)))
            }
            _ => None,
        }
    }

    fn pack_aggregate_bytes(value: &RuntimeValue) -> Option<Vec<u8>> {
        fn push_number(bytes: &mut Vec<u8>, value: &RuntimeValue) -> bool {
            match value {
                RuntimeValue::UInt(x) => {
                    bytes.extend_from_slice(&x.to_le_bytes());
                    true
                }
                RuntimeValue::Int(x) => {
                    bytes.extend_from_slice(&x.to_le_bytes());
                    true
                }
                RuntimeValue::Float(x) => {
                    bytes.extend_from_slice(&x.to_le_bytes());
                    true
                }
                RuntimeValue::Bool(x) => {
                    bytes.push(*x as u8);
                    true
                }
                RuntimeValue::Char(x) => {
                    bytes.push(*x as u8);
                    true
                }
                _ => false,
            }
        }

        match value {
            RuntimeValue::Aggregate(_, data) => {
                let field_count = data.as_ref().0.0.len();
                let mut bytes = Vec::with_capacity(field_count.saturating_mul(8));
                for (_, field) in data.as_ref().0.0.iter() {
                    if !push_number(&mut bytes, field) {
                        return None;
                    }
                }
                Some(bytes)
            }
            _ => None,
        }
    }

    pub fn call(
        &self,
        env: &mut VM,
        args: Vec<RuntimeValue>,
    ) -> Result<RuntimeValue, RuntimeError> {
        let mut arg_types = Vec::with_capacity(self.parameters.len());
        let mut ffi_args: Vec<FfiArg> = Vec::with_capacity(self.parameters.len());
        let mut libffi_args: Vec<Arg> = Vec::with_capacity(self.parameters.len());

        if args.len() != self.parameters.len() {
            return Err(RuntimeError::InvalidFunctionCall);
        }

        for (param, value) in self.parameters.iter().zip(args.into_iter()) {
            match &param.data_type {
                x if !matches!(x, ParserInnerType::FfiType(_)) => {
                    match (Self::resolve_value(env, value), x) {
                        (RuntimeValue::Str(x), ParserInnerType::Str) => {
                            arg_types.push(Type::pointer());
                            let value = CString::new(x.as_str())
                                .map_err(|_| RuntimeError::InvalidFunctionCall)?;
                            let ptr = value.as_ptr() as *const c_void;
                            Self::push_arg(
                                &mut ffi_args,
                                &mut libffi_args,
                                FfiArg::CString { value, ptr },
                            );
                        }
                        (RuntimeValue::UInt(x), ParserInnerType::UInt) => {
                            arg_types.push(Type::u64());
                            Self::push_arg(&mut ffi_args, &mut libffi_args, FfiArg::U64(x));
                        }
                        (RuntimeValue::UInt(x), ParserInnerType::Int) => {
                            arg_types.push(Type::i64());
                            Self::push_arg(&mut ffi_args, &mut libffi_args, FfiArg::I64(x as i64));
                        }
                        (RuntimeValue::UInt(x), ParserInnerType::Float) => {
                            arg_types.push(Type::f64());
                            Self::push_arg(&mut ffi_args, &mut libffi_args, FfiArg::F64(x as f64));
                        }
                        (RuntimeValue::Int(x), ParserInnerType::Int) => {
                            arg_types.push(Type::i64());
                            Self::push_arg(&mut ffi_args, &mut libffi_args, FfiArg::I64(x));
                        }
                        (RuntimeValue::Int(x), ParserInnerType::Float) => {
                            arg_types.push(Type::f64());
                            Self::push_arg(&mut ffi_args, &mut libffi_args, FfiArg::F64(x as f64));
                        }
                        (RuntimeValue::Int(x), ParserInnerType::UInt) => {
                            arg_types.push(Type::u64());
                            Self::push_arg(&mut ffi_args, &mut libffi_args, FfiArg::U64(x as u64));
                        }
                        (RuntimeValue::Float(x), ParserInnerType::Float) => {
                            arg_types.push(Type::f64());
                            Self::push_arg(&mut ffi_args, &mut libffi_args, FfiArg::F64(x));
                        }
                        (RuntimeValue::Float(x), ParserInnerType::UInt) => {
                            arg_types.push(Type::u64());
                            Self::push_arg(&mut ffi_args, &mut libffi_args, FfiArg::U64(x as u64));
                        }
                        (RuntimeValue::Float(x), ParserInnerType::Int) => {
                            arg_types.push(Type::i64());
                            Self::push_arg(&mut ffi_args, &mut libffi_args, FfiArg::I64(x as i64));
                        }
                        (RuntimeValue::Char(x), ParserInnerType::Char) => {
                            arg_types.push(Type::u8());
                            Self::push_arg(&mut ffi_args, &mut libffi_args, FfiArg::Char(x as u8));
                        }
                        (RuntimeValue::Bool(x), ParserInnerType::Bool) => {
                            arg_types.push(Type::u8());
                            Self::push_arg(&mut ffi_args, &mut libffi_args, FfiArg::Bool(x as u8));
                        }
                        (RuntimeValue::Bool(x), ParserInnerType::UInt) => {
                            arg_types.push(Type::u64());
                            Self::push_arg(&mut ffi_args, &mut libffi_args, FfiArg::U64(x as u64));
                        }
                        (RuntimeValue::Bool(x), ParserInnerType::Int) => {
                            arg_types.push(Type::i64());
                            Self::push_arg(&mut ffi_args, &mut libffi_args, FfiArg::I64(x as i64));
                        }
                        (RuntimeValue::UInt(x), ParserInnerType::Ptr(_)) => {
                            arg_types.push(Type::pointer());
                            Self::push_arg(
                                &mut ffi_args,
                                &mut libffi_args,
                                FfiArg::Ptr(x as *const c_void),
                            );
                        }
                        (RuntimeValue::Int(x), ParserInnerType::Ptr(_)) => {
                            arg_types.push(Type::pointer());
                            Self::push_arg(
                                &mut ffi_args,
                                &mut libffi_args,
                                FfiArg::Ptr(x as usize as *const c_void),
                            );
                        }
                        (RuntimeValue::Str(x), ParserInnerType::Ptr(_)) => {
                            arg_types.push(Type::pointer());
                            let value = CString::new(x.as_str())
                                .map_err(|_| RuntimeError::InvalidFunctionCall)?;
                            let ptr = value.as_ptr() as *const c_void;
                            Self::push_arg(
                                &mut ffi_args,
                                &mut libffi_args,
                                FfiArg::CString { value, ptr },
                            );
                        }
                        (RuntimeValue::Ptr(id), ParserInnerType::Ptr(_)) => {
                            arg_types.push(Type::pointer());
                            Self::push_arg(
                                &mut ffi_args,
                                &mut libffi_args,
                                FfiArg::Ptr(id as *const c_void),
                            );
                        }
                        (RuntimeValue::List(list), ParserInnerType::Ptr(_)) => {
                            arg_types.push(Type::pointer());
                            let mut bytes = Vec::new();
                            for item in list.as_ref().0.iter() {
                                let item = Self::resolve_value(env, item.clone());
                                match item {
                                    RuntimeValue::UInt(x) => bytes.push(x as u8),
                                    RuntimeValue::Int(x) => bytes.push(x as u8),
                                    RuntimeValue::Float(x) => bytes.push(x as u8),
                                    RuntimeValue::Bool(x) => bytes.push(x as u8),
                                    RuntimeValue::Char(x) => bytes.push(x as u8),
                                    _ => {
                                        return Err(RuntimeError::InvalidFunctionCall);
                                    }
                                }
                            }
                            let ptr = bytes.as_ptr() as *const c_void;
                            Self::push_arg(
                                &mut ffi_args,
                                &mut libffi_args,
                                FfiArg::Bytes { value: bytes, ptr },
                            );
                        }
                        (value, ParserInnerType::Ptr(_)) => {
                            arg_types.push(Type::pointer());
                            if let Some(bytes) =
                                Self::pack_aggregate_bytes(&Self::resolve_value(env, value))
                            {
                                let ptr = bytes.as_ptr() as *const c_void;
                                Self::push_arg(
                                    &mut ffi_args,
                                    &mut libffi_args,
                                    FfiArg::Bytes { value: bytes, ptr },
                                );
                            } else {
                                return Err(RuntimeError::InvalidFunctionCall);
                            }
                        }
                        (value, ParserInnerType::Struct(_))
                        | (value, ParserInnerType::StructWithGenerics { .. }) => {
                            let resolved = Self::resolve_value(env, value);
                            let (bytes, ty) = match Self::pack_struct_arg(env, resolved.clone()) {
                                Some(data) => data,
                                None => {
                                    return Err(RuntimeError::Ffi(format!(
                                        "unsupported struct arg {:?}",
                                        resolved
                                    )));
                                }
                            };
                            arg_types.push(ty.clone());
                            let mut backing = vec![0u64; (bytes.len() + 7) / 8];
                            if !bytes.is_empty() {
                                let raw = backing.as_mut_ptr() as *mut u8;
                                let raw_len = backing.len() * std::mem::size_of::<u64>();
                                let dst = unsafe { std::slice::from_raw_parts_mut(raw, raw_len) };
                                dst[..bytes.len()].copy_from_slice(&bytes);
                            }
                            Self::push_arg(
                                &mut ffi_args,
                                &mut libffi_args,
                                FfiArg::Struct { backing },
                            );
                        }
                        _ => return Err(RuntimeError::InvalidFunctionCall),
                    }
                }
                ParserInnerType::FfiType(x) => {
                    arg_types.push(Self::type_to_libffi_type(param));
                    let value = Self::resolve_value(env, value);
                    let arg = match (x, value) {
                        (
                            ParserFfiInnerType::U8 | ParserFfiInnerType::UChar,
                            RuntimeValue::UInt(x),
                        ) => FfiArg::U8(x as u8),
                        (
                            ParserFfiInnerType::U8 | ParserFfiInnerType::UChar,
                            RuntimeValue::Int(x),
                        ) => FfiArg::U8(x as u8),
                        (
                            ParserFfiInnerType::U8 | ParserFfiInnerType::UChar,
                            RuntimeValue::Float(x),
                        ) => FfiArg::U8(x as u8),
                        (
                            ParserFfiInnerType::I8 | ParserFfiInnerType::SChar,
                            RuntimeValue::Int(x),
                        ) => FfiArg::I8(x as i8),
                        (
                            ParserFfiInnerType::I8 | ParserFfiInnerType::SChar,
                            RuntimeValue::UInt(x),
                        ) => FfiArg::I8(x as i8),
                        (
                            ParserFfiInnerType::I8 | ParserFfiInnerType::SChar,
                            RuntimeValue::Float(x),
                        ) => FfiArg::I8(x as i8),
                        (
                            ParserFfiInnerType::U16 | ParserFfiInnerType::UShort,
                            RuntimeValue::UInt(x),
                        ) => FfiArg::U16(x as u16),
                        (
                            ParserFfiInnerType::U16 | ParserFfiInnerType::UShort,
                            RuntimeValue::Int(x),
                        ) => FfiArg::U16(x as u16),
                        (
                            ParserFfiInnerType::U16 | ParserFfiInnerType::UShort,
                            RuntimeValue::Float(x),
                        ) => FfiArg::U16(x as u16),
                        (
                            ParserFfiInnerType::I16 | ParserFfiInnerType::Short,
                            RuntimeValue::Int(x),
                        ) => FfiArg::I16(x as i16),
                        (
                            ParserFfiInnerType::I16 | ParserFfiInnerType::Short,
                            RuntimeValue::UInt(x),
                        ) => FfiArg::I16(x as i16),
                        (
                            ParserFfiInnerType::I16 | ParserFfiInnerType::Short,
                            RuntimeValue::Float(x),
                        ) => FfiArg::I16(x as i16),
                        (
                            ParserFfiInnerType::U32 | ParserFfiInnerType::UInt,
                            RuntimeValue::UInt(x),
                        ) => FfiArg::U32(x as u32),
                        (
                            ParserFfiInnerType::U32 | ParserFfiInnerType::UInt,
                            RuntimeValue::Int(x),
                        ) => FfiArg::U32(x as u32),
                        (
                            ParserFfiInnerType::U32 | ParserFfiInnerType::UInt,
                            RuntimeValue::Float(x),
                        ) => FfiArg::U32(x as u32),
                        (
                            ParserFfiInnerType::I32 | ParserFfiInnerType::Int,
                            RuntimeValue::Int(x),
                        ) => FfiArg::I32(x as i32),
                        (
                            ParserFfiInnerType::I32 | ParserFfiInnerType::Int,
                            RuntimeValue::UInt(x),
                        ) => FfiArg::I32(x as i32),
                        (
                            ParserFfiInnerType::I32 | ParserFfiInnerType::Int,
                            RuntimeValue::Float(x),
                        ) => FfiArg::I32(x as i32),
                        (
                            ParserFfiInnerType::U64
                            | ParserFfiInnerType::ULong
                            | ParserFfiInnerType::ULongLong,
                            RuntimeValue::UInt(x),
                        ) => FfiArg::U64(x as u64),
                        (
                            ParserFfiInnerType::U64
                            | ParserFfiInnerType::ULong
                            | ParserFfiInnerType::ULongLong,
                            RuntimeValue::Int(x),
                        ) => FfiArg::U64(x as u64),
                        (
                            ParserFfiInnerType::U64
                            | ParserFfiInnerType::ULong
                            | ParserFfiInnerType::ULongLong,
                            RuntimeValue::Float(x),
                        ) => FfiArg::U64(x as u64),
                        (
                            ParserFfiInnerType::I64
                            | ParserFfiInnerType::Long
                            | ParserFfiInnerType::LongLong,
                            RuntimeValue::Int(x),
                        ) => FfiArg::I64(x as i64),
                        (
                            ParserFfiInnerType::I64
                            | ParserFfiInnerType::Long
                            | ParserFfiInnerType::LongLong,
                            RuntimeValue::UInt(x),
                        ) => FfiArg::I64(x as i64),
                        (
                            ParserFfiInnerType::I64
                            | ParserFfiInnerType::Long
                            | ParserFfiInnerType::LongLong,
                            RuntimeValue::Float(x),
                        ) => FfiArg::I64(x as i64),
                        (ParserFfiInnerType::USize, RuntimeValue::UInt(x)) => FfiArg::U64(x as u64),
                        (ParserFfiInnerType::USize, RuntimeValue::Int(x)) => FfiArg::U64(x as u64),
                        (ParserFfiInnerType::USize, RuntimeValue::Float(x)) => {
                            FfiArg::U64(x as u64)
                        }
                        (ParserFfiInnerType::ISize, RuntimeValue::Int(x)) => FfiArg::I64(x as i64),
                        (ParserFfiInnerType::ISize, RuntimeValue::UInt(x)) => FfiArg::I64(x as i64),
                        (ParserFfiInnerType::ISize, RuntimeValue::Float(x)) => {
                            FfiArg::I64(x as i64)
                        }
                        (ParserFfiInnerType::F32, RuntimeValue::Float(x)) => FfiArg::F32(x as f32),
                        (ParserFfiInnerType::F32, RuntimeValue::Int(x)) => FfiArg::F32(x as f32),
                        (ParserFfiInnerType::F32, RuntimeValue::UInt(x)) => FfiArg::F32(x as f32),
                        (
                            ParserFfiInnerType::F64 | ParserFfiInnerType::LongDouble,
                            RuntimeValue::Float(x),
                        ) => FfiArg::F64(x as f64),
                        (
                            ParserFfiInnerType::F64 | ParserFfiInnerType::LongDouble,
                            RuntimeValue::Int(x),
                        ) => FfiArg::F64(x as f64),
                        (
                            ParserFfiInnerType::F64 | ParserFfiInnerType::LongDouble,
                            RuntimeValue::UInt(x),
                        ) => FfiArg::F64(x as f64),
                        (
                            ParserFfiInnerType::U8 | ParserFfiInnerType::UChar,
                            RuntimeValue::Bool(x),
                        ) => FfiArg::U8(x as u8),
                        (
                            ParserFfiInnerType::I8 | ParserFfiInnerType::SChar,
                            RuntimeValue::Bool(x),
                        ) => FfiArg::I8(x as i8),
                        (
                            ParserFfiInnerType::U8 | ParserFfiInnerType::UChar,
                            RuntimeValue::Char(x),
                        ) => FfiArg::U8(x as u8),
                        (
                            ParserFfiInnerType::I8 | ParserFfiInnerType::SChar,
                            RuntimeValue::Char(x),
                        ) => FfiArg::I8(x as i8),
                        _ => return Err(RuntimeError::InvalidFunctionCall),
                    };
                    Self::push_arg(&mut ffi_args, &mut libffi_args, arg);
                }
                _ => unreachable!(),
            }
        }

        let cif = Cif::new(arg_types, Self::type_to_libffi_type(&self.return_type));

        let symbol = unsafe {
            self.handle
                .get::<*const c_void>(self.symbol.as_bytes())
                .map_err(|_| RuntimeError::InvalidFunctionCall)?
        };

        let code = CodePtr::from_ptr(*symbol as *mut c_void);

        unsafe {
            match &self.return_type.data_type {
                x if !matches!(x, ParserInnerType::FfiType(_)) => match x {
                    ParserInnerType::Float => {
                        Ok(RuntimeValue::Float(cif.call(code, &mut libffi_args)))
                    }
                    ParserInnerType::UInt => {
                        Ok(RuntimeValue::UInt(cif.call(code, &mut libffi_args)))
                    }
                    ParserInnerType::Int => Ok(RuntimeValue::Int(cif.call(code, &mut libffi_args))),
                    ParserInnerType::Bool => {
                        let res: u8 = cif.call(code, &mut libffi_args);
                        Ok(RuntimeValue::Bool(res != 0))
                    }
                    ParserInnerType::Null => {
                        let _: () = cif.call(code, &mut libffi_args);
                        Ok(RuntimeValue::Null)
                    }
                    ParserInnerType::Char => {
                        let res: u8 = cif.call(code, &mut libffi_args);
                        Ok(RuntimeValue::Char(res as char))
                    }
                    ParserInnerType::Str => {
                        let res: *const c_char = cif.call(code, &mut libffi_args);
                        if res.is_null() {
                            Ok(RuntimeValue::Str(std::sync::Arc::new(String::new())))
                        } else {
                            let c_str = CStr::from_ptr(res);
                            Ok(RuntimeValue::Str(std::sync::Arc::new(
                                c_str.to_string_lossy().to_string(),
                            )))
                        }
                    }
                    ParserInnerType::Ptr(_) => {
                        let res: *const c_void = cif.call(code, &mut libffi_args);
                        Ok(RuntimeValue::UInt(res as u64))
                    }
                    _ => Err(RuntimeError::InvalidFunctionCall),
                },
                ParserInnerType::FfiType(x) => match x {
                    ParserFfiInnerType::F32 => {
                        let res: f32 = cif.call(code, &mut libffi_args);
                        Ok(RuntimeValue::Float(res as f64))
                    }
                    ParserFfiInnerType::F64 | ParserFfiInnerType::LongDouble => {
                        Ok(RuntimeValue::Float(cif.call(code, &mut libffi_args)))
                    }
                    ParserFfiInnerType::U8
                    | ParserFfiInnerType::U16
                    | ParserFfiInnerType::U32
                    | ParserFfiInnerType::U64
                    | ParserFfiInnerType::USize
                    | ParserFfiInnerType::UInt
                    | ParserFfiInnerType::UShort
                    | ParserFfiInnerType::ULong
                    | ParserFfiInnerType::ULongLong
                    | ParserFfiInnerType::UChar => {
                        let res: u64 = cif.call(code, &mut libffi_args);
                        Ok(RuntimeValue::UInt(res))
                    }
                    ParserFfiInnerType::I8
                    | ParserFfiInnerType::I16
                    | ParserFfiInnerType::I32
                    | ParserFfiInnerType::I64
                    | ParserFfiInnerType::ISize
                    | ParserFfiInnerType::Int
                    | ParserFfiInnerType::Short
                    | ParserFfiInnerType::Long
                    | ParserFfiInnerType::LongLong
                    | ParserFfiInnerType::SChar => {
                        let res: i64 = cif.call(code, &mut libffi_args);
                        Ok(RuntimeValue::Int(res))
                    }
                },
                _ => unreachable!(),
            }
        }
    }
}

pub mod translator;
pub mod values;
use calibre_mir::ast::{MiddleNode, MiddleNodeType};
use calibre_mir::environment::{MiddleEnvironment, MiddleObject};
use calibre_parser::ast::{Node, NodeType, ParserDataType, VarType};
use cranelift::prelude::isa::CallConv;
use cranelift::prelude::*;
use cranelift_module::{DataDescription, Linkage, Module};
use cranelift_object::object::read::{File as ObjectFile, RelocationKind, RelocationTarget};
use cranelift_object::object::{Object, ObjectSection, ObjectSymbol, SectionKind};
use cranelift_object::{ObjectBuilder, ObjectModule};
use libc::{MAP_ANON, MAP_PRIVATE, PROT_EXEC, PROT_READ, PROT_WRITE, mmap, mprotect, size_t};
use std::{collections::HashMap, error::Error, ffi::CString, ptr};

use crate::translator::{FunctionTranslator, layout::create_layout};

pub struct Compiler {
    /// The function builder context, which is reused across multiple
    /// FunctionBuilder instances.
    builder_context: FunctionBuilderContext,

    /// The main Cranelift context, which holds the state for codegen. Cranelift
    /// separates this from `Module` to allow for parallel compilation, with a
    /// context per thread, though this isn't in the simple demo here.
    ctx: codegen::Context,

    /// The data description, which is to data objects what `ctx` is to functions.
    data_description: DataDescription,
}

impl Default for Compiler {
    fn default() -> Self {
        Self {
            builder_context: FunctionBuilderContext::new(),
            ctx: codegen::Context::new(),
            data_description: DataDescription::new(),
        }
    }
}

impl Compiler {
    pub fn compile(
        &mut self,
        program: MiddleNode,
        env: MiddleEnvironment,
    ) -> Result<*const u8, Box<dyn Error>> {
        let mut main = None;

        if let MiddleNodeType::ScopeDeclaration { body, is_temp: _ } = program.node_type.clone() {
            for var in body {
                if let MiddleNodeType::VariableDeclaration {
                    var_type: VarType::Constant,
                    identifier,
                    value,
                    ..
                } = var.node_type.clone()
                {
                    match &value.node_type {
                        MiddleNodeType::FunctionDeclaration { .. } => {
                            let val = self.compile_const_fn(
                                if identifier.to_string() == "main" {
                                    "_start".to_string()
                                } else {
                                    identifier.to_string()
                                },
                                *value,
                                &env.objects,
                            )?;
                            if identifier.to_string() == "main" {
                                main = Some(val);
                            }
                        }
                        _ => todo!("Implement constant variables in the global scope"),
                    }
                }
            }
        }

        Ok(main.unwrap())
    }

    pub fn compile_const_fn(
        &mut self,
        identifier: String,
        value: MiddleNode,
        objects: &std::collections::HashMap<String, MiddleObject>,
    ) -> Result<*const u8, Box<dyn Error>> {
        if let MiddleNodeType::FunctionDeclaration {
            parameters,
            body,
            return_type,
            is_async: _,
        } = value.node_type
        {
            let mut flag_builder = settings::builder();
            flag_builder.set("use_colocated_libcalls", "false").unwrap();
            flag_builder.set("is_pic", "false").unwrap();
            let isa_builder = cranelift_native::builder().unwrap();
            let isa = isa_builder
                .finish(settings::Flags::new(flag_builder))
                .unwrap();

            let builder = ObjectBuilder::new(
                isa,
                "calibre".to_string(),
                cranelift_module::default_libcall_names(),
            )
            .unwrap();

            let mut module = ObjectModule::new(builder);
            let mut ctx = module.make_context();

            self.translate(
                parameters
                    .into_iter()
                    .map(|x| (x.0.to_string(), x.1.clone()))
                    .collect(),
                return_type,
                *body,
                &mut module,
                &mut ctx,
                objects,
            )?;

            let id = module
                .declare_function(&identifier, Linkage::Export, &ctx.func.signature)
                .map_err(|e| e.to_string())?;

            module
                .define_function(id, &mut ctx)
                .map_err(|e| e.to_string())?;
            module.clear_context(&mut ctx);

            let product = module.finish();
            let obj_bytes = product.emit().map_err(|e| e.to_string())?;

            let addr = unsafe { Self::load_object_and_resolve(&obj_bytes, &identifier)? };
            Ok(addr as *const u8)
        } else {
            todo!()
        }
    }

    fn translate(
        &mut self,
        params: Vec<(String, ParserDataType)>,
        return_type: Option<ParserDataType>,
        node: MiddleNode,
        module: &mut ObjectModule,
        ctx: &mut codegen::Context,
        objects: &std::collections::HashMap<String, MiddleObject>,
    ) -> Result<(), Box<dyn Error>> {
        let ptr = module.target_config().pointer_type();
        let types = crate::translator::Types::new(ptr);
        create_layout(ptr.bits());

        for p in &params {
            ctx.func
                .signature
                .params
                .push(AbiParam::new(types.get_type_from_parser_type(&p.1)));
        }

        if let Some(t) = return_type {
            ctx.func
                .signature
                .returns
                .push(AbiParam::new(types.get_type_from_parser_type(&t)));
        }

        ctx.func.signature.call_conv = CallConv::SystemV;

        // Create the builder to build a function.
        let mut builder = FunctionBuilder::new(&mut ctx.func, &mut self.builder_context);

        // Create the entry block, to start emitting code in.
        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);

        // Tell the builder to emit code in this block.
        builder.switch_to_block(entry_block);

        // And, tell the builder that this block will have no further
        // predecessors. Since it's the entry block, it won't have any
        // predecessors.
        builder.seal_block(entry_block);

        // Now translate the statements of the function body.
        let mut trans = FunctionTranslator {
            variables: HashMap::new(),
            types,
            builder,
            module,
            description: &mut self.data_description,
            objects,
            stop: None,
        };
        let value = trans.translate(node);

        // Emit the return instruction.
        trans.builder.ins().return_(&[value.value]);

        // Tell the builder we're done with this function.
        trans.builder.finalize();
        Ok(())
    }

    unsafe fn load_object_and_resolve(bytes: &[u8], name: &str) -> Result<usize, Box<dyn Error>> {
        let file = ObjectFile::parse(bytes).map_err(|e| format!("object parse error: {}", e))?;

        let mut section_bases: HashMap<usize, usize> = HashMap::new();

        let mut text_sections: Vec<(usize, usize)> = Vec::new();
        for section in file.sections() {
            let kind = section.kind();
            if kind == SectionKind::Text || kind == SectionKind::Data {
                let data = section
                    .data()
                    .map_err(|e| format!("section read error: {}", e))?;
                let size = data.len();
                if size == 0 {
                    section_bases.insert(section.index().0 as usize, 0);
                    continue;
                }

                let prot = if kind == SectionKind::Text {
                    PROT_READ | PROT_WRITE | PROT_EXEC
                } else {
                    PROT_READ | PROT_WRITE
                };

                unsafe {
                    let ptr = mmap(
                        ptr::null_mut(),
                        size as size_t,
                        prot,
                        MAP_PRIVATE | MAP_ANON,
                        -1,
                        0,
                    );
                    if ptr == libc::MAP_FAILED {
                        return Err("mmap failed".into());
                    }
                    let base = ptr as usize;
                    std::ptr::copy_nonoverlapping(data.as_ptr(), base as *mut u8, size);

                    if kind == SectionKind::Text {
                        text_sections.push((base, size));
                    }

                    section_bases.insert(section.index().0 as usize, base);
                }
            }
        }

        for section in file.sections() {
            for (offset, reloc) in section.relocations() {
                let target_addr = match reloc.target() {
                    RelocationTarget::Symbol(sym_index) => {
                        let sym = file
                            .symbol_by_index(sym_index)
                            .map_err(|e| format!("symbol lookup error: {}", e))?;
                        let sname = sym
                            .name()
                            .map_err(|e| format!("symbol name error: {}", e))?
                            .to_string();
                        if let Some(sec_idx) = sym.section_index() {
                            let sec_base = *section_bases
                                .get(&(sec_idx.0 as usize))
                                .ok_or("missing section base")?;
                            sec_base + (sym.address() as usize)
                        } else {
                            unsafe {
                                let c = CString::new(sname).unwrap();
                                let ptr = libc::dlsym(libc::RTLD_DEFAULT, c.as_ptr());
                                if ptr.is_null() {
                                    return Err("external symbol not found".into());
                                }
                                ptr as usize
                            }
                        }
                    }
                    RelocationTarget::Section(sec_idx) => *section_bases
                        .get(&(sec_idx.0 as usize))
                        .ok_or("missing section base")?,
                    _ => return Err("unsupported relocation target".into()),
                };

                let place_base = *section_bases
                    .get(&(section.index().0 as usize))
                    .ok_or("missing place section base")?;
                let place = place_base + (offset as usize);

                match reloc.kind() {
                    RelocationKind::Relative => {
                        let addend = reloc.addend();
                        let size_bits = reloc.size();
                        if size_bits == 32 {
                            let rel =
                                (target_addr as isize + addend as isize) - ((place) as isize + 4);
                            let dst = place as *mut i32;
                            unsafe {
                                std::ptr::write_unaligned(dst, rel as i32);
                            }
                        } else {
                            return Err("unsupported relative relocation size".into());
                        }
                    }
                    _ => {
                        let addend = reloc.addend();
                        let size_bits = reloc.size();
                        if size_bits == 64 {
                            let val = (target_addr as i64).wrapping_add(addend) as u64;
                            let dst = place as *mut u64;
                            unsafe {
                                std::ptr::write_unaligned(dst, val);
                            }
                        } else if size_bits == 32 {
                            let val = (target_addr as i64).wrapping_add(addend) as u32;
                            let dst = place as *mut u32;
                            unsafe {
                                std::ptr::write_unaligned(dst, val);
                            }
                        } else {
                            return Err("unsupported absolute relocation size".into());
                        }
                    }
                }
            }
        }

        for (base, size) in text_sections {
            unsafe {
                let ptr = base as *mut libc::c_void;
                mprotect(ptr, size as size_t, PROT_READ | PROT_EXEC);
            }
        }

        for symbol in file.symbols() {
            if let Ok(n) = symbol.name() {
                if n == name {
                    if let Some(sec_idx) = symbol.section_index() {
                        let base = *section_bases
                            .get(&(sec_idx.0 as usize))
                            .ok_or("missing section base")?;
                        let addr = base + (symbol.address() as usize);
                        return Ok(addr);
                    }
                }
            }
        }

        Err("symbol not found after loading".into())
    }
}

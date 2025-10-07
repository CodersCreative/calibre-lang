use std::{cell::OnceCell, collections::HashMap, sync::Mutex};

use crate::values::RuntimeType;

#[derive(Debug)]
pub(crate) struct TyLayouts {
    /// the pointer bit widths that all these type sizes were calculated with
    pointer_bit_width: u32,
    sizes: HashMap<RuntimeType, u32>,
    alignments: HashMap<RuntimeType, u32>,
    struct_layouts: HashMap<RuntimeType, StructLayout>,
    enum_layouts: HashMap<RuntimeType, EnumLayout>,
}

static LAYOUTS: Mutex<OnceCell<TyLayouts>> = Mutex::new(OnceCell::new());

pub trait GetLayoutInfo {
    fn size(&self) -> u32;
    fn align(&self) -> u32;
    fn stride(&self) -> u32;
    fn align_shift(&self) -> u8;
    fn struct_layout(&self) -> Option<StructLayout>;
    fn enum_layout(&self) -> Option<EnumLayout>;
}

impl GetLayoutInfo for RuntimeType {
    fn size(&self) -> u32 {
        // we could do `get_or_init` here, but the index would panic anyways
        LAYOUTS.lock().unwrap().get().unwrap().sizes[self]
    }

    fn align(&self) -> u32 {
        LAYOUTS.lock().unwrap().get().unwrap().alignments[self]
    }

    /// The amount of left shifts needed to get the right alignment.
    /// This is needed for cranelift.
    ///
    /// `1 << align_shift == align`
    fn align_shift(&self) -> u8 {
        let align = LAYOUTS.lock().unwrap().get().unwrap().alignments[self];
        assert!(align.is_power_of_two());
        // trailing_zeros(n) == log2(n) if and only if n is a power of two
        align.trailing_zeros() as u8
    }

    fn stride(&self) -> u32 {
        let layouts = LAYOUTS.lock().unwrap();
        let layouts = layouts.get().unwrap();
        let mask = layouts.alignments[self] - 1;
        (layouts.sizes[self] + mask) & !mask
    }

    fn struct_layout(&self) -> Option<StructLayout> {
        let layouts = LAYOUTS.lock().ok()?;
        let layouts = layouts.get()?;
        layouts.struct_layouts.get(self).cloned()
    }

    fn enum_layout(&self) -> Option<EnumLayout> {
        let layouts = LAYOUTS.lock().ok()?;
        let layouts = layouts.get()?;
        layouts.enum_layouts.get(self).cloned()
    }
}

/// Calcuates size, alignment, stride, and field offsets of types.
///
/// If called multiple times, new types will be calculated without discarding old results
///
/// Old results will only be dropped if you try to calculate the layout using a different pointer
/// width.
pub fn calc_layouts(tys: impl Iterator<Item = RuntimeType>, pointer_bit_width: u32) {
    let init = || TyLayouts {
        pointer_bit_width,
        sizes: HashMap::default(),
        alignments: HashMap::default(),
        struct_layouts: HashMap::default(),
        enum_layouts: HashMap::default(),
    };

    {
        let layouts = LAYOUTS.lock().unwrap();
        let layout = layouts.get_or_init(init);
        if layout.pointer_bit_width != pointer_bit_width {
            layouts.set(init()).unwrap();
        }
    }

    for ty in tys {
        calc_single(ty, pointer_bit_width);
    }

    {
        let mut layouts = LAYOUTS.lock().unwrap();
        let layouts = layouts.get_mut().unwrap();
        layouts.sizes.shrink_to_fit();
        layouts.alignments.shrink_to_fit();
        layouts.struct_layouts.shrink_to_fit();
        layouts.enum_layouts.shrink_to_fit();
    }
}

fn calc_single(ty: RuntimeType, pointer_bit_width: u32) {
    {
        let layouts = LAYOUTS.lock().unwrap();
        let layouts = layouts.get().unwrap();
        if layouts.sizes.contains_key(&ty) {
            return;
        }
    }

    let size = match ty {
        RuntimeType::Named(_) => 0,
        RuntimeType::Int | RuntimeType::Float => 64 / 8,
        RuntimeType::Range => 64 / 8 * 2,

        RuntimeType::Bool | RuntimeType::Char => 1,
        RuntimeType::Str => pointer_bit_width / 8,
        RuntimeType::Tuple(data) => {
            let mut size = 0;
            for d in data {
                size += d.stride();
                calc_single(d, pointer_bit_width);
            }
            size
        }
        RuntimeType::List(size, Some(t)) => {
            calc_single(*t, pointer_bit_width);
            t.stride() * size as u32
        }
        RuntimeType::List(_, None) => pointer_bit_width / 8,
        RuntimeType::Function { .. } => pointer_bit_width / 8,
        RuntimeType::Option(t) => {
            calc_single(*t, pointer_bit_width);

            let payload_size = t.size();
            let payload_align = t.align();

            let enum_layout = EnumLayout {
                // +1 for the discriminant
                size: payload_size + 1,
                align: payload_align,
                discriminant_offset: payload_size,
            };
            let size = enum_layout.size;

            {
                let mut layouts = LAYOUTS.lock().unwrap();
                layouts
                    .get_mut()
                    .unwrap()
                    .enum_layouts
                    .insert(ty, enum_layout);
            }

            size
        }
        RuntimeType::Result(error, ok) => {
            calc_single(*error, pointer_bit_width);
            calc_single(*ok, pointer_bit_width);

            let inner_size = error.size().max(ok.size());
            let inner_align = error.align().max(ok.align());

            let enum_layout = EnumLayout {
                // +1 for the discriminant
                size: inner_size + 1,
                align: inner_align,
                discriminant_offset: inner_size,
            };
            let size = enum_layout.size;

            {
                let mut layouts = LAYOUTS.lock().unwrap();
                layouts
                    .get_mut()
                    .unwrap()
                    .enum_layouts
                    .insert(ty, enum_layout);
            }

            size
        }
        RuntimeType::Dynamic => {
            let mut current_offset = 0;

            let typeid_size = 32 / 8;
            // let typeid_align = typeid_size.min(8);

            let rawptr_size = pointer_bit_width / 8;
            let rawptr_align = rawptr_size.min(8);

            current_offset += typeid_size;
            current_offset += padding_needed_for(current_offset, rawptr_align);

            current_offset += rawptr_size;

            current_offset
        }
        RuntimeType::Ref { .. } => pointer_bit_width / 8,
        RuntimeType::Struct { members, .. } => {
            let members = members.iter().map(|member| member.ty).collect::<Vec<_>>();
            for member_ty in &members {
                calc_single(*member_ty, pointer_bit_width);
            }
            let struct_layout = StructLayout::new(members);
            let size = struct_layout.size;

            {
                let mut layouts = LAYOUTS.lock().unwrap();
                layouts
                    .get_mut()
                    .unwrap()
                    .struct_layouts
                    .insert(ty, struct_layout);
            }

            size
        }
        _ => panic!(),
    };

    let align = match ty {
        RuntimeType::Named(_) => 1,
        RuntimeType::Int | RuntimeType::Float => size.min(8),
        RuntimeType::Range => size.min(16),
        RuntimeType::Bool | RuntimeType::Char => 1, // bools and chars are u8's
        RuntimeType::Str
        | RuntimeType::Ref { .. }
        | RuntimeType::Function { .. }
        | RuntimeType::List(_, None) => size.min(8),
        RuntimeType::Tuple(types) => {
            let mut val = 0;
            for t in types {
                val = val.max(t.align())
            }
            val
        }
        RuntimeType::List(_, Some(ty)) => ty.align(),
        RuntimeType::Struct { .. } => ty.struct_layout().unwrap().align,
        RuntimeType::Option(t) => t.align(),
        RuntimeType::Result(error, ok) => error.align().max(ok.align()),
        RuntimeType::Dynamic => {
            let typeid_size = 32 / 8;
            let typeid_align = typeid_size.min(8);

            let rawptr_size = pointer_bit_width / 8;
            let rawptr_align = rawptr_size.min(8);

            typeid_align.max(rawptr_align)
        }
        _ => panic!(),
    };

    assert!(align <= 8, "align is {align} (> 8)");

    {
        let mut layouts = LAYOUTS.lock().unwrap();
        let layouts = layouts.get_mut().unwrap();
        layouts.sizes.insert(ty, size);
        layouts.alignments.insert(ty, align);
    }
}

/// Used for enums and optionals
#[derive(Debug, Clone, Copy)]
pub(crate) struct EnumLayout {
    size: u32,
    align: u32,
    discriminant_offset: u32,
}

impl EnumLayout {
    pub(crate) fn discriminant_offset(&self) -> u32 {
        self.discriminant_offset
    }
}

#[derive(Debug, Clone)]
pub(crate) struct StructLayout {
    size: u32,
    align: u32,
    offsets: Vec<u32>,
}

/// checks if the offset is a multiple of the alignment
///
/// if not, returns the amount of bytes needed
/// to make it a valid offset
pub(crate) fn padding_needed_for(offset: u32, align: u32) -> u32 {
    let misalign = offset % align;
    if misalign > 0 {
        // the amount needed to round up to the next proper offset
        align - misalign
    } else {
        0
    }
}

impl StructLayout {
    pub(crate) fn new(fields: Vec<RuntimeType>) -> Self {
        let mut offsets = Vec::with_capacity(fields.len());
        let mut max_align = 1;
        let mut current_offset = 0;

        for field in fields {
            let field_align = field.align();
            if field_align > max_align {
                max_align = field_align;
            }

            current_offset += padding_needed_for(current_offset, field_align);

            offsets.push(current_offset);

            current_offset += field.size();
        }

        Self {
            size: current_offset,
            align: max_align,
            offsets,
        }
    }

    pub(crate) fn offsets(&self) -> &[u32] {
        &self.offsets
    }
}

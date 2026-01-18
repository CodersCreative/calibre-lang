use std::{cell::OnceCell, collections::HashMap, sync::Mutex};

use crate::values::RuntimeType;

#[derive(Debug)]
pub(crate) struct TyLayouts {
    /// the pointer bit widths that all these type sizes were calculated with
    pub pointer_bit_width: u32,
    pub sizes: HashMap<RuntimeType, u32>,
    pub alignments: HashMap<RuntimeType, u32>,
    pub aggregate_layouts: HashMap<RuntimeType, AggregateLayout>,
    pub enum_layouts: HashMap<RuntimeType, EnumLayout>,
}

static LAYOUTS: Mutex<OnceCell<TyLayouts>> = Mutex::new(OnceCell::new());

pub trait GetLayoutInfo {
    fn size(&self) -> u32;
    fn align(&self) -> u32;
    fn stride(&self) -> u32;
    fn align_shift(&self) -> u8;
    fn aggregate_layout(&self) -> Option<AggregateLayout>;
    fn enum_layout(&self) -> Option<EnumLayout>;
    fn ensure_layout(&self);
}

impl GetLayoutInfo for RuntimeType {
    fn size(&self) -> u32 {
        self.ensure_layout();
        // we could do `get_or_init` here, but the index would panic anyways
        LAYOUTS.lock().unwrap().get().unwrap().sizes[self]
    }

    fn align(&self) -> u32 {
        self.ensure_layout();
        LAYOUTS.lock().unwrap().get().unwrap().alignments[self]
    }

    /// The amount of left shifts needed to get the right alignment.
    /// This is needed for cranelift.
    ///
    /// `1 << align_shift == align`
    fn align_shift(&self) -> u8 {
        self.ensure_layout();
        let align = LAYOUTS.lock().unwrap().get().unwrap().alignments[self];
        assert!(align.is_power_of_two());
        // trailing_zeros(n) == log2(n) if and only if n is a power of two
        align.trailing_zeros() as u8
    }

    fn stride(&self) -> u32 {
        self.ensure_layout();
        let layouts = LAYOUTS.lock().unwrap();
        let layouts = layouts.get().unwrap();
        let mask = layouts.alignments[self] - 1;
        (layouts.sizes[self] + mask) & !mask
    }

    fn aggregate_layout(&self) -> Option<AggregateLayout> {
        self.ensure_layout();
        let layouts = LAYOUTS.lock().ok()?;
        let layouts = layouts.get()?;
        layouts.aggregate_layouts.get(self).cloned()
    }

    fn enum_layout(&self) -> Option<EnumLayout> {
        self.ensure_layout();
        let layouts = LAYOUTS.lock().ok()?;
        let layouts = layouts.get()?;
        layouts.enum_layouts.get(self).cloned()
    }

    fn ensure_layout(&self) {
        if !LAYOUTS
            .lock()
            .unwrap()
            .get()
            .unwrap()
            .sizes
            .contains_key(self)
        {
            calc_single(self);
        }
    }
}

pub fn create_layout(pointer_bit_width: u32) {
    let init = || TyLayouts {
        pointer_bit_width,
        sizes: HashMap::default(),
        alignments: HashMap::default(),
        aggregate_layouts: HashMap::default(),
        enum_layouts: HashMap::default(),
    };

    {
        let layouts = LAYOUTS.lock().unwrap();
        let layout = layouts.get_or_init(init);
        if layout.pointer_bit_width != pointer_bit_width {
            layouts.set(init()).unwrap();
        }
    }
}

fn calc_single(ty: &RuntimeType) {
    let pointer_bit_width = {
        let layouts = LAYOUTS.lock().unwrap();
        let layouts = layouts.get().unwrap();
        if layouts.sizes.contains_key(&ty) {
            return;
        }
        layouts.pointer_bit_width.clone()
    };

    let size = match &ty {
        RuntimeType::Named(_) => 0,
        RuntimeType::Int | RuntimeType::Float => 64 / 8,
        RuntimeType::Range => 64 / 8 * 2,

        RuntimeType::Bool | RuntimeType::Char => 1,
        RuntimeType::Str => pointer_bit_width / 8,
        RuntimeType::List(t) => {
            calc_single(&**t);
            t.stride() * 32 as u32
        }
        RuntimeType::Function { .. } => pointer_bit_width / 8,
        RuntimeType::Option(t) => {
            calc_single(&**t);

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
                    .insert(ty.clone(), enum_layout);
            }

            size
        }
        RuntimeType::Result(error, ok) => {
            calc_single(&**error);
            calc_single(&**ok);

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
                    .insert(ty.clone(), enum_layout);
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
        RuntimeType::Aggregate { members, .. } => {
            let members = members
                .iter()
                .map(|member| member.ty.clone())
                .collect::<Vec<_>>();
            for member_ty in &members {
                calc_single(member_ty);
            }
            let struct_layout = AggregateLayout::new(members);
            let size = struct_layout.size;

            {
                let mut layouts = LAYOUTS.lock().unwrap();
                layouts
                    .get_mut()
                    .unwrap()
                    .aggregate_layouts
                    .insert(ty.clone(), struct_layout);
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
        RuntimeType::Str | RuntimeType::Ref { .. } | RuntimeType::Function { .. } => size.min(8),
        RuntimeType::List(ty) => ty.align(),
        RuntimeType::Aggregate { .. } => {
            LAYOUTS
                .lock()
                .unwrap()
                .get()
                .unwrap()
                .aggregate_layouts
                .get(ty)
                .unwrap()
                .align
        }
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
        layouts.sizes.insert(ty.clone(), size);
        layouts.alignments.insert(ty.clone(), align);
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
pub(crate) struct AggregateLayout {
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

impl AggregateLayout {
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

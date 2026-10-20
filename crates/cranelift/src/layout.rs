use std::collections::VecDeque;

use calibre_lir::{
    MirDataType,
    environment::{LirRegistry, LirType},
};
use rustc_hash::FxHashMap;

const PRIMITIVE_TYPES: [MirDataType; 9] = [
    MirDataType::Int,
    MirDataType::UInt,
    MirDataType::Byte,
    MirDataType::Float,
    MirDataType::Bool,
    MirDataType::Char,
    MirDataType::Str,
    MirDataType::Range,
    MirDataType::Dynamic,
];

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Layouts {
    pub pointer_bit_width: u32,
    pub layouts: FxHashMap<MirDataType, Layout>,
}

impl Default for Layouts {
    fn default() -> Self {
        Self::new(None, 64)
    }
}

impl Layouts {
    pub fn new(registry: Option<&LirRegistry>, pointer_bit_width: u32) -> Self {
        let mut layouts = Self {
            pointer_bit_width,
            layouts: FxHashMap::default(),
        };

        for data_type in &PRIMITIVE_TYPES {
            layouts.ensure_layout(data_type);
        }

        if let Some(registry) = registry {
            let mut work_list = VecDeque::new();

            for (key, lir_type) in &registry.types {
                if let Some(layout) = Layout::from_lir_type(&layouts, lir_type) {
                    layouts.layouts.insert(
                        MirDataType::Struct {
                            identifier: key.clone(),
                            generic_types: Vec::new(),
                        },
                        layout,
                    );
                } else {
                    work_list.push_back((key, lir_type));
                }
            }

            let mut depth = 64;
            while let Some((key, lir_type)) = work_list.pop_front()
                && depth > 0
            {
                if let Some(layout) = Layout::from_lir_type(&layouts, lir_type) {
                    layouts.layouts.insert(
                        MirDataType::Struct {
                            identifier: key.clone(),
                            generic_types: Vec::new(),
                        },
                        layout,
                    );
                } else {
                    work_list.push_back((key, lir_type));
                }

                depth -= 1;
            }

            if !work_list.is_empty() {
                eprintln!("Could not resolve layouts for {} types", work_list.len());
            }
        }

        layouts
    }

    #[inline(always)]
    pub fn pointer_bytes(&self) -> u32 {
        self.pointer_bit_width / 8
    }

    #[inline(always)]
    pub fn get_layout(&self, data_type: &MirDataType) -> Option<&Layout> {
        self.layouts.get(data_type)
    }

    pub fn ensure_layout(&mut self, data_type: &MirDataType) {
        if self.layouts.contains_key(data_type) {
            return;
        }

        let pointer_bytes = self.pointer_bytes();

        let layout = match data_type {
            MirDataType::Never | MirDataType::Null => Layout {
                size: 0,
                align: 1,
                offsets: Vec::new(),
            },
            MirDataType::Bool | MirDataType::Char | MirDataType::Byte => Layout {
                size: 1,
                align: 1,
                offsets: Vec::new(),
            },
            MirDataType::Int | MirDataType::UInt | MirDataType::Float => Layout {
                size: 8,
                align: 8,
                offsets: Vec::new(),
            },
            MirDataType::Range => Layout {
                size: 16,
                align: 8,
                offsets: vec![0, 8],
            },
            MirDataType::Ref(_, _)
            | MirDataType::Ptr(_)
            | MirDataType::Str
            | MirDataType::Function { .. }
            | MirDataType::NativeFunction { .. } => Layout {
                size: pointer_bytes,
                align: pointer_bytes.min(8),
                offsets: Vec::new(),
            },
            // I need to make sure to basically box dyns
            // Keeps : type id + pointer
            MirDataType::Dynamic => {
                let type_id_size = 4;

                let ptr_align = pointer_bytes.min(8);
                let align = ptr_align.max(4);

                let mut current_offset = type_id_size;
                current_offset += padding_needed_for(current_offset, ptr_align);

                let offset = current_offset;
                current_offset += pointer_bytes;
                current_offset += padding_needed_for(current_offset, align);

                Layout {
                    size: current_offset,
                    align,
                    offsets: vec![0, offset],
                }
            }
            // Keeps : pointer + len + capacity
            MirDataType::List(_) => Layout {
                size: pointer_bytes * 3,
                align: pointer_bytes.min(8),
                offsets: vec![0, pointer_bytes, pointer_bytes * 2],
            },
            MirDataType::Tuple(elements) => {
                let mut offsets = Vec::with_capacity(elements.len());
                let mut align = 1;
                let mut current_offset = 0;

                for elem in elements {
                    self.ensure_layout(elem);
                    let Some(layout) = self.get_layout(elem) else {
                        return;
                    };

                    if layout.align > align {
                        align = layout.align;
                    }

                    current_offset += padding_needed_for(current_offset, layout.align);
                    offsets.push(current_offset);
                    current_offset += layout.size;
                }

                current_offset += padding_needed_for(current_offset, align);

                Layout {
                    size: current_offset,
                    align,
                    offsets,
                }
            }
            // Keeps : discriminant tag + payload
            MirDataType::Option(inner) => {
                self.ensure_layout(inner);
                let Some(inner_layout) = self.get_layout(inner) else {
                    return;
                };

                let tag_size = 4;
                let align = inner_layout.align.max(tag_size);

                let payload_offset =
                    tag_size + padding_needed_for(tag_size, inner_layout.align.max(1));

                let mut size = payload_offset + inner_layout.size;
                size += padding_needed_for(size, align);

                Layout {
                    size,
                    align,
                    offsets: vec![0, payload_offset],
                }
            }
            // Keeps : discriminant tag + payload
            MirDataType::Result { ok, err } => {
                self.ensure_layout(ok);
                self.ensure_layout(err);

                let Some(ok_layout) = self.get_layout(ok) else {
                    return;
                };
                let Some(err_layout) = self.get_layout(err) else {
                    return;
                };

                let payload_size = ok_layout.size.max(err_layout.size);
                let payload_align = ok_layout.align.max(err_layout.align);

                let tag_size = 4;
                let align = payload_align.max(tag_size);

                let payload_offset = tag_size + padding_needed_for(tag_size, payload_align.max(1));

                let mut size = payload_offset + payload_size;
                size += padding_needed_for(size, align);

                Layout {
                    size,
                    align,
                    offsets: vec![0, payload_offset],
                }
            }
            MirDataType::FfiType(_) | MirDataType::Big | MirDataType::Host => Layout {
                size: pointer_bytes,
                align: pointer_bytes.min(8),
                offsets: Vec::new(),
            },
            MirDataType::Struct { .. } => {
                return;
            }
        };

        self.layouts.insert(data_type.clone(), layout);
    }
}

/// checks if the offset is a multiple of the alignment
///
/// if not, returns the amount of bytes needed
/// to make it a valid offset
pub fn padding_needed_for(offset: u32, align: u32) -> u32 {
    if align == 0 {
        return 0;
    }
    let misalign = offset % align;
    if misalign > 0 { align - misalign } else { 0 }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Layout {
    pub size: u32,
    pub align: u32,
    // For enums, offsets[0] is discriminant and offsets[1] is payload.
    pub offsets: Vec<u32>,
}

impl Layout {
    pub fn from_lir_type(layouts: &Layouts, lir_type: &LirType) -> Option<Self> {
        match lir_type {
            LirType::Struct(fields) => {
                let mut offsets = Vec::with_capacity(fields.len());
                let mut max_align = 1;
                let mut current_offset = 0;

                for field in fields.iter() {
                    let layout = layouts.get_layout(&field.1)?;

                    if layout.align > max_align {
                        max_align = layout.align;
                    }

                    current_offset += padding_needed_for(current_offset, layout.align);
                    offsets.push(current_offset);
                    current_offset += layout.size;
                }

                current_offset += padding_needed_for(current_offset, max_align);

                Some(Self {
                    size: current_offset,
                    align: max_align,
                    offsets,
                })
            }
            LirType::Enum(variants) => {
                let tag_size = 4;
                let tag_align = 4;

                let mut size = 0;
                let mut align = 1;

                for (_, payload_type) in variants {
                    if let Some(payload_type) = payload_type {
                        let layout = layouts.get_layout(payload_type)?;

                        if layout.size > size {
                            size = layout.size;
                        }

                        if layout.align > align {
                            align = layout.align;
                        }
                    }
                }

                align = align.max(tag_align);
                let payload_offset = tag_size + padding_needed_for(tag_size, align.max(1));

                size += payload_offset;
                size += padding_needed_for(size, align);

                Some(Self {
                    size,
                    align,
                    offsets: vec![0, payload_offset],
                })
            }
        }
    }
}

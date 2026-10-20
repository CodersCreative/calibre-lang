use calibre_lir::{MirDataType, environment::LirType};
use rustc_hash::FxHashMap;

#[derive(Debug, Default)]
pub(crate) struct Layouts {
    pub pointer_bit_width: u32,
    pub layouts: FxHashMap<MirDataType, Layout>,
}

impl Layouts {
    pub(crate) fn get_layout(&self, data_type: &MirDataType) -> Option<&Layout> {
        self.layouts.get(data_type)
    }

    pub(crate) fn ensure_layout(&mut self, data_type: &MirDataType) {
        if self.layouts.contains_key(data_type) {
            return;
        }

        let pointer_bit_width = self.pointer_bit_width;

        let size = match data_type {
            // TODO Move to pointer_sized
            MirDataType::Int | MirDataType::Float => 64 / 8,
            MirDataType::Range => 64 / 8 * 2,
            MirDataType::Bool | MirDataType::Char => 1,
            // I think I need to try and auto box dyn
            MirDataType::Ref(_, _)
            | MirDataType::Str
            | MirDataType::Function { .. }
            | MirDataType::Dynamic => pointer_bit_width / 8,
            MirDataType::Option(inner) => {
                self.ensure_layout(inner);
                let Some(inner_layout) = self.get_layout(inner) else {
                    return;
                };

                let _ = self.layouts.insert(
                    data_type.clone(),
                    Layout {
                        size: inner_layout.size + 1,
                        align: inner_layout.align,
                        offsets: vec![inner_layout.size],
                    },
                );

                return;
            }
            MirDataType::Result { ok, err } => {
                self.ensure_layout(ok);
                self.ensure_layout(err);

                let Some(ok_layout) = self.get_layout(ok) else {
                    return;
                };
                let Some(err_layout) = self.get_layout(err) else {
                    return;
                };

                let inner_size = ok_layout.size.max(err_layout.size);
                let inner_align = ok_layout.align.max(err_layout.align);

                let _ = self.layouts.insert(
                    data_type.clone(),
                    Layout {
                        size: inner_size + 1,
                        align: inner_align,
                        offsets: vec![inner_size],
                    },
                );

                return;
            }
            _ => pointer_bit_width / 8,
        };

        let align = match data_type {
            MirDataType::Int | MirDataType::Float => size.min(8),
            MirDataType::Range => size.min(16),
            MirDataType::Bool | MirDataType::Char => 1,
            MirDataType::Ref(_, _) | MirDataType::Str | MirDataType::Function { .. } => size.min(8),
            _ => size.min(8),
        };

        let _ = self.layouts.insert(
            data_type.clone(),
            Layout {
                size,
                align,
                offsets: Vec::new(),
            },
        );
    }
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

#[derive(Debug, Clone)]
pub(crate) struct Layout {
    pub size: u32,
    pub align: u32,
    // If an enum the discriminant_offset is offset[0]
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

                Some(Self {
                    size: current_offset,
                    align: max_align,
                    offsets,
                })
            }
            LirType::Enum(_) => {
                // TODO Enum layouts
                None
            }
        }
    }
}

use calibre_lir::MirDataType;
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
            MirDataType::Ref(_, _) | MirDataType::Str | MirDataType::Function { .. } => {
                pointer_bit_width / 8
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

#[derive(Debug, Clone)]
pub(crate) struct Layout {
    pub size: u32,
    pub align: u32,
    pub offsets: Vec<u32>,
}

use crate::{VM, value::RuntimeValue};

pub mod r#async;
pub mod collections;
pub mod crypto;
pub mod env;
pub mod fs;
pub mod list;
pub mod net;
pub mod process;
pub mod regex;
pub mod str;

impl VM {
    pub fn setup_stdlib(&mut self) {
        let mapping_index = Self::build_mapping_index(self.mappings.as_ref(), true);
        let mut prepared = Vec::new();
        for (full_name, value) in RuntimeValue::natives() {
            let Some((scope, short)) = full_name.split_once('.') else {
                continue;
            };
            let key = VM::mapped_name(&mapping_index, short);
            prepared.push((key.clone(), value.clone()));
            if scope != "env" && key != short {
                prepared.push((short.to_string(), value));
            }
        }

        for (name, value) in prepared {
            let _ = self.variables.insert(name, value);
        }
    }
}

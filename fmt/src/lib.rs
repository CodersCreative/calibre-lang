use std::{error::Error, fs, io::Write, path::PathBuf};

use calibre_parser::ast::{NodeType, formatter::Formatter};

pub fn format_file(
    formatter: &mut Formatter,
    path: &PathBuf,
    output: &PathBuf,
) -> Result<(), Box<dyn Error>> {
    let contents = fs::read_to_string(path)?;
    let out = formatter.start_format(contents, None)?;
    fs::File::create(output)?.write_all(out.as_bytes())?;
    Ok(())
}

pub fn format_all(formatter: &mut Formatter, path: &PathBuf) -> Result<(), Box<dyn Error>> {
    let imports = formatter.get_imports(fs::read_to_string(&path)?)?;

    let base = path.parent().unwrap();

    for import in imports {
        let NodeType::ImportStatement {
            module,
            alias: _,
            values: _,
        } = import.node_type
        else {
            unreachable!()
        };

        if module.len() == 1 {
            let path = base.join(&format!("{}.cl", module[0]));
            if path.exists() {
                format_all(formatter, &path)?;
            } else {
                let path = base.join(&format!("{}/main.cl", module[0]));
                if path.exists() {
                    format_all(formatter, &path)?;
                }
            }
        }
    }

    format_file(formatter, path, path)
}

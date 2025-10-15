use calibre_parser::ast::Node;

use crate::Formatter;

impl Formatter {
    pub fn format(&mut self, node: Node) -> String {
        self.position = Some(node.span);

        todo!()
    }
}

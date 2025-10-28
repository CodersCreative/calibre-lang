use std::error::Error;

use calibre_parser::{
    Parser,
    ast::{Node, NodeType},
    lexer::{Span, Token, TokenType, Tokenizer},
};

pub mod format;

pub struct Formatter {
    pub lines: Vec<(String, Span)>,
    pub comments: Vec<Token>,
    pub max_width: usize,
    pub cur_tab: usize,
    pub position: Option<Span>,
}

impl Default for Formatter {
    fn default() -> Self {
        Self {
            lines: Vec::new(),
            comments: Vec::new(),
            max_width: 200,
            cur_tab: 0,
            position: None,
        }
    }
}

impl Formatter {
    pub fn generate_lines(&mut self, ast: Node) -> Result<(), Box<dyn Error>> {
        if let NodeType::ScopeDeclaration {
            body,
            is_temp: false,
        } = ast.node_type
        {
            for node in body {
                let line = self.format(&node);
                self.lines.push((line, node.span));
            }
        }

        Ok(())
    }

    pub fn start_format(&mut self, text: String) -> Result<String, Box<dyn Error>> {
        self.lines.clear();
        self.position = None;

        let mut tokenizer = Tokenizer::new(true);
        let tokens = tokenizer.tokenize(text)?;

        self.comments = tokens
            .iter()
            .filter(|x| x.token_type == TokenType::Comment)
            .cloned()
            .collect();
        let tokens = tokens
            .into_iter()
            .filter(|x| x.token_type != TokenType::Comment)
            .collect();

        let mut parser = Parser::default();
        let ast = parser.produce_ast(tokens);

        let _ = self.generate_lines(ast)?;

        let mut text = String::new();
        for line in self.lines.clone().into_iter() {
            text.push_str(&format!(
                "{};\n",
                self.fmt_txt_with_comments_and_tab(&line.0, &line.1, 0, true)
                    .trim_end()
            ));
        }

        Ok(text)
    }
}

const BASIC_CODE: &str = r#"
const thirty = fn () -> int => 30;

const forty = match {
    Some(x) => x,
};   
"#;

fn main() {
    let mut formatter = Formatter::default();
    println!(
        "{}",
        formatter.start_format(BASIC_CODE.to_string()).unwrap()
    );
}

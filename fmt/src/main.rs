use std::error::Error;

use calibre_parser::{
    Parser,
    ast::{Node, NodeType},
    lexer::{Token, TokenType, Tokenizer},
};

pub mod format;

pub struct Formatter {
    pub lines: Vec<String>,
    pub comments: Vec<Token>,
    pub max_width: usize,
}

impl Default for Formatter {
    fn default() -> Self {
        Self {
            lines: Vec::new(),
            comments: Vec::new(),
            max_width: 200,
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
                let temp =
                    handle_comment!(self.get_potential_comment(&node.span), self.format(&node));
                self.lines.push(temp);
            }
        }

        Ok(())
    }

    pub fn start_format(&mut self, text: String) -> Result<String, Box<dyn Error>> {
        self.lines.clear();

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
            text.push_str(&format!("{};\n\n", line.trim_end().trim_end_matches("\n")));
        }

        while !self.comments.is_empty() {
            text.push_str(&format!("{}\n\n", self.fmt_next_comment().unwrap()));
        }

        Ok(text.trim_end().trim_end_matches("\n").to_string())
    }
}

const BASIC_CODE: &str = r#"
// const thirty = fn () -> int => 30;

const forty = match {
    Some(x) => x,
};

// const thirty = fn () -> int => 30;
const thirty = fn () -> int => 30;  
"#;

fn main() {
    let mut formatter = Formatter::default();
    println!(
        "{}",
        formatter.start_format(BASIC_CODE.to_string()).unwrap()
    );
}

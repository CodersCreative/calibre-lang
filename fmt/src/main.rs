use std::{error::Error, fs, io::Write, path::PathBuf, str::FromStr};

use calibre_parser::{
    ast::{Node, NodeType},
    lexer::{Token, TokenType, Tokenizer},
};
use clap::Parser;

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
            let mut lines = self.get_scope_lines(&body);
            self.lines.append(&mut lines);
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

        let mut parser = calibre_parser::Parser::default();
        let ast = parser.produce_ast(tokens);

        if !parser.errors.is_empty() {
            return Err(format!("{:?}", parser.errors).into());
        }

        let _ = self.generate_lines(ast)?;

        let mut text = String::new();
        for line in self.lines.iter() {
            text.push_str(line);
        }

        while !self.comments.is_empty() {
            text.push_str(&format!("{}\n\n", self.fmt_next_comment().unwrap()));
        }

        Ok(text.trim_end().trim_end_matches("\n").to_string())
    }
}

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[arg(index(1))]
    path: Option<String>,
    #[arg(short, long)]
    output: Option<String>,
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();
    let mut formatter = Formatter::default();

    if let Some(path) = args.path {
        let path = PathBuf::from_str(&path)?;
        let contents = fs::read_to_string(&path)?;
        let output = formatter.start_format(contents)?;
        if let Some(out) = args.output {
            fs::File::create(&out)?.write_all(output.as_bytes())?;
        } else {
            fs::File::create(&path)?.write_all(output.as_bytes())?;
        }
    } else {
        todo!("Parsing of entire project not yet complete")
    }

    Ok(())
}

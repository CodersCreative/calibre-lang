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
    fn generate_lines(&mut self, ast: Node) -> Result<(), Box<dyn Error>> {
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

    fn start_format(&mut self, text: String) -> Result<String, Box<dyn Error>> {
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

    pub fn format_file(&mut self, path: &PathBuf, output: &PathBuf) -> Result<(), Box<dyn Error>> {
        let contents = fs::read_to_string(path)?;
        let out = self.start_format(contents)?;
        fs::File::create(output)?.write_all(out.as_bytes())?;
        Ok(())
    }

    fn get_imports(&self, contents: String) -> Result<Vec<Node>, Box<dyn Error>> {
        let mut tokenizer = Tokenizer::default();
        let tokens = tokenizer.tokenize(contents)?;
        let mut parser = calibre_parser::Parser::default();
        let NodeType::ScopeDeclaration { body, is_temp: _ } = parser.produce_ast(tokens).node_type
        else {
            unreachable!()
        };

        Ok(body
            .into_iter()
            .filter(|x| match x.node_type {
                NodeType::ImportStatement { .. } => true,
                _ => false,
            })
            .collect())
    }

    pub fn format_all(&mut self, path: &PathBuf) -> Result<(), Box<dyn Error>> {
        let imports = self.get_imports(fs::read_to_string(&path)?)?;

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
                    self.format_all(&path)?;
                } else {
                    let path = base.join(&format!("{}/main.cl", module[0]));
                    if path.exists() {
                        self.format_all(&path)?;
                    }
                }
            }
        }

        self.format_file(path, path)
    }
}

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[arg(index(1))]
    path: Option<String>,
    #[arg(short, long)]
    output: Option<String>,
    #[arg(short, long)]
    all: bool,
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();

    let path = if let Some(p) = args.path {
        PathBuf::from_str(&p)?
    } else {
        PathBuf::from_str("./main.cl")?
    };
    let mut formatter = Formatter::default();

    if args.all {
        formatter.format_all(&path)
    } else {
        let output = if let Some(x) = args.output {
            PathBuf::from_str(&x)?
        } else {
            path.clone()
        };

        formatter.format_file(&path, &output)
    }
}

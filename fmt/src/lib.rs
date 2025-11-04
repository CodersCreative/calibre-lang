use std::{error::Error, fs, io::Write, path::PathBuf};

use calibre_parser::{
    ast::{Node, NodeType},
    lexer::{Span, Token, TokenType, Tokenizer},
};

pub mod format;

pub struct Tab {
    character: char,
    amt: usize,
}

impl Default for Tab {
    fn default() -> Self {
        Self::new('\t', 1)
    }
}

impl Tab {
    pub fn new(character: char, amt: usize) -> Self {
        Self { character, amt }
    }

    pub fn get_singular_tab(&self) -> String {
        let mut txt = String::new();
        for _ in 0..self.amt {
            txt.push(self.character.clone());
        }
        txt
    }

    pub fn get_tab_from_amt(&self, amt: usize) -> String {
        let mut txt = String::new();
        for _ in 0..amt {
            txt.push_str(&self.get_singular_tab());
        }
        txt
    }
}

pub struct Formatter {
    pub lines: Vec<String>,
    pub comments: Vec<Token>,
    pub max_width: usize,
    pub tab: Tab,
}

impl Default for Formatter {
    fn default() -> Self {
        Self {
            lines: Vec::new(),
            comments: Vec::new(),
            max_width: 200,
            tab: Tab::default(),
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

    pub fn start_format(
        &mut self,
        text: String,
        range: Option<Span>,
    ) -> Result<String, Box<dyn Error>> {
        self.lines.clear();

        let mut tokenizer = Tokenizer::new(true);
        let mut tokens = tokenizer.tokenize(text)?;
        if let Some(range) = range {
            tokens = tokens
                .into_iter()
                .filter(|x| x.span.from >= range.from && x.span.to <= range.to)
                .collect()
        }

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
        let out = self.start_format(contents, None)?;
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

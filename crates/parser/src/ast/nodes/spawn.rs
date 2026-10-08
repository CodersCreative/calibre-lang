use crate::ast::{Substitutable, nodes::AstNode, types::ParserDataType};
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum SelectArmKind {
    Recv,
    Send,
    Default,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SelectArm {
    pub patterns: Vec<(SelectArmKind, Option<AstNode>, Option<AstNode>)>,
    pub conditionals: Vec<AstNode>,
    pub body: AstNode,
}

impl Substitutable for SelectArm {
    fn substitute(mut self, subst: &FxHashMap<String, ParserDataType>) -> Self {
        self.patterns = self
            .patterns
            .into_iter()
            .map(|x| {
                (
                    x.0,
                    x.1.map(|x| x.substitute(subst)),
                    x.2.map(|x| x.substitute(subst)),
                )
            })
            .collect();

        self.conditionals = self
            .conditionals
            .into_iter()
            .map(|x| x.substitute(subst))
            .collect();

        self.body = self.body.substitute(subst);

        self
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstSpawn {
    pub items: Vec<AstNode>,
    pub auto_wait: bool,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstSelect {
    pub arms: Vec<SelectArm>,
}

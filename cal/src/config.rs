use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Debug, Deserialize, Serialize)]
pub struct Config {
    pub package: Package,
    pub dependencies: Option<HashMap<String, Dependency>>,
    #[serde(rename = "dev-dependencies")]
    pub dev_dependencies: Option<HashMap<String, Dependency>>,
    pub features: Option<HashMap<String, Vec<String>>>,
    pub examples: Option<ExamplesConfig>,
    #[serde(rename = "example")]
    pub example_list: Option<Vec<Example>>,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct Package {
    pub name: String,
    pub description: String,
    pub version: String,
    pub authors: Vec<String>,
    pub cal: String,
    pub readme: String,
    pub homepage: String,
    pub src: String,
    pub repository: String,
    pub license: String,
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(untagged)]
pub enum Dependency {
    Simple(String),
    Detailed {
        git: String,
        #[serde(rename = "ref")]
        reference: String,
    },
}

#[derive(Debug, Deserialize, Serialize)]
pub struct ExamplesConfig {
    pub members: Vec<String>,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct Example {
    pub name: String,
    pub path: String,
}

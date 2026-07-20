use std::path::PathBuf;

use crate::{environment::MiddleEnvironment, tags::TagInfo};

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, Default)]
pub struct Testing {
    pub tests: Vec<Test>,
}

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum TestOrBench {
    Test,
    Bench,
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct Test {
    pub name: String,
    pub function_name: String,
    pub kind: TestOrBench,
    pub skip: bool,
    pub skip_reason: Option<String>,
    pub panics: bool,
    pub todo: bool,
    pub todo_reason: Option<String>,
    pub scope_id: u64,
    pub file_path: Option<PathBuf>,
    pub suites: Vec<String>,
}

impl Testing {
    pub fn get_test(&self, name: &str) -> Option<&Test> {
        self.tests.iter().find(|x| x.name == name)
    }

    pub fn get_all_tests(&self) -> Vec<&Test> {
        self.tests
            .iter()
            .filter(|m| m.kind == TestOrBench::Test)
            .collect()
    }

    pub fn get_all_benches(&self) -> Vec<&Test> {
        self.tests
            .iter()
            .filter(|m| m.kind == TestOrBench::Bench)
            .collect()
    }

    pub fn get_active_tests(&self) -> Vec<&Test> {
        self.tests
            .iter()
            .filter(|m| m.kind == TestOrBench::Test && !m.skip && !m.todo)
            .collect()
    }

    pub fn get_active_benches(&self) -> Vec<&Test> {
        self.tests
            .iter()
            .filter(|m| m.kind == TestOrBench::Bench && !m.skip && !m.todo)
            .collect()
    }

    pub fn get_inactive_tests(&self) -> Vec<&Test> {
        self.tests
            .iter()
            .filter(|m| m.kind == TestOrBench::Test && (m.skip || m.todo))
            .collect()
    }

    pub fn get_inactive_benches(&self) -> Vec<&Test> {
        self.tests
            .iter()
            .filter(|m| m.kind == TestOrBench::Bench && (m.skip || m.todo))
            .collect()
    }
}

impl MiddleEnvironment {
    pub fn register_test(
        &mut self,
        name: String,
        function_name: String,
        scope_id: u64,
        file_path: Option<PathBuf>,
    ) {
        let (skip, skip_reason) = self
            .tagging
            .tag_info
            .iter()
            .find_map(|tag| match tag {
                TagInfo::Skip(reason) => Some((true, reason.clone())),
                _ => None,
            })
            .unwrap_or((false, None));

        let (todo, todo_reason) = self
            .tagging
            .tag_info
            .iter()
            .find_map(|tag| match tag {
                TagInfo::Todo(reason) => Some((true, reason.clone())),
                _ => None,
            })
            .unwrap_or((false, None));

        self.testing.tests.push(Test {
            name,
            function_name,
            suites: self
                .tagging
                .tag_info
                .iter()
                .filter_map(|tag| match tag {
                    TagInfo::Suite(x) => Some(x.clone()),
                    _ => None,
                })
                .collect(),
            kind: if self.tagging.tag_info.contains(&TagInfo::Bench) {
                TestOrBench::Bench
            } else {
                TestOrBench::Test
            },
            panics: self.tagging.tag_info.contains(&TagInfo::Panics),
            skip,
            skip_reason,
            todo,
            todo_reason,
            scope_id,
            file_path,
        })
    }
}

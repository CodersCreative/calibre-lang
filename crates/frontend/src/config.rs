use crate::paths::*;
use calibre_vm::config::VMConfig;
use rustc_hash::{FxHashMap, FxHashSet};
use serde::{Deserialize, Serialize};
use std::{
    fs,
    path::{Path, PathBuf},
};

#[derive(Debug, Clone, Deserialize, Serialize, Default)]
pub struct Config {
    #[serde(default)]
    pub package: Package,
    #[serde(default)]
    pub members: Option<FxHashMap<String, Member>>,
    #[serde(default)]
    pub dependencies: Option<FxHashMap<String, Dependency>>,
    #[serde(rename = "dev-dependencies", default)]
    pub dev_dependencies: Option<FxHashMap<String, Dependency>>,
    #[serde(default)]
    pub features: Option<FxHashMap<String, Vec<String>>>,
    #[serde(default)]
    pub examples: Option<ExamplesConfig>,
    #[serde(rename = "example", default)]
    pub example_list: Option<Vec<Example>>,
    #[serde(default)]
    pub vm: VmConfigToml,
    #[serde(rename = "no-std", default)]
    pub no_std: bool,
}

impl Config {
    pub fn to_toml_string(&self) -> Option<String> {
        toml::to_string_pretty(self).ok()
    }
}

#[derive(Debug, Clone, Deserialize, Serialize, Default)]
pub struct Package {
    #[serde(default)]
    pub name: String,
    #[serde(default)]
    pub description: String,
    #[serde(default)]
    pub version: String,
    #[serde(default)]
    pub authors: Vec<String>,
    #[serde(default)]
    pub calibre: String,
    #[serde(default)]
    pub readme: String,
    #[serde(default)]
    pub homepage: String,
    #[serde(default = "default_src")]
    pub src: String,
    #[serde(default)]
    pub repository: String,
    #[serde(default)]
    pub license: String,
    #[serde(default = "default_include")]
    pub include: String,
}

fn default_src() -> String {
    "src/main.cal".to_string()
}

fn default_include() -> String {
    "include".to_string()
}

#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(untagged)]
pub enum Dependency {
    Simple(String),
    Path {
        path: PathBuf,
    },
    Git {
        git: String,
        #[serde(rename = "ref")]
        reference: String,
    },
}

#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(untagged)]
pub enum Member {
    // TODO Maybe allow for members from a seperate git repo
    Path { path: PathBuf },
}

#[derive(Debug, Clone, Deserialize, Serialize, Default)]
pub struct ExamplesConfig {
    #[serde(default)]
    pub members: Vec<String>,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Example {
    pub name: String,
    pub path: String,
}

#[derive(Debug, Clone, Deserialize, Serialize, Default)]
pub struct VmConfigToml {
    pub gc_interval: Option<u64>,
    pub async_max_per_thread: Option<usize>,
    pub async_quantum: Option<usize>,
}

impl From<VmConfigToml> for VMConfig {
    fn from(value: VmConfigToml) -> Self {
        Self {
            gc_interval: value.gc_interval,
            async_max_per_thread: value.async_max_per_thread,
            async_quantum: value.async_quantum,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ProjectContext {
    pub root: PathBuf,
    #[allow(dead_code)]
    pub manifest_path: PathBuf,
    pub config: Config,
}

impl From<&ProjectContext> for VMConfig {
    fn from(value: &ProjectContext) -> Self {
        value.config.vm.clone().into()
    }
}

impl ProjectContext {
    pub fn load_from_cwd() -> Result<Option<ProjectContext>, String> {
        let cwd = std::env::current_dir().map_err(|x| x.to_string())?;
        Self::load(&cwd)
    }

    pub fn load(start: impl AsRef<Path>) -> Result<Option<ProjectContext>, String> {
        let Some(manifest_path) = find_manifest_from(start) else {
            return Ok(None);
        };

        let manifest_content = fs::read_to_string(&manifest_path)
            .map_err(|e| format!("failed to read {:?}: {e}", manifest_path))?;

        let config: Config = toml::from_str(&manifest_content)
            .map_err(|e| format!("failed to parse {:?}: {e}", manifest_path))?;

        let manifest_path = std::fs::canonicalize(&manifest_path)
            .map_err(|e| format!("failed to canonicalize {:?}: {e}", manifest_path))?;

        let root = manifest_path
            .parent()
            .map(Path::to_path_buf)
            .ok_or_else(|| "manifest parent is missing".to_string())?;

        Ok(Some(ProjectContext {
            root,
            manifest_path,
            config,
        }))
    }
}

#[derive(Debug, Clone)]
pub struct ResolvedExample {
    pub name: String,
    pub path: PathBuf,
}

impl ProjectContext {
    pub fn resolve_examples(&self) -> Vec<ResolvedExample> {
        let mut explicit = FxHashMap::<String, PathBuf>::default();
        if let Some(list) = &self.config.example_list {
            for ex in list {
                explicit.insert(ex.name.clone(), self.root.join(&ex.path));
            }
        }

        let mut auto_paths = Vec::<PathBuf>::new();
        let mut symbolic_members = Vec::<String>::new();

        if let Some(ex_cfg) = &self.config.examples {
            for member in &ex_cfg.members {
                #[cfg(feature = "fs")]
                {
                    let has_glob =
                        member.contains('*') || member.contains('?') || member.contains('[');
                    if has_glob {
                        let pattern = self.root.join(member).to_string_lossy().to_string();
                        if let Ok(paths) = glob::glob(&pattern) {
                            for path in paths.flatten() {
                                if path.is_file()
                                    && path.extension().and_then(|x| x.to_str()) == Some("cal")
                                {
                                    auto_paths.push(path);
                                } else if path.is_dir() {
                                    collect_cal_sources(&path, &mut auto_paths);
                                }
                            }
                        }
                        continue;
                    }
                }

                let p = self.root.join(member);
                if p.is_file() {
                    auto_paths.push(p);
                } else if p.is_dir() {
                    collect_cal_sources(&p, &mut auto_paths);
                } else {
                    symbolic_members.push(member.clone());
                }
            }
        } else {
            auto_paths = default_examples(&self.root);
        }

        let mut dedupe = FxHashSet::default();
        auto_paths.retain(|p| dedupe.insert(p.clone()));

        let mut generated = Vec::<ResolvedExample>::new();
        let mut counters = FxHashMap::<String, usize>::default();

        for p in auto_paths {
            let mut base = auto_example_name(&p);
            if let Some(n) = counters.get_mut(&base) {
                *n += 1;
                base = format!("{base}{n}");
            } else {
                counters.insert(base.clone(), 0);
            }

            generated.push(ResolvedExample {
                name: base,
                path: p,
            });
        }

        if !symbolic_members.is_empty() {
            let mut fallback = Vec::new();
            collect_cal_sources(self.root.join("examples"), &mut fallback);

            for p in fallback {
                let n = auto_example_name(&p);
                if symbolic_members.iter().any(|m| m == &n) {
                    generated.push(ResolvedExample { name: n, path: p });
                }
            }
        }

        let mut seen = FxHashSet::default();
        generated.retain(|e| seen.insert(e.path.clone()));

        for (name, path) in explicit {
            generated.retain(|e| e.name != name);
            generated.push(ResolvedExample { name, path });
        }

        generated.sort_by(|a, b| a.name.cmp(&b.name));
        generated
    }

    pub fn resolve_example_paths(&self, name: &str) -> Option<PathBuf> {
        self.resolve_examples()
            .into_iter()
            .find(|ex| ex.name == name)
            .map(|ex| ex.path)
    }

    pub fn resolve_include_paths(&self) -> Vec<PathBuf> {
        let mut files = Vec::new();
        let include_dir = self.root.join(&self.config.package.include);
        collect_include_files(&include_dir, &mut files);
        files.sort();
        files
    }
}

fn default_examples(root: impl AsRef<Path>) -> Vec<PathBuf> {
    let mut files = Vec::new();
    collect_cal_sources(root.as_ref().join("examples"), &mut files);
    files
}

fn auto_example_name(path: impl AsRef<Path>) -> String {
    let file_name = path
        .as_ref()
        .file_name()
        .and_then(|x| x.to_str())
        .unwrap_or_default();

    if file_name == "main.cal" {
        path.as_ref()
            .parent()
            .and_then(|x| x.file_name())
            .and_then(|x| x.to_str())
            .unwrap_or("example")
            .to_string()
    } else {
        path.as_ref()
            .file_stem()
            .and_then(|x| x.to_str())
            .unwrap_or("example")
            .to_string()
    }
}

fn collect_include_files(dir: impl AsRef<Path>, out: &mut Vec<PathBuf>) {
    if !dir.as_ref().exists() {
        return;
    }

    let Ok(rd) = fs::read_dir(dir) else {
        return;
    };

    for entry in rd.flatten() {
        let p = entry.path();
        if p.is_dir() {
            collect_include_files(&p, out);
        } else {
            let ext = p.extension().and_then(|x| x.to_str());
            if ext == Some("calp") || ext == Some("jcalp") {
                out.push(p);
            }
        }
    }
}

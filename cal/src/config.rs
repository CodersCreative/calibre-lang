use serde::{Deserialize, Serialize};
use std::{
    collections::{HashMap, HashSet},
    fs,
    path::{Path, PathBuf},
};

#[derive(Debug, Clone, Deserialize, Serialize, Default)]
pub struct Config {
    #[serde(default)]
    pub package: Package,
    #[serde(default)]
    pub dependencies: Option<HashMap<String, Dependency>>,
    #[serde(rename = "dev-dependencies", default)]
    pub dev_dependencies: Option<HashMap<String, Dependency>>,
    #[serde(default)]
    pub features: Option<HashMap<String, Vec<String>>>,
    #[serde(default)]
    pub examples: Option<ExamplesConfig>,
    #[serde(rename = "example", default)]
    pub example_list: Option<Vec<Example>>,
    #[serde(default)]
    pub vm: VmConfigToml,
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
    pub cal: String,
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
}

fn default_src() -> String {
    "src/main.cal".to_string()
}

#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(untagged)]
pub enum Dependency {
    Simple(String),
    Detailed {
        git: String,
        #[serde(rename = "ref")]
        reference: String,
    },
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

#[derive(Debug, Clone)]
pub struct ProjectContext {
    pub root: PathBuf,
    pub manifest_path: PathBuf,
    pub config: Config,
}

#[derive(Debug, Clone)]
pub struct ResolvedExample {
    pub name: String,
    pub path: PathBuf,
}

pub fn find_manifest_from(start: &Path) -> Option<PathBuf> {
    let mut cur = if start.is_dir() {
        start.to_path_buf()
    } else {
        start.parent()?.to_path_buf()
    };
    loop {
        let candidate = cur.join("cal.toml");
        if candidate.exists() {
            return Some(candidate);
        }
        if !cur.pop() {
            return None;
        }
    }
}

pub fn load_project_from(start: &Path) -> Result<Option<ProjectContext>, String> {
    let Some(manifest_path) = find_manifest_from(start) else {
        return Ok(None);
    };
    let txt = fs::read_to_string(&manifest_path)
        .map_err(|e| format!("failed to read {:?}: {e}", manifest_path))?;
    let config: Config = toml::from_str(&txt)
        .map_err(|e| format!("failed to parse {:?}: {e}", manifest_path))?;
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

fn collect_cal_files(dir: &Path, out: &mut Vec<PathBuf>) {
    if !dir.exists() {
        return;
    }
    let Ok(rd) = fs::read_dir(dir) else {
        return;
    };
    for entry in rd.flatten() {
        let p = entry.path();
        if p.is_dir() {
            collect_cal_files(&p, out);
        } else if p.extension().and_then(|x| x.to_str()) == Some("cal") {
            out.push(p);
        }
    }
}

fn default_examples(root: &Path) -> Vec<PathBuf> {
    let mut files = Vec::new();
    collect_cal_files(&root.join("examples"), &mut files);
    files
}

fn auto_example_name(path: &Path) -> String {
    let file_name = path.file_name().and_then(|x| x.to_str()).unwrap_or_default();
    if file_name == "main.cal" {
        path.parent()
            .and_then(|x| x.file_name())
            .and_then(|x| x.to_str())
            .unwrap_or("example")
            .to_string()
    } else {
        path.file_stem()
            .and_then(|x| x.to_str())
            .unwrap_or("example")
            .to_string()
    }
}

pub fn resolve_examples(ctx: &ProjectContext) -> Vec<ResolvedExample> {
    let mut explicit = HashMap::<String, PathBuf>::new();
    if let Some(list) = &ctx.config.example_list {
        for ex in list {
            explicit.insert(ex.name.clone(), ctx.root.join(&ex.path));
        }
    }

    let mut auto_paths = Vec::<PathBuf>::new();
    let mut symbolic_members = Vec::<String>::new();
    if let Some(ex_cfg) = &ctx.config.examples {
        for member in &ex_cfg.members {
            let has_glob = member.contains('*') || member.contains('?') || member.contains('[');
            if has_glob {
                let pattern = ctx.root.join(member).to_string_lossy().to_string();
                if let Ok(paths) = glob::glob(&pattern) {
                    for path in paths.flatten() {
                        if path.is_file()
                            && path.extension().and_then(|x| x.to_str()) == Some("cal")
                        {
                            auto_paths.push(path);
                        } else if path.is_dir() {
                            collect_cal_files(&path, &mut auto_paths);
                        }
                    }
                }
                continue;
            }
            let p = ctx.root.join(member);
            if p.is_file() {
                auto_paths.push(p);
            } else if p.is_dir() {
                collect_cal_files(&p, &mut auto_paths);
            } else {
                symbolic_members.push(member.clone());
            }
        }
    } else {
        auto_paths = default_examples(&ctx.root);
    }

    let mut dedupe = HashSet::new();
    auto_paths.retain(|p| dedupe.insert(p.clone()));

    let mut generated = Vec::<ResolvedExample>::new();
    let mut counters = HashMap::<String, usize>::new();
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
        collect_cal_files(&ctx.root.join("examples"), &mut fallback);
        for p in fallback {
            let n = auto_example_name(&p);
            if symbolic_members.iter().any(|m| m == &n) {
                generated.push(ResolvedExample { name: n, path: p });
            }
        }
    }

    let mut seen = HashSet::new();
    generated.retain(|e| seen.insert(e.path.clone()));

    for (name, path) in explicit {
        generated.retain(|e| e.name != name);
        generated.push(ResolvedExample { name, path });
    }

    generated.sort_by(|a, b| a.name.cmp(&b.name));
    generated
}

pub fn resolve_example_by_name(ctx: &ProjectContext, name: &str) -> Option<PathBuf> {
    resolve_examples(ctx)
        .into_iter()
        .find(|ex| ex.name == name)
        .map(|ex| ex.path)
}

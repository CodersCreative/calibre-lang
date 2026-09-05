use crate::commands::package::package_project_targets;
use calibre_frontend::config::Dependency;
use calibre_frontend::config::ProjectContext;
use derive_builder::Builder;
use serde::{Deserialize, Serialize};
use std::{
    error::Error,
    path::{Path, PathBuf},
    process::Command,
};
use toml_edit::{Document, Item, Table, value};
use tracing::instrument;

#[derive(Builder, Debug)]
pub struct Get {
    git: Option<String>,
    path: Option<String>,
    reference: Option<String>,
    all: bool,
}

impl Get {
    #[instrument]
    pub async fn execute(self) -> Result<(), Box<dyn Error>> {
        let project =
            ProjectContext::load_from_cwd()?.expect("calibre.toml file required for dependencies");

        let registry_path = registry_file();
        let mut registry = if let Some(ref p) = registry_path {
            if p.exists() {
                Registry::load(p)
            } else {
                Registry::default()
            }
        } else {
            Registry::default()
        };

        match (self.git, self.path) {
            (Some(git), _) => {
                let reference = self.reference.clone();

                if let Some(existing) = registry.find(&git, &reference)
                    && existing.exists()
                {
                    let dest = project.root.join("include").join("pm");
                    let _ = std::fs::create_dir_all(&dest);
                    let dest = dest.join(existing.file_name().unwrap());

                    std::fs::copy(&existing, &dest)?;
                    println!("Reused package {}", dest.display());

                    if let Some(p) = registry_path {
                        let _ = registry.save(&p);
                    }

                    return Ok(());
                }

                let repo_dir = ensure_cloned_and_checked_out(&git, &reference).await?;

                let packaged = package_project_targets(&repo_dir, false).await?;

                if packaged.is_empty() {
                    return Err("no packaged artifacts produced".into());
                }

                let package_path = packaged[0].clone();
                registry.insert(git.clone(), reference.clone(), package_path.clone());

                let dest = project.root.join("include").join("pm");
                let _ = std::fs::create_dir_all(&dest);
                let dest = dest.join(package_path.file_name().unwrap());

                std::fs::copy(&package_path, &dest)?;
                println!("Installed package to {}", dest.display());

                if let Some(p) = registry_path {
                    registry.save(&p)?;
                }

                if let Ok(Some(dep_proj)) = ProjectContext::load(&repo_dir) {
                    let dep_name = if !dep_proj.config.package.name.is_empty() {
                        dep_proj.config.package.name.clone()
                    } else {
                        repo_dir
                            .file_name()
                            .and_then(|s| s.to_str())
                            .unwrap_or("dependency")
                            .to_string()
                    };

                    if project.manifest_path.to_str().is_some() {
                        let _ = add_dependency(
                            project.manifest_path.as_path(),
                            &dep_name,
                            Item::Table({
                                let mut t = Table::new();

                                t["git"] = value(git.clone());
                                if let Some(r) = &reference {
                                    t["ref"] = value(r.clone());
                                }

                                t
                            }),
                        );
                    }
                }
            }
            (_, Some(path)) => {
                let pathbuf = PathBuf::from(path.clone());
                let packaged = package_project_targets(&pathbuf, false).await?;

                if packaged.is_empty() {
                    return Err("no packaged artifacts produced".into());
                }

                let package_path = packaged[0].clone();

                let dest = project.root.join("include").join("pm");
                let _ = std::fs::create_dir_all(&dest);
                let dest = dest.join(package_path.file_name().unwrap());

                std::fs::copy(&package_path, &dest)?;
                println!("Installed package to {}", dest.display());

                if let Ok(Some(dep_proj)) = ProjectContext::load(&pathbuf) {
                    let dep_name = if !dep_proj.config.package.name.is_empty() {
                        dep_proj.config.package.name.clone()
                    } else {
                        pathbuf
                            .file_name()
                            .and_then(|s| s.to_str())
                            .unwrap_or("dependency")
                            .to_string()
                    };

                    let _ = add_dependency(
                        project.manifest_path.as_path(),
                        &dep_name,
                        Item::Table({
                            let mut t = Table::new();

                            t["path"] = value(
                                if pathbuf.is_absolute()
                                    && let Ok(rel) = pathbuf.strip_prefix(&project.root)
                                {
                                    rel.to_string_lossy().to_string()
                                } else {
                                    pathbuf.to_string_lossy().to_string()
                                },
                            );

                            t
                        }),
                    );
                }
            }
            (None, None) => {
                let deps_map = project.config.dependencies.clone().unwrap_or_default();

                let pm_dir = project.root.join("include").join("pm");
                if pm_dir.exists() {
                    for entry in std::fs::read_dir(&pm_dir)? {
                        let entry = entry?;
                        let path = entry.path();

                        if !path.is_file() {
                            continue;
                        }

                        let fname = path.file_name().and_then(|s| s.to_str()).unwrap_or("");
                        if let Some(pos) = fname.rfind('-') {
                            let name = &fname[..pos];
                            if !deps_map.contains_key(name) {
                                let _ = std::fs::remove_file(&path);
                                println!("Removed unused package {}", fname);
                            }
                        }
                    }
                }

                for (name, dep) in deps_map.iter() {
                    match dep {
                        Dependency::Path { path } => {
                            let full = if path.is_absolute() {
                                path.clone()
                            } else {
                                project.root.join(path)
                            };
                            let packaged = package_project_targets(&full, false).await?;

                            if !packaged.is_empty() {
                                let package_path = &packaged[0];

                                let dest = project.root.join("include").join("pm");
                                let _ = std::fs::create_dir_all(&dest);
                                let dest = dest.join(package_path.file_name().unwrap());

                                std::fs::copy(package_path, &dest)?;
                                println!("Rebuilt path dependency {} -> {}", name, dest.display());
                            }
                        }
                        Dependency::Git { git, reference } => {
                            let is_specific = {
                                let r = reference.as_str();
                                let hex = r.chars().all(|c| c.is_ascii_hexdigit());
                                (r.len() >= 7 && hex)
                                    || r.starts_with("refs/tags/")
                                    || r.starts_with("tags/")
                            };

                            if self.all || !is_specific {
                                let ref_opt = if reference.is_empty() {
                                    None
                                } else {
                                    Some(reference.clone())
                                };
                                let repo_dir = ensure_cloned_and_checked_out(git, &ref_opt).await?;
                                let dir_str = repo_dir.to_str().ok_or("invalid path")?;
                                let _ = run_git_cmd(&["-C", dir_str, "pull"]);

                                let packaged = package_project_targets(&repo_dir, false).await?;
                                if !packaged.is_empty() {
                                    let package_path = &packaged[0];

                                    let dest = project.root.join("include").join("pm");
                                    let _ = std::fs::create_dir_all(&dest);
                                    let dest = dest.join(package_path.file_name().unwrap());

                                    std::fs::copy(package_path, &dest)?;
                                    println!(
                                        "Rebuilt git dependency {} -> {}",
                                        name,
                                        dest.display()
                                    );
                                }
                            }
                        }
                        _ => {}
                    }
                }
            }
        }

        Ok(())
    }
}

#[derive(Serialize, Deserialize, Debug, Clone)]
struct RegistryEntry {
    repo: String,
    reference: Option<String>,
    package_path: PathBuf,
}

#[derive(Serialize, Deserialize, Debug, Default)]
struct Registry {
    entries: Vec<RegistryEntry>,
}

impl Registry {
    fn load(path: &PathBuf) -> Self {
        let file = std::fs::File::open(path);
        if let Ok(mut f) = file
            && let Ok(reg) = bincode::deserialize_from::<_, Registry>(&mut f)
        {
            return reg;
        }
        Registry::default()
    }

    fn save(&self, path: &PathBuf) -> Result<(), Box<dyn Error>> {
        let mut f = std::fs::File::create(path)?;
        bincode::serialize_into(&mut f, self)?;
        Ok(())
    }

    fn find(&self, repo: &str, reference: &Option<String>) -> Option<PathBuf> {
        self.entries
            .iter()
            .find(|e| e.repo == repo && e.reference == *reference)
            .map(|e| e.package_path.clone())
    }

    fn insert(&mut self, repo: String, reference: Option<String>, package_path: PathBuf) {
        self.entries
            .retain(|e| !(e.repo == repo && e.reference == reference));
        self.entries.push(RegistryEntry {
            repo,
            reference,
            package_path,
        });
    }
}

pub fn registry_file() -> Option<PathBuf> {
    calibre_frontend::paths::get_cache_dir().map(|d| d.join("registry.bin"))
}

fn normalize_ref(reference: &str) -> String {
    reference
        .trim_start_matches("refs/tags/")
        .trim_start_matches("refs/heads/")
        .trim_start_matches("tags/")
        .to_string()
}

fn run_git_cmd(args: &[&str]) -> Result<String, String> {
    let output = Command::new("git")
        .env("GIT_TERMINAL_PROMPT", "0")
        .args(args)
        .output()
        .map_err(|e| format!("Failed to execute git process: {e}"))?;

    if output.status.success() {
        Ok(String::from_utf8_lossy(&output.stdout).trim().to_string())
    } else {
        Err(String::from_utf8_lossy(&output.stderr).trim().to_string())
    }
}

pub async fn ensure_cloned_and_checked_out(
    repo: &str,
    reference: &Option<String>,
) -> Result<PathBuf, Box<dyn Error>> {
    let cache_git = calibre_frontend::paths::get_cache_dir()
        .ok_or("Failed to resolve user cache directory")?
        .join("git");

    std::fs::create_dir_all(&cache_git)?;

    let gh_repo_name = repo
        .trim()
        .rsplit_once('/')
        .map(|(_, s)| s.trim_end_matches(".git"))
        .filter(|s| !s.is_empty())
        .ok_or("Not a valid repository URL")?;

    let repo_dir = cache_git.join(gh_repo_name);
    let dir_str = repo_dir.to_str().ok_or("Invalid path encoding")?;

    if repo_dir.exists() {
        let _ = run_git_cmd(&["-C", dir_str, "fetch", "--all", "--tags", "--prune"]);
    } else {
        run_git_cmd(&["clone", "--recurse-submodules", repo, dir_str])
            .map_err(|err| format!("git clone failed: {err}"))?;
    }

    if let Some(r) = reference {
        let normalized = normalize_ref(r);

        let (target, target_branch) = if let Some((branch, commit)) = normalized.split_once('/') {
            (normalize_ref(commit), Some(branch))
        } else {
            (normalized, None)
        };

        let checkout_attempts = [
            vec![String::from("checkout"), target.clone()],
            vec![
                String::from("checkout"),
                format!("origin/{}", target.clone()),
            ],
            vec![
                String::from("checkout"),
                String::from("-b"),
                target.clone(),
                format!("origin/{}", target.clone()),
            ],
            vec![String::from("checkout"), format!("refs/tags/{}", target)],
        ];

        let mut checkout_successful = false;

        if let Some(branch) = target_branch {
            let _ = run_git_cmd(&["-C", dir_str, "checkout", branch]);
        }

        for args in checkout_attempts {
            let mut full_args = vec!["-C", dir_str];
            full_args.extend(args.iter().map(|x| x.as_str()));

            if run_git_cmd(&full_args).is_ok() {
                checkout_successful = true;
                break;
            }
        }

        if !checkout_successful {
            return Err(format!("Failed to checkout target reference '{}' in {}", r, repo).into());
        }
    }

    Ok(repo_dir)
}

fn add_dependency(path: impl AsRef<Path>, name: &str, item: Item) -> Result<(), Box<dyn Error>> {
    let s = std::fs::read_to_string(path.as_ref())?;
    let mut doc = s.parse::<Document<_>>()?.into_mut();
    let table = doc.as_table_mut();

    if !table.contains_key("dependencies") {
        table.insert("dependencies", Item::Table(Table::new()));
    }

    let deps_item = table
        .get_mut("dependencies")
        .ok_or("failed to get dependencies table")?;

    let deps_table = deps_item
        .as_table_mut()
        .ok_or("dependencies is not a table")?;

    if deps_table.get(name).is_none() {
        deps_table.insert(name, item);
        std::fs::write(path, doc.to_string())?;
    }

    Ok(())
}

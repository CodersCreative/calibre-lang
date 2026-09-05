use crate::config::ProjectContext;
use directories::ProjectDirs;
use std::{
    error::Error,
    path::{Path, PathBuf},
};

pub fn get_cache_dir() -> Option<PathBuf> {
    ProjectDirs::from("", "", "Calibre").map(|x| x.cache_dir().to_path_buf())
}

pub fn get_config_dir() -> Option<PathBuf> {
    ProjectDirs::from("", "", "Calibre").map(|x| x.config_dir().to_path_buf())
}

pub fn collect_cal_sources(dir: impl AsRef<Path>, out: &mut Vec<PathBuf>) {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return;
    };

    for entry in entries.flatten() {
        let path = entry.path();

        if path.is_dir() {
            let skip = path
                .file_name()
                .and_then(|x| x.to_str())
                .map(|name| name == "target" || name == ".git")
                .unwrap_or(false);
            if !skip {
                collect_cal_sources(&path, out);
            }
            continue;
        }

        if path.extension().and_then(|x| x.to_str()) == Some("cal") {
            out.push(path);
        }
    }
}

pub fn collect_project_sources(
    project: Option<&ProjectContext>,
    cwd: impl AsRef<Path>,
    out: &mut Vec<PathBuf>,
) {
    if let Some(project) = project {
        let src = project.root.join(&project.config.package.src);
        if src.is_dir() {
            collect_cal_sources(&src, out);
        } else if src.is_file() {
            out.push(src);
        }

        for example in project.resolve_examples() {
            if example.path.is_file() {
                out.push(example.path);
            } else if example.path.is_dir() {
                collect_cal_sources(&example.path, out);
            }
        }

        let tests_dir = project.root.join("tests");
        collect_cal_sources(&tests_dir, out);

        let bench_dir = project.root.join("bench");
        collect_cal_sources(&bench_dir, out);

        let benches_dir = project.root.join("benches");
        collect_cal_sources(&benches_dir, out);
    } else {
        collect_cal_sources(cwd, out);
    }
}

pub fn resolve_run_targets(
    project: Option<&ProjectContext>,
    paths: Vec<String>,
    example: Option<String>,
) -> Result<Vec<PathBuf>, Box<dyn Error>> {
    if !paths.is_empty() && example.is_some() {
        return Err("cannot use both a path and --example".into());
    }

    if let Some(example) = example {
        let Some(project) = project else {
            return Err("`--example` requires a calibre.toml project".into());
        };
        if let Some(path) = project.resolve_example_paths(&example) {
            return Ok(vec![path]);
        }
        return Err(format!("example `{example}` not found").into());
    }

    if paths.is_empty()
        && let Some(project) = project
    {
        let cwd = project.root.join(&project.config.package.src);
        let path1 = cwd.join("main.cal");
        let path2 = cwd.join("src/main.cal");

        match (
            cwd.exists() && cwd.is_file(),
            path1.exists() && path1.is_file(),
            path2.exists() && path2.is_file(),
        ) {
            (true, _, _) => return Ok(vec![cwd]),
            (_, true, _) => return Ok(vec![path1]),
            (_, _, true) => return Ok(vec![path2]),
            _ => {}
        }
    };

    paths
        .into_iter()
        .map(|path| {
            if path.ends_with(".cal") {
                Ok(PathBuf::from(path))
            } else {
                Ok(std::fs::canonicalize(path)?)
            }
        })
        .collect::<Result<Vec<_>, _>>()
}

pub fn find_manifest_from(start: impl AsRef<Path>) -> Option<PathBuf> {
    let mut cur = if start.as_ref().is_dir() {
        start.as_ref().to_path_buf()
    } else {
        start.as_ref().parent()?.to_path_buf()
    };

    loop {
        let calibre_candidate = cur.join("calibre.toml");
        if calibre_candidate.exists() {
            return Some(calibre_candidate);
        }
        if !cur.pop() {
            return None;
        }
    }
}

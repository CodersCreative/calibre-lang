use calibre_frontend::config::ProjectContext;
use derive_builder::Builder;
use std::{error::Error, process::Command};
use tracing::instrument;

#[derive(Builder, Debug)]
pub struct Get {
    git: Option<String>,
    path: Option<String>,
    reference: Option<String>,
}

impl Get {
    #[instrument]
    pub async fn execute(self) -> Result<(), Box<dyn Error>> {
        let project = ProjectContext::load_from_cwd()?.expect("calibre.toml file required for dependencies");
        
        match (self.git, self.path) {
            (_, Some(path)) => {}
            (Some(git), _) => {
                let gh_repo_name = git
                    .trim()
                    .rsplit_once("/")
                    .expect("Not a valid repo url")
                    .1
                    .trim_end_matches(".git");
                let path = calibre_frontend::paths::get_cache_dir()
                    .expect("Unable to get cache dir")
                    .join("git");

                // TODO Deal with ref
                let output = Command::new("git")
                    .arg("clone")
                    .arg(&git)
                    .arg(path.join(gh_repo_name).as_os_str()).output()?;

                if output.status.success() {

                }
            }
            _ => unimplemented!(),
        }

        Ok(())
    }
}

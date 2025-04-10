use std::error::Error;
use std::ffi::OsStr;
use std::{env, fs};
use std::path::PathBuf;
use std::sync::Mutex;
use libtest_mimic::Failed;

pub use ctor;

pub use test_macro::test_resources;
pub use libtest_mimic;

pub struct Test {
    pub name: String,
    pub group: String,
    pub path: String,
    pub test_fn: fn(&str) -> Result<(), Failed>,
}

fn visit_dir(path_prefix: &PathBuf, path: &PathBuf) -> Result<Vec<PathBuf>, Box<dyn Error>> {
    let mut tests = Vec::new();

    for entry in fs::read_dir(path)? {
        let entry = entry?;
        let file_type = entry.file_type()?;

        // Handle files
        let path = entry.path();
        if file_type.is_file() {
            if path.extension() == Some(OsStr::new("say")) {
                tests.push(path);
            }
        } else if file_type.is_dir() {
            // Handle directories
            tests.append(&mut visit_dir(&path_prefix, &path)?);
        }
    }

    Ok(tests)
}

fn collect_tests(test_fn: fn(path: &str) -> Result<(), Failed>, group: String, folder_path: String) -> Result<Vec<Test>, Box<dyn Error>> {
    let mut dir_prefix = env::current_dir()?;
    dir_prefix.push(folder_path);
    let paths = visit_dir(&dir_prefix, &dir_prefix)?;
    let mut trials = Vec::new();
    for path in paths {
        let name = path.strip_prefix(&dir_prefix)?.display().to_string();
        let path = path.display().to_string();
        let trial = Test {
            name,
            group: group.clone(),
            path,
            test_fn
        };
        trials.push(trial);
    }
    Ok(trials)
}

static TESTS: Mutex<Vec<Test>> = Mutex::new(Vec::new());

pub struct TestCollection {}

impl TestCollection {
    pub fn add_tests(test_fn: fn(path: &str) -> Result<(), Failed>, test_name: &str, folder_path: &str) {
        let trials = collect_tests(test_fn, test_name.to_string(), folder_path.to_string()).unwrap_or_else(|_| {
            panic!("Failed to collect tests from folder: {}", folder_path)
        });
        for trial in trials {
            TESTS.lock().unwrap().push(trial);
        }
    }

    pub fn collect_tests() -> Vec<Test> {
        std::mem::take(TESTS.lock().unwrap().as_mut())
    }
}

//! Module layering: a layer may name the layers below it, never one above.

use std::fs;
use std::path::{Path, PathBuf};

/// Per layer under `src/`, the crate paths its sources may not name. `crate::ast` is an alias for
/// `frontend::ast`, so it counts as `frontend`.
const FORBIDDEN: &[(&str, &[&str])] = &[
    ("core", &["frontend", "ast", "middle", "backend", "runtime"]),
    ("frontend", &["core", "middle", "backend", "runtime"]),
    ("middle", &["backend", "runtime"]),
    ("backend", &["runtime"]),
    ("runtime", &[]),
];

/// The first path segment of every `crate::` reference on a line. A brace group contributes the
/// leading segment of each of its members, so `use crate::{a, b::c};` names both `a` and `b`.
fn named_after_crate(line: &str) -> Vec<String> {
    let mut out = Vec::new();
    for tail in line.split("crate::").skip(1) {
        let members: Vec<&str> = match tail.strip_prefix('{') {
            Some(group) => group.split(&[',', '}'][..]).collect(),
            None => vec![tail],
        };
        out.extend(members.iter().filter_map(|m| {
            let seg: String = m.trim_start().chars().take_while(|c| c.is_alphanumeric() || *c == '_').collect();
            (!seg.is_empty()).then_some(seg)
        }));
    }
    out
}

fn rust_files(dir: &Path, out: &mut Vec<PathBuf>) {
    for entry in fs::read_dir(dir).unwrap_or_else(|e| panic!("read {}: {e}", dir.display())) {
        let path = entry.expect("dir entry").path();
        if path.is_dir() {
            rust_files(&path, out);
        } else if path.extension().is_some_and(|e| e == "rs") {
            out.push(path);
        }
    }
}

#[test]
fn a_layer_names_only_the_layers_below_it() {
    let mut violations = Vec::new();
    for (layer, forbidden) in FORBIDDEN {
        let dir = Path::new("src").join(layer);
        let mut files = Vec::new();
        rust_files(&dir, &mut files);
        assert!(!files.is_empty(), "no sources found under src/{layer}");

        for file in &files {
            let text = fs::read_to_string(file).expect("read source");
            for (i, line) in text.lines().enumerate() {
                if line.trim_start().starts_with("//") {
                    continue;
                }
                for named in named_after_crate(line) {
                    if forbidden.contains(&named.as_str()) {
                        violations.push(format!("{}:{} names crate::{named}", file.display(), i + 1));
                    }
                }
            }
        }
    }
    assert!(violations.is_empty(), "a layer names one above it:\n{}", violations.join("\n"));
}

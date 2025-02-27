mod inline;
mod file;

use crate::run;

pub fn assert_file<const COUNT: usize>(file: &str, r: Result<[&str; COUNT], String>) {
    let src = std::fs::read_to_string(file).unwrap();
    assert_out(file, src.as_str(), r)
}

pub fn assert_inline<const COUNT: usize>(src: &str, r: Result<[&str; COUNT], String>) {
    let out = run("inline", src);
    match (&out, r) {
        (Ok(l), Ok(r)) => assert_eq!(l, &r.iter().map(|s| String::from(*s)).collect::<Vec<String>>(), "{}", src),
        (Err(l), Err(r)) => assert_eq!(l.to_string(), r, "{}", src),
        (Err(err), _) => panic!("{err:?}"),
        _ => assert!(false, "{}", src)
    }
}

pub fn assert_out<const COUNT: usize>(file_name: &str, src: &str, r: Result<[&str; COUNT], String>) {
    let out = run(file_name, src);
    match (&out, r) {
        (Ok(l), Ok(r)) => assert_eq!(l, &r.iter().map(|s| String::from(*s)).collect::<Vec<String>>(), "{}", file_name),
        (Err(l), Err(r)) => assert_eq!(l.to_string(), r, "{}", file_name),
        (Err(err), _) => panic!("{err:?}"),
        _ => assert!(false, "{}", src)
    }
}
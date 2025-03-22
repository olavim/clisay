mod inline;
mod file;

use std::{backtrace::Backtrace, panic};

use anyhow::Error;
use regex::Regex;

use crate::run;

const REGEX_EXPECTED_ERROR: &str = r"//[ ]*error[ ]*:[ ]*([^\n\r]+)[ ]*(\r\n|\n|\r)?";
const REGEX_EXPECTED_OUT: &str = r"//[ ]*expect[ ]*:[ ]*([^\n\r]+)[ ]*(\r\n|\n|\r)?";
const REGEX_ERROR_MESSAGE: &str = r"(.*)(\s*at .*:(\d+))+";

pub fn test_folder(folder: &str) {
    let mut test_count = 0;
    let mut errors = Vec::new();
    
    let default_panic = std::panic::take_hook();
    panic::set_hook(Box::new(|_| {
        // do nothing
        let backtrace = Backtrace::capture();
        println!("{}", backtrace);
    }));

    for entry in std::fs::read_dir(folder).unwrap() {
        let path = entry.unwrap().path();
        if path.is_file() && path.to_str().unwrap().ends_with(".say") {
            test_count += 1;

            let path_str = path.to_str().unwrap().replace("\\", "/");
            let status = match panic::catch_unwind(|| test_file(&path_str)) {
                Ok(_) => "\x1b[92mok\x1b[0m",
                Err(err) => {
                    if let Some(msg) = err.downcast_ref::<String>() {
                        errors.push(msg.clone());
                    } else if let Some(msg) = err.downcast_ref::<&str>() {
                        errors.push(msg.to_string());
                    };
                    "\x1b[91mfail\x1b[0m"
                }
            };
            println!("file {} ... {}", path_str, status);
        }
    }

    panic::set_hook(default_panic);

    if !errors.is_empty() {
        println!("\nfailures:");
        for err in &errors {
            println!("{}", err);
        }
        panic!("{} out of {} tests failed", errors.len(), test_count);
    }
}

pub fn test_file(file: &str) {
    let src = std::fs::read_to_string(file).unwrap();
    let split_regex = Regex::new(r"// @split(\r\n|\r|\n)").unwrap();
    let sections = split_regex.split(&src).collect::<Vec<&str>>();

    for section in sections {
        let result = run(file, section);
    
        if let Some(expected_error) = parse_expected_error(section) {
            match result {
                Ok(_) => panic!("Expected error: {}", expected_error),
                Err(err) => assert_eq!(parse_error_message(err), expected_error, "{}", file)
            }
        } else {
            let expected_out = parse_expected_output(section);
            match result {
                Ok(out) => assert_eq!(out, expected_out.iter().map(|s| String::from(*s)).collect::<Vec<String>>(), "{}", file),
                Err(err) => panic!("{err:?}")
            }
        }
    }
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

fn parse_error_message(err: Error) -> String {
    let error_regex = Regex::new(REGEX_ERROR_MESSAGE).unwrap();
    let err_msg = err.to_string();

    if !error_regex.is_match(&err_msg) {
        return err_msg;
    }
    
    let captures = error_regex.captures(&err_msg).unwrap();
    let message = captures.get(1).unwrap().as_str();
    let line = captures.get(3).unwrap().as_str().parse::<i8>().unwrap();
    format!("[line {}] {}", line, message)
}

fn parse_expected_error(src: &str) -> Option<String> {
    let expected_regex = Regex::new(REGEX_EXPECTED_ERROR).unwrap();
    let expected_errors = Regex::new(r"\r\n|\r|\n").unwrap().split(src)
        .enumerate()
        .map(|(line, str)| expected_regex.captures(str).map(|m| (line, m.get(1).unwrap().as_str())))
        .filter(|o| o.is_some())
        .map(|o| o.unwrap())
        .collect::<Vec<(usize, &str)>>();

    if expected_errors.len() > 1 {
        panic!("Only one error is allowed per test file");
    }

    // let expected_errors = expected_regex
    //     .captures_iter(src)
    //     .map(|c| (c.get(2).unwrap().as_str(), c.get(1).unwrap().as_str().parse::<i8>().unwrap()))
    //     .collect::<Vec<(&str, i8)>>();

    match expected_errors.first() {
        Some((line, msg)) => Some(format!("[line {}] {}", line + 1, msg)),
        None => None
    }
}

fn parse_expected_output(src: &str) -> Vec<&str> {
    let expected_regex = Regex::new(REGEX_EXPECTED_OUT).unwrap();
    expected_regex.captures_iter(src).map(|c| c.get(1).unwrap().as_str()).collect::<Vec<&str>>()
}
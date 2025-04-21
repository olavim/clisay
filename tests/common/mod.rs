use std::fmt;
use std::panic;

use anyhow::Error;
use clisay::run;
use clisay::Output;
use libtest_mimic::Failed;
use regex::Regex;

const REGEX_SKIP: &str = r"^\s*//.*//";
const REGEX_EXPECTED_ERROR: &str = r"//[ ]*error[ ]*:[ ]*([^\n\r]+)[ ]*(\r\n|\n|\r)?";
const REGEX_EXPECTED_OUT: &str = r"//[ ]*expect[ ]*:[ ]*([^\n\r]+)[ ]*(\r\n|\n|\r)?";
const REGEX_ERROR_MESSAGE: &str = r"(.*)(\s*at .*:(\d+))+";
const REGEX_SPLIT: &str = r"// @split(\r\n|\r|\n)";

fn eq_or_fail<T: PartialEq + fmt::Debug>(expected: T, actual: T) -> Result<(), Failed> {
    if expected != actual {
        return Err(format!("Expected {expected:?}, got {actual:?}").into());
    }
    Ok(())
}

pub fn test_file(file: &str) -> Result<(), Failed> {
    let skip_regex = Regex::new(REGEX_SKIP).unwrap();
    let src = std::fs::read_to_string(file).unwrap();
    let src = Regex::new(r"\r\n|\r|\n").unwrap()
        .split(&src)
        .filter(|&l| !skip_regex.is_match(l))
        .collect::<Vec<&str>>()
        .join("\n");

    let split_regex = Regex::new(REGEX_SPLIT).unwrap();
    let sections = split_regex.split(&src).collect::<Vec<&str>>();

    for section in sections {
        let result = run(file, section);
        let out = Output::get_output();
        let out = if out.len() > 0 && out[0] == "=== Bytecode ===" {
            let end = out.iter().position(|s| s == "================").unwrap() + 1;
            out[end..].to_vec()
        } else {
            out
        };

        if let Some(expected_error) = parse_expected_error(section) {
            match result {
                Ok(_) => return Err(format!("Expected error: {expected_error}").into()),
                Err(err) => eq_or_fail(expected_error, parse_error_message(err))?
            }
        } else if let Err(err) = result {
            println!("{}", err.backtrace());
            return Err(format!("Unexpected error: {err}").into());
        }

        let expected_out = parse_expected_output(section);
        eq_or_fail(expected_out.iter().map(|s| String::from(*s)).collect::<Vec<String>>(), out)?;

        Output::clear();
    }

    Ok(())
}

pub fn assert_inline<const COUNT: usize>(src: &str, r: Result<[&str; COUNT], String>) {
    let out = run("inline", src);
    match (&out, r) {
        (Ok(l), Ok(r)) => assert_eq!(l, &r.iter().map(|s| String::from(*s)).collect::<Vec<String>>(), "{src}"),
        (Err(l), Err(r)) => assert_eq!(l.to_string(), r, "{src}"),
        (Err(err), _) => panic!("{err:?}"),
        _ => assert!(false, "{src}")
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

    match expected_errors.first() {
        Some((line, msg)) => Some(format!("[line {}] {}", line + 1, msg)),
        None => None
    }
}

fn parse_expected_output(src: &str) -> Vec<&str> {
    let expected_regex = Regex::new(REGEX_EXPECTED_OUT).unwrap();
    expected_regex.captures_iter(src).map(|c| c.get(1).unwrap().as_str()).collect::<Vec<&str>>()
}
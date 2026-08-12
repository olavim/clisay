use std::fmt;
use std::panic;

use anyhow::Error;
use clisay::{run, run_with, RunConfig};
use clisay::Output;
use libtest_mimic::Failed;
use regex::Regex;

const REGEX_SKIP: &str = r"^\s*//.*//";
const REGEX_EXPECTED_ERROR: &str = r"//[ ]*error[ ]*:[ ]*([^\n\r]+)[ ]*(\r\n|\n|\r)?";
const REGEX_EXPECTED_OUT: &str = r"//[ ]*expect[ ]*:[ ]*([^\n\r]+)[ ]*(\r\n|\n|\r)?";
const REGEX_EXPECTED_ASM: &str = r"//[ ]*expect asm[ ]*:[ ]*(\r\n|\n|\r)(//[ ]*[^\n\r]+[ ]*(\r\n|\n|\r|$))*";
const REGEX_EXPECTED_ERROR_FULL: &str = r"//[ ]*error\(full\)[ ]*:[ ]*(\r\n|\n|\r)(//[^\n\r]*(\r\n|\n|\r|$))*";
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

    let name = std::path::Path::new(file).file_name().and_then(|n| n.to_str()).unwrap_or(file);

    // A pinned disassembly describes the shipped pipeline. Forcing adds the checks the pass
    // elided, so the stream it produces is a different one by design.
    let force_checks = std::env::var_os("CLISAY_FORCE_CHECKS").is_some();
    if std::env::var_os("CLISAY_FLOOR_ONLY").is_some() {
        for section in sections {
            check_floor(name, section)?;
        }
        return Ok(());
    }
    for section in sections {
        check_section(name, section, RunConfig { force_checks, ..RunConfig::default() }, !force_checks)?;
        check_section(name, section, RunConfig { optimize: false, force_checks, floor_only: false }, false)
            .map_err(|failure| Failed::from(format!("with the optimizer off: {failure:?}")))?;
    }

    Ok(())
}

/// Runs a section with every placed guard dropped.
fn check_floor(name: &str, section: &str) -> Result<(), Failed> {
    let expected = parse_expected_error_full(section).or_else(|| parse_expected_error(section));
    let Some(expected) = expected else { return Ok(()) };
    let config = RunConfig { floor_only: true, ..RunConfig::default() };
    let result = run_with(name, section, config);
    Output::clear();
    match result {
        Err(_) => Ok(()),
        Ok(_) => Err(format!(
            "the runtime floor accepted a program the compiler refuses; only a placed guard catches it\n  refused as: {expected}").into()),
    }
}

fn check_section(name: &str, section: &str, config: RunConfig, check_asm: bool) -> Result<(), Failed> {
    let result = run_with(name, section, config);
    let out = Output::get_output();
    let asm_end_pos = if out.len() > 0 && out[0] == "=== Bytecode ===" {
        Some(out.iter().position(|s| s == "================").unwrap() + 1)
    } else {
        None
    };
    let asm = asm_end_pos
        .map(|pos| out[1..pos - 1].to_vec())
        .map(|v| v.iter().flat_map(|line| line.lines()).map(String::from).collect());
    let out = match asm_end_pos {
         Some(pos) => out[pos..].to_vec(),
         None => out
    };

    if let Some(expected_full) = parse_expected_error_full(section) {
        match result {
            Ok(_) => return Err("Expected an error, but the program ran".into()),
            Err(err) => eq_or_fail(expected_full, err.to_string())?
        }
    } else if let Some(expected_error) = parse_expected_error(section) {
        match result {
            Ok(_) => return Err(format!("Expected error: {expected_error}").into()),
            Err(err) => eq_or_fail(expected_error, parse_error_message(err))?
        }
    } else if let Err(err) = result {
        println!("{}", err.backtrace());
        return Err(format!("Unexpected error: {err}").into());
    }

    let expected_out = parse_expected_output(section);
    eq_or_fail(expected_out.into_iter().map(String::from).collect::<Vec<String>>(), out)?;

    if check_asm {
        if let Some(expected_asm) = parse_expected_asm(section) {
            eq_or_fail(expected_asm.into_iter().map(String::from).collect::<Vec<String>>(), asm.unwrap())?;
        }
    }

    Output::clear();
    Ok(())
}

/// Runs a program with every elided check forced back on, so a proof the pass made is put on
/// trial. Answers the program's output, or the refusal a forced check raised.
pub fn run_forced(src: &str) -> Result<Vec<String>, String> {
    Output::clear();
    let config = RunConfig { force_checks: true, ..RunConfig::default() };
    let result = run_with("forced", src, config).map_err(|e| e.to_string());
    Output::clear();
    result
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
    let err_msg = err.to_string();

    // Errors render as `error: <message>` then a `--> file:line:col` locator.
    let error_regex = Regex::new(r"error: (.*)\n --> .*:(\d+):\d+").unwrap();
    match error_regex.captures(&err_msg) {
        Some(caps) => format!("[line {}] {}", caps.get(2).unwrap().as_str(), caps.get(1).unwrap().as_str()),
        None => err_msg,
    }
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

fn parse_expected_error_full(src: &str) -> Option<String> {
    let regex = Regex::new(REGEX_EXPECTED_ERROR_FULL).unwrap();
    return regex.captures(src).map(|c| c.get(0).unwrap().as_str()
        .lines()
        .skip(1)
        .map(|l| {
            let l = l.strip_prefix("//").unwrap_or(l);
            l.strip_prefix(' ').unwrap_or(l)
        })
        .collect::<Vec<&str>>()
        .join("\n"));
}

fn parse_expected_output(src: &str) -> Vec<&str> {
    let expected_regex = Regex::new(REGEX_EXPECTED_OUT).unwrap();
    expected_regex.captures_iter(src)
        .map(|c| c.get(1).unwrap().as_str())
        .collect::<Vec<&str>>()
}

fn parse_expected_asm(src: &str) -> Option<Vec<&str>> {
    let expected_regex = Regex::new(REGEX_EXPECTED_ASM).unwrap();
    return expected_regex.captures(src)
        .map(|c| c.get(0).unwrap().as_str()
            .lines()
            .skip(1)
            .map(|l| l.strip_prefix("//").unwrap_or(l).trim())
            .filter(|l| l.len() > 0)
            .collect::<Vec<&str>>());
}
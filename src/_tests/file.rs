use super::{test_file, test_folder};

#[test]
fn classes() {
    test_folder("tests/classes");
}

#[test]
fn super_kw() {
    test_folder("tests/super");
}

#[test]
fn functions() {
    test_file("tests/functions.say");
}

#[test]
fn closures() {
    test_file("tests/closures.say");
}

#[test]
fn statements() {
    test_file("tests/statements.say");
}

#[test]
fn assignments() {
    test_folder("tests/assignment");
}

#[test]
fn operators() {
    test_folder("tests/operators");
}

#[test]
fn perf() {
    // test_file("tests/perf.say");
    test_file("tests/bin.say");
}
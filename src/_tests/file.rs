use super::{test_file, test_folder};

#[test]
fn parser() {
    test_folder("tests/parser");
}

#[test]
fn classes() {
    test_folder("tests/classes");
}

#[test]
fn super_() {
    test_folder("tests/super");
}

#[test]
fn functions() {
    test_folder("tests/functions");
}

#[test]
fn closures() {
    test_folder("tests/closures");
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
fn gc() {
    test_folder("tests/gc");
}
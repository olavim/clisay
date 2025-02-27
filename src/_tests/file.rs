use super::assert_file;

#[test]
fn classes() {
    assert_file("tests/classes.say", Ok(["10", "100", "30", "51", "30", "100", "120", "changed"]));
}

#[test]
fn functions() {
    assert_file("tests/functions.say", Ok(["13", "13", "1", "2"]));
}
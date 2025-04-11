use clisay::Output;
use libtest_mimic::Failed;

#[macro_use]
extern crate test_collector;

mod common;

fn main() {
    let mut args = libtest_mimic::Arguments::from_args();
    args.filter = args.filter.map(|name| name.replace("_test_dummy", ""));
    if args.exact {
        args.filter = args.filter.map(|name| format!("[{}]", name));
        args.exact = false;
    }

    let tests = test_collector::TestCollection::collect_tests()
        .into_iter()
        .map(|test| {
            libtest_mimic::Trial::test(test.name, move ||
                (test.test_fn)(&test.path)
                    .inspect(|_| Output::clear())
                    .inspect_err(|_| Output::flush())
            ).with_kind(test.group)
        })
        .collect::<Vec<_>>();

    Output::enable_capture(true);
    libtest_mimic::run(&args, tests).exit();
}

#[test_resources("tests/res/parser")]
fn parser(file: &str) -> Result<(), Failed> {
    common::test_file(file)
}

#[test_resources("tests/res/classes")]
fn classes(resource: &str) -> Result<(), Failed> {
    common::test_file(resource)
}

#[test_resources("tests/res/super")]
fn super_kw(resource: &str) -> Result<(), Failed> {
    common::test_file(resource)
}

#[test_resources("tests/res/functions")]
fn functions(resource: &str) -> Result<(), Failed> {
    common::test_file(resource)
}

#[test_resources("tests/res/closures")]
fn closures(resource: &str) -> Result<(), Failed> {
    common::test_file(resource)
}

#[test_resources("tests/res/arrays")]
fn arrays(resource: &str) -> Result<(), Failed> {
    common::test_file(resource)
}

#[test_resources("tests/res/statements")]
fn statements(resource: &str) -> Result<(), Failed> {
    common::test_file(resource)
}

#[test_resources("tests/res/assignment")]
fn assignments(resource: &str) -> Result<(), Failed> {
    common::test_file(resource)
}

#[test_resources("tests/res/operators")]
fn operators(resource: &str) -> Result<(), Failed> {
    common::test_file(resource)
}

#[test_resources("tests/res/gc")]
fn gc(resource: &str) -> Result<(), Failed> {
    common::test_file(resource)
}

#[test_resources("tests/res/this")]
fn this(resource: &str) -> Result<(), Failed> {
    common::test_file(resource)
}

use clisay::internals::try_resolve;

const POINT: &str = "type Point { pub x; pub y; init(a, b) { this.x = a; this.y = b; } }";

#[test]
fn matcher_unknown_type_ref_errors() {
    let err = try_resolve("if is foo { x } <- v { }").unwrap_err();
    assert!(err.contains("not a type or trait"), "{err}");
}

#[test]
fn duplicate_binder_in_array_errors() {
    let err = try_resolve("if [x, x] <- v { }").unwrap_err();
    assert!(err.contains("bound more than once"), "{err}");
}

#[test]
fn duplicate_binder_across_as_errors() {
    let err = try_resolve("if x @ { x } <- v { }").unwrap_err();
    assert!(err.contains("bound more than once"), "{err}");
}

#[test]
fn or_matcher_differing_binders_errors() {
    let src = format!("{POINT} if is Point {{ x }} | {{ y }} <- v {{ }}");
    let err = try_resolve(&src).unwrap_err();
    assert!(err.contains("same names"), "{err}");
}

#[test]
fn or_matcher_same_binders_resolves() {
    let src = format!("{POINT} if is Point {{ x }} | {{ x }} <- v {{ }}");
    assert!(try_resolve(&src).is_ok());
}

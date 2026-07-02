use clisay::internals::try_resolve;

const POINT: &str = "type Point { pub x; pub y; init(a, b) { this.x = a; this.y = b; } }";

#[test]
fn matcher_unknown_type_ref_errors() {
    let err = try_resolve("match v { is foo { x } => 0 }").unwrap_err();
    assert!(err.contains("not a type or trait"), "{err}");
}

#[test]
fn duplicate_binder_in_array_errors() {
    let err = try_resolve("match v { [x, x] => 0 }").unwrap_err();
    assert!(err.contains("bound more than once"), "{err}");
}

#[test]
fn duplicate_binder_across_as_errors() {
    let err = try_resolve("match v { x @ { x } => 0 }").unwrap_err();
    assert!(err.contains("bound more than once"), "{err}");
}

#[test]
fn or_matcher_differing_binders_errors() {
    let src = format!("{POINT} match v {{ is Point {{ x }} | {{ y }} => 0 }}");
    let err = try_resolve(&src).unwrap_err();
    assert!(err.contains("same names"), "{err}");
}

#[test]
fn or_matcher_same_binders_resolves() {
    let src = format!("{POINT} match v {{ is Point {{ x }} | {{ x }} => 0 }}");
    assert!(try_resolve(&src).is_ok());
}

#[test]
fn binding_one_liner_outside_a_condition_errors() {
    let err = try_resolve("say b = d ~ { kind };").unwrap_err();
    assert!(err.contains("only allowed in a condition"), "{err}");
}

#[test]
fn binding_one_liner_in_a_condition_resolves() {
    assert!(try_resolve("if d ~ { kind, dx, dy } { }").is_ok());
    assert!(try_resolve("if a && d ~ { kind } { }").is_ok());
}

#[test]
fn binderless_one_liner_outside_a_condition_resolves() {
    let src = format!("{POINT} say b = d ~ is Point | is Point;");
    assert!(try_resolve(&src).is_ok());
}

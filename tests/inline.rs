use common::assert_inline;

mod common;

#[test]
fn ops() {
    assert_inline("print(1 + 2);", Ok(["3"]));
    assert_inline("print(1 - 2);", Ok(["-1"]));
    assert_inline("print(1.6 * 2);", Ok(["3.2"]));
    assert_inline("print(1 / 2);", Ok(["0.5"]));
    assert_inline("print(2 << 1);", Ok(["4"]));
    assert_inline("print(4 >> 1);", Ok(["2"]));
    assert_inline("print(1 == 1);", Ok(["true"]));
    assert_inline("print(1 != 2);", Ok(["true"]));
    assert_inline("print(6 & 3);", Ok(["2"]));
    assert_inline("print(6 | 3);", Ok(["7"]));
    assert_inline("print(\"foo\");", Ok(["foo"]));
    assert_inline("print(\"foo\" == \"foo\");", Ok(["true"]));
    assert_inline("print(\"foo\" == \"bar\");", Ok(["false"]));
    assert_inline("print(3 & 1);", Ok(["1"]));
    assert_inline("print(3 > 1);", Ok(["true"]));
    assert_inline("print(3 < 1);", Ok(["false"]));
    assert_inline("say var a = 2; a = 4; print(a);", Ok(["4"]));
    assert_inline("say var a = 2; a += 4; print(a);", Ok(["6"]));
    assert_inline("say var a = 2; a *= 4; print(a);", Ok(["8"]));
    assert_inline("say var a = 2; a /= 4; print(a);", Ok(["0.5"]));
    assert_inline("say var a = 2; a <<= 1; print(a);", Ok(["4"]));
    assert_inline("say var a = 2; a >>= 1; print(a);", Ok(["1"]));
    assert_inline("say var a = 3; a &= 1; print(a);", Ok(["1"]));
    assert_inline("say var a = 2; a |= 1; print(a);", Ok(["3"]));
    assert_inline("say var a = 1; a ^= 2; print(a);", Ok(["3"]));
    assert_inline("say var a = 3; a += a += 1; print(a);", Ok(["7"]));
}

#[test]
fn discard_params_take_their_slot_without_a_name() {
    // A `_` reserves no name, so repeating it is not a duplicate declaration.
    assert_inline("fn f(_, x, _) { print(x); } f(1, 2, 3);", Ok(["2"]));
}

#[test]
fn runtime_error_shows_call_stack_trace() {
    // A runtime error lists each active call frame beneath the source frame. `a` calls `b` in a
    // statement rather than returning it, so it keeps its frame and appears.
    let src = "fn b()! { return 1 + true; }\nfn a()! { b(); return 0; }\na();";
    let err = clisay::run("trace", src).err().expect("expected a runtime error").to_string();
    assert!(err.contains("\tat b ("), "{err}");
    assert!(err.contains("\tat a ("), "{err}");
}

#[test]
fn type_body_captures_its_declaring_frame() {
    // Each execution of the declaration builds its own type, so the methods capture separately.
    let src = "fn mk(v)! { type T { pub fn get(this)! { return v; } } return T { }; }\
               say a = mk(1); say b = mk(2); print(a.get()); print(b.get());";
    assert_inline(src, Ok(["1", "2"]));
}

#[test]
fn a_capturing_factory_survives_leaving_its_function() {
    let src = "fn mk(v)! { type T { pub n; init(this) { this.n = v; } } return T; }\
               say maker = mk(7); say other = mk(9); print(maker().n); print(other().n);";
    assert_inline(src, Ok(["7", "9"]));
}

#[test]
fn member_ids_are_capped_at_one_byte() {
    // A member id is one byte, and the factory takes one whether or not the type declares one.
    let fields = |n: usize| (0..n).map(|i| format!("pub f{i};")).collect::<Vec<_>>().join(" ");
    assert_inline(&format!("type Big {{ {} }} print(\"ok\");", fields(254)), Ok(["ok"]));
    assert_inline::<0>(&format!("type Big {{ {} }}", fields(255)), Err("Too many members in type 'Big'".to_string()));
}

/// Forcing puts the checks the pass elided back into the stream.
#[test]
fn forced_checks_leave_a_correct_program_alone() {
    assert_eq!(common::run_forced("say n = 5; print(n!);"), Ok(vec!["5".to_string()]));
    assert_eq!(common::run_forced("fn get(f)? { if (f) { return null; } return \"v\"; } say x? = get(false); if (x != null) { print(x!); }"),
        Ok(vec!["v".to_string()]));
}

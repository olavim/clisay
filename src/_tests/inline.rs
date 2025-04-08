use super::assert_inline;

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
    assert_inline("say a = 2; a = 4; print(a);", Ok(["4"]));
    assert_inline("say a = 2; a += 4; print(a);", Ok(["6"]));
    assert_inline("say a = 2; a *= 4; print(a);", Ok(["8"]));
    assert_inline("say a = 2; a /= 4; print(a);", Ok(["0.5"]));
    assert_inline("say a = 2; a <<= 1; print(a);", Ok(["4"]));
    assert_inline("say a = 2; a >>= 1; print(a);", Ok(["1"]));
    assert_inline("say a = 3; a &= 1; print(a);", Ok(["1"]));
    assert_inline("say a = 2; a |= 1; print(a);", Ok(["3"]));
    assert_inline("say a = 1; a ^= 2; print(a);", Ok(["3"]));
    assert_inline("say a = 3; a += a += 1; print(a);", Ok(["7"]));
}

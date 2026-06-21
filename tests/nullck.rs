use clisay::internals::nullck;

// An `unknown` dict read into a non-null local needs a barrier.
#[test]
fn unknown_into_non_null_local_records_barrier() {
    let barriers = nullck("say d = { a: 1 }; say k = d[\"a\"];");
    assert_eq!(barriers.len(), 1);
}

// `??` consumes the `unknown` itself, so the non-null result needs no barrier.
#[test]
fn coalesce_records_no_barrier() {
    let barriers = nullck("say d = { a: 1 }; say x = d[\"a\"] ?? 5;");
    assert_eq!(barriers.len(), 0);
}

// `!` on a possibly-null value is a manual barrier.
#[test]
fn assert_records_barrier() {
    let barriers = nullck("say maybe? = null; say w = maybe!;");
    assert_eq!(barriers.len(), 1);
}

// `!` on a proven non-null value is elided.
#[test]
fn assert_on_non_null_elided() {
    let barriers = nullck("say n = 5; say w = n!;");
    assert_eq!(barriers.len(), 0);
}

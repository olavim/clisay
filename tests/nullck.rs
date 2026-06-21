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

// A built-in with a non-null return resolves to a value, so no barrier guards the slot.
#[test]
fn builtin_non_null_return_needs_no_barrier() {
    let barriers = nullck("say t = time();");
    assert_eq!(barriers.len(), 0);
}

// A native method with a non-null return resolves precisely, so its result needs no barrier.
#[test]
fn native_method_non_null_return_needs_no_barrier() {
    let barriers = nullck("say n = [1, 2].length();");
    assert_eq!(barriers.len(), 0);
}

// An unknown method on a dynamic receiver still crosses into a non-null slot with a barrier.
#[test]
fn unknown_method_still_records_barrier() {
    let barriers = nullck("say n = [1, 2].notNative();");
    assert_eq!(barriers.len(), 1);
}

// An unknown value passed to a non-null native parameter is guarded by a barrier.
#[test]
fn unknown_native_arg_records_barrier() {
    let barriers = nullck("say d = { a: 1 }; gcStress(d[\"a\"]);");
    assert_eq!(barriers.len(), 1);
}

// An unknown value passed to a non-null user parameter is guarded by a barrier.
#[test]
fn unknown_user_arg_records_barrier() {
    let barriers = nullck("fn f(x) { } say d = { a: 1 }; f(d[\"a\"]);");
    assert_eq!(barriers.len(), 1);
}

// An unknown value stored into a non-null field is guarded by a barrier.
#[test]
fn unknown_into_non_null_field_records_barrier() {
    let barriers = nullck("type T { pub mut v; init() { this.v = 1; } } say t = T(); say d = { k: 1 }; t.v = d[\"k\"];");
    assert_eq!(barriers.len(), 1);
}

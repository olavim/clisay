//! Value hashing and equality.

use std::hash::Hasher;

use fnv::FnvHashSet;
use rustc_hash::FxHasher as WordHasher;

use super::objects::{self, ObjArray, ObjInstance, ObjType, ObjectKind, UNDECLARED};
use super::value::{Value, ValueKind};

#[inline(always)]
pub fn quick_eq(a: Value, b: Value) -> Option<bool> {
    if a.is_number() && b.is_number() {
        return Some(a.as_number() == b.as_number());
    }
    if a == b || !a.is_object() || !b.is_object() {
        return Some(a == b);
    }
    None
}

#[inline]
pub fn shallow_eq(a: Value, b: Value) -> Option<bool> {
    if let Some(equal) = quick_eq(a, b) {
        return Some(equal);
    }
    let (ValueKind::Object(kind), ValueKind::Object(other)) = (a.kind(), b.kind()) else { return Some(false) };
    // A `Ref` is the one value with identity, and the two are not the same object.
    if kind != other || !compares_deeply(kind) || objects::is_ref(a) || objects::is_ref(b) {
        return Some(false);
    }
    None
}

pub fn deep_eq(a: Value, b: Value, depth_limit: usize) -> bool {
    let mut pending = Pending::default();
    let mut next = Some((a, b, 0));
    while let Some((a, b, depth)) = next.take().or_else(|| pending.queue.pop()) {
        if !compare_level(a, b, depth, depth_limit, &mut pending) {
            return false;
        }
    }
    true
}

#[derive(Default)]
struct Pending {
    queue: Vec<(Value, Value, usize)>,
    queued_shared: FnvHashSet<(Value, Value)>,
}

impl Pending {
    fn push(&mut self, a: Value, b: Value, depth: usize) {
        if objects::is_shared(a) && objects::is_shared(b) && !self.queued_shared.insert((a, b)) {
            return;
        }
        self.queue.push((a, b, depth));
    }
}

/// Compares `a` and `b` by type and length, and compares their values or queues the value comparison into `pending`.
fn compare_level(a: Value, b: Value, depth: usize, depth_limit: usize, pending: &mut Pending) -> bool {
    debug_assert!(depth < depth_limit, "depth limit reached");
    let ValueKind::Object(kind) = a.kind() else { unreachable!("only objects compare deeply") };
    if is_container_kind(kind) {
        if let (Some(x), Some(y)) = (objects::cached_hash(a), objects::cached_hash(b)) {
            if x != y {
                return false;
            }
        }
    }
    let next = depth + 1;
    let mut compare_or_queue = |x: Value, y: Value| match shallow_eq(x, y) {
        Some(equal) => equal,
        None => {
            pending.push(x, y, next);
            true
        },
    };
    match kind {
        ObjectKind::Array => {
            let (xs, ys) = unsafe { (ObjArray::elements(a.as_object().as_array_ptr()), ObjArray::elements(b.as_object().as_array_ptr())) };
            xs.len() == ys.len() && xs.iter().zip(ys).all(|(&x, &y)| compare_or_queue(x, y))
        },
        ObjectKind::Dict => {
            let (xs, ys) = unsafe { (&(*a.as_object().as_dict_ptr()).entries, &(*b.as_object().as_dict_ptr()).entries) };
            xs.len() == ys.len() && xs.iter().all(|(key, &x)| ys.get(key).is_some_and(|&y| compare_or_queue(x, y)))
        },
        ObjectKind::Instance => {
            let (x, y) = unsafe { (&*a.as_object().as_instance_ptr(), &*b.as_object().as_instance_ptr()) };
            same_declaration(x.ty, y.ty) && instance_fields(a).iter().zip(instance_fields(b)).all(|(&x, &y)| compare_or_queue(x, y))
        },
        ObjectKind::Type => same_declaration(a.as_object().as_type_ptr(), b.as_object().as_type_ptr()),
        ObjectKind::String
            | ObjectKind::Function
            | ObjectKind::NativeFunction
            | ObjectKind::BoundMethod
            | ObjectKind::Closure => unreachable!("compare_level shouldn't be called for kinds that compare by identity"),
    }
}

fn same_declaration(a: *mut ObjType, b: *mut ObjType) -> bool {
    declaration_key(a) == declaration_key(b)
}

fn declaration_key(ty: *mut ObjType) -> u64 {
    match unsafe { (*ty).id } {
        UNDECLARED => ty as u64,
        id => id as u64,
    }
}

fn is_container_kind(kind: ObjectKind) -> bool {
    matches!(kind, ObjectKind::Array | ObjectKind::Dict | ObjectKind::Instance)
}

fn compares_deeply(kind: ObjectKind) -> bool {
    is_container_kind(kind) || kind == ObjectKind::Type
}

fn instance_fields<'a>(value: Value) -> &'a [Value] {
    let instance = value.as_object().as_instance_ptr();
    unsafe { &ObjInstance::values(instance)[..(*(*instance).ty).field_count as usize] }
}

pub fn contains_itself(value: Value) -> bool {
    let mut on_path = FnvHashSet::default();
    let mut searched = FnvHashSet::default();
    let mut pending = vec![(value, false)];
    while let Some((value, exiting)) = pending.pop() {
        if exiting {
            on_path.remove(&value);
            searched.insert(value);
            continue;
        }
        if !is_container_not_ref(value) || searched.contains(&value) {
            continue;
        }
        if !on_path.insert(value) {
            return true;
        }
        pending.push((value, true));
        push_contained(value, &mut pending, |element| (element, false));
    }
    false
}

/// Whether `value` is the unshared `target`, or contains it through unshared containers.
#[cfg(debug_assertions)]
pub fn is_or_contains_unshared(value: Value, target: Value, depth_limit: usize) -> bool {
    if value != target && !is_container_not_ref(value) {
        return false;
    }
    let mut pending = vec![(value, 0usize)];
    while let Some((value, depth)) = pending.pop() {
        if value == target {
            return true;
        }
        if !is_container_not_ref(value) || objects::is_shared(value) {
            continue;
        }
        assert!(depth < depth_limit, "depth limit reached");
        push_contained(value, &mut pending, |element| (element, depth + 1));
    }
    false
}

fn is_container_not_ref(value: Value) -> bool {
    matches!(value.kind(), ValueKind::Object(kind) if is_container_kind(kind)) && !objects::is_ref(value)
}

/// Queues every value a container contains, including dict keys.
fn push_contained<T>(value: Value, pending: &mut Vec<T>, entry: impl Fn(Value) -> T) {
    let ValueKind::Object(kind) = value.kind() else { return };
    match kind {
        ObjectKind::Array => pending.extend(unsafe { ObjArray::elements(value.as_object().as_array_ptr()) }.iter().map(|&element| entry(element))),
        ObjectKind::Dict => pending.extend(unsafe { &(*value.as_object().as_dict_ptr()).entries }.iter()
            .flat_map(|(key, &element)| [entry(key.0), entry(element)])),
        _ => pending.extend(instance_fields(value).iter().map(|&element| entry(element))),
    }
}

pub fn deep_hash(value: Value) -> u32 {
    let ValueKind::Object(kind) = value.kind() else { return scalar_hash(value) };
    if !is_container_kind(kind) || objects::is_ref(value) {
        return object_hash(value, kind);
    }
    if let Some(hash) = objects::cached_hash(value) {
        return hash;
    }
    let start = object_hash(value, kind);
    let hash = match kind {
        ObjectKind::Array => hash_words([start as u64].into_iter()
            .chain(unsafe { ObjArray::elements(value.as_object().as_array_ptr()) }.iter().map(|&element| shallow_hash(element) as u64))),
        // Entries have no order, so their hashes are summed rather than chained.
        ObjectKind::Dict => unsafe { &(*value.as_object().as_dict_ptr()).entries }.iter()
            .fold(start, |hash, (key, &element)| hash.wrapping_add(hash_words([shallow_hash(key.0) as u64, shallow_hash(element) as u64]))),
        _ => hash_words([start as u64].into_iter().chain(instance_fields(value).iter().map(|&element| shallow_hash(element) as u64))),
    };
    objects::cache_hash(value, hash);
    hash
}

fn shallow_hash(value: Value) -> u32 {
    match value.kind() {
        ValueKind::Object(kind) => object_hash(value, kind),
        _ => scalar_hash(value),
    }
}

fn scalar_hash(value: Value) -> u32 {
    if value.is_number() {
        // `-0.0 == 0.0`, so the two hash alike.
        let number = value.as_number();
        return hash_words([if number == 0.0 { 0 } else { number.to_bits() }]);
    }
    hash_words([value.to_bits()])
}

fn object_hash(value: Value, kind: ObjectKind) -> u32 {
    unsafe {
        match kind {
            ObjectKind::Array => hash_words([0x0a, (*value.as_object().as_array_ptr()).len as u64]),
            ObjectKind::Dict => hash_words([0x0d, (*value.as_object().as_dict_ptr()).entries.len() as u64]),
            ObjectKind::Instance if objects::is_ref(value) => hash_words([value.to_bits()]),
            ObjectKind::Instance => hash_words([0x1a, declaration_key((*value.as_object().as_instance_ptr()).ty)]),
            ObjectKind::Type => hash_words([0x7e, declaration_key(value.as_object().as_type_ptr())]),
            ObjectKind::String
                | ObjectKind::Function
                | ObjectKind::NativeFunction
                | ObjectKind::BoundMethod
                | ObjectKind::Closure => hash_words([value.to_bits()]),
        }
    }
}

/// Hashes a sequence of words with FxHash, the hasher rustc uses, and folds the 64-bit result into
/// the 32 bits a container's hash cache has room for.
fn hash_words(words: impl IntoIterator<Item = u64>) -> u32 {
    let mut hasher = WordHasher::default();
    for word in words {
        hasher.write_u64(word);
    }
    let hash = hasher.finish();
    (hash >> 32) as u32 ^ hash as u32
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::gc::Gc;
    use crate::core::objects::{ObjArray, ObjDict};
    use crate::core::value::DictKey;

    /// A program cannot build a cycle, so one is built by hand.
    fn two_cycles(gc: &mut Gc) -> (Value, Value) {
        let a = gc.alloc_array(&[], 0);
        let b = gc.alloc_array(&[], 0);
        unsafe {
            ObjArray::push(a, Value::from(a));
            ObjArray::push(b, Value::from(b));
        }
        (Value::from(a), Value::from(b))
    }

    #[test]
    #[cfg(debug_assertions)]
    #[should_panic(expected = "depth limit reached")]
    fn comparing_a_value_that_contains_itself_is_caught_rather_than_run_forever() {
        let mut gc = Gc::new();
        let (a, b) = two_cycles(&mut gc);
        deep_eq(a, b, gc.object_count());
    }

    fn collisions(values: impl IntoIterator<Item = Value>) -> usize {
        let mut seen = std::collections::HashSet::new();
        values.into_iter().filter(|&value| !seen.insert(deep_hash(value))).count()
    }

    #[test]
    fn distinct_keys_of_each_shape_rarely_collide() {
        let mut gc = Gc::new();
        let whole = collisions((0..10_000).map(|i| Value::from(i as f64)));
        let fractions = collisions((0..10_000).map(|i| Value::from(i as f64 / 7.0)));
        let strings = collisions((0..10_000).map(|i| Value::from(gc.intern(format!("key{i}")))));
        let pairs = collisions((0..10_000).map(|i| {
            let pair = vec![Value::from((i / 100) as f64), Value::from((i % 100) as f64)];
            Value::from(gc.alloc_array(&pair, pair.len()))
        }));
        let name = Value::from(gc.intern("k"));
        let dicts = collisions((0..10_000).map(|i| {
            let entries = [(DictKey(name), Value::from(i as f64))].into_iter().collect();
            Value::from(gc.alloc(ObjDict::new(entries)))
        }));
        eprintln!("collisions: whole {whole}, fractions {fractions}, strings {strings}, pairs {pairs}, dicts {dicts}");
        for count in [whole, fractions, strings, pairs, dicts] {
            assert!(count <= 2, "a shape of key collides {count} times in 10,000");
        }
    }

    #[test]
    fn a_value_that_contains_itself_is_found() {
        let mut gc = Gc::new();
        let (a, _) = two_cycles(&mut gc);
        assert!(contains_itself(a));
    }

    #[test]
    fn an_array_that_contains_the_same_element_twice_does_not_contain_itself() {
        let mut gc = Gc::new();
        let element = Value::from(gc.alloc_array(&[], 0));
        assert!(!contains_itself(Value::from(gc.alloc_array(&[element, element], 2))));
    }
}

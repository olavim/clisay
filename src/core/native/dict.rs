use crate::core::gc::Gc;
use crate::core::host::Host;
use crate::core::objects::{NativeFn, ObjNativeFn, ObjString};
use crate::core::value::Value;

use super::NativeType;

/// The `dict` method surface, reached via `.` (data is reached via `[]`). The
/// method set is deliberately small; none allocate, so they need no extra rooting.
pub struct NativeDict;

impl NativeDict {
    /// `d.size()` — the number of entries.
    fn size(host: &mut dyn Host, target: Value) -> Result<(), anyhow::Error> {
        let dict = unsafe { &*target.as_object().as_dict_ptr() };
        host.push(Value::from(dict.entries.len() as f64));
        Ok(())
    }

    /// `d.has(key)` — whether `key` is present (by value, no coercion).
    fn has(host: &mut dyn Host, target: Value, key: Value) -> Result<(), anyhow::Error> {
        let dict = unsafe { &*target.as_object().as_dict_ptr() };
        host.push(if dict.entries.contains_key(&key) { Value::TRUE } else { Value::FALSE });
        Ok(())
    }

    /// `d.remove(key)` — removes `key`, yielding its value or `null` if absent.
    fn remove(host: &mut dyn Host, target: Value, key: Value) -> Result<(), anyhow::Error> {
        let dict = unsafe { &mut *target.as_object().as_dict_ptr() };
        let removed = dict.entries.remove(&key).unwrap_or(Value::NULL);
        host.push(removed);
        Ok(())
    }
}

impl NativeType for NativeDict {
    fn get_name(&self) -> &'static str {
        "dict"
    }

    fn methods(&self, gc: &mut Gc) -> Vec<(*mut ObjString, ObjNativeFn)> {
        let size = gc.intern("size");
        let has = gc.intern("has");
        let remove = gc.intern("remove");
        vec![
            (size, ObjNativeFn::new(size, 0, (|host, target, _args| Self::size(host, target)) as NativeFn)),
            (has, ObjNativeFn::new(has, 1, (|host, target, args| Self::has(host, target, args[0])) as NativeFn)),
            (remove, ObjNativeFn::new(remove, 1, (|host, target, args| Self::remove(host, target, args[0])) as NativeFn)),
        ]
    }
}

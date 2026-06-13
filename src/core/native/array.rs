use anyhow::bail;

use crate::core::gc::{Gc, GcTraceable};
use crate::core::host::Host;
use crate::core::objects::{NativeFn, ObjNativeFn, ObjString};
use crate::core::value::{Value, ValueKind};

use super::NativeType;

pub struct NativeArray;

impl NativeArray {
    /// Validates an array index against `len`: it must be a non-negative integer
    /// number in bounds. Shared by `get` and `set` so reads and writes reject the
    /// same inputs identically.
    fn checked_index(index: Value, len: usize) -> Result<usize, anyhow::Error> {
        if !matches!(index.kind(), ValueKind::Number) {
            bail!("Invalid array index: {}", index.fmt());
        }

        let i = index.as_number();
        if i.fract() != 0.0 {
            bail!("Invalid array index: {}", index.fmt());
        }

        if i < 0.0 || i >= len as f64 {
            bail!("Array index out of bounds: {}", index.fmt());
        }

        Ok(i as usize)
    }

    fn get(host: &mut dyn Host, target: Value, prop: Value) -> Result<(), anyhow::Error> {
        let array = unsafe { &*target.as_object().as_array_ptr() };
        let index = Self::checked_index(prop, array.values.len())?;
        host.push(array.values[index]);
        Ok(())
    }

    fn set(host: &mut dyn Host, target: Value, index: Value, value: Value) -> Result<(), anyhow::Error> {
        let array = unsafe { &mut *target.as_object().as_array_ptr() };
        let i = Self::checked_index(index, array.values.len())?;
        array.values[i] = value;
        host.push(value);
        Ok(())
    }

    fn length(host: &mut dyn Host, target: Value) -> Result<(), anyhow::Error> {
        let target = unsafe { &*target.as_object().as_array_ptr() };
        host.push(Value::from(target.values.len() as f64));
        Ok(())
    }
}

impl NativeType for NativeArray {
    fn get_name(&self) -> &'static str {
        "Array"
    }

    fn methods(&self, gc: &mut Gc) -> Vec<(*mut ObjString, ObjNativeFn)> {
        let length = gc.intern("length");
        vec![(length, ObjNativeFn::new(length, 0, (|host, target, _args| Self::length(host, target)) as NativeFn))]
    }

    fn getter(&self, gc: &mut Gc) -> Option<ObjNativeFn> {
        let name = gc.intern("get");
        Some(ObjNativeFn::new(name, 1, (|host, target, args| Self::get(host, target, args[0])) as NativeFn))
    }

    fn setter(&self, gc: &mut Gc) -> Option<ObjNativeFn> {
        let name = gc.intern("set");
        Some(ObjNativeFn::new(name, 2, (|host, target, args| Self::set(host, target, args[0], args[1])) as NativeFn))
    }
}
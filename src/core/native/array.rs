use anyhow::bail;

use crate::core::gc::{Gc, GcTraceable};
use crate::core::host::Host;
use crate::core::objects::{NativeFn, ObjArray, ObjNativeFn, ObjString};
use crate::core::value::{Value, ValueKind};

use super::{NativeType, NativeTypeBuilder};

pub struct NativeArray;

impl NativeArray {
    fn checked_index(index: Value, len: usize) -> Result<usize, anyhow::Error> {
        if let Some(at) = Self::element_index(index, len) {
            return Ok(at);
        }
        match index.kind() {
            ValueKind::Number if index.as_number().fract() == 0.0 =>
                bail!("Array index out of bounds: {}", index.fmt()),
            _ => bail!("Invalid array index: {}", index.fmt()),
        }
    }

    pub(crate) fn element_index(index: Value, len: usize) -> Option<usize> {
        if !index.is_number() {
            return None;
        }
        let i = index.as_number();
        // A length fits in a u32, so an index inside it converts exactly.
        if !(i >= 0.0 && i < len as u32 as f64) {
            return None;
        }
        let at = unsafe { i.to_int_unchecked::<u32>() };
        (at as f64 == i).then_some(at as usize)
    }

    fn get(host: &mut dyn Host, target: Value, prop: Value) -> Result<(), anyhow::Error> {
        let array = target.as_object().as_array_ptr();
        let index = Self::checked_index(prop, unsafe { (*array).len } as usize)?;
        host.push(unsafe { ObjArray::get(array, index) });
        Ok(())
    }

    fn set(host: &mut dyn Host, target: Value, index: Value, value: Value) -> Result<(), anyhow::Error> {
        let i = Self::checked_index(index, unsafe { (*target.as_object().as_array_ptr()).len } as usize)?;
        let value = host.share_into(target, value)?;
        unsafe { ObjArray::set(target.as_object().as_array_ptr(), i, value) };
        host.push(value);
        Ok(())
    }

    fn length(host: &mut dyn Host, target: Value) -> Result<(), anyhow::Error> {
        let len = unsafe { (*target.as_object().as_array_ptr()).len };
        host.push(Value::from(len as f64));
        Ok(())
    }

    fn push(host: &mut dyn Host, target: Value, value: Value) -> Result<(), anyhow::Error> {
        let value = host.share_into(target, value)?;
        unsafe { ObjArray::push(target.as_object().as_array_ptr(), value) };
        host.push(Value::NULL);
        Ok(())
    }
}

impl NativeTypeBuilder for NativeArray {
    fn kind(&self) -> NativeType {
        NativeType::Array
    }

    fn methods(&self, gc: &mut Gc) -> Vec<(*mut ObjString, ObjNativeFn)> {
        let length = gc.intern("length");
        let push = gc.intern("push");
        vec![
            (length, ObjNativeFn::new(length, 0, (|host, target, _args| Self::length(host, target)) as NativeFn)),
            (push, ObjNativeFn::new(push, 1, (|host, target, args| Self::push(host, target, args[0])) as NativeFn)),
        ]
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
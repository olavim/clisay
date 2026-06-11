use anyhow::bail;

use crate::core::gc::{Gc, GcTraceable};
use crate::core::host::Host;
use crate::core::objects::{NativeFn, ObjNativeFn, ObjString};
use crate::core::value::{Value, ValueKind};

use super::NativeType;

pub struct NativeArray;

impl NativeArray {
    fn get(host: &mut dyn Host, target: Value, prop: Value) -> Result<(), anyhow::Error> {
        if !matches!(prop.kind(), ValueKind::Number) {
            bail!("Invalid array index: {}", prop.fmt());
        };

        let index = prop.as_number();

        if index.fract() != 0.0 {
            bail!("Invalid array index: {}", prop.fmt());
        }

        let array = unsafe { &*target.as_object().as_array_ptr() };

        if index < 0.0 || index >= array.values.len() as f64 {
            bail!("Array index out of bounds: {}", prop.fmt());
        }

        host.push(array.values[index as usize]);
        Ok(())
    }

    fn set(host: &mut dyn Host, target: Value, index: Value, value: Value) -> Result<(), anyhow::Error> {
        let target = unsafe { &mut *target.as_object().as_array_ptr() };
        let index = index.as_number() as usize;
        target.values[index] = value;
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
use std::collections::HashMap;

use anyhow::bail;

use crate::vm::gc::{Gc, GcTraceable};
use crate::vm::objects::{NativeFn, ObjNativeFn, ObjString};
use crate::vm::value::{Value, ValueKind};

use super::NativeType;

pub struct NativeArray;

impl NativeArray {
    fn get(target: Value, prop: Value) -> Result<Value, anyhow::Error> {
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

        Ok(array.values[index as usize])
    }

    fn set(target: Value, index: Value, value: Value) -> Result<Value, anyhow::Error> {
        let target = unsafe { &mut *target.as_object().as_array_ptr() };
        let index = index.as_number() as usize;
        target.values[index] = value;
        Ok(value)
    }

    fn length(target: Value) -> Result<Value, anyhow::Error> {
        let target = unsafe { &*target.as_object().as_array_ptr() };
        Ok(Value::from(target.values.len() as f64))
    }
}

impl NativeType for NativeArray {
    fn get_name(&self) -> &'static str {
        "Array"
    }

    fn instance_methods(&self, gc: &mut Gc) -> HashMap<*mut ObjString, ObjNativeFn> {
        let methods = vec![
            (gc.intern("length"), 0, (|_vm, target, _args| Self::length(target)) as NativeFn),
            (gc.preset_identifiers.get, 1, (|_vm, target, args| Self::get(target, args[0])) as NativeFn),
            (gc.preset_identifiers.set, 2, (|_vm, target, args| Self::set(target, args[0], args[1])) as NativeFn),
        ]
            .into_iter()
            .map(|(name, arity, function)| (name, ObjNativeFn::new(name, arity, function)))
            .collect::<HashMap<_, _>>();

        methods
    }
}
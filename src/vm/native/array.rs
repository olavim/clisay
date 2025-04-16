use std::collections::HashMap;

use anyhow::bail;

use crate::vm::gc::{Gc, GcTraceable};
use crate::vm::objects::{NativeFn, ObjNativeFn, ObjString};
use crate::vm::value::{Value, ValueKind};
use crate::vm::vm::Vm;

use super::NativeType;

pub struct NativeArray;

impl NativeArray {
    fn get(vm: &mut Vm, target: Value, prop: Value) -> Result<(), anyhow::Error> {
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

        vm.stack.push(array.values[index as usize]);
        Ok(())
    }

    fn set(vm: &mut Vm, target: Value, index: Value, value: Value) -> Result<(), anyhow::Error> {
        let target = unsafe { &mut *target.as_object().as_array_ptr() };
        let index = index.as_number() as usize;
        target.values[index] = value;
        vm.stack.push(value);
        Ok(())
    }

    fn length(vm: &mut Vm, target: Value) -> Result<(), anyhow::Error> {
        let target = unsafe { &*target.as_object().as_array_ptr() };
        vm.stack.push(Value::from(target.values.len() as f64));
        Ok(())
    }
}

impl NativeType for NativeArray {
    fn get_name(&self) -> &'static str {
        "Array"
    }

    fn instance_methods(&self, gc: &mut Gc) -> HashMap<*mut ObjString, ObjNativeFn> {
        let methods = vec![
            (gc.intern("length"), 0, (|vm, target, _args| Self::length(vm, target)) as NativeFn),
            (gc.preset_identifiers.get, 1, (|vm, target, args| Self::get(vm, target, args[0])) as NativeFn),
            (gc.preset_identifiers.set, 2, (|vm, target, args| Self::set(vm, target, args[0], args[1])) as NativeFn),
        ]
            .into_iter()
            .map(|(name, arity, function)| (name, ObjNativeFn::new(name, arity, function)))
            .collect::<HashMap<_, _>>();

        methods
    }
}
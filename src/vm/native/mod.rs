use std::collections::HashMap;

use anyhow::bail;

use super::gc::{Gc, GcTraceable};
use super::objects::{ClassMember, NativeFn, ObjClass, ObjNativeFn, ObjString};
use super::value::{Value, ValueKind};

pub trait NativeType {
    fn get_name(&self) -> &'static str;
    fn init(&self, gc: &mut Gc) -> HashMap<*mut ObjString, ObjNativeFn>;

    fn build_class(&self, gc: &mut Gc) -> ObjClass {
        let mut class = ObjClass::new(gc.intern(self.get_name()));
        let methods = self.init(gc);

        let mut member_id = 0;
        for (name, method) in methods {
            class.members.insert(name, ClassMember::Method(member_id));
            class.methods.insert(member_id, gc.alloc(method).into());
            member_id += 1;
        }

        class
    }
}

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

    fn init(&self, gc: &mut Gc) -> HashMap<*mut ObjString, ObjNativeFn> {
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
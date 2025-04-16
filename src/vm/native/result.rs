use std::collections::HashMap;

use anyhow::bail;

use crate::vm::gc::Gc;
use crate::vm::objects::{NativeFn, ObjNativeFn, ObjResult, ObjString};
use crate::vm::value::Value;

use super::NativeType;

pub struct NativeResult;

impl NativeResult {
    fn is_ok(target: Value) -> bool {
        !Self::is_error(target)
    }

    fn is_error(target: Value) -> bool {
        unsafe { (*target.as_object().as_result_ptr()).error }
    }

    fn unwrap(target: Value) -> Result<Value, anyhow::Error> {
        let result = unsafe { &*target.as_object().as_result_ptr() };
        if result.error {
            bail!("Unwrapping an error result");
        } else {
            Ok(result.value)
        }
    }
}

impl NativeType for NativeResult {
    fn get_name(&self) -> &'static str {
        "Result"
    }

    fn instance_methods(&self, gc: &mut Gc) -> HashMap<*mut ObjString, ObjNativeFn> {
        let methods = vec![
            (gc.intern("isOk"), 0, (|_vm, target, _args| Ok(Value::from(Self::is_ok(target)))) as NativeFn),
            (gc.intern("isError"), 0, (|_vm, target, _args| Ok(Value::from(Self::is_error(target)))) as NativeFn),
            (gc.intern("unwrap"), 0, (|_vm, target, _args| Self::unwrap(target)) as NativeFn)
        ]
            .into_iter()
            .map(|(name, arity, function)| (name, ObjNativeFn::new(name, arity, function)))
            .collect::<HashMap<_, _>>();

        methods
    }
}

pub struct NativeOk;
pub struct NativeError;

trait ResultInit {
    fn initializer(gc: &mut Gc, value: Value) -> Value {
        let result = gc.alloc(ObjResult::new(value, Self::is_error()));
        Value::from(result)
    }

    fn is_error() -> bool;
    fn name() -> &'static str;
}

impl ResultInit for NativeOk {
    fn is_error() -> bool { false }
    fn name() -> &'static str { "Ok" }
}

impl ResultInit for NativeError {
    fn is_error() -> bool { true }
    fn name() -> &'static str { "Error" }
}

impl<T: ResultInit> NativeType for T {
    fn get_name(&self) -> &'static str {
        Self::name()
    }

    fn instance_methods(&self, gc: &mut Gc) -> HashMap<*mut ObjString, ObjNativeFn> {
        let methods = vec![
            (gc.preset_identifiers.init, 1, (|vm, _target, args| Ok(Self::initializer(&mut vm.gc, args[0]))) as NativeFn)
        ]
            .into_iter()
            .map(|(name, arity, function)| (name, ObjNativeFn::new(name, arity, function)))
            .collect::<HashMap<_, _>>();

        methods
    }
}

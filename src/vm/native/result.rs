use std::collections::HashMap;

use anyhow::bail;

use crate::vm::gc::Gc;
use crate::vm::objects::{NativeFn, ObjNativeFn, ObjResult, ObjString};
use crate::vm::value::Value;
use crate::vm::vm::Vm;

use super::NativeType;

pub struct NativeResult;

impl NativeResult {
    fn is_ok(vm: &mut Vm, target: Value) -> Result<(), anyhow::Error> {
        let error = unsafe { (*target.as_object().as_result_ptr()).error };
        vm.stack.push(Value::from(!error));
        Ok(())
    }

    fn is_error(vm: &mut Vm, target: Value) -> Result<(), anyhow::Error> {
        let error = unsafe { (*target.as_object().as_result_ptr()).error };
        vm.stack.push(Value::from(error));
        Ok(())
    }

    fn unwrap(vm: &mut Vm, target: Value) -> Result<(), anyhow::Error> {
        let result = unsafe { &*target.as_object().as_result_ptr() };
        if result.error {
            bail!("Unwrapping an error result");
        } else {
            vm.stack.push(result.value);
            Ok(())
        }
    }

    fn catch(vm: &mut Vm, target: Value, callback: Value) -> Result<(), anyhow::Error> {
        let result = unsafe { &*target.as_object().as_result_ptr() };
        if result.error {
            vm.stack.push(callback);
            vm.stack.push(result.value);
            vm.call(1, callback)?;
        } else {
            vm.stack.push(result.value);
        }
        Ok(())
    }
}

impl NativeType for NativeResult {
    fn get_name(&self) -> &'static str {
        "Result"
    }

    fn instance_methods(&self, gc: &mut Gc) -> HashMap<*mut ObjString, ObjNativeFn> {
        let methods = vec![
            (gc.intern("isOk"), 0, (|vm, target, _args| Self::is_ok(vm, target)) as NativeFn),
            (gc.intern("isError"), 0, (|vm, target, _args|Self::is_error(vm, target)) as NativeFn),
            (gc.intern("unwrap"), 0, (|vm, target, _args| Self::unwrap(vm, target)) as NativeFn),
            (gc.intern("catch"), 1, (|vm, target, args| Self::catch(vm, target, args[0])) as NativeFn)
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
    fn initializer(vm: &mut Vm, value: Value) -> Result<(), anyhow::Error> {
        let result = vm.gc.alloc(ObjResult::new(value, Self::is_error()));
        vm.stack.push(Value::from(result));
        Ok(())
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
            (gc.preset_identifiers.init, 1, (|vm, _target, args| Self::initializer(vm, args[0])) as NativeFn)
        ]
            .into_iter()
            .map(|(name, arity, function)| (name, ObjNativeFn::new(name, arity, function)))
            .collect::<HashMap<_, _>>();

        methods
    }
}

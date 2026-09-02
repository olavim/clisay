use super::*;

macro_rules! check_arity {
    ($vm:expr, $arg_count:expr, $arity:expr, $func_name:expr) => {
        if $arg_count != $arity as usize {
            let name = unsafe { &(*$func_name).value };
            return $vm.error(format!("{} expects {} arguments, but was called with {}", name, $arity, $arg_count));
        }
    }
}

impl Vm {
    #[cold]
    #[inline(never)]
    pub(super) fn stack_overflow(&mut self) -> anyhow::Error {
        self.error("Stack overflow").unwrap_err()
    }

    pub(super) fn push_frame(&mut self, closure: *mut ObjClosure, stack_start: *mut Value, ip_start: usize) -> Result<(), anyhow::Error> {
        if self.frames.is_full() {
            return Err(self.stack_overflow());
        }
        self.frames.push(CallFrame {
            closure,
            return_ip: self.ip,
            stack_start,
        });
        self.ip = unsafe { self.chunk.code.as_ptr().offset(ip_start as isize) };
        Ok(())
    }

    pub(super) fn op_tail_call(&mut self) -> Result<(), anyhow::Error> {
        let arg_count = self.read_next() as usize;
        let value = self.stack.peek(arg_count);
        match self.frame_a_tail_call_may_take(value) {
            Some(closure) => self.take_frame_for(arg_count, closure),
            None => self.call(arg_count, value),
        }
    }

    fn frame_a_tail_call_may_take(&self, value: Value) -> Option<*mut ObjClosure> {
        if !value.is_object() {
            return None;
        }
        let object = value.as_object();
        if object.tag() != objects::TAG_CLOSURE {
            return None;
        }
        let frame = self.frames.top();
        let takeable = unsafe {
            // The script runs in the one frame with no closure, and has no caller to hand back to.
            !(*frame).closure.is_null()
        };
        takeable.then(|| object.as_closure_ptr())
    }

    fn take_frame_for(&mut self, arg_count: usize, closure_ptr: *mut ObjClosure) -> Result<(), anyhow::Error> {
        let closure = unsafe { &*closure_ptr };
        check_arity!(self, arg_count, closure.arity, closure.name);
        let frame = self.frames.top();
        let stack_start = unsafe { (*frame).stack_start };
        let callee = self.stack.offset(arg_count);
        self.close_upvalues(stack_start);
        unsafe {
            std::ptr::copy(callee, stack_start, arg_count + 1);
            self.stack.set_top(stack_start.add(arg_count + 1));
        }
        self.check_arguments_accepted(closure.param_accepts, stack_start, arg_count)?;
        let held = unsafe { (*frame).closure };
        unsafe { (*frame).closure = closure_ptr; }
        self.record_tail_call_breadcrumb(frame, held, closure_ptr);
        self.ip = unsafe { self.chunk.code.as_ptr().add(closure.ip_start) };
        Ok(())
    }

    pub(crate) fn call(&mut self, arg_count: usize, value: Value) -> Result<(), anyhow::Error> {
        if !value.is_callable() {
            return self.error(format!("{} is not callable", value.fmt()));
        }

        let object = value.as_object();
        let tag = object.tag();

        match tag {
            objects::TAG_CLOSURE => self.call_closure(arg_count, object.as_closure_ptr()),
            objects::TAG_NATIVE_FUNCTION => self.call_native(arg_count, object.as_native_function_ptr()),
            objects::TAG_BOUND_METHOD => self.call_bound_method(arg_count, object.as_bound_method_ptr()),
            objects::TAG_TYPE => self.call_type(arg_count, object.as_type_ptr()),
            objects::TAG_FUNCTION => self.error(format!("{} is not callable", value.fmt())),
            _ => unsafe { std::hint::unreachable_unchecked() }
        }
    }

    pub(super) fn slot_addr(&self, slot: usize) -> *mut Value {
        unsafe { (*self.frames.top()).stack_start.add(slot) }
    }

    pub(super) fn op_pop_scope(&mut self) {
        let count = self.read_next() as usize;
        let expected_slot_count = self.read_next() as usize;
        let base = unsafe { self.stack.top().sub(count) };
        debug_assert_eq!(self.frame_slot_count(), expected_slot_count, "a scope left a stack its compiler did not expect");
        self.close_upvalues(base);
        self.stack.set_top(base);
    }

    fn frame_slot_count(&self) -> usize {
        let frame = self.frames.top();
        if frame.is_null() {
            return self.stack.len();
        }
        let start = unsafe { (*frame).stack_start };
        (self.stack.top() as usize - start as usize) / std::mem::size_of::<Value>()
    }

    pub(super) fn call_native(&mut self, arg_count: usize, native_fn_ptr: *mut ObjNativeFn) -> Result<(), anyhow::Error> {
        let func = unsafe { &*native_fn_ptr };
        check_arity!(self, arg_count, func.arity as usize, func.name);
        let args = self.stack.pop_slice(arg_count);
        // The "target" is the first value in a call window. For method calls, this is the instance.
        let target = self.stack.pop();
        match (func.function)(self, target, args) {
            Ok(_) => Ok(()),
            Err(err) => self.error(err.downcast::<String>()?),
        }
    }

    #[inline]
    pub(super) fn enter_closure(&mut self, arg_count: usize, closure_ptr: *mut ObjClosure) -> Result<(), anyhow::Error> {
        let closure = unsafe { &*closure_ptr };
        let stack_start = self.stack.offset(arg_count);
        check_arity!(self, arg_count, closure.arity, closure.name);
        self.check_arguments_accepted(closure.param_accepts, stack_start, arg_count)?;
        self.push_frame(closure_ptr, stack_start, closure.ip_start)
    }

    fn call_closure(&mut self, arg_count: usize, closure_ptr: *mut ObjClosure) -> Result<(), anyhow::Error> {
        self.enter_closure(arg_count, closure_ptr)
    }

    fn call_bound_method(&mut self, arg_count: usize, bound_method_ptr: *mut ObjBoundMethod) -> Result<(), anyhow::Error> {
        let bound_method = unsafe { &*bound_method_ptr };
        let method = bound_method.method;
        match method.tag() {
            objects::TAG_CLOSURE => {
                self.stack.set(arg_count, Value::from(bound_method.target));
                self.enter_closure(arg_count, method.as_closure_ptr())?;
            },
            objects::TAG_NATIVE_FUNCTION => {
                self.stack.set(arg_count, Value::from(bound_method.target));
                self.call_native(arg_count, method.as_native_function_ptr())?;
            },
            _ => unsafe { std::hint::unreachable_unchecked() }
        };
        Ok(())
    }

    /// Brace construction `C { f: v, ... }`.
    pub(super) fn op_construct(&mut self) -> Result<(), anyhow::Error> {
        let field_count = self.read_next() as usize;
        let mut field_ids = [0u8; u8::MAX as usize + 1];
        for slot in field_ids.iter_mut().take(field_count) {
            *slot = self.read_next();
        }

        let type_val = self.stack.peek(field_count);
        if !type_val.is_object() || type_val.as_object().tag() != objects::TAG_TYPE {
            return self.error(format!("Cannot construct: {} is not a type", type_val.fmt()));
        }

        let type_ptr = type_val.as_object().as_type_ptr();
        let ty = unsafe { &*type_ptr };

        let instance_ptr = self.alloc(ObjInstance::new(type_ptr));
        let instance = unsafe { &mut *instance_ptr };
        for j in 0..field_count {
            let slot = self.stack.offset(field_count - 1 - j);
            instance.set(field_ids[j], unsafe { *slot });
        }

        // A construction verifies each `gives` delegate actually provides its trait.
        for &(field_id, field_name, trait_name, trait_id) in ty.gives.iter() {
            let value = instance.get(field_id);
            let provides = matches!(value.kind(), ValueKind::Object(ObjectKind::Instance))
                && unsafe { &*(*value.as_object().as_instance_ptr()).ty }.provided.contains(&trait_id);
            if !provides {
                let msg = format!("Delegate field '{}' does not provide trait '{}'",
                    unsafe { &(*field_name).value }, unsafe { &(*trait_name).value });
                return self.error(msg);
            }
        }

        self.stack.truncate(field_count + 1);
        self.stack.push(Value::from(instance_ptr));
        Ok(())
    }

    fn call_type(&mut self, arg_count: usize, type_ptr: *mut ObjType) -> Result<(), anyhow::Error> {
        let ty = unsafe { &*type_ptr };
        let Some(factory_obj) = ty.factory() else {
            let name = unsafe { &(*ty.name).value };
            return self.error(format!("'{name}' has no factory; construct it with a brace like '{name}{{ .. }}'"));
        };
        match factory_obj.tag() {
            objects::TAG_FUNCTION => {
                let factory_ref = factory_obj.as_function_ptr();
                let closure = self.create_closure(factory_ref);
                // Root the fresh closure on the value stack: it isn't reachable yet and
                // the instance allocation below can trigger GC.
                self.stack.push(Value::from(closure));
                let instance = self.alloc(ObjInstance::new(type_ptr));
                self.stack.pop();
                self.stack.set(arg_count, Value::from(instance));
                self.enter_closure(arg_count, closure.as_closure_ptr())
            },
            objects::TAG_CLOSURE => {
                let instance = self.alloc(ObjInstance::new(type_ptr));
                self.stack.set(arg_count, Value::from(instance));
                self.enter_closure(arg_count, factory_obj.as_closure_ptr())
            },
            objects::TAG_NATIVE_FUNCTION => {
                let factory_native = factory_obj.as_native_function_ptr();
                let instance = self.alloc(ObjInstance::new(type_ptr));
                self.stack.set(arg_count, Value::from(instance));
                self.call_native(arg_count, factory_native)?;
                Ok(())
            },
            _ => unsafe { std::hint::unreachable_unchecked() }
        }
    }
}

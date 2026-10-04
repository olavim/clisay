use crate::core::objects::FLAG_RETURNS_VALUE;
use super::*;

#[inline]
pub(super) fn return_arity_matches(closure: *mut ObjClosure, flags: u8) -> bool {
    flags & ir::CALL_WANTS_VALUE == 0 || unsafe { (*closure).header.has(FLAG_RETURNS_VALUE) }
}

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

    pub(super) fn push_checked(&mut self, value: Value) -> Result<(), anyhow::Error> {
        if !self.stack.has_room(1) {
            return Err(self.stack_overflow());
        }
        self.stack.push(value);
        Ok(())
    }

    #[cfg(debug_assertions)]
    pub(super) fn take_frame_generation(&mut self) -> FrameGeneration {
        self.next_frame_generation = self.next_frame_generation.wrapping_add(1);
        Value::wrap_frame_generation(self.next_frame_generation)
    }

    #[cfg(not(debug_assertions))]
    pub(super) fn take_frame_generation(&mut self) -> FrameGeneration {
        self.next_frame_generation
    }

    pub(super) fn push_frame(&mut self, closure: *mut ObjClosure, stack_start: *mut Value, ip_start: usize) -> Result<(), anyhow::Error> {
        if self.frames.is_full() {
            return Err(self.stack_overflow());
        }
        let generation = self.take_frame_generation();
        self.frames.push(CallFrame::new(closure, self.ip, stack_start, generation));
        self.ip = unsafe { self.chunk.code.as_ptr().offset(ip_start as isize) };
        Ok(())
    }

    #[inline]
    pub(super) fn op_tail_call(&mut self) -> Result<(), anyhow::Error> {
        let operand = self.read_next();
        let arg_count = self.read_next() as usize;
        let value = self.stack.peek(arg_count);
        match self.frame_a_tail_call_may_take(value) {
            Some(closure) => self.take_frame_for(arg_count, closure, operand),
            None => self.call(arg_count, value, operand),
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

    fn take_frame_for(&mut self, arg_count: usize, closure_ptr: *mut ObjClosure, flags: u8) -> Result<(), anyhow::Error> {
        let closure = unsafe { &*closure_ptr };
        check_arity!(self, arg_count, closure.arity, closure.name);
        if !return_arity_matches(closure_ptr, flags) {
            return Err(self.wrong_return_arity_error(closure_ptr));
        }
        let frame = self.frames.top();
        let stack_start = unsafe { (*frame).stack_start };
        let callee = self.stack.offset(arg_count);
        unsafe {
            std::ptr::copy(callee, stack_start, arg_count + 1);
            self.stack.set_top(stack_start.add(arg_count + 1));
        }
        self.check_arguments_accepted(closure.param_list_pool_id, stack_start, arg_count, flags)?;

        #[cfg(debug_assertions)]
        self.assert_no_anchor_into_this_frame(stack_start, arg_count);

        let held = unsafe { (*frame).closure };
        unsafe { (*frame).generation = self.take_frame_generation() };
        unsafe { (*frame).closure = closure_ptr; }
        self.record_tail_call_breadcrumb(frame, held, closure_ptr);
        self.ip = unsafe { self.chunk.code.as_ptr().add(closure.ip_start) };
        Ok(())
    }

    #[cfg(debug_assertions)]
    fn assert_no_anchor_into_this_frame(&self, stack_start: *mut Value, arg_count: usize) {
        let first = unsafe { stack_start.offset_from(self.stack.bottom()) } as usize;
        for slot in 1..=arg_count {
            let value = unsafe { *stack_start.add(slot) };
            assert!(!value.is_anchor() || value.anchor_index() < first,
                "a tail call reuses the frame an anchor argument names");
        }
    }

    pub(crate) fn call(&mut self, arg_count: usize, value: Value, flags: u8) -> Result<(), anyhow::Error> {
        if !value.is_callable() {
            return self.error(format!("{} is not callable", value.fmt()));
        }

        let object = value.as_object();
        let tag = object.tag();

        match tag {
            objects::TAG_CLOSURE => self.call_closure(arg_count, object.as_closure_ptr(), flags),
            objects::TAG_NATIVE_FUNCTION => self.call_native(arg_count, object.as_native_function_ptr(), flags),
            objects::TAG_BOUND_METHOD => self.call_bound_method(arg_count, object.as_bound_method_ptr(), flags),
            objects::TAG_TYPE => self.call_type(arg_count, object.as_type_ptr(), flags),
            objects::TAG_FUNCTION => self.error(format!("{} is not callable", value.fmt())),
            _ => unsafe { std::hint::unreachable_unchecked() }
        }
    }

    pub(super) fn slot_addr(&self, slot: usize) -> *mut Value {
        unsafe { (*self.frames.top()).stack_start.add(slot) }
    }

    pub(super) fn op_pop_scope(&mut self) {
        let count = self.read_next() as usize;
        let expected_height = self.read_next() as usize;
        let scope_start = unsafe { self.stack.top().sub(count) };
        debug_assert_eq!(self.frame_stack_height(), expected_height, "a scope left a stack its compiler did not expect");
        self.stack.set_top(scope_start);
    }

    fn frame_stack_height(&self) -> usize {
        let frame = self.frames.top();
        if frame.is_null() {
            return self.stack.len();
        }
        let start = unsafe { (*frame).stack_start };
        (self.stack.top() as usize - start as usize) / std::mem::size_of::<Value>()
    }

    pub(super) fn call_native(&mut self, arg_count: usize, native_fn_ptr: *mut ObjNativeFn, flags: u8) -> Result<(), anyhow::Error> {
        let func = unsafe { &*native_fn_ptr };
        check_arity!(self, arg_count, func.arity as usize, func.name);

        self.at_forced_check();

        let first_arg = unsafe { self.stack.top().sub(arg_count) };

        if (0..arg_count).any(|at| unsafe { *first_arg.add(at) }.is_anchor()) {
            return self.error(format!("{} does not take an anchor", unsafe { &*func.name }.value));
        }

        if func.wants_anchor_receiver {
            if !unsafe { *first_arg.sub(1) }.is_anchor() {
                return self.refuse_value_receiver(func.name);
            }
            for at in 0..arg_count {
                self.share_in_place(unsafe { first_arg.add(at) });
            }
            let receiver = unsafe { first_arg.sub(1) };
            let resolved = self.receiver_for_write(unsafe { *receiver })?;
            unsafe { *receiver = resolved };
        } else if flags & ir::CALL_PASSES_ANCHOR_RECEIVER != 0 {
            return self.refuse_unwanted_anchor_receiver(&unsafe { &*func.name }.value);
        }

        let args = self.stack.pop_slice(arg_count);

        // The "target" is the first value in a call window. For method calls, this is the instance.
        // A caller that could not resolve the callee hands it over as an anchor.
        let target = self.stack.pop();
        let target = self.anchor_value(target)?;
        let mark = self.hold(&args);
        self.hold(&[target]);
        let outcome = (func.function)(self, target, args);
        self.release_held(mark);
        match outcome {
            Ok(_) => Ok(()),
            Err(err) => self.error(err.downcast::<String>()?),
        }
    }

    #[cold]
    #[inline(never)]
    pub(super) fn refuse_unwanted_anchor_receiver(&self, callee: &str) -> Result<(), anyhow::Error> {
        self.error_help(format!("`{callee}` does not want an anchor receiver"), "call it without `&`")
    }

    fn refuse_value_receiver(&self, method: *mut ObjString) -> Result<(), anyhow::Error> {
        self.error_help(format!("`{}` wants an anchor receiver", unsafe { &*method }.value),
            "an anchor names a binding or a path into one, and this receiver is a value")
    }

    /// Refuses a value receiver passed to a callee that wants an anchor receiver, and vice-versa.
    /// An anchor that arrives without a `&` in the source is read as a value.
    fn match_receiver_to_callee(&mut self, closure: &ObjClosure, stack_start: *mut Value, flags: u8) -> Result<(), anyhow::Error> {
        let receiver = unsafe { *stack_start };
        if closure.param_list_pool_id & ir::WANTS_ANCHOR_RECEIVER != 0 {
            return match receiver.is_anchor() {
                true => Ok(()),
                false => self.refuse_value_receiver(closure.name),
            };
        }
        if receiver.is_anchor() {
            if flags & ir::CALL_PASSES_ANCHOR_RECEIVER != 0 {
                return self.refuse_unwanted_anchor_receiver(&unsafe { &*closure.name }.value);
            }
            let read = self.anchor_value(receiver)?;
            unsafe { *stack_start = read };
        }
        Ok(())
    }

    #[inline]
    pub(super) fn wrong_return_arity_error(&self, closure: *mut ObjClosure) -> anyhow::Error {
        let name = unsafe { &(*(*closure).name).value };
        self.error_help(
            "Unexpected void return",
            format!("`{name}` returns no value; call it as a statement, or return a value from every path"),
        ).unwrap_err()
    }

    pub(super) fn enter_closure(&mut self, arg_count: usize, closure_ptr: *mut ObjClosure, flags: u8) -> Result<(), anyhow::Error> {
        let closure = unsafe { &*closure_ptr };
        let stack_start = self.stack.offset(arg_count);
        check_arity!(self, arg_count, closure.arity, closure.name);

        self.match_receiver_to_callee(closure, stack_start, flags)?;
        self.check_arguments_accepted(closure.param_list_pool_id, stack_start, arg_count, flags)?;

        if !return_arity_matches(closure_ptr, flags) {
            return Err(self.wrong_return_arity_error(closure_ptr));
        }

        self.push_frame(closure_ptr, stack_start, closure.ip_start)
    }

    fn call_closure(&mut self, arg_count: usize, closure_ptr: *mut ObjClosure, flags: u8) -> Result<(), anyhow::Error> {
        self.enter_closure(arg_count, closure_ptr, flags)
    }

    fn call_bound_method(&mut self, arg_count: usize, bound_method_ptr: *mut ObjBoundMethod, flags: u8) -> Result<(), anyhow::Error> {
        let bound_method = unsafe { &*bound_method_ptr };
        let method = bound_method.method;
        match method.tag() {
            objects::TAG_CLOSURE => {
                self.stack.set(arg_count, Value::from(bound_method.target));
                self.enter_closure(arg_count, method.as_closure_ptr(), flags)?;
            },
            objects::TAG_NATIVE_FUNCTION => {
                self.stack.set(arg_count, Value::from(bound_method.target));
                self.call_native(arg_count, method.as_native_function_ptr(), flags)?;
            },
            _ => unsafe { std::hint::unreachable_unchecked() }
        };
        Ok(())
    }

    /// Brace construction `C { f: v, ... }`.
    pub(super) fn op_construct(&mut self) -> Result<(), anyhow::Error> {
        let field_ids = self.read_byte_list();
        let field_count = field_ids.len();

        let type_val = self.stack.peek(field_count);
        if !type_val.is_object() || type_val.as_object().tag() != objects::TAG_TYPE {
            return self.error(format!("Cannot construct: {} is not a type", type_val.fmt()));
        }

        let type_ptr = type_val.as_object().as_type_ptr();
        let ty = unsafe { &*type_ptr };

        for j in 0..field_count {
            self.share_in_place(self.stack.offset(field_count - 1 - j));
        }

        let instance_ptr = self.alloc_instance(type_ptr);
        for j in 0..field_count {
            let value = unsafe { *self.stack.offset(field_count - 1 - j) };
            let witness_set_pool_id = self.field_witness_set_pool_id(instance_ptr, field_ids[j]);
            self.check_value_accepted(value, witness_set_pool_id)?;
            unsafe { ObjInstance::set(instance_ptr, field_ids[j], value) };
        }

        // A construction verifies each `gives` delegate actually provides its trait.
        for &(field_id, field_name, trait_name, trait_id) in ty.gives.iter() {
            let value = unsafe { ObjInstance::get(instance_ptr, field_id) };
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

    fn call_type(&mut self, arg_count: usize, type_ptr: *mut ObjType, flags: u8) -> Result<(), anyhow::Error> {
        let ty = unsafe { &*type_ptr };
        let Some(factory_obj) = ty.factory() else {
            let name = unsafe { &(*ty.name).value };
            return self.error(format!("'{name}' has no factory; construct it with a brace like '{name}{{ .. }}'"));
        };
        match factory_obj.tag() {
            objects::TAG_FUNCTION => {
                let factory_ref = factory_obj.as_function_ptr();
                let closure = self.create_closure(factory_ref);
                let mark = self.hold(&[Value::from(closure)]);
                let instance = self.alloc_instance(type_ptr);
                self.stack.set(arg_count, Value::from(instance));
                let entered = self.enter_closure(arg_count, closure.as_closure_ptr(), flags);
                self.release_held(mark);
                entered
            },
            objects::TAG_CLOSURE => {
                let instance = self.alloc_instance(type_ptr);
                self.stack.set(arg_count, Value::from(instance));
                self.enter_closure(arg_count, factory_obj.as_closure_ptr(), flags)
            },
            objects::TAG_NATIVE_FUNCTION => {
                let factory_native = factory_obj.as_native_function_ptr();
                let instance = self.alloc_instance(type_ptr);
                self.stack.set(arg_count, Value::from(instance));
                self.call_native(arg_count, factory_native, 0)?;
                Ok(())
            },
            _ => unsafe { std::hint::unreachable_unchecked() }
        }
    }
}

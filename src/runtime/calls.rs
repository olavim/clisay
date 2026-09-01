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

    pub(super) fn push_frame(&mut self, closure: *mut ObjClosure, stack_start: *mut Value, ip_start: usize, seal: bool) -> Result<(), anyhow::Error> {
        if self.frames.is_full() {
            return Err(self.stack_overflow());
        }
        self.frames.push(CallFrame {
            closure,
            return_ip: self.ip,
            stack_start,
            seal,
        });
        self.ip = unsafe { self.chunk.code.as_ptr().offset(ip_start as isize) };
        Ok(())
    }

    pub(super) fn op_tail_call(&mut self) -> Result<(), anyhow::Error> {
        let arg_count = self.read_next() as usize;
        let value = self.stack.peek(arg_count);
        match self.frame_a_tail_call_may_take(value) {
            Some(closure) => self.take_frame_for(arg_count, closure),
            None => self.call(arg_count, value, true),
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
        unsafe {
            (*frame).closure = closure_ptr;
            (*frame).seal = true;
        }
        self.record_tail_call_breadcrumb(frame, held, closure_ptr);
        self.ip = unsafe { self.chunk.code.as_ptr().add(closure.ip_start) };
        Ok(())
    }

    pub(super) fn op_call_mut(&mut self) -> Result<(), anyhow::Error> {
        let arg_count = self.read_next() as usize;
        let value = self.stack.peek(arg_count);
        self.call(arg_count, value, false)
    }

    /// `seal` rides onto the pushed frame so a factory's `RETURN_FAC` knows whether to freeze.
    pub(crate) fn call(&mut self, arg_count: usize, value: Value, seal: bool) -> Result<(), anyhow::Error> {
        if !value.is_callable() {
            return self.error(format!("{} is not callable", value.fmt()));
        }

        let object = value.as_object();
        let tag = object.tag();

        match tag {
            objects::TAG_CLOSURE => self.call_closure(arg_count, object.as_closure_ptr(), seal),
            objects::TAG_NATIVE_FUNCTION => self.call_native(arg_count, object.as_native_function_ptr()),
            objects::TAG_BOUND_METHOD => self.call_bound_method(arg_count, object.as_bound_method_ptr(), seal),
            objects::TAG_TYPE => self.call_type(arg_count, object.as_type_ptr(), seal),
            objects::TAG_FUNCTION => self.error(format!("{} is not callable", value.fmt())),
            _ => unsafe { std::hint::unreachable_unchecked() }
        }
    }

    pub(super) fn op_assert_immutable(&mut self) -> Result<(), anyhow::Error> {
        let forced = self.at_elided_site();
        let value = self.stack.peek(0);
        if !objects::is_mutable_container(value) {
            return Ok(());
        }
        if forced {
            return self.refuted_elision_error("a value proven immutable is mutable");
        }
        let position = self.get_source_position().clone();
        let label = format!("`{}` is mutable", position.snippet());
        self.raise(Diagnostic::new(objects::MUTABLE_IN_IMMUTABLE, position)
            .with_label(label)
            .with_help("an immutable value is immutable all the way down; freeze this value, or mark the construction `mut`"))
    }

    pub(super) fn op_assert_no_retain(&mut self) -> Result<(), anyhow::Error> {
        let arg_count = self.read_next() as usize;
        let owed_idx = u16::from_le_bytes([self.read_next(), self.read_next()]);
        let count = self.read_next() as usize;
        let callee = self.stack.peek(arg_count);
        for _ in 0..count {
            let position = self.read_next() as usize;
            if let Some(owed) = self.owed_at(owed_idx, position) {
                if self.callee_retains(callee, position) {
                    let name = self.get_source_position().snippet();
                    return self.error_labeled(objects::retained_owed_value(owed),
                        format!("`{name}` owes '{owed}' but the callee retains it"));
                }
            }
        }
        Ok(())
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

    fn owed_at(&self, owed_idx: u16, position: usize) -> Option<&str> {
        self.chunk.owed_names[owed_idx as usize].iter()
            .find(|(p, _)| *p as usize == position)
            .map(|(_, name)| &**name)
    }

    fn callee_retains(&self, callee: Value, position: usize) -> bool {
        if !callee.is_callable() {
            return false;
        }
        let object = callee.as_object();
        match object.tag() {
            objects::TAG_CLOSURE => unsafe { &*object.as_closure_ptr() }.retains_at(position),
            objects::TAG_BOUND_METHOD => {
                let method = unsafe { &*object.as_bound_method_ptr() }.method;
                method.tag() == objects::TAG_CLOSURE && unsafe { &*method.as_closure_ptr() }.retains_at(position)
            },
            objects::TAG_TYPE => {
                let init = unsafe { &*object.as_type_ptr() }.factory();
                matches!(init, Some(obj) if callable_retains(obj, position))
            },
            _ => false,
        }
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

    fn call_closure(&mut self, arg_count: usize, closure_ptr: *mut ObjClosure, seal: bool) -> Result<(), anyhow::Error> {
        let closure = unsafe { &*closure_ptr };
        check_arity!(self, arg_count, closure.arity, closure.name);
        let stack_start = self.stack.offset(arg_count);
        self.check_arguments_accepted(closure.param_accepts, stack_start, arg_count)?;
        self.push_frame(closure_ptr, stack_start, closure.ip_start, seal)?;
        Ok(())
    }

    fn call_bound_method(&mut self, arg_count: usize, bound_method_ptr: *mut ObjBoundMethod, seal: bool) -> Result<(), anyhow::Error> {
        let bound_method = unsafe { &*bound_method_ptr };
        let method = bound_method.method;
        match method.tag() {
            objects::TAG_CLOSURE => {
                let closure_ptr = method.as_closure_ptr();
                let closure = unsafe { &*closure_ptr };
                if closure.mut_receiver && self.receiver_rejects_mut(bound_method.target) {
                    return self.readonly_receiver_error(closure.name, bound_method.target);
                }
                check_arity!(self, arg_count, closure.arity, closure.name);
                let stack_start = self.stack.set(arg_count, Value::from(bound_method.target));
                self.check_arguments_accepted(closure.param_accepts, stack_start, arg_count)?;
                self.push_frame(closure_ptr, stack_start, closure.ip_start, seal)?;
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

        // A plain brace freezes the instance in place; `mut K{..}` (seal 0) leaves it mutable.
        let seal = self.read_next() != 0;

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
            let value = unsafe { *slot };
            // A frozen instance is immutable all the way down.
            if seal && objects::is_mutable_container(value) {
                return self.mutable_in_immutable_error();
            }
            instance.set(field_ids[j], value);
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

        if seal {
            crate::core::objects::freeze_value(Value::from(instance_ptr), self.current_pos_index());
        }
        self.stack.truncate(field_count + 1);
        self.stack.push(Value::from(instance_ptr));
        Ok(())
    }

    fn call_type(&mut self, arg_count: usize, type_ptr: *mut ObjType, seal: bool) -> Result<(), anyhow::Error> {
        let ty = unsafe { &*type_ptr };
        let Some(factory_obj) = ty.factory() else {
            let name = unsafe { &(*ty.name).value };
            return self.error(format!("'{name}' has no factory; construct it with a brace like '{name}{{ .. }}'"));
        };
        match factory_obj.tag() {
            objects::TAG_FUNCTION => {
                let factory_ref = factory_obj.as_function_ptr();
                let factory = unsafe { &*factory_ref };
                check_arity!(self, arg_count, factory.arity, factory.name);

                let closure = self.create_closure(factory_ref);
                // Root the fresh closure on the value stack: it isn't reachable yet and
                // the instance allocation below can trigger GC.
                self.stack.push(Value::from(closure));
                let instance = self.alloc(ObjInstance::new(type_ptr));
                self.stack.pop();
                let stack_start = self.stack.set(arg_count, Value::from(instance));
                self.check_arguments_accepted(factory.param_accepts, stack_start, arg_count)?;
                self.push_frame(closure.as_closure_ptr(), stack_start, factory.ip_start, seal)?;
                Ok(())
            },
            objects::TAG_CLOSURE => {
                let closure_ptr = factory_obj.as_closure_ptr();
                let closure = unsafe { &*closure_ptr };
                check_arity!(self, arg_count, closure.arity, closure.name);

                let instance = self.alloc(ObjInstance::new(type_ptr));
                let stack_start = self.stack.set(arg_count, Value::from(instance));
                self.check_arguments_accepted(closure.param_accepts, stack_start, arg_count)?;
                self.push_frame(closure_ptr, stack_start, closure.ip_start, seal)?;
                Ok(())
            },
            // A native factory receives the fresh instance as its target and fills its fields. The
            // seal is applied here rather than in the native, so `mut K(..)` reaches a built-in
            // type the same way `RETURN_FAC` carries it out of a script factory.
            objects::TAG_NATIVE_FUNCTION => {
                let factory_native = factory_obj.as_native_function_ptr();
                let instance = self.alloc(ObjInstance::new(type_ptr));
                self.stack.set(arg_count, Value::from(instance));
                self.call_native(arg_count, factory_native)?;
                if seal {
                    objects::freeze_value(self.stack.peek(0), self.current_pos_index());
                }
                Ok(())
            },
            _ => unsafe { std::hint::unreachable_unchecked() }
        }
    }
}

fn callable_retains(obj: Object, position: usize) -> bool {
    match obj.tag() {
        objects::TAG_FUNCTION => unsafe { &*obj.as_function_ptr() }.retains_at(position),
        objects::TAG_CLOSURE => unsafe { &*obj.as_closure_ptr() }.retains_at(position),
        _ => false,
    }
}

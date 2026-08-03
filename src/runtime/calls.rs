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
    /// The stack-overflow error, kept off the hot call path. `#[cold]` +
    /// `#[inline(never)]` so the bulky error/trace formatting isn't inlined into
    /// the dispatch loop's `CALL` arm (which would bloat the hot loop body).
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
            write_depth: self.write_owners.len(),
        });
        self.ip = unsafe { self.chunk.code.as_ptr().offset(ip_start as isize) };
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
            _ => unsafe { std::hint::unreachable_unchecked() }
        }
    }

    pub(super) fn op_assert_immutable(&mut self) -> Result<(), anyhow::Error> {
        let value = self.stack.peek(0);
        if !objects::is_mutable_container(value) {
            return Ok(());
        }
        let position = self.get_source_position().clone();
        let label = format!("`{}` is mutable", position.snippet());
        self.raise(Diagnostic::new(objects::MUTABLE_IN_IMMUTABLE, position)
            .with_label(label)
            .with_help("an immutable value is immutable all the way down; freeze this value, or mark the construction `mut`"))
    }

    /// The path-write barrier. A write through a path has no binding denoting the element it
    /// reaches, so it takes no writer slot of its own. It fails only when one is already held by
    /// something other than its own root.
    pub(super) fn op_assert_no_other_writer(&mut self) -> Result<(), anyhow::Error> {
        let base = self.read_next() as usize;
        let value = self.stack.peek(0);
        if !value.is_object() || !value.as_object().is_write_owned() {
            return Ok(());
        }
        // A container that took an element writes it through its own paths. Only a path rooted
        // somewhere else is a second writer.
        match self.owns_write(value, self.slot_addr(base)) {
            true => Ok(()),
            false => Err(self.second_writer_error(value)),
        }
    }

    /// Assert that nothing is holding the write-ownership of the value on top.
    pub(super) fn op_assert_no_writer(&mut self) -> Result<(), anyhow::Error> {
        let value = self.stack.peek(0);
        match value.is_object() && value.as_object().is_write_owned() {
            true => Err(self.second_writer_error(value)),
            false => Ok(()),
        }
    }

    /// The persist barrier. A parameter may hold a mutable borrowed from the caller, and storing
    /// it somewhere that outlives the call would keep it reachable past the borrow.
    pub(super) fn op_assert_not_borrowed(&mut self) -> Result<(), anyhow::Error> {
        let value = self.stack.peek(0);
        if !value.is_borrowed() {
            return Ok(());
        }
        let position = self.get_source_position().clone();
        let label = format!("`{}` is a mutable value borrowed from the caller", position.snippet());
        self.raise(Diagnostic::new(objects::PERSISTED_BORROW, position)
            .with_label(label)
            .with_help("a borrowed value cannot be stored where it outlives the borrow; take the parameter by `*mut` to own it, or `copy` it before storing"))
    }

    /// An argument the caller reads again after the call. The compiler could not tell whether the
    /// callee consumes it, so the reader demanded this proof that it only borrowed.
    pub(super) fn op_assert_not_consumed(&mut self) -> Result<(), anyhow::Error> {
        let arg_count = self.read_next() as usize;
        let count = self.read_next() as usize;
        let call_pos = self.get_source_position().clone();
        let callee = self.stack.peek(arg_count);
        for _ in 0..count {
            let position = self.read_next() as usize;
            if self.callee_escapes(callee, position) {
                let read = self.get_source_position().clone();
                let label = format!("`{}` used here", read.snippet());
                return self.raise(Diagnostic::new(objects::CONSUMED_ARGUMENT, read)
                    .with_label(label)
                    .with_context_span(call_pos, "this call took it"));
            }
        }
        Ok(())
    }

    /// The opaque-call mode barrier. An argument the caller must keep alive may not be handed to a
    /// callee that consumes it, so this asserts the callee borrows the guarded parameter.
    pub(super) fn op_assert_borrow(&mut self) -> Result<(), anyhow::Error> {
        let arg_count = self.read_next() as usize;
        let count = self.read_next() as usize;
        let callee = self.stack.peek(arg_count);
        for _ in 0..count {
            let position = self.read_next() as usize;
            if self.callee_escapes(callee, position) {
                let label = format!("the callee lets `{}` escape", self.get_source_position().snippet());
                return self.error_labeled(objects::ESCAPED_BORROW, label);
            }
        }
        Ok(())
    }

    /// Marks the listed argument positions borrowed for the following call, so a store of any of
    /// them traps. Each entry saves the prior bit for nesting; a matching `RELEASE_BORROW` restores.
    pub(super) fn op_mark_borrow(&mut self) {
        let arg_count = self.read_next() as usize;
        let count = self.read_next() as usize;
        for _ in 0..count {
            let position = self.read_next() as usize;
            let value = self.stack.peek(arg_count - 1 - position);
            let prev = value.is_borrowed();
            // A frozen `no persist` value is still marked: immutability stops mutation, not the
            // persist that the borrow bit traps.
            if value.is_object() {
                value.as_object().set_borrowed(true);
            }
            self.borrows.push((value, prev));
        }
    }

    /// Takes the writer slot for an element the compiler could not name. A literal key is settled
    /// statically, so only a computed one reaches here.
    pub(super) fn op_take_write_ownership(&mut self) -> Result<(), anyhow::Error> {
        let slot = self.read_next() as usize;
        self.take_write_ownership(slot, false)
    }

    /// A container taking the writer slot for an element it is given.
    pub(super) fn op_transfer_write_ownership(&mut self) -> Result<(), anyhow::Error> {
        let slot = self.read_next() as usize;
        self.take_write_ownership(slot, true)
    }

    /// Takes the writer slot for the value on top in the name of a local. The holder may take it
    /// again as often as it likes, so only a different holder is a second writer.
    fn take_write_ownership(&mut self, slot: usize, given: bool) -> Result<(), anyhow::Error> {
        let value = self.stack.peek(0);
        if !value.is_object() {
            return Ok(());
        }
        let holder = self.slot_addr(slot);
        if value.as_object().is_write_owned() {
            return match self.owns_write(value, holder) {
                true => Ok(()),
                false => Err(self.second_writer_error(value)),
            };
        }
        value.as_object().set_write_owned(true);
        self.write_owners.push(WriteOwner { value, holder, at: self.current_pos_index(), given });
        Ok(())
    }

    /// The address of a local in the running frame, which is what identifies a holder.
    fn slot_addr(&self, slot: usize) -> *mut Value {
        unsafe { (*self.frames.top()).stack_start.add(slot) }
    }

    /// Whether this name is the one already holding the value's writer slot.
    fn owns_write(&self, value: Value, holder: *mut Value) -> bool {
        self.write_owners.iter().rev().find(|held| held.value == value)
            .is_some_and(|held| held.holder == holder)
    }

    /// The trap for a second name taking the writer slot for one element.
    #[cold]
    #[inline(never)]
    fn second_writer_error(&mut self, value: Value) -> anyhow::Error {
        let here = self.get_source_position().clone();
        let held = self.write_owners.iter().rev().find(|held| held.value == value).copied();
        let given = held.is_some_and(|held| held.given);
        let mut diagnostic = match given {
            true => Diagnostic::new(objects::WROTE_GIVEN_ELEMENT, here.clone())
                .with_label(format!("`{}` is written here", here.snippet())),
            false => Diagnostic::new(objects::SECOND_ELEMENT_WRITER, here.clone())
                .with_label("this takes a second writer for the element"),
        };
        if let Some(held) = held {
            let taken = self.chunk.code_pos[held.at as usize].clone();
            if taken.start != here.start {
                let label = match given {
                    true => format!("`{}` is stored here, which takes write-ownership", taken.snippet()),
                    false => "another name already writes it here".to_string(),
                };
                diagnostic = diagnostic.with_context_span(taken, label);
            }
        }
        let help = match given {
            true => "an element has one write-owner; reading it here is still fine, but to write it, go through the container it was stored into",
            false => "one element has one writer; let the other name go out of scope first, or only read through this one",
        };
        self.raise(diagnostic.with_help(help)).unwrap_err()
    }

    /// Gives back the slot a local holds, where its value is about to change.
    pub(super) fn op_release_write_ownership_at(&mut self) {
        let slot = self.read_next() as usize;
        let holder = self.slot_addr(slot);
        for k in (0..self.write_owners.len()).rev() {
            if self.write_owners[k].holder == holder {
                let held = self.write_owners.remove(k);
                self.release_write(held.value);
            }
        }
    }

    /// Gives back whatever the scope's own `count` values still hold.
    pub(super) fn op_release_write_ownership(&mut self) {
        let count = self.read_next() as usize;
        let floor = unsafe { self.stack.top().sub(count) };
        for k in (0..self.write_owners.len()).rev() {
            if self.write_owners[k].holder >= floor {
                let held = self.write_owners.remove(k);
                self.release_write(held.value);
            }
        }
    }

    /// Clears one value's writer slot. A value that holds none is left alone.
    fn release_write(&self, value: Value) {
        if value.is_object() && value.as_object().is_write_owned() {
            value.as_object().set_write_owned(false);
        }
    }

    /// Clears every held writer slot above `depth`, for an exit that skips the scope releases.
    pub(super) fn release_writes_above(&mut self, depth: usize) {
        while self.write_owners.len() > depth {
            let held = self.write_owners.pop().unwrap();
            self.release_write(held.value);
        }
    }

    /// Restores the last `count` marked borrows after a call returns.
    pub(super) fn op_release_borrow(&mut self) {
        let count = self.read_next() as usize;
        for _ in 0..count {
            if let Some((value, prev)) = self.borrows.pop() {
                if value.is_object() {
                    value.as_object().set_borrowed(prev);
                }
            }
        }
    }

    /// Whether a callable lets its argument at `position` escape, as opposed to borrowing it.
    fn callee_escapes(&self, callee: Value, position: usize) -> bool {
        if !callee.is_callable() {
            return false;
        }
        let object = callee.as_object();
        match object.tag() {
            objects::TAG_CLOSURE => unsafe { &*object.as_closure_ptr() }.escapes(position),
            objects::TAG_BOUND_METHOD => {
                let method = unsafe { &*object.as_bound_method_ptr() }.method;
                method.tag() == objects::TAG_CLOSURE && unsafe { &*method.as_closure_ptr() }.escapes(position)
            },
            objects::TAG_TYPE => {
                let init = unsafe { &*object.as_type_ptr() }.factory();
                matches!(init, Some(obj) if obj.tag() == objects::TAG_FUNCTION
                    && unsafe { &*obj.as_function_ptr() }.escapes(position))
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
            Err(err) => self.error(err.downcast::<String>()?)
        }
    }

    fn call_closure(&mut self, arg_count: usize, closure_ptr: *mut ObjClosure, seal: bool) -> Result<(), anyhow::Error> {
        let closure = unsafe { &*closure_ptr };
        check_arity!(self, arg_count, closure.arity, closure.name);
        self.push_frame(closure_ptr, self.stack.offset(arg_count), closure.ip_start, seal)
    }

    fn call_bound_method(&mut self, arg_count: usize, bound_method_ptr: *mut ObjBoundMethod, seal: bool) -> Result<(), anyhow::Error> {
        let bound_method = unsafe { &*bound_method_ptr };
        let method = bound_method.method;
        match method.tag() {
            objects::TAG_CLOSURE => {
                let closure_ptr = method.as_closure_ptr();
                let closure = unsafe { &*closure_ptr };
                if closure.mut_receiver && self.receiver_rejects_mut(bound_method.target) {
                    return self.error_readonly_receiver(closure.name, bound_method.target);
                }
                check_arity!(self, arg_count, closure.arity, closure.name);
                let stack_start = self.stack.set(arg_count, Value::from(bound_method.target));
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

    /// Brace construction `C { f: v, ... }`. Reads the brace field ids, allocates the instance,
    /// sets the brace fields from the stack, then verifies `gives`. The stack holds
    /// `[C, brace values..]` in source order. A brace does not run the factory.
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
            let value = self.stack.peek(field_count - 1 - j);
            self.ensure_not_borrowed(value)?;
            instance.set(field_ids[j], value);
        }

        // A construction verifies each `gives` delegate actually provides its trait.
        for &(field_id, field_name, trait_name) in ty.gives.iter() {
            let value = instance.get(field_id);
            let provides = matches!(value.kind(), ValueKind::Object(ObjectKind::Instance))
                && unsafe { &*(*value.as_object().as_instance_ptr()).ty }.provided.contains(&trait_name);
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
                self.push_frame(closure.as_closure_ptr(), stack_start, factory.ip_start, seal)
            },
            // A native factory receives the fresh instance as its target and fills its fields.
            objects::TAG_NATIVE_FUNCTION => {
                let factory_native = factory_obj.as_native_function_ptr();
                let instance = self.alloc(ObjInstance::new(type_ptr));
                self.stack.set(arg_count, Value::from(instance));
                self.call_native(arg_count, factory_native)
            },
            _ => unsafe { std::hint::unreachable_unchecked() }
        }
    }
}

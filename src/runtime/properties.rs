use super::*;

/// What a member lookup found, for the admission test.
enum MemberValue {
    Value(Value),
    Method,
    Absent,
}

/// The advice for a persisted borrow where no name can be quoted back.
const PERSIST_HELP: &str = "a borrowed value cannot be stored in a destination that outlives the call; take the parameter by `*mut` to own it, or `copy` it before storing";

impl Vm {
    #[inline]
    fn resolve_cached_type_property(&mut self, type_ptr: *mut ObjType, prop: *mut ObjString) -> Option<TypeMember> {
        let site = self.ip as usize;
        let slot = (site >> 4) & (INDEX_CACHE_SIZE - 1);
        let ty = unsafe { &*type_ptr };
        let entry = unsafe { self.index_cache.get_unchecked_mut(slot) };
        if entry.site == site && entry.prop == prop && entry.ty == ty.id {
            return Some(entry.member);
        }
        let member = ty.resolve(prop)?;
        *entry = IndexCache { site, prop, ty: ty.id, member };
        Some(member)
    }

    fn bind_method(&mut self, target: Value, method: Object) -> Value {
        if method.tag() == objects::TAG_FUNCTION {
            let closure = self.create_closure(method.as_function_ptr());
            self.stack.push(Value::from(closure));
            let bound = self.alloc(ObjBoundMethod::new(target, closure));
            self.stack.pop();
            Value::from(bound)
        } else {
            Value::from(self.alloc(ObjBoundMethod::new(target, method)))
        }
    }

    /// Fused method call (`INVOKE`). Fast-paths an instance method call.
    pub(super) fn op_invoke(&mut self) -> Result<(), anyhow::Error> {
        let name_idx = self.read_next() as usize;
        let arg_count = self.read_next() as usize;
        let root_kind = self.read_next();
        let root_operand = self.read_next();
        let is_dot = self.read_next() != 0;
        let name = self.chunk.constants[name_idx].as_object().as_string_ptr();
        let receiver = self.stack.peek(arg_count);

        if matches!(receiver.kind(), ValueKind::Object(ObjectKind::Instance)) {
            let type_ptr = unsafe { (*receiver.as_object().as_instance_ptr()).ty };
            if let Some(TypeMember::Method(id)) = self.resolve_cached_type_property(type_ptr, name) {
                let method = unsafe { &*type_ptr }.get_method(id);
                if matches!(method.tag(), objects::TAG_FUNCTION | objects::TAG_CLOSURE) {
                    return self.invoke_method(method, arg_count, root_kind, root_operand);
                }
            }
        }

        self.invoke_member_slow(name, arg_count, root_kind, root_operand, is_dot)
    }

    /// Pushes a frame for an instance method without allocating a bound method.
    fn invoke_method(&mut self, method: Object, arg_count: usize, root_kind: u8, root_operand: u8) -> Result<(), anyhow::Error> {
        let root = self.take_write_root(root_kind, root_operand);

        // A capturing method is already a closure bound to the frame that declared its type. Any
        // other method captures nothing and is closed here.
        let is_bound = method.tag() == objects::TAG_CLOSURE;
        let (name, arity, ip_start, mut_receiver, retain_receiver) = match is_bound {
            true => {
                let closure = unsafe { &*method.as_closure_ptr() };
                (closure.name, closure.arity, closure.ip_start, closure.mut_receiver, closure.retain_receiver)
            },
            false => {
                let func = unsafe { &*method.as_function_ptr() };
                (func.name, func.arity, func.ip_start, func.mut_receiver, func.retain_receiver)
            },
        };
        if mut_receiver {
            let target = self.stack.peek(arg_count);
            if self.receiver_rejects_mut(target) {
                return self.readonly_receiver_error(name, target);
            }
            self.ensure_writable(target, root)?;
        }
        if arg_count != arity as usize {
            let text = unsafe { &(*name).value };
            return self.error(format!("{} expects {} arguments, but was called with {}", text, arity, arg_count));
        }
        let closure_ptr = match is_bound {
            true => method.as_closure_ptr(),
            false => self.create_closure(method.as_function_ptr()).as_closure_ptr(),
        };
        let stack_start = self.stack.offset(arg_count);
        self.push_frame(closure_ptr, stack_start, ip_start, true)?;
        let m = unsafe { (*closure_ptr).call_masks() };
        self.transfer_argument_write_ownership(m.retain_mask, m.escape_mask, m.needs_borrow_mark, m.param_accepts, stack_start, arg_count, ReceiverSlot::declared(retain_receiver, unsafe { (*closure_ptr).receiver_needs_borrow }))?;
        Ok(())
    }

    /// Invokes `this.name(args)`.
    pub(super) fn op_invoke_this(&mut self) -> Result<(), anyhow::Error> {
        let member_id = self.read_next();
        let arg_count = self.read_next() as usize;
        let root_kind = self.read_next();
        let root_operand = self.read_next();
        let receiver = self.stack.peek(arg_count);
        let ValueKind::Object(ObjectKind::Instance) = receiver.kind() else {
            return self.error(format!("Invalid property access: {}", receiver.fmt()));
        };

        // Fields are numbered before methods, so the id says which kind this is without reading
        // the value. Only a function or a closure takes a frame. Anything else is called as a value.
        let ty = unsafe { &*(*receiver.as_object().as_instance_ptr()).ty };
        if member_id >= ty.field_count {
            if let Some(method) = ty.methods.get(&member_id).copied() {
                if matches!(method.tag(), objects::TAG_FUNCTION | objects::TAG_CLOSURE) {
                    return self.invoke_method(method, arg_count, root_kind, root_operand);
                }
            }
        }

        self.invoke_this_field(receiver, member_id, arg_count, root_kind, root_operand)
    }

    /// Calls a field that holds a callable, such as `this.cb()`. The call does not
    /// bind a receiver. The value is called as it is. A bound method still carries
    /// its original receiver.
    fn invoke_this_field(&mut self, receiver: Value, member_id: u8, arg_count: usize, root_kind: u8, root_operand: u8) -> Result<(), anyhow::Error> {
        let root = self.take_write_root(root_kind, root_operand);
        let instance_ref = receiver.as_object().as_instance_ptr();
        let callable = self.get_property_by_id(instance_ref, member_id);
        if self.callable_writes_receiver(callable) {
            self.claim_write_ownership_through(receiver, root)?;
        }
        self.stack.set(arg_count, callable);
        self.native_receiver_is_frame_local = self.root_is_frame_local(root_kind, root_operand);
        let called = self.call(arg_count, callable, true);
        self.native_receiver_is_frame_local = false;
        called
    }

    fn invoke_member_slow(&mut self, name: *mut ObjString, arg_count: usize, root_kind: u8, root_operand: u8, is_dot: bool) -> Result<(), anyhow::Error> {
        let root = self.take_write_root(root_kind, root_operand);

        // Resolving the property allocates a bound method, which can collect. The arguments stay
        // on the stack across it, since a copy held anywhere else would not be a root.
        let receiver = self.stack.peek(arg_count);
        self.stack.push(receiver);
        self.stack.push(Value::from(name));

        // The two reads agree everywhere except for dict, where `.` is the method surface and `[]` is the data.
        match is_dot {
            true => self.op_get_property()?,
            false => self.op_get_index()?,
        }

        // The callable takes the receiver's slot, which is where a call reads it from.
        let callable = self.stack.pop();
        if self.callable_writes_receiver(callable) {
            self.claim_write_ownership_through(receiver, root)?;
        }
        self.stack.set(arg_count, callable);
        self.native_receiver_is_frame_local = self.root_is_frame_local(root_kind, root_operand);
        let called = self.call(arg_count, callable, true);
        self.native_receiver_is_frame_local = false;
        called
    }

    fn get_instance_property(&mut self, instance_ptr: *mut ObjInstance, prop: *mut ObjString) -> Option<Value> {
        let instance = unsafe { &*instance_ptr };
        let ty = unsafe { &*instance.ty };

        match self.resolve_cached_type_property(instance.ty, prop) {
            Some(TypeMember::Field(id)) => Some(unsafe { (*instance_ptr).get(id) }),
            Some(TypeMember::Method(id)) => {
                let method = ty.get_method(id);
                Some(self.bind_method(instance_ptr.into(), method))
            },
            None => None
        }
    }

    fn get_property_by_id(&mut self, instance_ref: *mut ObjInstance, id: u8) -> Value {
        // A capturing method and a field holding a function both sit in the slot as a closure, so
        // the value cannot say which it is. Fields are numbered before methods, so the id says it.
        let ty = unsafe { &*(*instance_ref).ty };
        match id >= ty.field_count {
            true => match ty.methods.get(&id) {
                Some(method) => self.bind_method(instance_ref.into(), *method),
                None => unsafe { (*instance_ref).get(id) },
            },
            false => unsafe { (*instance_ref).get(id) },
        }
    }

    fn get_native_type_index(&mut self, native_type_ptr: *mut ObjType, target: Value, prop: Value) -> Result<(), anyhow::Error> {
        let native_type = unsafe { &*native_type_ptr };

        if matches!(prop.kind(), ValueKind::Object(ObjectKind::String)) {
            let Some(method) = native_type.resolve_method(prop.as_object().as_string_ptr()) else {
                return match native_type_ptr {
                    _ if native_type_ptr == self.native_types.array => self.error(format!("Invalid array index: {}", prop.fmt())),
                    _ => self.error(format!("Invalid index: {} does not have method {}", target.fmt(), prop.fmt())),
                }
            };

            let bound_method = self.alloc(ObjBoundMethod::new(target, method));
            self.stack.push(Value::from(bound_method));
            return Ok(());
        }

        let getter = native_type.getter().unwrap();
        self.stack.push(target);
        self.stack.push(prop);
        return self.call_native(1, getter.as_native_function_ptr());
    }

    fn set_native_type_index(&mut self, native_type_ptr: *mut ObjType, target: Value, prop: Value) -> Result<(), anyhow::Error> {
        let native_type = unsafe { &*native_type_ptr };
        let setter = native_type.setter().unwrap();
        let value = self.stack.pop();
        self.stack.push(target);
        self.stack.push(prop);
        self.stack.push(value);
        return self.call_native(2, setter.as_native_function_ptr());
    }

    fn get_instance_index(&mut self, target: Value, prop: Value) -> Result<(), anyhow::Error> {
        let instance_ref = target.as_object().as_instance_ptr();
        let ty = unsafe { &*(*instance_ref).ty };

        // A type instance is indexed only by member name (a string). `inst["x"]` reads the same
        // member as `inst.x`; any non-string key is an error (instances have no keyed data).
        if !matches!(prop.kind(), ValueKind::Object(ObjectKind::String)) {
            return self.error(format!(
                "Invalid index: {} is indexed by member name, not {}",
                unsafe { &*ty.name }.value, prop.fmt()
            ));
        }

        let prop_str = prop.as_object().as_string_ptr();
        // Only externally-visible members are in the name map: private/`inner` members aren't
        // found here (internal `this.x` resolves to a member id and never reaches this path).
        if let Some(value) = self.get_instance_property(instance_ref, prop_str) {
            self.stack.push(value);
            return Ok(());
        }

        self.error(format!(
            "Invalid index: {} doesn't have member {}",
            unsafe { &*ty.name }.value,
            prop.fmt()
        ))
    }


    pub(super) fn running_slot_table(&self) -> u16 {
        let closure = unsafe { (*self.frames.top()).closure };
        match closure.is_null() {
            true => ir::SLOT_ACCEPTS_SCRIPT_FRAME,
            false => unsafe { (*closure).slot_accepts },
        }
    }

    pub(super) fn slot_accepts_at(&self, slot: u8, at: usize) -> u16 {
        self.slot_accepts_in(self.running_slot_table(), slot, at)
    }

    pub(super) fn slot_accepts_in(&self, table: u16, slot: u8, at: usize) -> u16 {
        let Some(rows) = self.chunk.slot_accepts.get(table as usize) else { return ir::SLOT_ACCEPTS_ANYTHING };
        rows.iter().rev()
            .find(|r| r.slot == slot && r.from <= at && at < r.to)
            .map_or(ir::SLOT_ACCEPTS_ANYTHING, |r| r.accepts)
    }


    #[inline]
    pub(super) fn check_arguments_accepted(&mut self, param_accepts: u16, stack_start: *mut Value, arity: usize) -> Result<(), anyhow::Error> {
        if !objects::arguments_may_carry_witness(stack_start, arity) {
            return Ok(());
        }
        self.check_each_argument_accepted(param_accepts, stack_start, arity)
    }

    #[cold]
    pub(super) fn check_each_argument_accepted(&mut self, param_accepts: u16, stack_start: *mut Value, arity: usize) -> Result<(), anyhow::Error> {
        let row = &self.chunk.param_accepts[param_accepts as usize];
        debug_assert_eq!(row.len(), arity, "a callable's accept row covers every parameter");
        let refused = (0..arity.min(row.len())).find_map(|position| {
            let value = unsafe { *stack_start.add(position + 1) };
            let allowed = row[position];
            (objects::may_carry_witness(value) && !self.accepts_value(value, allowed)).then_some((value, allowed))
        });
        match refused {
            Some((value, allowed)) => self.check_value_accepted(value, allowed),
            None => Ok(()),
        }
    }

    #[cold]
    fn store_field(&mut self, instance_ref: *mut ObjInstance, field: u8, value: Value) -> Result<(), anyhow::Error> {
        #[cfg(debug_assertions)]
        assert_field_slot(unsafe { &*instance_ref }, field);

        let accepted = self.accept_field_write(instance_ref, field, value)?;
        self.write_field(accepted);
        Ok(())
    }

    /// Refuses a value the field does not accept.
    #[cold]
    /// What a field accepts. A field id the type does not have refuses nothing.
    pub(super) fn field_accepts(&self, instance_ref: *mut ObjInstance, field: u8) -> u16 {
        let ty = unsafe { &*(*instance_ref).ty };
        ty.field_accepts.get(field as usize).copied().unwrap_or(ir::SLOT_ACCEPTS_ANYTHING)
    }


    fn set_instance_index(&mut self, prop: Value, target: Value) -> Result<(), anyhow::Error> {
        let instance_ref = target.as_object().as_instance_ptr();
        let instance = unsafe { &mut *instance_ref };
        let ty = unsafe { &*instance.ty };

        // Same name-only rule as reads: a non-string key has no member to assign.
        if !matches!(prop.kind(), ValueKind::Object(ObjectKind::String)) {
            return self.error(format!(
                "Invalid index: {} is indexed by member name, not {}",
                unsafe { &*ty.name }.value, prop.fmt()
            ));
        }

        match ty.resolve(prop.as_object().as_string_ptr()) {
            Some(TypeMember::Field(id)) => self.store_field(instance_ref, id, self.stack.peek(0)),
            Some(TypeMember::Method(_)) => self.error(format!("Cannot assign to method '{}'", prop.as_object().as_string())),
            None => self.error(format!(
                "Invalid index: {} doesn't have member {}",
                unsafe { &*ty.name }.value,
                prop.fmt()
            )),
        }
    }

    pub(super) fn op_set_field_pop(&mut self) -> Result<(), anyhow::Error> {
        self.set_field::<true>()
    }

    pub(super) fn op_set_field(&mut self) -> Result<(), anyhow::Error> {
        self.set_field::<false>()
    }

    fn set_field<const POP: bool>(&mut self) -> Result<(), anyhow::Error> {
        let member_id = self.read_next();
        let root_kind = self.read_next();
        let root_operand = self.read_next();
        let target = self.stack.pop();
        if !matches!(target.kind(), ValueKind::Object(ObjectKind::Instance)) {
            return self.error(format!("Invalid property access: {}", target.fmt()));
        }
        self.ensure_mutable(target)?;
        self.arbitrate_write(target, root_kind, root_operand)?;

        let instance_ref = target.as_object().as_instance_ptr();

        // Ask before the pop, which would prune the mark this reads.
        let slot = self.stack.offset(0);
        let value = unsafe { *slot };
        let borrowed = self.ensure_borrowed_does_not_persist(value, slot, root_kind, root_operand)?;
        if POP {
            self.stack.pop();
        }

        // Record before storing, so a store this refuses has not already mutated the instance.
        self.container_took(target, value, borrowed)?;
        self.store_field(instance_ref, member_id, value)
    }

    pub(super) fn op_get_index(&mut self) -> Result<(), anyhow::Error> {
        // Both operands stay on the stack: resolving a member can allocate a bound method,
        // which can trigger gc, and a receiver held only in a local is not a root.
        let prop = self.stack.peek(0);
        let target = self.stack.peek(1);
        let base = self.stack.len() - 2;
        let ValueKind::Object(object_kind) = target.kind() else {
            self.stack.truncate(2);
            return self.error(format!("Invalid property access: {}", target.fmt()));
        };

        let outcome = match object_kind {
            ObjectKind::Instance => self.get_instance_index(target, prop),
            ObjectKind::Array => self.get_native_type_index(self.native_types.array, target, prop),
            ObjectKind::Dict => self.get_dict_index(target, prop),
            _ => self.error(format!("Invalid property access: {}", target.fmt()))
        };

        // The operands are dropped on either outcome. A failed lookup that left the stack deeper
        // than it found it would resume a catch on a stack that is not the one it left.
        if outcome.is_err() {
            self.stack.truncate(self.stack.len() - base);
            return outcome;
        }
        let result = self.stack.pop();
        self.stack.truncate(self.stack.len() - base);
        self.stack.push(result);
        Ok(())
    }

    #[inline]
    fn store_operands(&self) -> (Value, Value, Value) {
        (self.stack.peek(2), self.stack.peek(1), self.stack.peek(0))
    }

    #[inline]
    fn drop_store_path(&mut self) {
        let value = self.stack.peek(0);
        self.stack.pop();
        self.stack.pop();
        self.stack.set(0, value);
    }

    fn ensure_mutable(&self, target: Value) -> Result<(), anyhow::Error> {
        if !matches!(target.kind(), ValueKind::Object(_)) {
            return Ok(());
        }
        if target.as_object().is_immutable() {
            return self.immutable_error(target);
        }
        if target.as_object().is_write_retired() {
            let label = format!("`{}` is written here", self.get_source_position().snippet());
            return self.error_labeled(objects::WROTE_TRANSFERRED_ELEMENT, label);
        }
        Ok(())
    }

    #[inline]
    pub(super) fn container_took_from(&mut self, container: Value, slot: *mut Value) -> Result<(), anyhow::Error> {
        let value = unsafe { *slot };
        let borrowed = self.slot_carries_borrow(slot, value);
        self.container_took(container, value, borrowed)
    }

    pub(super) fn record_held_borrow_from(&mut self, container: Value, slot: *mut Value) {
        if self.slot_carries_borrow(slot, unsafe { *slot }) {
            objects::mark_holds_borrow(container);
        }
    }

    pub(super) fn container_took(&mut self, container: Value, value: Value, borrowed: bool) -> Result<(), anyhow::Error> {
        objects::container_took(self, container, value, borrowed)
            .map_err(|_| self.gave_transferred_element_error())
    }

    #[cold]
    #[inline(never)]
    fn gave_transferred_element_error(&self) -> anyhow::Error {
        self.raise(Diagnostic::new(objects::GAVE_TRANSFERRED_ELEMENT, self.get_source_position().clone())
            .with_label("this store would hand the container a writer")
            .with_help("write-ownership was given away and nothing handed it back; reading the value is still fine"))
            .unwrap_err()
    }

    #[inline]
    pub(super) fn slot_carries_borrow(&self, slot: *mut Value, value: Value) -> bool {
        objects::carries_borrow(value) || self.stack.is_borrowed(slot)
    }

    pub(super) fn ensure_borrowed_does_not_persist(&self, value: Value, slot: *mut Value, root_kind: u8, root_operand: u8) -> Result<bool, anyhow::Error> {
        let borrowed = self.slot_carries_borrow(slot, value);
        if borrowed && !self.root_is_frame_local(root_kind, root_operand) {
            return Err(self.persisted_borrow_error());
        }
        Ok(borrowed)
    }

    #[cold]
    #[inline(never)]
    pub(super) fn persisted_borrow_error(&self) -> anyhow::Error {
        let destination = self.get_source_position().clone();
        let site = self.code_index_at(self.ip);
        let named = self.chunk.source_at(site, ir::SourceRole::StoredName);
        let stored = named.or_else(|| self.chunk.source_at(site, ir::SourceRole::StoredValue));
        let Some(pos) = stored else {
            return self.raise(Diagnostic::new(objects::PERSISTED_BORROW, destination)
                .with_label("this store outlives the borrow")
                .with_help(PERSIST_HELP)).unwrap_err();
        };
        let help = match named {
            Some(pos) => format!("you can retain `{0}` by declaring the parameter `*{0}`", pos.snippet()),
            None => PERSIST_HELP.to_string(),
        };
        self.raise(Diagnostic::new(objects::PERSISTED_BORROW, pos.clone())
            .with_label(format!("`{}` is borrowed", pos.snippet()))
            .with_context_span(destination, "this destination outlives the borrow")
            .with_help(help))
            .unwrap_err()
    }

    pub(super) fn root_is_frame_local(&self, kind: u8, operand: u8) -> bool {
        ir::write_root_kind(kind) == ir::WRITE_ROOT_LOCAL
            && self.frame_arity().is_some_and(|arity| operand as usize > arity)
    }

    /// Settles the one-writer rule for a store.
    fn arbitrate_write(&mut self, target: Value, kind: u8, operand: u8) -> Result<(), anyhow::Error> {
        if kind & ir::WRITE_ROOT_UNSHARED != 0 {
            debug_assert!(ir::write_root_kind(kind) != ir::WRITE_ROOT_STASH,
                "a stashed root is popped by the store, so it can never be proven unshared");
            return Ok(());
        }
        let root = self.take_write_root(kind, operand);
        self.claim_write_ownership_through(target, root)
    }

    fn frame_arity(&self) -> Option<usize> {
        let frame = self.frames.top();
        if frame.is_null() {
            return None;
        }
        let closure = unsafe { (*frame).closure };
        match closure.is_null() {
            true => None,
            false => Some(unsafe { (*closure).arity } as usize),
        }
    }

    pub(super) fn op_set_index(&mut self) -> Result<(), anyhow::Error> {
        let root_kind = self.read_next();
        let root_operand = self.read_next();
        let (target, prop, stored) = self.store_operands();
        let ValueKind::Object(object_kind) = target.kind() else {
            return self.error(format!("Invalid property access: {}", target.fmt()));
        };
        self.ensure_mutable(target)?;
        self.arbitrate_write(target, root_kind, root_operand)?;
        let borrowed = self.ensure_borrowed_does_not_persist(stored, self.stack.offset(0), root_kind, root_operand)?;

        self.container_took(target, stored, borrowed)?;
        self.drop_store_path();

        match object_kind {
            ObjectKind::Instance => self.set_instance_index(prop, target)?,
            ObjectKind::Array => self.set_native_type_index(self.native_types.array, target, prop)?,
            ObjectKind::Dict => self.set_dict_index(target, prop)?,
            _ => self.error(format!("Invalid property access: {}", target.fmt()))?,
        }
        Ok(())
    }

    pub(super) fn op_get_index_or_null(&mut self) {
        let const_idx = self.read_next() as usize;
        let key = self.chunk.constants[const_idx];
        // The receiver stays on the stack: reading a member can allocate a bound method.
        let receiver = self.stack.peek(0);
        let value = match receiver.kind() {
            ValueKind::Object(ObjectKind::Dict) => {
                unsafe { &*receiver.as_object().as_dict_ptr() }.entries.get(&DictKey(key)).copied().unwrap_or(Value::NULL)
            },
            ValueKind::Object(ObjectKind::Instance) if matches!(key.kind(), ValueKind::Object(ObjectKind::String)) => {
                let instance = receiver.as_object().as_instance_ptr();
                self.get_instance_property(instance, key.as_object().as_string_ptr()).unwrap_or(Value::NULL)
            },
            _ => Value::NULL,
        };
        self.stack.set(0, value);
    }

    /// Dotted access `target.name`.
    pub(super) fn op_get_property(&mut self) -> Result<(), anyhow::Error> {
        // Both operands stay on the stack: resolving a member can allocate a bound method
        // and trigger gc, and a receiver held only in a local is not a gc root.
        let prop = self.stack.peek(0);
        let target = self.stack.peek(1);
        let base = self.stack.len() - 2;
        let ValueKind::Object(object_kind) = target.kind() else {
            self.stack.truncate(2);
            return self.error(format!("Invalid property access: {}", target.fmt()));
        };

        let outcome = match object_kind {
            ObjectKind::Instance => self.get_instance_index(target, prop),
            ObjectKind::Array => self.get_native_type_index(self.native_types.array, target, prop),
            ObjectKind::Dict => self.get_dict_method(target, prop),
            _ => self.error(format!("Invalid property access: {}", target.fmt()))
        };

        // The operands are dropped on either outcome. A failed lookup that left the stack deeper
        // than it found it would resume a catch on a stack that is not the one it left.
        if outcome.is_err() {
            self.stack.truncate(self.stack.len() - base);
            return outcome;
        }
        let result = self.stack.pop();
        self.stack.truncate(self.stack.len() - base);
        self.stack.push(result);
        Ok(())
    }

    /// Dotted store `target.name = v`.
    pub(super) fn op_set_property(&mut self) -> Result<(), anyhow::Error> {
        let root_kind = self.read_next();
        let root_operand = self.read_next();
        let (target, prop, stored) = self.store_operands();
        let ValueKind::Object(object_kind) = target.kind() else {
            return self.error(format!("Invalid property access: {}", target.fmt()));
        };
        self.ensure_mutable(target)?;
        self.arbitrate_write(target, root_kind, root_operand)?;
        let borrowed = self.ensure_borrowed_does_not_persist(stored, self.stack.offset(0), root_kind, root_operand)?;

        self.container_took(target, stored, borrowed)?;
        self.drop_store_path();

        match object_kind {
            ObjectKind::Instance => self.set_instance_index(prop, target)?,
            ObjectKind::Array => self.set_native_type_index(self.native_types.array, target, prop)?,
            ObjectKind::Dict => self.error(format!(
                "Cannot assign to dict method '{}'; dict data is assigned with []",
                prop.as_object().as_string()
            ))?,
            _ => self.error(format!("Invalid property access: {}", target.fmt()))?,
        }
        Ok(())
    }

    /// Whether a member satisfies what its declaration admits.
    pub(super) fn op_member_admits(&mut self) {
        let const_idx = self.read_next() as usize;
        let allowed = self.read_allowed_witnesses();
        let key = self.chunk.constants[const_idx];
        let receiver = self.stack.pop();

        let admits = match self.member_value(receiver, key) {
            // A method is a reference, never null and never a witness, so it always admits.
            MemberValue::Method => true,
            // A surface asks for a member, so a receiver without one exposes no surface.
            MemberValue::Absent => false,
            MemberValue::Value(value) => self.accepts_value(value, allowed),
        };
        self.stack.push(Value::from(admits));
    }

    /// A member's value for the admission test.
    fn member_value(&self, receiver: Value, key: Value) -> MemberValue {
        match receiver.kind() {
            ValueKind::Object(ObjectKind::Dict) => {
                let entries = &unsafe { &*receiver.as_object().as_dict_ptr() }.entries;
                match entries.get(&DictKey(key)) {
                    Some(value) => MemberValue::Value(*value),
                    None => MemberValue::Absent,
                }
            },
            ValueKind::Object(ObjectKind::Instance) if matches!(key.kind(), ValueKind::Object(ObjectKind::String)) => {
                let instance_ptr = receiver.as_object().as_instance_ptr();
                let ty = unsafe { &*(*instance_ptr).ty };
                match ty.resolve(key.as_object().as_string_ptr()) {
                    Some(TypeMember::Field(id)) => MemberValue::Value(unsafe { (*instance_ptr).get(id) }),
                    Some(TypeMember::Method(_)) => MemberValue::Method,
                    None => MemberValue::Absent,
                }
            },
            _ => MemberValue::Absent,
        }
    }

    pub(super) fn op_has_member(&mut self) {
        let const_idx = self.read_next() as usize;
        let key = self.chunk.constants[const_idx];
        let receiver = self.stack.pop();
        let present = match receiver.kind() {
            ValueKind::Object(ObjectKind::Dict) => {
                unsafe { &*receiver.as_object().as_dict_ptr() }.entries.contains_key(&DictKey(key))
            },
            ValueKind::Object(ObjectKind::Instance) if matches!(key.kind(), ValueKind::Object(ObjectKind::String)) => {
                let ty = unsafe { &*(*receiver.as_object().as_instance_ptr()).ty };
                ty.resolve(key.as_object().as_string_ptr()).is_some()
            },
            _ => false,
        };
        self.stack.push(Value::from(present));
    }

    pub(super) fn op_is_shaped(&mut self) {
        let receiver = self.stack.pop();
        let shaped = matches!(receiver.kind(),
            ValueKind::Object(ObjectKind::Dict) | ValueKind::Object(ObjectKind::Instance));
        self.stack.push(Value::from(shaped));
    }

    /// Reads one element of a value being matched, by an offset from the front or from the back.
    pub(super) fn op_array_elem(&mut self) {
        let offset = self.read_next() as usize;
        let from_back = self.read_next() != 0;
        let receiver = self.stack.peek(0);
        let value = match receiver.kind() {
            ValueKind::Object(ObjectKind::Array) => {
                let values = &unsafe { &*receiver.as_object().as_array_ptr() }.values;
                let index = match from_back {
                    true => values.len().checked_sub(offset),
                    false => Some(offset),
                };
                index.and_then(|i| values.get(i)).copied().unwrap_or(Value::NULL)
            },
            _ => Value::NULL,
        };
        self.stack.set(0, value);
    }

    pub(super) fn op_array_len(&mut self) {
        let receiver = self.stack.pop();
        let len = match receiver.kind() {
            ValueKind::Object(ObjectKind::Array) => unsafe { &*receiver.as_object().as_array_ptr() }.values.len() as f64,
            _ => -1.0,
        };
        self.stack.push(Value::from(len));
    }

    fn get_dict_method(&mut self, target: Value, prop: Value) -> Result<(), anyhow::Error> {
        let dict_type = unsafe { &*self.native_types.dict };
        if matches!(prop.kind(), ValueKind::Object(ObjectKind::String)) {
            if let Some(method) = dict_type.resolve_method(prop.as_object().as_string_ptr()) {
                let bound = self.alloc(ObjBoundMethod::new(target, method));
                self.stack.push(Value::from(bound));
                return Ok(());
            }
            return self.error(format!("dict has no method '{}'", prop.as_object().as_string()));
        }
        self.error(format!("Invalid dict property: {}", prop.fmt()))
    }

    fn get_dict_index(&mut self, target: Value, prop: Value) -> Result<(), anyhow::Error> {
        let dict = unsafe { &*target.as_object().as_dict_ptr() };
        let value = dict.entries.get(&DictKey(prop)).copied().unwrap_or(Value::NULL);
        self.stack.push(value);
        Ok(())
    }

    fn set_dict_index(&mut self, target: Value, prop: Value) -> Result<(), anyhow::Error> {
        let value = self.stack.peek(0);
        let dict = unsafe { &mut *target.as_object().as_dict_ptr() };
        dict.entries.insert(DictKey(prop), value);
        Ok(())
    }

    pub(super) fn op_get_field(&mut self) -> Result<(), anyhow::Error> {
        let member_id = self.read_next();
        let value = self.stack.pop();
        if !matches!(value.kind(), ValueKind::Object(ObjectKind::Instance)) {
            return self.error(format!("Invalid property access: {}", value.fmt()));
        }

        let object = value.as_object();
        let instance_ref = object.as_instance_ptr();
        let value = self.get_property_by_id(instance_ref, member_id);
        self.stack.push(value);
        Ok(())
    }
}

#[cfg(debug_assertions)]
fn assert_field_slot(instance: &ObjInstance, member_id: u8) {
    let ty = unsafe { &*instance.ty };
    debug_assert!(member_id < ty.field_count, "write to method slot {member_id} of {}", unsafe { &*ty.name }.value);
}

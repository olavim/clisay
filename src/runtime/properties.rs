use super::*;
use crate::core::native::NativeType;

enum MemberValue {
    Value(Value),
    Method,
    Absent,
}

#[inline]
pub(super) fn array_element(target: Value, index: usize) -> Value {
    unsafe { ObjArray::get(target.as_object().as_array_ptr(), index) }
}

#[inline]
pub(super) fn array_element_index(target: Value, key: Value) -> Option<usize> {
    if !target.is_object() || target.as_object().kind() != ObjectKind::Array {
        return None;
    }
    let len = unsafe { (*target.as_object().as_array_ptr()).len } as usize;
    NativeArray::element_index(key, len)
}

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

    fn bind_method(&mut self, target: Value, method: Object) -> Result<Value, anyhow::Error> {
        if let Some(name) = anchor_receiver_method_name(method) {
            return self.refuse_binding_anchored_method(name);
        }
        if method.tag() == objects::TAG_FUNCTION {
            let closure = self.create_closure(method.as_function_ptr());
            self.stack.push(Value::from(closure));
            let bound = self.alloc(ObjBoundMethod::new(target, closure));
            self.stack.pop();
            Ok(Value::from(bound))
        } else {
            Ok(Value::from(self.alloc(ObjBoundMethod::new(target, method))))
        }
    }

    #[cold]
    #[inline(never)]
    fn refuse_binding_anchored_method(&self, name: *mut ObjString) -> Result<Value, anyhow::Error> {
        Err(self.error_help(
            format!("`{}` wants an anchor receiver, so it cannot be bound as a value", unsafe { &*name }.value),
            "a bound method keeps a copy of its receiver, so the method would write only the copy",
        ).unwrap_err())
    }

    fn short_circuit_on_witness_callee(&mut self, flags: u8, callable: Value, arg_count: usize) -> bool {
        if flags & ir::CALL_TESTS_CALLEE == 0 || !self.is_witness(callable) {
            return false;
        }
        self.stack.truncate(arg_count);
        self.stack.set(0, callable);
        true
    }

    #[inline]
    pub(super) fn op_invoke(&mut self) -> Result<(), anyhow::Error> {
        let name = self.read_member_name();
        let flags = self.read_next();
        let arg_count = self.read_next() as usize;
        let is_dot = self.read_next() != 0;
        let receiver = self.anchor_value(self.stack.peek(arg_count))?;

        if matches!(receiver.kind(), ValueKind::Object(ObjectKind::Instance)) {
            let type_ptr = unsafe { (*receiver.as_object().as_instance_ptr()).ty };
            if let Some(TypeMember::Method(id)) = self.resolve_cached_type_property(type_ptr, name) {
                let method = unsafe { &*type_ptr }.get_method(id);
                if matches!(method.tag(), objects::TAG_FUNCTION | objects::TAG_CLOSURE) {
                    return self.invoke_method(method, arg_count, flags);
                }
            }
        }

        self.invoke_member_slow(name, arg_count, is_dot, flags)
    }

    fn invoke_method(&mut self, method: Object, arg_count: usize, flags: u8) -> Result<(), anyhow::Error> {
        if method.tag() == objects::TAG_CLOSURE {
            return self.enter_closure(arg_count, method.as_closure_ptr(), flags);
        }
        let closure = self.create_closure(method.as_function_ptr());
        let mark = self.hold(&[Value::from(closure)]);
        let entered = self.enter_closure(arg_count, closure.as_closure_ptr(), flags);
        self.release_held(mark);
        entered
    }

    pub(super) fn op_invoke_this(&mut self) -> Result<(), anyhow::Error> {
        let member_id = self.read_next();
        let flags = self.read_next();
        let arg_count = self.read_next() as usize;
        let receiver = self.anchor_value(self.stack.peek(arg_count))?;
        let ValueKind::Object(ObjectKind::Instance) = receiver.kind() else {
            return self.error(format!("Invalid property access: {}", receiver.fmt()));
        };

        // Fields are numbered before methods, so the id says which kind this is without reading
        // the value. Only a function or a closure takes a frame. Anything else is called as a value.
        let ty = unsafe { &*(*receiver.as_object().as_instance_ptr()).ty };
        if member_id >= ty.field_count {
            if let Some(method) = ty.methods.get(&member_id).copied() {
                if matches!(method.tag(), objects::TAG_FUNCTION | objects::TAG_CLOSURE) {
                    return self.invoke_method(method, arg_count, flags);
                }
            }
        }

        self.invoke_this_field(receiver, member_id, arg_count, flags)
    }

    fn invoke_this_field(&mut self, receiver: Value, member_id: u8, arg_count: usize, flags: u8) -> Result<(), anyhow::Error> {
        let instance_ref = receiver.as_object().as_instance_ptr();
        let callable = self.get_property_by_id(instance_ref, member_id)?;

        if self.short_circuit_on_witness_callee(flags, callable, arg_count) {
            return Ok(());
        }

        if flags & ir::CALL_PASSES_ANCHOR_RECEIVER != 0 {
            let name = member_name(unsafe { &*(*instance_ref).ty }, member_id).unwrap_or_else(|| format!("member {member_id}"));
            return self.refuse_unwanted_anchor_receiver(&name);
        }

        self.stack.set(arg_count, callable);
        self.call(arg_count, callable, flags)
    }

    fn invoke_member_slow(&mut self, name: *mut ObjString, arg_count: usize, is_dot: bool, flags: u8) -> Result<(), anyhow::Error> {
        let receiver = self.anchor_value(self.stack.peek(arg_count))?;
        if let Some(native) = self.native_method_of(receiver, name, is_dot) {
            return self.call_native(arg_count, native, flags);
        }
        // Resolving the property allocates a bound method, which can collect.
        self.stack.push(receiver);
        self.stack.push(Value::from(name));

        // The two reads agree everywhere except for dict, where `.` is the method surface and `[]` is the data.
        match is_dot {
            true => self.op_get_property()?,
            false => self.op_get_index()?,
        }

        let callable = self.stack.pop();
        if self.short_circuit_on_witness_callee(flags, callable, arg_count) {
            return Ok(());
        }

        // A method that takes `&var this` was invoked earlier, so any callee left is a value a member holds.
        if flags & ir::CALL_PASSES_ANCHOR_RECEIVER != 0 {
            return self.refuse_unwanted_anchor_receiver(&unsafe { &*name }.value);
        }

        // The callable takes the receiver's slot, which is where a call reads it from.
        self.stack.set(arg_count, callable);
        self.call(arg_count, callable, flags)
    }

    fn native_method_of(&self, receiver: Value, name: *mut ObjString, is_dot: bool) -> Option<*mut ObjNativeFn> {
        let (ty, kind) = match receiver.kind() {
            ValueKind::Object(ObjectKind::Array) => (self.native_types.array, NativeType::Array),
            ValueKind::Object(ObjectKind::Dict) => (self.native_types.dict, NativeType::Dict),
            _ => return None,
        };
        if !is_dot && kind.keys_can_shadow_members() {
            return None;
        }
        let method = unsafe { &*ty }.resolve_method(name)?;
        (method.tag() == objects::TAG_NATIVE_FUNCTION).then(|| method.as_native_function_ptr())
    }

    fn get_instance_property(&mut self, instance_ptr: *mut ObjInstance, prop: *mut ObjString) -> Result<Option<Value>, anyhow::Error> {
        let instance = unsafe { &*instance_ptr };
        let ty = unsafe { &*instance.ty };

        match self.resolve_cached_type_property(instance.ty, prop) {
            Some(TypeMember::Field(id)) => Ok(Some(unsafe { ObjInstance::get(instance_ptr, id) })),
            Some(TypeMember::Method(id)) => {
                let method = ty.get_method(id);
                self.bind_method(instance_ptr.into(), method).map(Some)
            },
            None => Ok(None)
        }
    }

    fn get_property_by_id(&mut self, instance_ref: *mut ObjInstance, id: u8) -> Result<Value, anyhow::Error> {
        // A capturing method and a field holding a function both sit in the slot as a closure, so
        // the value cannot say which it is. Fields are numbered before methods, so the id says it.
        let ty = unsafe { &*(*instance_ref).ty };
        match id >= ty.field_count {
            true => match ty.methods.get(&id) {
                Some(method) => self.bind_method(instance_ref.into(), *method),
                None => Ok(unsafe { ObjInstance::get(instance_ref, id) }),
            },
            false => Ok(unsafe { ObjInstance::get(instance_ref, id) }),
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

            let bound_method = self.bind_method(target, method)?;
            self.stack.push(bound_method);
            return Ok(());
        }

        let getter = native_type.getter().unwrap();
        self.stack.push(target);
        self.stack.push(prop);
        return self.call_native(1, getter.as_native_function_ptr(), 0);
    }

    fn set_native_type_index(&mut self, native_type_ptr: *mut ObjType, target: Value, prop: Value) -> Result<(), anyhow::Error> {
        let native_type = unsafe { &*native_type_ptr };
        let setter = native_type.setter().unwrap();
        let value = self.stack.pop();
        self.stack.push(target);
        self.stack.push(prop);
        self.stack.push(value);
        return self.call_native(2, setter.as_native_function_ptr(), 0);
    }

    fn get_instance_index(&mut self, target: Value, prop: Value) -> Result<(), anyhow::Error> {
        let instance_ref = target.as_object().as_instance_ptr();
        let ty = unsafe { &*(*instance_ref).ty };

        if prop.is_member_key() {
            let value = self.get_property_by_id(instance_ref, prop.member_key_id())?;
            self.stack.push(value);
            return Ok(());
        }

        if !matches!(prop.kind(), ValueKind::Object(ObjectKind::String)) {
            return self.error(format!(
                "Invalid index: {} is indexed by member name, not {}",
                unsafe { &*ty.name }.value, prop.fmt()
            ));
        }

        let type_ptr = unsafe { (*instance_ref).ty };
        match self.resolve_cached_type_property(type_ptr, prop.as_object().as_string_ptr()) {
            Some(TypeMember::Field(id)) => self.stack.push(unsafe { ObjInstance::get(instance_ref, id) }),
            Some(TypeMember::Method(id)) => {
                let bound = self.bind_method(target, ty.get_method(id))?;
                self.stack.push(bound);
            },
            None => return self.refuse_absent_member(instance_ref, ty, prop),
        }
        Ok(())
    }

    #[cold]
    fn refuse_absent_member(&self, instance: *mut ObjInstance, ty: &ObjType, prop: Value) -> Result<(), anyhow::Error> {
        let message = format!("Invalid index: {} doesn't have member {}", unsafe { &*ty.name }.value, prop.fmt());
        match unsafe { (*instance).header.has(objects::FLAG_IS_REF) } {
            true => self.error_help(message, "a `Ref` is read and written with `@`, `update` or `mutate`"),
            false => self.error(message),
        }
    }


    pub(super) fn running_slot_witness_set_pool_id(&self) -> u16 {
        let closure = unsafe { (*self.frames.top()).closure };
        match closure.is_null() {
            true => ir::SCRIPT_SLOT_WITNESS_SET_POOL_ID,
            false => unsafe { (*closure).slot_witness_set_pool_id },
        }
    }

    /// The witness set of the running frame's `slot` at instruction `at`.
    pub(super) fn slot_witness_set_pool_id_at(&self, slot: u8, at: usize) -> u16 {
        let Some(slot_witness_sets) = self.chunk.slot_witness_set_pool.get(self.running_slot_witness_set_pool_id() as usize) else { return ir::NO_WITNESS_SET };
        slot_witness_sets.iter().rev()
            .find(|s| s.slot == slot && s.from <= at && at < s.to)
            .map_or(ir::NO_WITNESS_SET, |s| s.witness_set_pool_id)
    }


    #[inline(always)]
    pub(super) fn check_arguments_accepted(&mut self, params: u16, stack_start: *mut Value, arity: usize, flags: u8) -> Result<(), anyhow::Error> {
        if flags & ir::CALL_ARGS_SETTLED != 0 {
            return Ok(());
        }

        self.at_forced_check();

        let kinds_proven = flags & ir::CALL_KINDS_PROVEN != 0;
        let check_accepts = objects::arguments_need_accepts_check(stack_start, arity);
        match check_accepts || !kinds_proven && params & ir::PARAM_LIST_HAS_ANCHOR != 0 {
            true => self.check_arguments(params & ir::PARAM_LIST_POOL_ID, stack_start, arity, kinds_proven, check_accepts),
            false => Ok(()),
        }
    }

    #[cold]
    #[inline(never)]
    fn check_arguments(&mut self, param_list_pool_id: u16, stack_start: *mut Value, arity: usize, kinds_proven: bool, check_accepts: bool) -> Result<(), anyhow::Error> {
        let forced = self.at_forced_check();
        match self.check_argument_list(param_list_pool_id, stack_start, arity, kinds_proven, check_accepts) {
            Err(_) if forced => self.refuted_elision_error("an argument proven accepted is refused"),
            checked => checked,
        }
    }

    fn check_argument_list(&mut self, param_list_pool_id: u16, stack_start: *mut Value, arity: usize, kinds_proven: bool, check_accepts: bool) -> Result<(), anyhow::Error> {
        if !kinds_proven {
            self.check_argument_kinds(param_list_pool_id, stack_start, arity)?;
        }
        match check_accepts {
            true => self.check_each_argument_accepted(param_list_pool_id, stack_start, arity),
            false => Ok(()),
        }
    }

    #[cold]
    fn check_argument_kinds(&mut self, param_list_pool_id: u16, stack_start: *mut Value, arity: usize) -> Result<(), anyhow::Error> {
        let param_list = &self.chunk.param_list_pool[param_list_pool_id as usize];
        for position in 0..arity.min(param_list.len()) {
            let value = unsafe { *stack_start.add(position + 1) };
            let takes_anchor = param_list[position] & ir::PARAM_IS_ANCHOR != 0;
            if takes_anchor != value.is_anchor() {
                return match takes_anchor {
                    true => self.error("a `&var` parameter takes an anchor"),
                    false => self.error("this parameter does not take an anchor"),
                };
            }
        }
        Ok(())
    }

    #[cold]
    pub(super) fn check_each_argument_accepted(&mut self, param_list_pool_id: u16, stack_start: *mut Value, arity: usize) -> Result<(), anyhow::Error> {
        let param_list = &self.chunk.param_list_pool[param_list_pool_id as usize];
        debug_assert_eq!(param_list.len(), arity, "a callable's parameter list covers every parameter");
        let refused = (0..arity.min(param_list.len())).find_map(|position| {
            let value = unsafe { *stack_start.add(position + 1) };
            let witness_set_pool_id = param_list[position] & !ir::PARAM_IS_ANCHOR;
            (objects::needs_accepts_check(value) && !self.accepts_value(value, witness_set_pool_id)).then_some((value, witness_set_pool_id))
        });
        match refused {
            Some((value, witness_set_pool_id)) => self.check_value_accepted(value, witness_set_pool_id),
            None => Ok(()),
        }
    }

    /// Stores into a field. With `requires_var`, a field not declared `var` refuses the store.
    #[inline]
    pub(super) fn store_field(&mut self, instance_ref: *mut ObjInstance, field: u8, value: Value, requires_var: bool) -> Result<Value, anyhow::Error> {
        #[cfg(debug_assertions)]
        assert_field_slot(unsafe { &*instance_ref }, field);

        if requires_var && !unsafe { &*(*instance_ref).ty }.var_fields.has(field) {
            return Err(self.non_var_field_error(instance_ref, field));
        }

        if value.is_number() || value.is_bool() {
            objects::detach_replaced(unsafe { &(*instance_ref).header }, || unsafe { ObjInstance::get(instance_ref, field) });
            unsafe { ObjInstance::set(instance_ref, field, value) };
            return Ok(value);
        }

        self.store_field_object(instance_ref, field, value)
    }

    #[inline(never)]
    fn store_field_object(&mut self, instance_ref: *mut ObjInstance, field: u8, value: Value) -> Result<Value, anyhow::Error> {
        let accepted = self.accept_field_write(instance_ref, field, value)?;
        objects::detach_replaced(unsafe { &(*instance_ref).header }, || unsafe { ObjInstance::get(instance_ref, field) });
        let stored = accepted.value;
        self.write_field(accepted);
        Ok(stored)
    }

    #[inline]
    pub(super) fn field_witness_set_pool_id(&self, instance_ref: *mut ObjInstance, field: u8) -> u16 {
        let ty = unsafe { &*(*instance_ref).ty };
        ty.field_witness_set_pool_ids.get(field as usize).copied().unwrap_or(ir::NO_WITNESS_SET)
    }


    fn set_instance_index(&mut self, prop: Value, target: Value, requires_var: bool) -> Result<(), anyhow::Error> {
        let instance_ref = target.as_object().as_instance_ptr();
        let instance = unsafe { &mut *instance_ref };
        let ty = unsafe { &*instance.ty };

        if prop.is_member_key() {
            return self.store_named_field(instance_ref, prop.member_key_id(), requires_var);
        }

        // Same name-only rule as reads: a non-string key has no member to assign.
        if !matches!(prop.kind(), ValueKind::Object(ObjectKind::String)) {
            return self.error(format!(
                "Invalid index: {} is indexed by member name, not {}",
                unsafe { &*ty.name }.value, prop.fmt()
            ));
        }

        match self.resolve_cached_type_property(instance.ty, prop.as_object().as_string_ptr()) {
            Some(TypeMember::Field(id)) => self.store_named_field(instance_ref, id, requires_var),
            Some(TypeMember::Method(_)) => self.error(format!("Cannot assign to method '{}'", prop.as_object().as_string())),
            None => self.refuse_absent_member(instance_ref, ty, prop),
        }
    }

    #[inline]
    pub(super) fn store_named_field(&mut self, instance_ref: *mut ObjInstance, field: u8, requires_var: bool) -> Result<(), anyhow::Error> {
        let stored = self.store_field(instance_ref, field, self.stack.peek(0), requires_var)?;
        self.stack.set(0, stored);
        Ok(())
    }

    #[cold]
    pub(super) fn anchor_admits_mismatch(&self) -> anyhow::Error {
        self.error("an anchor and the parameter it fills do not admit the same values").unwrap_err()
    }

    #[cold]
    pub(super) fn unanchorable_field_error(&self, instance_ref: *mut ObjInstance, field: u8) -> anyhow::Error {
        let label = field_label(instance_ref, field);
        if !unsafe { &*(*instance_ref).ty }.var_fields.has(field) {
            return self.error(format!("Cannot anchor non-var field `{label}`")).unwrap_err();
        }
        self.error(format!("Cannot anchor field `{label}`; it owes an obligation without a witness")).unwrap_err()
    }

    #[cold]
    pub(super) fn non_var_field_error(&self, instance_ref: *mut ObjInstance, field: u8) -> anyhow::Error {
        self.error(format!("Cannot reassign field `{}`", field_label(instance_ref, field))).unwrap_err()
    }

    pub(super) fn op_get_member(&mut self) -> Result<(), anyhow::Error> {
        let name = self.read_member_name();
        let target = self.stack.peek(0);
        if let Some((instance, id)) = self.cached_field(target, name) {
            self.stack.set(0, unsafe { ObjInstance::get(instance, id) });
            return Ok(());
        }
        self.stack.push(Value::from(name));
        self.op_get_property()
    }

    pub(super) fn op_set_member(&mut self) -> Result<(), anyhow::Error> {
        let name = self.read_member_name();
        let target = self.stack.peek(0);

        if let Some((instance, id)) = self.cached_field(target, name) {
            let stored = self.store_field(instance, id, self.stack.peek(1), true)?;
            self.stack.pop();
            self.stack.set(0, stored);
            return Ok(());
        }

        let value = self.stack.peek(1);
        self.stack.set(1, target);
        self.stack.set(0, Value::from(name));
        self.stack.push(value);
        self.set_property(true)
    }

    #[inline]
    fn read_member_name(&mut self) -> *mut ObjString {
        let idx = self.read_next() as usize;
        self.chunk.constants[idx].as_object().as_string_ptr()
    }

    /// The instance and field id a key reaches on `target`, when it names a field.
    #[inline]
    pub(super) fn field_at(&mut self, target: Value, key: Value) -> Option<(*mut ObjInstance, u8)> {
        if key.is_member_key() {
            let ValueKind::Object(ObjectKind::Instance) = target.kind() else { return None };
            let instance = target.as_object().as_instance_ptr();
            let id = key.member_key_id();
            return (id < unsafe { &*(*instance).ty }.field_count).then_some((instance, id));
        }
        match key.kind() {
            ValueKind::Object(ObjectKind::String) => self.cached_field(target, key.as_object().as_string_ptr()),
            _ => None,
        }
    }

    /// The instance and field id `.name` reaches on `target`, when it names a field.
    #[inline]
    fn cached_field(&mut self, target: Value, name: *mut ObjString) -> Option<(*mut ObjInstance, u8)> {
        let ValueKind::Object(ObjectKind::Instance) = target.kind() else { return None };
        let instance = target.as_object().as_instance_ptr();
        match self.resolve_cached_type_property(unsafe { (*instance).ty }, name)? {
            TypeMember::Field(id) => Some((instance, id)),
            TypeMember::Method(_) => None,
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
        let target = self.stack.peek(0);
        if !matches!(target.kind(), ValueKind::Object(ObjectKind::Instance)) {
            return self.error(format!("Invalid property access: {}", target.fmt()));
        }

        let instance_ref = target.as_object().as_instance_ptr();
        let stored = self.store_field(instance_ref, member_id, self.stack.peek(1), false)?;

        self.stack.pop();
        match POP {
            true => { self.stack.pop(); },
            false => { self.stack.set(0, stored); },
        }
        Ok(())
    }

    pub(super) fn op_get_index(&mut self) -> Result<(), anyhow::Error> {
        // Both operands stay on the stack: resolving a member can allocate a bound method,
        // which can trigger gc, and a receiver held only in a local is not a root.
        let prop = self.stack.peek(0);
        let target = self.stack.peek(1);

        if let Some(index) = array_element_index(target, prop) {
            let value = array_element(target, index);
            self.stack.truncate(2);
            self.stack.push(value);
            return Ok(());
        }

        let operands_start = self.stack.len() - 2;
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
            self.stack.truncate(self.stack.len() - operands_start);
            return outcome;
        }
        let result = self.stack.pop();
        self.stack.truncate(self.stack.len() - operands_start);
        self.stack.push(result);
        Ok(())
    }

    #[inline]
    fn store_operands(&self) -> (Value, Value) {
        (self.stack.peek(2), self.stack.peek(1))
    }

    #[inline]
    fn drop_store_path(&mut self) {
        let value = self.stack.peek(0);
        self.stack.pop();
        self.stack.pop();
        self.stack.set(0, value);
    }

    #[inline]
    pub(super) fn store_array_element(&mut self, target: Value, index: usize, value: Value) -> Result<Value, anyhow::Error> {
        debug_assert!(!value.is_anchor(), "cannot store an anchor into an array");
        let stored = self.share_into_container(target, value)?;
        objects::detach_replaced(unsafe { &*target.as_object().as_header_ptr() }, || array_element(target, index));
        unsafe { ObjArray::set(target.as_object().as_array_ptr(), index, stored) };
        Ok(stored)
    }

    pub(super) fn op_set_index(&mut self) -> Result<(), anyhow::Error> {
        let target = self.stack.peek(0);
        if let Some(index) = array_element_index(target, self.stack.peek(2)) {
            let stored = self.store_array_element(target, index, self.stack.peek(1))?;
            self.stack.truncate(2);
            self.stack.set(0, stored);
            return Ok(());
        }

        self.stack.bury_top(2);
        self.set_index_on_object(true)
    }

    #[inline]
    pub(super) fn set_index(&mut self, requires_var: bool) -> Result<(), anyhow::Error> {
        let (target, prop) = self.store_operands();

        if let Some(index) = array_element_index(target, prop) {
            let stored = self.store_array_element(target, index, self.stack.peek(0))?;
            self.stack.set(0, stored);
            self.drop_store_path();
            return Ok(());
        }
        self.set_index_on_object(requires_var)
    }

    pub(super) fn set_index_on_object(&mut self, requires_var: bool) -> Result<(), anyhow::Error> {
        let (target, prop) = self.store_operands();
        let ValueKind::Object(object_kind) = target.kind() else {
            return self.error(format!("Invalid property access: {}", target.fmt()));
        };
        self.drop_store_path();

        let mark = self.hold(&[target, prop]);
        let outcome = match object_kind {
            ObjectKind::Instance => self.set_instance_index(prop, target, requires_var),
            ObjectKind::Array => self.set_native_type_index(self.native_types.array, target, prop),
            ObjectKind::Dict => self.set_dict_index(target, prop),
            _ => self.error(format!("Invalid property access: {}", target.fmt())),
        };
        self.release_held(mark);
        outcome
    }

    pub(super) fn op_get_index_or_null(&mut self) -> Result<(), anyhow::Error> {
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
                self.get_instance_property(instance, key.as_object().as_string_ptr())?.unwrap_or(Value::NULL)
            },
            _ => Value::NULL,
        };
        self.stack.set(0, value);
        Ok(())
    }

    pub(super) fn op_get_property(&mut self) -> Result<(), anyhow::Error> {
        // Both operands stay on the stack: resolving a member can allocate a bound method
        // and trigger gc, and a receiver held only in a local is not a gc root.
        let prop = self.stack.peek(0);
        let target = self.stack.peek(1);
        let operands_start = self.stack.len() - 2;
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
            self.stack.truncate(self.stack.len() - operands_start);
            return outcome;
        }
        let result = self.stack.pop();
        self.stack.truncate(self.stack.len() - operands_start);
        self.stack.push(result);
        Ok(())
    }

    #[inline]
    pub(super) fn op_load_step_for_write(&mut self) -> Result<(), anyhow::Error> {
        let is_dot = self.read_next() != 0;
        self.step_for_write(is_dot)
    }

    /// Replaces the container and key on top with what the key holds, and forks it if it's shared.
    #[inline]
    pub(super) fn step_for_write(&mut self, is_dot: bool) -> Result<(), anyhow::Error> {
        let container = self.stack.peek(1);
        objects::step_through(container);
        let key = self.stack.peek(0);
        match is_dot {
            true => self.op_get_property()?,
            false => self.op_get_index()?,
        }
        match objects::is_shared(self.stack.peek(0)) {
            true => self.fork_into_container(container, key, is_dot),
            false => Ok(()),
        }
    }

    #[inline]
    fn ref_instance(&self, holder: Value) -> Option<*mut ObjInstance> {
        objects::is_ref(holder).then(|| holder.as_object().as_instance_ptr())
    }

    pub(super) fn op_load_ref(&mut self) -> Result<(), anyhow::Error> {
        let holder = self.stack.peek(0);
        let Some(instance) = self.ref_instance(holder) else { return self.refuse_not_ref(holder) };
        if ref_locked(instance) {
            return self.refuse_locked_ref("read");
        }
        self.stack.set(0, unsafe { ObjInstance::get(instance, objects::REF_VALUE_FIELD) });
        Ok(())
    }

    pub(super) fn op_load_ref_for_write(&mut self) -> Result<(), anyhow::Error> {
        let holder = self.stack.peek(0);
        let Some(instance) = self.ref_instance(holder) else { return self.refuse_not_ref(holder) };
        if ref_locked(instance) {
            return self.refuse_locked_ref("written");
        }
        let value = unsafe { ObjInstance::get(instance, objects::REF_VALUE_FIELD) };
        if !objects::is_shared(value) {
            self.stack.set(0, value);
            return Ok(());
        }
        self.fork_into_ref(instance, value)
    }

    pub(super) fn op_store_ref<const POP: bool>(&mut self) -> Result<(), anyhow::Error> {
        let holder = self.stack.peek(1);
        let Some(instance) = self.ref_instance(holder) else { return self.refuse_not_ref(holder) };
        if ref_locked(instance) {
            return self.refuse_locked_ref("written");
        }
        let stored = self.store_field(instance, objects::REF_VALUE_FIELD, self.stack.peek(0), false)?;
        self.stack.pop();
        match POP {
            true => { self.stack.pop(); },
            false => { self.stack.set(0, stored); },
        }
        Ok(())
    }

    #[cold]
    fn fork_into_ref(&mut self, instance: *mut ObjInstance, value: Value) -> Result<(), anyhow::Error> {
        let forked = self.fork(value);
        let mark = self.hold(&[forked]);
        let stored = self.store_field(instance, objects::REF_VALUE_FIELD, forked, false);
        self.release_held(mark);
        let stored = stored?;
        objects::clear_shared(stored);
        self.stack.set(0, stored);
        Ok(())
    }

    #[cold]
    fn refuse_locked_ref(&self, access: &str) -> Result<(), anyhow::Error> {
        self.error(format!("A `Ref` cannot be {} while its `mutate` runs", access))
    }

    #[cold]
    fn refuse_not_ref(&self, value: Value) -> Result<(), anyhow::Error> {
        self.error(format!("`@` needs a `Ref`, found {}", value.fmt()))
    }

    #[cold]
    fn fork_into_container(&mut self, container: Value, key: Value, is_dot: bool) -> Result<(), anyhow::Error> {
        let mark = self.hold(&[container, key]);
        let forked = self.fork(self.stack.peek(0));
        self.release_held(mark);
        self.stack.push(container);
        self.stack.push(key);
        self.stack.push(forked);
        match is_dot {
            // The fork is the VM putting back what the container already held, not a store the
            // program wrote, so the field does not have to be `var`.
            true => self.set_property(false)?,
            false => self.set_index(false)?,
        }
        // The store decides what the container ends up holding, so the walk continues into that
        // rather than into the fork it passed on.
        let stored = self.stack.pop();
        objects::clear_shared(stored);
        self.stack.set(0, stored);
        Ok(())
    }

    /// `target.name = v`
    pub(super) fn op_set_property(&mut self) -> Result<(), anyhow::Error> {
        self.stack.bury_top(2);
        self.set_property(true)
    }

    #[inline]
    pub(super) fn set_property(&mut self, requires_var: bool) -> Result<(), anyhow::Error> {
        let (target, prop) = self.store_operands();
        let ValueKind::Object(object_kind) = target.kind() else {
            return self.error(format!("Invalid property access: {}", target.fmt()));
        };
        self.drop_store_path();

        // The store can fork a value off an anchor path, which allocates, so the target stays rooted across it.
        let mark = self.hold(&[target]);
        let outcome = match object_kind {
            ObjectKind::Instance => self.set_instance_index(prop, target, requires_var),
            ObjectKind::Array => self.set_native_type_index(self.native_types.array, target, prop),
            ObjectKind::Dict => self.error(format!(
                "Cannot assign to dict method '{}'; dict data is assigned with []",
                prop.as_object().as_string()
            )),
            _ => self.error(format!("Invalid property access: {}", target.fmt())),
        };
        self.release_held(mark);
        outcome
    }

    /// Whether a member satisfies what its declaration admits.
    pub(super) fn op_member_admits(&mut self) {
        let const_idx = self.read_next() as usize;
        let witness_set_pool_id = self.read_witness_set_pool_id();
        let key = self.chunk.constants[const_idx];
        let receiver = self.stack.pop();

        let admits = match self.member_value(receiver, key) {
            MemberValue::Method => true,
            MemberValue::Absent => false,
            MemberValue::Value(value) => self.accepts_value(value, witness_set_pool_id),
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
                    Some(TypeMember::Field(id)) => MemberValue::Value(unsafe { ObjInstance::get(instance_ptr, id) }),
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

    pub(super) fn op_is_dict(&mut self) {
        let receiver = self.stack.pop();
        self.stack.push(Value::from(matches!(receiver.kind(), ValueKind::Object(ObjectKind::Dict))));
    }

    /// Reads one element of a value being matched, by an offset from the front or from the back.
    pub(super) fn op_array_elem(&mut self) {
        let offset = self.read_next() as usize;
        let from_back = self.read_next() != 0;
        let receiver = self.stack.peek(0);
        let value = match receiver.kind() {
            ValueKind::Object(ObjectKind::Array) => {
                let values = unsafe { ObjArray::elements(receiver.as_object().as_array_ptr()) };
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
            ValueKind::Object(ObjectKind::Array) => unsafe { (*receiver.as_object().as_array_ptr()).len as f64 },
            _ => -1.0,
        };
        self.stack.push(Value::from(len));
    }

    fn get_dict_method(&mut self, target: Value, prop: Value) -> Result<(), anyhow::Error> {
        let dict_type = unsafe { &*self.native_types.dict };
        if matches!(prop.kind(), ValueKind::Object(ObjectKind::String)) {
            if let Some(method) = dict_type.resolve_method(prop.as_object().as_string_ptr()) {
                let bound = self.bind_method(target, method)?;
                self.stack.push(bound);
                return Ok(());
            }
            return self.error(format!("dict has no method '{}'", prop.as_object().as_string()));
        }
        self.error(format!("Invalid dict property: {}", prop.fmt()))
    }

    fn get_dict_index(&mut self, target: Value, prop: Value) -> Result<(), anyhow::Error> {
        debug_assert!(!prop.is_anchor(), "a dict key cannot be an anchor");
        let dict = unsafe { &*target.as_object().as_dict_ptr() };
        let Some(value) = dict.entries.get(&DictKey(prop)).copied() else {
            return self.error(format!("Dict key not found: {}", prop.fmt()));
        };
        self.stack.push(value);
        Ok(())
    }

    fn set_dict_index(&mut self, target: Value, prop: Value) -> Result<(), anyhow::Error> {
        debug_assert!(!prop.is_anchor() && !self.stack.peek(0).is_anchor(), "a dict key cannot be an anchor");
        let value = self.share_into_container(target, self.stack.peek(0))?;
        self.stack.set(0, value);

        let key = match objects::is_container(prop) {
            true => self.share_into_container(target, prop)?,
            false => prop,
        };

        self.assert_key_is_acyclic(key);
        let dict = unsafe { &mut *target.as_object().as_dict_ptr() };
        if let Some(replaced) = dict.entries.insert(DictKey(key), value) {
            objects::detach_replaced(&dict.header, || replaced);
        }
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
        let value = self.get_property_by_id(instance_ref, member_id)?;
        self.stack.push(value);
        Ok(())
    }
}

#[cfg(debug_assertions)]
fn assert_field_slot(instance: &ObjInstance, member_id: u8) {
    let ty = unsafe { &*instance.ty };
    debug_assert!(member_id < ty.field_count, "write to method slot {member_id} of {}", unsafe { &*ty.name }.value);
}

fn anchor_receiver_method_name(method: Object) -> Option<*mut ObjString> {
    let (takes_anchor, name) = match method.tag() {
        objects::TAG_FUNCTION => {
            let func = unsafe { &*method.as_function_ptr() };
            (func.param_list_pool_id & ir::WANTS_ANCHOR_RECEIVER != 0, func.name)
        },
        objects::TAG_CLOSURE => {
            let closure = unsafe { &*method.as_closure_ptr() };
            (closure.param_list_pool_id & ir::WANTS_ANCHOR_RECEIVER != 0, closure.name)
        },
        objects::TAG_NATIVE_FUNCTION => {
            let native = unsafe { &*method.as_native_function_ptr() };
            (native.wants_anchor_receiver, native.name)
        },
        _ => return None,
    };
    takes_anchor.then_some(name)
}

fn member_name(ty: &ObjType, id: u8) -> Option<String> {
    ty.members.iter().find(|(_, member)| member.id() == id).map(|(name, _)| unsafe { &**name }.value.clone())
}

fn field_label(instance_ref: *mut ObjInstance, field: u8) -> String {
    let ty = unsafe { &*(*instance_ref).ty };
    format!("{}.{}", unsafe { &*ty.name }.value, member_name(ty, field).unwrap_or_default())
}

#[inline]
pub(super) fn ref_locked(instance: *mut ObjInstance) -> bool {
    !unsafe { ObjInstance::get(instance, objects::REF_LOCK_FIELD) }.is_falsy()
}

use super::*;

impl Vm {
    #[inline]
    fn resolve_cached_class_property(&mut self, class_ptr: *mut ObjType, prop: *mut ObjString) -> Option<ClassMember> {
        let site = self.ip as usize;
        let slot = (site >> 4) & (INDEX_CACHE_SIZE - 1);
        let entry = unsafe { self.index_cache.get_unchecked_mut(slot) };
        if entry.site == site && entry.class == class_ptr {
            return Some(entry.member);
        }
        let member = unsafe { &*class_ptr }.resolve(prop)?;
        *entry = IndexCache { site, class: class_ptr, member };
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

    /// Fused method call (`INVOKE`). Fast-paths an instance method — pushing the
    /// frame with the receiver as slot 0 and no bound-method allocation — and
    /// falls back to a faithful "get the member, then call it" for every other
    /// case (fields holding callables, getters, native receivers, errors).
    pub(super) fn op_invoke(&mut self) -> Result<(), anyhow::Error> {
        let name_idx = self.read_next() as usize;
        let arg_count = self.read_next() as usize;
        let name = self.chunk.constants[name_idx].as_object().as_string_ptr();
        let receiver = self.stack.peek(arg_count);

        if matches!(receiver.kind(), ValueKind::Object(ObjectKind::Instance)) {
            let class_ptr = unsafe { (*receiver.as_object().as_instance_ptr()).class };
            if let Some(ClassMember::Method(id)) = self.resolve_cached_class_property(class_ptr, name) {
                // External `obj.method()` cannot reach a private/`inner` method.
                if unsafe { &*class_ptr }.non_public.contains(&id) {
                    return self.error(format!("Member '{}' is private", unsafe { &*name }.value));
                }
                let method = unsafe { &*class_ptr }.get_method(id);
                if method.tag() == objects::TAG_FUNCTION {
                    return self.invoke_method(method, arg_count);
                }
            }
        }

        self.invoke_member_slow(name, arg_count)
    }

    /// Pushes a frame for an instance method without allocating a bound method.
    /// The receiver already sits at the base of the call window, so it becomes
    /// slot 0 (`this`).
    fn invoke_method(&mut self, method: Object, arg_count: usize) -> Result<(), anyhow::Error> {
        let func_ptr = method.as_function_ptr();
        let func = unsafe { &*func_ptr };
        if arg_count != func.arity as usize {
            let name = unsafe { &(*func.name).value };
            return self.error(format!("{} expects {} arguments, but was called with {}", name, func.arity, arg_count));
        }
        let ip_start = func.ip_start;
        let closure = self.create_closure(func_ptr);
        self.push_frame(closure.as_closure_ptr(), self.stack.offset(arg_count), ip_start)
    }

    /// Fallback for non-fast-path receivers/members: fetch `receiver.name` exactly
    /// as `GET_INDEX` would (handling fields, getters, native types, and errors),
    /// then call the resulting value with the original arguments.
    ///
    /// When the member resolves through a getter, fetching it pushes a frame and
    /// the value isn't available until that frame returns, so the call is deferred
    /// (see [`Vm::complete_pending_invokes`]); otherwise it completes immediately.
    fn invoke_member_slow(&mut self, name: *mut ObjString, arg_count: usize) -> Result<(), anyhow::Error> {
        let mut args: SmallVec<[Value; 4]> = SmallVec::with_capacity(arg_count);
        for i in (0..arg_count).rev() {
            args.push(self.stack.peek(i));
        }
        self.stack.truncate(arg_count);     // leaves [receiver]
        self.stack.push(Value::from(name)); // [receiver, name]

        let depth = self.frames.len();
        // INVOKE is always a `recv.name(args)` (`.`-call), so resolve via property
        // access. This routes a `dict` to its method surface, not its keyed data.
        self.op_get_property()?;

        if self.frames.len() == depth {
            // Synchronous (field value / native): the member value is on the stack.
            self.finish_invoke(args)
        } else {
            // A getter frame was pushed; complete the call when it returns.
            self.pending_invokes.push(PendingInvoke { args, depth });
            Ok(())
        }
    }

    /// Calls the just-resolved member value (on the stack top) with `args`.
    fn finish_invoke(&mut self, args: SmallVec<[Value; 4]>) -> Result<(), anyhow::Error> {
        let arg_count = args.len();
        let callable = self.stack.peek(0);
        for arg in args {
            self.stack.push(arg);
        }
        self.call(arg_count, callable)
    }

    /// Fires any deferred invoke whose getter frame has now returned (its value is
    /// on the stack top). Called after a `RETURN` when invokes are pending.
    pub(super) fn complete_pending_invokes(&mut self) -> Result<(), anyhow::Error> {
        while self.pending_invokes.last().is_some_and(|p| p.depth == self.frames.len()) {
            let pending = self.pending_invokes.pop().unwrap();
            self.finish_invoke(pending.args)?;
        }
        Ok(())
    }

    fn invoke_accessor(&mut self, accessor: Object, arg_count: usize) -> Result<(), anyhow::Error> {
        match accessor.tag() {
            objects::TAG_FUNCTION => {
                let func_ptr = accessor.as_function_ptr();
                let closure = self.create_closure(func_ptr);
                let ip_start = unsafe { (*func_ptr).ip_start };
                self.push_frame(closure.as_closure_ptr(), self.stack.offset(arg_count), ip_start)
            },
            objects::TAG_NATIVE_FUNCTION => self.call_native(arg_count, accessor.as_native_function_ptr()),
            _ => unsafe { std::hint::unreachable_unchecked() }
        }
    }

    fn get_instance_property(&mut self, instance_ptr: *mut ObjInstance, prop: *mut ObjString) -> Option<Value> {
        let instance = unsafe { &*instance_ptr };
        let class = unsafe { &*instance.class };

        match self.resolve_cached_class_property(instance.class, prop) {
            Some(ClassMember::Field(id)) => Some(unsafe { (*instance_ptr).get(id) }),
            Some(ClassMember::Method(id)) => {
                let method = class.get_method(id);
                Some(self.bind_method(instance_ptr.into(), method))
            },
            None => None
        }
    }

    fn get_property_by_id(&mut self, instance_ref: *mut ObjInstance, id: u8) -> Value {
        let value = unsafe { (*instance_ref).get(id) };
        match value.kind() {
            ValueKind::Object(ObjectKind::Function) => self.bind_method(instance_ref.into(), value.as_object()),
            _ => value
        }
    }

    fn get_native_type_index(&mut self, native_class_ptr: *mut ObjType, target: Value, prop: Value) -> Result<(), anyhow::Error> {
        let native_class = unsafe { &*native_class_ptr };

        if matches!(prop.kind(), ValueKind::Object(ObjectKind::String)) {
            let Some(method) = native_class.resolve_method(prop.as_object().as_string_ptr()) else {
                return match native_class_ptr {
                    _ if native_class_ptr == self.native_types.array => self.error(format!("Invalid array index: {}", prop.fmt())),
                    _ => self.error(format!("Invalid index: {} does not have method {}", target.fmt(), prop.fmt())),
                }
            };

            let bound_method = self.alloc(ObjBoundMethod::new(target, method));
            self.stack.push(Value::from(bound_method));
            return Ok(());
        }

        let getter = native_class.getter().unwrap();
        self.stack.push(target);
        self.stack.push(prop);
        return self.call_native(1, getter.as_native_function_ptr());
    }

    fn set_native_type_index(&mut self, native_class_ptr: *mut ObjType, target: Value, prop: Value) -> Result<(), anyhow::Error> {
        let native_class = unsafe { &*native_class_ptr };
        let setter = native_class.setter().unwrap();
        let value = self.stack.pop();
        self.stack.push(target);
        self.stack.push(prop);
        self.stack.push(value);
        return self.call_native(2, setter.as_native_function_ptr());
    }

    fn get_instance_index(&mut self, target: Value, prop: Value) -> Result<(), anyhow::Error> {
        let instance_ref = target.as_object().as_instance_ptr();

        if matches!(prop.kind(), ValueKind::Object(ObjectKind::String)) {
            let prop_str = prop.as_object().as_string_ptr();
            // External `obj.member` access: a private/`inner` member is not reachable.
            // (`this.x` never reaches here because it resolves to a member id internally, and
            // per-trait private/qualified slots aren't in the runtime name map at all.)
            if let Some(member) = self.resolve_cached_class_property(unsafe { (*instance_ref).class }, prop_str) {
                let id = match member { ClassMember::Field(id) | ClassMember::Method(id) => id };
                if unsafe { &*(*instance_ref).class }.non_public.contains(&id) {
                    return self.error(format!("Member '{}' is private", prop.as_object().as_string()));
                }
            }
            if let Some(value) = self.get_instance_property(instance_ref, prop_str) {
                self.stack.push(value);
                return Ok(());
            }
        }

        let class = unsafe { &*(*instance_ref).class };

        if let Some(getter) = class.getter() {
            self.stack.push(target);
            self.stack.push(prop);
            return self.invoke_accessor(getter, 1);
        }

        self.error(format!(
            "Invalid index: {} doesn't have member {} and doesn't have a getter",
            unsafe { &*class.name }.value,
            prop.fmt()
        ))
    }

    fn set_instance_index(&mut self, prop: Value, target: Value) -> Result<(), anyhow::Error> {
        let instance_ref = target.as_object().as_instance_ptr();
        let instance = unsafe { &mut *instance_ref };
        let class = unsafe { &*instance.class };

        if matches!(prop.kind(), ValueKind::Object(ObjectKind::String)) {
            let member = class.resolve(prop.as_object().as_string_ptr());

            if let Some(member) = member {
                if let ClassMember::Field(id) = member {
                    if class.non_public.contains(&id) {
                        return self.error(format!("Member '{}' is private", prop.as_object().as_string()));
                    }
                    let value = self.stack.peek(0);
                    instance.set(id, value);
                    return Ok(());
                }

                return self.error(format!("Cannot assign to method '{}'", prop.as_object().as_string()));
            }
        }

        if let Some(setter) = class.setter() {
            let value = self.stack.pop();
            self.stack.push(target);
            self.stack.push(prop);
            self.stack.push(value);
            return self.invoke_accessor(setter, 2);
        }

        self.error(format!(
            "Invalid index: {} doesn't have member {} and doesn't have a setter",
            unsafe { &*class.name }.value,
            prop.fmt()
        ))
    }

    pub(super) fn op_set_property_by_id_pop(&mut self) -> Result<(), anyhow::Error> {
        let member_id = self.read_next();
        let target = self.stack.pop();
        if !matches!(target.kind(), ValueKind::Object(ObjectKind::Instance)) {
            return self.error(format!("Invalid property access: {}", target.fmt()));
        }

        let value = self.stack.pop();
        let instance = unsafe { &mut *target.as_object().as_instance_ptr() };
        instance.set(member_id, value);
        Ok(())
    }

    pub(super) fn op_get_index(&mut self) -> Result<(), anyhow::Error> {
        let prop = self.stack.pop();
        let target = self.stack.pop();
        let ValueKind::Object(object_kind) = target.kind() else {
            return self.error(format!("Invalid property access: {}", target.fmt()));
        };

        match object_kind {
            ObjectKind::Instance => self.get_instance_index(target, prop),
            ObjectKind::Array => self.get_native_type_index(self.native_types.array, target, prop),
            ObjectKind::Dict => self.get_dict_index(target, prop),
            _ => self.error(format!("Invalid property access: {}", target.fmt()))
        }
    }

    pub(super) fn op_set_index(&mut self) -> Result<(), anyhow::Error> {
        let prop = self.stack.pop();
        let target = self.stack.pop();
        let ValueKind::Object(object_kind) = target.kind() else {
            return self.error(format!("Invalid property access: {}", target.fmt()));
        };

        match object_kind {
            ObjectKind::Instance => self.set_instance_index(prop, target),
            ObjectKind::Array => self.set_native_type_index(self.native_types.array, target, prop),
            ObjectKind::Dict => self.set_dict_index(target, prop),
            _ => self.error(format!("Invalid property access: {}", target.fmt()))
        }
    }

    /// Dotted access `target.name`: the value's declared interface. Same stack
    /// protocol as `op_get_index`, but a `dict` resolves to its **method** surface
    /// (`.`) rather than its keyed data (`[]`), so `d.has` and `d["has"]` never collide.
    pub(super) fn op_get_property(&mut self) -> Result<(), anyhow::Error> {
        let prop = self.stack.pop();
        let target = self.stack.pop();
        let ValueKind::Object(object_kind) = target.kind() else {
            return self.error(format!("Invalid property access: {}", target.fmt()));
        };

        match object_kind {
            ObjectKind::Instance => self.get_instance_index(target, prop),
            ObjectKind::Array => self.get_native_type_index(self.native_types.array, target, prop),
            ObjectKind::Dict => self.get_dict_method(target, prop),
            _ => self.error(format!("Invalid property access: {}", target.fmt()))
        }
    }

    /// Dotted store `target.name = v`. Instances assign the named field; a `dict`
    /// method is not assignable (use `[]` for data).
    pub(super) fn op_set_property(&mut self) -> Result<(), anyhow::Error> {
        let prop = self.stack.pop();
        let target = self.stack.pop();
        let ValueKind::Object(object_kind) = target.kind() else {
            return self.error(format!("Invalid property access: {}", target.fmt()));
        };

        match object_kind {
            ObjectKind::Instance => self.set_instance_index(prop, target),
            ObjectKind::Array => self.set_native_type_index(self.native_types.array, target, prop),
            ObjectKind::Dict => self.error(format!(
                "Cannot assign to dict method '{}'; dict data is assigned with []",
                prop.as_object().as_string()
            )),
            _ => self.error(format!("Invalid property access: {}", target.fmt()))
        }
    }

    /// Resolves `dict.name` to a bound method of the `dict` method surface.
    fn get_dict_method(&mut self, target: Value, prop: Value) -> Result<(), anyhow::Error> {
        let dict_class = unsafe { &*self.native_types.dict };
        if matches!(prop.kind(), ValueKind::Object(ObjectKind::String)) {
            if let Some(method) = dict_class.resolve_method(prop.as_object().as_string_ptr()) {
                let bound = self.alloc(ObjBoundMethod::new(target, method));
                self.stack.push(Value::from(bound));
                return Ok(());
            }
            return self.error(format!("dict has no method '{}'", prop.as_object().as_string()));
        }
        self.error(format!("Invalid dict property: {}", prop.fmt()))
    }

    /// Reads `dict[key]` by value key. A missing key yields `null` — dict reads are
    /// the dynamic boundary; absence is simply unset (no error).
    fn get_dict_index(&mut self, target: Value, prop: Value) -> Result<(), anyhow::Error> {
        let dict = unsafe { &*target.as_object().as_dict_ptr() };
        let value = dict.entries.get(&prop).copied().unwrap_or(Value::NULL);
        self.stack.push(value);
        Ok(())
    }

    /// Writes `dict[key] = value`. The rhs is on the stack top; it stays there as
    /// the assignment expression's result (mirrors the array/native setter path).
    fn set_dict_index(&mut self, target: Value, prop: Value) -> Result<(), anyhow::Error> {
        let value = self.stack.peek(0);
        let dict = unsafe { &mut *target.as_object().as_dict_ptr() };
        dict.entries.insert(prop, value);
        Ok(())
    }

    pub(super) fn op_get_property_by_id(&mut self) -> Result<(), anyhow::Error> {
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

    pub(super) fn op_set_property_by_id(&mut self) -> Result<(), anyhow::Error> {
        let member_id = self.read_next();
        let value = self.stack.pop();
        if !matches!(value.kind(), ValueKind::Object(ObjectKind::Instance)) {
            return self.error(format!("Invalid property access: {}", value.fmt()));
        }

        let object = value.as_object();
        let instance_ref = object.as_instance_ptr();
        let value = self.stack.peek(0);
        let instance = unsafe { &mut *instance_ref };
        instance.set(member_id, value);
        Ok(())
    }
}

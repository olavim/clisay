use super::*;

impl Vm {
    #[inline]
    fn resolve_cached_type_property(&mut self, type_ptr: *mut ObjType, prop: *mut ObjString) -> Option<TypeMember> {
        let site = self.ip as usize;
        let slot = (site >> 4) & (INDEX_CACHE_SIZE - 1);
        let entry = unsafe { self.index_cache.get_unchecked_mut(slot) };
        if entry.site == site && entry.ty == type_ptr {
            return Some(entry.member);
        }
        let member = unsafe { &*type_ptr }.resolve(prop)?;
        *entry = IndexCache { site, ty: type_ptr, member };
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
        let name = self.chunk.constants[name_idx].as_object().as_string_ptr();
        let receiver = self.stack.peek(arg_count);

        if matches!(receiver.kind(), ValueKind::Object(ObjectKind::Instance)) {
            let type_ptr = unsafe { (*receiver.as_object().as_instance_ptr()).ty };
            if let Some(TypeMember::Method(id)) = self.resolve_cached_type_property(type_ptr, name) {
                let method = unsafe { &*type_ptr }.get_method(id);
                if method.tag() == objects::TAG_FUNCTION {
                    return self.invoke_method(method, arg_count);
                }
            }
        }

        self.invoke_member_slow(name, arg_count)
    }

    /// Pushes a frame for an instance method without allocating a bound method.
    fn invoke_method(&mut self, method: Object, arg_count: usize) -> Result<(), anyhow::Error> {
        let func_ptr = method.as_function_ptr();
        let func = unsafe { &*func_ptr };
        if arg_count != func.arity as usize {
            let name = unsafe { &(*func.name).value };
            return self.error(format!("{} expects {} arguments, but was called with {}", name, func.arity, arg_count));
        }
        let ip_start = func.ip_start;
        let closure = self.create_closure(func_ptr);
        self.push_frame(closure.as_closure_ptr(), self.stack.offset(arg_count), ip_start, true)
    }

    fn invoke_member_slow(&mut self, name: *mut ObjString, arg_count: usize) -> Result<(), anyhow::Error> {
        let mut args: SmallVec<[Value; 4]> = SmallVec::with_capacity(arg_count);
        for i in (0..arg_count).rev() {
            args.push(self.stack.peek(i));
        }

        self.stack.truncate(arg_count);
        self.stack.push(Value::from(name));

        // INVOKE is always a `recv.name(args)`.
        self.op_get_property()?;

        let callable = self.stack.peek(0);
        for arg in args {
            self.stack.push(arg);
        }
        self.call(arg_count, callable, true)
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
        let value = unsafe { (*instance_ref).get(id) };
        match value.kind() {
            ValueKind::Object(ObjectKind::Function) => self.bind_method(instance_ref.into(), value.as_object()),
            _ => value
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
            Some(TypeMember::Field(id)) => {
                let value = self.stack.peek(0);
                instance.set(id, value);
                Ok(())
            },
            Some(TypeMember::Method(_)) => self.error(format!("Cannot assign to method '{}'", prop.as_object().as_string())),
            None => self.error(format!(
                "Invalid index: {} doesn't have member {}",
                unsafe { &*ty.name }.value,
                prop.fmt()
            )),
        }
    }

    pub(super) fn op_set_field_pop(&mut self) -> Result<(), anyhow::Error> {
        let member_id = self.read_next();
        let target = self.stack.pop();
        if !matches!(target.kind(), ValueKind::Object(ObjectKind::Instance)) {
            return self.error(format!("Invalid property access: {}", target.fmt()));
        }
        self.ensure_mutable(target)?;

        let value = self.stack.pop();
        self.ensure_not_borrowed(value)?;
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

    fn ensure_mutable(&self, target: Value) -> Result<(), anyhow::Error> {
        if matches!(target.kind(), ValueKind::Object(_)) && target.as_object().is_immutable() {
            return self.error_immutable(target);
        }
        Ok(())
    }

    /// Traps a store of a borrowed value: it may not be persisted while it is borrowed.
    pub(super) fn ensure_not_borrowed(&self, value: Value) -> Result<(), anyhow::Error> {
        if value.is_borrowed() {
            let label = format!("`{}` is borrowed here", self.get_source_position().snippet());
            return self.error_labeled(objects::PERSISTED_BORROW, label);
        }
        Ok(())
    }

    pub(super) fn op_set_index(&mut self) -> Result<(), anyhow::Error> {
        let prop = self.stack.pop();
        let target = self.stack.pop();
        let ValueKind::Object(object_kind) = target.kind() else {
            return self.error(format!("Invalid property access: {}", target.fmt()));
        };
        self.ensure_mutable(target)?;
        self.ensure_not_borrowed(self.stack.peek(0))?;

        match object_kind {
            ObjectKind::Instance => self.set_instance_index(prop, target),
            ObjectKind::Array => self.set_native_type_index(self.native_types.array, target, prop),
            ObjectKind::Dict => self.set_dict_index(target, prop),
            _ => self.error(format!("Invalid property access: {}", target.fmt()))
        }
    }

    pub(super) fn op_get_index_or_null(&mut self) {
        let const_idx = self.read_next() as usize;
        let key = self.chunk.constants[const_idx];
        let receiver = self.stack.pop();
        let value = match receiver.kind() {
            ValueKind::Object(ObjectKind::Dict) => {
                unsafe { &*receiver.as_object().as_dict_ptr() }.entries.get(&key).copied().unwrap_or(Value::NULL)
            },
            ValueKind::Object(ObjectKind::Instance) if matches!(key.kind(), ValueKind::Object(ObjectKind::String)) => {
                let instance = receiver.as_object().as_instance_ptr();
                self.get_instance_property(instance, key.as_object().as_string_ptr()).unwrap_or(Value::NULL)
            },
            _ => Value::NULL,
        };
        self.stack.push(value);
    }

    /// Dotted access `target.name`.
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

    /// Dotted store `target.name = v`.
    pub(super) fn op_set_property(&mut self) -> Result<(), anyhow::Error> {
        let prop = self.stack.pop();
        let target = self.stack.pop();
        let ValueKind::Object(object_kind) = target.kind() else {
            return self.error(format!("Invalid property access: {}", target.fmt()));
        };
        self.ensure_mutable(target)?;
        self.ensure_not_borrowed(self.stack.peek(0))?;

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

    pub(super) fn op_has_member(&mut self) {
        let const_idx = self.read_next() as usize;
        let key = self.chunk.constants[const_idx];
        let receiver = self.stack.pop();
        let present = match receiver.kind() {
            ValueKind::Object(ObjectKind::Dict) => {
                unsafe { &*receiver.as_object().as_dict_ptr() }.entries.contains_key(&key)
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

    pub(super) fn op_array_len(&mut self) {
        let receiver = self.stack.pop();
        let len = match receiver.kind() {
            ValueKind::Object(ObjectKind::Array) => unsafe { &*receiver.as_object().as_array_ptr() }.values.len() as f64,
            _ => -1.0,
        };
        self.stack.push(Value::from(len));
    }

    /// Resolves `dict.name` to a bound method of the `dict` method surface.
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

    /// Reads `dict[key]` by value key. A missing key yields `null`.
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

    pub(super) fn op_set_field(&mut self) -> Result<(), anyhow::Error> {
        let member_id = self.read_next();
        let value = self.stack.pop();
        if !matches!(value.kind(), ValueKind::Object(ObjectKind::Instance)) {
            return self.error(format!("Invalid property access: {}", value.fmt()));
        }
        self.ensure_mutable(value)?;

        let object = value.as_object();
        let instance_ref = object.as_instance_ptr();
        let value = self.stack.peek(0);
        self.ensure_not_borrowed(value)?;
        let instance = unsafe { &mut *instance_ref };
        instance.set(member_id, value);
        Ok(())
    }
}

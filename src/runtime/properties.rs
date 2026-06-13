use super::*;

impl Vm {
    #[inline]
    fn resolve_cached_class_property(&mut self, class_ptr: *mut ObjClass, prop: *mut ObjString) -> Option<ClassMember> {
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

    fn get_native_type_index(&mut self, native_class_ptr: *mut ObjClass, target: Value, prop: Value) -> Result<(), anyhow::Error> {
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

    fn set_native_type_index(&mut self, native_class_ptr: *mut ObjClass, target: Value, prop: Value) -> Result<(), anyhow::Error> {
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
            if let Some(value) = self.get_instance_property(instance_ref, prop.as_object().as_string_ptr()) {
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
            _ => self.error(format!("Invalid property access: {}", target.fmt()))
        }
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

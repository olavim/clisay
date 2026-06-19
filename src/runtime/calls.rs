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

    pub(super) fn push_frame(&mut self, closure: *mut ObjClosure, stack_start: *mut Value, ip_start: usize) -> Result<(), anyhow::Error> {
        if self.frames.is_full() {
            return Err(self.stack_overflow());
        }
        self.frames.push(CallFrame {
            closure,
            return_ip: self.ip,
            stack_start
        });
        self.ip = unsafe { self.chunk.code.as_ptr().offset(ip_start as isize) };
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
            objects::TAG_CLASS => self.call_class(arg_count, object.as_class_ptr()),
            _ => unsafe { std::hint::unreachable_unchecked() }
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

    fn call_closure(&mut self, arg_count: usize, closure_ptr: *mut ObjClosure) -> Result<(), anyhow::Error> {
        let closure = unsafe { &*closure_ptr };
        check_arity!(self, arg_count, closure.arity, closure.name);
        self.push_frame(closure_ptr, self.stack.offset(arg_count), closure.ip_start)
    }

    fn call_bound_method(&mut self, arg_count: usize, bound_method_ptr: *mut ObjBoundMethod) -> Result<(), anyhow::Error> {
        let bound_method = unsafe { &*bound_method_ptr };
        let method = bound_method.method;
        match method.tag() {
            objects::TAG_CLOSURE => {
                let closure_ptr = method.as_closure_ptr();
                let closure = unsafe { &*closure_ptr };
                check_arity!(self, arg_count, closure.arity, closure.name);
                let stack_start = self.stack.set(arg_count, Value::from(bound_method.target));
                self.push_frame(closure_ptr, stack_start, closure.ip_start)?;
            },
            objects::TAG_NATIVE_FUNCTION => {
                self.stack.set(arg_count, Value::from(bound_method.target));
                self.call_native(arg_count, method.as_native_function_ptr())?;
            },
            _ => unsafe { std::hint::unreachable_unchecked() }
        };
        Ok(())
    }

    /// Brace construction `C(args) { f: v, ... }`. Reads the brace field ids then the init arg
    /// count, allocates the instance, sets the brace fields from the stack, then runs `init`.
    /// The stack holds `[C, args.., brace values..]` in source order.
    pub(super) fn op_construct(&mut self) -> Result<(), anyhow::Error> {
        let field_count = self.read_next() as usize;
        let mut field_ids = [0u8; u8::MAX as usize + 1];
        for slot in field_ids.iter_mut().take(field_count) {
            *slot = self.read_next();
        }
        let arg_count = self.read_next() as usize;

        let class_val = self.stack.peek(field_count + arg_count);
        if !class_val.is_object() || class_val.as_object().tag() != objects::TAG_CLASS {
            return self.error(format!("Cannot construct: {} is not a type", class_val.fmt()));
        }
        let class_ptr = class_val.as_object().as_class_ptr();
        let class = unsafe { &*class_ptr };
        let init_obj = class.initializer().unwrap();
        if init_obj.tag() != objects::TAG_FUNCTION {
            return self.error("Cannot brace-construct this type");
        }
        let init_ref = init_obj.as_function_ptr();
        let init = unsafe { &*init_ref };
        check_arity!(self, arg_count, init.arity, init.name);

        // Allocate, rooting the init closure across the allocation (it can trigger GC).
        let closure = self.create_closure(init_ref);
        self.stack.push(Value::from(closure));
        let instance_ptr = self.alloc(ObjInstance::new(class_ptr));
        self.stack.pop();

        // Brace values sit on top of the stack in field order; set them before init runs.
        let instance = unsafe { &mut *instance_ptr };
        for j in 0..field_count {
            let value = self.stack.peek(field_count - 1 - j);
            instance.set(field_ids[j], value);
        }
        // Drop the brace values, then run init on the instance (it returns `this`).
        self.stack.truncate(field_count);
        let stack_start = self.stack.set(arg_count, Value::from(instance_ptr));
        self.push_frame(closure.as_closure_ptr(), stack_start, init.ip_start)
    }

    fn call_class(&mut self, arg_count: usize, class_ptr: *mut ObjType) -> Result<(), anyhow::Error> {
        let class = unsafe { &*class_ptr };
        let init_method_obj = class.initializer().unwrap();
        match init_method_obj.tag() {
            objects::TAG_FUNCTION => {
                let init_method_ref = init_method_obj.as_function_ptr();
                let init_method = unsafe { &*init_method_ref };
                check_arity!(self, arg_count, init_method.arity, init_method.name);

                let closure = self.create_closure(init_method_ref);
                // Root the fresh closure on the value stack: it isn't reachable yet and
                // the instance allocation below can trigger GC.
                self.stack.push(Value::from(closure));
                let instance = self.alloc(ObjInstance::new(class_ptr));
                self.stack.pop();
                let stack_start = self.stack.set(arg_count, Value::from(instance));
                self.push_frame(closure.as_closure_ptr(), stack_start, init_method.ip_start)
            },
            objects::TAG_NATIVE_FUNCTION => {
                self.call_native(arg_count, init_method_obj.as_native_function_ptr())?;
                return Ok(());
            },
            _ => unsafe { std::hint::unreachable_unchecked() }
        }
    }
}

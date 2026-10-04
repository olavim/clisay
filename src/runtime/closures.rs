use crate::core::objects::CaptureLocation;
use crate::core::objects::FLAG_RETURNS_VALUE;
use super::*;

fn capturing_methods(template: *const ObjType) -> SmallVec<[(u8, *mut ObjFn); 8]> {
    unsafe { &*template }.methods.iter()
        .filter(|(_, method)| method.tag() == objects::TAG_FUNCTION)
        .map(|(&id, method)| (id, method.as_function_ptr()))
        .filter(|(_, function)| !unsafe { &**function }.capture_locations.is_empty())
        .collect()
}

impl Vm {
    /// What the running closure captured at slot `idx`.
    #[inline]
    pub(super) fn capture(&self, idx: usize) -> Value {
        unsafe { ObjClosure::capture_at((*self.frames.top()).closure, idx) }
    }

    pub(super) fn create_closure(&mut self, function: *mut ObjFn) -> Object {
        let closure = self.allocate_closure(function);
        let mark = self.hold(&[Value::from(closure)]);
        self.bind_captures(closure, function);
        self.release_held(mark);
        closure.into()
    }

    /// A closure with its captures still empty, which `bind_captures` then fills.
    fn allocate_closure(&mut self, function: *mut ObjFn) -> *mut ObjClosure {
        let fn_ref = unsafe { &*function };
        let count = fn_ref.capture_locations.len();
        let (name, arity, ip_start) = (fn_ref.name, fn_ref.arity, fn_ref.ip_start);
        let param_list_pool_id = fn_ref.param_list_pool_id;
        let slot_witness_set_pool_id = fn_ref.slot_witness_set_pool_id;
        let returns_value = fn_ref.header.has(FLAG_RETURNS_VALUE);

        self.maybe_collect();

        let closure = self.gc.alloc_closure(name, arity, ip_start, count, param_list_pool_id, slot_witness_set_pool_id);
        if returns_value {
            unsafe { (*closure).header.set(FLAG_RETURNS_VALUE) };
        }
        closure
    }

    /// Fills a closure's captures based on its function's capture locations.
    pub(super) fn bind_captures(&mut self, closure: *mut ObjClosure, function: *mut ObjFn) {
        let locations: SmallVec<[CaptureLocation; 8]> = unsafe { &*function }.capture_locations.iter().copied().collect();
        for (i, at) in locations.into_iter().enumerate() {
            let value = match at.is_local {
                true => unsafe { *self.slot_addr(at.location as usize) },
                false => self.capture(at.location as usize),
            };
            let value = self.share(value);
            unsafe { ObjClosure::set_capture(closure, i, value) };
        }
    }

    /// Builds a type from a template, with a closure for each capturing method. With `bind` the
    /// captures are filled now. Without it, `BIND_TYPE_CAPTURES` fills them at the declaration's line.
    pub(super) fn build_type(&mut self, bind: bool) -> Result<(), anyhow::Error> {
        let const_idx = self.read_next() as usize;
        let template = self.chunk.constants[const_idx].as_object().as_type_ptr();

        let capturing = capturing_methods(template);
        let mut ty = unsafe { &*template }.duplicate();

        if !self.stack.has_room(capturing.len() + 1) {
            return Err(self.stack_overflow());
        }

        // Capturing can collect, so each new closure is kept on the stack.
        for &(_, function) in &capturing {
            let closure = self.new_closure(function, bind);
            self.stack.push(Value::from(closure));
        }

        let depth = capturing.len();
        for (i, &(id, _)) in capturing.iter().enumerate() {
            ty.methods.insert(id, self.stack.peek(depth - 1 - i).as_object());
        }
        ty.build_template();

        let ty = self.gc.alloc(ty);
        self.stack.truncate(depth);
        self.stack.push(Value::from(ty));
        Ok(())
    }

    pub(super) fn op_bind_captures(&mut self) {
        let slot = self.read_next() as usize;
        let const_idx = self.read_next() as usize;
        let holder = unsafe { *self.slot_addr(slot) };

        debug_assert!(holder.is_object(), "BIND_CLOSURE_CAPTURES names a slot holding a declaration");

        let function = self.chunk.constants[const_idx].as_object().as_function_ptr();
        self.bind_captures(holder.as_object().as_closure_ptr(), function);
    }

    pub(super) fn op_bind_type_captures(&mut self) {
        let slot = self.read_next() as usize;
        let const_idx = self.read_next() as usize;
        let holder = unsafe { *self.slot_addr(slot) };

        debug_assert!(holder.is_object(), "BIND_TYPE_CAPTURES names a slot holding a type");

        let template = self.chunk.constants[const_idx].as_object().as_type_ptr();
        let bindable = capturing_methods(template);
        let ty = holder.as_object().as_type_ptr();
        for (id, function) in bindable {
            let method = unsafe { &*ty }.methods[&id];
            debug_assert!(method.tag() == objects::TAG_CLOSURE, "a capturing method is a closure");
            self.bind_captures(method.as_closure_ptr(), function);
        }
    }

    pub(super) fn build_closure(&mut self, bind: bool) -> Result<(), anyhow::Error> {
        let const_idx = self.read_next() as usize;
        let function = self.chunk.constants[const_idx].as_object().as_function_ptr();
        let closure = self.new_closure(function, bind);
        self.push_checked(Value::from(closure))
    }

    /// A closure over `function`. With `bind` its captures are filled now.
    fn new_closure(&mut self, function: *mut ObjFn, bind: bool) -> Object {
        match bind {
            true => self.create_closure(function),
            false => self.allocate_closure(function).into(),
        }
    }
}

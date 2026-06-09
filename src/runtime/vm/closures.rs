use super::*;

impl Vm {
    pub(super) fn get_upvalue(&self, idx: usize) -> *mut ObjUpvalue {
        unsafe {
            let closure = &*(*self.frames.top()).closure;
            closure.upvalue_at(idx)
        }
    }

    fn capture_upvalue(&mut self, location: *mut Value) -> *mut ObjUpvalue {
        match self.open_upvalues.iter().find(|&&upvalue| unsafe { (*upvalue).location } == location) {
            Some(&upvalue) => upvalue,
            None =>  {
                let upvalue = self.alloc(ObjUpvalue::new(location));
                self.open_upvalues.push(upvalue);
                upvalue
            }
        }
    }

    pub(super) fn close_upvalues(&mut self, after: *const Value) {
        for idx in (0..self.open_upvalues.len()).rev() {
            unsafe {
                let upvalue = *self.open_upvalues.get_unchecked(idx);
                if after <= (*upvalue).location {
                    (*upvalue).close();
                    self.open_upvalues.swap_remove(idx);
                }
            }
        }
    }

    pub(super) fn create_closure(&mut self, function: *mut ObjFn) -> Object {
        let fn_ref = unsafe { &*function };
        let upvalue_count = fn_ref.upvalues.len();

        // Gather the captured upvalues into a stack-resident scratch buffer first,
        // then allocate the exact-sized closure in one shot. Capturing must happen
        // before allocation since capturing can trigger GC.
        let mut upvalues: SmallVec<[*mut ObjUpvalue; 8]> = SmallVec::with_capacity(upvalue_count);
        for i in 0..upvalue_count {
            let fn_upval = &fn_ref.upvalues[i];
            let upvalue = if fn_upval.is_local {
                self.capture_upvalue(unsafe { (*self.frames.top()).stack_start.add(fn_upval.location as usize) })
            } else {
                self.get_upvalue(fn_upval.location as usize)
            };

            upvalues.push(upvalue);
        }

        let (name, arity, ip_start) = (fn_ref.name, fn_ref.arity, fn_ref.ip_start);
        if self.gc.should_collect() {
            self.start_gc();
        }
        self.gc.alloc_closure(name, arity, ip_start, &upvalues).into()
    }

    pub(super) fn op_close_upvalue(&mut self) {
        let location = self.read_next() as usize;
        let p = unsafe { (*self.frames.top()).stack_start.add(location) };
        self.close_upvalues(p);
        self.stack.truncate(1);
    }

    pub(super) fn op_push_closure(&mut self) -> Result<(), anyhow::Error> {
        let const_idx = self.read_next() as usize;
        let value = self.chunk.constants[const_idx];
        let fn_ref = value.as_object().as_function_ptr();
        let closure = self.create_closure(fn_ref);
        self.stack.push(Value::from(closure));
        Ok(())
    }
}

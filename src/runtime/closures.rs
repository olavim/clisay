use super::*;

impl Vm {
    pub(super) fn get_upvalue(&self, idx: usize) -> *mut ObjUpvalue {
        unsafe { ObjClosure::upvalue_at((*self.frames.top()).closure, idx) }
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
        debug_assert!(after <= self.stack.top() as *const Value, "closing upvalues above the live stack top");
        for idx in (0..self.open_upvalues.len()).rev() {
            unsafe {
                let upvalue = *self.open_upvalues.get_unchecked(idx);
                if after <= (*upvalue).location {
                    (*upvalue).close();
                    self.open_upvalues.swap_remove(idx);
                }
            }
        }
        debug_assert!(!self.open_upvalues.iter().any(|&u| after <= unsafe { (*u).location }),
            "an upvalue at or above the closed slot stayed open");
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

            // The closure can outlive the frame the capture came from, so a scope exit there must
            // not release what the captured value holds.
            crate::core::objects::record_escape(unsafe { *(*upvalue).location });
            upvalues.push(upvalue);
        }

        let (name, arity, ip_start) = (fn_ref.name, fn_ref.arity, fn_ref.ip_start);
        let (escape_mask, move_mask) = (fn_ref.escape_mask, fn_ref.move_mask);
        let mut_receiver = fn_ref.mut_receiver;
        if self.gc.should_collect() {
            self.start_gc();
        }
        self.gc.alloc_closure(name, arity, ip_start, &upvalues, escape_mask, move_mask, mut_receiver).into()
    }

    pub(super) fn op_close_upvalue(&mut self) {
        let location = self.read_next() as usize;
        let p = unsafe { (*self.frames.top()).stack_start.add(location) };
        debug_assert!((p as *const Value) < self.stack.top() as *const Value, "CLOSE_UPVALUE operand is not a live local");
        self.close_upvalues(p);
        self.stack.truncate(1);
    }

    /// Builds a type whose methods capture.
    pub(super) fn op_build_type(&mut self) -> Result<(), anyhow::Error> {
        let const_idx = self.read_next() as usize;
        let template = self.chunk.constants[const_idx].as_object().as_type_ptr();

        // Read the template out in one go.
        let (mut ty, capturing) = {
            let template = unsafe { &*template };
            let capturing: SmallVec<[(u8, *mut ObjFn); 8]> = template.methods.iter()
                .filter(|(_, method)| method.tag() == objects::TAG_FUNCTION)
                .map(|(&id, method)| (id, method.as_function_ptr()))
                .filter(|(_, function)| !unsafe { &**function }.upvalues.is_empty())
                .collect();
            (template.duplicate(), capturing)
        };

        if !self.stack.has_room(capturing.len() + 1) {
            return Err(self.stack_overflow());
        }

        // Capturing can collect, so each new closure is kept on the stack.
        for &(_, function) in &capturing {
            let closure = self.create_closure(function);
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

    pub(super) fn op_push_closure(&mut self) -> Result<(), anyhow::Error> {
        let const_idx = self.read_next() as usize;
        let value = self.chunk.constants[const_idx];
        let fn_ref = value.as_object().as_function_ptr();
        let closure = self.create_closure(fn_ref);
        self.stack.push(Value::from(closure));
        Ok(())
    }
}

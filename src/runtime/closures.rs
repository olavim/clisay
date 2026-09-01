use super::*;

impl Vm {
    pub(super) fn get_upvalue(&self, idx: usize) -> *mut ObjUpvalue {
        unsafe { ObjClosure::upvalue_at((*self.frames.top()).closure, idx) }
    }

    fn capture_upvalue(&mut self, location: *mut Value, slot: u8, table: u16, at: usize) -> *mut ObjUpvalue {
        match self.open_upvalues.iter().find(|&&upvalue| unsafe { (*upvalue).location } == location) {
            Some(&upvalue) => upvalue,
            None =>  {
                let accepts = self.slot_accepts_in(table, slot, at);
                let upvalue = self.alloc(ObjUpvalue::new(location, accepts));
                self.open_upvalues.push(upvalue);
                upvalue
            }
        }
    }

    fn close_open_upvalues(&mut self, should_close: impl Fn(*const Value) -> bool) {
        for idx in (0..self.open_upvalues.len()).rev() {
            unsafe {
                let upvalue = *self.open_upvalues.get_unchecked(idx);
                if should_close((*upvalue).location) {
                    (*upvalue).close();
                    self.open_upvalues.swap_remove(idx);
                }
            }
        }
    }

    pub(super) fn close_upvalues(&mut self, after: *const Value) {
        debug_assert!(after <= self.stack.top() as *const Value, "closing upvalues above the live stack top");
        self.close_open_upvalues(|location| after <= location);
        debug_assert!(!self.open_upvalues.iter().any(|&u| after <= unsafe { (*u).location }),
            "an upvalue at or above the closed slot stayed open");
    }

    pub(super) fn store_through_upvalue(&mut self, idx: usize, value: Value) -> Result<(), anyhow::Error> {
        let upvalue = self.get_upvalue(idx);
        let accepted = self.accept_upvalue_write(upvalue, value)?;
        self.write_upvalue(accepted);
        Ok(())
    }

    pub(super) fn create_closure(&mut self, function: *mut ObjFn) -> Object {
        let fn_ref = unsafe { &*function };
        let upvalue_count = fn_ref.upvalues.len();

        // Gather the captured upvalues into a stack-resident scratch buffer first,
        // then allocate the exact-sized closure in one shot. Capturing must happen
        // before allocation since capturing can trigger GC.
        let (table, at) = (self.running_slot_table(), self.code_index_at(self.ip));
        let mut upvalues: SmallVec<[*mut ObjUpvalue; 8]> = SmallVec::with_capacity(upvalue_count);
        for i in 0..upvalue_count {
            let fn_upval = &fn_ref.upvalues[i];
            let upvalue = if fn_upval.is_local {
                self.capture_upvalue(unsafe { (*self.frames.top()).stack_start.add(fn_upval.location as usize) }, fn_upval.location, table, at)
            } else {
                self.get_upvalue(fn_upval.location as usize)
            };

            // The closure can outlive the frame the capture came from, so a scope exit there must
            // not release what the captured value holds.
            objects::record_escape(unsafe { *(*upvalue).location });
            upvalues.push(upvalue);
        }

        let (name, arity, ip_start) = (fn_ref.name, fn_ref.arity, fn_ref.ip_start);
        let (escape_mask, retain_mask, needs_borrow_mark) = (fn_ref.escape_mask, fn_ref.retain_mask, fn_ref.needs_borrow_mark);
        let receiver_needs_borrow = fn_ref.receiver_needs_borrow;
        let mut_receiver = fn_ref.mut_receiver;
        let retain_receiver = fn_ref.retain_receiver;
        let param_accepts = fn_ref.param_accepts;
        let slot_accepts = fn_ref.slot_accepts;
        if self.gc.should_collect() {
            self.start_gc();
        }
        let closure: Object = self.gc.alloc_closure(name, arity, ip_start, &upvalues, escape_mask, retain_mask, needs_borrow_mark, mut_receiver, retain_receiver, receiver_needs_borrow, param_accepts, slot_accepts).into();
        closure
    }

    pub(super) fn op_close_upvalue(&mut self) {
        let location = self.read_next() as usize;
        let p = self.slot_addr(location);
        debug_assert!((p as *const Value) < self.stack.top() as *const Value, "CLOSE_UPVALUE operand is not a live local");
        self.close_upvalues(p);
        self.stack.truncate(1);
    }

    pub(super) fn op_close_slot_upvalue(&mut self) {
        let location = self.read_next() as usize;
        let p = self.slot_addr(location);
        debug_assert!((p as *const Value) < self.stack.top() as *const Value, "CLOSE_SLOT_UPVALUE operand is not a live local");
        self.close_open_upvalues(|location| std::ptr::eq(location, p as *const Value));
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

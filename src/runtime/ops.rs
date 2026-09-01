use super::*;

macro_rules! num_binop_methods {
    ( $( $name:ident => |$a:ident, $b:ident| $body:expr, $token:literal );+ $(;)? ) => {
        $(
            pub(super) fn $name(&mut self) -> Result<(), anyhow::Error> {
                self.binary_op_number(|$a, $b| Value::from($body), $token)
            }
        )+
    };
}

macro_rules! value_binop_methods {
    ( $( $name:ident => |$a:ident, $b:ident| $body:expr );+ $(;)? ) => {
        $(
            pub(super) fn $name(&mut self) -> Result<(), anyhow::Error> {
                self.binary_op(|$a, $b| Value::from($body))
            }
        )+
    };
}

macro_rules! unary_op_methods {
    ( $( $name:ident => |$v:ident| $check:ident => $body:expr );+ $(;)? ) => {
        $(
            pub(super) fn $name(&mut self) -> Result<(), anyhow::Error> {
                let $v = self.stack.pop();
                if !$v.$check() {
                    bail!("Invalid operand")
                }
                self.stack.push(Value::from($body));
                Ok(())
            }
        )+
    };
}

impl Vm {
    pub(super) fn op_dup2(&mut self) {
        let under = self.stack.peek(1);
        let over = self.stack.peek(0);
        self.stack.push(under);
        self.stack.push(over);
    }

    fn unwind_to(&mut self, stack_start: *mut Value, _leaving: Value) -> Result<(), anyhow::Error> {
        self.close_upvalues(stack_start);
        self.stack.set_top(stack_start);
        Ok(())
    }

    pub(super) fn op_return(&mut self) -> Result<bool, anyhow::Error> {
        if unsafe { (*self.frames.top()).closure.is_null() } {
            return Ok(false);
        }

        let frame = self.frames.pop();
        self.ip = frame.return_ip;
        if !self.tail_breadcrumbs.is_empty() {
            self.release_tail_breadcrumbs();
        }

        // Handing a borrowed value back does not end the borrow.
        let handed_back_from = self.stack.offset(0);
        let handed_back_borrow = !self.stack.peek(0).is_object()
            && self.stack.borrow_outlives(handed_back_from, frame.stack_start);
        let value = self.stack.pop();
        // The value outlives this frame, so the scope releases below must not let go of it.
        objects::record_escape(value);
        self.unwind_to(frame.stack_start, value)?;
        self.stack.push(value);
        if handed_back_borrow {
            let into = self.stack.offset(0);
            self.stack.mark_borrowed(into, self.stack.borrow_origin(handed_back_from));
        }
        Ok(true)
    }

    pub(super) fn op_return_factory(&mut self) -> Result<(), anyhow::Error> {
        let frame = self.frames.pop();
        self.ip = frame.return_ip;

        let value = self.stack.pop();
        objects::record_escape(value);
        self.unwind_to(frame.stack_start, value)?;
        if frame.seal {
            crate::core::objects::freeze_value(value, self.current_pos_index());
        }
        self.stack.push(value);
        Ok(())
    }

    pub(super) fn op_throw(&mut self) -> Result<(), anyhow::Error> {
        let value = self.stack.pop();
        self.throw_value(value)
    }

    pub(super) fn throw_value(&mut self, value: Value) -> Result<(), anyhow::Error> {
        let caught_here = self.try_frames.iter().rev()
            .find(|f| f.kind == TryKind::Catch)
            .is_some_and(|f| f.origin == self.frames.top_ptr());
        if !caught_here {
        }
        // A thrown value passes every scope between here and the handler, so none of them may
        // let go of it.
        objects::record_escape(value);
        if self.try_frames.len() == 0 {
            return self.error(format!("Uncaught exception: {}", value.fmt()));
        }

        let frame = self.try_frames.pop().unwrap();
        // Restore borrows marked since the `try` began, whose frame exits the unwind skips.
        self.frames.set_top(frame.origin);
        if !self.tail_breadcrumbs.is_empty() {
            self.release_tail_breadcrumbs();
        }
        self.unwind_to(frame.stack_start, value)?;
        self.ip = frame.handler_ip;
        self.stack.push(value);
        Ok(())
    }

    pub(super) fn op_push_try(&mut self, kind: TryKind) {
        let handler_pos = as_short!(self.read_next(), self.read_next()) as usize;
        self.try_frames.push(TryFrame {
            kind,
            origin: self.frames.top_ptr(),
            handler_ip: unsafe { self.chunk.code.as_ptr().add(handler_pos) },
            stack_start: self.stack.top(),
        });
    }

    pub(super) fn op_pop_try(&mut self) {
        self.try_frames.pop();
    }

    pub(super) fn op_jump_if_false_or_pop(&mut self) {
        let offset = as_short!(self.read_next(), self.read_next()) as usize;
        if self.stack.peek(0).is_falsy() {
            self.ip = unsafe { self.chunk.code.as_ptr().add(offset) };
        } else {
            self.stack.truncate(1);
        }
    }

    pub(super) fn op_jump_if_true_or_pop(&mut self) {
        let offset = as_short!(self.read_next(), self.read_next()) as usize;
        if !self.stack.peek(0).is_falsy() {
            self.ip = unsafe { self.chunk.code.as_ptr().add(offset) };
        } else {
            self.stack.truncate(1);
        }
    }

    pub(super) fn op_jump_if_not_null_or_pop(&mut self) {
        let offset = as_short!(self.read_next(), self.read_next()) as usize;
        if !self.stack.peek(0).is_null() {
            self.ip = unsafe { self.chunk.code.as_ptr().add(offset) };
        } else {
            self.stack.truncate(1);
        }
    }

    pub(super) fn op_jump_if_null(&mut self) {
        let offset = as_short!(self.read_next(), self.read_next()) as usize;
        if self.stack.peek(0).is_null() {
            self.ip = unsafe { self.chunk.code.as_ptr().add(offset) };
        }
    }

    fn is_err(&self, value: Value) -> bool {
        let ValueKind::Object(ObjectKind::Instance) = value.kind() else { return false };
        let err_id = unsafe { &*self.native_types.err }.id;
        let ty = unsafe { &*(*value.as_object().as_instance_ptr()).ty };
        ty.provided.contains(&err_id)
    }

    pub(super) fn op_jump_if_clean(&mut self) {
        let offset = as_short!(self.read_next(), self.read_next()) as usize;
        let value = self.stack.peek(0);
        if !value.is_null() && !self.is_err(value) {
            self.ip = unsafe { self.chunk.code.as_ptr().add(offset) };
        }
    }

    pub(super) fn op_jump_if_is(&mut self) {
        let offset = as_short!(self.read_next(), self.read_next()) as usize;
        let id = u16::from_le_bytes([self.read_next(), self.read_next()]);
        let value = self.stack.peek(0);
        let provides = matches!(value.kind(), ValueKind::Object(ObjectKind::Instance))
            && unsafe { &*(*value.as_object().as_instance_ptr()).ty }.provided.contains(&id);
        if provides {
            self.ip = unsafe { self.chunk.code.as_ptr().add(offset) };
        }
    }

    pub(super) fn op_jump_if_bad(&mut self) {
        let offset = as_short!(self.read_next(), self.read_next()) as usize;
        let value = self.stack.peek(0);
        if value.is_null() || self.is_err(value) {
            self.ip = unsafe { self.chunk.code.as_ptr().add(offset) };
        }
    }

    pub(super) fn op_barrier_guard(&mut self) -> Result<(), anyhow::Error> {
        let allowed = self.read_allowed_witnesses();
        let value = self.stack.peek(0);
        if value.is_null() {
            return match self.accepts_null(allowed) {
                true => Ok(()),
                false => self.error("unexpected null"),
            };
        }
        if self.carries_disallowed_witness(value, allowed) {
            let bad = self.stack.pop();
            return self.throw_value(bad);
        }
        Ok(())
    }

    pub(super) fn op_assert_non_null(&mut self) -> Result<(), anyhow::Error> {
        let forced = self.at_elided_site();
        if self.stack.peek(0).is_null() {
            return match forced {
                true => self.refuted_elision_error("a value proven non-null is null"),
                false => self.error("unexpected null"),
            };
        }
        Ok(())
    }

    pub(super) fn op_array(&mut self) -> Result<(), anyhow::Error> {
        let len = self.read_next() as usize;
        let seal = self.read_next() != 0;
        // Copy the elements without popping them first: they must stay on the stack
        // and remain GC roots because the allocation below can trigger a collection.
        let values = unsafe {
            let start = self.stack.top().sub(len);
            std::slice::from_raw_parts(start, len).to_vec()
        };
        let array = self.alloc(ObjArray::new(values));
        let container = Value::from(array);
        self.take_elements(container, len, 1, seal)?;
        self.stack.truncate(len);
        self.push_built_container(container, seal);
        Ok(())
    }

    /// Refuses a mutable element in a sealed literal, which is all a container asks of what it takes.
    fn take_elements(&mut self, _container: Value, count: usize, step: usize, seal: bool) -> Result<(), anyhow::Error> {
        if !seal {
            return Ok(());
        }
        for i in (0..count).step_by(step) {
            if objects::is_mutable_container(unsafe { *self.stack.offset(i) }) {
                return self.mutable_in_immutable_error();
            }
        }
        Ok(())
    }

    fn push_immutable(&mut self, value: Value) {
        value.as_object().set_immutable(self.current_pos_index());
        self.stack.push(value);
    }

    fn push_built_container(&mut self, value: Value, seal: bool) {
        match seal {
            true => self.push_immutable(value),
            false => self.stack.push(value),
        }
    }

    /// Replaces the array on top with a fresh copy of `array[prefix .. len - suffix]`.
    pub(super) fn op_array_middle(&mut self) {
        let prefix = self.read_next() as usize;
        let suffix = self.read_next() as usize;
        // Keep the source array on the stack as a GC root across the allocation below.
        let target = self.stack.peek(0);
        // A match may take the middle before it has tested the length, so anything the slice does
        // not reach reads as null rather than faulting.
        let source = match target.kind() {
            ValueKind::Object(ObjectKind::Array) => unsafe { &(*target.as_object().as_array_ptr()).values },
            _ => { self.stack.set(0, Value::NULL); return; },
        };
        let Some(end) = source.len().checked_sub(suffix).filter(|end| *end >= prefix) else {
            self.stack.set(0, Value::NULL);
            return;
        };
        let values = source[prefix..end].to_vec();
        // A slice holds only what it copied, so the elements answer rather than the source.
        let array = self.alloc(ObjArray::new(values));
        self.stack.truncate(1);
        self.push_immutable(Value::from(array));
    }

    pub(super) fn op_dict(&mut self) -> Result<(), anyhow::Error> {
        let count = self.read_next() as usize;
        let seal = self.read_next() != 0;
        let n = count * 2;
        // Build the entry map from the key/value pairs still on the stack; they
        // stay rooted there until after the allocation (which may collect).
        let mut entries = fnv::FnvHashMap::with_capacity_and_hasher(count, Default::default());
        unsafe {
            let start = self.stack.top().sub(n);
            let pairs = std::slice::from_raw_parts(start, n);
            for pair in pairs.chunks_exact(2) {
                entries.insert(DictKey(pair[0]), pair[1]);
            }
        }
        let dict = self.alloc(ObjDict::new(entries));
        let container = Value::from(dict);
        // Every second slot is a value, counting from the top where the last pair's value sits.
        self.take_elements(container, n, 2, seal)?;
        self.stack.truncate(n);
        self.push_built_container(container, seal);
        Ok(())
    }

    /// Clears the immutable bit on the value on top of the stack.
    pub(super) fn op_mut(&mut self) {
        let value = self.stack.peek(0);
        if value.is_object() {
            value.as_object().set_mutable();
        }
    }

    pub(super) fn op_seal_check(&mut self) -> Result<(), anyhow::Error> {
        let container = self.stack.peek(0);
        let mutable = match container.kind() {
            ValueKind::Object(ObjectKind::Array) =>
                unsafe { &(*container.as_object().as_array_ptr()).values }.iter().any(|v| crate::core::objects::is_mutable_container(*v)),
            ValueKind::Object(ObjectKind::Dict) =>
                unsafe { (*container.as_object().as_dict_ptr()).entries.values() }.any(|v| crate::core::objects::is_mutable_container(*v)),
            _ => false,
        };
        if mutable {
            return self.mutable_in_immutable_error();
        }
        Ok(())
    }

    pub(super) fn op_push_type(&mut self) {
        let const_idx = self.read_next() as usize;
        let value = self.chunk.constants[const_idx];
        self.stack.push(value);
    }

    pub(super) fn op_load_global(&mut self) -> Result<(), anyhow::Error> {
        let const_idx = self.read_next() as usize;
        let constant = &self.chunk.constants[const_idx];
        let string = constant.as_object().as_string_ptr();
        let Some(&value) = self.globals.get(&string) else {
            return self.error(format!("Undefined variable '{}'", unsafe { &*string }.value));
        };
        self.stack.push(value);
        Ok(())
    }

    pub(super) fn op_is(&mut self) {
        let id = u16::from_le_bytes([self.read_next(), self.read_next()]);
        let receiver = self.stack.pop();
        let provides = matches!(receiver.kind(), ValueKind::Object(ObjectKind::Instance))
            && unsafe { &*(*receiver.as_object().as_instance_ptr()).ty }.provided.contains(&id);
        self.stack.push(Value::from(provides));
    }

    pub(super) fn op_add(&mut self) -> Result<(), anyhow::Error> {
        let b = self.stack.pop();
        let a = self.stack.pop();

        if a.is_number() && b.is_number() {
            self.stack.push(Value::from(a.as_number() + b.as_number()));
            return Ok(());
        }

        let result = match (a.kind(), b.kind()) {
            (ValueKind::Object(ObjectKind::String), ValueKind::Object(ObjectKind::String)) => {
                let a = a.as_object();
                let b = b.as_object();
                let s = [a.as_string().as_str(), b.as_string().as_str()].concat();
                Value::from(self.intern(s))
            },
            _ => {
                return self.error(format!("Operator '+' cannot be applied to operands {} and {}", a, b))
            }
        };

        self.stack.push(result);
        Ok(())
    }

    num_binop_methods! {
        op_subtract           => |a, b| a - b,  "-";
        op_multiply           => |a, b| a * b,  "*";
        op_divide             => |a, b| a / b,  "/";
        op_less_than          => |a, b| a < b,  "<";
        op_less_than_equal    => |a, b| a <= b, "<=";
        op_greater_than       => |a, b| a > b,  ">";
        op_greater_than_equal => |a, b| a >= b, ">=";
        op_left_shift         => |a, b| ((a as i64) << (b as i64)) as f64, "<<";
        op_right_shift        => |a, b| ((a as i64) >> (b as i64)) as f64, ">>";
        op_bit_and            => |a, b| ((a as i64) & (b as i64)) as f64, "&&";
        op_bit_or             => |a, b| ((a as i64) | (b as i64)) as f64, "||";
        op_bit_xor            => |a, b| ((a as i64) ^ (b as i64)) as f64, "^";
    }

    value_binop_methods! {
        op_equal     => |a, b| a.value_eq(b);
        op_not_equal => |a, b| !a.value_eq(b);
    }

    unary_op_methods! {
        op_negate  => |v| is_number => -v.as_number();
        op_bit_not => |v| is_number => !(v.as_number() as i64) as f64;
    }

    fn binary_op_number<F: Fn(f64, f64) -> Value>(&mut self, func: F, token: impl Into<String>) -> Result<(), anyhow::Error> {
        let b = self.stack.pop();
        let a = self.stack.pop();

        if !a.is_number() || !b.is_number() {
            return self.error(format!("Operator '{}' cannot be applied to operands {} and {}", token.into(), a, b));
        }

        self.stack.push(func(a.as_number(), b.as_number()));
        Ok(())
    }

    fn binary_op<F: Fn(Value, Value) -> Value>(&mut self, func: F) -> Result<(), anyhow::Error> {
        let b = self.stack.pop();
        let a = self.stack.pop();
        self.stack.push(func(a, b));
        Ok(())
    }
}

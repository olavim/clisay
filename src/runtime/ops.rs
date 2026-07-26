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
    pub(super) fn op_return(&mut self) -> Result<bool, anyhow::Error> {
        if self.frames.len() == 1 {
            return Ok(false);
        }

        let frame = self.frames.pop();

        self.ip = frame.return_ip;
        self.close_upvalues(frame.stack_start);

        let value = self.stack.pop();
        self.stack.set_top(frame.stack_start);
        self.stack.push(value);
        Ok(true)
    }

    /// A factory's return: like `op_return`, but deep-freezes the returned instance when the frame's
    /// seal bit is set.
    pub(super) fn op_return_fac(&mut self) {
        let frame = self.frames.pop();
        self.ip = frame.return_ip;
        self.close_upvalues(frame.stack_start);

        let value = self.stack.pop();
        if frame.seal {
            crate::core::objects::freeze_value(value, self.current_pos_index());
        }
        self.stack.set_top(frame.stack_start);
        self.stack.push(value);
    }

    pub(super) fn op_throw(&mut self) -> Result<(), anyhow::Error> {
        let value = self.stack.pop();
        self.throw_value(value)
    }

    pub(super) fn throw_value(&mut self, value: Value) -> Result<(), anyhow::Error> {
        if self.try_frames.len() == 0 {
            return self.error(format!("Uncaught exception: {}", value.fmt()));
        }

        let frame = self.try_frames.pop().unwrap();
        // Restore borrows marked since the `try` began, whose `RELEASE_BORROW` the unwind skips.
        while self.borrows.len() > frame.borrow_depth {
            let (v, prev) = self.borrows.pop().unwrap();
            if v.is_object() { v.as_object().set_borrowed(prev); }
        }
        self.frames.set_top(frame.origin);
        self.stack.set_top(frame.stack_start);
        self.ip = frame.handler_ip;
        self.stack.push(value);
        Ok(())
    }

    pub(super) fn op_push_try(&mut self) {
        let handler_pos = as_short!(self.read_next(), self.read_next()) as usize;
        self.try_frames.push(TryFrame {
            origin: self.frames.top_ptr(),
            handler_ip: unsafe { self.chunk.code.as_ptr().add(handler_pos) },
            stack_start: self.stack.top(),
            borrow_depth: self.borrows.len()
        });
    }

    pub(super) fn op_pop_try(&mut self) {
        self.try_frames.pop();
    }

    /// `&&`: keep the top value and jump to the end when it is falsy (the result
    /// is the left operand); otherwise pop it and fall through to evaluate the right.
    pub(super) fn op_jump_if_false_or_pop(&mut self) {
        let offset = as_short!(self.read_next(), self.read_next()) as usize;
        if self.stack.peek(0).is_falsy() {
            self.ip = unsafe { self.chunk.code.as_ptr().add(offset) };
        } else {
            self.stack.truncate(1);
        }
    }

    /// `||`: keep the top value and jump to the end when it is truthy; otherwise
    /// pop it and fall through to evaluate the right operand.
    pub(super) fn op_jump_if_true_or_pop(&mut self) {
        let offset = as_short!(self.read_next(), self.read_next()) as usize;
        if !self.stack.peek(0).is_falsy() {
            self.ip = unsafe { self.chunk.code.as_ptr().add(offset) };
        } else {
            self.stack.truncate(1);
        }
    }

    /// `??`: keep the top value and jump to the end when it is non-null (the result is the left
    /// operand); otherwise pop it and fall through to evaluate the fallback.
    pub(super) fn op_jump_if_not_null_or_pop(&mut self) {
        let offset = as_short!(self.read_next(), self.read_next()) as usize;
        if !self.stack.peek(0).is_null() {
            self.ip = unsafe { self.chunk.code.as_ptr().add(offset) };
        } else {
            self.stack.truncate(1);
        }
    }

    /// `?.`/`?[`: keep the top value and jump to the end when it is null (the result is null);
    /// otherwise leave the receiver and fall through to the member access.
    pub(super) fn op_jump_if_null(&mut self) {
        let offset = as_short!(self.read_next(), self.read_next()) as usize;
        if self.stack.peek(0).is_null() {
            self.ip = unsafe { self.chunk.code.as_ptr().add(offset) };
        }
    }

    fn is_err(&self, value: Value) -> bool {
        matches!(value.kind(), ValueKind::Object(ObjectKind::Instance))
            && unsafe { (*value.as_object().as_instance_ptr()).ty == self.native_types.err }
    }

    /// A discharge test: keep the top value and jump when it is clean.
    pub(super) fn op_jump_if_clean(&mut self) {
        let offset = as_short!(self.read_next(), self.read_next()) as usize;
        let value = self.stack.peek(0);
        if !value.is_null() && !self.is_err(value) {
            self.ip = unsafe { self.chunk.code.as_ptr().add(offset) };
        }
    }

    /// Keep the top value and jump when `value is <name>`.
    pub(super) fn op_jump_if_is(&mut self) {
        let offset = as_short!(self.read_next(), self.read_next()) as usize;
        let const_idx = self.read_next() as usize;
        let name = self.chunk.constants[const_idx].as_object().as_string_ptr();
        let value = self.stack.peek(0);
        let provides = matches!(value.kind(), ValueKind::Object(ObjectKind::Instance))
            && unsafe { &*(*value.as_object().as_instance_ptr()).ty }.provided.contains(&name);
        if provides {
            self.ip = unsafe { self.chunk.code.as_ptr().add(offset) };
        }
    }

    /// Keep the top value and jump when it is bad (an obligation witness).
    pub(super) fn op_jump_if_bad(&mut self) {
        let offset = as_short!(self.read_next(), self.read_next()) as usize;
        let value = self.stack.peek(0);
        if value.is_null() || self.is_err(value) {
            self.ip = unsafe { self.chunk.code.as_ptr().add(offset) };
        }
    }

    /// Guards an unknown value at a destination. Throws a value that provides a registered witness
    /// the destination does not allow, aborts on a disallowed null, and passes everything else.
    pub(super) fn op_barrier_guard(&mut self) -> Result<(), anyhow::Error> {
        let null_allowed = self.read_next() != 0;
        let count = self.read_next() as usize;
        let mut allow: Vec<*mut ObjString> = Vec::with_capacity(count);
        for _ in 0..count {
            let idx = self.read_next() as usize;
            allow.push(self.chunk.constants[idx].as_object().as_string_ptr());
        }
        let value = self.stack.peek(0);
        if value.is_null() {
            return if null_allowed { Ok(()) } else { self.error("unexpected null") };
        }
        if matches!(value.kind(), ValueKind::Object(ObjectKind::Instance)) {
            let ty = unsafe { &*(*value.as_object().as_instance_ptr()).ty };
            for &name in &ty.provided {
                if self.witnesses.contains(&name) && !allow.contains(&name) {
                    let bad = self.stack.pop();
                    return self.throw_value(bad);
                }
            }
        }
        Ok(())
    }

    pub(super) fn op_assert_non_null(&mut self) -> Result<(), anyhow::Error> {
        if self.stack.peek(0).is_null() {
            return self.error("unexpected null");
        }
        Ok(())
    }

    pub(super) fn op_array(&mut self) {
        let len = self.read_next() as usize;
        // Copy the elements without popping them first: they must stay on the stack
        // and remain GC roots because the allocation below can trigger a collection.
        let values = unsafe {
            let start = self.stack.top().sub(len);
            std::slice::from_raw_parts(start, len).to_vec()
        };
        let array = self.alloc(ObjArray::new(values));
        self.stack.truncate(len);
        self.push_immutable(Value::from(array));
    }

    /// Pushes a freshly built container, sealed immutable by default.
    fn push_immutable(&mut self, value: Value) {
        value.as_object().set_immutable(self.current_pos_index());
        self.stack.push(value);
    }

    /// Replaces the array on top with a fresh copy of `array[prefix .. len - suffix]`.
    pub(super) fn op_array_middle(&mut self) {
        let prefix = self.read_next() as usize;
        let suffix = self.read_next() as usize;
        // Keep the source array on the stack as a GC root across the allocation below.
        let target = self.stack.peek(0);
        let values = unsafe {
            let source = &(*target.as_object().as_array_ptr()).values;
            source[prefix..source.len() - suffix].to_vec()
        };
        let slice = self.alloc(ObjArray::new(values));
        self.stack.truncate(1);
        self.push_immutable(Value::from(slice));
    }

    pub(super) fn op_dict(&mut self) {
        let count = self.read_next() as usize;
        let n = count * 2;
        // Build the entry map from the key/value pairs still on the stack; they
        // stay rooted there until after the allocation (which may collect).
        let mut entries = fnv::FnvHashMap::with_capacity_and_hasher(count, Default::default());
        unsafe {
            let start = self.stack.top().sub(n);
            let pairs = std::slice::from_raw_parts(start, n);
            for pair in pairs.chunks_exact(2) {
                entries.insert(pair[0], pair[1]);
            }
        }
        let dict = self.alloc(ObjDict::new(entries));
        self.stack.truncate(n);
        self.push_immutable(Value::from(dict));
    }

    /// Clears the immutable bit on the value on top of the stack.
    pub(super) fn op_mut(&mut self) {
        let value = self.stack.peek(0);
        if value.is_object() {
            value.as_object().set_mutable();
        }
    }

    /// Traps a mutable element in the immutable container on top of the stack. The compile pass
    /// rejects known-mutable elements, so this only fires for a value of unknown capability that
    /// turns out mutable at runtime.
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
            return self.error_seal();
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

    /// `x is T`: pushes whether the receiver's type provides the trait/type named by the constant
    /// operand. Never errors: a non-instance receiver (null/number/dict/…) yields `false`.
    pub(super) fn op_is(&mut self) {
        let const_idx = self.read_next() as usize;
        let name = self.chunk.constants[const_idx].as_object().as_string_ptr();
        let receiver = self.stack.pop();
        let provides = matches!(receiver.kind(), ValueKind::Object(ObjectKind::Instance))
            && unsafe { &*(*receiver.as_object().as_instance_ptr()).ty }.provided.contains(&name);
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

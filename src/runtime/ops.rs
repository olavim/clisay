use super::*;
use crate::backend::bytecode::chunk::HandlerRange;
use crate::core::equality;

macro_rules! num_binop_methods {
    ( $( $name:ident => |$a:ident, $b:ident| $body:expr, $token:literal );+ $(;)? ) => {
        $(
            #[inline]
            pub(super) fn $name(&mut self) -> Result<(), anyhow::Error> {
                self.binary_op_number(|$a, $b| Value::from($body), $token)
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

    fn unwind_to(&mut self, stack_start: *mut Value) {
        self.stack.set_top(stack_start);
    }

    pub(super) fn op_return_factory(&mut self) -> Result<(), anyhow::Error> {
        let frame = self.frames.pop();
        self.ip = frame.return_ip;

        let value = self.stack.pop();
        self.unwind_to(frame.stack_start);
        self.stack.push(value);
        Ok(())
    }

    pub(super) fn op_throw(&mut self) -> Result<(), anyhow::Error> {
        throw_value(self.stack.pop())
    }

    /// Unwinds to the innermost handler and hands it the value.
    pub(super) fn catch_thrown(&mut self, value: Value) -> Result<(), anyhow::Error> {
        let Some((frame, handler)) = self.innermost_handler() else {
            return self.error(format!("Uncaught exception: {}", value.fmt()));
        };

        let origin = unsafe { frame.add(1) };
        self.copy_out_unwound_frames(origin);
        self.frames.set_top(origin);
        if !self.tail_breadcrumbs.is_empty() {
            self.release_tail_breadcrumbs();
        }
        self.unwind_to(unsafe { (*frame).stack_start.add(handler.frame_stack_height as usize) });
        self.ip = unsafe { self.chunk.code.as_ptr().add(handler.handler as usize) };
        self.stack.push(value);
        Ok(())
    }

    fn innermost_handler(&self) -> Option<(*mut CallFrame, HandlerRange)> {
        self.frames_at_ips().find_map(|(frame, ip)| self.chunk.handler_at(self.code_index_at(ip)).map(|handler| (frame, handler)))
    }

    /// Copies out the anchor parameters of every frame a throw leaves, innermost first.
    fn copy_out_unwound_frames(&mut self, origin: *mut CallFrame) {
        let mut frame_top = self.stack.top();
        let mut at = self.frames.top_ptr();
        while at > origin {
            at = unsafe { at.sub(1) };
            let frame = unsafe { *at };
            if !frame.closure.is_null() {
                let table = unsafe { (*frame.closure).slot_witness_set_pool_id } as usize;
                for i in 0..self.chunk.anchor_params[table].len() {
                    let (anchor_slot, value_slot) = self.chunk.anchor_params[table][i];
                    if unsafe { frame.stack_start.add(value_slot as usize + 1) } >= frame_top {
                        continue;
                    }
                    self.copy_anchor_out(frame.stack_start, anchor_slot, value_slot);
                }
            }
            frame_top = frame.stack_start;
        }
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

    pub(super) fn op_jump_if_clean_or_pop(&mut self) {
        let offset = as_short!(self.read_next(), self.read_next()) as usize;
        let value = self.stack.peek(0);
        if !self.is_witness(value) {
            self.ip = unsafe { self.chunk.code.as_ptr().add(offset) };
        } else {
            self.stack.truncate(1);
        }
    }

    pub(super) fn is_witness(&self, value: Value) -> bool {
        value.is_null() || self.is_object_witness(value)
    }

    fn is_object_witness(&self, value: Value) -> bool {
        let ValueKind::Object(ObjectKind::Instance) = value.kind() else { return false };
        !unsafe { &*(*value.as_object().as_instance_ptr()).ty }.witness_ids.is_empty()
    }

    pub(super) fn op_jump_if_clean(&mut self) {
        let offset = as_short!(self.read_next(), self.read_next()) as usize;
        let value = self.stack.peek(0);
        if !self.is_witness(value) {
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
        if self.is_witness(value) {
            self.ip = unsafe { self.chunk.code.as_ptr().add(offset) };
        }
    }

    #[inline]
    pub(super) fn op_barrier_guard(&mut self) -> Result<(), anyhow::Error> {
        let witness_set_pool_id = self.read_witness_set_pool_id();
        let forced = self.at_forced_check();
        let value = self.anchor_value(self.stack.peek(0))?;
        match self.barrier_guard(value, witness_set_pool_id) {
            Err(_) if forced => self.refuted_elision_error("a value proven accepted is refused"),
            checked => checked,
        }
    }

    #[inline]
    fn barrier_guard(&mut self, value: Value, witness_set_pool_id: u16) -> Result<(), anyhow::Error> {
        if value.is_null() {
            return match self.accepts_null(witness_set_pool_id) {
                true => Ok(()),
                false => self.error("unexpected null"),
            };
        }
        if self.carries_disallowed_witness(value, witness_set_pool_id) {
            self.stack.pop();
            return throw_value(value);
        }
        Ok(())
    }

    pub(super) fn op_assert_non_null(&mut self) -> Result<(), anyhow::Error> {
        let forced = self.at_forced_check();
        if self.stack.peek(0).is_null() {
            return match forced {
                true => self.refuted_elision_error("a value proven non-null is null"),
                false => throw_value(self.stack.pop()),
            };
        }
        Ok(())
    }

    /// Writes an unnamed slot. A temp is not a binding, so nothing takes the value over and it is
    /// stored as it is, where a write to a binding copies.
    pub(super) fn op_store_temp_pop(&mut self) {
        let slot = self.read_next() as usize;
        let value = self.stack.pop();
        unsafe { *self.slot_addr(slot) = value };
    }

    #[inline]
    pub(super) fn op_array(&mut self) -> Result<(), anyhow::Error> {
        let len = self.read_next() as usize;
        // Copy the elements without popping them first. They must stay on the stack
        // because the allocation below can trigger gc.
        let start = unsafe { self.stack.top().sub(len) };
        for i in 0..len {
            let at = unsafe { start.add(i) };
            refuse_no_persist(unsafe { *at })?;
            self.share_in_place(at);
        }
        let array = self.alloc_array(unsafe { std::slice::from_raw_parts(start, len) });
        self.stack.truncate(len);
        self.stack.push(Value::from(array));
        Ok(())
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
            ValueKind::Object(ObjectKind::Array) => unsafe { ObjArray::elements(target.as_object().as_array_ptr()) },
            _ => { self.stack.set(0, Value::NULL); return; },
        };
        let Some(end) = source.len().checked_sub(suffix).filter(|end| *end >= prefix) else {
            self.stack.set(0, Value::NULL);
            return;
        };
        let array = self.alloc_array(&source[prefix..end]);
        self.stack.truncate(1);
        self.stack.push(Value::from(array));

        for element in unsafe { ObjArray::elements_mut(array) } {
            self.share_in_place(element);
        }
    }

    pub(super) fn op_dict(&mut self) -> Result<(), anyhow::Error> {
        let count = self.read_next() as usize;
        let n = count * 2;
        let mut entries = fnv::FnvHashMap::with_capacity_and_hasher(count, Default::default());
        let start = unsafe { self.stack.top().sub(n) };

        for i in 0..n {
            let at = unsafe { start.add(i) };
            if i % 2 == 1 || objects::is_container(unsafe { *at }) {
                refuse_no_persist(unsafe { *at })?;
                self.share_in_place(at);
            }
        }

        for i in (0..n).step_by(2) {
            self.assert_key_is_acyclic(unsafe { *start.add(i) });
        }

        unsafe {
            let pairs = std::slice::from_raw_parts(start, n);
            for pair in pairs.chunks_exact(2) {
                entries.insert(DictKey(pair[0]), pair[1]);
            }
        }
        let dict = self.alloc(ObjDict::new(entries));
        self.stack.truncate(n);
        self.stack.push(Value::from(dict));
        Ok(())
    }

    pub(super) fn assert_key_is_acyclic(&self, key: Value) {
        debug_assert!(!equality::contains_itself(key), "a key that contains itself entered a dict");
    }

    pub(super) fn op_dict_rest(&mut self, values_only: bool) -> Result<(), anyhow::Error> {
        let named = self.read_next() as usize;
        let start = unsafe { self.stack.top().sub(named) };
        let keys: Vec<DictKey> = unsafe { std::slice::from_raw_parts(start, named) }.iter().map(|k| DictKey(*k)).collect();
        let target = self.stack.peek(named);

        if !matches!(target.kind(), ValueKind::Object(ObjectKind::Dict)) {
            return self.error(format!("A rest pattern needs a dict, but got {}", target.fmt()));
        }

        let entries: Vec<(DictKey, Value)> = unsafe { &*target.as_object().as_dict_ptr() }.entries.iter()
            .filter(|(key, _)| !keys.contains(key))
            .map(|(key, value)| (*key, *value))
            .collect();

        let built = match values_only {
            true => {
                let elements: Vec<Value> = entries.into_iter().map(|(_, value)| value).collect();
                Value::from(self.alloc_array(&elements))
            },
            false => {
                let map: fnv::FnvHashMap<DictKey, Value> = entries.into_iter().collect();
                Value::from(self.alloc(ObjDict::new(map)))
            },
        };
        self.stack.truncate(named + 1);
        self.stack.push(built);
        Ok(())
    }

    pub(super) fn op_push_type(&mut self) -> Result<(), anyhow::Error> {
        let const_idx = self.read_next() as usize;
        let value = self.chunk.constants[const_idx];
        self.push_checked(value)
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

    pub(super) fn op_equal(&mut self) -> Result<(), anyhow::Error> {
        self.push_equality(true)
    }

    pub(super) fn op_not_equal(&mut self) -> Result<(), anyhow::Error> {
        self.push_equality(false)
    }

    fn push_equality(&mut self, equal: bool) -> Result<(), anyhow::Error> {
        let b = self.stack.pop();
        let a = self.stack.pop();
        let answer = self.values_equal(a, b) == equal;
        self.stack.push(Value::from(answer));
        Ok(())
    }

    /// What `a == b` answers. No value nests deeper than the number of objects that exist.
    pub(super) fn values_equal(&self, a: Value, b: Value) -> bool {
        a.value_eq(b, self.gc.object_count())
    }

    unary_op_methods! {
        op_negate  => |v| is_number => -v.as_number();
        op_bit_not => |v| is_number => !(v.as_number() as i64) as f64;
    }

    fn binary_op_number<F: Fn(f64, f64) -> Value>(&mut self, func: F, token: &str) -> Result<(), anyhow::Error> {
        let b = self.stack.pop();
        let a = self.stack.pop();

        if !a.is_number() || !b.is_number() {
            return self.operands_refused(token, a, b);
        }

        self.stack.push(func(a.as_number(), b.as_number()));
        Ok(())
    }

    #[cold]
    #[inline(never)]
    pub(super) fn operands_refused(&self, token: &str, a: Value, b: Value) -> Result<(), anyhow::Error> {
        self.error(format!("Operator '{}' cannot be applied to operands {} and {}", token, a, b))
    }
}

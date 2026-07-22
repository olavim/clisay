//! Tail-call threaded dispatch.

use super::*;

type R = Result<Vec<String>, anyhow::Error>;

/// Read one operand byte, advancing the local cursor.
macro_rules! rb {
    ($ip:ident) => {{ let b = unsafe { *$ip }; $ip = unsafe { $ip.add(1) }; b }}
}

/// Read a `u16` operand, advancing the local cursor.
macro_rules! rs {
    ($ip:ident) => {{
        let lo = unsafe { *$ip }; $ip = unsafe { $ip.add(1) };
        let hi = unsafe { *$ip }; $ip = unsafe { $ip.add(1) };
        as_short!(lo, hi)
    }}
}

/// Push onto the stack top.
macro_rules! push {
    ($top:ident, $v:expr) => {{ let v = $v; unsafe { *$top = v; } $top = unsafe { $top.add(1) }; }}
}

/// Pop from the stack top.
macro_rules! pop {
    ($top:ident) => {{ $top = unsafe { $top.sub(1) }; unsafe { *$top } }}
}

/// Peek `n` slots below the stack top.
macro_rules! peek {
    ($top:ident, $n:expr) => { unsafe { *$top.sub($n + 1) } }
}

/// Reads the opcode at `ip` and tail-calls its handler.
pub(super) fn dispatch(vm: &mut Vm, ip: *const OpCode, top: *mut Value, base: *mut Value) -> R {
    let op = unsafe { *ip };
    let ip = unsafe { ip.add(1) };
    match op {
        opcode::LOAD_LOCAL => become load_local(vm, ip, top, base),
        opcode::STORE_LOCAL => become store_local(vm, ip, top, base),
        opcode::STORE_LOCAL_POP => become store_local_pop(vm, ip, top, base),
        opcode::LOAD_UPVALUE => become load_upvalue(vm, ip, top, base),
        opcode::STORE_UPVALUE => become store_upvalue(vm, ip, top, base),
        opcode::STORE_UPVALUE_POP => become store_upvalue_pop(vm, ip, top, base),
        opcode::PUSH_CONSTANT => become push_constant(vm, ip, top, base),
        opcode::PUSH_NULL => become push_null(vm, ip, top, base),
        opcode::PUSH_TRUE => become push_true(vm, ip, top, base),
        opcode::PUSH_FALSE => become push_false(vm, ip, top, base),
        opcode::POP => become pop_op(vm, ip, top, base),
        opcode::JUMP => become jump(vm, ip, top, base),
        opcode::JUMP_IF_FALSE => become jump_if_false(vm, ip, top, base),
        opcode::JUMP_IF_GE => become jump_if_ge(vm, ip, top, base),
        opcode::JUMP_IF_GT => become jump_if_gt(vm, ip, top, base),
        opcode::JUMP_IF_LE => become jump_if_le(vm, ip, top, base),
        opcode::JUMP_IF_LT => become jump_if_lt(vm, ip, top, base),
        opcode::JUMP_IF_GE_LOCAL_CONST => become jump_if_ge_lc(vm, ip, top, base),
        opcode::JUMP_IF_GT_LOCAL_CONST => become jump_if_gt_lc(vm, ip, top, base),
        opcode::JUMP_IF_LE_LOCAL_CONST => become jump_if_le_lc(vm, ip, top, base),
        opcode::JUMP_IF_LT_LOCAL_CONST => become jump_if_lt_lc(vm, ip, top, base),
        opcode::JUMP_IF_EQ => become jump_if_eq(vm, ip, top, base),
        opcode::JUMP_IF_NEQ => become jump_if_neq(vm, ip, top, base),
        opcode::STORE_LOCAL_ADD_LOCAL_LOCAL => become store_local_add(vm, ip, top, base),
        opcode::ADD_LOCAL_CONST => become add_local_const(vm, ip, top, base),
        opcode::ADD_CONST_LOCAL => become add_const_local(vm, ip, top, base),
        opcode::INC_LOCAL => become inc_local(vm, ip, top, base),
        opcode::DEC_LOCAL => become dec_local(vm, ip, top, base),
        opcode::ADD => become add(vm, ip, top, base),
        opcode::SUB_LOCAL_CONST => become sub_local_const(vm, ip, top, base),
        opcode::SUB_CONST_LOCAL => become sub_const_local(vm, ip, top, base),
        opcode::SUBTRACT => become subtract(vm, ip, top, base),
        opcode::MULTIPLY => become multiply(vm, ip, top, base),
        opcode::DIVIDE => become divide(vm, ip, top, base),
        opcode::CALL => become call(vm, ip, top, base),
        opcode::RETURN => become ret(vm, ip, top, base),
        opcode::HALT => become halt(vm, ip, top, base),
        opcode::NOT => become not(vm, ip, top, base),
        opcode::DUP => become dup(vm, ip, top, base),
        _ => become cold(vm, ip, top, base),
    }
}

/// Less common opcodes.
fn cold(vm: &mut Vm, ip: *const OpCode, top: *mut Value, _base: *mut Value) -> R {
    let op = unsafe { *ip.sub(1) };
    vm.stack.set_top(top);
    vm.ip = ip;
    match op {
        opcode::CONSTRUCT => vm.op_construct()?,
        opcode::THROW => vm.op_throw()?,
        opcode::PUSH_TRY => vm.op_push_try(),
        opcode::POP_TRY => vm.op_pop_try(),
        opcode::JUMP_IF_FALSE_OR_POP => vm.op_jump_if_false_or_pop(),
        opcode::JUMP_IF_TRUE_OR_POP => vm.op_jump_if_true_or_pop(),
        opcode::JUMP_IF_NOT_NULL_OR_POP => vm.op_jump_if_not_null_or_pop(),
        opcode::JUMP_IF_NULL => vm.op_jump_if_null(),
        opcode::JUMP_IF_CLEAN => vm.op_jump_if_clean(),
        opcode::JUMP_IF_BAD => vm.op_jump_if_bad(),
        opcode::JUMP_IF_IS => vm.op_jump_if_is(),
        opcode::ASSERT_NON_NULL => vm.op_assert_non_null()?,
        opcode::BARRIER_GUARD => vm.op_barrier_guard()?,
        opcode::ASSERT_BORROW => vm.op_assert_borrow()?,
        opcode::MARK_BORROW => vm.op_mark_borrow(),
        opcode::RELEASE_BORROW => vm.op_release_borrow(),
        opcode::CLOSE_UPVALUE => vm.op_close_upvalue(),
        opcode::ARRAY => vm.op_array(),
        opcode::DICT => vm.op_dict(),
        opcode::PUSH_CLOSURE => vm.op_push_closure()?,
        opcode::PUSH_TYPE => vm.op_push_type(),
        opcode::LOAD_GLOBAL => vm.op_load_global()?,
        opcode::INVOKE => vm.op_invoke()?,
        opcode::GET_INDEX => vm.op_get_index()?,
        opcode::SET_INDEX => vm.op_set_index()?,
        opcode::GET_INDEX_OR_NULL => vm.op_get_index_or_null(),
        opcode::GET_PROPERTY => vm.op_get_property()?,
        opcode::SET_PROPERTY => vm.op_set_property()?,
        opcode::GET_FIELD => vm.op_get_field()?,
        opcode::SET_FIELD => vm.op_set_field()?,
        opcode::SET_FIELD_POP => vm.op_set_field_pop()?,
        opcode::NEGATE => vm.op_negate()?,
        opcode::LEFT_SHIFT => vm.op_left_shift()?,
        opcode::RIGHT_SHIFT => vm.op_right_shift()?,
        opcode::BIT_AND => vm.op_bit_and()?,
        opcode::BIT_OR => vm.op_bit_or()?,
        opcode::BIT_XOR => vm.op_bit_xor()?,
        opcode::BIT_NOT => vm.op_bit_not()?,
        opcode::EQUAL => vm.op_equal()?,
        opcode::NOT_EQUAL => vm.op_not_equal()?,
        opcode::LESS_THAN => vm.op_less_than()?,
        opcode::LESS_THAN_EQUAL => vm.op_less_than_equal()?,
        opcode::GREATER_THAN => vm.op_greater_than()?,
        opcode::GREATER_THAN_EQUAL => vm.op_greater_than_equal()?,
        opcode::IS => vm.op_is(),
        opcode::HAS_MEMBER => vm.op_has_member(),
        opcode::IS_SHAPED => vm.op_is_shaped(),
        opcode::ARRAY_LEN => vm.op_array_len(),
        opcode::ARRAY_MIDDLE => vm.op_array_middle(),
        _ => unsafe { std::hint::unreachable_unchecked() }
    }
    let top = vm.stack.top();
    let base = unsafe { (*vm.frames.top()).stack_start };
    become dispatch(vm, vm.ip, top, base)
}

fn load_local(vm: &mut Vm, ip: *const OpCode, top: *mut Value, base: *mut Value) -> R {
    let mut ip = ip;
    let mut top = top;
    let idx = rb!(ip) as usize;
    push!(top, unsafe { *base.add(idx) });
    become dispatch(vm, ip, top, base)
}

fn store_local(vm: &mut Vm, ip: *const OpCode, top: *mut Value, base: *mut Value) -> R {
    let mut ip = ip;
    let idx = rb!(ip) as usize;
    let value = peek!(top, 0);
    unsafe { *base.add(idx) = value };
    become dispatch(vm, ip, top, base)
}

fn store_local_pop(vm: &mut Vm, ip: *const OpCode, top: *mut Value, base: *mut Value) -> R {
    let mut ip = ip;
    let mut top = top;
    let idx = rb!(ip) as usize;
    let value = pop!(top);
    unsafe { *base.add(idx) = value };
    become dispatch(vm, ip, top, base)
}

fn load_upvalue(vm: &mut Vm, ip: *const OpCode, top: *mut Value, base: *mut Value) -> R {
    let mut ip = ip;
    let mut top = top;
    let idx = rb!(ip) as usize;
    let upvalue = vm.get_upvalue(idx);
    push!(top, unsafe { *(*upvalue).location });
    become dispatch(vm, ip, top, base)
}

fn store_upvalue(vm: &mut Vm, ip: *const OpCode, top: *mut Value, base: *mut Value) -> R {
    let mut ip = ip;
    let idx = rb!(ip) as usize;
    let value = peek!(top, 0);
    // A captured variable outlives the lending call, so a borrowed value may not be stored into one.
    if value.is_borrowed() {
        vm.stack.set_top(top);
        vm.ip = ip;
        vm.ensure_not_borrowed(value)?;
    }
    let upvalue = vm.get_upvalue(idx);
    unsafe { *(*upvalue).location = value };
    become dispatch(vm, ip, top, base)
}

fn store_upvalue_pop(vm: &mut Vm, ip: *const OpCode, top: *mut Value, base: *mut Value) -> R {
    let mut ip = ip;
    let mut top = top;
    let idx = rb!(ip) as usize;
    let value = pop!(top);
    if value.is_borrowed() {
        vm.stack.set_top(top);
        vm.ip = ip;
        vm.ensure_not_borrowed(value)?;
    }
    let upvalue = vm.get_upvalue(idx);
    unsafe { *(*upvalue).location = value };
    become dispatch(vm, ip, top, base)
}

fn push_constant(vm: &mut Vm, ip: *const OpCode, top: *mut Value, base: *mut Value) -> R {
    let mut ip = ip;
    let mut top = top;
    let idx = rb!(ip) as usize;
    push!(top, vm.chunk.constants[idx]);
    become dispatch(vm, ip, top, base)
}

fn push_null(vm: &mut Vm, ip: *const OpCode, top: *mut Value, base: *mut Value) -> R {
    let mut top = top;
    push!(top, Value::NULL);
    become dispatch(vm, ip, top, base)
}

fn push_true(vm: &mut Vm, ip: *const OpCode, top: *mut Value, base: *mut Value) -> R {
    let mut top = top;
    push!(top, Value::TRUE);
    become dispatch(vm, ip, top, base)
}

fn push_false(vm: &mut Vm, ip: *const OpCode, top: *mut Value, base: *mut Value) -> R {
    let mut top = top;
    push!(top, Value::FALSE);
    become dispatch(vm, ip, top, base)
}

fn pop_op(vm: &mut Vm, ip: *const OpCode, top: *mut Value, base: *mut Value) -> R {
    let top = unsafe { top.sub(1) };
    become dispatch(vm, ip, top, base)
}

fn jump(vm: &mut Vm, ip: *const OpCode, top: *mut Value, base: *mut Value) -> R {
    let lo = unsafe { *ip };
    let hi = unsafe { *ip.add(1) };
    let offset = as_short!(lo, hi) as usize;
    become dispatch(vm, unsafe { vm.chunk.code.as_ptr().add(offset) }, top, base)
}

fn jump_if_false(vm: &mut Vm, ip: *const OpCode, top: *mut Value, base: *mut Value) -> R {
    let mut ip = ip;
    let mut top = top;
    let offset = rs!(ip) as usize;
    let value = pop!(top);
    if value.is_falsy() {
        become dispatch(vm, unsafe { vm.chunk.code.as_ptr().add(offset) }, top, base);
    }
    become dispatch(vm, ip, top, base)
}

macro_rules! cmp_jump_fn {
    ($fn:ident, $op:tt, $token:literal) => {
        fn $fn(vm: &mut Vm, ip: *const OpCode, top: *mut Value, base: *mut Value) -> R {
            let mut ip = ip;
            let mut top = top;
            let offset = rs!(ip) as usize;
            let b = pop!(top);
            let a = pop!(top);
            if !a.is_number() || !b.is_number() {
                vm.stack.set_top(top);
                vm.ip = ip;
                vm.error(format!("Operator '{}' cannot be applied to operands {} and {}", $token, a, b))?;
            } else if a.as_number() $op b.as_number() {
                ip = unsafe { vm.chunk.code.as_ptr().add(offset) };
            }
            become dispatch(vm, ip, top, base)
        }
    }
}

cmp_jump_fn!(jump_if_ge, >=, "<");
cmp_jump_fn!(jump_if_gt, >, "<=");
cmp_jump_fn!(jump_if_le, <=, ">");
cmp_jump_fn!(jump_if_lt, <, ">=");

/// Jump if `local <op> const`
macro_rules! cmp_jump_lc_fn {
    ($fn:ident, $op:tt, $token:literal) => {
        fn $fn(vm: &mut Vm, ip: *const OpCode, top: *mut Value, base: *mut Value) -> R {
            let mut ip = ip;
            let offset = rs!(ip) as usize;
            let a_idx = rb!(ip) as usize;
            let b_idx = rb!(ip) as usize;
            let a = unsafe { *base.add(a_idx) };
            let b = vm.chunk.constants[b_idx];
            if !a.is_number() {
                vm.stack.set_top(top);
                vm.ip = ip;
                vm.error(format!("Operator '{}' cannot be applied to operands {} and {}", $token, a, b))?;
            } else if a.as_number() $op b.as_number() {
                ip = unsafe { vm.chunk.code.as_ptr().add(offset) };
            }
            become dispatch(vm, ip, top, base)
        }
    }
}

cmp_jump_lc_fn!(jump_if_ge_lc, >=, "<");
cmp_jump_lc_fn!(jump_if_gt_lc, >, "<=");
cmp_jump_lc_fn!(jump_if_le_lc, <=, ">");
cmp_jump_lc_fn!(jump_if_lt_lc, <, ">=");

fn jump_if_eq(vm: &mut Vm, ip: *const OpCode, top: *mut Value, base: *mut Value) -> R {
    let mut ip = ip;
    let mut top = top;
    let offset = rs!(ip) as usize;
    let b = pop!(top);
    let a = pop!(top);
    if a.value_eq(b) {
        ip = unsafe { vm.chunk.code.as_ptr().add(offset) };
    }
    become dispatch(vm, ip, top, base)
}

fn jump_if_neq(vm: &mut Vm, ip: *const OpCode, top: *mut Value, base: *mut Value) -> R {
    let mut ip = ip;
    let mut top = top;
    let offset = rs!(ip) as usize;
    let b = pop!(top);
    let a = pop!(top);
    if !a.value_eq(b) {
        ip = unsafe { vm.chunk.code.as_ptr().add(offset) };
    }
    become dispatch(vm, ip, top, base)
}

fn store_local_add(vm: &mut Vm, ip: *const OpCode, top: *mut Value, base: *mut Value) -> R {
    let mut ip = ip;
    let mut top = top;
    let dst = rb!(ip) as usize;
    let a_idx = rb!(ip) as usize;
    let b_idx = rb!(ip) as usize;
    let a = unsafe { *base.add(a_idx) };
    let b = unsafe { *base.add(b_idx) };
    if a.is_number() && b.is_number() {
        unsafe { *base.add(dst) = Value::from(a.as_number() + b.as_number()) };
    } else {
        push!(top, a);
        push!(top, b);
        vm.stack.set_top(top);
        vm.ip = ip;
        vm.op_add()?;
        top = vm.stack.top();
        let result = pop!(top);
        unsafe { *base.add(dst) = result };
    }
    become dispatch(vm, ip, top, base)
}

/// In-place `local = local <+/-> const`.
macro_rules! inc_dec_fn {
    ($fn:ident, $op:tt, $slow:ident) => {
        fn $fn(vm: &mut Vm, ip: *const OpCode, top: *mut Value, base: *mut Value) -> R {
            let mut ip = ip;
            let mut top = top;
            let l = rb!(ip) as usize;
            let c = rb!(ip) as usize;
            let a = unsafe { *base.add(l) };
            let b = vm.chunk.constants[c];
            if a.is_number() && b.is_number() {
                unsafe { *base.add(l) = Value::from(a.as_number() $op b.as_number()) };
            } else {
                push!(top, a);
                push!(top, b);
                vm.stack.set_top(top);
                vm.ip = ip;
                vm.$slow()?;
                top = vm.stack.top();
                let result = pop!(top);
                unsafe { *base.add(l) = result };
            }
            become dispatch(vm, ip, top, base)
        }
    }
}

inc_dec_fn!(inc_local, +, op_add);
inc_dec_fn!(dec_local, -, op_subtract);

/// Fused `local <op> const`.
macro_rules! fused_lc_fn {
    ($fn:ident, $op:tt, $slow:ident) => {
        fn $fn(vm: &mut Vm, ip: *const OpCode, top: *mut Value, base: *mut Value) -> R {
            let mut ip = ip;
            let mut top = top;
            let a_idx = rb!(ip) as usize;
            let b_idx = rb!(ip) as usize;
            let a = unsafe { *base.add(a_idx) };
            let b = vm.chunk.constants[b_idx];
            if a.is_number() && b.is_number() {
                push!(top, Value::from(a.as_number() $op b.as_number()));
            } else {
                push!(top, a);
                push!(top, b);
                vm.stack.set_top(top);
                vm.ip = ip;
                vm.$slow()?;
                top = vm.stack.top();
            }
            become dispatch(vm, ip, top, base)
        }
    }
}

/// Fused `const <op> local`.
macro_rules! fused_cl_fn {
    ($fn:ident, $op:tt, $slow:ident) => {
        fn $fn(vm: &mut Vm, ip: *const OpCode, top: *mut Value, base: *mut Value) -> R {
            let mut ip = ip;
            let mut top = top;
            let a_idx = rb!(ip) as usize;
            let b_idx = rb!(ip) as usize;
            let a = vm.chunk.constants[a_idx];
            let b = unsafe { *base.add(b_idx) };
            if a.is_number() && b.is_number() {
                push!(top, Value::from(a.as_number() $op b.as_number()));
            } else {
                push!(top, a);
                push!(top, b);
                vm.stack.set_top(top);
                vm.ip = ip;
                vm.$slow()?;
                top = vm.stack.top();
            }
            become dispatch(vm, ip, top, base)
        }
    }
}

fused_lc_fn!(add_local_const, +, op_add);
fused_cl_fn!(add_const_local, +, op_add);
fused_lc_fn!(sub_local_const, -, op_subtract);
fused_cl_fn!(sub_const_local, -, op_subtract);

macro_rules! num_binop_fn {
    ($fn:ident, $op:tt, $slow:ident) => {
        fn $fn(vm: &mut Vm, ip: *const OpCode, top: *mut Value, base: *mut Value) -> R {
            let mut top = top;
            let b = peek!(top, 0);
            let a = peek!(top, 1);
            if a.is_number() && b.is_number() {
                top = unsafe { top.sub(2) };
                push!(top, Value::from(a.as_number() $op b.as_number()));
            } else {
                vm.stack.set_top(top);
                vm.ip = ip;
                vm.$slow()?;
                top = vm.stack.top();
            }
            become dispatch(vm, ip, top, base)
        }
    }
}

num_binop_fn!(add, +, op_add);
num_binop_fn!(subtract, -, op_subtract);
num_binop_fn!(multiply, *, op_multiply);
num_binop_fn!(divide, /, op_divide);

#[inline]
fn closure_call(value: Value, arg_count: usize) -> Option<(*mut ObjClosure, usize)> {
    if value.is_callable() {
        let object = value.as_object();
        if object.tag() == objects::TAG_CLOSURE {
            let ptr = object.as_closure_ptr();
            let closure = unsafe { &*ptr };
            if arg_count == closure.arity as usize {
                return Some((ptr, closure.ip_start));
            }
        }
    }
    None
}

fn call(vm: &mut Vm, ip: *const OpCode, top: *mut Value, _base: *mut Value) -> R {
    let mut ip = ip;
    let arg_count = rb!(ip) as usize;
    let value = peek!(top, arg_count);
    let code_base = vm.chunk.code.as_ptr();
    let site = unsafe { ip.offset_from(code_base) } as usize;
    let slot = site & (CALL_CACHE_SIZE - 1);

    // Resolve the callee: a cache hit skips the checks and closure deref.
    let cache = unsafe { *vm.call_cache.get_unchecked(slot) };
    let (closure, ip_start) = if cache.site == site && cache.callee == value {
        (cache.closure, cache.ip_start)
    } else if let Some((closure, ip_start)) = closure_call(value, arg_count) {
        unsafe { *vm.call_cache.get_unchecked_mut(slot) = CallCache { site, callee: value, closure, ip_start } };
        (closure, ip_start)
    } else {
        vm.stack.set_top(top);
        vm.ip = ip;
        vm.call(arg_count, value)?;
        let top = vm.stack.top();
        let base = unsafe { (*vm.frames.top()).stack_start };
        become dispatch(vm, vm.ip, top, base);
    };

    if vm.frames.is_full() {
        vm.stack.set_top(top);
        vm.ip = ip;
        return Err(vm.stack_overflow());
    }

    let stack_start = unsafe { top.sub(arg_count + 1) };
    vm.frames.push(CallFrame { closure, return_ip: ip, stack_start });
    become dispatch(vm, unsafe { code_base.add(ip_start) }, top, stack_start)
}

/// Terminates the program.
fn halt(vm: &mut Vm, _ip: *const OpCode, _top: *mut Value, _base: *mut Value) -> R {
    Ok(std::mem::take(&mut vm.out))
}

fn ret(vm: &mut Vm, ip: *const OpCode, top: *mut Value, _base: *mut Value) -> R {
    // The top-level ends in HALT, so every RETURN has a caller frame to pop.
    if vm.open_upvalues.is_empty() {
        let frame = vm.frames.pop();
        let value = unsafe { *top.sub(1) };
        unsafe { *frame.stack_start = value };
        let top = unsafe { frame.stack_start.add(1) };
        let base = unsafe { (*vm.frames.top()).stack_start };
        become dispatch(vm, frame.return_ip, top, base);
    }

    vm.stack.set_top(top);
    vm.ip = ip;
    vm.op_return()?;
    let top = vm.stack.top();
    let base = unsafe { (*vm.frames.top()).stack_start };
    become dispatch(vm, vm.ip, top, base)
}

fn not(vm: &mut Vm, ip: *const OpCode, top: *mut Value, base: *mut Value) -> R {
    let mut top = top;
    let v = pop!(top);
    push!(top, Value::from(v.is_falsy()));
    become dispatch(vm, ip, top, base)
}

fn dup(vm: &mut Vm, ip: *const OpCode, top: *mut Value, base: *mut Value) -> R {
    let mut top = top;
    push!(top, peek!(top, 0));
    become dispatch(vm, ip, top, base)
}

//! Tail-call threaded dispatch. Each op has a handler of its own, which ends by jumping through
//! `HANDLERS` to the next op.

// A handler is named after its opcode, as `op_LOAD_LOCAL`.
#![allow(non_snake_case)]

use super::calls::return_arity_matches;
use crate::core::equality;
use super::*;

type R = Result<(), anyhow::Error>;

macro_rules! read_u8 {
    ($ip:ident) => {{ let b = unsafe { *$ip }; $ip = unsafe { $ip.add(1) }; b }}
}

macro_rules! read_u16 {
    ($ip:ident) => {{
        let lo = unsafe { *$ip }; $ip = unsafe { $ip.add(1) };
        let hi = unsafe { *$ip }; $ip = unsafe { $ip.add(1) };
        as_short!(lo, hi)
    }}
}

/// Peek `n` slots below the stack top.
macro_rules! peek {
    ($top:ident, $n:expr) => { unsafe { *$top.sub($n + 1) } }
}

macro_rules! pop {
    ($top:ident) => {{
        $top = unsafe { $top.sub(1) };
        unsafe { *$top }
    }}
}

macro_rules! push {
    ($vm:ident, $ip:ident, $top:ident, $stack_start:ident, $v:expr) => {{
        let v = $v;
        if $top >= $vm.stack.end() { become stack_full($vm, $ip, $top, $stack_start); }
        unsafe { *$top = v; }
        $top = unsafe { $top.add(1) };
    }}
}

macro_rules! constant {
    ($vm:ident, $idx:expr) => {{
        let idx = $idx;
        debug_assert!(idx < $vm.chunk.constants.len(), "constant {idx} is outside the pool");
        unsafe { *$vm.chunk.constants.get_unchecked(idx) }
    }}
}

macro_rules! next {
    ($vm:expr, $ip:expr, $top:expr, $stack_start:expr) => {{
        let ip: *const OpCode = $ip;
        let op = unsafe { *ip } as usize;
        become OP_HANDLERS[op]($vm, unsafe { ip.add(1) }, $top, $stack_start)
    }}
}

/// Continues at code `offset` when `cond` holds, and at `ip` otherwise.
macro_rules! jump_if {
    ($vm:expr, $cond:expr, $offset:expr, $ip:expr, $top:expr, $stack_start:expr) => {{
        // Branch into different code based on the condition. If we instead just picked
        // the target ip based on the condition and then continued execution, LLVM would
        // generate a conditional move instead of a branch. Waiting on the comparison
        // turns out to be slower than waiting on the branch predictor.
        if $cond {
            // `next!` uses `become` so the code won't continue below this if.
            next!($vm, unsafe { $vm.chunk.code.as_ptr().add($offset) }, $top, $stack_start);
        }
        next!($vm, $ip, $top, $stack_start)
    }}
}

type Handler = fn(&mut Vm, *const OpCode, *mut Value, *mut Value) -> R;

/// Runs the code.
#[inline(never)]
pub(super) fn dispatch(vm: &mut Vm, ip: *const OpCode, top: *mut Value, stack_start: *mut Value) -> R {
    next!(vm, ip, top, stack_start)
}

macro_rules! vm_op {
    ($fn:ident, $vm:ident => $body:expr) => {
        #[inline(never)]
        fn $fn($vm: &mut Vm, ip: *const OpCode, top: *mut Value, _stack_start: *mut Value) -> R {
            $vm.stack.set_top(top);
            $vm.ip = ip;
            $body;
            let top = $vm.stack.top();
            let stack_start = unsafe { (*$vm.frames.top()).stack_start };
            next!($vm, $vm.ip, top, stack_start)
        }
    }
}

/// Lists every op's handler. A handler is named after its op, so `LOAD_LOCAL` runs in
/// `op_LOAD_LOCAL`. A `vm_backed` op's handler is generated.
macro_rules! handlers {
    (
        hot { $( $hot_op:ident, )* }
        vm_backed($vm:ident) { $( $vm_op:ident => $body:expr, )* }
    ) => {
        $( vm_op!(${concat(op_, $vm_op)}, $vm => $body); )*

        const OP_HANDLER_ROWS: &[(OpCode, Handler)] = &[
            $( (opcode::$hot_op, ${concat(op_, $hot_op)} as Handler), )*
            $( (opcode::$vm_op, ${concat(op_, $vm_op)} as Handler), )*
        ];
    }
}

handlers! {
    hot {
        PUSH_CONSTANT,
        PUSH_NULL,
        PUSH_TRUE,
        PUSH_FALSE,
        POP,
        DISCARD_CHECKED,
        DUP,
        NOT,

        LOAD_LOCAL,
        LOAD_LOCAL_FOR_WRITE,
        STORE_LOCAL,
        STORE_LOCAL_FRESH,
        STORE_LOCAL_FRESH_POP,
        STORE_LOCAL_POP,
        LOAD_CAPTURE,
        SHARE_LOCAL,

        ADD,
        SUBTRACT,
        MULTIPLY,
        DIVIDE,
        ADD_LOCAL_CONST,
        ADD_CONST_LOCAL,
        SUB_LOCAL_CONST,
        SUB_CONST_LOCAL,
        INC_LOCAL,
        DEC_LOCAL,
        STORE_LOCAL_ADD_LOCAL_LOCAL,

        JUMP,
        JUMP_IF_FALSE,
        JUMP_IF_GE,
        JUMP_IF_GT,
        JUMP_IF_LE,
        JUMP_IF_LT,
        JUMP_IF_GE_LOCAL_CONST,
        JUMP_IF_GT_LOCAL_CONST,
        JUMP_IF_LE_LOCAL_CONST,
        JUMP_IF_LT_LOCAL_CONST,
        JUMP_IF_EQ,
        JUMP_IF_NEQ,

        CALL,
        RETURN,
        RETURN_SHARED,
        HALT,
    }
    vm_backed(vm) {
        CONSTRUCT => vm.op_construct()?,
        TAIL_CALL => vm.op_tail_call()?,
        RETURN_FAC => vm.op_return_factory()?,
        THROW => vm.op_throw()?,
        JUMP_IF_FALSE_OR_POP => vm.op_jump_if_false_or_pop(),
        JUMP_IF_TRUE_OR_POP => vm.op_jump_if_true_or_pop(),
        JUMP_IF_CLEAN_OR_POP => vm.op_jump_if_clean_or_pop(),
        JUMP_IF_CLEAN => vm.op_jump_if_clean(),
        JUMP_IF_BAD => vm.op_jump_if_bad(),
        JUMP_IF_IS => vm.op_jump_if_is(),
        ASSERT_NON_NULL => vm.op_assert_non_null()?,
        DUP2 => vm.op_dup2(),
        BARRIER_GUARD => vm.op_barrier_guard()?,
        POP_SCOPE => vm.op_pop_scope(),
        PUSH_SLOT_ANCHOR => vm.op_push_slot_anchor(),
        FORM_ANCHOR_PATH => vm.op_form_anchor_path()?,
        LOAD_ANCHOR => vm.op_load_anchor()?,
        STORE_ANCHOR => vm.op_store_anchor()?,
        STORE_TEMP_POP => vm.op_store_temp_pop(),
        ARRAY => vm.op_array()?,
        DICT => vm.op_dict()?,
        BUILD_CLOSURE => vm.build_closure(true)?,
        PUSH_TYPE => vm.op_push_type()?,
        BUILD_TYPE => vm.build_type(true)?,
        BUILD_CLOSURE_UNBOUND => vm.build_closure(false)?,
        BUILD_TYPE_UNBOUND => vm.build_type(false)?,
        PUSH_UNASSIGNED => vm.push_checked(Value::unassigned())?,
        LOAD_GLOBAL => vm.op_load_global()?,
        INVOKE => vm.op_invoke()?,
        INVOKE_THIS => vm.op_invoke_this()?,
        GET_INDEX => vm.op_get_index()?,
        LOAD_STEP_FOR_WRITE => vm.op_load_step_for_write()?,
        LOAD_ANCHOR_FOR_WRITE => vm.op_load_anchor_for_write()?,
        BIND_CLOSURE_CAPTURES => vm.op_bind_captures(),
        BIND_TYPE_CAPTURES => vm.op_bind_type_captures(),
        COPY_OBJECT => vm.op_copy_object(),
        GET_INDEX_OR_NULL => vm.op_get_index_or_null()?,
        GET_PROPERTY => vm.op_get_property()?,
        GET_MEMBER => vm.op_get_member()?,
        SET_MEMBER => vm.op_set_member()?,
        CHECK_ANCHOR_ROOT => vm.op_check_anchor_root()?,
        RECORD_ANCHOR_ROOT => vm.op_record_anchor_root()?,
        COPY_ANCHOR_OUT => vm.op_copy_anchor_out(),
        COPY_ANCHOR_IN => vm.op_copy_anchor_in()?,
        GET_FIELD => vm.op_get_field()?,
        SET_FIELD => vm.op_set_field()?,
        SET_FIELD_POP => vm.op_set_field_pop()?,
        NEGATE => vm.op_negate()?,
        LEFT_SHIFT => vm.op_left_shift()?,
        RIGHT_SHIFT => vm.op_right_shift()?,
        BIT_AND => vm.op_bit_and()?,
        BIT_OR => vm.op_bit_or()?,
        BIT_XOR => vm.op_bit_xor()?,
        BIT_NOT => vm.op_bit_not()?,
        EQUAL => vm.op_equal()?,
        NOT_EQUAL => vm.op_not_equal()?,
        LESS_THAN => vm.op_less_than()?,
        LESS_THAN_EQUAL => vm.op_less_than_equal()?,
        GREATER_THAN => vm.op_greater_than()?,
        GREATER_THAN_EQUAL => vm.op_greater_than_equal()?,
        IS => vm.op_is(),
        HAS_MEMBER => vm.op_has_member(),
        MEMBER_ADMITS => vm.op_member_admits(),
        IS_SHAPED => vm.op_is_shaped(),
        IS_DICT => vm.op_is_dict(),
        LOAD_REF => vm.op_load_ref()?,
        LOAD_REF_FOR_WRITE => vm.op_load_ref_for_write()?,
        STORE_REF => vm.op_store_ref::<false>()?,
        STORE_REF_POP => vm.op_store_ref::<true>()?,
        SET_INDEX => vm.op_set_index()?,
        SET_PROPERTY => vm.op_set_property()?,
        DICT_REST => vm.op_dict_rest(false)?,
        DICT_REST_VALUES => vm.op_dict_rest(true)?,
        ARRAY_LEN => vm.op_array_len(),
        ARRAY_MIDDLE => vm.op_array_middle(),
        ARRAY_ELEM => vm.op_array_elem(),
    }
}

const fn handler_table(rows: &[(OpCode, Handler)]) -> [Handler; 256] {
    let mut table = [unknown_op as Handler; 256];
    let mut filled = [false; 256];

    let mut i = 0;
    while i < rows.len() {
        let (op, handler) = rows[i];
        assert!(!filled[op as usize], "an opcode has two handlers");
        table[op as usize] = handler;
        filled[op as usize] = true;
        i += 1;
    }

    let mut op = 0;
    while op < opcode::COUNT {
        assert!(filled[op], "an opcode should have a handler");
        op += 1;
    }

    table
}

/// Each opcode's handler.
static OP_HANDLERS: [Handler; 256] = handler_table(OP_HANDLER_ROWS);

/// Fills the table slots of opcodes that do not exist.
fn unknown_op(_vm: &mut Vm, ip: *const OpCode, _top: *mut Value, _stack_start: *mut Value) -> R {
    unreachable!("no handler for opcode {}", unsafe { *ip.sub(1) })
}

fn op_PUSH_CONSTANT(vm: &mut Vm, ip: *const OpCode, top: *mut Value, stack_start: *mut Value) -> R {
    let mut ip = ip;
    let mut top = top;
    let idx = read_u8!(ip) as usize;
    push!(vm, ip, top, stack_start, constant!(vm, idx));
    next!(vm, ip, top, stack_start)
}

fn op_PUSH_NULL(vm: &mut Vm, ip: *const OpCode, top: *mut Value, stack_start: *mut Value) -> R {
    let mut top = top;
    push!(vm, ip, top, stack_start, Value::NULL);
    next!(vm, ip, top, stack_start)
}

fn op_PUSH_TRUE(vm: &mut Vm, ip: *const OpCode, top: *mut Value, stack_start: *mut Value) -> R {
    let mut top = top;
    push!(vm, ip, top, stack_start, Value::TRUE);
    next!(vm, ip, top, stack_start)
}

fn op_PUSH_FALSE(vm: &mut Vm, ip: *const OpCode, top: *mut Value, stack_start: *mut Value) -> R {
    let mut top = top;
    push!(vm, ip, top, stack_start, Value::FALSE);
    next!(vm, ip, top, stack_start)
}

fn op_POP(vm: &mut Vm, ip: *const OpCode, top: *mut Value, stack_start: *mut Value) -> R {
    let top = unsafe { top.sub(1) };
    next!(vm, ip, top, stack_start)
}

fn op_DISCARD_CHECKED(vm: &mut Vm, ip: *const OpCode, top: *mut Value, stack_start: *mut Value) -> R {
    if objects::must_be_used(peek!(top, 0)) || vm.counts_forced_checks() {
        become discard_checked_slow(vm, ip, top, stack_start);
    }
    let top = unsafe { top.sub(1) };
    next!(vm, ip, top, stack_start)
}

fn op_DUP(vm: &mut Vm, ip: *const OpCode, top: *mut Value, stack_start: *mut Value) -> R {
    let mut top = top;
    push!(vm, ip, top, stack_start, peek!(top, 0));
    next!(vm, ip, top, stack_start)
}

fn op_NOT(vm: &mut Vm, ip: *const OpCode, top: *mut Value, stack_start: *mut Value) -> R {
    let v = peek!(top, 0);
    unsafe { *top.sub(1) = Value::from(v.is_falsy()) };
    next!(vm, ip, top, stack_start)
}

fn op_LOAD_LOCAL(vm: &mut Vm, ip: *const OpCode, top: *mut Value, stack_start: *mut Value) -> R {
    let mut ip = ip;
    let mut top = top;
    let idx = read_u8!(ip) as usize;
    let from = unsafe { stack_start.add(idx) };
    debug_assert!(!unsafe { *from }.is_unassigned(), "LOAD_LOCAL read an unassigned slot the check pass should have refused");
    push!(vm, ip, top, stack_start, unsafe { *from });
    next!(vm, ip, top, stack_start)
}

fn op_LOAD_LOCAL_FOR_WRITE(vm: &mut Vm, ip: *const OpCode, top: *mut Value, stack_start: *mut Value) -> R {
    let start = ip;
    let mut ip = ip;
    let mut top = top;
    let idx = read_u8!(ip) as usize;
    let value = unsafe { *stack_start.add(idx) };
    debug_assert!(!value.is_unassigned(), "LOAD_LOCAL_FOR_WRITE read an unassigned slot the check pass should have refused");
    if objects::is_shared(value) {
        become load_local_forking(vm, start, top, stack_start);
    }
    push!(vm, ip, top, stack_start, value);
    next!(vm, ip, top, stack_start)
}

macro_rules! store_local_fn {
    ($fn:ident, $pop:literal, $fresh:literal) => {
        fn $fn(vm: &mut Vm, ip: *const OpCode, top: *mut Value, stack_start: *mut Value) -> R {
            let value = peek!(top, 0);
            if objects::needs_accepts_check(value) || !$fresh && value.is_object() {
                become store_local_checked::<$pop, $fresh>(vm, ip, top, stack_start);
            }
            unsafe { *stack_start.add(*ip as usize) = value };
            next!(vm, unsafe { ip.add(1) }, unsafe { top.sub($pop as usize) }, stack_start)
        }
    }
}

store_local_fn!(op_STORE_LOCAL, false, false);
store_local_fn!(op_STORE_LOCAL_FRESH, false, true);
store_local_fn!(op_STORE_LOCAL_FRESH_POP, true, true);
store_local_fn!(op_STORE_LOCAL_POP, true, false);

fn op_LOAD_CAPTURE(vm: &mut Vm, ip: *const OpCode, top: *mut Value, stack_start: *mut Value) -> R {
    let mut ip = ip;
    let mut top = top;
    let idx = read_u8!(ip) as usize;
    debug_assert!(!vm.capture(idx).is_unassigned(), "LOAD_CAPTURE read an unbound capture the check pass should have refused");
    push!(vm, ip, top, stack_start, vm.capture(idx));
    next!(vm, ip, top, stack_start)
}

fn op_SHARE_LOCAL(vm: &mut Vm, ip: *const OpCode, top: *mut Value, stack_start: *mut Value) -> R {
    let start = ip;
    let mut ip = ip;
    let idx = read_u8!(ip) as usize;
    if unsafe { *stack_start.add(idx) }.is_object() {
        become share_local_object(vm, start, top, stack_start);
    }
    next!(vm, ip, top, stack_start)
}

macro_rules! num_binop_fn {
    ($fn:ident, $op:tt, $vm_op:ident) => {
        fn $fn(vm: &mut Vm, ip: *const OpCode, top: *mut Value, stack_start: *mut Value) -> R {
            let b = peek!(top, 0);
            let a = peek!(top, 1);
            if !a.is_number() || !b.is_number() {
                become ${concat($fn, _slow)}(vm, ip, top, stack_start);
            }
            // Two operands come off and one result goes on, so the stack cannot overflow.
            let top = unsafe { top.sub(1) };
            unsafe { *top.sub(1) = Value::from(a.as_number() $op b.as_number()) };
            next!(vm, ip, top, stack_start)
        }

        // A string `+` takes this path, so it stays apart from the shared `arith_slow`.
        vm_op!(${concat($fn, _slow)}, vm => vm.$vm_op()?);
    }
}

num_binop_fn!(op_ADD, +, op_add);
num_binop_fn!(op_SUBTRACT, -, op_subtract);
num_binop_fn!(op_MULTIPLY, *, op_multiply);
num_binop_fn!(op_DIVIDE, /, op_divide);

/// Fused `local <op> const`.
macro_rules! fused_lc_fn {
    ($fn:ident, $op:tt) => {
        fn $fn(vm: &mut Vm, ip: *const OpCode, top: *mut Value, stack_start: *mut Value) -> R {
            let start = ip;
            let mut ip = ip;
            let mut top = top;
            let a_idx = read_u8!(ip) as usize;
            let b_idx = read_u8!(ip) as usize;
            let a = unsafe { *stack_start.add(a_idx) };
            let b = constant!(vm, b_idx);
            if !a.is_number() || !b.is_number() {
                become arith_slow(vm, start, top, stack_start);
            }
            push!(vm, ip, top, stack_start, Value::from(a.as_number() $op b.as_number()));
            next!(vm, ip, top, stack_start)
        }
    }
}

/// Fused `const <op> local`.
macro_rules! fused_cl_fn {
    ($fn:ident, $op:tt) => {
        fn $fn(vm: &mut Vm, ip: *const OpCode, top: *mut Value, stack_start: *mut Value) -> R {
            let start = ip;
            let mut ip = ip;
            let mut top = top;
            let a_idx = read_u8!(ip) as usize;
            let b_idx = read_u8!(ip) as usize;
            let a = constant!(vm, a_idx);
            let b = unsafe { *stack_start.add(b_idx) };
            if !a.is_number() || !b.is_number() {
                become arith_slow(vm, start, top, stack_start);
            }
            push!(vm, ip, top, stack_start, Value::from(a.as_number() $op b.as_number()));
            next!(vm, ip, top, stack_start)
        }
    }
}

fused_lc_fn!(op_ADD_LOCAL_CONST, +);
fused_cl_fn!(op_ADD_CONST_LOCAL, +);
fused_lc_fn!(op_SUB_LOCAL_CONST, -);
fused_cl_fn!(op_SUB_CONST_LOCAL, -);

/// In-place `local = local <+/-> const`.
macro_rules! inc_dec_fn {
    ($fn:ident, $op:tt) => {
        fn $fn(vm: &mut Vm, ip: *const OpCode, top: *mut Value, stack_start: *mut Value) -> R {
            let start = ip;
            let mut ip = ip;
            let l = read_u8!(ip) as usize;
            let c = read_u8!(ip) as usize;
            let a = unsafe { *stack_start.add(l) };
            let b = constant!(vm, c);
            if !a.is_number() || !b.is_number() {
                become arith_slow(vm, start, top, stack_start);
            }
            unsafe { *stack_start.add(l) = Value::from(a.as_number() $op b.as_number()) };
            next!(vm, ip, top, stack_start)
        }
    }
}

inc_dec_fn!(op_INC_LOCAL, +);
inc_dec_fn!(op_DEC_LOCAL, -);

fn op_STORE_LOCAL_ADD_LOCAL_LOCAL(vm: &mut Vm, ip: *const OpCode, top: *mut Value, stack_start: *mut Value) -> R {
    let start = ip;
    let mut ip = ip;
    let dst = read_u8!(ip) as usize;
    let a_idx = read_u8!(ip) as usize;
    let b_idx = read_u8!(ip) as usize;
    let a = unsafe { *stack_start.add(a_idx) };
    let b = unsafe { *stack_start.add(b_idx) };
    if !a.is_number() || !b.is_number() {
        become arith_slow(vm, start, top, stack_start);
    }
    unsafe { *stack_start.add(dst) = Value::from(a.as_number() + b.as_number()) };
    next!(vm, ip, top, stack_start)
}

fn op_JUMP(vm: &mut Vm, ip: *const OpCode, top: *mut Value, stack_start: *mut Value) -> R {
    let lo = unsafe { *ip };
    let hi = unsafe { *ip.add(1) };
    let offset = as_short!(lo, hi) as usize;
    next!(vm, unsafe { vm.chunk.code.as_ptr().add(offset) }, top, stack_start)
}

fn op_JUMP_IF_FALSE(vm: &mut Vm, ip: *const OpCode, top: *mut Value, stack_start: *mut Value) -> R {
    let mut ip = ip;
    let mut top = top;
    let offset = read_u16!(ip) as usize;
    let value = pop!(top);
    jump_if!(vm, value.is_falsy(), offset, ip, top, stack_start)
}

macro_rules! cmp_jump_fn {
    ($fn:ident, $op:tt) => {
        fn $fn(vm: &mut Vm, ip: *const OpCode, top: *mut Value, stack_start: *mut Value) -> R {
            let start = ip;
            let mut ip = ip;
            let offset = read_u16!(ip) as usize;
            let b = peek!(top, 0);
            let a = peek!(top, 1);
            if !a.is_number() || !b.is_number() {
                become compare_error(vm, start, top, stack_start);
            }
            jump_if!(vm, a.as_number() $op b.as_number(), offset, ip, unsafe { top.sub(2) }, stack_start)
        }
    }
}

cmp_jump_fn!(op_JUMP_IF_GE, >=);
cmp_jump_fn!(op_JUMP_IF_GT, >);
cmp_jump_fn!(op_JUMP_IF_LE, <=);
cmp_jump_fn!(op_JUMP_IF_LT, <);

/// Jump if `local <op> const`
macro_rules! cmp_jump_lc_fn {
    ($fn:ident, $op:tt) => {
        fn $fn(vm: &mut Vm, ip: *const OpCode, top: *mut Value, stack_start: *mut Value) -> R {
            let start = ip;
            let mut ip = ip;
            let offset = read_u16!(ip) as usize;
            let a_idx = read_u8!(ip) as usize;
            let b_idx = read_u8!(ip) as usize;
            let a = unsafe { *stack_start.add(a_idx) };
            let b = constant!(vm, b_idx);
            if !a.is_number() {
                become compare_error(vm, start, top, stack_start);
            }
            jump_if!(vm, a.as_number() $op b.as_number(), offset, ip, top, stack_start)
        }
    }
}

cmp_jump_lc_fn!(op_JUMP_IF_GE_LOCAL_CONST, >=);
cmp_jump_lc_fn!(op_JUMP_IF_GT_LOCAL_CONST, >);
cmp_jump_lc_fn!(op_JUMP_IF_LE_LOCAL_CONST, <=);
cmp_jump_lc_fn!(op_JUMP_IF_LT_LOCAL_CONST, <);

macro_rules! eq_jump_fn {
    ($fn:ident, $jumps_when:expr) => {
        fn $fn(vm: &mut Vm, ip: *const OpCode, top: *mut Value, stack_start: *mut Value) -> R {
            let start = ip;
            let mut ip = ip;
            let offset = read_u16!(ip) as usize;
            let b = peek!(top, 0);
            let a = peek!(top, 1);
            let Some(equal) = equality::quick_eq(a, b) else {
                become eq_jump_deep(vm, start, top, stack_start);
            };
            jump_if!(vm, equal == $jumps_when, offset, ip, unsafe { top.sub(2) }, stack_start)
        }
    }
}

eq_jump_fn!(op_JUMP_IF_EQ, true);
eq_jump_fn!(op_JUMP_IF_NEQ, false);

fn op_CALL(vm: &mut Vm, ip: *const OpCode, top: *mut Value, stack_start: *mut Value) -> R {
    let start = ip;
    let mut ip = ip;
    let operand = read_u8!(ip);
    let arg_count = read_u8!(ip) as usize;

    if operand & ir::CALL_ARGS_SETTLED == 0 && arg_count != 0 {
        become call_slow(vm, start, top, stack_start);
    }

    let value = peek!(top, arg_count);
    let code_base = vm.chunk.code.as_ptr();
    let site = unsafe { ip.offset_from(code_base) } as usize;
    let cache = unsafe { *vm.call_cache.get_unchecked(site & (CALL_CACHE_SIZE - 1)) };

    if cache.site != site || cache.callee != value || vm.frames.is_full() {
        become call_slow(vm, start, top, stack_start);
    }

    let callee_stack_start = unsafe { top.sub(arg_count + 1) };
    let generation = vm.take_frame_generation();
    vm.frames.push(CallFrame::new(cache.closure, ip, callee_stack_start, generation));
    next!(vm, unsafe { code_base.add(cache.ip_start) }, top, callee_stack_start)
}

fn op_RETURN(vm: &mut Vm, ip: *const OpCode, top: *mut Value, stack_start: *mut Value) -> R {
    if !vm.tail_breadcrumbs.is_empty() {
        become ret_releasing(vm, ip, top, stack_start);
    }

    // The top-level ends in HALT, so every RETURN has a caller frame to pop.
    let frame = vm.frames.pop();
    let (ip, top, caller_stack_start) = return_to_caller(vm, frame, top);
    next!(vm, ip, top, caller_stack_start)
}

fn op_RETURN_SHARED(vm: &mut Vm, ip: *const OpCode, top: *mut Value, stack_start: *mut Value) -> R {
    if unsafe { *top.sub(1) }.is_object() {
        become ret_shared_object(vm, ip, top, stack_start);
    }
    become op_RETURN(vm, ip, top, stack_start)
}

/// Terminates the program.
#[cfg_attr(not(debug_assertions), allow(unused_variables))]
fn op_HALT(vm: &mut Vm, _ip: *const OpCode, _top: *mut Value, _stack_start: *mut Value) -> R {
    #[cfg(debug_assertions)]
    vm.report_forks();
    Ok(())
}

#[cold]
#[inline(never)]
fn discard_checked_slow(vm: &mut Vm, ip: *const OpCode, top: *mut Value, stack_start: *mut Value) -> R {
    vm.stack.set_top(top);
    vm.ip = ip;
    let forced = vm.at_forced_check();
    let value = peek!(top, 0);
    if objects::must_be_used(value) {
        if forced {
            return vm.refuted_elision_error("a result proven safe to discard has to be used");
        }
        let ty = unsafe { &*(*value.as_object().as_instance_ptr()).ty };
        return vm.error_help(format!("a discarded `{}` has to be used", unsafe { &(*ty.name).value }),
            "use it, or discard it on purpose with `say _ = ...`");
    }
    let top = unsafe { top.sub(1) };
    next!(vm, ip, top, stack_start)
}

#[cold]
#[inline(never)]
fn stack_full(vm: &mut Vm, ip: *const OpCode, top: *mut Value, _stack_start: *mut Value) -> R {
    vm.stack.set_top(top);
    vm.ip = ip;
    Err(vm.stack_overflow())
}

/// A write that reaches a shared value forks it first, so the write lands in a copy.
#[inline(never)]
fn load_local_forking(vm: &mut Vm, ip: *const OpCode, top: *mut Value, stack_start: *mut Value) -> R {
    let mut ip = ip;
    let mut top = top;
    let idx = read_u8!(ip) as usize;
    let from = unsafe { stack_start.add(idx) };
    vm.stack.set_top(top);
    vm.ip = ip;
    let forked = vm.fork(unsafe { *from });
    unsafe { *from = forked };
    push!(vm, ip, top, stack_start, forked);
    next!(vm, ip, top, stack_start)
}

/// A local store whose value has to be checked against the slot.
#[inline(never)]
fn store_local_checked<const POP: bool, const FRESH: bool>(vm: &mut Vm, ip: *const OpCode, top: *mut Value, stack_start: *mut Value) -> R {
    let mut ip = ip;
    let mut top = top;
    let idx = read_u8!(ip) as usize;

    let value = match POP {
        true => pop!(top),
        false => peek!(top, 0),
    };

    match FRESH {
        true => {
            vm.check_slot_accepts(idx as u8, ip, top, value)?;
            unsafe { *stack_start.add(idx) = value };
        },
        false => {
            let accepted = vm.accept_slot_write(stack_start, idx as u8, ip, top, value)?;
            vm.write_slot(accepted);
        },
    }

    next!(vm, ip, top, stack_start)
}

#[inline(never)]
fn share_local_object(vm: &mut Vm, ip: *const OpCode, top: *mut Value, stack_start: *mut Value) -> R {
    let mut ip = ip;
    let idx = read_u8!(ip) as usize;
    vm.share_in_place(unsafe { stack_start.add(idx) });
    next!(vm, ip, top, stack_start)
}

/// The arithmetic handlers' path for operands that are not both numbers.
#[cold]
#[inline(never)]
fn arith_slow(vm: &mut Vm, ip: *const OpCode, top: *mut Value, stack_start: *mut Value) -> R {
    let op = unsafe { *ip.sub(1) };
    let mut ip = ip;
    let mut top = top;

    // The local the result goes to, when it doesn't stay on the stack.
    let dst = match op {
        opcode::INC_LOCAL | opcode::DEC_LOCAL => {
            let l = read_u8!(ip) as usize;
            let c = read_u8!(ip) as usize;
            push!(vm, ip, top, stack_start, unsafe { *stack_start.add(l) });
            push!(vm, ip, top, stack_start, vm.chunk.constants[c]);
            Some(l)
        },
        opcode::ADD_LOCAL_CONST | opcode::SUB_LOCAL_CONST => {
            let (a_idx, b_idx) = (read_u8!(ip) as usize, read_u8!(ip) as usize);
            let a = unsafe { *stack_start.add(a_idx) };
            let b = vm.chunk.constants[b_idx];
            push!(vm, ip, top, stack_start, a);
            push!(vm, ip, top, stack_start, b);
            None
        },
        opcode::ADD_CONST_LOCAL | opcode::SUB_CONST_LOCAL => {
            let (a_idx, b_idx) = (read_u8!(ip) as usize, read_u8!(ip) as usize);
            let a = vm.chunk.constants[a_idx];
            let b = unsafe { *stack_start.add(b_idx) };
            push!(vm, ip, top, stack_start, a);
            push!(vm, ip, top, stack_start, b);
            None
        },
        opcode::STORE_LOCAL_ADD_LOCAL_LOCAL => {
            let (out, a_idx, b_idx) = (read_u8!(ip) as usize, read_u8!(ip) as usize, read_u8!(ip) as usize);
            let a = unsafe { *stack_start.add(a_idx) };
            let b = unsafe { *stack_start.add(b_idx) };
            push!(vm, ip, top, stack_start, a);
            push!(vm, ip, top, stack_start, b);
            Some(out)
        },
        _ => unreachable!("{} has a slow path of its own", opcode::name(op)),
    };

    vm.stack.set_top(top);
    vm.ip = ip;

    match op {
        opcode::DEC_LOCAL | opcode::SUB_LOCAL_CONST | opcode::SUB_CONST_LOCAL => vm.op_subtract()?,
        _ => vm.op_add()?,
    }

    let mut top = vm.stack.top();

    if let Some(l) = dst {
        let result = pop!(top);
        unsafe { *stack_start.add(l) = result };
    }

    next!(vm, ip, top, stack_start)
}

#[cold]
#[inline(never)]
fn compare_error(vm: &mut Vm, ip: *const OpCode, top: *mut Value, stack_start: *mut Value) -> R {
    let op = unsafe { *ip.sub(1) };
    let mut ip = ip;
    let mut top = top;
    let _offset = read_u16!(ip);

    let (a, b) = match op {
        opcode::JUMP_IF_GE_LOCAL_CONST | opcode::JUMP_IF_GT_LOCAL_CONST
        | opcode::JUMP_IF_LE_LOCAL_CONST | opcode::JUMP_IF_LT_LOCAL_CONST => {
            let (a_idx, b_idx) = (read_u8!(ip) as usize, read_u8!(ip) as usize);
            (unsafe { *stack_start.add(a_idx) }, vm.chunk.constants[b_idx])
        },
        _ => {
            let b = pop!(top);
            (pop!(top), b)
        },
    };

    // A jump leaves a loop or a branch when its condition fails, so the source wrote the opposite.
    let token = match op {
        opcode::JUMP_IF_GE | opcode::JUMP_IF_GE_LOCAL_CONST => "<",
        opcode::JUMP_IF_GT | opcode::JUMP_IF_GT_LOCAL_CONST => "<=",
        opcode::JUMP_IF_LE | opcode::JUMP_IF_LE_LOCAL_CONST => ">",
        _ => ">=",
    };

    vm.stack.set_top(top);
    vm.ip = ip;
    vm.operands_refused(token, a, b)
}

#[inline(never)]
fn eq_jump_deep(vm: &mut Vm, ip: *const OpCode, top: *mut Value, stack_start: *mut Value) -> R {
    let jumps_when = unsafe { *ip.sub(1) } == opcode::JUMP_IF_EQ;
    let mut ip = ip;
    let mut top = top;
    let offset = read_u16!(ip) as usize;
    let b = pop!(top);
    let a = pop!(top);
    jump_if!(vm, vm.values_equal(a, b) == jumps_when, offset, ip, top, stack_start)
}

#[inline(never)]
fn call_slow(vm: &mut Vm, ip: *const OpCode, top: *mut Value, stack_start: *mut Value) -> R {
    let mut ip = ip;
    let operand = read_u8!(ip);
    let arg_count = read_u8!(ip) as usize;
    let value = peek!(top, arg_count);
    let code_base = vm.chunk.code.as_ptr();
    let site = unsafe { ip.offset_from(code_base) } as usize;
    let slot = site & (CALL_CACHE_SIZE - 1);

    // Resolve the callee: a cache hit skips the checks and closure deref.
    let cache = unsafe { *vm.call_cache.get_unchecked(slot) };
    let (closure, ip_start) = if cache.site == site && cache.callee == value {
        (cache.closure, cache.ip_start)
    } else if let Some((closure, ip_start)) = closure_call(value, arg_count).filter(|(c, _)| return_arity_matches(*c, operand)) {
        unsafe { *vm.call_cache.get_unchecked_mut(slot) = CallCache { site, callee: value, closure, ip_start } };
        (closure, ip_start)
    } else {
        vm.stack.set_top(top);
        vm.ip = ip;
        vm.call(arg_count, value, operand)?;
        let top = vm.stack.top();
        let stack_start = unsafe { (*vm.frames.top()).stack_start };
        next!(vm, vm.ip, top, stack_start);
    };

    if vm.frames.is_full() {
        become stack_full(vm, ip, top, stack_start);
    }

    let callee_stack_start = unsafe { top.sub(arg_count + 1) };

    if arg_count != 0 {
        vm.stack.set_top(top);
        vm.ip = ip;
        vm.check_arguments_accepted(unsafe { (*closure).param_list_pool_id }, callee_stack_start, arg_count, operand)?;
    }

    let generation = vm.take_frame_generation();
    vm.frames.push(CallFrame::new(closure, ip, callee_stack_start, generation));
    next!(vm, unsafe { code_base.add(ip_start) }, top, callee_stack_start)
}

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

/// A return that cleans breadcrumbs left by tail calls.
#[cold]
#[inline(never)]
fn ret_releasing(vm: &mut Vm, _ip: *const OpCode, top: *mut Value, _stack_start: *mut Value) -> R {
    let frame = vm.frames.pop();
    vm.release_tail_breadcrumbs();
    let (ip, top, caller_stack_start) = return_to_caller(vm, frame, top);
    next!(vm, ip, top, caller_stack_start)
}

/// Returns object after marking it shared.
#[inline(never)]
fn ret_shared_object(vm: &mut Vm, ip: *const OpCode, top: *mut Value, stack_start: *mut Value) -> R {
    vm.stack.set_top(top);
    vm.ip = ip;
    vm.share_in_place(unsafe { top.sub(1) });
    become op_RETURN(vm, ip, top, stack_start)
}

/// Moves the returned value into the slot that held the function, where the caller expects it.
/// Gives the `ip`, stack top and stack start the caller resumes with.
#[inline(always)]
fn return_to_caller(vm: &Vm, frame: CallFrame, top: *mut Value) -> (*const OpCode, *mut Value, *mut Value) {
    let value = unsafe { *top.sub(1) };
    unsafe { *frame.stack_start = value };
    let top = unsafe { frame.stack_start.add(1) };
    let caller_stack_start = unsafe { (*vm.frames.top()).stack_start };
    (frame.return_ip, top, caller_stack_start)
}

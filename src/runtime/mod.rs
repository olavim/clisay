use std::collections::HashMap;
use std::hash::BuildHasherDefault;

use anyhow::bail;
use rustc_hash::FxHasher;
use smallvec::SmallVec;

use crate::Output;
use crate::core::objects::{ObjBoundMethod, ObjInstance};
use crate::core::value::ValueKind;
use crate::frontend::lex::SourcePosition;

use crate::core::native::array::NativeArray;
use crate::core::native::dict::NativeDict;
use crate::core::native::NativeType;
use crate::core::stack::{CachedStack, Stack};
use crate::core::value::Value;
use crate::core::gc::{Gc, GcTraceable};
use crate::core::host::Host;
use crate::core::objects::{self, TypeMember, NativeFn, ObjArray, ObjDict, ObjType, ObjClosure, ObjFn, ObjNativeFn, ObjString, ObjUpvalue, Object, ObjectKind};

use crate::backend::bytecode::chunk::BytecodeChunk;
use crate::backend::bytecode::opcode::{self, OpCode};

const MAX_STACK: usize = 16384;
const MAX_FRAMES: usize = 256;
const INDEX_CACHE_SIZE: usize = 2048;

#[derive(Clone, Copy)]
struct IndexCache {
    site: usize,
    class: *mut ObjType,
    member: TypeMember
}

struct NativeTypes {
    array: *mut ObjType,
    dict: *mut ObjType
}

impl GcTraceable for NativeTypes {
    fn mark(&self, gc: &mut Gc) {
        gc.mark_object(self.array);
        gc.mark_object(self.dict);
    }
    
    fn fmt(&self) -> String {
        unimplemented!()
    }
    
    fn size(&self) -> usize {
        unimplemented!()
    }
}

#[derive(Clone, Copy)]
pub struct CallFrame {
    closure: *mut ObjClosure,
    return_ip: *const OpCode,
    stack_start: *mut Value
}

#[derive(Clone, Copy)]
pub struct TryFrame {
    origin: *mut CallFrame,
    handler_ip: *const OpCode,
    stack_start: *mut Value
}

/// A method invoke (`INVOKE`) whose member resolved through a getter — itself a
/// frame call. The arguments are parked here until the getter frame returns its
/// value (at call-stack depth `depth`), at which point the value is called.
struct PendingInvoke {
    args: SmallVec<[Value; 4]>,
    depth: usize
}

pub struct Vm {
    pub(crate) gc: Gc,
    ip: *const OpCode,
    chunk: BytecodeChunk,
    globals: HashMap<*mut ObjString, Value, BuildHasherDefault<FxHasher>>,
    pub(crate) stack: Stack<Value, MAX_STACK>,
    frames: CachedStack<CallFrame, MAX_FRAMES>,
    try_frames: Vec<TryFrame>,
    open_upvalues: Vec<*mut ObjUpvalue>,
    native_types: NativeTypes,
    index_cache: Box<[IndexCache]>,
    pending_invokes: Vec<PendingInvoke>,
    out: Vec<String>
}

macro_rules! as_short {
    ($l:expr, $r:expr) => { ($l as u16) | (($r as u16) << 8) }
}

mod calls;
mod closures;
mod properties;
mod ops;

#[cfg(debug_assertions)]
fn disassemble(chunk: &BytecodeChunk) {
    Output::println("=== Bytecode ===");
    Output::println(chunk.fmt());
    Output::println("================");
}

fn build_native_type(gc: &mut Gc, native_type: impl NativeType) -> *mut ObjType {
    let class = native_type.build_class(gc);
    gc.alloc(class)
}

/// Executes a compiled `chunk`, returning captured output.
pub fn execute(chunk: BytecodeChunk, gc: Gc) -> Result<Vec<String>, anyhow::Error> {
    Vm::execute(chunk, gc)
}

impl Host for Vm {
    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn gc(&mut self) -> &mut Gc {
        &mut self.gc
    }

    fn collect(&mut self) {
        self.start_gc();
    }

    fn print(&mut self, text: String) {
        self.out.push(text.clone());
        Output::println(text);
    }
}

impl Vm {
    pub fn execute(chunk: BytecodeChunk, mut gc: Gc) -> Result<Vec<String>, anyhow::Error> {
        #[cfg(debug_assertions)] {
            disassemble(&chunk);
        }

        let native_types = NativeTypes {
            array: build_native_type(&mut gc, NativeArray),
            dict: build_native_type(&mut gc, NativeDict)
        };

        let mut vm = Vm {
            gc,
            ip: std::ptr::null(),
            chunk,
            globals: HashMap::default(),
            stack: Stack::new(),
            frames: CachedStack::new(),
            try_frames: Vec::new(),
            open_upvalues: Vec::new(),
            native_types,
            index_cache: vec![IndexCache { site: 0, class: std::ptr::null_mut(), member: TypeMember::Field(0) }; INDEX_CACHE_SIZE].into_boxed_slice(),
            pending_invokes: Vec::new(),
            out: Vec::new()
        };

        vm.stack.init();
        vm.frames.init();
        vm.ip = vm.chunk.code.as_ptr();

        vm.frames.push(CallFrame {
            closure: std::ptr::null_mut(),
            return_ip: std::ptr::null(),
            stack_start: vm.stack.top()
        });

        vm.define_native("print", 1, |vm, _target, args| {
            let value = args[0];
            let value_str = match value.kind() {
                ValueKind::Null => String::from("null"),
                ValueKind::Number => format!("{}", value.as_number()),
                ValueKind::Boolean => format!("{}", value.as_bool()),
                ValueKind::Object(ObjectKind::String) => format!("{}", value.as_object().as_string()),
                ValueKind::Object(_) => format!("{}", value.as_object().fmt())
            };
            vm.print(value_str);
            vm.push(Value::NULL);
            Ok(())
        });

        vm.define_native("time", 0, |vm, _target, _args| {
            let time = std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH).unwrap().as_millis() as f64;
            vm.push(Value::from(time));
            Ok(())
        });

        vm.define_native("gcHeapSize", 0, |vm, _target, _args| {
            let bytes = vm.gc().bytes_allocated as f64;
            vm.push(Value::from(bytes));
            Ok(())
        });

        vm.define_native("gcCollect", 0, |vm, _target, _args| {
            vm.collect();
            vm.push(Value::NULL);
            Ok(())
        });

        vm.define_native("gcStress", 1, |vm, _target, args| {
            vm.gc().stress = args[0].as_bool();
            vm.push(Value::NULL);
            Ok(())
        });

        Ok(vm.interpret()?)
    }

    fn stringify_frame(&self, frame: &CallFrame) -> String {
        let name = unsafe { &(*(*frame.closure).name).value };
        let pos = unsafe { 
            let idx = frame.return_ip.offset_from(self.chunk.code.as_ptr());
            &self.chunk.code_pos[idx as usize]
        };
        format!("\tat {} ({})", name, pos)
    }

    fn error(&self, message: impl Into<String>) -> Result<(), anyhow::Error> {
        let trace = self.frames.iter().skip(1).rev()
            .map(|frame| self.stringify_frame(&frame))
            .collect::<Vec<String>>().join("\n");

        let pos = self.get_source_position();
        bail!(format!("{}\nat {}\n{}", message.into(), pos, trace))
    }

    fn intern(&mut self, name: impl Into<String>) -> *mut ObjString {
        if self.gc.should_collect() {
            self.start_gc();
        }

        self.gc.intern(name)
    }

    fn alloc<T: GcTraceable>(&mut self, obj: T) -> *mut T
        where *mut T: Into<Object>
    {
        if self.gc.should_collect() {
            self.start_gc();
        }

        self.gc.alloc(obj)
    }

    fn define_native(&mut self, name: impl Into<String>, arity: u8, function: NativeFn) {
        let name_ref = self.gc.intern(name.into());
        let native = ObjNativeFn::new(name_ref, arity, function);
        let value = Value::from(self.gc.alloc(native));
        self.globals.insert(name_ref, value);
    }

    fn start_gc(&mut self) {
        self.chunk.mark(&mut self.gc);
        self.native_types.mark(&mut self.gc);

        for (&name, value) in &self.globals {
            self.gc.mark_object(name);
            value.mark(&mut self.gc);
        }

        for &upvalue in &self.open_upvalues {
            self.gc.mark_object(upvalue);
        }

        for pending in &self.pending_invokes {
            for value in &pending.args {
                value.mark(&mut self.gc);
            }
        }

        for value in self.stack.iter() {
            value.mark(&mut self.gc);
        }

        self.gc.collect();
    }

    #[inline]
    pub fn read_next(&mut self) -> OpCode {
        let op = unsafe { *self.ip };
        self.ip = unsafe { self.ip.add(1) };
        op
    }

    pub fn get_source_position(&self) -> &SourcePosition {
        unsafe { 
            let idx = self.ip.offset_from(self.chunk.code.as_ptr()) as usize - 1;
            &self.chunk.code_pos[idx]
        }
    }

    fn interpret(mut self) -> Result<Vec<String>, anyhow::Error> {
        let code_base = self.chunk.code.as_ptr();
        let mut ip = self.ip;

        macro_rules! read_byte {
            () => {{ let b = unsafe { *ip }; ip = unsafe { ip.add(1) }; b }}
        }
        macro_rules! read_short {
            () => {{ let lo = read_byte!(); let hi = read_byte!(); as_short!(lo, hi) }}
        }
        macro_rules! peek_short {
            () => {{ let lo = unsafe { *ip }; let hi = unsafe { *ip.add(1) }; as_short!(lo, hi) }}
        }

        // Run a handler that relies on `self.ip` (operand reads, jumps, errors,
        // GC source positions): publish the local cursor, run, then reload it.
        macro_rules! delegate {
            ($call:expr) => {{ self.ip = ip; $call; ip = self.ip; }}
        }

        // Numeric binary op with the slow path (strings / type error) delegated.
        macro_rules! num_binop {
            ($op:tt, $slow:ident) => {{
                let b = self.stack.peek(0);
                let a = self.stack.peek(1);
                if a.is_number() && b.is_number() {
                    self.stack.truncate(2);
                    self.stack.push(Value::from(a.as_number() $op b.as_number()));
                } else {
                    delegate!(self.$slow()?);
                }
            }}
        }

        // Fused numeric compare-and-branch; branches when `cmp(a, b)` holds.
        macro_rules! cmp_jump {
            ($op:tt, $token:literal) => {{
                let offset = read_short!() as usize;
                let b = self.stack.pop();
                let a = self.stack.pop();
                if !a.is_number() || !b.is_number() {
                    self.ip = ip;
                    self.error(format!("Operator '{}' cannot be applied to operands {} and {}", $token, a, b))?;
                } else if a.as_number() $op b.as_number() {
                    ip = unsafe { code_base.add(offset) };
                }
            }}
        }

        // Fused `local <cmp> const` compare-and-branch.
        macro_rules! cmp_jump_local_const {
            ($op:tt, $token:literal) => {{
                let offset = read_short!() as usize;
                let a_idx = read_byte!() as usize;
                let b_idx = read_byte!() as usize;
                let a = unsafe { *(*self.frames.top()).stack_start.add(a_idx) };
                let b = self.chunk.constants[b_idx];
                if !a.is_number() {
                    self.ip = ip;
                    self.error(format!("Operator '{}' cannot be applied to operands {} and {}", $token, a, b))?;
                } else if a.as_number() $op b.as_number() {
                    ip = unsafe { code_base.add(offset) };
                }
            }}
        }

        loop {
            let op = read_byte!();
            match op {
                // ---- inlined hot ops (operate on the register-resident `ip`) ----
                opcode::GET_LOCAL => {
                    let idx = read_byte!() as usize;
                    let value = unsafe { *(*self.frames.top()).stack_start.add(idx) };
                    self.stack.push(value);
                },
                opcode::SET_LOCAL => {
                    let idx = read_byte!() as usize;
                    unsafe { *(*self.frames.top()).stack_start.add(idx) = self.stack.peek(0) };
                },
                opcode::SET_LOCAL_POP => {
                    let idx = read_byte!() as usize;
                    let value = self.stack.pop();
                    unsafe { *(*self.frames.top()).stack_start.add(idx) = value };
                },
                opcode::GET_UPVALUE => {
                    let idx = read_byte!() as usize;
                    let upvalue = self.get_upvalue(idx);
                    self.stack.push(unsafe { *(*upvalue).location });
                },
                opcode::SET_UPVALUE => {
                    let idx = read_byte!() as usize;
                    let upvalue = self.get_upvalue(idx);
                    unsafe { *(*upvalue).location = self.stack.peek(0) };
                },
                opcode::SET_UPVALUE_POP => {
                    let idx = read_byte!() as usize;
                    let upvalue = self.get_upvalue(idx);
                    let value = self.stack.pop();
                    unsafe { *(*upvalue).location = value };
                },
                opcode::PUSH_CONSTANT => {
                    let idx = read_byte!() as usize;
                    self.stack.push(self.chunk.constants[idx]);
                },
                opcode::PUSH_NULL => self.stack.push(Value::NULL),
                opcode::PUSH_TRUE => self.stack.push(Value::TRUE),
                opcode::PUSH_FALSE => self.stack.push(Value::FALSE),
                opcode::POP => self.stack.truncate(1),
                opcode::JUMP => {
                    let offset = peek_short!() as usize;
                    ip = unsafe { code_base.add(offset) };
                },
                opcode::JUMP_IF_FALSE => {
                    let offset = read_short!() as usize;
                    let value = self.stack.pop();
                    if value.is_falsy() {
                        ip = unsafe { code_base.add(offset) };
                    }
                },
                opcode::JUMP_IF_GE => cmp_jump!(>=, "<"),
                opcode::JUMP_IF_GT => cmp_jump!(>, "<="),
                opcode::JUMP_IF_LE => cmp_jump!(<=, ">"),
                opcode::JUMP_IF_LT => cmp_jump!(<, ">="),
                opcode::JUMP_IF_GE_LOCAL_CONST => cmp_jump_local_const!(>=, "<"),
                opcode::JUMP_IF_GT_LOCAL_CONST => cmp_jump_local_const!(>, "<="),
                opcode::JUMP_IF_LE_LOCAL_CONST => cmp_jump_local_const!(<=, ">"),
                opcode::JUMP_IF_LT_LOCAL_CONST => cmp_jump_local_const!(<, ">="),
                opcode::JUMP_IF_EQ => {
                    let offset = read_short!() as usize;
                    let b = self.stack.pop();
                    let a = self.stack.pop();
                    if a.value_eq(b) { ip = unsafe { code_base.add(offset) }; }
                },
                opcode::JUMP_IF_NEQ => {
                    let offset = read_short!() as usize;
                    let b = self.stack.pop();
                    let a = self.stack.pop();
                    if !a.value_eq(b) { ip = unsafe { code_base.add(offset) }; }
                },
                opcode::SET_LOCAL_ADD_LOCAL_LOCAL => {
                    let dst = read_byte!() as usize;
                    let a_idx = read_byte!() as usize;
                    let b_idx = read_byte!() as usize;
                    let frame = unsafe { (*self.frames.top()).stack_start };
                    let a = unsafe { *frame.add(a_idx) };
                    let b = unsafe { *frame.add(b_idx) };
                    if a.is_number() && b.is_number() {
                        unsafe { *frame.add(dst) = Value::from(a.as_number() + b.as_number()) };
                    } else {
                        self.stack.push(a);
                        self.stack.push(b);
                        self.ip = ip;
                        self.op_add()?;
                        ip = self.ip;
                        let result = self.stack.pop();
                        unsafe { *frame.add(dst) = result };
                    }
                },
                opcode::ADD_LOCAL_CONST => {
                    let a_idx = read_byte!() as usize;
                    let b_idx = read_byte!() as usize;
                    let a = unsafe { *(*self.frames.top()).stack_start.add(a_idx) };
                    let b = self.chunk.constants[b_idx];
                    if a.is_number() && b.is_number() {
                        self.stack.push(Value::from(a.as_number() + b.as_number()));
                    } else {
                        self.stack.push(a);
                        self.stack.push(b);
                        self.ip = ip;
                        self.op_add()?;
                        ip = self.ip;
                    }
                },
                // Fused value-producing `const + local`. `+` does not commute for string concat,
                // so the const operand stays first (unlike the numeric case it would be equivalent).
                opcode::ADD_CONST_LOCAL => {
                    let a_idx = read_byte!() as usize;
                    let b_idx = read_byte!() as usize;
                    let a = self.chunk.constants[a_idx];
                    let b = unsafe { *(*self.frames.top()).stack_start.add(b_idx) };
                    if a.is_number() && b.is_number() {
                        self.stack.push(Value::from(a.as_number() + b.as_number()));
                    } else {
                        self.stack.push(a);
                        self.stack.push(b);
                        self.ip = ip;
                        self.op_add()?;
                        ip = self.ip;
                    }
                },
                opcode::ADD => num_binop!(+, op_add),
                opcode::SUB_LOCAL_CONST => {
                    let a_idx = read_byte!() as usize;
                    let b_idx = read_byte!() as usize;
                    let a = unsafe { *(*self.frames.top()).stack_start.add(a_idx) };
                    let b = self.chunk.constants[b_idx];
                    if a.is_number() && b.is_number() {
                        self.stack.push(Value::from(a.as_number() - b.as_number()));
                    } else {
                        self.stack.push(a);
                        self.stack.push(b);
                        self.ip = ip;
                        self.op_subtract()?;
                        ip = self.ip;
                    }
                },
                // Fused value-producing `const - local` (const is numeric by emission).
                opcode::SUB_CONST_LOCAL => {
                    let a_idx = read_byte!() as usize;
                    let b_idx = read_byte!() as usize;
                    let a = self.chunk.constants[a_idx];
                    let b = unsafe { *(*self.frames.top()).stack_start.add(b_idx) };
                    if a.is_number() && b.is_number() {
                        self.stack.push(Value::from(a.as_number() - b.as_number()));
                    } else {
                        self.stack.push(a);
                        self.stack.push(b);
                        self.ip = ip;
                        self.op_subtract()?;
                        ip = self.ip;
                    }
                },
                opcode::SUBTRACT => num_binop!(-, op_subtract),
                opcode::MULTIPLY => num_binop!(*, op_multiply),
                opcode::DIVIDE => num_binop!(/, op_divide),

                // ---- control flow / cold ops: sync, delegate, reload ----
                opcode::CALL => {
                    let arg_count = read_byte!() as usize;
                    let value = self.stack.peek(arg_count);
                    // Fast path: monomorphic closure call with exact arity, inlined
                    // so the common case never leaves the dispatch loop.
                    if value.is_callable() {
                        let object = value.as_object();
                        if object.tag() == objects::TAG_CLOSURE {
                            let closure_ptr = object.as_closure_ptr();
                            let closure = unsafe { &*closure_ptr };
                            if arg_count == closure.arity as usize {
                                if self.frames.is_full() {
                                    self.ip = ip;
                                    return Err(self.stack_overflow());
                                }
                                let stack_start = self.stack.offset(arg_count);
                                self.frames.push(CallFrame {
                                    closure: closure_ptr,
                                    return_ip: ip,
                                    stack_start
                                });
                                ip = unsafe { code_base.add(closure.ip_start) };
                                continue;
                            }
                        }
                    }
                    // Slow path: native fns, bound methods, classes, arity errors.
                    self.ip = ip;
                    self.call(arg_count, value)?;
                    ip = self.ip;
                },
                opcode::RETURN => {
                    self.ip = ip;
                    if !self.op_return()? {
                        return Ok(self.out);
                    }
                    ip = self.ip;
                },
                opcode::THROW => delegate!(self.op_throw()?),
                opcode::PUSH_TRY => delegate!(self.op_push_try()),
                opcode::POP_TRY => self.op_pop_try(),
                // `&&`/`||` short-circuit. Cold (no loop runs them), so kept out of
                // the inlined hot block to keep the dispatch loop body small.
                opcode::JUMP_IF_FALSE_OR_POP => delegate!(self.op_jump_if_false_or_pop()),
                opcode::JUMP_IF_TRUE_OR_POP => delegate!(self.op_jump_if_true_or_pop()),
                opcode::CLOSE_UPVALUE => delegate!(self.op_close_upvalue()),
                opcode::ARRAY => delegate!(self.op_array()),
                opcode::DICT => delegate!(self.op_dict()),
                opcode::PUSH_CLOSURE => delegate!(self.op_push_closure()?),
                opcode::PUSH_CLASS => delegate!(self.op_push_class()),
                opcode::GET_GLOBAL => delegate!(self.op_get_global()?),
                opcode::INVOKE => delegate!(self.op_invoke()?),
                opcode::GET_INDEX => delegate!(self.op_get_index()?),
                opcode::SET_INDEX => delegate!(self.op_set_index()?),
                opcode::GET_PROPERTY => delegate!(self.op_get_property()?),
                opcode::SET_PROPERTY => delegate!(self.op_set_property()?),
                opcode::GET_PROPERTY_ID => delegate!(self.op_get_property_by_id()?),
                opcode::SET_PROPERTY_ID => delegate!(self.op_set_property_by_id()?),
                opcode::SET_PROPERTY_ID_POP => delegate!(self.op_set_property_by_id_pop()?),
                opcode::NEGATE => delegate!(self.op_negate()?),
                opcode::NOT => self.op_not(),
                opcode::LEFT_SHIFT => delegate!(self.op_left_shift()?),
                opcode::RIGHT_SHIFT => delegate!(self.op_right_shift()?),
                opcode::BIT_AND => delegate!(self.op_bit_and()?),
                opcode::BIT_OR => delegate!(self.op_bit_or()?),
                opcode::BIT_XOR => delegate!(self.op_bit_xor()?),
                opcode::BIT_NOT => delegate!(self.op_bit_not()?),
                opcode::EQUAL => delegate!(self.op_equal()?),
                opcode::NOT_EQUAL => delegate!(self.op_not_equal()?),
                opcode::LESS_THAN => delegate!(self.op_less_than()?),
                opcode::LESS_THAN_EQUAL => delegate!(self.op_less_than_equal()?),
                opcode::GREATER_THAN => delegate!(self.op_greater_than()?),
                opcode::GREATER_THAN_EQUAL => delegate!(self.op_greater_than_equal()?),
                _ => unsafe { std::hint::unreachable_unchecked() }
            }
        }
    }
}

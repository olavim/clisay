use std::mem;

use anyhow::bail;
use arrayvec::ArrayVec;
use fnv::FnvHashMap;

use crate::bytecode::objects::{ObjBoundMethod, ObjInstance};
use crate::bytecode::value::ValueKind;
use crate::lexer::{tokenize, TokenStream};

use super::chunk::BytecodeChunk;
use super::parser::Parser;
use super::value::Value;
use super::OpCode;
use super::gc::{Gc, GcTraceable};
use super::objects::{ClassMember, ObjClass, ObjClosure, ObjFn, ObjNativeFn, ObjString, ObjUpvalue, Object, ObjectKind};
use super::compiler::Compiler;

const MAX_STACK: usize = 16384;
const MAX_FRAMES: usize = 256;

pub struct CallFrame {
    return_ip: *const OpCode,
    stack_start: *mut Value
}

impl CallFrame {
    pub fn default() -> Self {
        CallFrame {
            return_ip: std::ptr::null(),
            stack_start: std::ptr::null_mut()
        }
    }
}

pub struct Vm<'out> {
    gc: Gc,
    ip: *const OpCode,
    chunk: BytecodeChunk,
    globals: FnvHashMap<*mut ObjString, Value>,
    stack: [Value; 16384],
    stack_top: *mut Value,
    frames: ArrayVec<CallFrame, MAX_FRAMES>,
    frame: CallFrame,
    open_upvalues: Vec<*mut ObjUpvalue>,
    out: &'out mut Vec<String>
}

macro_rules! as_short {
    ($l:expr, $r:expr) => { (($l as u16) << 8) | ($r as u16) }
}

impl<'out> Vm<'out> {
    pub fn run(file_name: &str, src: &str) -> Result<Vec<String>, anyhow::Error> {
        let mut gc = Gc::new();
        let tokens = tokenize(String::from(file_name), String::from(src))?;
        let ast = Parser::parse(&mut gc, &mut TokenStream::new(&tokens))?;
        let chunk = Compiler::compile(&ast, &mut gc)?;

        // println!("=== Bytecode ===");
        // print!("{}", chunk.fmt());
        // println!("================");

        let mut out = Vec::new();
        let mut vm = Vm {
            gc,
            ip: chunk.code.as_ptr(),
            chunk,
            globals: FnvHashMap::default(),
            stack: [Value::NULL; MAX_STACK],
            stack_top: std::ptr::null_mut(),
            frames: ArrayVec::new(),
            frame: CallFrame::default(),
            open_upvalues: Vec::new(),
            out: &mut out
        };

        vm.stack_top = vm.stack.as_mut_ptr();
        vm.frame = CallFrame {
            return_ip: std::ptr::null(),
            stack_start: vm.stack_top
        };

        vm.define_native("print", 1, |vm| {
            let value = vm.peek(0);
            let value_str = match value.kind() {
                ValueKind::Null => String::from("null"),
                ValueKind::Number => format!("{}", value.as_number()),
                ValueKind::Boolean => format!("{}", value.as_bool()),
                ValueKind::Object(_) => format!("{}", value.as_object().fmt())
            };
            vm.out.push(value_str.clone());
            println!("{}", value_str);
            vm.push(Value::NULL);
        });

        vm.define_native("time", 0, |vm| {
            let time = std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH).unwrap().as_millis() as f64;
            vm.push(Value::from(time));
        });

        vm.define_native("gcHeapSize", 0, |vm| {
            vm.push(Value::from(vm.gc.bytes_allocated as f64));
        });

        vm.define_native("gcCollect", 0, |vm| {
            vm.start_gc();
            vm.push(Value::NULL);
        });

        vm.interpret()?;
        Ok(out)
    }

    fn error(&self, message: impl Into<String>) -> Result<(), anyhow::Error> {
        let trace = self.frames.iter().rev()
            .map(|frame| {
                let function = unsafe { &*self.get_closure(&frame).function };
                let name = unsafe { &(*function.name).value };
                let pos = unsafe { 
                    let idx = frame.return_ip.offset_from(self.chunk.code.as_ptr());
                    &self.chunk.code_pos[idx as usize]
                };
                format!("\tat {} ({})", name, pos)
            })
            .collect::<Vec<String>>().join("\n");

        let pos = unsafe { 
            let idx = self.ip.offset_from(self.chunk.code.as_ptr()) as usize - 1;
            &self.chunk.code_pos[idx]
        };

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

    fn define_native(&mut self, name: impl Into<String>, arity: u8, function: fn(&mut Vm)) {
        let name_ref = self.gc.intern(name.into());
        let native = ObjNativeFn::new(name_ref, arity, function);
        let value = Value::from(self.gc.alloc(native));
        self.globals.insert(name_ref, value);
    }

    fn start_gc(&mut self) {
        self.chunk.mark(&mut self.gc);

        for (&name, value) in &self.globals {
            self.gc.mark_object(name);
            value.mark(&mut self.gc);
        }

        for &upvalue in &self.open_upvalues {
            self.gc.mark_object(upvalue);
        }

        let mut p = self.stack.as_ptr();
        while p != self.stack_top {
            unsafe {
                (*p).mark(&mut self.gc);
                p = p.offset(1);
            }
        }

        self.gc.collect();
    }

    #[inline]
    fn get_upvalue(&self, idx: usize) -> *mut ObjUpvalue {
        self.get_closure(&self.frame).upvalues[idx]
    }

    fn get_closure(&self, frame: &CallFrame) -> &ObjClosure {
        let value = unsafe { *frame.stack_start };
        let closure_ref = unsafe { value.as_object().closure };
        unsafe { &*closure_ref }
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

    fn close_upvalues(&mut self, after: *const Value) {
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

    #[inline]
    fn peek(&self, offset: usize) -> Value {
        unsafe { *self.stack_top.offset(-(offset as isize) - 1) }
    }

    #[inline]
    fn pop(&mut self) {
        self.stack_top = unsafe { self.stack_top.offset(-1) };
    }

    #[inline]
    fn pop_n(&mut self, count: usize) {
        self.stack_top = unsafe { self.stack_top.sub(count) };
    }

    #[inline]
    fn pop_get(&mut self) -> Value {
        unsafe {
            self.stack_top = self.stack_top.offset(-1);
            *self.stack_top
        }
    }

    #[inline]
    fn push(&mut self, value: Value) {
        unsafe {
            *self.stack_top = value;
            self.stack_top = self.stack_top.offset(1);
        }
    }

    #[inline]
    fn set(&mut self, offset: usize, value: Value) -> *mut Value {
        unsafe {
            let ptr = self.stack_top.sub(offset);
            *ptr = value;
            ptr
        }
    }

    fn create_closure(&mut self, function: *mut ObjFn) -> *mut ObjClosure {
        let mut closure = ObjClosure::new(function);
        let upvalue_count = unsafe { (*function).upvalues.len() };

        for i in 0..upvalue_count {
            let fn_upval = unsafe { &(*function).upvalues[i] };
            let upvalue = if fn_upval.is_local {
                self.capture_upvalue(unsafe { self.frame.stack_start.add(fn_upval.location as usize) })
            } else {
                self.get_upvalue(fn_upval.location as usize)
            };

            closure.upvalues.push(upvalue);
        }

        self.alloc(closure)
    }

    fn get_property(&mut self, instance_ref: *mut ObjInstance, prop: *mut ObjString) -> Option<Value> {
        let instance = unsafe { &*instance_ref };
        let class = unsafe { &*instance.class };

        match class.resolve(prop) {
            Some(ClassMember::Field(id)) |
            Some(ClassMember::Method(id)) => Some(self.get_property_by_id(instance_ref, id)),
            None => None
        }
    }

    fn get_property_by_id(&mut self, instance_ref: *mut ObjInstance, id: u8) -> Value {
        let value = unsafe { (*instance_ref).get(id) };
        match value.kind() {
            ValueKind::Object(ObjectKind::Function) => {
                let closure = self.create_closure(unsafe { value.as_object().function });
                let method = self.alloc(ObjBoundMethod::new(instance_ref, closure));
                Value::from(method)
            },
            _ => value
        }
    }

    #[inline]
    fn push_frame(&mut self, ip: *const OpCode, stack_start: *mut Value) {
        let frame = CallFrame {
            return_ip: ip,
            stack_start
        };
        unsafe { self.frames.push_unchecked(mem::replace(&mut self.frame, frame)); }
    }

    fn call_native(&mut self, arg_count: usize, ptr: *mut ObjNativeFn) -> Result<(), anyhow::Error> {
        let func = unsafe { &(*ptr).function };
        func(self);
        let result = self.peek(0);
        self.pop_n(arg_count + 2);
        self.push(result);
        Ok(())
    }

    fn call_function(&mut self, arg_count: usize, ptr: *mut ObjFn) -> Result<(), anyhow::Error> {
        self.push_frame(self.ip, unsafe { self.stack_top.sub(arg_count + 1) });
        let ip_start = unsafe { (*ptr).ip_start };
        self.ip = unsafe { self.chunk.code.as_ptr().offset(ip_start as isize) };
        Ok(())
    }

    fn call_closure(&mut self, arg_count: usize, ptr: *mut ObjClosure) -> Result<(), anyhow::Error> {
        let function = unsafe { (*ptr).function };
        self.call_function(arg_count, function)
    }

    fn call_bound_method(&mut self, arg_count: usize, ptr: *mut ObjBoundMethod) -> Result<(), anyhow::Error> {
        let instance = unsafe { (*ptr).instance };
        let stack_start = self.set(arg_count + 1, Value::from(instance));
        self.push_frame(self.ip, stack_start);

        let function = unsafe { &*(*(*ptr).closure).function };
        self.ip = unsafe { self.chunk.code.as_ptr().offset(function.ip_start as isize) };
        Ok(())
    }

    fn call_class(&mut self, arg_count: usize, ptr: *mut ObjClass) -> Result<(), anyhow::Error> {
        let class = unsafe { &*ptr };
        let init_id = class.resolve_id(self.gc.intern("init")).unwrap();
        let init_ref = class.get_method(init_id).unwrap();
        
        let closure = self.create_closure(init_ref);
        self.push(Value::from(closure));

        let instance = self.alloc(ObjInstance::new(ptr));
        let stack_start = self.set(arg_count + 1, Value::from(instance));
        self.push_frame(self.ip, stack_start);

        let init = unsafe { &*init_ref };
        self.ip = unsafe { self.chunk.code.as_ptr().offset(init.ip_start as isize) };
        Ok(())
    }

    fn read_op(&mut self) -> OpCode {
        let op = unsafe { *self.ip };
        self.ip = unsafe { self.ip.offset(1) };
        op
    }

    fn interpret(&mut self) -> Result<(), anyhow::Error> {
        loop {
            let op = self.read_op();
            match op {
                OpCode::Return => {
                    if !self.op_return() {
                        return Ok(());
                    }
                },
                OpCode::Pop => self.op_pop(),
                OpCode::CloseUpvalue(location) => self.op_close_upvalue(location),
                OpCode::Call(arg_count) => self.op_call(arg_count)?,
                OpCode::Jump(lpos, rpos) => self.op_jump(lpos, rpos),
                OpCode::JumpIfFalse(lpos, rpos) => self.op_jump_if_false(lpos, rpos),
                OpCode::PushConstant(const_idx) => self.op_push_constant(const_idx),
                OpCode::PushNull => self.op_push_null(),
                OpCode::PushTrue => self.op_push_true(),
                OpCode::PushFalse => self.op_push_false(),
                OpCode::PushClosure(const_idx) => self.op_push_closure(const_idx)?,
                OpCode::PushClass(const_idx) => self.op_push_class(const_idx),
                OpCode::GetGlobal(const_idx) => self.op_get_global(const_idx)?,
                OpCode::SetGlobal(const_idx) => self.op_set_global(const_idx)?,
                OpCode::GetLocal(idx) => self.op_get_local(idx),
                OpCode::SetLocal(idx) => self.op_set_local(idx),
                OpCode::GetUpvalue(idx) => self.op_get_upvalue(idx),
                OpCode::SetUpvalue(idx) => self.op_set_upvalue(idx),
                OpCode::GetProperty(const_idx) => self.op_get_property(const_idx)?,
                OpCode::SetProperty(const_idx) => self.op_set_property(const_idx)?,
                OpCode::GetPropertyId(member_id) => self.op_get_property_by_id(member_id)?,
                OpCode::SetPropertyId(member_id) => self.op_set_property_by_id(member_id)?,
                OpCode::Add => self.op_add()?,
                OpCode::Subtract => self.op_subtract()?,
                OpCode::Multiply => self.op_multiply()?,
                OpCode::Divide => self.op_divide()?,
                OpCode::Equal => self.op_equal()?,
                OpCode::NotEqual => self.op_not_equal()?,
                OpCode::LessThan => self.op_less_than()?,
                OpCode::LessThanEqual => self.op_less_than_equal()?,
                OpCode::GreaterThan => self.op_greater_than()?,
                OpCode::GreaterThanEqual => self.op_greater_than_equal()?,
                OpCode::Negate => self.op_negate()?,
                OpCode::Not => self.op_not()?,
                OpCode::And => self.op_and()?,
                OpCode::Or => self.op_or()?,
                OpCode::LeftShift => self.op_left_shift()?,
                OpCode::RightShift => self.op_right_shift()?,
                OpCode::BitAnd => self.op_bit_and()?,
                OpCode::BitOr => self.op_bit_or()?,
                OpCode::BitXor => self.op_bit_xor()?,
                OpCode::BitNot => self.op_bit_not()?
            }
        }
    }
    
    fn op_return(&mut self) -> bool {
        let value = self.pop_get();
        let frame = match self.frames.pop() {
            Some(frame) => frame,
            None => return false
        };

        self.ip = self.frame.return_ip;
        self.close_upvalues(self.frame.stack_start);

        self.stack_top = self.frame.stack_start;
        self.push(value);

        self.frame = frame;
        true
    }

    fn op_pop(&mut self) {
        self.pop();
    }

    fn op_close_upvalue(&mut self, location: u8) {
        let p = unsafe { self.frame.stack_start.add(location as usize) };
        self.close_upvalues(p);
        self.pop();
    }

    fn op_call(&mut self, arg_count: u8) -> Result<(), anyhow::Error> {
        let arg_count = arg_count as usize;
        let object = self.peek(arg_count).as_object();
        match object.kind() {
            ObjectKind::Function => self.call_function(arg_count, unsafe { object.function }),
            ObjectKind::NativeFunction => self.call_native(arg_count, unsafe { object.native_function }),
            ObjectKind::Closure => self.call_closure(arg_count, unsafe { object.closure }),
            ObjectKind::BoundMethod => self.call_bound_method(arg_count, unsafe { object.bound_method }),
            ObjectKind::Class => self.call_class(arg_count, unsafe { object.class }),
            _ => unreachable!()
        }
    }

    fn op_jump(&mut self, lpos: u8, rpos: u8) {
        let offset = as_short!(lpos, rpos) as isize;
        self.ip = unsafe { self.chunk.code.as_ptr().offset(offset) };
    }

    fn op_jump_if_false(&mut self, lpos: u8, rpos: u8) {
        let offset = as_short!(lpos, rpos) as isize;
        let value = self.pop_get();
        if value.is_bool() && !value.as_bool() {
            self.ip = unsafe { self.chunk.code.as_ptr().offset(offset) };
        }
    }

    fn op_push_constant(&mut self, const_idx: u8) {
        let value = self.chunk.constants[const_idx as usize].clone();
        self.push(value);
    }

    fn op_push_null(&mut self) {
        self.push(Value::NULL)
    }

    fn op_push_true(&mut self) {
        self.push(Value::TRUE)
    }

    fn op_push_false(&mut self) {
        self.push(Value::FALSE)
    }

    fn op_push_closure(&mut self, const_idx: u8) -> Result<(), anyhow::Error> {
        let value = self.chunk.constants[const_idx as usize];
        let fn_ref = unsafe { value.as_object().function };
        let closure = self.create_closure(fn_ref);
        self.push(Value::from(closure));
        Ok(())
    }

    fn op_push_class(&mut self, const_idx: u8) {
        let value = self.chunk.constants[const_idx as usize];
        self.push(value);
    }

    fn op_get_global(&mut self, const_idx: u8) -> Result<(), anyhow::Error> {
        let constant = &self.chunk.constants[const_idx as usize];
        let string = unsafe { &constant.as_object().string };
        let value = self.globals[string];
        self.push(value);
        Ok(())
    }

    fn op_set_global(&mut self, const_idx: u8) -> Result<(), anyhow::Error> {
        let constant = &self.chunk.constants[const_idx as usize];
        let string = unsafe { constant.as_object().string };
        let value = self.pop_get();
        self.globals.insert(string, value);
        Ok(())
    }

    fn op_get_local(&mut self, idx: u8) {
        let value = unsafe { *self.frame.stack_start.add(idx as usize) };
        self.push(value);
    }

    fn op_set_local(&mut self, idx: u8) {
        unsafe {
            *self.frame.stack_start.add(idx as usize) = self.peek(0);
        };
    }

    fn op_get_upvalue(&mut self, idx: u8) {
        let upvalue = self.get_upvalue(idx as usize);
        self.push(unsafe { *(*upvalue).location });
    }

    fn op_set_upvalue(&mut self, idx: u8) {
        let upvalue = self.get_upvalue(idx as usize);
        unsafe { *(*upvalue).location = self.peek(0) };
    }

    fn op_get_property(&mut self, const_idx: u8) -> Result<(), anyhow::Error> {
        let value = self.pop_get();
        if !matches!(value.kind(), ValueKind::Object(ObjectKind::Instance)) {
            return self.error(format!("Invalid property access: {}", value.fmt()));
        }

        let object = value.as_object();
        let instance_ref = unsafe { object.instance };
        let prop = unsafe { self.chunk.constants[const_idx as usize].as_object().string };

        if let Some(value) = self.get_property(instance_ref, prop) {
            self.push(value.clone());
            Ok(())
        } else {
            bail!("Property not found")
        }
    }

    fn op_set_property(&mut self, const_idx: u8) -> Result<(), anyhow::Error> {
        let value = self.pop_get();
        if !matches!(value.kind(), ValueKind::Object(ObjectKind::Instance)) {
            return self.error(format!("Invalid property access: {}", value.fmt()));
        }

        let object = value.as_object();
        let instance_ref = unsafe { object.instance };
        let prop = unsafe { self.chunk.constants[const_idx as usize].as_object().string };

        let instance = unsafe { &mut *instance_ref };
        let class = unsafe { &*instance.class };
        match class.resolve(prop) {
            Some(ClassMember::Field(id)) => {
                let value = self.peek(0);
                instance.values.insert(id, value);
                Ok(())
            },
            Some(ClassMember::Method(_)) => bail!("Cannot set method"),
            None => bail!("No such property")
        }
    }

    fn op_get_property_by_id(&mut self, member_id: u8) -> Result<(), anyhow::Error> {
        let value = self.pop_get();
        if !matches!(value.kind(), ValueKind::Object(ObjectKind::Instance)) {
            return self.error(format!("Invalid property access: {}", value.fmt()));
        }

        let object = value.as_object();
        let instance_ref = unsafe { object.instance };
        let value = self.get_property_by_id(instance_ref, member_id);
        self.push(value);
        Ok(())
    }

    fn op_set_property_by_id(&mut self, member_id: u8) -> Result<(), anyhow::Error> {
        let value = self.pop_get();
        if !matches!(value.kind(), ValueKind::Object(ObjectKind::Instance)) {
            return self.error(format!("Invalid property access: {}", value.fmt()));
        }

        let object = value.as_object();
        let instance_ref = unsafe { object.instance };
        let value = self.peek(0);
        let instance = unsafe { &mut *instance_ref };
        instance.set(member_id, value);
        Ok(())
    }

    fn op_add(&mut self) -> Result<(), anyhow::Error> {
        let b = self.pop_get();
        let a = self.pop_get();
        let result = match (a.kind(), b.kind()) {
            (ValueKind::Number, ValueKind::Number) => Value::from(a.as_number() + b.as_number()),
            (ValueKind::Object(ObjectKind::String), ValueKind::Object(ObjectKind::String)) => {
                let sa = unsafe { &*a.as_object().string }.value.as_str();
                let sb = unsafe { &*b.as_object().string }.value.as_str();
                let s = [sa, sb].concat();
                Value::from(self.intern(s))
            },
            _ => {
                return self.error(format!("Operator '+' cannot be applied to operands {} and {}", a, b))
            }
        };
        self.push(result);
        Ok(())
    }
    
    fn op_subtract(&mut self) -> Result<(), anyhow::Error> {
        self.binary_op_number(|a, b| Value::from(a - b), "-")
    }
    
    fn op_multiply(&mut self) -> Result<(), anyhow::Error> {
        self.binary_op_number(|a, b| Value::from(a * b), "*")
    }
    
    fn op_divide(&mut self) -> Result<(), anyhow::Error> {
        self.binary_op_number(|a, b| Value::from(a / b), "/")
    }
    
    fn op_equal(&mut self) -> Result<(), anyhow::Error> {
        self.binary_op(|a, b| Value::from(a == b))
    }

    fn op_not_equal(&mut self) -> Result<(), anyhow::Error> {
        self.binary_op(|a, b| Value::from(a != b))
    }
    
    fn op_less_than(&mut self) -> Result<(), anyhow::Error> {
        self.binary_op_number(|a, b| Value::from(a < b), "<")
    }
    
    fn op_less_than_equal(&mut self) -> Result<(), anyhow::Error> {
        self.binary_op_number(|a, b| Value::from(a <= b), "<=")
    }
    
    fn op_greater_than(&mut self) -> Result<(), anyhow::Error> {
        self.binary_op_number(|a, b| Value::from(a > b), ">")
    }
    
    fn op_greater_than_equal(&mut self) -> Result<(), anyhow::Error> {
        self.binary_op_number(|a, b| Value::from(a >= b), ">=")
    }
    
    fn op_negate(&mut self) -> Result<(), anyhow::Error> {
        let value = self.pop_get();
        if !value.is_number() {
            bail!("Invalid operand")
        }
        self.push(Value::from(-value.as_number()));
        Ok(())
    }
    
    fn op_not(&mut self) -> Result<(), anyhow::Error> {
        let value = self.pop_get();
        if !value.is_bool() {
            bail!("Invalid operand")
        }
        self.push(Value::from(!value.as_bool()));
        Ok(())
    }
    
    fn op_and(&mut self) -> Result<(), anyhow::Error> {
        self.binary_op(|a, b| Value::from(a.as_bool() && b.as_bool()))
    }
    
    fn op_or(&mut self) -> Result<(), anyhow::Error> {
        self.binary_op(|a, b| Value::from(a.as_bool() || b.as_bool()))
    }

    fn op_left_shift(&mut self) -> Result<(), anyhow::Error> {
        self.binary_op_number(|a, b| Value::from(((a as i64) << (b as i64)) as f64), "<<")
    }

    fn op_right_shift(&mut self) -> Result<(), anyhow::Error> {
        self.binary_op_number(|a, b| Value::from(((a as i64) >> (b as i64)) as f64), ">>")
    }

    fn op_bit_and(&mut self) -> Result<(), anyhow::Error> {
        self.binary_op_number(|a, b| Value::from(((a as i64) & (b as i64)) as f64), "&&")
    }

    fn op_bit_or(&mut self) -> Result<(), anyhow::Error> {
        self.binary_op_number(|a, b| Value::from(((a as i64) | (b as i64)) as f64), "||")
    }

    fn op_bit_xor(&mut self) -> Result<(), anyhow::Error> {
        self.binary_op_number(|a, b| Value::from(((a as i64) ^ (b as i64)) as f64), "^")
    }

    fn op_bit_not(&mut self) -> Result<(), anyhow::Error> {
        let value = self.pop_get();
        if !value.is_number() {
            bail!("Invalid operand")
        }
        self.push(Value::from(!(value.as_number() as i64) as f64));
        Ok(())
    }

    fn binary_op_number<F: Fn(f64, f64) -> Value>(&mut self, func: F, token: impl Into<String>) -> Result<(), anyhow::Error> {
        let b = self.pop_get();
        let a = self.pop_get();

        if !a.is_number() || !b.is_number() {
            return self.error(format!("Operator '{}' cannot be applied to operands {} and {}", token.into(), a, b));
        }

        self.push(func(a.as_number(), b.as_number()));
        Ok(())
    }

    fn binary_op<F: Fn(Value, Value) -> Value>(&mut self, func: F) -> Result<(), anyhow::Error> {
        let b = self.pop_get();
        let a = self.pop_get();
        self.push(func(a, b));
        Ok(())
    }
}

use std::collections::HashMap;
use std::hash::BuildHasherDefault;

use anyhow::bail;
use rustc_hash::FxHasher;

use crate::vm::objects::{ObjBoundMethod, ObjInstance};
use crate::vm::value::ValueKind;
use crate::lexer::{tokenize, TokenStream};

use super::chunk::BytecodeChunk;
use super::opcode::{self, OpCode};
use super::parser::Parser;
use super::stack::{CachedStack, Stack};
use super::value::Value;
use super::gc::{Gc, GcTraceable};
use super::objects::{self, ClassMember, ObjClass, ObjClosure, ObjFn, ObjNativeFn, ObjString, ObjUpvalue, Object};
use super::compiler::Compiler;

const MAX_STACK: usize = 16384;
const MAX_FRAMES: usize = 256;

#[derive(Clone, Copy)]
pub struct CallFrame {
    closure: *mut ObjClosure,
    return_ip: *const OpCode,
    stack_start: *mut Value
}

pub struct Vm<'out> {
    gc: Gc,
    chunk: BytecodeChunk,
    globals: HashMap<*mut ObjString, Value, BuildHasherDefault<FxHasher>>,
    stack: Stack<Value, MAX_STACK>,
    frames: CachedStack<CallFrame, MAX_FRAMES>,
    open_upvalues: Vec<*mut ObjUpvalue>,
    out: &'out mut Vec<String>
}

macro_rules! as_short {
    ($l:expr, $r:expr) => { ($l as u16) | (($r as u16) << 8) }
}

fn disassemble(chunk: &BytecodeChunk) {
    println!("=== Bytecode ===");
    print!("{}", chunk.fmt());
    println!("================");
}

impl<'out> Vm<'out> {
    pub fn run(file_name: &str, src: &str) -> Result<Vec<String>, anyhow::Error> {
        let mut gc = Gc::new();
        let tokens = tokenize(String::from(file_name), String::from(src))?;
        let ast = Parser::parse(&mut gc, &mut TokenStream::new(&tokens))?;
        let chunk = Compiler::compile(&ast, &mut gc)?;

        #[cfg(feature = "debug")] {
            disassemble(&chunk);
        }

        let mut out = Vec::new();
        let mut vm = Vm {
            gc,
            chunk,
            globals: HashMap::default(),
            stack: Stack::new(),
            frames: CachedStack::new(),
            open_upvalues: Vec::new(),
            out: &mut out
        };

        vm.stack.init();
        vm.frames.init();
        vm.chunk.make_readable();

        vm.frames.push(CallFrame {
            closure: std::ptr::null_mut(),
            return_ip: std::ptr::null(),
            stack_start: vm.stack.top()
        });

        vm.define_native("print", 1, |vm| {
            let value = vm.stack.peek(0);
            let value_str = match value.kind() {
                ValueKind::Null => String::from("null"),
                ValueKind::Number => format!("{}", value.as_number()),
                ValueKind::Boolean => format!("{}", value.as_bool()),
                ValueKind::Object(_) => format!("{}", value.as_object().fmt())
            };
            vm.out.push(value_str.clone());
            println!("{}", value_str);
            vm.stack.push(Value::NULL);
        });

        vm.define_native("time", 0, |vm| {
            let time = std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH).unwrap().as_millis() as f64;
            vm.stack.push(Value::from(time));
        });

        vm.define_native("gcHeapSize", 0, |vm| {
            vm.stack.push(Value::from(vm.gc.bytes_allocated as f64));
        });

        vm.define_native("gcCollect", 0, |vm| {
            vm.start_gc();
            vm.stack.push(Value::NULL);
        });

        vm.interpret()?;
        Ok(out)
    }

    fn stringify_frame(&self, frame: &CallFrame) -> String {
        let name = unsafe { &(*(*(*frame.closure).function).name).value };
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

        let pos = self.chunk.get_source_position();
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

        for value in self.stack.iter() {
            value.mark(&mut self.gc);
        }

        self.gc.collect();
    }

    fn get_upvalue(&self, idx: usize) -> *mut ObjUpvalue {
        unsafe { *(*self.frames.top.closure).upvalues.get_unchecked(idx as usize) }
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

    fn create_closure(&mut self, function: *mut ObjFn) -> *mut ObjClosure {
        let mut closure = ObjClosure::new(function);
        let upvalue_count = unsafe { (*function).upvalues.len() };

        for i in 0..upvalue_count {
            let fn_upval = unsafe { &(*function).upvalues[i] };
            let upvalue = if fn_upval.is_local {
                self.capture_upvalue(unsafe { self.frames.top.stack_start.add(fn_upval.location as usize) })
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
            ValueKind::Object(tag) if tag == objects::TAG_FUNCTION => {
                let closure = self.create_closure(value.as_object().as_function());
                let method = self.alloc(ObjBoundMethod::new(instance_ref, closure));
                Value::from(method)
            },
            _ => value
        }
    }

    fn push_frame(&mut self, closure: *mut ObjClosure, stack_start: *mut Value, ip_start: usize) {
        self.frames.push(CallFrame {
            closure,
            return_ip: self.chunk.get_position(),
            stack_start
        });
        self.chunk.set_position(unsafe { self.chunk.code.as_ptr().offset(ip_start as isize) });
    }

    fn call_native(&mut self, arg_count: usize, obj: Object) -> Result<(), anyhow::Error> {
        let func = unsafe { &*obj.as_native_function() };
        if arg_count != func.arity as usize {
            return self.error(format!("{} expects {} arguments, but got {}", unsafe { &*func.name }.value, func.arity, arg_count));
        }

        (func.function)(self);
        let result = self.stack.peek(0);
        self.stack.truncate(arg_count + 2);
        self.stack.push(result);
        Ok(())
    }

    fn call_closure(&mut self, arg_count: usize, obj: Object) -> Result<(), anyhow::Error> {
        let closure_ptr = obj.as_closure();
        let func = unsafe { &*(*closure_ptr).function };
        if arg_count != func.arity as usize {
            return self.error(format!("{} expects {} arguments, but got {}", unsafe { &*func.name }.value, func.arity, arg_count));
        }
        self.push_frame(closure_ptr, self.stack.offset(arg_count), func.ip_start);
        Ok(())
    }

    fn call_bound_method(&mut self, arg_count: usize, obj: Object) -> Result<(), anyhow::Error> {
        let bound_method = unsafe { &*obj.as_bound_method() };
        let func = unsafe { std::ptr::read((*bound_method.closure).function) };
        if arg_count != func.arity as usize {
            return self.error(format!("{} expects {} arguments, but got {}", unsafe { &*func.name }.value, func.arity, arg_count));
        }

        let stack_start = self.stack.set(arg_count, Value::from(bound_method.instance));
        self.push_frame(bound_method.closure, stack_start, func.ip_start);
        Ok(())
    }

    fn call_class(&mut self, arg_count: usize, obj: Object) -> Result<(), anyhow::Error> {
        let class_ptr = obj.as_class();
        let class = unsafe { &*class_ptr };
        let init_id = class.resolve_id(self.intern("init")).unwrap();
        let init_ref = class.get_method(init_id).unwrap();
        let init = unsafe { &*init_ref };
        if arg_count != init.arity as usize {
            return self.error(format!("{} expects {} arguments, but got {}", unsafe { &*init.name }.value, init.arity, arg_count));
        }
        
        let closure = self.create_closure(init_ref);
        self.stack.push(Value::from(closure));

        let instance = self.alloc(ObjInstance::new(class_ptr));
        let stack_start = self.stack.set(arg_count, Value::from(instance));

        self.push_frame(closure, stack_start, init.ip_start);
        Ok(())
    }

    fn interpret(&mut self) -> Result<(), anyhow::Error> {
        loop {
            let op = self.chunk.read_next();
            match op {
                opcode::RETURN => {
                    if !self.op_return() {
                        return Ok(());
                    }
                },
                opcode::POP => self.op_pop(),
                opcode::CLOSE_UPVALUE => self.op_close_upvalue(),
                opcode::CALL => self.op_call()?,
                opcode::JUMP => self.op_jump(),
                opcode::JUMP_IF_FALSE => self.op_jump_if_false(),
                opcode::PUSH_CONSTANT => self.op_push_constant(),
                opcode::PUSH_NULL => self.op_push_null(),
                opcode::PUSH_TRUE => self.op_push_true(),
                opcode::PUSH_FALSE => self.op_push_false(),
                opcode::PUSH_CLOSURE => self.op_push_closure()?,
                opcode::PUSH_CLASS => self.op_push_class(),
                opcode::GET_GLOBAL => self.op_get_global()?,
                opcode::SET_GLOBAL => unreachable!(),
                opcode::GET_LOCAL => self.op_get_local(),
                opcode::SET_LOCAL => self.op_set_local(),
                opcode::GET_UPVALUE => self.op_get_upvalue(),
                opcode::SET_UPVALUE => self.op_set_upvalue(),
                opcode::GET_PROPERTY => self.op_get_property()?,
                opcode::SET_PROPERTY => self.op_set_property()?,
                opcode::GET_PROPERTY_ID => self.op_get_property_by_id()?,
                opcode::SET_PROPERTY_ID => self.op_set_property_by_id()?,
                opcode::ADD => self.op_add()?,
                opcode::SUBTRACT => self.op_subtract()?,
                opcode::MULTIPLY => self.op_multiply()?,
                opcode::DIVIDE => self.op_divide()?,
                opcode::EQUAL => self.op_equal()?,
                opcode::NOT_EQUAL => self.op_not_equal()?,
                opcode::LESS_THAN => self.op_less_than()?,
                opcode::LESS_THAN_EQUAL => self.op_less_than_equal()?,
                opcode::GREATER_THAN => self.op_greater_than()?,
                opcode::GREATER_THAN_EQUAL => self.op_greater_than_equal()?,
                opcode::NEGATE => self.op_negate()?,
                opcode::NOT => self.op_not()?,
                opcode::AND => self.op_and()?,
                opcode::OR => self.op_or()?,
                opcode::LEFT_SHIFT => self.op_left_shift()?,
                opcode::RIGHT_SHIFT => self.op_right_shift()?,
                opcode::BIT_AND => self.op_bit_and()?,
                opcode::BIT_OR => self.op_bit_or()?,
                opcode::BIT_XOR => self.op_bit_xor()?,
                opcode::BIT_NOT => self.op_bit_not()?,
                _ => unsafe { std::hint::unreachable_unchecked() }
            }
        }
    }
    
    fn op_return(&mut self) -> bool {
        if self.frames.len() == 1 {
            return false;
        }

        let frame = self.frames.pop();

        self.chunk.set_position(frame.return_ip);
        self.close_upvalues(frame.stack_start);

        let value = self.stack.pop();
        self.stack.set_top(frame.stack_start);
        self.stack.push(value);
        true
    }

    fn op_pop(&mut self) {
        self.stack.truncate(1);
    }

    fn op_close_upvalue(&mut self) {
        let location = self.chunk.read_next() as usize;
        let p = unsafe { self.frames.top.stack_start.add(location) };
        self.close_upvalues(p);
        self.stack.truncate(1);
    }

    fn op_call(&mut self) -> Result<(), anyhow::Error> {
        let arg_count = self.chunk.read_next() as usize;
        let value = self.stack.peek(arg_count);

        if !value.is_callable() {
            return self.error(format!("{} is not callable", value.fmt()));
        }

        let object = value.as_object();
        match object.tag() {
            objects::TAG_CLOSURE => self.call_closure(arg_count, object),
            objects::TAG_NATIVE_FUNCTION => self.call_native(arg_count, object),
            objects::TAG_BOUND_METHOD => self.call_bound_method(arg_count, object),
            objects::TAG_CLASS => self.call_class(arg_count, object),
            _ => unsafe { std::hint::unreachable_unchecked() }
        }
    }

    fn op_jump(&mut self) {
        let offset = as_short!(self.chunk.read_next(), self.chunk.read_next()) as isize;
        self.chunk.set_position(unsafe { self.chunk.code.as_ptr().offset(offset) });
    }

    fn op_jump_if_false(&mut self) {
        let offset = as_short!(self.chunk.read_next(), self.chunk.read_next()) as isize;
        let value = self.stack.pop();
        if value.is_bool() && !value.as_bool() {
            self.chunk.set_position(unsafe { self.chunk.code.as_ptr().offset(offset) });
        }
    }

    fn op_push_constant(&mut self) {
        let const_idx = self.chunk.read_next() as usize;
        let value = self.chunk.constants[const_idx];
        self.stack.push(value);
    }

    fn op_push_null(&mut self) {
        self.stack.push(Value::NULL)
    }

    fn op_push_true(&mut self) {
        self.stack.push(Value::TRUE)
    }

    fn op_push_false(&mut self) {
        self.stack.push(Value::FALSE)
    }

    fn op_push_closure(&mut self) -> Result<(), anyhow::Error> {
        let const_idx = self.chunk.read_next() as usize;
        let value = self.chunk.constants[const_idx];
        let fn_ref = value.as_object().as_function();
        let closure = self.create_closure(fn_ref);
        self.stack.push(Value::from(closure));
        Ok(())
    }

    fn op_push_class(&mut self) {
        let const_idx = self.chunk.read_next() as usize;
        let value = self.chunk.constants[const_idx];
        self.stack.push(value);
    }

    fn op_get_global(&mut self) -> Result<(), anyhow::Error> {
        let const_idx = self.chunk.read_next() as usize;
        let constant = &self.chunk.constants[const_idx];
        let string = constant.as_object().as_string();
        let value = self.globals[&string];
        self.stack.push(value);
        Ok(())
    }

    fn op_get_local(&mut self) {
        let idx = self.chunk.read_next() as usize;
        let value = unsafe { *self.frames.top.stack_start.add(idx) };
        self.stack.push(value);
    }

    fn op_set_local(&mut self) {
        let idx = self.chunk.read_next() as usize;
        unsafe {
            *self.frames.top.stack_start.add(idx) = self.stack.peek(0);
        };
    }

    fn op_get_upvalue(&mut self) {
        let idx = self.chunk.read_next() as usize;
        let upvalue = self.get_upvalue(idx);
        self.stack.push(unsafe { *(*upvalue).location });
    }

    fn op_set_upvalue(&mut self) {
        let idx = self.chunk.read_next() as usize;
        let upvalue = self.get_upvalue(idx);
        unsafe { *(*upvalue).location = self.stack.peek(0) };
    }

    fn op_get_property(&mut self) -> Result<(), anyhow::Error> {
        let const_idx = self.chunk.read_next() as usize;
        let value = self.stack.pop();
        if !matches!(value.kind(), ValueKind::Object(tag) if tag == objects::TAG_INSTANCE) {
            return self.error(format!("Invalid property access: {}", value.fmt()));
        }

        let object = value.as_object();
        let instance_ref = object.as_instance();
        let prop = self.chunk.constants[const_idx].as_object().as_string();

        if let Some(value) = self.get_property(instance_ref, prop) {
            self.stack.push(value);
            Ok(())
        } else {
            bail!("Property not found")
        }
    }

    fn op_set_property(&mut self) -> Result<(), anyhow::Error> {
        let const_idx = self.chunk.read_next() as usize;
        let value = self.stack.pop();
        if !matches!(value.kind(), ValueKind::Object(tag) if tag == objects::TAG_INSTANCE) {
            return self.error(format!("Invalid property access: {}", value.fmt()));
        }

        let object = value.as_object();
        let instance_ref = object.as_instance();
        let prop = self.chunk.constants[const_idx].as_object().as_string();

        let instance = unsafe { &mut *instance_ref };
        let class = unsafe { &*instance.class };
        match class.resolve(prop) {
            Some(ClassMember::Field(id)) => {
                let value = self.stack.peek(0);
                instance.values.insert(id, value);
                Ok(())
            },
            Some(ClassMember::Method(_)) => return self.error(format!("Cannot assign to instance method '{}'", unsafe { &*prop }.value)),
            None => bail!("No such property")
        }
    }

    fn op_get_property_by_id(&mut self) -> Result<(), anyhow::Error> {
        let member_id = self.chunk.read_next();
        let value = self.stack.pop();
        if !matches!(value.kind(), ValueKind::Object(tag) if tag == objects::TAG_INSTANCE) {
            return self.error(format!("Invalid property access: {}", value.fmt()));
        }

        let object = value.as_object();
        let instance_ref = object.as_instance();
        let value = self.get_property_by_id(instance_ref, member_id);
        self.stack.push(value);
        Ok(())
    }

    fn op_set_property_by_id(&mut self) -> Result<(), anyhow::Error> {
        let member_id = self.chunk.read_next();
        let value = self.stack.pop();
        if !matches!(value.kind(), ValueKind::Object(tag) if tag == objects::TAG_INSTANCE) {
            return self.error(format!("Invalid property access: {}", value.fmt()));
        }

        let object = value.as_object();
        let instance_ref = object.as_instance();
        let value = self.stack.peek(0);
        let instance = unsafe { &mut *instance_ref };
        instance.set(member_id, value);
        Ok(())
    }

    fn op_add(&mut self) -> Result<(), anyhow::Error> {
        let b = self.stack.pop();
        let a = self.stack.pop();
        let result = match (a.kind(), b.kind()) {
            (ValueKind::Number, ValueKind::Number) => Value::from(a.as_number() + b.as_number()),
            (ValueKind::Object(tag1), ValueKind::Object(tag2)) if tag1 == objects::TAG_STRING && tag2 == objects::TAG_STRING => {
                let sa = unsafe { &*a.as_object().as_string() }.value.as_str();
                let sb = unsafe { &*b.as_object().as_string() }.value.as_str();
                let s = [sa, sb].concat();
                Value::from(self.intern(s))
            },
            _ => {
                return self.error(format!("Operator '+' cannot be applied to operands {} and {}", a, b))
            }
        };
        self.stack.push(result);
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
        let value = self.stack.pop();
        if !value.is_number() {
            bail!("Invalid operand")
        }
        self.stack.push(Value::from(-value.as_number()));
        Ok(())
    }
    
    fn op_not(&mut self) -> Result<(), anyhow::Error> {
        let value = self.stack.pop();
        if !value.is_bool() {
            bail!("Invalid operand")
        }
        self.stack.push(Value::from(!value.as_bool()));
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
        let value = self.stack.pop();
        if !value.is_number() {
            bail!("Invalid operand")
        }
        self.stack.push(Value::from(!(value.as_number() as i64) as f64));
        Ok(())
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

use std::collections::HashMap;
use std::hash::BuildHasherDefault;

use anyhow::bail;
use rustc_hash::FxHasher;

use crate::Output;
use crate::parser::Parser;
use crate::vm::objects::{ObjBoundMethod, ObjInstance};
use crate::vm::value::ValueKind;
use crate::lexer::{tokenize, SourcePosition, TokenStream};

use super::chunk::BytecodeChunk;
use super::native::array::NativeArray;
use super::native::result::{NativeError, NativeOk, NativeResult};
use super::native::NativeType;
use super::opcode::{self, OpCode};
use super::stack::{CachedStack, Stack};
use super::value::Value;
use super::gc::{Gc, GcTraceable};
use super::objects::{self, ClassMember, NativeFn, ObjArray, ObjClass, ObjClosure, ObjFn, ObjNativeFn, ObjString, ObjUpvalue, Object, ObjectKind};
use super::compiler::Compiler;

const MAX_STACK: usize = 16384;
const MAX_FRAMES: usize = 256;

struct NativeTypes {
    array: *mut ObjClass,
    result: *mut ObjClass,
    ok_result: *mut ObjClass,
    error_result: *mut ObjClass
}

impl GcTraceable for NativeTypes {
    fn mark(&self, gc: &mut Gc) {
        gc.mark_object(self.array);
        gc.mark_object(self.result);
        gc.mark_object(self.ok_result);
        gc.mark_object(self.error_result);
    }
    
    fn fmt(&self) -> String {
        unreachable!()
    }
    
    fn size(&self) -> usize {
        unreachable!()
    }
}

#[derive(Clone, Copy)]
pub struct CallFrame {
    closure: *mut ObjClosure,
    return_ip: *const OpCode,
    stack_start: *mut Value
}

#[derive(Clone, Copy)]
pub struct InlambdaFrame {
    origin: *mut CallFrame
}

pub struct Vm {
    pub(crate) gc: Gc,
    ip: *const OpCode,
    chunk: BytecodeChunk,
    globals: HashMap<*mut ObjString, Value, BuildHasherDefault<FxHasher>>,
    pub(crate) stack: Stack<Value, MAX_STACK>,
    frames: CachedStack<CallFrame, MAX_FRAMES>,
    inlambdas: Vec<InlambdaFrame>,
    open_upvalues: Vec<*mut ObjUpvalue>,
    native_types: NativeTypes,
    out: Vec<String>
}

macro_rules! as_short {
    ($l:expr, $r:expr) => { ($l as u16) | (($r as u16) << 8) }
}

macro_rules! check_arity {
    ($vm:expr, $arg_count:expr, $arity:expr, $func_name:expr) => {
        if $arg_count != $arity as usize {
            let name = unsafe { &(*$func_name).value };
            return $vm.error(format!("{} expects {} arguments, but was called with {}", name, $arity, $arg_count));
        }
    }
}

#[cfg(debug_assertions)]
fn disassemble(chunk: &BytecodeChunk) {
    Output::println("=== Bytecode ===");
    Output::println(chunk.fmt());
    Output::println("================");
}

fn build_native_type(gc: &mut Gc, native_type: impl NativeType) -> *mut ObjClass {
    let class = native_type.build_class(gc);
    gc.alloc(class)
}

impl Vm {
    pub fn run(file_name: &str, src: &str) -> Result<Vec<String>, anyhow::Error> {
        let mut gc = Gc::new();
        let tokens = tokenize(String::from(file_name), String::from(src))?;
        let ast = Parser::parse(&mut TokenStream::new(&tokens))?;
        let chunk = Compiler::compile(&ast, &mut gc)?;

        #[cfg(debug_assertions)] {
            disassemble(&chunk);
        }

        let native_types = NativeTypes {
            array: build_native_type(&mut gc, NativeArray),
            result: build_native_type(&mut gc, NativeResult),
            ok_result: build_native_type(&mut gc, NativeOk),
            error_result: build_native_type(&mut gc, NativeError)
        };

        let mut vm = Vm {
            gc,
            ip: std::ptr::null(),
            chunk,
            globals: HashMap::default(),
            stack: Stack::new(),
            frames: CachedStack::new(),
            open_upvalues: Vec::new(),
            inlambdas: Vec::new(),
            native_types,
            out: Vec::new()
        };

        // Define initializable native types
        vm.globals.insert(vm.gc.intern("Error"), vm.native_types.error_result.into());
        vm.globals.insert(vm.gc.intern("Ok"), vm.native_types.ok_result.into());

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
            vm.out.push(value_str.clone());
            Output::println(value_str);
            vm.stack.push(Value::NULL);
            Ok(())
        });

        vm.define_native("time", 0, |vm, _target, _args| {
            let time = std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH).unwrap().as_millis() as f64;
            vm.stack.push(Value::from(time));
            Ok(())
        });

        vm.define_native("gcHeapSize", 0, |vm, _target, _args| {
            vm.stack.push(Value::from(vm.gc.bytes_allocated as f64));
            Ok(())
        });

        vm.define_native("gcCollect", 0, |vm, _target, _args| {
            vm.start_gc();
            vm.stack.push(Value::NULL);
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

    fn create_closure(&mut self, function: *mut ObjFn) -> Object {
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

        self.alloc(closure).into()
    }

    fn get_instance_property(&mut self, instance_ptr: *mut ObjInstance, prop: *mut ObjString) -> Option<Value> {
        let instance = unsafe { &*instance_ptr };
        let class = unsafe { &*instance.class };

        match class.resolve(prop) {
            Some(ClassMember::Field(id)) => Some(unsafe { (*instance_ptr).get(id) }),
            Some(ClassMember::Method(id)) => {
                let method = class.get_method(id);
                match method.tag() {
                    objects::TAG_FUNCTION => {
                        let closure = self.create_closure(method.as_function_ptr());
                        let bound_method = self.alloc(ObjBoundMethod::new(instance_ptr.into(), closure));
                        return Some(Value::from(bound_method));
                    },
                    objects::TAG_NATIVE_FUNCTION => {
                        let bound_method = self.alloc(ObjBoundMethod::new(instance_ptr.into(), method));
                        return Some(Value::from(bound_method));
                    },
                    _ => unreachable!()
                }
            },
            None => None
        }
    }

    fn get_property_by_id(&mut self, instance_ref: *mut ObjInstance, id: u8) -> Value {
        let value = unsafe { (*instance_ref).get(id) };
        match value.kind() {
            ValueKind::Object(ObjectKind::Function) => {
                let closure = self.create_closure(value.as_object().as_function_ptr());
                let method = self.alloc(ObjBoundMethod::new(instance_ref.into(), closure));
                Value::from(method)
            },
            _ => value
        }
    }

    fn push_frame(&mut self, closure: *mut ObjClosure, stack_start: *mut Value, ip_start: usize) {
        self.frames.push(CallFrame {
            closure,
            return_ip: self.ip,
            stack_start
        });
        self.ip = unsafe { self.chunk.code.as_ptr().offset(ip_start as isize) };
    }

    pub(crate) fn call(&mut self, arg_count: usize, value: Value) -> Result<(), anyhow::Error> {
        if !value.is_callable() {
            return self.error(format!("{} is not callable", value.fmt()));
        }
    
        let object = value.as_object();
        let tag = object.tag();
    
        match tag {
            objects::TAG_CLOSURE => self.call_closure(arg_count, object.as_closure_ptr()),
            objects::TAG_NATIVE_FUNCTION => self.call_native(arg_count, object.as_native_function_ptr()),
            objects::TAG_BOUND_METHOD => self.call_bound_method(arg_count, object.as_bound_method_ptr()),
            objects::TAG_CLASS => self.call_class(arg_count, object.as_class_ptr()),
            _ => unreachable!()
        }
    }

    fn call_native(&mut self, arg_count: usize, native_fn_ptr: *mut ObjNativeFn) -> Result<(), anyhow::Error> {
        let func = unsafe { &*native_fn_ptr };
        check_arity!(self, arg_count, func.arity as usize, func.name);
        let args = self.stack.pop_slice(arg_count);
        // The "target" is the first value in a call window. For method calls, this is the instance.
        let target = self.stack.pop();
        match (func.function)(self, target, args) {
            Ok(_) => Ok(()),
            Err(err) => self.error(err.downcast::<String>()?)
        }
    }

    fn call_closure(&mut self, arg_count: usize, closure_ptr: *mut ObjClosure) -> Result<(), anyhow::Error> {
        let closure = unsafe { &*closure_ptr };
        check_arity!(self, arg_count, closure.arity, closure.name);
        self.push_frame(closure_ptr, self.stack.offset(arg_count), closure.ip_start);
        Ok(())
    }

    fn call_bound_method(&mut self, arg_count: usize, bound_method_ptr: *mut ObjBoundMethod) -> Result<(), anyhow::Error> {
        let bound_method = unsafe { &*bound_method_ptr };
        let method = bound_method.method;
        match method.tag() {
            objects::TAG_CLOSURE => {
                let closure_ptr = method.as_closure_ptr();
                let closure = unsafe { &*closure_ptr };
                check_arity!(self, arg_count, closure.arity, closure.name);
                let stack_start = self.stack.set(arg_count, Value::from(bound_method.target));
                self.push_frame(closure_ptr, stack_start, closure.ip_start);
            },
            objects::TAG_NATIVE_FUNCTION => {
                self.stack.set(arg_count, Value::from(bound_method.target));
                self.call_native(arg_count, method.as_native_function_ptr())?;
            },
            _ => unreachable!()
        };
        Ok(())
    }

    fn call_class(&mut self, arg_count: usize, class_ptr: *mut ObjClass) -> Result<(), anyhow::Error> {
        let class = unsafe { &*class_ptr };
        let init_method_obj = class.resolve_method(self.gc.preset_identifiers.init).unwrap();
        match init_method_obj.tag() {
            objects::TAG_FUNCTION => {
                let init_method_ref = init_method_obj.as_function_ptr();
                let init_method = unsafe { &*init_method_ref };
                check_arity!(self, arg_count, init_method.arity, init_method.name);
                
                let closure = self.create_closure(init_method_ref);
                let instance = self.alloc(ObjInstance::new(class_ptr));
                let stack_start = self.stack.set(arg_count, Value::from(instance));
                self.push_frame(closure.as_closure_ptr(), stack_start, init_method.ip_start);
                Ok(())
            },
            objects::TAG_NATIVE_FUNCTION => {
                self.call_native(arg_count, init_method_obj.as_native_function_ptr())?;
                return Ok(());
            },
            _ => unreachable!()
        }
    }

    fn get_native_type_index(&mut self, native_class_ptr: *mut ObjClass, target: Value, prop: Value) -> Result<(), anyhow::Error> {
        let native_class = unsafe { &*native_class_ptr };

        if matches!(prop.kind(), ValueKind::Object(ObjectKind::String)) {
            let Some(method) = native_class.resolve_method(prop.as_object().as_string_ptr()) else {
                return match native_class_ptr {
                    _ if native_class_ptr == self.native_types.array => self.error(format!("Invalid array index: {}", prop.fmt())),
                    _ => self.error(format!("Invalid index: {} does not have method {}", target.fmt(), prop.fmt())),
                }
            };
            
            let bound_method = self.alloc(ObjBoundMethod::new(target, method));
            self.stack.push(Value::from(bound_method));
            return Ok(());
        }

        let getter = native_class.resolve_method(self.gc.preset_identifiers.get).unwrap();
        self.stack.push(target);
        self.stack.push(prop);
        return self.call_native(1, getter.as_native_function_ptr());
    }

    fn set_native_type_index(&mut self, native_class_ptr: *mut ObjClass, target: Value, prop: Value) -> Result<(), anyhow::Error> {
        let native_class = unsafe { &*native_class_ptr };
        let setter = native_class.resolve_method(self.gc.preset_identifiers.set).unwrap();
        let value = self.stack.pop();
        self.stack.push(target);
        self.stack.push(prop);
        self.stack.push(value);
        return self.call_native(2, setter.as_native_function_ptr());
    }
    
    fn get_instance_index(&mut self, target: Value, prop: Value) -> Result<(), anyhow::Error> {
        let instance_ref = target.as_object().as_instance_ptr();

        if matches!(prop.kind(), ValueKind::Object(ObjectKind::String)) {
            if let Some(value) = self.get_instance_property(instance_ref, prop.as_object().as_string_ptr()) {
                self.stack.push(value);
                return Ok(());
            }
        }

        let class = unsafe { &*(*instance_ref).class };

        if let Some(getter) = class.resolve_method(self.gc.preset_identifiers.get) {
            self.stack.push(target);
            self.stack.push(prop);

            match getter.tag() {
                objects::TAG_FUNCTION => {
                    let func_ptr = getter.as_function_ptr();
                    let closure = self.create_closure(func_ptr);
                    let ip_start = unsafe { (*func_ptr).ip_start };
                    self.push_frame(closure.as_closure_ptr(), self.stack.offset(1), ip_start);
                    return Ok(());
                },
                objects::TAG_NATIVE_FUNCTION => {
                    return self.call_native(1, getter.as_native_function_ptr());
                },
                _ => unreachable!()
            }
        }

        self.error(format!(
            "Invalid index: {} doesn't have member {} and doesn't have a getter",
            unsafe { &*class.name }.value,
            prop.fmt()
        ))
    }
    
    fn set_instance_index(&mut self, prop: Value, target: Value) -> Result<(), anyhow::Error> {
        let instance_ref = target.as_object().as_instance_ptr();
        let instance = unsafe { &mut *instance_ref };
        let class = unsafe { &*instance.class };

        if matches!(prop.kind(), ValueKind::Object(ObjectKind::String)) {
            let member = class.resolve(prop.as_object().as_string_ptr());
    
            if let Some(member) = member {
                if let ClassMember::Field(id) = member {
                    let value = self.stack.peek(0);
                    instance.values.insert(id, value);
                    return Ok(());
                }
    
                return self.error(format!("Cannot assign to method '{}'", prop.as_object().as_string()));
            }
        }

        if let Some(setter) = class.resolve_method(self.gc.preset_identifiers.set) {
            let value = self.stack.pop();
            self.stack.push(target);
            self.stack.push(prop);
            self.stack.push(value);

            match setter.tag() {
                objects::TAG_FUNCTION => {
                    let func_ptr = setter.as_function_ptr();
                    let closure = self.create_closure(func_ptr);
                    let ip_start = unsafe { (*func_ptr).ip_start };
                    self.push_frame(closure.as_closure_ptr(), self.stack.offset(2), ip_start);
                    return Ok(());
                },
                objects::TAG_NATIVE_FUNCTION => {
                    return self.call_native(2, setter.as_native_function_ptr());
                },
                _ => unreachable!()
            }
        }

        self.error(format!(
            "Invalid index: {} doesn't have member {} and doesn't have a setter",
            unsafe { &*class.name }.value,
            prop.fmt()
        ))
    }

    fn interpret(mut self) -> Result<Vec<String>, anyhow::Error> {
        loop {
            let op = self.read_next();
            match op {
                opcode::CALL => self.op_call()?,
                opcode::JUMP => self.op_jump(),
                opcode::JUMP_IF_FALSE => self.op_jump_if_false(),
                opcode::CLOSE_UPVALUE => self.op_close_upvalue(),
                opcode::ARRAY => self.op_array(),
                opcode::RETURN => {
                    if !self.op_return() {
                        return Ok(self.out);
                    }
                },
                opcode::POP => self.op_pop(),
                opcode::POP_INLAMBDA => self.op_pop_inlambda(),
                opcode::PUSH_CONSTANT => self.op_push_constant(),
                opcode::PUSH_NULL => self.op_push_null(),
                opcode::PUSH_TRUE => self.op_push_true(),
                opcode::PUSH_FALSE => self.op_push_false(),
                opcode::PUSH_CLOSURE => self.op_push_closure()?,
                opcode::PUSH_INLAMBDA => self.op_push_inlambda()?,
                opcode::PUSH_CLASS => self.op_push_class(),
                opcode::GET_GLOBAL => self.op_get_global()?,
                opcode::SET_GLOBAL => unreachable!(),
                opcode::GET_LOCAL => self.op_get_local(),
                opcode::SET_LOCAL => self.op_set_local(),
                opcode::GET_UPVALUE => self.op_get_upvalue(),
                opcode::SET_UPVALUE => self.op_set_upvalue(),
                opcode::GET_INDEX => self.op_get_index()?,
                opcode::SET_INDEX => self.op_set_index()?,
                opcode::GET_PROPERTY_ID => self.op_get_property_by_id()?,
                opcode::SET_PROPERTY_ID => self.op_set_property_by_id()?,
                opcode::ADD => self.op_add()?,
                opcode::SUBTRACT => self.op_subtract()?,
                opcode::MULTIPLY => self.op_multiply()?,
                opcode::DIVIDE => self.op_divide()?,
                opcode::NEGATE => self.op_negate()?,
                opcode::LEFT_SHIFT => self.op_left_shift()?,
                opcode::RIGHT_SHIFT => self.op_right_shift()?,
                opcode::BIT_AND => self.op_bit_and()?,
                opcode::BIT_OR => self.op_bit_or()?,
                opcode::BIT_XOR => self.op_bit_xor()?,
                opcode::BIT_NOT => self.op_bit_not()?,
                opcode::EQUAL => self.op_equal()?,
                opcode::NOT_EQUAL => self.op_not_equal()?,
                opcode::LESS_THAN => self.op_less_than()?,
                opcode::LESS_THAN_EQUAL => self.op_less_than_equal()?,
                opcode::GREATER_THAN => self.op_greater_than()?,
                opcode::GREATER_THAN_EQUAL => self.op_greater_than_equal()?,
                opcode::NOT => self.op_not()?,
                opcode::AND => self.op_and()?,
                opcode::OR => self.op_or()?,
                _ => unreachable!()
            }
        }
    }
    
    
    fn op_return(&mut self) -> bool {
        if self.frames.len() == 1 {
            return false;
        }

        if self.inlambdas.len() > 0 {
            let inlambda = self.inlambdas.pop().unwrap();
            self.frames.set_top(inlambda.origin);
        }

        let frame = self.frames.pop();

        self.ip = frame.return_ip;
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
        let location = self.read_next() as usize;
        let p = unsafe { self.frames.top.stack_start.add(location) };
        self.close_upvalues(p);
        self.stack.truncate(1);
    }

    fn op_array(&mut self) {
        let len = self.read_next() as usize;
        let array = ObjArray::new(self.stack.pop_slice(len));
        let array = self.alloc(array);
        self.stack.push(Value::from(array));
    }

    fn op_call(&mut self) -> Result<(), anyhow::Error> {
        let arg_count = self.read_next() as usize;
        let value = self.stack.peek(arg_count);
        self.call(arg_count, value)
    }
    
    fn op_jump(&mut self) {
        let offset = as_short!(self.read_next(), self.read_next()) as isize;
        self.ip = unsafe { self.chunk.code.as_ptr().offset(offset) };
    }

    fn op_jump_if_false(&mut self) {
        let offset = as_short!(self.read_next(), self.read_next()) as isize;
        let value = self.stack.pop();
        if value.is_bool() && !value.as_bool() {
            self.ip = unsafe { self.chunk.code.as_ptr().offset(offset) };
        }
    }

    fn op_push_constant(&mut self) {
        let const_idx = self.read_next() as usize;
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
        let const_idx = self.read_next() as usize;
        let value = self.chunk.constants[const_idx];
        let fn_ref = value.as_object().as_function_ptr();
        let closure = self.create_closure(fn_ref);
        self.stack.push(Value::from(closure));
        Ok(())
    }

    fn op_push_inlambda(&mut self) -> Result<(), anyhow::Error> {
        let const_idx = self.read_next() as usize;
        let value = self.chunk.constants[const_idx];
        let fn_ref = value.as_object().as_function_ptr();
        let closure = self.create_closure(fn_ref);
        self.inlambdas.push(InlambdaFrame {
            origin: self.frames.top_ptr()
        });
        self.stack.push(Value::from(closure));
        Ok(())
    }

    fn op_pop_inlambda(&mut self) {
        self.inlambdas.pop();
        let frame = self.frames.pop();

        self.ip = frame.return_ip;
        self.close_upvalues(frame.stack_start);

        let value = self.stack.pop();
        self.stack.set_top(frame.stack_start);
        self.stack.push(value);
    }

    fn op_push_class(&mut self) {
        let const_idx = self.read_next() as usize;
        let value = self.chunk.constants[const_idx];
        self.stack.push(value);
    }

    fn op_get_global(&mut self) -> Result<(), anyhow::Error> {
        let const_idx = self.read_next() as usize;
        let constant = &self.chunk.constants[const_idx];
        let string = constant.as_object().as_string_ptr();
        let value = self.globals[&string];
        self.stack.push(value);
        Ok(())
    }

    fn op_get_local(&mut self) {
        let idx = self.read_next() as usize;
        let value = unsafe { *self.frames.top.stack_start.add(idx) };
        self.stack.push(value);
    }

    fn op_set_local(&mut self) {
        let idx = self.read_next() as usize;
        unsafe {
            *self.frames.top.stack_start.add(idx) = self.stack.peek(0);
        };
    }

    fn op_get_upvalue(&mut self) {
        let idx = self.read_next() as usize;
        let upvalue = self.get_upvalue(idx);
        self.stack.push(unsafe { *(*upvalue).location });
    }

    fn op_set_upvalue(&mut self) {
        let idx = self.read_next() as usize;
        let upvalue = self.get_upvalue(idx);
        unsafe { *(*upvalue).location = self.stack.peek(0) };
    }

    fn op_get_index(&mut self) -> Result<(), anyhow::Error> {
        let prop = self.stack.pop();
        let target = self.stack.pop();
        let ValueKind::Object(object_kind) = target.kind() else {
            return self.error(format!("Invalid property access: {}", target.fmt()));
        };

        match object_kind {
            ObjectKind::Instance => self.get_instance_index(target, prop),
            ObjectKind::Array => self.get_native_type_index(self.native_types.array, target, prop),
            ObjectKind::Result => self.get_native_type_index(self.native_types.result, target, prop),
            _ => self.error(format!("Invalid property access: {}", target.fmt()))
        }
    }
    
    fn op_set_index(&mut self) -> Result<(), anyhow::Error> {
        let prop = self.stack.pop();
        let target = self.stack.pop();
        let ValueKind::Object(object_kind) = target.kind() else {
            return self.error(format!("Invalid property access: {}", target.fmt()));
        };

        match object_kind {
            ObjectKind::Instance => self.set_instance_index(prop, target),
            ObjectKind::Array => self.set_native_type_index(self.native_types.array, target, prop),
            _ => self.error(format!("Invalid property access: {}", target.fmt()))
        }
    }
    
    fn op_get_property_by_id(&mut self) -> Result<(), anyhow::Error> {
        let member_id = self.read_next();
        let value = self.stack.pop();
        if !matches!(value.kind(), ValueKind::Object(ObjectKind::Instance)) {
            return self.error(format!("Invalid property access: {}", value.fmt()));
        }

        let object = value.as_object();
        let instance_ref = object.as_instance_ptr();
        let value = self.get_property_by_id(instance_ref, member_id);
        self.stack.push(value);
        Ok(())
    }

    fn op_set_property_by_id(&mut self) -> Result<(), anyhow::Error> {
        let member_id = self.read_next();
        let value = self.stack.pop();
        if !matches!(value.kind(), ValueKind::Object(ObjectKind::Instance)) {
            return self.error(format!("Invalid property access: {}", value.fmt()));
        }

        let object = value.as_object();
        let instance_ref = object.as_instance_ptr();
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

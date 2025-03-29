use anyhow::bail;
use nohash_hasher::IntMap;

use crate::bytecode::objects::{BoundMethod, Instance};
use crate::lexer::{tokenize, TokenStream};

use super::parser::Parser;
use super::OpCode;
use super::gc::{Gc, GcRef, GcTraceable};
use super::objects::{BytecodeChunk, Class, ClassMember, Closure, Function, NativeFunction, Upvalue, Value};
use super::compiler::Compiler;

const MAX_STACK: usize = 16384;

pub struct CallFrame {
    return_ip: *const OpCode,
    stack_start: *mut Value
}

pub struct Vm<'out> {
    gc: Gc,
    ip: *const OpCode,
    chunk: BytecodeChunk,
    globals: IntMap<GcRef<String>, Value>,
    stack: [Value; 16384],
    stack_top: *mut Value,
    frames: Vec<CallFrame>,
    open_upvalues: Vec<GcRef<Upvalue>>,
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

        println!("=== Bytecode ===");
        print!("{}", chunk.fmt(&gc));
        println!("================");

        let mut out = Vec::new();
        let mut vm = Vm {
            gc,
            ip: chunk.code.as_ptr(),
            chunk,
            globals: IntMap::default(),
            stack: [Value::Null; MAX_STACK],
            stack_top: std::ptr::null_mut(),
            frames: Vec::new(),
            open_upvalues: Vec::new(),
            out: &mut out
        };

        vm.stack_top = vm.stack.as_mut_ptr();
        vm.frames.push(CallFrame {
            return_ip: std::ptr::null(),
            stack_start: vm.stack_top
        });

        vm.define_native("print", 1, |vm| {
            let value_str = match vm.peek(0) {
                Value::Null => String::from("null"),
                Value::Number(num) => num.to_string(),
                Value::Boolean(b) => b.to_string(),
                Value::String(gc_ref) => vm.gc.get(&gc_ref).clone(),
                Value::Function(gc_ref) => match vm.gc.get(&gc_ref) {
                    Function { name, .. } => format!("<fn {}>", vm.gc.get(name))
                },
                Value::Closure(gc_ref) => match vm.gc.get(&gc_ref) {
                    Closure { function, .. } => format!("<closure {}>", vm.gc.get(&vm.gc.get(function).name))
                },
                Value::BoundMethod(gc_ref) => match vm.gc.get(&gc_ref) {
                    BoundMethod { closure, .. } => format!("<bound method {}>", vm.gc.get(&vm.gc.get(&vm.gc.get(closure).function).name))
                },
                Value::NativeFunction(gc_ref) => match vm.gc.get(&gc_ref) {
                    NativeFunction { name, .. } => format!("<native fn {}>", vm.gc.get(name))
                },
                Value::Class(gc_ref) => match vm.gc.get(&gc_ref) {
                    Class { name, .. } => format!("<class {}>", vm.gc.get(name))
                },
                Value::Instance(gc_ref) => match vm.gc.get(&gc_ref) {
                    Instance { class, .. } => format!("<instance {}>", vm.gc.get(&vm.gc.get(class).name))
                }
            };
            vm.out.push(value_str.clone());
            println!("{}", value_str);
            vm.push(Value::Null);
        });

        vm.define_native("time", 0, |vm| {
            let time = std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH).unwrap().as_millis() as f64;
            vm.push(Value::Number(time));
        });

        vm.define_native("gcHeapSize", 0, |vm| {
            vm.push(Value::Number(vm.gc.bytes_allocated as f64));
        });

        vm.define_native("gcCollect", 0, |vm| {
            vm.start_gc();
            vm.push(Value::Null);
        });

        vm.interpret()?;
        Ok(out)
    }

    fn error(&self, message: impl Into<String>) -> Result<(), anyhow::Error> {
        let trace = self.frames[1..].iter().rev()
            .map(|frame| {
                let function = self.get_closure(&frame).function;
                let function = self.gc.get(&function);
                let name = self.gc.get(&function.name);
                let pos = unsafe { 
                    let idx = frame.return_ip.offset_from(self.chunk.code.as_ptr());
                    &self.chunk.code_pos[idx as usize]
                };
                format!("at {} ({})", name, pos)
            })
            .collect::<Vec<String>>().join("\n");

        let pos = unsafe { 
            let idx = self.ip.offset_from(self.chunk.code.as_ptr()) as usize - 1;
            &self.chunk.code_pos[idx]
        };

        bail!(format!("{}\nat {}\n{}", message.into(), pos, trace))
    }

    fn intern(&mut self, name: impl Into<String>) -> GcRef<String> {
        if self.gc.should_collect() {
            self.start_gc();
        }

        self.gc.intern(name)
    }

    fn alloc<T: GcTraceable + 'static>(&mut self, obj: T) -> GcRef<T> {
        if self.gc.should_collect() {
            self.start_gc();
        }

        self.gc.alloc(obj)
    }

    fn define_native(&mut self, name: impl Into<String>, arity: u8, function: fn(&mut Vm)) {
        let name_ref = self.gc.intern(name.into());
        let native = NativeFunction {
            name: name_ref,
            arity,
            function
        };

        let value = Value::NativeFunction(self.gc.alloc(native));
        self.globals.insert(name_ref, value);
    }

    fn start_gc(&mut self) {
        self.chunk.mark_refs(&mut self.gc);

        for (name, value) in &self.globals {
            self.gc.mark_object(name);
            value.mark_refs(&mut self.gc);
        }

        for upvalue in &self.open_upvalues {
            self.gc.mark_object(upvalue);
        }

        let mut p = self.stack.as_ptr();
        while p != self.stack_top {
            unsafe {
                (*p).mark_refs(&mut self.gc);
                p = p.offset(1);
            }
        }

        self.gc.collect();
    }

    fn current_stack_start(&mut self) -> *mut Value {
        match self.frames.last() {
            Some(frame) => frame.stack_start,
            None => self.stack.as_mut_ptr()
        }
    }

    fn current_closure(&self) -> &Closure {
        self.get_closure(&self.frames[self.frames.len() - 1])
    }

    fn get_closure(&self, frame: &CallFrame) -> &Closure {
        let value = unsafe { *frame.stack_start };
        let Value::Closure(closure_ref) = value else {
            unreachable!()
        };
        self.gc.get(&closure_ref)
    }

    fn capture_upvalue(&mut self, location: u8) -> GcRef<Upvalue> {
        let mut i = 0;
        while i < self.open_upvalues.len() {
            let upvalue_ref = &self.open_upvalues[i];
            let upvalue = self.gc.get(upvalue_ref);
            if upvalue.location == location {
                return *upvalue_ref;
            }

            if upvalue.location > location {
                break;
            }

            i += 1;
        }

        let current_offset = unsafe { self.current_stack_start().offset_from(self.stack.as_ptr()) };
        let stack_location = current_offset as u8 + location;
        let upvalue = self.alloc(Upvalue { location: stack_location, closed: None });
        self.open_upvalues.insert(i, upvalue);
        upvalue
    }

    fn close_upvalues(&mut self, after: *const Value) {
        let offset = unsafe { after.offset_from(self.stack.as_ptr()) };
        let mut split_idx = 0;
        for i in 0..self.open_upvalues.len() {
            let location = self.gc.get(&self.open_upvalues[i]).location;
            if location >= offset as u8 {
                let value = self.get(location);
                let upvalue = self.gc.get_mut(&self.open_upvalues[i]);
                upvalue.closed = Some(value);

                if split_idx == 0 {
                    split_idx = i;
                }
            }
        }

        self.open_upvalues.truncate(split_idx);
    }

    #[inline]
    fn get(&self, idx: impl Into<usize>) -> Value {
        let idx = idx.into();
        self.stack[idx]
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
    fn set(&mut self, idx: impl Into<usize>, value: Value) {
        let idx = idx.into();
        assert!(idx < self.stack.len());
        self.stack[idx] = value;
    }

    #[inline]
    fn truncate(&mut self, count: usize) {
        self.stack_top = unsafe { self.stack_top.sub(count) };
    }

    fn create_closure(&mut self, function: &GcRef<Function>) -> GcRef<Closure> {
        let mut closure = Closure::new(*function);
        let upvalue_count = self.gc.get(function).upvalues.len();

        for i in 0..upvalue_count {
            let fn_upval = &self.gc.get(function).upvalues[i];
            let upvalue = if fn_upval.is_local {
                self.capture_upvalue(fn_upval.location)
            } else {
                let closure = self.current_closure();
                closure.upvalues[fn_upval.location as usize]
            };

            closure.upvalues.push(upvalue);
        }

        self.alloc(closure)
    }

    fn get_property(&mut self, instance_ref: &GcRef<Instance>, prop: &GcRef<String>) -> Option<Value> {
        let instance = self.gc.get(instance_ref);
        let class = self.gc.get(&instance.class);

        match class.resolve(prop) {
            Some(ClassMember::Field(id)) |
            Some(ClassMember::Method(id)) => Some(self.get_property_by_id(instance_ref, id)),
            None => None
        }
    }

    fn get_property_by_id(&mut self, instance_ref: &GcRef<Instance>, id: u8) -> Value {
        let instance = self.gc.get(instance_ref);

        match instance.get(id, &self.gc) {
            Value::Function(func_ref) => {
                let closure = self.create_closure(&func_ref);
                let method = self.alloc(BoundMethod { closure, instance: *instance_ref });
                Value::BoundMethod(method)
            },
            value => value
        }
    }

    fn call_native(&mut self, arg_count: usize, gc_ref: &GcRef<NativeFunction>) -> Result<(), anyhow::Error> {
        let func = self.gc.get(gc_ref).function;
        func(self);
        let result = self.peek(0);
        self.truncate(arg_count + 2);
        self.push(result);
        Ok(())
    }

    fn call_closure(&mut self, arg_count: usize, gc_ref: &GcRef<Closure>) -> Result<(), anyhow::Error> {
        let closure = self.gc.get(gc_ref);
        let function = self.gc.get(&closure.function);
        self.frames.push(CallFrame {
            return_ip: self.ip, 
            stack_start: unsafe { self.stack_top.sub(arg_count + 1) }
        });
        self.ip = unsafe { self.chunk.code.as_ptr().offset(function.ip_start as isize) };
        Ok(())
    }

    fn call_bound_method(&mut self, arg_count: usize, gc_ref: &GcRef<BoundMethod>) -> Result<(), anyhow::Error> {
        let instance = self.gc.get(gc_ref).instance;

        let stack_start = unsafe { self.stack_top.sub(arg_count + 1) };
        unsafe {
            *stack_start = Value::Instance(instance);
        };

        self.frames.push(CallFrame {
            return_ip: self.ip, 
            stack_start
        });

        let closure = self.gc.get(gc_ref).closure;
        let function = self.gc.get(&closure).function;
        self.ip = unsafe { self.chunk.code.as_ptr().offset(self.gc.get(&function).ip_start as isize) };
        Ok(())
    }

    fn call_class(&mut self, arg_count: usize, class_ref: &GcRef<Class>) -> Result<(), anyhow::Error> {
        let init_str = self.gc.intern("init");
        let init_id = self.gc.get(class_ref).resolve_id(&init_str).unwrap();
        let init_ref = self.gc.get(class_ref).get_method(init_id).unwrap();
        
        let closure = self.create_closure(&init_ref);
        self.push(Value::Closure(closure));

        let mut instance = Instance::new(*class_ref);
        let class = self.gc.get(class_ref);
        for field in &class.fields {
            instance.values.insert(*field, Value::Null);
        }

        let instance = self.alloc(instance);
        let stack_start = unsafe { self.stack_top.sub(arg_count + 1) };
        unsafe {
            *stack_start = Value::Instance(instance);
        };

        let function = self.gc.get(&init_ref);
        self.frames.push(CallFrame {
            return_ip: self.ip, 
            stack_start
        });
        self.ip = unsafe { self.chunk.code.as_ptr().offset(function.ip_start as isize) };
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
                OpCode::Not => self.op_not()?
            }

            // for i in 0..self.stack_top {
            //     print!("{} ", self.get(i).fmt(&self.gc));
            // }
            // println!();
        }
    }
    
    fn op_return(&mut self) -> bool {
        let value = self.pop_get();
        let frame = self.frames.pop().unwrap();

        if self.frames.is_empty() {
            return false;
        }

        self.ip = frame.return_ip;
        self.close_upvalues(frame.stack_start);

        self.stack_top = frame.stack_start;
        self.push(value);
        true
    }

    fn op_pop(&mut self) {
        self.pop();
    }

    fn op_close_upvalue(&mut self, location: u8) {
        let p = unsafe { self.current_stack_start().add(location as usize) };
        self.close_upvalues(p);
        self.pop();
    }

    fn op_call(&mut self, arg_count: u8) -> Result<(), anyhow::Error> {
        let arg_count = arg_count as usize;
        let function = self.peek(arg_count);
        Ok(match &function {
            Value::NativeFunction(gc_ref) => self.call_native(arg_count, gc_ref)?,
            Value::Closure(gc_ref) => self.call_closure(arg_count, gc_ref)?,
            Value::BoundMethod(gc_ref) => self.call_bound_method(arg_count, gc_ref)?,
            Value::Class(class_ref) => self.call_class(arg_count, class_ref)?,
            _ => unreachable!()
        })
    }

    fn op_jump(&mut self, lpos: u8, rpos: u8) {
        let offset = as_short!(lpos, rpos) as isize;
        self.ip = unsafe { self.chunk.code.as_ptr().offset(offset) };
    }

    fn op_jump_if_false(&mut self, lpos: u8, rpos: u8) {
        let offset = as_short!(lpos, rpos) as isize;
        if let Value::Boolean(false) = self.pop_get() {
            self.ip = unsafe { self.chunk.code.as_ptr().offset(offset) };
        }
    }

    fn op_push_constant(&mut self, const_idx: u8) {
        let value = self.chunk.constants[const_idx as usize].clone();
        self.push(value);
    }

    fn op_push_null(&mut self) {
        self.push(Value::Null)
    }

    fn op_push_true(&mut self) {
        self.push(Value::Boolean(true))
    }

    fn op_push_false(&mut self) {
        self.push(Value::Boolean(false))
    }

    fn op_push_closure(&mut self, const_idx: u8) -> Result<(), anyhow::Error> {
        let value = self.chunk.constants[const_idx as usize];
        let Value::Function(fn_ref) = value else {
            bail!("Invalid closure")
        };
        let closure = self.create_closure(&fn_ref);
        self.push(Value::Closure(closure));
        Ok(())
    }

    fn op_push_class(&mut self, const_idx: u8) {
        let value = self.chunk.constants[const_idx as usize];
        self.push(value);
    }

    fn op_get_global(&mut self, const_idx: u8) -> Result<(), anyhow::Error> {
        let constant = &self.chunk.constants[const_idx as usize];
        let global_name = match constant {
            Value::String(gc_ref) => gc_ref,
            _ => bail!("Invalid global name")
        };
        let value = self.globals[global_name];
        self.push(value);
        Ok(())
    }

    fn op_set_global(&mut self, const_idx: u8) -> Result<(), anyhow::Error> {
        let constant = &self.chunk.constants[const_idx as usize];
        let global_name = match constant {
            Value::String(gc_ref) => *gc_ref,
            _ => bail!("Invalid global name")
        };
        let value = self.pop_get();
        self.globals.insert(global_name, value);
        Ok(())
    }

    fn op_get_local(&mut self, idx: u8) {
        let value = unsafe { *self.current_stack_start().add(idx as usize) };
        self.push(value);
    }

    fn op_set_local(&mut self, idx: u8) {
        unsafe {
            *self.current_stack_start().add(idx as usize) = self.peek(0);
        };
    }

    fn op_get_upvalue(&mut self, idx: u8) {
        let upvalue = self.current_closure().upvalues[idx as usize];
        let upvalue = self.gc.get(&upvalue);
        if let Some(value) = upvalue.closed {
            self.push(value);
        } else {
            let value = self.get(upvalue.location);
            self.push(value);
        }
    }

    fn op_set_upvalue(&mut self, idx: u8) {
        let upvalue = self.current_closure().upvalues[idx as usize];
        if let Some(_) = self.gc.get(&upvalue).closed {
            self.gc.get_mut(&upvalue).closed = Some(self.peek(0));
        } else {
            self.set(self.gc.get(&upvalue).location, self.peek(0));
        }
    }

    fn op_get_property(&mut self, const_idx: u8) -> Result<(), anyhow::Error> {
        let Value::Instance(instance_ref) = self.pop_get() else {
            bail!("Invalid property access")
        };
        let Value::String(prop) = self.chunk.constants[const_idx as usize] else {
            bail!("Invalid property access")
        };

        if let Some(value) = self.get_property(&instance_ref, &prop) {
            self.push(value.clone());
            Ok(())
        } else {
            bail!("Property not found")
        }
    }

    fn op_set_property(&mut self, const_idx: u8) -> Result<(), anyhow::Error> {
        let Value::Instance(instance_ref) = self.pop_get() else {
            bail!("Invalid property access")
        };
        let Value::String(prop) = self.chunk.constants[const_idx as usize] else {
            bail!("Invalid property access")
        };
        let class_ref = self.gc.get(&instance_ref).class;
        let class = self.gc.get(&class_ref);
        match class.resolve(&prop) {
            Some(ClassMember::Field(id)) => {
                let value = self.peek(0);
                let instance = self.gc.get_mut(&instance_ref);
                instance.values.insert(id, value);
                Ok(())
            },
            Some(ClassMember::Method(_)) => {
                bail!("Cannot set method")
            },
            None => {
                bail!("No such property")
            }
        }
    }

    fn op_get_property_by_id(&mut self, member_id: u8) -> Result<(), anyhow::Error> {
        let Value::Instance(instance_ref) = self.pop_get() else {
            bail!("Invalid property access")
        };
        let value = self.get_property_by_id(&instance_ref, member_id);
        self.push(value);
        Ok(())
    }

    fn op_set_property_by_id(&mut self, member_id: u8) -> Result<(), anyhow::Error> {
        let Value::Instance(instance_ref) = self.pop_get() else {
            bail!("Invalid property access")
        };
        let value = self.peek(0);
        let instance = self.gc.get_mut(&instance_ref);
        instance.set(member_id, value);
        Ok(())
    }

    fn op_add(&mut self) -> Result<(), anyhow::Error> {
        let result = match (self.pop_get(), self.pop_get()) {
            (Value::Number(b), Value::Number(a)) => Value::Number(a + b),
            (Value::String(b), Value::String(a)) => {
                let s = [self.gc.get(&a).as_str(), self.gc.get(&b).as_str()].concat();
                Value::String(self.intern(s))
            },
            (b, a) => {
                return self.error(format!("Operator '+' cannot be applied to {} and {}", a, b))
            }
        };
        self.push(result);
        Ok(())
    }
    
    fn op_subtract(&mut self) -> Result<(), anyhow::Error> {
        self.binary_op_number(|a, b| Value::Number(a - b))
    }
    
    fn op_multiply(&mut self) -> Result<(), anyhow::Error> {
        self.binary_op_number(|a, b| Value::Number(a * b))
    }
    
    fn op_divide(&mut self) -> Result<(), anyhow::Error> {
        self.binary_op_number(|a, b| Value::Number(a / b))
    }
    
    fn op_equal(&mut self) -> Result<(), anyhow::Error> {
        self.binary_op(|a, b| Value::Boolean(values_equal(a, b)))
    }

    fn op_not_equal(&mut self) -> Result<(), anyhow::Error> {
        self.binary_op(|a, b| Value::Boolean(!values_equal(a, b)))
    }
    
    fn op_less_than(&mut self) -> Result<(), anyhow::Error> {
        self.binary_op_number(|a, b| Value::Boolean(a < b))
    }
    
    fn op_less_than_equal(&mut self) -> Result<(), anyhow::Error> {
        self.binary_op_number(|a, b| Value::Boolean(a <= b))
    }
    
    fn op_greater_than(&mut self) -> Result<(), anyhow::Error> {
        self.binary_op_number(|a, b| Value::Boolean(a > b))
    }
    
    fn op_greater_than_equal(&mut self) -> Result<(), anyhow::Error> {
        self.binary_op_number(|a, b| Value::Boolean(a >= b))
    }
    
    fn op_negate(&mut self) -> Result<(), anyhow::Error> {
        let Value::Number(value) = self.pop_get() else {
            bail!("Invalid operand")
        };
        self.push(Value::Number(-value));
        Ok(())
    }
    
    fn op_not(&mut self) -> Result<(), anyhow::Error> {
        let Value::Boolean(value) = self.pop_get() else {
            bail!("Invalid operand")
        };
        self.push(Value::Boolean(!value));
        Ok(())
    }

    fn binary_op_number<F: Fn(f64, f64) -> Value>(&mut self, func: F) -> Result<(), anyhow::Error> {
        let Value::Number(b) = self.pop_get() else {
            bail!("Invalid operand")
        };
        let Value::Number(a) = self.pop_get() else {
            bail!("Invalid operand")
        };
        self.push(func(a, b));
        Ok(())
    }

    fn binary_op<F: Fn(Value, Value) -> Value>(&mut self, func: F) -> Result<(), anyhow::Error> {
        let b = self.pop_get();
        let a = self.pop_get();
        self.push(func(a, b));
        Ok(())
    }
}

fn values_equal(a: Value, b: Value) -> bool {
    match (a, b) {
        (Value::Number(a), Value::Number(b)) => a == b,
        (Value::Boolean(a), Value::Boolean(b)) => a == b,
        (Value::String(a), Value::String(b)) => a == b,
        (Value::NativeFunction(a), Value::NativeFunction(b)) => a == b,
        (Value::Class(a), Value::Class(b)) => a == b,
        (Value::Instance(a), Value::Instance(b)) => a == b,
        (Value::Function(a), Value::Function(b)) => a == b,
        (Value::Closure(a), Value::Closure(b)) => a == b,
        (Value::BoundMethod(a), Value::BoundMethod(b)) => a == b,
        (Value::Null, Value::Null) => true,
        (_, _) => false
    }
}

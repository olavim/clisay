use anyhow::bail;
use nohash_hasher::IntMap;

use crate::bytecode::objects::{BoundMethod, Instance};
use crate::lexer::{tokenize, TokenStream};

use super::operator::Operator;
use super::parser::Parser;
use super::OpCode;
use super::gc::{Gc, GcRef, GcTraceable};
use super::objects::{BytecodeChunk, Class, ClassMember, Closure, Function, NativeFunction, Upvalue, Value};
use super::compiler::Compiler;

pub struct CallFrame {
    return_ip: usize,
    stack_start: usize
}

pub struct Vm<'out> {
    gc: Gc,
    ip: usize,
    chunk: BytecodeChunk,
    globals: IntMap<GcRef<String>, Value>,
    stack: [Value; 16384],
    stack_top: usize,
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
            ip: 0,
            chunk,
            globals: IntMap::default(),
            stack: [Value::Null; 16384],
            stack_top: 0,
            frames: Vec::new(),
            open_upvalues: Vec::new(),
            out: &mut out
        };

        vm.define_native("print", 1, |vm| {
            let value_str = match vm.get(vm.stack_top - 1) {
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
        let trace = self.frames.iter().rev()
            .map(|frame| {
                let function = self.get_closure(&frame).function;
                let function = self.gc.get(&function);
                let name = self.gc.get(&function.name);
                format!("at {} ({})", name, self.chunk.code_pos[frame.return_ip])
            })
            .collect::<Vec<String>>().join("\n");

        bail!(format!("{}\nat {}\n{}", message.into(), self.chunk.code_pos[self.ip - 1], trace))
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

        for i in 0..self.stack_top {
            self.get(i).mark_refs(&mut self.gc);
        }

        self.gc.collect();
    }

    fn current_stack_start(&self) -> usize {
        match self.frames.last() {
            Some(frame) => frame.stack_start,
            None => 0
        }
    }

    fn current_closure(&self) -> &Closure {
        self.get_closure(&self.frames[self.frames.len() - 1])
    }

    fn get_closure(&self, frame: &CallFrame) -> &Closure {
        let Value::Closure(closure_ref) = self.get(frame.stack_start) else {
            unreachable!()
        };
        self.gc.get(&closure_ref)
    }

    fn capture_upvalue(&mut self, location: u8) -> GcRef<Upvalue> {
        let stack_location = self.current_stack_start() as u8 + location;
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

        let upvalue = self.alloc(Upvalue { location: stack_location, closed: None });
        self.open_upvalues.insert(i, upvalue);
        upvalue
    }

    fn close_upvalues(&mut self, stack_start: u8) {
        let mut split_idx = 0;
        for i in 0..self.open_upvalues.len() {
            let location = self.gc.get(&self.open_upvalues[i]).location;
            if location >= stack_start {
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
        if idx >= self.stack_top {
            unreachable!("Stack underflow")
        }
        self.stack[idx]
    }

    #[inline]
    fn peek(&self) -> Value {
        self.get(self.stack_top - 1)
    }

    #[inline]
    fn set(&mut self, idx: impl Into<usize>, value: Value) {
        let idx = idx.into();
        if idx > self.stack_top {
            unreachable!("Stack underflow")
        }
        self.stack[idx] = value;
    }

    #[inline]
    fn pop(&mut self) {
        self.stack_top -= 1;
    }

    #[inline]
    fn pop_get(&mut self) -> Value {
        let value = self.peek();
        self.pop();
        value
    }

    #[inline]
    fn push(&mut self, value: Value) {
        if self.stack_top >= self.stack.len() {
            panic!("Stack overflow")
        }
        self.set(self.stack_top, value);
        self.stack_top += 1;
    }

    #[inline]
    fn truncate(&mut self, after: usize) {
        for i in after..self.stack_top {
            self.set(i, Value::Null);
        }
        self.stack_top = after;
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

    fn interpret(&mut self) -> Result<(), anyhow::Error> {
        macro_rules! read_op {
            () => {{
                let op = self.chunk.code[self.ip];
                self.ip += 1;
                op
            }}
        }

        let instruction_count = self.chunk.code.len();
        while self.ip < instruction_count {
            let op = read_op!();
            match op {
                OpCode::Return => {
                    let value = self.pop_get();
                    if let Some(frame) = self.frames.pop() {
                        self.ip = frame.return_ip;
                        self.close_upvalues(frame.stack_start as u8);

                        self.truncate(frame.stack_start);
                        self.push(value);
                    } else {
                        return Ok(());
                    }
                },
                OpCode::Pop => { self.pop(); },
                OpCode::CloseUpvalue(location) => {
                    self.close_upvalues(self.current_stack_start() as u8 + location);
                    self.pop();
                },
                OpCode::Call(arg_count) => {
                    let arg_count = arg_count as usize;
                    let function = self.get(self.stack_top - arg_count - 1);

                    match &function {
                        Value::NativeFunction(gc_ref) => {
                            let func = self.gc.get(gc_ref).function;
                            func(self);
                            let result = self.pop_get();
                            self.truncate(self.stack_top - arg_count - 1);
                            self.push(result);
                        },
                        Value::Closure(gc_ref) => {
                            let closure = self.gc.get(gc_ref);
                            let function = self.gc.get(&closure.function);
                            self.frames.push(CallFrame {
                                return_ip: self.ip, 
                                stack_start: self.stack_top - arg_count - 1
                            });
                            self.ip = function.ip_start;
                        },
                        Value::BoundMethod(gc_ref) => {
                            let stack_start = self.stack_top - arg_count - 1;
                            let instance = self.gc.get(gc_ref).instance;
                            self.set(stack_start, Value::Instance(instance));

                            self.frames.push(CallFrame {
                                return_ip: self.ip, 
                                stack_start
                            });

                            let closure = self.gc.get(gc_ref).closure;
                            let function = self.gc.get(&closure).function;
                            self.ip = self.gc.get(&function).ip_start;
                        },
                        Value::Class(class_ref) => {
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

                            let stack_start = self.stack_top - arg_count - 1;
                            let instance = self.alloc(instance);
                            self.set(stack_start, Value::Instance(instance));

                            let function = self.gc.get(&init_ref);
                            self.frames.push(CallFrame {
                                return_ip: self.ip, 
                                stack_start
                            });
                            self.ip = function.ip_start;
                        },
                        _ => unreachable!()
                    }
                },
                OpCode::Jump(lpos, rpos) => { self.ip = as_short!(lpos, rpos) as usize },
                OpCode::JumpIfFalse(lpos, rpos) => {
                    let pos = as_short!(lpos, rpos) as usize;
                    if let Value::Boolean(false) = self.pop_get() {
                        self.ip = pos;
                    }
                },
                OpCode::PushConstant(const_idx) => {
                    let value = self.chunk.constants[const_idx as usize].clone();
                    self.push(value);
                },
                OpCode::PushNull => self.push(Value::Null),
                OpCode::PushTrue => self.push(Value::Boolean(true)),
                OpCode::PushFalse => self.push(Value::Boolean(false)),
                OpCode::PushClosure(const_idx) => {
                    let value = self.chunk.constants[const_idx as usize];
                    let Value::Function(fn_ref) = value else {
                        bail!("Invalid closure")
                    };

                    let closure = self.create_closure(&fn_ref);
                    self.push(Value::Closure(closure));
                },
                OpCode::PushClass(const_idx) => {
                    let value = self.chunk.constants[const_idx as usize];
                    self.push(value);
                },
                OpCode::GetGlobal(const_idx) => {
                    let constant = &self.chunk.constants[const_idx as usize];
                    let global_name = match constant {
                        Value::String(gc_ref) => gc_ref,
                        _ => bail!("Invalid global name")
                    };
                    let value = self.globals[global_name];
                    self.push(value);
                },
                OpCode::SetGlobal(const_idx) => {
                    let constant = &self.chunk.constants[const_idx as usize];
                    let global_name = match constant {
                        Value::String(gc_ref) => *gc_ref,
                        _ => bail!("Invalid global name")
                    };
                    let value = self.pop_get();
                    self.globals.insert(global_name, value);
                },
                OpCode::GetLocal(idx) => {
                    let local_idx = idx as usize + self.current_stack_start();
                    let value = self.get(local_idx);
                    self.push(value);
                },
                OpCode::SetLocal(idx) => {
                    let local_idx = idx as usize + self.current_stack_start();
                    self.set(local_idx, self.peek());
                },
                OpCode::GetUpvalue(idx) => {
                    let upvalue = self.current_closure().upvalues[idx as usize];
                    let upvalue = self.gc.get(&upvalue);
                    if let Some(value) = upvalue.closed {
                        self.push(value);
                    } else {
                        let value = self.get(upvalue.location);
                        self.push(value);
                    }
                },
                OpCode::SetUpvalue(idx) => {
                    let upvalue = self.current_closure().upvalues[idx as usize];
                    if let Some(_) = self.gc.get(&upvalue).closed {
                        self.gc.get_mut(&upvalue).closed = Some(self.peek());
                    } else {
                        self.set(self.gc.get(&upvalue).location, self.peek());
                    }
                },
                OpCode::GetProperty(const_idx) => {
                    let Value::Instance(instance_ref) = self.pop_get() else {
                        bail!("Invalid property access")
                    };

                    let Value::String(prop) = self.chunk.constants[const_idx as usize] else {
                        bail!("Invalid property access")
                    };

                    if let Some(value) = self.get_property(&instance_ref, &prop) {
                        self.push(value.clone());
                    } else {
                        bail!("Property not found")
                    }
                },
                OpCode::SetProperty(const_idx) => {
                    let Value::Instance(instance_ref) = self.pop_get() else {
                        bail!("Invalid property access")
                    };

                    let Value::String(prop) = self.chunk.constants[const_idx as usize] else {
                        bail!("Invalid property access")
                    };

                    let value = self.peek();
                    let class_ref = self.gc.get(&instance_ref).class;
                    let class = self.gc.get(&class_ref);
                    match class.resolve(&prop) {
                        Some(ClassMember::Field(id)) => {
                            let instance = self.gc.get_mut(&instance_ref);
                            instance.values.insert(id, value.clone());
                        },
                        Some(ClassMember::Method(_)) => {
                            bail!("Cannot set method")
                        },
                        None => {
                            bail!("No such property")
                        }
                    }
                },
                OpCode::GetThisProperty(member_id) => {
                    let Value::Instance(instance_ref) = self.pop_get() else {
                        bail!("Invalid property access")
                    };

                    let value = self.get_property_by_id(&instance_ref, member_id);
                    self.push(value);
                },
                OpCode::SetThisProperty(member_id) => {
                    let Value::Instance(instance_ref) = self.pop_get() else {
                        bail!("Invalid property access")
                    };

                    let value = self.peek();
                    let instance = self.gc.get_mut(&instance_ref);
                    instance.set(member_id, value);
                },
                OpCode::Add |
                OpCode::Subtract |
                OpCode::Multiply |
                OpCode::Divide |
                OpCode::Equal |
                OpCode::NotEqual |
                OpCode::LessThan |
                OpCode::LessThanEqual |
                OpCode::GreaterThan |
                OpCode::GreaterThanEqual => {
                    let b = self.pop_get();
                    let a = self.pop_get();
                    let result = match (op, a, b) {
                        (OpCode::Add, Value::Number(a), Value::Number(b)) => Value::Number(a + b),
                        (OpCode::Add, Value::String(a), Value::String(b)) => {
                            let mut a = self.gc.get(&a).to_owned();
                            a.push_str(self.gc.get(&b).as_str());
                            Value::String(self.intern(a))
                        },
                        (OpCode::Subtract, Value::Number(a), Value::Number(b)) => Value::Number(a - b),
                        (OpCode::Multiply, Value::Number(a), Value::Number(b)) => Value::Number(a * b),
                        (OpCode::Divide, Value::Number(a), Value::Number(b)) => Value::Number(a / b),
                        (OpCode::Equal, Value::Number(a), Value::Number(b)) => Value::Boolean(a == b),
                        (OpCode::Equal, Value::Boolean(a), Value::Boolean(b)) => Value::Boolean(a == b),
                        (OpCode::Equal, Value::String(a), Value::String(b)) => Value::Boolean(a == b),
                        (OpCode::NotEqual, Value::Number(a), Value::Number(b)) => Value::Boolean(a != b),
                        (OpCode::LessThan, Value::Number(a), Value::Number(b)) => Value::Boolean(a < b),
                        (OpCode::LessThanEqual, Value::Number(a), Value::Number(b)) => Value::Boolean(a <= b),
                        (OpCode::GreaterThan, Value::Number(a), Value::Number(b)) => Value::Boolean(a > b),
                        (OpCode::GreaterThanEqual, Value::Number(a), Value::Number(b)) => Value::Boolean(a >= b),
                        (op, a, b) => {
                            let operator = Operator::from_opcode(op);
                            return self.error(format!("Operator '{}' cannot be applied to {} and {}", operator, a, b))
                        }
                    };
                    self.push(result);
                },
                OpCode::Negate |
                OpCode::Not => {
                    let value = self.pop_get();
                    let result = match (op, value) {
                        (OpCode::Negate, Value::Number(val)) => Value::Number(-val),
                        (OpCode::Not, Value::Boolean(val)) => Value::Boolean(!val),
                        _ => bail!("Invalid unary operand")
                    };
                    self.push(result);
                }
            }

            // for i in 0..self.stack_top {
            //     print!("{} ", self.get(i).fmt(&self.gc));
            // }
            // println!();
        }

        Ok(())
    }
}

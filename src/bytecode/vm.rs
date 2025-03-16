use std::collections::BTreeMap;

use anyhow::bail;
use nohash_hasher::IntMap;

use crate::{bytecode::{gc::GcObjFormatter, Closure, Function, OpCode, Upvalue}, lexer};

use super::{gc::GcRef, BytecodeChunk, Compiler, Gc, NativeFunction, Value};

pub struct CallFrame {
    return_ip: usize,
    stack_start: usize,
    closure: GcRef<Closure>
}

pub struct Vm {
    gc: Gc,
    ip: usize,
    chunk: BytecodeChunk,
    globals: IntMap<GcRef<String>, Value>,
    stack: Vec<Value>,
    frames: Vec<CallFrame>,
    open_upvalues: BTreeMap<u8, GcRef<Upvalue>>
}

impl Vm {
    pub fn new() -> Vm {
        let mut vm = Vm { 
            gc: Gc::new(),
            ip: 0,
            chunk: BytecodeChunk::new(),
            globals: IntMap::default(),
            stack: Vec::new(),
            frames: Vec::new(),
            open_upvalues: BTreeMap::default()
        };

        vm.define_native("print", 1, |vm, args| {
            let value_str = match &args[0] {
                Value::Null => String::from("null"),
                Value::Number(num) => num.to_string(),
                Value::Boolean(b) => b.to_string(),
                Value::String(gc_ref) => vm.gc.get(gc_ref).clone(),
                Value::Function(gc_ref) => match &*vm.gc.get(gc_ref) {
                    Function { name, .. } => format!("<fn {}>", vm.gc.get(name))
                },
                Value::Closure(gc_ref) => match &*vm.gc.get(gc_ref) {
                    Closure { name, .. } => format!("<closure {}>", vm.gc.get(name))
                },
                Value::NativeFunction(gc_ref) => match &*vm.gc.get(gc_ref) {
                    NativeFunction { name, .. } => format!("<native fn {}>", vm.gc.get(name))
                }
            };
            println!("{}", value_str);
            Value::Null
        });

        vm.define_native("clock", 0, |_vm, _args| {
            Value::Number(std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH).unwrap().as_millis() as f64)
        });

        return vm;
    }

    fn define_native(&mut self, name: impl Into<String>, arity: u8, function: fn(vm: &Vm, &[Value]) -> Value) {
        let name_ref = self.gc.intern(name.into());
        let native = NativeFunction {
            name: name_ref,
            arity,
            function
        };

        let value = Value::NativeFunction(self.gc.alloc(native));
        self.globals.insert(name_ref, value);
    }

    pub fn run(&mut self, file_name: &str, src: &str) -> Result<(), anyhow::Error> {
        let tokens = lexer::tokenize(String::from(file_name), String::from(src))?;
        Compiler::write(&mut self.chunk, &tokens, &mut self.gc)?;
        self.disassemble_chunk();
        println!("===");
        self.interpret()?;
        Ok(())
    }

    pub fn disassemble_chunk(&mut self) {
        let mut i = 0;
        while i < self.chunk.code.len() {
            let op = OpCode::from_byte(self.chunk.code[i]);
            print!("{i}: {op}");
            i += 1;

            match op {
                OpCode::PushConstant | OpCode::GetGlobal => {
                    let constant_idx = self.chunk.code[i] as usize;
                    print!(" {}", GcObjFormatter::new(&self.chunk.constants[constant_idx], &self.gc));
                    i += 1;
                },
                OpCode::PushClosure => {
                    let constant_idx = self.chunk.code[i] as usize;
                    let function = &self.chunk.constants[constant_idx];
                    i += 1;

                    if let Value::Function(gc_ref) = function {
                        let function = self.gc.get(gc_ref);
                        print!(" {}", GcObjFormatter::new(function, &self.gc));

                        for _ in 0..function.upvalue_count {
                            let is_local = self.chunk.code[i] == 1;
                            let index = self.chunk.code[i + 1] as usize;
                            print!("\n  {} {}", if is_local { "local" } else { "upvalue" }, index);
                            i += 2;
                        }
                    }
                },
                OpCode::Call => {
                    let arg_count = self.chunk.code[i];
                    print!(" {arg_count}");
                    i += 1;
                },
                OpCode::GetLocal | OpCode::GetUpvalue => {
                    print!(" <{}>", self.chunk.code[i]);
                    i += 1;
                },
                OpCode::Jump | OpCode::JumpIfFalse => {
                    let pos = (self.chunk.code[i] as u16) << 8 | (self.chunk.code[i + 1] as u16);
                    print!(" <{}>", pos);
                    i += 2;
                },
                _ => {}
            }

            print!("\n");
        }
    }

    fn current_stack_start(&self) -> usize {
        match self.frames.last() {
            Some(frame) => frame.stack_start,
            None => 0
        }
    }

    fn current_closure(&self) -> &Closure {
        self.gc.get(&self.frames.last().unwrap().closure)
    }

    fn capture_upvalue(&mut self, location: u8) -> GcRef<Upvalue> {
        let stack_location = self.current_stack_start() as u8 + location;
        if let Some(upvalue) = self.open_upvalues.get(&stack_location) {
            return *upvalue;
        }

        let upvalue = self.gc.alloc(Upvalue { location: stack_location, closed: None });
        self.open_upvalues.insert(stack_location, upvalue);
        upvalue
    }

    fn close_upvalues(&mut self, stack_start: u8) {
        for (_, upvalue_ref) in self.open_upvalues.split_off(&stack_start) {
            let upvalue = self.gc.get_mut(&upvalue_ref);
            upvalue.closed = Some(self.stack[upvalue.location as usize]);
        }
    }

    fn interpret(&mut self) -> Result<(), anyhow::Error> {
        macro_rules! read_byte {
            () => {{
                let byte = self.chunk.code[self.ip];
                self.ip += 1;
                byte
            }}
        }
        macro_rules! read_short {
            () => {(read_byte!() as u16) << 8 | (read_byte!() as u16)}
        }

        let instruction_count = self.chunk.code.len();
        while self.ip < instruction_count {
            let op = OpCode::from_byte(read_byte!());
            match op {
                OpCode::Return => {
                    let value = self.stack.pop().unwrap();
                    if let Some(frame) = self.frames.pop() {
                        self.ip = frame.return_ip;
                        self.close_upvalues(frame.stack_start as u8);
                        self.stack.truncate(frame.stack_start);
                        self.stack.push(value);
                    } else {
                        return Ok(());
                    }
                },
                OpCode::Pop => { self.stack.pop(); },
                OpCode::CloseUpvalue => {
                    let location = read_byte!();
                    self.close_upvalues(self.current_stack_start() as u8 + location);
                    self.stack.pop();
                },
                OpCode::Call => {
                    let arg_count = read_byte!() as usize;
                    let function = self.stack[self.stack.len() - 1 - arg_count];

                    match &function {
                        Value::NativeFunction(gc_ref) => {
                            let native = self.gc.get(gc_ref);
                            let args = &self.stack[self.stack.len() - arg_count..];
                            let value = (native.function)(self, args);
                            self.stack.truncate(self.stack.len() - arg_count - 1);
                            self.stack.push(value);
                        },
                        Value::Closure(gc_ref) => {
                            let closure = self.gc.get(gc_ref);
                            let function = self.gc.get(&closure.function);
                            let stack_start = self.stack.len() - arg_count - 1;
                            self.frames.push(CallFrame { 
                                return_ip: self.ip, 
                                stack_start,
                                closure: *gc_ref
                            });
                            self.ip = function.ip_start;
                        },
                        _ => unreachable!()
                    }
                },
                OpCode::Jump => { self.ip = read_short!() as usize; },
                OpCode::JumpIfFalse => {
                    let pos = read_short!() as usize;
                    if let Value::Boolean(false) = self.stack.pop().unwrap() {
                        self.ip = pos;
                    }
                },
                OpCode::PushConstant => {
                    let constant_idx = read_byte!() as usize;
                    let value = self.chunk.constants[constant_idx].clone();
                    self.stack.push(value);
                },
                OpCode::PushNull => self.stack.push(Value::Null),
                OpCode::PushTrue => self.stack.push(Value::Boolean(true)),
                OpCode::PushFalse => self.stack.push(Value::Boolean(false)),
                OpCode::PushClosure => {
                    let constant_idx = read_byte!() as usize;
                    let function_ref = match &self.chunk.constants[constant_idx] {
                        Value::Function(gc_ref) => gc_ref,
                        _ => bail!("Invalid closure")
                    };
                    let function = self.gc.get(function_ref);

                    let mut closure = Closure {
                        name: function.name,
                        function: *function_ref,
                        upvalues: Vec::new()
                    };

                    for _ in 0..function.upvalue_count {
                        let is_local = read_byte!() == 1;
                        let location = read_byte!();
                        
                        let upvalue = if is_local {
                            self.capture_upvalue(location)
                        } else {
                            let closure = self.gc.get(&self.frames.last().unwrap().closure);
                            closure.upvalues[location as usize]
                        };

                        closure.upvalues.push(upvalue);
                    }

                    self.stack.push(Value::Closure(self.gc.alloc(closure)));
                },
                OpCode::GetGlobal => {
                    let constant_idx = read_byte!() as usize;
                    let constant = &self.chunk.constants[constant_idx];
                    let global_name = match constant {
                        Value::String(gc_ref) => gc_ref,
                        _ => bail!("Invalid global name")
                    };
                    let value = self.globals[global_name];
                    self.stack.push(value);
                },
                OpCode::SetGlobal => {
                    let constant_idx = read_byte!() as usize;
                    let constant = &self.chunk.constants[constant_idx];
                    let global_name = match constant {
                        Value::String(gc_ref) => *gc_ref,
                        _ => bail!("Invalid global name")
                    };
                    let value = self.stack.pop().unwrap();
                    self.globals.insert(global_name, value);
                },
                OpCode::GetLocal => {
                    let local_idx = read_byte!() as usize + self.current_stack_start();
                    let value = self.stack[local_idx];
                    self.stack.push(value);
                },
                OpCode::SetLocal => {
                    let local_idx = read_byte!() as usize + self.current_stack_start();
                    let value = self.stack.pop().unwrap();
                    self.stack[local_idx] = value;
                },
                OpCode::GetUpvalue => {
                    let idx = read_byte!();
                    let upvalue = self.current_closure().upvalues[idx as usize];
                    let upvalue = self.gc.get(&upvalue);
                    if let Some(value) = upvalue.closed {
                        self.stack.push(value);
                    } else {
                        let value = self.stack[upvalue.location as usize];
                        self.stack.push(value);
                    }
                },
                OpCode::SetUpvalue => {
                    let idx = read_byte!();
                    let upvalue = self.current_closure().upvalues[idx as usize];
                    let upvalue = self.gc.get_mut(&upvalue);
                    let value = self.stack.pop().unwrap();

                    if let Some(_) = upvalue.closed {
                        upvalue.closed = Some(value);
                    } else {
                        self.stack[upvalue.location as usize] = value;
                    }
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
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    let result = match (op, a, b) {
                        (OpCode::Add, Value::Number(a), Value::Number(b)) => Value::Number(a + b),
                        (OpCode::Add, Value::String(a), Value::String(b)) => {
                            let mut a = self.gc.get(&a).to_owned();
                            a.push_str(self.gc.get(&b).as_str());
                            Value::String(self.gc.intern(a))
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
                        _ => bail!("Invalid binary operands")
                    };
                    self.stack.push(result);
                },
                OpCode::Negate |
                OpCode::Not => {
                    let value = self.stack.pop().unwrap();
                    let result = match (op, value) {
                        (OpCode::Negate, Value::Number(val)) => Value::Number(-val),
                        (OpCode::Not, Value::Boolean(val)) => Value::Boolean(!val),
                        _ => bail!("Invalid unary operand")
                    };
                    self.stack.push(result);
                },
                _ => unreachable!("Invalid opcode")
            }
        
            // print!("{} [", self.ip);
            // for value in &self.stack {
            //     print!("{} ", GcObjFormatter::new(value, &self.gc));
            // }
            // println!("]");
        }

        Ok(())
    }
}

use std::{any::Any, fmt, mem};

use anyhow::bail;
use nohash_hasher::{IntMap, IntSet};

use crate::lexer::SourcePosition;

use super::gc::{Gc, GcRef, GcTraceable};
use super::vm::Vm;
use super::OpCode;

#[derive(Clone)]
pub struct BytecodeChunk {
    pub code: Vec<OpCode>,
    pub constants: Vec<Value>,
    pub code_pos: Vec<SourcePosition>
}

impl BytecodeChunk {
    pub fn new() -> BytecodeChunk {
        return BytecodeChunk { code: Vec::new(), constants: Vec::new(), code_pos: Vec::new() };
    }

    pub fn write(&mut self, op: OpCode, pos: &SourcePosition) {
        self.code.push(op);
        self.code_pos.push(pos.clone());
    }

    pub fn add_constant(&mut self, value: Value) -> Result<u8, anyhow::Error> {
        if self.constants.len() >= u8::MAX as usize {
            bail!("Too many constants");
        }

        self.constants.push(value);
        Ok((self.constants.len() - 1) as u8)
    }
}

impl GcTraceable for BytecodeChunk {
    fn fmt(&self, gc: &Gc) -> String {
        let mut string = String::new();

        macro_rules! push_fmt {
            ($($t:tt)*) => {{
                string.push_str(&format!($($t)*));
            }};
        }

        let mut i = 0;
        while i < self.code.len() {
            let op = self.code[i];
            i += 1;

            push_fmt!("{i}: ");

            match op {
                OpCode::Return => push_fmt!("RET"),
                OpCode::Pop => push_fmt!("POP"),
                OpCode::Call(arg_count) => push_fmt!("CALL {arg_count}"),
                OpCode::Jump(lpos, rpos) => push_fmt!("JUMP <{}>", (lpos as u16) << 8 | (rpos as u16)),
                OpCode::JumpIfFalse(lpos, rpos) => push_fmt!("JUMP_F <{}>", (lpos as u16) << 8 | (rpos as u16)),
                OpCode::CloseUpvalue(idx) => push_fmt!("CLOSE_UPVALUE <{}>", idx),
                OpCode::PushNull => push_fmt!("NULL"),
                OpCode::PushTrue => push_fmt!("TRUE"),
                OpCode::PushFalse => push_fmt!("FALSE"),
                OpCode::PushConstant(const_idx) => {
                    push_fmt!("CONST {}", self.constants[const_idx as usize].fmt(gc));
                },
                OpCode::PushClosure(idx) => {
                    if let Value::Function(gc_ref) = &self.constants[idx as usize] {
                        let function = gc.get(gc_ref);
                        push_fmt!("CLOSURE {}", function.fmt(gc));
                    }
                },
                OpCode::PushClass(idx) => {
                    if let Value::Class(gc_ref) = &self.constants[idx as usize] {
                        let class = gc.get(gc_ref);
                        push_fmt!("CLASS {}", class.fmt(gc));
                    }
                },
                OpCode::Add => push_fmt!("ADD"),
                OpCode::Subtract => push_fmt!("SUB"),
                OpCode::Multiply => push_fmt!("MUL"),
                OpCode::Divide => push_fmt!("DIV"),
                OpCode::Negate => push_fmt!("NEG"),
                OpCode::Equal => push_fmt!("EQ"),
                OpCode::NotEqual => push_fmt!("NEQ"),
                OpCode::LessThan => push_fmt!("LT"),
                OpCode::LessThanEqual => push_fmt!("LTE"),
                OpCode::GreaterThan => push_fmt!("GT"),
                OpCode::GreaterThanEqual => push_fmt!("GTE"),
                OpCode::Not => push_fmt!("NOT"),
                OpCode::GetLocal(loc_idx) => push_fmt!("GET_LOCAL <{}>", loc_idx),
                OpCode::SetLocal(loc_idx) => push_fmt!("SET_LOCAL <{}>", loc_idx),
                OpCode::GetUpvalue(loc_idx) => push_fmt!("GET_UPVAL <{}>", loc_idx),
                OpCode::SetUpvalue(loc_idx) => push_fmt!("SET_UPVAL <{}>", loc_idx),
                OpCode::GetProperty(const_idx) => {
                    push_fmt!("GET_PROP <{}>", self.constants[const_idx as usize].fmt(gc))
                },
                OpCode::SetProperty(const_idx) => {
                    push_fmt!("SET_PROP <{}>", self.constants[const_idx as usize].fmt(gc))
                },
                OpCode::GetPropertyId(member_id) => {
                    push_fmt!("GET_PROP_ID <{}>", member_id)
                },
                OpCode::SetPropertyId(member_id) => {
                    push_fmt!("SET_PROP_ID <{}>", member_id)
                },
                OpCode::GetGlobal(const_idx) => {
                    push_fmt!("GET_GLOBAL {}", self.constants[const_idx as usize].fmt(gc));
                },
                OpCode::SetGlobal(const_idx) => {
                    push_fmt!("SET_GLOBAL {}", self.constants[const_idx as usize].fmt(gc));
                }
            }

            push_fmt!("\n");
        }
        
        string
    }

    fn mark_refs(&self, gc: &mut Gc) {
        for constant in &self.constants {
            constant.mark_refs(gc);
        }
    }

    fn size(&self) -> usize {
        mem::size_of::<BytecodeChunk>()
            + self.code.capacity() * mem::size_of::<OpCode>()
            + self.constants.capacity() * mem::size_of::<Value>()
            + self.code_pos.capacity() * mem::size_of::<SourcePosition>()
    }

    fn as_any(&self) -> &dyn Any {
        return self;
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

pub struct NativeFunction {
    pub name: GcRef<String>,
    pub arity: u8,
    pub function: fn(vm: &mut Vm)
}

impl GcTraceable for NativeFunction {
    fn fmt(&self, gc: &Gc) -> String {
        format!("<native fn {}>", gc.get(&self.name))
    }

    fn mark_refs(&self, gc: &mut Gc) {
        gc.mark_object(&self.name);
    }

    fn size(&self) -> usize {
        mem::size_of::<NativeFunction>()
    }

    fn as_any(&self) -> &dyn Any {
        return self;
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

#[derive(Clone, Copy)]
pub struct UpvalueLocation {
    pub is_local: bool,
    pub location: u8
}

pub struct Function {
    pub name: GcRef<String>,
    pub arity: u8,
    pub ip_start: usize,
    pub upvalues: Vec<UpvalueLocation>
}

impl GcTraceable for Function {
    fn fmt(&self, gc: &Gc) -> String {
        format!("<fn {}>", gc.get(&self.name))
    }

    fn mark_refs(&self, gc: &mut Gc) {
        gc.mark_object(&self.name);
    }

    fn size(&self) -> usize {
        mem::size_of::<Function>() + self.upvalues.capacity() * mem::size_of::<UpvalueLocation>()
    }

    fn as_any(&self) -> &dyn Any {
        return self;
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

pub struct Closure {
    pub function: GcRef<Function>,
    pub upvalues: Vec<GcRef<Upvalue>>
}

impl Closure {
    pub fn new(function: GcRef<Function>) -> Closure {
        return Closure { function, upvalues: Vec::new() };
    }
}

impl GcTraceable for Closure {
    fn fmt(&self, gc: &Gc) -> String {
        format!("<closure {}>", gc.get(&gc.get(&self.function).name))
    }

    fn mark_refs(&self, gc: &mut Gc) {
        gc.mark_object(&self.function);

        for upvalue in &self.upvalues {
            gc.mark_object(upvalue);
        }
    }

    fn size(&self) -> usize {
        mem::size_of::<Closure>() + self.upvalues.capacity() * mem::size_of::<GcRef<Upvalue>>()
    }

    fn as_any(&self) -> &dyn Any {
        return self;
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

pub struct BoundMethod {
    pub instance: GcRef<Instance>,
    pub closure: GcRef<Closure>
}

impl GcTraceable for BoundMethod {
    fn fmt(&self, gc: &Gc) -> String {
        format!("<bound method {}>", gc.get(&gc.get(&gc.get(&self.closure).function).name))
    }

    fn mark_refs(&self, gc: &mut Gc) {
        gc.mark_object(&self.instance);
        gc.mark_object(&self.closure);
    }

    fn size(&self) -> usize {
        mem::size_of::<BoundMethod>()
    }

    fn as_any(&self) -> &dyn Any {
        return self;
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

type MemberId = u8;

#[derive(Clone, Copy, PartialEq)]
pub enum ClassMember {
    Field(MemberId),
    Method(MemberId)
}

pub struct Class {
    pub name: GcRef<String>,
    members: IntMap<GcRef<String>, ClassMember>,
    pub fields: IntSet<MemberId>,
    methods: IntMap<MemberId, GcRef<Function>>,
    next_member_id: MemberId
}

impl Class {
    pub fn new(name: GcRef<String>, superclass: Option<&Class>) -> Class {
        let mut class = Class {
            name,
            members: IntMap::default(),
            fields: IntSet::default(),
            methods: IntMap::default(),
            next_member_id: 1
        };

        if let Some(superclass) = superclass {
            class.inherit(superclass);
        }

        class
    }

    fn inherit(&mut self, superclass: &Class) {
        self.next_member_id = superclass.next_member_id;
        self.members = superclass.members.clone();
        self.fields = superclass.fields.clone();
        self.methods = superclass.methods.clone();
        self.methods.remove(&0);
    }

    pub fn declare_field(&mut self, name: GcRef<String>) {
        self.members.insert(name, ClassMember::Field(self.next_member_id));
        self.fields.insert(self.next_member_id);
        self.next_member_id += 1;
    }

    pub fn declare_method(&mut self, name: GcRef<String>) {
        self.members.insert(name, ClassMember::Method(self.next_member_id));
        self.next_member_id += 1;
    }

    pub fn define_method(&mut self, name: GcRef<String>, method: GcRef<Function>) {
        let ClassMember::Method(id) = self.resolve(&name).unwrap() else {
            unreachable!("Cannot define field as method");
        };
        self.methods.insert(id, method);
    }

    pub fn resolve(&self, name: &GcRef<String>) -> Option<ClassMember> {
        self.members.get(name).copied()
    }

    pub fn resolve_id(&self, name: &GcRef<String>) -> Option<MemberId> {
        match self.members.get(name) {
            Some(ClassMember::Field(id)) |
            Some(ClassMember::Method(id)) => Some(*id),
            None => None
        }
    }

    pub fn get_method(&self, id: MemberId) -> Option<GcRef<Function>> {
        self.methods.get(&id).copied()
    }
}

impl GcTraceable for Class {
    fn fmt(&self, gc: &Gc) -> String {
        format!("<class {}>", gc.get(&self.name))
    }

    fn mark_refs(&self, gc: &mut Gc) {
        gc.mark_object(&self.name);
    }

    fn size(&self) -> usize {
        mem::size_of::<Class>()
            + self.members.capacity() * (mem::size_of::<GcRef<String>>() + mem::size_of::<ClassMember>())
    }

    fn as_any(&self) -> &dyn Any {
        return self;
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

pub struct Instance {
    pub class: GcRef<Class>,
    pub values: IntMap<MemberId, Value>
}

impl Instance {
    pub fn new(class: GcRef<Class>) -> Instance {
        Instance { class, values: IntMap::default() }
    }

    pub fn get(&self, id: MemberId, gc: &Gc) -> Value {
        match self.values.get(&id) {
            Some(value) => *value,
            None => {
                let class = gc.get(&self.class);
                let func = class.get_method(id).unwrap();
                Value::Function(func)
            }
        }
    }

    pub fn set(&mut self, id: MemberId, value: Value) {
        self.values.insert(id, value);
    }
}

impl GcTraceable for Instance {
    fn fmt(&self, gc: &Gc) -> String {
        format!("<instance {}>", gc.get(&self.class).fmt(gc))
    }

    fn mark_refs(&self, gc: &mut Gc) {
        gc.mark_object(&self.class);
        for (_, value) in &self.values {
            value.mark_refs(gc);
        }
    }

    fn size(&self) -> usize {
        mem::size_of::<Class>() + self.values.capacity() * mem::size_of::<(GcRef<String>, Value)>()
    }

    fn as_any(&self) -> &dyn Any {
        return self;
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

pub struct Upvalue {
    pub location: u8,
    pub closed: Option<Value>
}

impl GcTraceable for Upvalue {
    fn fmt(&self, _gc: &Gc) -> String {
        format!("<up {}>", self.location)
    }

    fn mark_refs(&self, gc: &mut Gc) {
        if let Some(value) = &self.closed {
            value.mark_refs(gc);
        }
    }

    fn size(&self) -> usize {
        mem::size_of::<Upvalue>()
    }

    fn as_any(&self) -> &dyn Any {
        return self;
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

// pub struct Value64(u64);

// impl Value64 {
//     /**
//      * 0x7FF8000000000000 is the QNaN representation of a 64-bit float.
//      * A value represents a number if its QNaN bits are not set.
//      * 
//      * We also reserve an additional bit to differentiate between NaNs and
//      * other value types in Clisay, like booleans and objects.
//      * 
//      * If these bits are set, the value does not represent a 64-bit float.
//      */
//     const NAN_MASK: u64 = 0x7FFC000000000000;
//     const NULL: u64 = 0b01;
//     const TRUE: u64 = 0b10;
//     const FALSE: u64 = 0b11;

//     /** Object type values contain a 32-bit pointer value */
//     const OBJECT: u64 = 0x7FFC000000000000;
//     const PTR_MASK: u64 = 0xFFFFFFFF0;

//     pub fn is_number(&self) -> bool {
//         (self.0 & Self::NAN_MASK) != Self::NAN_MASK
//     }

//     pub fn is_bool(&self) -> bool {
//         (self.0 & Self::BOOL_MASK) != 0
//     }
// }

#[derive(Clone, Copy)]
pub enum Value {
    Null,
    Number(f64),
    Boolean(bool),
    String(GcRef<String>),
    Function(GcRef<Function>),
    Closure(GcRef<Closure>),
    BoundMethod(GcRef<BoundMethod>),
    Class(GcRef<Class>),
    Instance(GcRef<Instance>),
    NativeFunction(GcRef<NativeFunction>)
}

impl GcTraceable for Value {
    fn fmt(&self, gc: &Gc) -> String {
        match self {
            Value::Null => format!("null"),
            Value::Number(num) => format!("{num}"),
            Value::Boolean(b) => format!("{b}"),

            // GcRef types
            Value::String(gc_ref) => gc.get(gc_ref).fmt(gc),
            Value::Function(gc_ref) => gc.get(gc_ref).fmt(gc),
            Value::Closure(gc_ref) => gc.get(gc_ref).fmt(gc),
            Value::BoundMethod(gc_ref) => gc.get(gc_ref).fmt(gc),
            Value::NativeFunction(gc_ref) => gc.get(gc_ref).fmt(gc),
            Value::Class(gc_ref) => gc.get(gc_ref).fmt(gc),
            Value::Instance(gc_ref) => gc.get(gc_ref).fmt(gc)
        }
    }

    fn mark_refs(&self, gc: &mut Gc) {
        match self {
            Value::Null |
            Value::Number(_) |
            Value::Boolean(_) => {},
            Value::String(gc_ref) => gc.mark_object(gc_ref),
            Value::Function(gc_ref) => gc.mark_object(gc_ref),
            Value::Closure(gc_ref) => gc.mark_object(gc_ref),
            Value::BoundMethod(gc_ref) => gc.mark_object(gc_ref),
            Value::NativeFunction(gc_ref) => gc.mark_object(gc_ref),
            Value::Class(gc_ref) => gc.mark_object(gc_ref),
            Value::Instance(gc_ref) => gc.mark_object(gc_ref)
        }
    }

    fn size(&self) -> usize {
        mem::size_of::<Value>()
    }
    
    fn as_any(&self) -> &dyn Any {
        unreachable!();
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        unreachable!();
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Value) -> bool {
        return match (self, other) {
            (Value::Null, Value::Null) => true,
            (Value::Number(a), Value::Number(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Function(a), Value::Function(b)) => a == b,
            (Value::Closure(a), Value::Closure(b)) => a == b,
            (Value::BoundMethod(a), Value::BoundMethod(b)) => a == b,
            (Value::NativeFunction(a), Value::NativeFunction(b)) => a == b,
            (Value::Class(a), Value::Class(b)) => a == b,
            (Value::Instance(a), Value::Instance(b)) => a == b,
            _ => false
        };
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Value) -> Option<std::cmp::Ordering> {
        return match (self, other) {
            (Value::Number(a), Value::Number(b)) => a.partial_cmp(b),
            _ => None
        };
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            Value::Null => "null",
            Value::Number(_) => "number",
            Value::Boolean(_) => "boolean",
            Value::String(_) => "string",
            Value::Function(_) => "function",
            Value::Closure(_) => "function",
            Value::BoundMethod(_) => "function",
            Value::NativeFunction(_) => "function",
            Value::Class(_) => "class",
            Value::Instance(_) => "instance"
        })
    }
}
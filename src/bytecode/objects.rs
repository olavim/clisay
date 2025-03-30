use std::{fmt, mem};

use nohash_hasher::{IntMap, IntSet};

use super::gc::{Gc, GcTraceable};
use super::vm::Vm;

#[derive(Clone, Copy)]
pub enum ObjectKind {
    String,
    Function,
    NativeFunction,
    BoundMethod,
    Closure,
    Class,
    Instance,
    Upvalue
}

#[repr(C)]
pub struct ObjectHeader {
    pub kind: ObjectKind,
    pub marked: bool
}

impl ObjectHeader {
    pub fn new(kind: ObjectKind) -> ObjectHeader {
        ObjectHeader { kind, marked: false }
    }
}

#[repr(C)]
pub union Object {
    pub header: *mut ObjectHeader,

    pub string: *mut ObjString,
    pub function: *mut ObjFn,
    pub native_function: *mut ObjNativeFn,
    pub bound_method: *mut ObjBoundMethod,
    pub closure: *mut ObjClosure,
    pub class: *mut ObjClass,
    pub instance: *mut ObjInstance,
    pub upvalue: *mut ObjUpvalue
}

impl Object {
    pub fn kind(&self) -> ObjectKind {
        unsafe { (*self.header).kind }
    }

    pub fn free(&self) -> usize {
        macro_rules! free_object {
            ($ptr:expr) => {{
                let size = unsafe { (*$ptr).size() };
                let _ = unsafe { Box::from_raw($ptr) };
                size
            }};
        }

        match self.kind() {
            ObjectKind::String => free_object!(self.string),
            ObjectKind::Function => free_object!(self.function),
            ObjectKind::NativeFunction => free_object!(self.native_function),
            ObjectKind::BoundMethod => free_object!(self.bound_method),
            ObjectKind::Closure => free_object!(self.closure),
            ObjectKind::Class => free_object!(self.class),
            ObjectKind::Instance => free_object!(self.instance),
            ObjectKind::Upvalue => free_object!(self.upvalue)
        }
    }

    pub fn mark_refs(&self, gc: &mut Gc) {
        let trace: &dyn GcTraceable = match self.kind() {
            ObjectKind::String => unsafe { &*self.string },
            ObjectKind::Function => unsafe { &*self.function },
            ObjectKind::NativeFunction => unsafe { &*self.native_function },
            ObjectKind::BoundMethod => unsafe { &*self.bound_method },
            ObjectKind::Closure => unsafe { &*self.closure },
            ObjectKind::Class => unsafe { &*self.class },
            ObjectKind::Instance => unsafe { &*self.instance },
            ObjectKind::Upvalue => unsafe { &*self.upvalue }
        };
        trace.mark_refs(gc);
    }
}

macro_rules! impl_from_for_object {
    ($name:ident, $class:tt) => {
        impl From<*mut $class> for Object {
            fn from($name: *mut $class) -> Self {
                Object { $name }
            }
        }
    };
}

impl_from_for_object!(header, ObjectHeader);
impl_from_for_object!(string, ObjString);
impl_from_for_object!(function, ObjFn);
impl_from_for_object!(native_function, ObjNativeFn);
impl_from_for_object!(bound_method, ObjBoundMethod);
impl_from_for_object!(closure, ObjClosure);
impl_from_for_object!(class, ObjClass);
impl_from_for_object!(instance, ObjInstance);
impl_from_for_object!(upvalue, ObjUpvalue);

#[repr(C)]
pub struct ObjString {
    pub header: ObjectHeader,
    pub value: String
}

impl ObjString {
    pub fn new(value: String) -> ObjString {
        ObjString {
            header: ObjectHeader::new(ObjectKind::String),
            value
        }
    }
}

impl GcTraceable for ObjString {
    fn fmt(&self, _gc: &Gc) -> String {
        format!("{}", self.value)
    }

    fn mark_refs(&self, _gc: &mut Gc) { }

    fn size(&self) -> usize {
        mem::size_of::<ObjString>() + self.value.capacity()
    }
}

#[derive(Clone, Copy)]
pub struct UpvalueLocation {
    pub is_local: bool,
    pub location: u8
}

#[repr(C)]
pub struct ObjFn {
    pub header: ObjectHeader,
    pub name: *mut ObjString,
    pub arity: u8,
    pub ip_start: usize,
    pub upvalues: Vec<UpvalueLocation>
}

impl ObjFn {
    pub fn new(name: *mut ObjString, arity: u8, ip_start: usize, upvalues: Vec<UpvalueLocation>) -> ObjFn {
        ObjFn {
            header: ObjectHeader::new(ObjectKind::Function),
            name,
            arity,
            ip_start,
            upvalues
        }
    }
}

impl GcTraceable for ObjFn {
    fn fmt(&self, _gc: &Gc) -> String {
        format!("<fn {}>", unsafe { &(*self.name).value })
    }

    fn mark_refs(&self, gc: &mut Gc) {
        gc.mark_object(self.name);
    }

    fn size(&self) -> usize {
        mem::size_of::<ObjFn>() + self.upvalues.capacity() * mem::size_of::<UpvalueLocation>()
    }
}

#[repr(C)]
pub struct ObjNativeFn {
    pub header: ObjectHeader,
    pub name: *mut ObjString,
    pub arity: u8,
    pub function: fn(vm: &mut Vm)
}

impl ObjNativeFn {
    pub fn new(name: *mut ObjString, arity: u8, function: fn(vm: &mut Vm)) -> ObjNativeFn {
        ObjNativeFn {
            header: ObjectHeader::new(ObjectKind::NativeFunction),
            name,
            arity,
            function
        }
    }
}

impl GcTraceable for ObjNativeFn {
    fn fmt(&self, _gc: &Gc) -> String {
        format!("<native fn {}>", unsafe { &(*self.name).value })
    }

    fn mark_refs(&self, gc: &mut Gc) {
        gc.mark_object(self.name);
    }

    fn size(&self) -> usize {
        mem::size_of::<ObjNativeFn>()
    }
}

#[repr(C)]
pub struct ObjClosure {
    pub header: ObjectHeader,
    pub function: *mut ObjFn,
    pub upvalues: Vec<*mut ObjUpvalue>
}

impl ObjClosure {
    pub fn new(function: *mut ObjFn) -> ObjClosure {
        return ObjClosure {
            header: ObjectHeader::new(ObjectKind::Closure),
            function, 
            upvalues: Vec::new() 
        };
    }
}

impl GcTraceable for ObjClosure {
    fn fmt(&self, _gc: &Gc) -> String {
        format!("<closure {}>", unsafe { &(*(*self.function).name).value } )
    }

    fn mark_refs(&self, gc: &mut Gc) {
        gc.mark_object(self.function);

        for upvalue in &self.upvalues {
            gc.mark_object(*upvalue);
        }
    }

    fn size(&self) -> usize {
        mem::size_of::<ObjClosure>() + self.upvalues.capacity() * mem::size_of::<*mut ObjUpvalue>()
    }
}

#[repr(C)]
pub struct ObjBoundMethod {
    pub header: ObjectHeader,
    pub instance: *mut ObjInstance,
    pub closure: *mut ObjClosure
}

impl ObjBoundMethod {
    pub fn new(instance: *mut ObjInstance, closure: *mut ObjClosure) -> ObjBoundMethod {
        ObjBoundMethod {
            header: ObjectHeader::new(ObjectKind::BoundMethod),
            instance,
            closure
        }
    }
}

impl GcTraceable for ObjBoundMethod {
    fn fmt(&self, _gc: &Gc) -> String {
        let closure = unsafe { &*self.closure };
        let function = unsafe { &*closure.function };
        format!("<bound method {}>", unsafe { &(*function.name).value } )
    }

    fn mark_refs(&self, gc: &mut Gc) {
        gc.mark_object(self.instance);
        gc.mark_object(self.closure);
    }

    fn size(&self) -> usize {
        mem::size_of::<ObjBoundMethod>()
    }
}

type MemberId = u8;

#[derive(Clone, Copy, PartialEq)]
pub enum ClassMember {
    Field(MemberId),
    Method(MemberId)
}

#[repr(C)]
pub struct ObjClass {
    pub header: ObjectHeader,
    pub name: *mut ObjString,
    members: IntMap<usize, ClassMember>,
    pub fields: IntSet<MemberId>,
    methods: IntMap<MemberId, *mut ObjFn>,
    next_member_id: MemberId
}

impl ObjClass {
    pub fn new(name: *mut ObjString, superclass: Option<&ObjClass>) -> ObjClass {
        let mut class = ObjClass {
            header: ObjectHeader::new(ObjectKind::Class),
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

    fn inherit(&mut self, superclass: &ObjClass) {
        self.next_member_id = superclass.next_member_id;
        self.members = superclass.members.clone();
        self.fields = superclass.fields.clone();
        self.methods = superclass.methods.clone();
        self.methods.remove(&0);
    }

    pub fn declare_field(&mut self, name: *mut ObjString) {
        self.members.insert(name as usize, ClassMember::Field(self.next_member_id));
        self.fields.insert(self.next_member_id);
        self.next_member_id += 1;
    }

    pub fn declare_method(&mut self, name: *mut ObjString) {
        self.members.insert(name as usize, ClassMember::Method(self.next_member_id));
        self.next_member_id += 1;
    }

    pub fn define_method(&mut self, name: *mut ObjString, method: *mut ObjFn) {
        let ClassMember::Method(id) = self.resolve(name).unwrap() else {
            unreachable!("Cannot define field as method");
        };
        self.methods.insert(id, method);
    }

    pub fn resolve(&self, name: *mut ObjString) -> Option<ClassMember> {
        self.members.get(&(name as usize)).copied()
    }

    pub fn resolve_id(&self, name: *mut ObjString) -> Option<MemberId> {
        match self.members.get(&(name as usize)) {
            Some(ClassMember::Field(id)) |
            Some(ClassMember::Method(id)) => Some(*id),
            None => None
        }
    }

    pub fn get_method(&self, id: MemberId) -> Option<*mut ObjFn> {
        self.methods.get(&id).copied()
    }
}

impl GcTraceable for ObjClass {
    fn fmt(&self, _gc: &Gc) -> String {
        format!("<class {}>", unsafe { &(*self.name).value })
    }

    fn mark_refs(&self, gc: &mut Gc) {
        gc.mark_object(self.name);
    }

    fn size(&self) -> usize {
        mem::size_of::<ObjClass>()
            + self.members.capacity() * (mem::size_of::<*mut String>() + mem::size_of::<ClassMember>())
    }
}

#[repr(C)]
pub struct ObjInstance {
    pub header: ObjectHeader,
    pub class: *mut ObjClass,
    pub values: IntMap<MemberId, Value>
}

impl ObjInstance {
    pub fn new(class: *mut ObjClass) -> ObjInstance {
        ObjInstance {
            header: ObjectHeader::new(ObjectKind::Instance),
            class,
            values: IntMap::default()
        }
    }

    pub fn get(&self, id: MemberId, _gc: &Gc) -> Value {
        match self.values.get(&id) {
            Some(value) => *value,
            None => {
                let class = unsafe { &*self.class };
                let func = class.get_method(id).unwrap();
                Value::Function(func)
            }
        }
    }

    pub fn set(&mut self, id: MemberId, value: Value) {
        self.values.insert(id, value);
    }
}

impl GcTraceable for ObjInstance {
    fn fmt(&self, gc: &Gc) -> String {
        let class = unsafe { &*self.class };
        format!("<instance {}>", class.fmt(gc))
    }

    fn mark_refs(&self, gc: &mut Gc) {
        gc.mark_object(self.class);
        for (_, value) in &self.values {
            value.mark_refs(gc);
        }
    }

    fn size(&self) -> usize {
        mem::size_of::<ObjClass>() + self.values.capacity() * mem::size_of::<(MemberId, Value)>()
    }
}

#[repr(C)]
pub struct ObjUpvalue {
    pub header: ObjectHeader,
    pub location: u8,
    pub closed: Option<Value>
}

impl ObjUpvalue {
    pub fn new(location: u8) -> ObjUpvalue {
        ObjUpvalue {
            header: ObjectHeader::new(ObjectKind::Upvalue),
            location,
            closed: None
        }
    }

    pub fn close(&mut self, value: Value) {
        self.closed = Some(value);
    }
}

impl GcTraceable for ObjUpvalue {
    fn fmt(&self, _gc: &Gc) -> String {
        format!("<up {}>", self.location)
    }

    fn mark_refs(&self, gc: &mut Gc) {
        if let Some(value) = &self.closed {
            value.mark_refs(gc);
        }
    }

    fn size(&self) -> usize {
        mem::size_of::<ObjUpvalue>()
    }
}

// /** NaN boxed value */
// #[derive(Clone, Copy, Eq, PartialEq)]
// pub struct Value64(u64);

// impl Value64 {
//     /**
//      * 0x7FF8000000000000 is the QNaN representation of a 64-bit float.
//      * A value represents a number if its QNaN bits are not set.
//      * 
//      * We also reserve an additional bit to differentiate between NaNs and
//      * other value types in Clisay, like booleans and objects.
//      * 
//      * If these bits are set, the value does not represent an f64.
//      */
//     const NAN_MASK: u64 = 0x7FFC000000000000;
//     const SIGN: u64 = 0x8000000000000000;

//     /**
//      * The sign and QNaN bits are set for object values. 
//      * This takes 14 bits, leaving room for a 50-bit pointer.
//      * 
//      * Technically 64-bit architectures have 64-bit pointers, but in practice
//      * common architectures only use the first 48 bits.
//      */
//     const OBJECT_MASK: u64 = Self::SIGN | Self::NAN_MASK;

//     pub const NULL: Self = Self(Self::NAN_MASK | 0b01);
//     pub const TRUE: Self = Self(Self::NAN_MASK | 0b10);
//     pub const FALSE: Self = Self(Self::NAN_MASK | 0b11);

//     pub fn is_number(self) -> bool {
//         (self.0 & Self::NAN_MASK) != Self::NAN_MASK
//     }

//     pub fn is_bool(self) -> bool {
//         Self(self.0 | 0b01) == Self::TRUE
//     }

//     pub fn is_null(self) -> bool {
//         self == Self::NULL
//     }

//     pub fn is_object(self) -> bool {
//         (self.0 & Self::OBJECT_MASK) == Self::OBJECT_MASK
//     }

//     pub fn as_number(self) -> f64 {
//         f64::from_bits(self.0)
//     }

//     pub fn as_bool(self) -> bool {
//         self == Self::TRUE
//     }
// }

// impl From<f64> for Value64 {
//     fn from(value: f64) -> Self {
//         Self(value.to_bits())
//     }
// }

// impl From<bool> for Value64 {
//     fn from(value: bool) -> Self {
//         if value { Self::TRUE } else { Self::FALSE }
//     }
// }

#[derive(Clone, Copy)]
pub enum Value {
    Null,
    Number(f64),
    Boolean(bool),
    String(*mut ObjString),
    Function(*mut ObjFn),
    Closure(*mut ObjClosure),
    BoundMethod(*mut ObjBoundMethod),
    Class(*mut ObjClass),
    Instance(*mut ObjInstance),
    NativeFunction(*mut ObjNativeFn)
}

impl GcTraceable for Value {
    fn fmt(&self, gc: &Gc) -> String {
        match *self {
            Value::Null => format!("null"),
            Value::Number(num) => format!("{num}"),
            Value::Boolean(b) => format!("{b}"),

            // GcRef types
            Value::String(gc_ref) => unsafe { &*gc_ref }.fmt(gc),
            Value::Function(gc_ref) => unsafe { &*gc_ref }.fmt(gc),
            Value::Closure(gc_ref) => unsafe { &*gc_ref }.fmt(gc),
            Value::BoundMethod(gc_ref) => unsafe { &*gc_ref }.fmt(gc),
            Value::NativeFunction(gc_ref) => unsafe { &*gc_ref }.fmt(gc),
            Value::Class(gc_ref) => unsafe { &*gc_ref }.fmt(gc),
            Value::Instance(gc_ref) => unsafe { &*gc_ref }.fmt(gc)
        }
    }

    fn mark_refs(&self, gc: &mut Gc) {
        match *self {
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
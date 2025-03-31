use std::{fmt, mem};

use nohash_hasher::{IntMap, IntSet};

use super::gc::{Gc, GcTraceable};
use super::value::Value;
use super::vm::Vm;

#[derive(Clone, Copy, PartialEq)]
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

impl fmt::Display for ObjectKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let kind = match self {
            ObjectKind::String => "string",
            ObjectKind::Function => "function",
            ObjectKind::NativeFunction => "function",
            ObjectKind::BoundMethod => "function",
            ObjectKind::Closure => "function",
            ObjectKind::Class => "class",
            ObjectKind::Instance => "object",
            ObjectKind::Upvalue => "upvalue"
        };
        write!(f, "{}", kind)
    }
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

#[derive(Clone, Copy)]
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
    pub fn kind(self) -> ObjectKind {
        unsafe { (*self.header).kind }
    }

    pub fn free(self) -> usize {
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

    fn as_traceable(&self) -> &dyn GcTraceable {
        match self.kind() {
            ObjectKind::String => unsafe { &*self.string },
            ObjectKind::Function => unsafe { &*self.function },
            ObjectKind::NativeFunction => unsafe { &*self.native_function },
            ObjectKind::BoundMethod => unsafe { &*self.bound_method },
            ObjectKind::Closure => unsafe { &*self.closure },
            ObjectKind::Class => unsafe { &*self.class },
            ObjectKind::Instance => unsafe { &*self.instance },
            ObjectKind::Upvalue => unsafe { &*self.upvalue }
        }
    }
}

impl GcTraceable for Object {
    fn fmt(&self) -> String {
        self.as_traceable().fmt()
    }

    fn mark(&self, gc: &mut Gc) {
        gc.mark_object(*self);
        self.as_traceable().mark(gc);
    }

    fn size(&self) -> usize {
        self.as_traceable().size()
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
    fn fmt(&self) -> String {
        format!("{}", self.value)
    }

    fn mark(&self, _gc: &mut Gc) { }

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
    fn fmt(&self) -> String {
        format!("<fn {}>", unsafe { &(*self.name).value })
    }

    fn mark(&self, gc: &mut Gc) {
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
    fn fmt(&self) -> String {
        format!("<native fn {}>", unsafe { &(*self.name).value })
    }

    fn mark(&self, gc: &mut Gc) {
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
    fn fmt(&self) -> String {
        format!("<closure {}>", unsafe { &(*(*self.function).name).value } )
    }

    fn mark(&self, gc: &mut Gc) {
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
    fn fmt(&self) -> String {
        let closure = unsafe { &*self.closure };
        let function = unsafe { &*closure.function };
        format!("<bound method {}>", unsafe { &(*function.name).value } )
    }

    fn mark(&self, gc: &mut Gc) {
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
    fn fmt(&self) -> String {
        format!("<class {}>", unsafe { &(*self.name).value })
    }

    fn mark(&self, gc: &mut Gc) {
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
        let mut values = IntMap::default();
        for field in unsafe { &(*class).fields } {
            values.insert(*field, Value::NULL);
        }
        
        ObjInstance {
            header: ObjectHeader::new(ObjectKind::Instance),
            class,
            values
        }
    }

    pub fn get(&self, id: MemberId) -> Value {
        match self.values.get(&id) {
            Some(value) => *value,
            None => {
                let class = unsafe { &*self.class };
                let func = class.get_method(id).unwrap();
                Value::from(func)
            }
        }
    }

    pub fn set(&mut self, id: MemberId, value: Value) {
        self.values.insert(id, value);
    }
}

impl GcTraceable for ObjInstance {
    fn fmt(&self) -> String {
        let class = unsafe { &*self.class };
        format!("<instance {}>", class.fmt())
    }

    fn mark(&self, gc: &mut Gc) {
        gc.mark_object(self.class);
        for (_, value) in &self.values {
            value.mark(gc);
        }
    }

    fn size(&self) -> usize {
        mem::size_of::<ObjClass>() + self.values.capacity() * mem::size_of::<(MemberId, Value)>()
    }
}

#[repr(C)]
pub struct ObjUpvalue {
    pub header: ObjectHeader,
    pub location: *mut Value,
    pub closed: Value
}

impl ObjUpvalue {
    pub fn new(location: *mut Value) -> ObjUpvalue {
        ObjUpvalue {
            header: ObjectHeader::new(ObjectKind::Upvalue),
            location,
            closed: Value::NULL
        }
    }

    pub fn close(&mut self) {
        self.closed = unsafe { *self.location };
        self.location = &raw mut self.closed;
    }
}

impl GcTraceable for ObjUpvalue {
    fn fmt(&self) -> String {
        format!("<up {}>", unsafe { &*self.location })
    }

    fn mark(&self, gc: &mut Gc) {
        unsafe { &*self.location }.mark(gc);
    }

    fn size(&self) -> usize {
        mem::size_of::<ObjUpvalue>()
    }
}

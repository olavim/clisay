use std::{fmt, mem};

use fnv::FnvHashMap;
use nohash_hasher::{IntMap, IntSet};

use super::gc::{Gc, GcTraceable};
use super::value::Value;
use super::vm::{CallFrame, Vm};

#[derive(Clone, Copy, PartialEq)]
pub enum ObjectKind {
    Closure,
    NativeFunction,
    Class,
    BoundMethod,
    String,
    Function,
    Instance,
    Upvalue,
    Array,
    Result
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
            ObjectKind::Upvalue => "upvalue",
            ObjectKind::Array => "array",
            ObjectKind::Result => "result"
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
#[repr(align(8))]
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
    pub upvalue: *mut ObjUpvalue,
    pub array: *mut ObjArray,
    pub result: *mut ObjResult
}

pub const TAG_HEADER: u8 = 0;
pub const TAG_RESULT: u8 = 1;
pub const TAG_CLOSURE: u8 = 3;
pub const TAG_FUNCTION: u8 = 4;
pub const TAG_NATIVE_FUNCTION: u8 = 5;
pub const TAG_BOUND_METHOD: u8 = 6;
pub const TAG_CLASS: u8 = 7;

const PTR_TAG: usize = 0b111;
const PTR_TAG_U8: u8 = 0b111;

fn without_tag<T>(ptr: *mut T) -> *mut T {
    ((ptr as usize) & !PTR_TAG) as *mut T
}

impl Object {
    #[inline]
    pub fn tag(self) -> u8 {
        unsafe { (self.header as u8) & PTR_TAG_U8 }
    }

    pub fn free(self) -> usize {
        macro_rules! free_object {
            ($ptr:expr) => {{
                let size = unsafe { (*$ptr).size() };
                let _ = unsafe { Box::from_raw($ptr) };
                size
            }};
        }

        match self.tag() {
            TAG_RESULT => free_object!(self.as_result_ptr()),
            TAG_FUNCTION => free_object!(self.as_function_ptr()),
            TAG_CLOSURE => free_object!(self.as_closure_ptr()),
            TAG_NATIVE_FUNCTION => free_object!(self.as_native_function_ptr()),
            TAG_BOUND_METHOD => free_object!(self.as_bound_method_ptr()),
            TAG_CLASS => free_object!(self.as_class_ptr()),
            _ => match unsafe { (*self.as_header_ptr()).kind } {
                ObjectKind::String => free_object!(self.as_string_ptr()),
                ObjectKind::Instance => free_object!(self.as_instance_ptr()),
                ObjectKind::Upvalue => free_object!(self.as_upvalue_ptr()),
                ObjectKind::Array => free_object!(self.as_array_ptr()),
                _ => unsafe { std::hint::unreachable_unchecked() }
            }
        }
    }

    fn as_traceable(&self) -> &dyn GcTraceable {
        unsafe { 
            match self.tag() {
                TAG_RESULT => &*self.as_result_ptr(),
                TAG_FUNCTION => &*self.as_function_ptr(),
                TAG_CLOSURE => &*self.as_closure_ptr(),
                TAG_NATIVE_FUNCTION => &*self.as_native_function_ptr(),
                TAG_BOUND_METHOD => &*self.as_bound_method_ptr(),
                TAG_CLASS => &*self.as_class_ptr(),
                _ => match (*self.as_header_ptr()).kind  {
                    ObjectKind::String => &*self.as_string_ptr(),
                    ObjectKind::Instance => &*self.as_instance_ptr(),
                    ObjectKind::Upvalue => &*self.as_upvalue_ptr(),
                    ObjectKind::Array => &*self.as_array_ptr(),
                    _ => std::hint::unreachable_unchecked()
                }
            }
        }
    }

    #[inline]
    pub fn as_header_ptr(&self) -> *mut ObjectHeader {
        unsafe { without_tag(self.header) }
    }

    #[inline]
    pub fn as_string_ptr(&self) -> *mut ObjString {
        unsafe { without_tag(self.string) }
    }

    #[inline]
    pub fn as_string(&self) -> &String {
        unsafe { &(*self.as_string_ptr()).value }
    }

    #[inline]
    pub fn as_function_ptr(&self) -> *mut ObjFn {
        unsafe { without_tag(self.function) }
    }

    #[inline]
    pub fn as_native_function_ptr(&self) -> *mut ObjNativeFn {
        unsafe { without_tag(self.native_function) }
    }

    #[inline]
    pub fn as_bound_method_ptr(&self) -> *mut ObjBoundMethod {
        unsafe { without_tag(self.bound_method) }
    }

    #[inline]
    pub fn as_closure_ptr(&self) -> *mut ObjClosure {
        unsafe { without_tag(self.closure) }
    }

    #[inline]
    pub fn as_class_ptr(&self) -> *mut ObjClass {
        unsafe { without_tag(self.class) }
    }

    #[inline]
    pub fn as_instance_ptr(&self) -> *mut ObjInstance {
        unsafe { without_tag(self.instance) }
    }

    #[inline]
    pub fn as_upvalue_ptr(&self) -> *mut ObjUpvalue {
        unsafe { without_tag(self.upvalue) }
    }

    #[inline]
    pub fn as_array_ptr(&self) -> *mut ObjArray {
        unsafe { without_tag(self.array) }
    }

    #[inline]
    pub fn as_result_ptr(&self) -> *mut ObjResult {
        unsafe { without_tag(self.result) }
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
    ($name:ident, $kind:tt, $tag:ident) => {
        impl From<*mut $kind> for Object {
            fn from($name: *mut $kind) -> Self {
                let $name = ($name as u64 | $tag as u64) as *mut $kind;
                Object { $name }
            }
        }
    };
}

impl_from_for_object!(string, ObjString, TAG_HEADER);
impl_from_for_object!(instance, ObjInstance, TAG_HEADER);
impl_from_for_object!(upvalue, ObjUpvalue, TAG_HEADER);
impl_from_for_object!(array, ObjArray, TAG_HEADER);

impl_from_for_object!(result, ObjResult, TAG_RESULT);
impl_from_for_object!(function, ObjFn, TAG_FUNCTION);
impl_from_for_object!(native_function, ObjNativeFn, TAG_NATIVE_FUNCTION);
impl_from_for_object!(bound_method, ObjBoundMethod, TAG_BOUND_METHOD);
impl_from_for_object!(closure, ObjClosure, TAG_CLOSURE);
impl_from_for_object!(class, ObjClass, TAG_CLASS);

#[repr(align(8))]
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
        format!("\"{}\"", self.value)
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

#[repr(align(8))]
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

pub type NativeFn = fn(vm: &mut Vm, target: Value, args: Vec<Value>) -> Result<(), anyhow::Error>;

#[repr(align(8))]
#[repr(C)]
pub struct ObjNativeFn {
    pub header: ObjectHeader,
    pub name: *mut ObjString,
    pub arity: u8,
    pub function: NativeFn
}

impl ObjNativeFn {
    pub fn new(name: *mut ObjString, arity: u8, function: NativeFn) -> ObjNativeFn {
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

#[repr(align(8))]
#[repr(C)]
pub struct ObjClosure {
    pub header: ObjectHeader,
    pub name: *mut ObjString,
    pub arity: u8,
    pub ip_start: usize,
    pub upvalues: Vec<*mut ObjUpvalue>
}

impl ObjClosure {
    pub fn new(function: *mut ObjFn) -> ObjClosure {
        let func = unsafe { &*function };
        return ObjClosure {
            header: ObjectHeader::new(ObjectKind::Closure),
            name: func.name,
            arity: func.arity,
            ip_start: func.ip_start, 
            upvalues: Vec::new() 
        };
    }
}

impl GcTraceable for ObjClosure {
    fn fmt(&self) -> String {
        format!("<closure {}>", unsafe { &*(*self.name).value } )
    }

    fn mark(&self, gc: &mut Gc) {
        gc.mark_object(self.name);

        for &upvalue in &self.upvalues {
            gc.mark_object(upvalue);
        }
    }

    fn size(&self) -> usize {
        mem::size_of::<ObjClosure>() + self.upvalues.capacity() * mem::size_of::<*mut ObjUpvalue>()
    }
}

#[repr(align(8))]
#[repr(C)]
pub struct ObjBoundMethod {
    pub header: ObjectHeader,
    pub target: Value,
    pub method: Object
}

impl ObjBoundMethod {
    pub fn new(target: Value, method: Object) -> ObjBoundMethod {
        ObjBoundMethod {
            header: ObjectHeader::new(ObjectKind::BoundMethod),
            target,
            method
        }
    }
}

impl GcTraceable for ObjBoundMethod {
    fn fmt(&self) -> String {
        format!("<bound method {}>", self.method.fmt())
    }

    fn mark(&self, gc: &mut Gc) {
        self.target.mark(gc);
        self.method.mark(gc);
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

#[repr(align(8))]
#[repr(C)]
pub struct ObjClass {
    pub header: ObjectHeader,
    pub name: *mut ObjString,
    pub members: FnvHashMap<*mut ObjString, ClassMember>,
    pub fields: IntSet<MemberId>,
    pub methods: IntMap<MemberId, Object>
}

impl ObjClass {
    pub fn new(name: *mut ObjString) -> ObjClass {
        ObjClass {
            header: ObjectHeader::new(ObjectKind::Class),
            name,
            members: FnvHashMap::default(),
            fields: IntSet::default(),
            methods: IntMap::default()
        }
    }

    pub fn resolve(&self, name: *mut ObjString) -> Option<ClassMember> {
        self.members.get(&name).copied()
    }

    pub fn resolve_method(&self, name: *mut ObjString) -> Option<Object> {
        match self.members.get(&name) {
            Some(ClassMember::Method(id)) => self.methods.get(id).copied(),
            _ => None
        }
    }

    pub fn resolve_id(&self, name: *mut ObjString) -> Option<MemberId> {
        match self.members.get(&name) {
            Some(ClassMember::Field(id)) |
            Some(ClassMember::Method(id)) => Some(*id),
            None => None
        }
    }

    pub fn get_method(&self, id: MemberId) -> Object {
        self.methods[&id]
    }
}

impl GcTraceable for ObjClass {
    fn fmt(&self) -> String {
        format!("<class {}>", unsafe { &(*self.name).value })
    }

    fn mark(&self, gc: &mut Gc) {
        gc.mark_object(self.name);
        for (&name, _) in &self.members {
            gc.mark_object(name);
        }
        for (_, &method) in &self.methods {
            gc.mark_object(method);
        }
    }

    fn size(&self) -> usize {
        mem::size_of::<ObjClass>()
            + self.members.capacity() * (mem::size_of::<*mut String>() + mem::size_of::<ClassMember>())
    }
}

#[repr(align(8))]
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
                Value::from(class.get_method(id))
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

#[repr(align(8))]
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

#[repr(align(8))]
#[repr(C)]
pub struct ObjArray {
    pub header: ObjectHeader,
    pub values: Vec<Value>
}

impl ObjArray {
    pub fn new(values: Vec<Value>) -> ObjArray {
        ObjArray {
            header: ObjectHeader::new(ObjectKind::Array),
            values
        }
    }
}

impl GcTraceable for ObjArray {
    fn fmt(&self) -> String {
        format!("<array {}>", self.values.len())
    }

    fn mark(&self, gc: &mut Gc) {
        for value in &self.values {
            value.mark(gc);
        }
    }

    fn size(&self) -> usize {
        mem::size_of::<ObjArray>() + self.values.capacity() * mem::size_of::<Value>()
    }
}

#[repr(align(8))]
#[repr(C)]
pub struct ObjResult {
    pub header: ObjectHeader,
    pub value: Value,
    pub error: bool
}

impl ObjResult {
    pub fn new(value: Value, error: bool) -> ObjResult {
        ObjResult {
            header: ObjectHeader::new(ObjectKind::Result),
            value,
            error
        }
    }
}

impl GcTraceable for ObjResult {
    fn fmt(&self) -> String {
        format!("<result {}>", if self.error { "error" } else { "ok" })
    }

    fn mark(&self, gc: &mut Gc) {
        self.value.mark(gc);
    }

    fn size(&self) -> usize {
        mem::size_of::<ObjResult>()
    }
}

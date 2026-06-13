use std::{fmt, mem};

use fnv::FnvHashMap;
use nohash_hasher::{IntMap, IntSet};

use super::gc::{Gc, GcTraceable};
use super::host::Host;
use super::value::Value;

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

/// A tagged pointer to a heap object. Objects are 8-aligned, so the low 3 bits of
/// the pointer can hold a `tag` that lets the hot call path classify callables without
/// dereferencing the object.
pub const TAG_HEADER: u8 = 0;
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

/// The single source of truth for the set of heap object types. Each row is
/// `Kind => Struct, union_field, accessor, pointer_tag, display_name` and drives
/// everything that must enumerate the object kinds.
macro_rules! objects {
    ( $( $kind:ident => $ty:ty, $field:ident, $accessor:ident, $tag:ident, $display:literal );+ $(;)? ) => {
        #[derive(Clone, Copy, PartialEq)]
        pub enum ObjectKind {
            $( $kind ),+
        }

        impl fmt::Display for ObjectKind {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "{}", match self {
                    $( ObjectKind::$kind => $display ),+
                })
            }
        }

        #[derive(Clone, Copy)]
        #[repr(align(8))]
        #[repr(C)]
        pub union Object {
            pub header: *mut ObjectHeader,
            $( pub $field: *mut $ty ),+
        }

        impl Object {
            $(
                #[inline]
                pub fn $accessor(&self) -> *mut $ty {
                    without_tag(unsafe { self.$field })
                }
            )+

            #[inline]
            pub fn kind(&self) -> ObjectKind {
                unsafe { (*self.as_header_ptr()).kind }
            }

            fn as_traceable(&self) -> &dyn GcTraceable {
                match self.kind() {
                    $( ObjectKind::$kind => unsafe { &*self.$accessor() }, )+
                }
            }

            /// Drops the object's owned data in place but does **not** deallocate
            /// the backing block — that is left to the GC's free list so the
            /// allocation can be recycled.
            pub fn free(self) -> (usize, usize) {
                match self.kind() {
                    $(
                        ObjectKind::$kind => {
                            let ptr = self.$accessor();
                            let accounted = unsafe { (*ptr).size() };
                            let layout = unsafe { (*ptr).layout_size() };
                            unsafe { std::ptr::drop_in_place(ptr) };
                            (accounted, layout)
                        }
                    ),+
                }
            }
        }

        $(
            impl From<*mut $ty> for Object {
                #[inline]
                fn from(ptr: *mut $ty) -> Self {
                    Object { header: (ptr as u64 | $tag as u64) as *mut ObjectHeader }
                }
            }
        )+
    };
}

objects! {
    String         => ObjString,      string,          as_string_ptr,           TAG_HEADER,          "string";
    Instance       => ObjInstance,    instance,        as_instance_ptr,         TAG_HEADER,          "instance";
    Upvalue        => ObjUpvalue,     upvalue,         as_upvalue_ptr,          TAG_HEADER,          "upvalue";
    Array          => ObjArray,       array,           as_array_ptr,            TAG_HEADER,          "array";
    Function       => ObjFn,          function,        as_function_ptr,         TAG_FUNCTION,        "function";
    NativeFunction => ObjNativeFn,    native_function, as_native_function_ptr,  TAG_NATIVE_FUNCTION, "function";
    BoundMethod    => ObjBoundMethod, bound_method,    as_bound_method_ptr,     TAG_BOUND_METHOD,    "function";
    Closure        => ObjClosure,     closure,         as_closure_ptr,          TAG_CLOSURE,         "function";
    Class          => ObjClass,       class,           as_class_ptr,            TAG_CLASS,           "class";
}

impl Object {
    #[inline]
    pub fn tag(self) -> u8 {
        unsafe { (self.header as u8) & PTR_TAG_U8 }
    }

    #[inline]
    pub fn as_header_ptr(&self) -> *mut ObjectHeader {
        unsafe { without_tag(self.header) }
    }

    #[inline]
    pub fn as_string(&self) -> &String {
        unsafe { &(*self.as_string_ptr()).value }
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

pub type NativeFn = fn(host: &mut dyn Host, target: Value, args: Vec<Value>) -> Result<(), anyhow::Error>;

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

/// A closure's captured upvalues are stored as a trailing array in the same
/// allocation as the struct, sized to the exact capture count.
#[repr(align(8))]
#[repr(C)]
pub struct ObjClosure {
    pub header: ObjectHeader,
    pub name: *mut ObjString,
    pub arity: u8,
    pub upvalue_count: u8,
    pub ip_start: usize
}

impl ObjClosure {
    /// Byte offset of the trailing upvalue array.
    const UPVALUES_OFFSET: usize = mem::size_of::<ObjClosure>();

    #[inline]
    pub fn alloc_size(count: usize) -> usize {
        Self::UPVALUES_OFFSET + count * mem::size_of::<*mut ObjUpvalue>()
    }

    #[inline]
    fn upvalues_ptr(&self) -> *mut *mut ObjUpvalue {
        unsafe { (self as *const ObjClosure as *mut u8).add(Self::UPVALUES_OFFSET) as *mut *mut ObjUpvalue }
    }

    #[inline]
    pub fn upvalues(&self) -> &[*mut ObjUpvalue] {
        unsafe { std::slice::from_raw_parts(self.upvalues_ptr(), self.upvalue_count as usize) }
    }

    /// Reads the captured upvalue at `idx`.
    /// Safety: callers must guarantee `idx < upvalue_count`.
    #[inline]
    pub unsafe fn upvalue_at(&self, idx: usize) -> *mut ObjUpvalue {
        unsafe { *self.upvalues_ptr().add(idx) }
    }
}

impl GcTraceable for ObjClosure {
    fn fmt(&self) -> String {
        format!("<closure {}>", unsafe { &*(*self.name).value } )
    }

    fn mark(&self, gc: &mut Gc) {
        gc.mark_object(self.name);

        for &upvalue in self.upvalues() {
            gc.mark_object(upvalue);
        }
    }

    fn size(&self) -> usize {
        // The trailing upvalue array shares the struct's allocation.
        Self::alloc_size(self.upvalue_count as usize)
    }

    fn layout_size(&self) -> usize {
        Self::alloc_size(self.upvalue_count as usize)
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
        gc.mark_object(self.method);
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
    /// Field and regular-method names. The accessors/initializer are *not* here;
    /// they're addressed structurally via the id fields below.
    pub members: FnvHashMap<*mut ObjString, ClassMember>,
    pub fields: IntSet<MemberId>,
    pub methods: IntMap<MemberId, Object>,
    pub member_count: u8,
    pub getter_id: Option<MemberId>,
    pub setter_id: Option<MemberId>,
    /// `None` for native classes, which have no initializer.
    pub init_id: Option<MemberId>,
    /// Prebuilt initial instance values (method slots filled, fields `NULL`).
    pub template: Box<[Value]>
}

impl ObjClass {
    pub fn new(name: *mut ObjString) -> ObjClass {
        ObjClass {
            header: ObjectHeader::new(ObjectKind::Class),
            name,
            members: FnvHashMap::default(),
            fields: IntSet::default(),
            methods: IntMap::default(),
            member_count: 0,
            getter_id: None,
            setter_id: None,
            init_id: None,
            template: Box::new([])
        }
    }

    /// Builds the initial instance-value template from the finalized members.
    /// Call once after `methods`/`member_count` are fully populated.
    pub fn build_template(&mut self) {
        let mut values = vec![Value::NULL; self.member_count as usize].into_boxed_slice();
        for (&id, &method) in &self.methods {
            values[id as usize] = Value::from(method);
        }
        self.template = values;
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

    pub fn get_method(&self, id: MemberId) -> Object {
        self.methods[&id]
    }

    pub fn getter(&self) -> Option<Object> {
        self.getter_id.map(|id| self.methods[&id])
    }

    pub fn setter(&self) -> Option<Object> {
        self.setter_id.map(|id| self.methods[&id])
    }

    pub fn initializer(&self) -> Option<Object> {
        self.init_id.map(|id| self.methods[&id])
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
            + self.template.len() * mem::size_of::<Value>()
    }
}

#[repr(align(8))]
#[repr(C)]
pub struct ObjInstance {
    pub header: ObjectHeader,
    pub class: *mut ObjClass,
    /// Member values indexed directly by member id.
    pub values: Box<[Value]>
}

impl ObjInstance {
    pub fn new(class_ptr: *mut ObjClass) -> ObjInstance {
        let class = unsafe { &*class_ptr };
        ObjInstance {
            header: ObjectHeader::new(ObjectKind::Instance),
            class: class_ptr,
            values: class.template.clone()
        }
    }

    #[inline]
    pub fn get(&self, id: MemberId) -> Value {
        self.values[id as usize]
    }

    #[inline]
    pub fn set(&mut self, id: MemberId, value: Value) {
        self.values[id as usize] = value;
    }
}

impl GcTraceable for ObjInstance {
    fn fmt(&self) -> String {
        let class = unsafe { &*self.class };
        format!("<instance {}>", class.fmt())
    }

    fn mark(&self, gc: &mut Gc) {
        gc.mark_object(self.class);
        for value in self.values.iter() {
            value.mark(gc);
        }
    }

    fn size(&self) -> usize {
        mem::size_of::<ObjInstance>() + self.values.len() * mem::size_of::<Value>()
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

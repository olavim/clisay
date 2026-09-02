use std::{fmt, mem};

use fnv::FnvHashMap;
use nohash_hasher::{IntMap, IntSet};

use super::gc::{Gc, GcTraceable};
use super::host::Host;
use super::value::{DictKey, Value};

#[inline]
pub fn arguments_may_carry_witness(stack_start: *mut Value, arity: usize) -> bool {
    (0..arity).any(|i| may_carry_witness(unsafe { *stack_start.add(i + 1) }))
}

/// A type or trait declaration's runtime identity.
pub type TypeId = u16;

#[repr(C)]
pub struct ObjectHeader {
    pub kind: ObjectKind,
    pub marked: bool,
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
pub const TAG_TYPE: u8 = 7;

const PTR_TAG: usize = 0b111;
const PTR_TAG_U8: u8 = 0b111;

fn without_tag<T>(ptr: *mut T) -> *mut T {
    ((ptr as usize) & !PTR_TAG) as *mut T
}

/// The single source of truth for the set of heap object types.
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

            /// Marks everything the object owns. Dispatching on the concrete type keeps the
            /// allocation pointer, which whatever an object stores past its struct needs.
            fn mark_owned(&self, gc: &mut Gc) {
                match self.kind() {
                    $(
                        ObjectKind::$kind => {
                            let ptr = self.$accessor();
                            unsafe { (*ptr).mark(gc) };
                            unsafe { <$ty>::mark_trailing(ptr, gc) };
                        }
                    ),+
                }
            }

            /// What the object occupies now, which includes anything it grew into after it was
            /// allocated.
            pub fn size(self) -> usize {
                match self.kind() {
                    $(
                        ObjectKind::$kind => unsafe { (*self.$accessor()).size() }
                    ),+
                }
            }

            /// Drops the object's owned data in place but does **not** deallocate
            /// the backing block, which is left to the GC's free list so the
            /// allocation can be recycled. Answers the block's size, for the free list.
            pub fn free(self) -> usize {
                match self.kind() {
                    $(
                        ObjectKind::$kind => {
                            let ptr = self.$accessor();
                            let layout = unsafe { (*ptr).layout_size() };
                            unsafe { std::ptr::drop_in_place(ptr) };
                            layout
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
    Type           => ObjType,        ty,              as_type_ptr,             TAG_TYPE,            "type";
    Dict           => ObjDict,         dict,            as_dict_ptr,             TAG_HEADER,          "dict";
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
        self.mark_owned(gc);
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
    pub arity: u8,
    pub param_accepts: u16,
    pub slot_accepts: u16,
    pub name: *mut ObjString,
    pub ip_start: usize,
    pub upvalues: Vec<UpvalueLocation>,
}

impl ObjFn {
    pub fn new(name: *mut ObjString, arity: u8, ip_start: usize, upvalues: Vec<UpvalueLocation>, param_accepts: u16, slot_accepts: u16) -> ObjFn {
        ObjFn {
            header: ObjectHeader::new(ObjectKind::Function),
            name,
            arity,
            ip_start,
            upvalues,
            param_accepts,
            slot_accepts
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

#[repr(align(8))]
#[repr(C)]
pub struct ObjClosure {
    pub header: ObjectHeader,
    pub arity: u8,
    pub upvalue_count: u8,
    pub param_accepts: u16,
    pub slot_accepts: u16,
    pub name: *mut ObjString,
    pub ip_start: usize,
}

impl ObjClosure {
    /// Byte offset of the trailing upvalue array.
    const UPVALUES_OFFSET: usize = mem::size_of::<ObjClosure>();

    #[inline]
    pub fn alloc_size(count: usize) -> usize {
        Self::UPVALUES_OFFSET + count * mem::size_of::<*mut ObjUpvalue>()
    }

    /// The trailing upvalue array.
    #[inline]
    pub unsafe fn upvalues_ptr(closure: *const ObjClosure) -> *mut *mut ObjUpvalue {
        unsafe { (closure as *mut u8).add(Self::UPVALUES_OFFSET) as *mut *mut ObjUpvalue }
    }

    #[inline]
    pub unsafe fn upvalue_at(closure: *const ObjClosure, idx: usize) -> *mut ObjUpvalue {
        unsafe { *Self::upvalues_ptr(closure).add(idx) }
    }
}

impl GcTraceable for ObjClosure {
    fn fmt(&self) -> String {
        format!("<closure {}>", unsafe { &*(*self.name).value } )
    }

    fn mark(&self, gc: &mut Gc) {
        gc.mark_object(self.name);
    }

    unsafe fn mark_trailing(ptr: *const ObjClosure, gc: &mut Gc) {
        for i in 0..unsafe { (*ptr).upvalue_count } as usize {
            gc.mark_object(unsafe { Self::upvalue_at(ptr, i) });
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

#[inline]
pub fn may_carry_witness(value: Value) -> bool {
    if value.is_number() || value.is_bool() {
        return false;
    }

    if value.is_null() {
        return true;
    }

    if !value.is_object() || value.as_object().kind() != ObjectKind::Instance {
        return false;
    }

    let ty = unsafe { &*(*value.as_object().as_instance_ptr()).ty };
    !ty.witness_ids.is_empty()
}

#[derive(Clone, Copy, PartialEq)]
pub enum TypeMember {
    Field(MemberId),
    Method(MemberId)
}

impl TypeMember {
    pub fn id(&self) -> MemberId {
        let (TypeMember::Field(id) | TypeMember::Method(id)) = self;
        *id
    }
}

#[derive(Clone)]
pub struct BuiltinLayout {
    pub id: TypeId,
    pub members: Vec<(String, TypeMember)>,
    /// Members below this id are fields and the rest are methods, as on `ObjType`.
    pub field_count: MemberId,
    pub factory_id: MemberId,
    pub member_count: MemberId,
}

#[repr(align(8))]
#[repr(C)]
pub struct ObjType {
    pub header: ObjectHeader,
    pub name: *mut ObjString,
    /// Members below this id are fields and the rest are methods.
    pub field_count: MemberId,
    pub id: TypeId,
    pub members: FnvHashMap<*mut ObjString, TypeMember>,
    pub methods: IntMap<MemberId, Object>,
    /// The declaration id of every trait/type this type provides: its own, plus every `with`-mixed trait.
    pub provided: IntSet<TypeId>,
    /// The obligation witnesses this type provides, by id, sorted.
    pub witness_ids: Box<[u16]>,
    /// What each field accepts, by field id, as a witness-pool index.
    pub field_accepts: Box<[u16]>,
    pub member_count: u8,
    pub getter_id: Option<MemberId>,
    pub setter_id: Option<MemberId>,
    pub factory_id: Option<MemberId>,
    /// Prebuilt initial instance values.
    pub template: Box<[Value]>,
    /// The `gives` delegations.
    pub gives: Box<[(MemberId, *mut ObjString, *mut ObjString, TypeId)]>
}

impl ObjType {
    pub fn new(name: *mut ObjString) -> ObjType {
        ObjType {
            header: ObjectHeader::new(ObjectKind::Type),
            name,
            members: FnvHashMap::default(),
            field_count: 0,
            id: TypeId::MAX,
            methods: IntMap::default(),
            provided: IntSet::default(),
            witness_ids: Box::new([]),
            field_accepts: Box::new([]),
            member_count: 0,
            getter_id: None,
            setter_id: None,
            factory_id: None,
            template: Box::new([]),
            gives: Box::new([])
        }
    }

    pub fn duplicate(&self) -> ObjType {
        ObjType {
            header: ObjectHeader::new(ObjectKind::Type),
            name: self.name,
            members: self.members.clone(),
            field_count: self.field_count,
            id: self.id,
            methods: self.methods.clone(),
            provided: self.provided.clone(),
            witness_ids: self.witness_ids.clone(),
            field_accepts: self.field_accepts.clone(),
            member_count: self.member_count,
            getter_id: self.getter_id,
            setter_id: self.setter_id,
            factory_id: self.factory_id,
            template: Box::new([]),
            gives: self.gives.clone(),
        }
    }

    pub fn build_template(&mut self) {
        let mut values = vec![Value::NULL; self.member_count as usize].into_boxed_slice();
        for (&id, &method) in &self.methods {
            values[id as usize] = Value::from(method);
        }
        self.template = values;
    }

    pub fn resolve(&self, name: *mut ObjString) -> Option<TypeMember> {
        self.members.get(&name).copied()
    }

    pub fn resolve_method(&self, name: *mut ObjString) -> Option<Object> {
        match self.members.get(&name) {
            Some(TypeMember::Method(id)) => self.methods.get(id).copied(),
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

    pub fn factory(&self) -> Option<Object> {
        self.factory_id.map(|id| self.methods[&id])
    }
}

impl GcTraceable for ObjType {
    fn fmt(&self) -> String {
        format!("<type {}>", unsafe { &(*self.name).value })
    }

    fn mark(&self, gc: &mut Gc) {
        gc.mark_object(self.name);
        for (&name, _) in &self.members {
            gc.mark_object(name);
        }
        for (_, &method) in &self.methods {
            gc.mark_object(method);
        }
        for &(_, field_name, trait_name, _) in &self.gives {
            gc.mark_object(field_name);
            gc.mark_object(trait_name);
        }
    }

    fn size(&self) -> usize {
        mem::size_of::<ObjType>()
            + self.members.capacity() * (mem::size_of::<*mut String>() + mem::size_of::<TypeMember>())
            + self.methods.capacity() * (mem::size_of::<MemberId>() + mem::size_of::<Object>())
            + self.provided.capacity() * mem::size_of::<TypeId>()
            + self.witness_ids.len() * mem::size_of::<u16>()
            + self.template.len() * mem::size_of::<Value>()
            + self.gives.len() * mem::size_of::<(MemberId, *mut ObjString, *mut ObjString, TypeId)>()
    }
}

#[repr(align(8))]
#[repr(C)]
pub struct ObjInstance {
    pub header: ObjectHeader,
    pub ty: *mut ObjType,
    /// Member values indexed directly by member id.
    pub values: Box<[Value]>
}

impl ObjInstance {
    pub fn new(type_ptr: *mut ObjType) -> ObjInstance {
        let ty = unsafe { &*type_ptr };
        ObjInstance {
            header: ObjectHeader::new(ObjectKind::Instance),
            ty: type_ptr,
            values: ty.template.clone()
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
        let ty = unsafe { &*self.ty };
        format!("<instance {}>", ty.fmt())
    }

    fn mark(&self, gc: &mut Gc) {
        gc.mark_object(self.ty);
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
    pub accepts: u16,
    pub location: *mut Value,
    pub closed: Value,
}

impl ObjUpvalue {
    pub fn is_closed(&self) -> bool {
        std::ptr::eq(self.location as *const Value, &raw const self.closed)
    }

    pub fn new(location: *mut Value, accepts: u16) -> ObjUpvalue {
        ObjUpvalue {
            header: ObjectHeader::new(ObjectKind::Upvalue),
            location,
            closed: Value::NULL,
            accepts
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
pub struct ObjDict {
    pub header: ObjectHeader,
    pub entries: FnvHashMap<DictKey, Value>
}

impl ObjDict {
    pub fn new(entries: FnvHashMap<DictKey, Value>) -> ObjDict {
        ObjDict {
            header: ObjectHeader::new(ObjectKind::Dict),
            entries
        }
    }
}

impl GcTraceable for ObjDict {
    fn fmt(&self) -> String {
        format!("<dict {}>", self.entries.len())
    }

    fn mark(&self, gc: &mut Gc) {
        for (key, value) in &self.entries {
            key.0.mark(gc);
            value.mark(gc);
        }
    }

    fn size(&self) -> usize {
        mem::size_of::<ObjDict>()
            + self.entries.capacity() * (mem::size_of::<Value>() + mem::size_of::<Value>())
    }
}

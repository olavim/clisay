use std::{fmt, mem};

use fnv::FnvHashMap;
use nohash_hasher::{IntMap, IntSet};

use super::gc::{Gc, GcTraceable};
use super::host::Host;
use super::value::{DictKey, Value, ValueKind};

#[inline]
pub fn arguments_need_accepts_check(stack_start: *mut Value, arity: usize) -> bool {
    (0..arity).any(|i| needs_accepts_check(unsafe { *stack_start.add(i + 1) }))
}

/// A type or trait declaration's runtime identity.
pub type TypeId = u16;
pub const UNDECLARED: TypeId = TypeId::MAX;

/// GC has reached this object.
pub const FLAG_MARKED: u8 = 1 << 0;
/// A second holder may have this value, so a write to it will fork.
pub const FLAG_SHARED: u8 = 1 << 1;
pub const FLAG_ELEMENTS_SHARED: u8 = 1 << 2;
pub const FLAG_ON_ANCHOR_PATH: u8 = 1 << 3;
pub const FLAG_IS_REF: u8 = 1 << 4;
pub const FLAG_RETURNS_VALUE: u8 = 1 << 5;
/// The container's `hash` field is its deep hash. Set only while it is shared, since only an
/// unshared value is written in place.
pub const FLAG_HASHED: u8 = 1 << 6;
/// A store replaced this container while it was on an anchor's path, so an anchor to it no
/// longer reaches the storage it was formed on.
pub const FLAG_DETACHED: u8 = 1 << 7;

pub const REF_VALUE_FIELD: MemberId = 0;
pub const REF_LOCK_FIELD: MemberId = 1;

#[repr(C)]
pub struct ObjectHeader {
    pub kind: ObjectKind,
    pub flags: u8,
}

impl ObjectHeader {
    pub fn new(kind: ObjectKind) -> ObjectHeader {
        ObjectHeader { kind, flags: 0 }
    }

    #[inline]
    pub fn has(&self, flag: u8) -> bool {
        self.flags & flag != 0
    }

    #[inline]
    pub fn set(&mut self, flag: u8) {
        self.flags |= flag;
    }

    #[inline]
    pub fn clear(&mut self, flag: u8) {
        self.flags &= !flag;
    }
}

// A tagged pointer to a heap object. Objects are 8-aligned, so the low 3 bits of
// the pointer can hold a `tag` that makes it possible to classify callables without
// dereferencing the object.
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

            /// Marks everything the object owns.
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

            /// Drops the object's owned data in place but doesn't deallocate
            /// the backing block.
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
    Array          => ObjArray,       array,           as_array_ptr,            TAG_HEADER,          "array";
    Function       => ObjFn,          function,        as_function_ptr,         TAG_FUNCTION,        "function";
    NativeFunction => ObjNativeFn,    native_function, as_native_function_ptr,  TAG_NATIVE_FUNCTION, "function";
    BoundMethod    => ObjBoundMethod, bound_method,    as_bound_method_ptr,     TAG_BOUND_METHOD,    "function";
    Closure        => ObjClosure,     closure,         as_closure_ptr,          TAG_CLOSURE,         "function";
    Type           => ObjType,        ty,              as_type_ptr,             TAG_TYPE,            "type";
    Dict           => ObjDict,        dict,            as_dict_ptr,             TAG_HEADER,          "dict";
}

#[inline]
pub fn mark_shared(value: Value) {
    if value.is_object() {
        unsafe { (*value.as_object().as_header_ptr()).set(FLAG_SHARED) };
    }
}

/// Marks a container on an anchor's path, and says whether it's a `Ref`.
#[inline]
pub fn mark_on_anchor_path(value: Value) -> bool {
    if !value.is_object() {
        return false;
    }
    let header = unsafe { &mut *value.as_object().as_header_ptr() };
    header.set(FLAG_ON_ANCHOR_PATH);
    header.has(FLAG_IS_REF)
}

#[inline]
pub fn step_through(value: Value) {
    if !value.is_object() {
        return;
    }
    let header = value.as_object().as_header_ptr();
    let flags = unsafe { (*header).flags };
    if flags & FLAG_ELEMENTS_SHARED != 0 {
        mark_every_element(value, header);
    }
}

#[inline]
fn has_flag(value: Value, flag: u8) -> bool {
    value.is_object() && unsafe { (*value.as_object().as_header_ptr()).has(flag) }
}

#[inline]
pub fn is_detached(value: Value) -> bool {
    has_flag(value, FLAG_DETACHED)
}

#[inline]
pub fn is_on_anchor_path(value: Value) -> bool {
    has_flag(value, FLAG_ON_ANCHOR_PATH)
}

#[inline]
pub fn is_ref(value: Value) -> bool {
    has_flag(value, FLAG_IS_REF)
}

#[inline]
fn detach_if_on_anchor_path(replaced: Value) {
    if is_on_anchor_path(replaced) {
        detach(replaced);
    }
}

/// Detaches the value a store replaces inside `container`.
#[inline]
pub fn detach_replaced(container: &ObjectHeader, replaced: impl FnOnce() -> Value) {
    if container.has(FLAG_ON_ANCHOR_PATH) {
        detach_if_on_anchor_path(replaced());
    }
}

#[cold]
pub fn clear_anchor_path_flags(value: Value) {
    for_each_element(value, |element| if is_on_anchor_path(element) { clear_anchor_path_flags(element) });
    unsafe { (*value.as_object().as_header_ptr()).clear(FLAG_ON_ANCHOR_PATH | FLAG_DETACHED) };
}

#[cold]
fn detach(value: Value) {
    let header = unsafe { &mut *value.as_object().as_header_ptr() };
    if header.has(FLAG_DETACHED) {
        return;
    }
    header.set(FLAG_DETACHED);
    for_each_element(value, |element| if is_on_anchor_path(element) { detach(element) });
}

/// Where a flagged element sits in its container
pub enum ElementAt {
    Index(usize),
    Key(DictKey),
}

pub fn elements_on_anchor_path(value: Value) -> Vec<(ElementAt, Value)> {
    let at_index = |(i, v): (usize, &Value)| (ElementAt::Index(i), *v);
    match value.kind() {
        ValueKind::Object(ObjectKind::Array) => unsafe { ObjArray::elements(value.as_object().as_array_ptr()).iter() }
            .enumerate().filter(|(_, v)| is_on_anchor_path(**v)).map(at_index).collect(),
        ValueKind::Object(ObjectKind::Instance) => unsafe { ObjInstance::values(value.as_object().as_instance_ptr()).iter() }
            .enumerate().filter(|(_, v)| is_on_anchor_path(**v)).map(at_index).collect(),
        ValueKind::Object(ObjectKind::Dict) => unsafe { (&*value.as_object().as_dict_ptr()).entries.iter() }
            .filter(|(_, v)| is_on_anchor_path(**v)).map(|(k, v)| (ElementAt::Key(*k), *v)).collect(),
        _ => Vec::new(),
    }
}

pub fn set_element(container: Value, at: &ElementAt, value: Value) {
    match (container.kind(), at) {
        (ValueKind::Object(ObjectKind::Array), ElementAt::Index(i)) => unsafe { ObjArray::elements_mut(container.as_object().as_array_ptr())[*i] = value },
        (ValueKind::Object(ObjectKind::Instance), ElementAt::Index(i)) => unsafe { ObjInstance::values_mut(container.as_object().as_instance_ptr())[*i] = value },
        (ValueKind::Object(ObjectKind::Dict), ElementAt::Key(k)) => { unsafe { (&mut *container.as_object().as_dict_ptr()).entries.insert(*k, value) }; },
        _ => {},
    }
}

#[inline]
pub fn mark_as_ref(value: Value) {
    if value.is_object() {
        unsafe { (*value.as_object().as_header_ptr()).set(FLAG_IS_REF) };
    }
}

#[cfg(debug_assertions)]
pub fn debug_assert_store_is_acyclic(container: Value, value: Value, depth_limit: usize) {
    // A `Ref` is allowed to contain itself.
    if is_ref(container) {
        return;
    }
    assert!(!super::equality::is_or_contains_unshared(value, container, depth_limit), "a store made a container contain itself");
}

#[inline]
pub fn may_not_persist(value: Value) -> bool {
    if !value.is_object() {
        return false;
    }
    let object = value.as_object();
    unsafe { (*object.as_header_ptr()).kind == ObjectKind::Instance && (*(*object.as_instance_ptr()).ty).no_persist }
}

#[inline]
pub fn is_container(value: Value) -> bool {
    matches!(value.kind(), ValueKind::Object(ObjectKind::Array | ObjectKind::Dict | ObjectKind::Instance))
}

#[inline]
pub fn clear_shared(value: Value) {
    if value.is_object() {
        unsafe { (*value.as_object().as_header_ptr()).clear(FLAG_SHARED | FLAG_HASHED) };
    }
}

/// A container's cached deep hash, if it has one.
#[inline]
pub fn cached_hash(value: Value) -> Option<u32> {
    let header = unsafe { &*value.as_object().as_header_ptr() };
    header.has(FLAG_HASHED).then(|| unsafe { *hash_slot_of(value) })
}

pub fn cache_hash(value: Value, hash: u32) {
    let header = unsafe { &mut *value.as_object().as_header_ptr() };
    // A write to a shared container lands in a copy, so the shared one keeps its contents and its
    // hash. An unshared container is written in place, which would leave a cached hash stale.
    if !header.has(FLAG_SHARED) {
        return;
    }
    unsafe { *hash_slot_of(value) = hash };
    header.set(FLAG_HASHED);
}

fn hash_slot_of(value: Value) -> *mut u32 {
    let object = value.as_object();
    unsafe {
        match (*object.as_header_ptr()).kind {
            ObjectKind::Array => &raw mut (*object.as_array_ptr()).hash,
            ObjectKind::Dict => &raw mut (*object.as_dict_ptr()).hash,
            _ => &raw mut (*object.as_instance_ptr()).hash,
        }
    }
}

#[inline]
fn defer_element_marks(original: Value, copy: Value) {
    unsafe { (*original.as_object().as_header_ptr()).set(FLAG_ELEMENTS_SHARED) };
    unsafe { (*copy.as_object().as_header_ptr()).set(FLAG_ELEMENTS_SHARED) };
}

pub fn mark_elements_shared(value: Value) {
    if !value.is_object() {
        return;
    }
    let header = value.as_object().as_header_ptr();
    if unsafe { !(*header).has(FLAG_ELEMENTS_SHARED) } {
        return;
    }
    mark_every_element(value, header);
}

#[cold]
pub fn mark_elements_shared_except_on_anchor_path(original: Value, copy: Value) {
    unsafe { (*copy.as_object().as_header_ptr()).clear(FLAG_ELEMENTS_SHARED) };
    unsafe { (*original.as_object().as_header_ptr()).clear(FLAG_ELEMENTS_SHARED) };
    for_each_element(original, |element| if !is_on_anchor_path(element) { mark_shared(element) });
}

#[cold]
fn mark_every_element(value: Value, header: *mut ObjectHeader) {
    unsafe { (*header).clear(FLAG_ELEMENTS_SHARED) };
    for_each_element(value, mark_shared);
}

fn for_each_element(value: Value, mut f: impl FnMut(Value)) {
    match value.kind() {
        ValueKind::Object(ObjectKind::Array) => unsafe { ObjArray::elements(value.as_object().as_array_ptr()).iter().for_each(|v| f(*v)) },
        ValueKind::Object(ObjectKind::Dict) => unsafe { (*value.as_object().as_dict_ptr()).entries.values().for_each(|v| f(*v)) },
        ValueKind::Object(ObjectKind::Instance) => unsafe { ObjInstance::values(value.as_object().as_instance_ptr()).iter().for_each(|v| f(*v)) },
        _ => {},
    }
}

#[inline]
pub fn is_shared(value: Value) -> bool {
    has_flag(value, FLAG_SHARED)
}

#[cfg(debug_assertions)]
pub fn element_count(value: Value) -> usize {
    match value.kind() {
        ValueKind::Object(ObjectKind::Array) => unsafe { (*value.as_object().as_array_ptr()).len as usize },
        ValueKind::Object(ObjectKind::Dict) => unsafe { (*value.as_object().as_dict_ptr()).entries.len() },
        ValueKind::Object(ObjectKind::Instance) => unsafe { (*value.as_object().as_instance_ptr()).member_count as usize },
        _ => 0,
    }
}

/// Creates a shallow copy.
#[cold]
pub fn copy_object(gc: &mut Gc, value: Value) -> Value {
    if is_ref(value) {
        return value;
    }
    let ValueKind::Object(kind) = value.kind() else { return value };
    let object = value.as_object();
    match kind {
        ObjectKind::Array => {
            let source = object.as_array_ptr();
            let copy = Value::from(gc.alloc_array(unsafe { ObjArray::elements(source) }, unsafe { (*source).capacity } as usize));
            defer_element_marks(value, copy);
            copy
        },
        ObjectKind::Dict => {
            let entries = unsafe { (*object.as_dict_ptr()).entries.clone() };
            let copy = Value::from(gc.alloc(ObjDict::new(entries)));
            defer_element_marks(value, copy);
            copy
        },
        ObjectKind::Instance => {
            let source = object.as_instance_ptr();
            let copy = Value::from(gc.alloc_instance(unsafe { (*source).ty }, unsafe { ObjInstance::values(source) }));
            defer_element_marks(value, copy);
            copy
        },
        _ => value,
    }
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
pub struct CaptureLocation {
    pub is_local: bool,
    pub location: u8
}

#[repr(align(8))]
#[repr(C)]
pub struct ObjFn {
    pub header: ObjectHeader,
    pub arity: u8,
    pub param_list_pool_id: u16,
    pub slot_witness_set_pool_id: u16,
    pub name: *mut ObjString,
    pub ip_start: usize,
    pub capture_locations: Vec<CaptureLocation>,
}

impl ObjFn {
    pub fn new(name: *mut ObjString, arity: u8, ip_start: usize, capture_locations: Vec<CaptureLocation>, param_list_pool_id: u16, slot_witness_set_pool_id: u16) -> ObjFn {
        ObjFn {
            header: ObjectHeader::new(ObjectKind::Function),
            name,
            arity,
            ip_start,
            capture_locations,
            param_list_pool_id,
            slot_witness_set_pool_id
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
        mem::size_of::<ObjFn>() + self.capture_locations.capacity() * mem::size_of::<CaptureLocation>()
    }
}

pub type NativeFn = fn(host: &mut dyn Host, target: Value, args: Vec<Value>) -> Result<(), anyhow::Error>;

#[repr(align(8))]
#[repr(C)]
pub struct ObjNativeFn {
    pub header: ObjectHeader,
    pub name: *mut ObjString,
    pub arity: u8,
    /// Whether it wants an anchor receiver, as a method declaring `&var this` does.
    pub wants_anchor_receiver: bool,
    pub function: NativeFn
}

impl ObjNativeFn {
    pub fn new(name: *mut ObjString, arity: u8, function: NativeFn) -> ObjNativeFn {
        ObjNativeFn {
            header: ObjectHeader::new(ObjectKind::NativeFunction),
            name,
            arity,
            wants_anchor_receiver: false,
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

// A closure's captured upvalues are stored as a trailing array.
#[repr(align(8))]
#[repr(C)]
pub struct ObjClosure {
    pub header: ObjectHeader,
    pub arity: u8,
    pub capture_count: u8,
    pub param_list_pool_id: u16,
    pub slot_witness_set_pool_id: u16,
    pub name: *mut ObjString,
    pub ip_start: usize,
}

impl ObjClosure {
    /// Byte offset of the trailing capture array.
    const CAPTURES_OFFSET: usize = mem::size_of::<ObjClosure>();

    #[inline]
    pub fn alloc_size(count: usize) -> usize {
        Self::CAPTURES_OFFSET + count * mem::size_of::<Value>()
    }

    /// The trailing capture array. A closure holds the values it captured, not the slots.
    #[inline]
    pub unsafe fn captures_ptr(closure: *const ObjClosure) -> *mut Value {
        unsafe { (closure as *mut u8).add(Self::CAPTURES_OFFSET) as *mut Value }
    }

    #[inline]
    pub unsafe fn capture_at(closure: *const ObjClosure, idx: usize) -> Value {
        unsafe { *Self::captures_ptr(closure).add(idx) }
    }

    #[inline]
    pub unsafe fn set_capture(closure: *mut ObjClosure, idx: usize, value: Value) {
        unsafe { *Self::captures_ptr(closure).add(idx) = value };
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
        for i in 0..unsafe { (*ptr).capture_count } as usize {
            unsafe { Self::capture_at(ptr, i) }.mark(gc);
        }
    }

    fn size(&self) -> usize {
        self.layout_size()
    }

    fn layout_size(&self) -> usize {
        Self::alloc_size(self.capture_count as usize)
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
pub fn needs_accepts_check(value: Value) -> bool {
    if value.is_number() || value.is_bool() {
        return false;
    }

    if value.is_null() || value.is_anchor() {
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
    pub methods: Vec<(MemberId, u8)>,
    pub var_fields: VarFields,
    pub anchorable_fields: VarFields,
    pub field_witness_set_pool_ids: Box<[u16]>,
    pub no_persist: bool,
}

/// A bit per field id, set where the field was declared `var`. A type declares at most 255 fields.
#[derive(Clone, Copy, PartialEq)]
pub struct VarFields([u64; 4]);

impl VarFields {
    pub fn none() -> VarFields {
        VarFields([0; 4])
    }

    pub fn set(&mut self, field: MemberId) {
        self.0[field as usize >> 6] |= 1 << (field & 63);
    }

    #[inline]
    pub fn has(&self, field: MemberId) -> bool {
        self.0[field as usize >> 6] & (1 << (field & 63)) != 0
    }
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
    pub provided: IntSet<TypeId>,
    /// The obligation witnesses this type provides, by id, sorted.
    pub witness_ids: Box<[u16]>,
    /// Whether its instances witness a `no persist` obligation.
    pub no_persist: bool,
    /// Each field's witness set, by field id.
    pub field_witness_set_pool_ids: Box<[u16]>,
    pub var_fields: VarFields,
    pub anchorable_fields: VarFields,
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
            id: UNDECLARED,
            methods: IntMap::default(),
            provided: IntSet::default(),
            witness_ids: Box::new([]),
            no_persist: false,
            field_witness_set_pool_ids: Box::new([]),
            var_fields: VarFields::none(),
            anchorable_fields: VarFields::none(),
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
            no_persist: self.no_persist,
            field_witness_set_pool_ids: self.field_witness_set_pool_ids.clone(),
            var_fields: self.var_fields,
            anchorable_fields: self.anchorable_fields,
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
    pub member_count: u8,
    pub hash: u32,
    pub ty: *mut ObjType,
}

impl ObjInstance {
    /// Byte offset of the trailing member array.
    const VALUES_OFFSET: usize = mem::size_of::<ObjInstance>();

    #[inline]
    pub fn alloc_size(count: usize) -> usize {
        Self::VALUES_OFFSET + count * mem::size_of::<Value>()
    }

    #[inline]
    fn values_ptr(instance: *const ObjInstance) -> *mut Value {
        (instance as *mut u8).wrapping_add(Self::VALUES_OFFSET) as *mut Value
    }

    /// The member values, indexed by member id.
    #[inline]
    pub unsafe fn values<'a>(instance: *const ObjInstance) -> &'a [Value] {
        std::slice::from_raw_parts(Self::values_ptr(instance), (*instance).member_count as usize)
    }

    #[inline]
    pub unsafe fn values_mut<'a>(instance: *mut ObjInstance) -> &'a mut [Value] {
        std::slice::from_raw_parts_mut(Self::values_ptr(instance), (*instance).member_count as usize)
    }

    #[inline]
    pub unsafe fn member_ptr(instance: *const ObjInstance, id: MemberId) -> *mut Value {
        debug_assert!(id < unsafe { (*instance).member_count }, "member id higher than the number of members in the instance");
        unsafe { Self::values_ptr(instance).add(id as usize) }
    }

    #[inline]
    pub unsafe fn get(instance: *const ObjInstance, id: MemberId) -> Value {
        unsafe { *Self::member_ptr(instance, id) }
    }

    #[inline]
    pub unsafe fn set(instance: *mut ObjInstance, id: MemberId, value: Value) {
        unsafe { *Self::member_ptr(instance, id) = value };
    }
}

impl GcTraceable for ObjInstance {
    fn fmt(&self) -> String {
        let ty = unsafe { &*self.ty };
        format!("<instance {}>", ty.fmt())
    }

    fn mark(&self, gc: &mut Gc) {
        gc.mark_object(self.ty);
    }

    unsafe fn mark_trailing(ptr: *const ObjInstance, gc: &mut Gc) {
        for value in unsafe { Self::values(ptr) }.iter() {
            value.mark(gc);
        }
    }

    fn size(&self) -> usize {
        self.layout_size()
    }

    fn layout_size(&self) -> usize {
        Self::alloc_size(self.member_count as usize)
    }
}

#[repr(align(8))]
#[repr(C)]
pub struct ObjArray {
    pub header: ObjectHeader,
    pub hash: u32,
    /// The first element. It points into the array's own allocation until it grows,
    /// and then at a heap buffer.
    pub data: *mut Value,
    pub len: u32,
    /// How many elements fit where `data` points.
    pub capacity: u32,
    /// How many elements fit in the array's own allocation.
    pub inline_capacity: u32,
}

impl ObjArray {
    /// Byte offset of the trailing element array.
    const ELEMENTS_OFFSET: usize = mem::size_of::<ObjArray>();

    #[inline]
    pub fn alloc_size(inline_capacity: usize) -> usize {
        Self::ELEMENTS_OFFSET + inline_capacity * mem::size_of::<Value>()
    }

    #[inline]
    pub fn inline_ptr(array: *const ObjArray) -> *mut Value {
        (array as *mut u8).wrapping_add(Self::ELEMENTS_OFFSET) as *mut Value
    }

    #[inline]
    pub unsafe fn elements<'a>(array: *const ObjArray) -> &'a [Value] {
        std::slice::from_raw_parts((*array).data, (*array).len as usize)
    }

    #[inline]
    pub unsafe fn elements_mut<'a>(array: *mut ObjArray) -> &'a mut [Value] {
        std::slice::from_raw_parts_mut((*array).data, (*array).len as usize)
    }

    #[inline]
    unsafe fn is_data_on_heap(array: *const ObjArray) -> bool {
        (*array).data != Self::inline_ptr(array)
    }

    #[inline]
    pub unsafe fn get_ptr(array: *const ObjArray, index: usize) -> *mut Value {
        debug_assert!(index < (*array).len as usize, "element index past the array's length");
        (*array).data.add(index)
    }

    #[inline]
    pub unsafe fn get(array: *const ObjArray, index: usize) -> Value {
        *Self::get_ptr(array, index)
    }

    #[inline]
    pub unsafe fn set(array: *mut ObjArray, index: usize, value: Value) {
        *Self::get_ptr(array, index) = value;
    }

    #[inline]
    pub unsafe fn push(array: *mut ObjArray, value: Value) {
        if (*array).len == (*array).capacity {
            Self::grow(array);
        }
        *(*array).data.add((*array).len as usize) = value;
        (*array).len += 1;
    }

    #[cold]
    unsafe fn grow(array: *mut ObjArray) {
        let capacity = ((*array).capacity as usize * 2).max(4);
        let len = (*array).len as usize;
        let buffer = match Self::is_data_on_heap(array) {
            true => {
                let mut buffer = Vec::from_raw_parts((*array).data, len, (*array).capacity as usize);
                buffer.reserve_exact(capacity - len);
                buffer
            },
            false => {
                let mut buffer = Vec::with_capacity(capacity);
                buffer.extend_from_slice(Self::elements(array));
                buffer
            },
        };
        let mut buffer = mem::ManuallyDrop::new(buffer);
        (*array).data = buffer.as_mut_ptr();
        (*array).capacity = buffer.capacity() as u32;
    }

    unsafe fn free_buffer(array: *mut ObjArray) {
        if Self::is_data_on_heap(array) {
            drop(Vec::from_raw_parts((*array).data, 0, (*array).capacity as usize));
        }
    }
}

impl Drop for ObjArray {
    fn drop(&mut self) {
        unsafe { Self::free_buffer(self) };
    }
}

impl GcTraceable for ObjArray {
    fn fmt(&self) -> String {
        format!("<array {}>", self.len)
    }

    fn mark(&self, _gc: &mut Gc) {}

    unsafe fn mark_trailing(ptr: *const ObjArray, gc: &mut Gc) {
        for value in unsafe { Self::elements(ptr) } {
            value.mark(gc);
        }
    }

    fn size(&self) -> usize {
        let buffer = match unsafe { Self::is_data_on_heap(self) } {
            true => self.capacity as usize * mem::size_of::<Value>(),
            false => 0,
        };
        self.layout_size() + buffer
    }

    fn layout_size(&self) -> usize {
        Self::alloc_size(self.inline_capacity as usize)
    }
}

#[repr(align(8))]
#[repr(C)]
pub struct ObjDict {
    pub header: ObjectHeader,
    pub hash: u32,
    pub entries: FnvHashMap<DictKey, Value>
}

impl ObjDict {
    pub fn new(entries: FnvHashMap<DictKey, Value>) -> ObjDict {
        ObjDict {
            header: ObjectHeader::new(ObjectKind::Dict),
            hash: 0,
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

#[cfg(test)]
mod layout {
    use super::*;

    #[test]
    fn the_header_fits_before_the_first_pointer() {
        assert_eq!(mem::size_of::<ObjectHeader>(), 2);
        assert_eq!(mem::offset_of!(ObjClosure, name), 8);
        assert_eq!(mem::offset_of!(ObjFn, name), 8);
        assert_eq!(mem::offset_of!(ObjInstance, member_count), 2);
    }

    #[test]
    fn the_hash_cache_fits_before_the_first_pointer() {
        assert_eq!(mem::offset_of!(ObjArray, data), 8);
        assert_eq!(mem::offset_of!(ObjDict, entries), 8);
        assert_eq!(mem::offset_of!(ObjInstance, ty), 8);
    }
}

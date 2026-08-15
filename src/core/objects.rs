use std::{fmt, mem};

use anyhow::bail;
use fnv::FnvHashMap;
use nohash_hasher::{IntMap, IntSet};

use super::gc::{Gc, GcTraceable};
use super::host::Host;
use super::value::{DictKey, Value, ValueKind};

/// The runtime diagnostic raised when a mutation hits an immutable value.
pub const IMMUTABLE_MUTATION: &str = "cannot mutate an immutable value";

/// The runtime diagnostic raised when a mutable value of unknown capability lands in an immutable
/// container at construction.
pub const MUTABLE_IN_IMMUTABLE: &str = "cannot store a mutable value in an immutable container";

/// Sentinel for `ObjectHeader::immutable_origin`: the value is mutable, or no site was recorded.
pub const NO_ORIGIN: u32 = u32::MAX;

/// The runtime diagnostic raised when an opaque call would let a borrowed argument escape.
pub const RETAINED_BORROW: &str = "cannot pass a borrowed argument to a callee that retains it";

/// The runtime diagnostic raised when an opaque call would retain a value the caller still owes an
/// obligation on. Named after the obligation, so it reads like the refusal a resolved call gets.
pub fn retained_owed_value(owed: &str) -> String {
    format!("cannot pass a value owing '{owed}' to a callee that retains it")
}

/// The runtime diagnostic raised when a borrowed value is persisted at a store site.
pub const PERSISTED_BORROW: &str = "cannot persist a borrowed value";
pub const SECOND_ELEMENT_WRITER: &str = "cannot write an element another name already writes";
pub const WROTE_GIVEN_ELEMENT: &str = "cannot write an element whose write-ownership moved to another container";
pub const WROTE_TRANSFERRED_ELEMENT: &str = "cannot write a value whose write-ownership was given away";
pub const GAVE_TRANSFERRED_ELEMENT: &str = "cannot give away a value whose write-ownership was given away";
/// A call that would retain a value an earlier retain already took the write-ownership of.
pub const RETAINED_TWICE: &str = "cannot retain a value whose write-ownership was already taken";

/// Whether a value is a mutable container. Only Array, Dict, and Instance carry the immutable bit;
/// every other object kind and every primitive is always an immutable value.
pub fn is_mutable_container(value: Value) -> bool {
    matches!(value.kind(), ValueKind::Object(ObjectKind::Array | ObjectKind::Dict | ObjectKind::Instance))
        && !value.as_object().is_immutable()
}

/// Whether a value's kind can hold write-ownership over another value, and so needs the marks a
/// scope exit reads. A mutable container owns what is stored into it, and a closure owns what it
/// captured. Nothing else can be asked who may write a value it holds.
pub fn can_own_writes(value: Value) -> bool {
    is_mutable_container(value)
        || matches!(value.kind(), ValueKind::Object(ObjectKind::Closure))
}

/// Marks a value that left the frame that built it. A scope exit must not release what such a
/// value holds, since something outside still reaches it. Only a holder needs the bit, since the
/// question it answers is whether to release the writer slot it holds.
pub fn record_escape(value: Value) {
    if can_own_writes(value) {
        unsafe { (*value.as_object().as_header_ptr()).set(FLAG_ESCAPED, true); }
    }
}

/// Whether a value keeps a lend alive, by being borrowed itself or by holding one.
pub fn carries_borrow(value: Value) -> bool {
    value.is_object() && unsafe { (*value.as_object().as_header_ptr()).has(FLAG_BORROWED | FLAG_HOLDS_BORROW) }
}

pub fn mark_holds_borrow(container: Value) {
    if container.is_object() {
        unsafe { (*container.as_object().as_header_ptr()).set(FLAG_HOLDS_BORROW, true); }
    }
}

/// Records that a borrow reached `container` if `value` carries a borrow.
pub fn record_held_borrow(container: Value, value: Value) {
    if carries_borrow(value) {
        mark_holds_borrow(container);
    }
}

/// Both records a container makes when a value enters it: what it now holds, and who may write it.
/// The host is told too, since a store made inside a primitive never passes through the bytecode.
/// Generic rather than `dyn`, to keep the bytecode store path a static call.
pub fn container_took<H: Host + ?Sized>(host: &mut H, container: Value, value: Value) -> Result<(), anyhow::Error> {
    host.note_containment(container, value);
    record_held_borrow(container, value);
    give_container_write_ownership(container, value)
}

/// A closure taking write-ownership of what it captured. A capture cannot be refused the way a
/// store can: the closure already exists by the time this is asked, and a retired value keeps its
/// retirement, which traps at its next write anyway.
pub fn closure_captured<H: Host + ?Sized>(host: &mut H, closure: Value, value: Value) {
    let _ = container_took(host, closure, value);
}

/// What a value records about who holds its write-ownership once the name holding the value is gone.
pub enum RecordedHolder {
    Container,
    Nobody,
}

pub fn held_by_aggregate(value: Value) -> bool {
    let owner = value.as_object().container_write_owner();
    owner.is_object()
        && matches!(owner.kind(), ValueKind::Object(ObjectKind::Array | ObjectKind::Dict | ObjectKind::Instance))
}

/// Who the value says writes it, if anyone.
pub fn recorded_holder(value: Value) -> RecordedHolder {
    match value.as_object().container_write_owner() {
        owner if owner.is_object() => RecordedHolder::Container,
        _ => RecordedHolder::Nobody,
    }
}

/// Moves `value`'s write-ownership to `container`. Refuses the store where there is no
/// write-ownership left to move.
fn give_container_write_ownership(container: Value, value: Value) -> Result<(), anyhow::Error> {
    if !is_mutable_container(value) || value == container {
        return Ok(());
    }
    // A retired value has no write-ownership to give.
    if value.as_object().is_write_retired() {
        bail!("{GAVE_TRANSFERRED_ELEMENT}");
    }
    value.as_object().set_container_write_owner(container);
    Ok(())
}

/// Whether `root` write-owns `value`, directly or through the containers between them. A false
/// says nothing about whether `root` contains it, only about who may write it.
pub fn write_ownership_reaches(value: Value, root: Value) -> bool {
    // A path that starts at its own target reached through nothing, so there is no container
    // between them to disagree with.
    if value == root {
        return true;
    }
    if !root.is_object() {
        return false;
    }
    // Two walkers at different speeds. A container reachable from its own element is a cycle a
    // single walker would follow forever.
    let (mut slow, mut fast) = (value, value);
    loop {
        for _ in 0..2 {
            if !fast.is_object() {
                return false;
            }
            fast = fast.as_object().container_write_owner();
            if fast == root {
                return true;
            }
        }
        slow = slow.as_object().container_write_owner();
        if slow == fast {
            return false;
        }
    }
}

/// Marks a value immutable, then its container children. `origin` is the code index of the freeze
/// site, recorded on each newly-frozen value so a later mutation can point back at it.
pub fn freeze_value(value: Value, origin: u32) {
    let ValueKind::Object(kind) = value.kind() else { return };
    let object = value.as_object();
    if object.is_immutable() {
        return;
    }
    object.set_immutable(origin);
    match kind {
        ObjectKind::Array => for &v in unsafe { &(*object.as_array_ptr()).values } { freeze_value(v, origin); },
        ObjectKind::Dict => for &v in unsafe { (*object.as_dict_ptr()).entries.values() } { freeze_value(v, origin); },
        ObjectKind::Instance => {
            let instance = unsafe { &*object.as_instance_ptr() };
            let ty = unsafe { &*instance.ty };
            for id in 0..ty.field_count { freeze_value(instance.get(id), origin); }
        },
        _ => {},
    }
}

/// A type or trait declaration's runtime identity.
pub type TypeId = u16;

/// Reached by the collector this cycle.
pub const FLAG_MARKED: u8 = 1 << 0;
/// Frozen, so a store through it traps.
pub const FLAG_IMMUTABLE: u8 = 1 << 1;
/// Lent as a borrow to an active call, so a store of it traps.
pub const FLAG_BORROWED: u8 = 1 << 2;
/// Set while one name holds the writer slot for this value, so a second writer traps.
pub const FLAG_WRITE_OWNED: u8 = 1 << 3;
/// Set where a value left the frame that built it. A return, a throw, a store through an upvalue,
/// and a capture all do that. Never cleared, so the bit only ever holds back a release a scope exit
/// would have made.
pub const FLAG_ESCAPED: u8 = 1 << 4;
/// Set where the value's write-ownership went to a `*mut` parameter that never handed it on, so
/// every write to it traps.
pub const FLAG_WRITE_RETIRED: u8 = 1 << 5;
/// Set where a borrowed value went into this aggregate.
pub const FLAG_HOLDS_BORROW: u8 = 1 << 6;

#[repr(C)]
pub struct ObjectHeader {
    pub kind: ObjectKind,
    /// The header's boolean state, packed so another flag costs no object memory. Every heap object
    /// carries this header, so a byte here is a byte per allocation.
    flags: u8,
    /// Code index of the site that made this value immutable, or `NO_ORIGIN` while it is mutable.
    pub immutable_origin: u32
}

impl ObjectHeader {
    pub fn new(kind: ObjectKind) -> ObjectHeader {
        ObjectHeader { kind, flags: 0, immutable_origin: NO_ORIGIN }
    }

    #[inline]
    pub fn has(&self, flag: u8) -> bool {
        self.flags & flag != 0
    }

    #[inline]
    pub fn set(&mut self, flag: u8, on: bool) {
        match on {
            true => self.flags |= flag,
            false => self.flags &= !flag,
        }
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
    pub fn is_immutable(&self) -> bool {
        unsafe { (*self.as_header_ptr()).has(FLAG_IMMUTABLE) }
    }

    #[inline]
    pub fn set_immutable(&self, origin: u32) {
        let header = unsafe { &mut *self.as_header_ptr() };
        header.set(FLAG_IMMUTABLE, true);
        header.immutable_origin = origin;
    }

    #[inline]
    pub fn set_mutable(&self) {
        let header = unsafe { &mut *self.as_header_ptr() };
        header.set(FLAG_IMMUTABLE, false);
        header.immutable_origin = NO_ORIGIN;
    }

    /// The code index of the site that made this value immutable, if one was recorded.
    #[inline]
    pub fn immutable_origin(&self) -> Option<u32> {
        match unsafe { (*self.as_header_ptr()).immutable_origin } {
            NO_ORIGIN => None,
            origin => Some(origin)
        }
    }

    #[inline]
    pub fn is_borrowed(&self) -> bool {
        unsafe { (*self.as_header_ptr()).has(FLAG_BORROWED) }
    }

    #[inline]
    pub fn set_borrowed(&self, value: bool) {
        unsafe { (*self.as_header_ptr()).set(FLAG_BORROWED, value); }
    }

    /// Whether the last trace reached this object. Only meaningful between a trace and its sweep.
    #[inline]
    pub fn is_marked(&self) -> bool {
        unsafe { (*self.as_header_ptr()).has(FLAG_MARKED) }
    }

    /// Whether this value left its frame by a route no container records.
    #[inline]
    pub fn is_escaped(&self) -> bool {
        unsafe { (*self.as_header_ptr()).has(FLAG_ESCAPED) }
    }

    #[inline]
    pub fn is_write_owned(&self) -> bool {
        unsafe { (*self.as_header_ptr()).has(FLAG_WRITE_OWNED) }
    }

    #[inline]
    pub fn set_write_owned(&self, value: bool) {
        unsafe { (*self.as_header_ptr()).set(FLAG_WRITE_OWNED, value); }
    }

    #[inline]
    pub fn is_write_retired(&self) -> bool {
        unsafe { (*self.as_header_ptr()).has(FLAG_WRITE_RETIRED) }
    }

    #[inline]
    pub fn set_write_retired(&self, value: bool) {
        unsafe { (*self.as_header_ptr()).set(FLAG_WRITE_RETIRED, value); }
    }

    /// Whether a borrow was put into this aggregate at some point in its life.
    #[inline]
    pub fn holds_borrow(&self) -> bool {
        unsafe { (*self.as_header_ptr()).has(FLAG_HOLDS_BORROW) }
    }

    /// The container this value was last stored into, or null.
    #[inline]
    pub fn container_write_owner(&self) -> Value {
        unsafe {
            match (*self.as_header_ptr()).kind {
                ObjectKind::Array => (*self.as_array_ptr()).container_write_owner,
                ObjectKind::Dict => (*self.as_dict_ptr()).container_write_owner,
                ObjectKind::Instance => (*self.as_instance_ptr()).container_write_owner,
                _ => Value::NULL,
            }
        }
    }

    #[inline]
    pub fn set_container_write_owner(&self, owner: Value) {
        unsafe {
            match (*self.as_header_ptr()).kind {
                ObjectKind::Array => (*self.as_array_ptr()).container_write_owner = owner,
                ObjectKind::Dict => (*self.as_dict_ptr()).container_write_owner = owner,
                ObjectKind::Instance => (*self.as_instance_ptr()).container_write_owner = owner,
                kind => unreachable!("{kind} takes no writer slot, so it has no owner to set"),
            }
        }
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
    pub name: *mut ObjString,
    pub arity: u8,
    /// Whether the method declared `this: mut`, so a call has to prove its receiver is mutable.
    pub mut_receiver: bool,
    pub ip_start: usize,
    pub upvalues: Vec<UpvalueLocation>,
    /// One bit per parameter that lets its argument out of the caller's sole reach: it retains it,
    /// hands it back, or passes it to a callee that would. Parameters past 63 are read as borrowing.
    pub escape_mask: u64,
    /// One bit per parameter that retains its argument (`*`), so the call can transfer each one's
    /// write-ownership without codegen naming the positions.
    pub retain_mask: u64
}

impl ObjFn {
    /// Whether the parameter at `position` lets its argument escape, as opposed to borrowing it.
    #[inline]
    pub fn escapes(&self, position: usize) -> bool {
        position < 64 && self.escape_mask & (1u64 << position) != 0
    }

    pub fn new(name: *mut ObjString, arity: u8, ip_start: usize, upvalues: Vec<UpvalueLocation>, escape_mask: u64, retain_mask: u64, mut_receiver: bool) -> ObjFn {
        ObjFn {
            header: ObjectHeader::new(ObjectKind::Function),
            name,
            arity,
            mut_receiver,
            ip_start,
            upvalues,
            escape_mask,
            retain_mask
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
    /// Whether the method writes its receiver.
    pub mutates: bool,
    pub function: NativeFn
}

impl ObjNativeFn {
    pub fn new(name: *mut ObjString, arity: u8, function: NativeFn) -> ObjNativeFn {
        ObjNativeFn::of(name, arity, false, function)
    }

    /// A native that writes its receiver. Named apart from `new` so a mutator cannot be declared
    /// without saying so.
    pub fn mutating(name: *mut ObjString, arity: u8, function: NativeFn) -> ObjNativeFn {
        ObjNativeFn::of(name, arity, true, function)
    }

    fn of(name: *mut ObjString, arity: u8, mutates: bool, function: NativeFn) -> ObjNativeFn {
        ObjNativeFn {
            header: ObjectHeader::new(ObjectKind::NativeFunction),
            name,
            arity,
            mutates,
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
    pub mut_receiver: bool,
    pub ip_start: usize,
    pub escape_mask: u64,
    pub retain_mask: u64
}

impl ObjClosure {
    /// Byte offset of the trailing upvalue array.
    const UPVALUES_OFFSET: usize = mem::size_of::<ObjClosure>();

    /// Whether the parameter at `position` lets its argument escape, as opposed to borrowing it.
    #[inline]
    pub fn escapes(&self, position: usize) -> bool {
        position < 64 && self.escape_mask & (1u64 << position) != 0
    }

    #[inline]
    pub fn alloc_size(count: usize) -> usize {
        Self::UPVALUES_OFFSET + count * mem::size_of::<*mut ObjUpvalue>()
    }

    /// The trailing upvalue array. Takes the allocation pointer rather than `&self`, because a
    /// reference to the struct reaches only its own bytes and the array sits past them.
    /// Safety: `closure` must be a pointer returned by [`Gc::alloc_closure`].
    #[inline]
    pub unsafe fn upvalues_ptr(closure: *const ObjClosure) -> *mut *mut ObjUpvalue {
        unsafe { (closure as *mut u8).add(Self::UPVALUES_OFFSET) as *mut *mut ObjUpvalue }
    }

    /// Reads the captured upvalue at `idx`.
    /// Safety: as `upvalues_ptr`, and `idx < upvalue_count`.
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
    /// Field and regular-method names. The accessors/factory are *not* here;
    /// they're addressed structurally via the id fields below.
    pub members: FnvHashMap<*mut ObjString, TypeMember>,
    pub methods: IntMap<MemberId, Object>,
    /// The declaration id of every trait/type this type provides: its own, plus every `with`-mixed trait.
    pub provided: IntSet<TypeId>,
    /// The obligation witnesses this type provides, by id, sorted. Almost always empty: a type
    /// provides one per witness trait it mixes, and most types mix none.
    pub witness_ids: Box<[u16]>,
    pub member_count: u8,
    pub getter_id: Option<MemberId>,
    pub setter_id: Option<MemberId>,
    /// `None` for a factory-less type, or a native type.
    pub factory_id: Option<MemberId>,
    /// Prebuilt initial instance values (method slots filled, fields `NULL`).
    pub template: Box<[Value]>,
    /// The `gives` delegations, `(field id, field name, trait name, trait id)`. A construction
    /// verifies each field provides that trait.
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
            member_count: 0,
            getter_id: None,
            setter_id: None,
            factory_id: None,
            template: Box::new([]),
            gives: Box::new([])
        }
    }

    /// Copies a type so its methods can be rebound for one execution. Every field but the header
    /// is copied as-is. The caller reinstalls the methods that capture.
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
            member_count: self.member_count,
            getter_id: self.getter_id,
            setter_id: self.setter_id,
            factory_id: self.factory_id,
            template: Box::new([]),
            gives: self.gives.clone(),
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
    /// The container that write-owns this instance, or null. Weak, so a collection clears it.
    /// A binding can write-own one too, but a frame slot is not reachable from the heap, so that
    /// half lives in the write-ownership table instead.
    pub container_write_owner: Value,
    pub ty: *mut ObjType,
    /// Member values indexed directly by member id.
    pub values: Box<[Value]>
}

impl ObjInstance {
    pub fn new(type_ptr: *mut ObjType) -> ObjInstance {
        let ty = unsafe { &*type_ptr };
        ObjInstance {
            header: ObjectHeader::new(ObjectKind::Instance),
            container_write_owner: Value::NULL,
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
    pub location: *mut Value,
    pub closed: Value
}

impl ObjUpvalue {
    pub fn is_closed(&self) -> bool {
        std::ptr::eq(self.location as *const Value, &raw const self.closed)
    }

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
    /// The container that write-owns this array, or null. Weak, so a collection clears it.
    /// A binding can write-own one too, but a frame slot is not reachable from the heap, so that
    /// half lives in the write-ownership table instead.
    pub container_write_owner: Value,
    pub values: Vec<Value>
}

impl ObjArray {
    pub fn new(values: Vec<Value>) -> ObjArray {
        ObjArray {
            header: ObjectHeader::new(ObjectKind::Array),
            container_write_owner: Value::NULL,
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

/// The loose tier: an open, value-keyed map. Keys are `Value`s compared by
/// identity/equality with no coercion (`5`, `"5"`, and `true` are distinct keys).
/// Data lives here, keyed by `[]`; methods live on the native `dict` surface.
#[repr(align(8))]
#[repr(C)]
pub struct ObjDict {
    pub header: ObjectHeader,
    /// The container that write-owns this dict, or null. Weak, so a collection clears it.
    /// A binding can write-own one too, but a frame slot is not reachable from the heap, so that
    /// half lives in the write-ownership table instead.
    pub container_write_owner: Value,
    pub entries: FnvHashMap<DictKey, Value>
}

impl ObjDict {
    pub fn new(entries: FnvHashMap<DictKey, Value>) -> ObjDict {
        ObjDict {
            header: ObjectHeader::new(ObjectKind::Dict),
            container_write_owner: Value::NULL,
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

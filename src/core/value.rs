use std::hash::{Hash, Hasher};
use super::equality;
use std::{fmt, mem};

use super::gc::{Gc, GcTraceable};
use super::objects::{self, Object, ObjectKind};

pub enum ValueKind {
    Null,
    Number,
    Boolean,
    Object(ObjectKind),
    Anchor,
    MemberKey,
}

impl fmt::Display for ValueKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            ValueKind::Null => format!("null"),
            ValueKind::Number => format!("number"),
            ValueKind::Boolean => format!("boolean"),
            ValueKind::Object(kind) => format!("{}", kind),
            ValueKind::Anchor => format!("anchor"),
            ValueKind::MemberKey => format!("member"),
        })
    }
}

/// NaN boxed value
#[derive(Clone, Copy, Eq, PartialEq, Hash)]
pub struct Value(u64);

/// Which frame owned a slot when an anchor named it.
#[cfg(debug_assertions)]
pub type FrameGeneration = u16;
#[cfg(not(debug_assertions))]
pub type FrameGeneration = ();

impl Value {
     /// 0x7FF8000000000000 is the QNaN representation of a 64-bit float.
     /// A value represents a number if its QNaN bits are not set.
     /// 
     /// We also reserve an additional bit to differentiate between NaNs and
     /// other value types in Clisay, like booleans and objects.
     /// 
     /// If these bits are set, the value does not represent an f64.
    const NAN_MASK: u64 = 0x7FFC000000000000;
    const SIGN: u64 = 1 << 63; // 0x8000000000000000;

    /// The sign and QNaN bits are set for object values. 
    /// This takes 14 bits, leaving room for a 50-bit pointer.
    /// 
    /// Technically 64-bit architectures have 64-bit pointers, but in practice
    /// common architectures only use the first 48 bits.
    const OBJECT_MASK: u64 = Self::SIGN | Self::NAN_MASK;
    const PTR_MASK: u64 = 0x0000FFFFFFFFFFFF;
    
    const CALLABLE_MASK: u64 = Self::OBJECT_MASK | (1 << 48); // 0x4000000000000000

    const SLOT_ANCHOR_TAG: u64 = 0b100;
    const PATH_ANCHOR_TAG: u64 = 0b101;
    const ANCHOR_WITNESS_SET_SHIFT: u64 = 32;

    #[cfg(debug_assertions)]
    const ANCHOR_GENERATION_SHIFT: u64 = 3;
    #[cfg(debug_assertions)]
    const ANCHOR_GENERATION_MASK: u64 = (1 << 5) - 1;

    #[cfg(debug_assertions)]
    pub fn wrap_frame_generation(counter: u16) -> FrameGeneration {
        counter & Self::ANCHOR_GENERATION_MASK as u16
    }

    const MEMBER_KEY_TAG: u64 = 0b110;
    const ANCHOR_DOT_BIT: u64 = 1 << 32;
    const ANCHOR_FIELD_SHIFT: u64 = 40;

    pub const NULL: Self = Self(Self::NAN_MASK | 0b01);
    /// Poisons a slot whose binding is not assigned yet in debug builds. It's null's
    /// tag with a payload, which costs no tag, since null is matched by its exact bits.
    const UNASSIGNED: Self = Self(Self::NULL.0 | (1 << 8));
    pub const TRUE: Self = Self(Self::NAN_MASK | 0b10);
    pub const FALSE: Self = Self(Self::NAN_MASK | 0b11);

    #[cfg(debug_assertions)]
    pub const fn unassigned() -> Self { Self::UNASSIGNED }
    #[cfg(not(debug_assertions))]
    pub const fn unassigned() -> Self { Self::NULL }

    pub fn kind(self) -> ValueKind {
        if self.is_null() {
            ValueKind::Null
        } else if self.is_bool() {
            ValueKind::Boolean
        } else if self.is_number() {
            ValueKind::Number
        } else if self.is_object() {
            ValueKind::Object(unsafe { (*self.as_object().as_header_ptr()).kind })
        } else if self.is_member_key() {
            ValueKind::MemberKey
        } else {
            ValueKind::Anchor
        }
    }

    pub fn is_unassigned(self) -> bool {
        self == Self::UNASSIGNED
    }

    pub fn is_number(self) -> bool {
        (self.0 & Self::NAN_MASK) != Self::NAN_MASK
    }

    /// Language-level equality, what `==` answers.
    #[inline]
    pub fn value_eq(self, other: Self, depth_limit: usize) -> bool {
        equality::shallow_eq(self, other).unwrap_or_else(|| equality::deep_eq(self, other, depth_limit))
    }

    #[inline]
    pub fn from_bits(bits: u64) -> Value {
        Value(bits)
    }

    pub fn to_bits(self) -> u64 {
        self.0
    }

    pub fn is_bool(self) -> bool {
        Self(self.0 | 0b01) == Self::FALSE
    }

    pub fn is_null(self) -> bool {
        self == Self::NULL
    }

    pub fn is_falsy(self) -> bool {
        self.is_null() || self == Self::FALSE
    }

    pub fn is_callable(self) -> bool {
        (self.0 & Self::CALLABLE_MASK) == Self::CALLABLE_MASK
    }

    #[inline]
    pub fn slot_anchor(index: usize, witness_set_pool_id: u16, generation: FrameGeneration) -> Value {
        Value(Self::NAN_MASK
            | ((witness_set_pool_id as u64) << Self::ANCHOR_WITNESS_SET_SHIFT)
            | Self::anchor_index_bits(index, generation)
            | Self::SLOT_ANCHOR_TAG)
    }

    #[inline]
    fn anchor_index_bits(index: usize, generation: FrameGeneration) -> u64 {
        ((index as u64) << 8) | Self::anchor_generation_bits(generation)
    }

    #[cfg(debug_assertions)]
    #[inline]
    fn anchor_generation_bits(generation: FrameGeneration) -> u64 {
        ((generation as u64) & Self::ANCHOR_GENERATION_MASK) << Self::ANCHOR_GENERATION_SHIFT
    }

    #[cfg(not(debug_assertions))]
    #[inline]
    fn anchor_generation_bits(_generation: FrameGeneration) -> u64 { 0 }

    #[cfg(debug_assertions)]
    pub fn anchor_generation(self) -> FrameGeneration {
        ((self.0 >> Self::ANCHOR_GENERATION_SHIFT) & Self::ANCHOR_GENERATION_MASK) as u16
    }

    pub fn anchor_witness_set_pool_id(self) -> u16 {
        (self.0 >> Self::ANCHOR_WITNESS_SET_SHIFT) as u16
    }

    pub const ANCHOR_KEY_OFFSET: usize = 1;

    /// The frame slot holding a path anchor's key for `step`, given the anchor's first slot.
    pub const fn anchor_key_slot(first: usize, steps: usize, step: usize) -> usize {
        first + Self::ANCHOR_KEY_OFFSET + steps - 1 - step
    }

    #[inline]
    pub fn anchor_path(index: usize, is_dot: bool, generation: FrameGeneration) -> Value {
        Value(Self::NAN_MASK
            | ((is_dot as u64) << 32)
            | Self::anchor_index_bits(index, generation)
            | Self::PATH_ANCHOR_TAG)
    }

    /// A path anchor that also carries the id of the field it names.
    pub fn with_anchor_field(self, id: u8) -> Value {
        debug_assert!(id < u8::MAX, "a field id should fit");
        Value(self.0 | ((id as u64 + 1) << Self::ANCHOR_FIELD_SHIFT))
    }

    pub fn anchor_field(self) -> Option<u8> {
        match (self.0 >> Self::ANCHOR_FIELD_SHIFT) as u8 {
            0 => None,
            id => Some(id - 1),
        }
    }

    pub fn anchor_is_dot(self) -> bool {
        self.0 & Self::ANCHOR_DOT_BIT != 0
    }

    pub fn is_anchor(self) -> bool {
        (self.0 & (Self::OBJECT_MASK | 0b110)) == (Self::NAN_MASK | Self::SLOT_ANCHOR_TAG)
    }

    pub fn is_anchor_path(self) -> bool {
        (self.0 & (Self::OBJECT_MASK | 0b111)) == (Self::NAN_MASK | Self::PATH_ANCHOR_TAG)
    }

    #[inline]
    pub fn anchor_index(self) -> usize {
        ((self.0 >> 8) & 0xFFFFFF) as usize
    }

    pub fn member_key(id: u8) -> Value {
        Value(Self::NAN_MASK | ((id as u64) << 8) | Self::MEMBER_KEY_TAG)
    }

    pub fn is_member_key(self) -> bool {
        (self.0 & (Self::OBJECT_MASK | 0b111)) == (Self::NAN_MASK | Self::MEMBER_KEY_TAG)
    }

    pub fn member_key_id(self) -> u8 {
        (self.0 >> 8) as u8
    }

    pub fn is_object(self) -> bool {
        (self.0 & Self::OBJECT_MASK) == Self::OBJECT_MASK
    }

    pub fn as_number(self) -> f64 {
        f64::from_bits(self.0)
    }

    pub fn as_bool(self) -> bool {
        self == Self::TRUE
    }

    pub fn as_object(self) -> Object {
        Object { header: (self.0 & Self::PTR_MASK) as *mut objects::ObjectHeader }
    }
}

impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Self(value.to_bits())
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        if value { Self::TRUE } else { Self::FALSE }
    }
}

impl<T: Into<Object>> From<T> for Value {
    fn from(object: T) -> Self {
        let object: Object = object.into();
        let tag = object.tag();
        let mask = if tag >= objects::TAG_CLOSURE { Self::CALLABLE_MASK } else { Self::OBJECT_MASK };
        let ptr = unsafe { object.header };
        Self((ptr as u64) | mask | (tag as u64))
    }
}

impl GcTraceable for Value {
    fn fmt(&self) -> String {
        match self.kind() {
            ValueKind::Null => format!("null"),
            ValueKind::Number => format!("{}", self.as_number()),
            ValueKind::Boolean => format!("{}", self.as_bool()),
            ValueKind::Object(_) => self.as_object().fmt(),
            ValueKind::Anchor => String::from("anchor"),
            ValueKind::MemberKey => format!("member {}", self.member_key_id()),
        }
    }

    fn mark(&self, gc: &mut Gc) {
        if self.is_object() {
            gc.mark_object(self.as_object());
        }
    }

    fn size(&self) -> usize {
        mem::size_of::<Value>()
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.kind())
    }
}

/// A dict key. Equality and hashing follow `Value::value_eq`, so a lookup answers the way `==` does.
#[derive(Clone, Copy)]
pub struct DictKey(pub Value);

impl PartialEq for DictKey {
    /// The bit test comes first so a key always equals itself. `value_eq` says `NaN != NaN`, which
    /// would leave a stored key unreachable by the very value that stored it.
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0 || self.0.value_eq(other.0, usize::MAX)
    }
}

impl Eq for DictKey {}

impl Hash for DictKey {
    fn hash<H: Hasher>(&self, state: &mut H) {
        equality::deep_hash(self.0).hash(state);
    }
}

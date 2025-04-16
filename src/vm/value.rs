use std::{fmt, mem};

use super::gc::{Gc, GcTraceable};
use super::objects::{self, Object, ObjectKind};

pub enum ValueKind {
    Null,
    Number,
    Boolean,
    Object(ObjectKind)
}

impl fmt::Display for ValueKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            ValueKind::Null => format!("null"),
            ValueKind::Number => format!("number"),
            ValueKind::Boolean => format!("boolean"),
            ValueKind::Object(kind) => format!("{}", kind)
        })
    }
}

/** NaN boxed value */
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Value(u64);

impl Value {
    /**
     * 0x7FF8000000000000 is the QNaN representation of a 64-bit float.
     * A value represents a number if its QNaN bits are not set.
     * 
     * We also reserve an additional bit to differentiate between NaNs and
     * other value types in Clisay, like booleans and objects.
     * 
     * If these bits are set, the value does not represent an f64.
     */
    const NAN_MASK: u64 = 0x7FFC000000000000;
    const SIGN: u64 = 1 << 63; // 0x8000000000000000;

    /**
     * The sign and QNaN bits are set for object values. 
     * This takes 14 bits, leaving room for a 50-bit pointer.
     * 
     * Technically 64-bit architectures have 64-bit pointers, but in practice
     * common architectures only use the first 48 bits.
     */
    const OBJECT_MASK: u64 = Self::SIGN | Self::NAN_MASK;
    const PTR_MASK: u64 = 0x0000FFFFFFFFFFFF;
    
    const CALLABLE_MASK: u64 = Self::OBJECT_MASK | (1 << 48); // 0x4000000000000000

    pub const NULL: Self = Self(Self::NAN_MASK | 0b01);
    pub const TRUE: Self = Self(Self::NAN_MASK | 0b10);
    pub const FALSE: Self = Self(Self::NAN_MASK | 0b11);

    pub fn kind(self) -> ValueKind {
        if self.is_null() {
            ValueKind::Null
        } else if self.is_bool() {
            ValueKind::Boolean
        } else if self.is_number() {
            ValueKind::Number
        } else if self.is_object() {
            ValueKind::Object(unsafe { (*self.as_object().as_header_ptr()).kind })
        } else {
            unreachable!("Invalid value type")
        }
    }

    pub fn is_number(self) -> bool {
        (self.0 & Self::NAN_MASK) != Self::NAN_MASK
    }

    pub fn is_bool(self) -> bool {
        Self(self.0 | 0b01) == Self::FALSE
    }

    pub fn is_null(self) -> bool {
        self == Self::NULL
    }

    pub fn is_callable(self) -> bool {
        (self.0 & Self::CALLABLE_MASK) == Self::CALLABLE_MASK
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
            ValueKind::Object(_) => self.as_object().fmt()
        }
    }

    fn mark(&self, gc: &mut Gc) {
        if self.is_object() {
            self.as_object().mark(gc);
        }
    }

    fn size(&self) -> usize {
        mem::size_of::<Value>()
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self.kind() {
            ValueKind::Null => format!("null"),
            ValueKind::Number => format!("number"),
            ValueKind::Boolean => format!("boolean"),
            ValueKind::Object(ObjectKind::Function) => format!("function"),
            ValueKind::Object(ObjectKind::BoundMethod) => format!("function"),
            ValueKind::Object(ObjectKind::NativeFunction) => format!("function"),
            ValueKind::Object(ObjectKind::Closure) => format!("function"),
            ValueKind::Object(ObjectKind::Class) => format!("class"),
            ValueKind::Object(ObjectKind::Instance) => format!("instance"),
            ValueKind::Object(ObjectKind::String) => format!("string"),
            ValueKind::Object(ObjectKind::Upvalue) => format!("upvalue"),
            ValueKind::Object(ObjectKind::Array) => format!("array"),
            ValueKind::Object(ObjectKind::Result) => format!("result")
        })
    }
}
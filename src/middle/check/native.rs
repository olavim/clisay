//! Fixed nullability signatures for built-in globals and native-type methods.

use crate::middle::hir::ReturnShape;

/// A native's per-parameter nullability and return shape.
pub struct NativeSig {
    /// Per-parameter nullability. A `false` slot rejects a null or nullable argument.
    pub params: &'static [bool],
    pub ret: ReturnShape,
}

/// The signature of a built-in global, or `None` when the name is not a built-in.
pub fn builtin(name: &str) -> Option<NativeSig> {
    let sig = match name {
        "print" => NativeSig { params: &[true], ret: ReturnShape::Void },
        "time" => NativeSig { params: &[], ret: ReturnShape::NonNull },
        "gcHeapSize" => NativeSig { params: &[], ret: ReturnShape::NonNull },
        "gcCollect" => NativeSig { params: &[], ret: ReturnShape::Void },
        "gcStress" => NativeSig { params: &[false], ret: ReturnShape::Void },
        _ => return None,
    };
    Some(sig)
}

/// The signature of a native-type method, or `None` when the name is not a native method.
pub fn native_method(name: &str) -> Option<NativeSig> {
    let sig = match name {
        "length" => NativeSig { params: &[], ret: ReturnShape::NonNull },
        "size" => NativeSig { params: &[], ret: ReturnShape::NonNull },
        "has" => NativeSig { params: &[true], ret: ReturnShape::NonNull },
        "remove" => NativeSig { params: &[true], ret: ReturnShape::Nullable },
        _ => return None,
    };
    Some(sig)
}

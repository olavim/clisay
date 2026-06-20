//! Fixed nullability signatures for built-in globals and native-type methods.

use crate::middle::hir::ReturnShape;

/// A native's per-parameter nullability and return shape.
#[allow(dead_code)]
pub struct NativeSig {
    pub params: &'static [bool],
    pub ret: ReturnShape,
}

/// The signature of a built-in global, or `None` when the name is not a built-in.
#[allow(dead_code)]
pub fn builtin(name: &str) -> Option<NativeSig> {
    let sig = match name {
        "print" => NativeSig { params: &[true], ret: ReturnShape::Void },
        "time" => NativeSig { params: &[], ret: ReturnShape::NonNull },
        "gcHeapSize" => NativeSig { params: &[], ret: ReturnShape::NonNull },
        "gcCollect" => NativeSig { params: &[], ret: ReturnShape::Void },
        "gcStress" => NativeSig { params: &[true], ret: ReturnShape::Void },
        _ => return None,
    };
    Some(sig)
}

#[allow(dead_code)]
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

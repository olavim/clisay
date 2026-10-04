//! Fixed signatures for built-in globals and native-type methods.

use crate::core::native::NativeType;

#[derive(Clone, Copy)]
pub struct ObSet {
    pub opt: bool,
    pub fails: bool,
}

impl ObSet {
    const CLEAN: ObSet = ObSet { opt: false, fails: false };
    const ANY: ObSet = ObSet { opt: true, fails: true };
    const OPT: ObSet = ObSet { opt: true, fails: false };

    /// Whether the set admits any value.
    pub fn admits_any(self) -> bool {
        // For now admitting both opt and fails means "any".
        self.opt && self.fails
    }
}

#[derive(Clone, Copy)]
pub struct RetSig {
    pub set: ObSet,
    pub void: bool,
}

impl RetSig {
    const CLEAN: RetSig = RetSig { set: ObSet::CLEAN, void: false };
    const VOID: RetSig = RetSig { set: ObSet::CLEAN, void: true };
    const OPT: RetSig = RetSig { set: ObSet::OPT, void: false };
}

/// Whether a method stores its argument.
#[derive(Clone, Copy, PartialEq)]
pub enum Container {
    None,
    Preserves,
}

pub struct NativeSig {
    pub params: &'static [ObSet],
    pub ret: RetSig,
    pub container: Container,
    pub wants_anchor_receiver: bool,
}

impl NativeSig {
    const fn new(params: &'static [ObSet], ret: RetSig) -> NativeSig {
        NativeSig {
            params,
            ret,
            container: Container::None,
            wants_anchor_receiver: false,
        }
    }
}

pub fn builtin(name: &str) -> Option<NativeSig> {
    let sig = match name {
        "print" => NativeSig::new(&[ObSet::ANY], RetSig::VOID),
        "time" => NativeSig::new(&[], RetSig::CLEAN),
        "gcHeapSize" => NativeSig::new(&[], RetSig::CLEAN),
        "gcCollect" => NativeSig::new(&[], RetSig::VOID),
        "gcStress" => NativeSig::new(&[ObSet::CLEAN], RetSig::VOID),
        _ => return None,
    };
    Some(sig)
}

pub fn native_method(ty: NativeType, name: &str) -> Option<NativeSig> {
    let sig = match (ty, name) {
        (NativeType::Array, "length") => NativeSig::new(&[], RetSig::CLEAN),
        (NativeType::Array, "push") => NativeSig { container: Container::Preserves, ..NativeSig::new(&[ObSet::ANY], RetSig::VOID) },
        (NativeType::Dict, "size") => NativeSig::new(&[], RetSig::CLEAN),
        (NativeType::Dict, "containsKey") => NativeSig::new(&[ObSet::ANY], RetSig::CLEAN),
        (NativeType::Dict, "remove") => NativeSig::new(&[ObSet::ANY], RetSig::OPT),
        _ => return None,
    };
    Some(NativeSig { wants_anchor_receiver: ty.method_wants_anchor_receiver(name), ..sig })
}

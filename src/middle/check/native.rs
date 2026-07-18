//! Fixed obligation signatures for built-in globals and native-type methods.

/// The obligation set a native slot ranges over. Builtins touch only `opt` and `fails`.
#[derive(Clone, Copy)]
pub struct ObSet {
    pub opt: bool,
    pub fails: bool,
}

impl ObSet {
    const CLEAN: ObSet = ObSet { opt: false, fails: false };
    const ANY: ObSet = ObSet { opt: true, fails: true };
    const OPT: ObSet = ObSet { opt: true, fails: false };
}

/// A native return: the obligations its result carries and whether it yields no value.
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

/// Whether a method stores its argument, so a pending argument's obligation flows onto the
/// receiver container.
#[derive(Clone, Copy, PartialEq)]
pub enum Container {
    None,
    Preserves,
}

/// A native's per-parameter accepted obligation set, its return, and container preservation.
pub struct NativeSig {
    pub params: &'static [ObSet],
    pub ret: RetSig,
    pub container: Container,
}

impl NativeSig {
    const fn new(params: &'static [ObSet], ret: RetSig) -> NativeSig {
        NativeSig { params, ret, container: Container::None }
    }
}

/// The signature of a built-in global, or `None` when the name is not a built-in.
pub fn builtin(name: &str) -> Option<NativeSig> {
    let sig = match name {
        // `print` stringifies any value, so it accepts every obligation.
        "print" => NativeSig::new(&[ObSet::ANY], RetSig::VOID),
        "time" => NativeSig::new(&[], RetSig::CLEAN),
        "gcHeapSize" => NativeSig::new(&[], RetSig::CLEAN),
        "gcCollect" => NativeSig::new(&[], RetSig::VOID),
        "gcStress" => NativeSig::new(&[ObSet::CLEAN], RetSig::VOID),
        "freeze" => NativeSig::new(&[ObSet::ANY], RetSig::CLEAN),
        _ => return None,
    };
    Some(sig)
}

/// The signature of a native-type method, or `None` when the name is not a native method.
pub fn native_method(name: &str) -> Option<NativeSig> {
    let sig = match name {
        "length" => NativeSig::new(&[], RetSig::CLEAN),
        "size" => NativeSig::new(&[], RetSig::CLEAN),
        "has" => NativeSig::new(&[ObSet::ANY], RetSig::CLEAN),
        "remove" => NativeSig::new(&[ObSet::ANY], RetSig::OPT),
        // `push` stores its argument, so a pending element flows onto the array.
        "push" => NativeSig { params: &[ObSet::ANY], ret: RetSig::VOID, container: Container::Preserves },
        _ => return None,
    };
    Some(sig)
}

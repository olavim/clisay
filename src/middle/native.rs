//! Fixed signatures for built-in globals and native-type methods.

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
    pub mutates_receiver: bool,
}

impl NativeSig {
    const fn new(params: &'static [ObSet], ret: RetSig) -> NativeSig {
        NativeSig {
            params,
            ret,
            container: Container::None,
            mutates_receiver: false,
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
        "freeze" => NativeSig::new(&[ObSet::ANY], RetSig::CLEAN),
        _ => return None,
    };
    Some(sig)
}

pub fn native_method(name: &str) -> Option<NativeSig> {
    let sig = match name {
        "length" => NativeSig::new(&[], RetSig::CLEAN),
        "size" => NativeSig::new(&[], RetSig::CLEAN),
        "has" => NativeSig::new(&[ObSet::ANY], RetSig::CLEAN),
        "remove" => NativeSig { mutates_receiver: true, ..NativeSig::new(&[ObSet::ANY], RetSig::OPT) },
        "push" => NativeSig { container: Container::Preserves, mutates_receiver: true, ..NativeSig::new(&[ObSet::ANY], RetSig::VOID) },
        _ => return None,
    };
    Some(sig)
}

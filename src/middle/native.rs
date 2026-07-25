//! Fixed signatures for built-in globals and native-type methods. This is the compile-time view of
//! the native types whose runtime definitions live in `src/core/native/`. The check pass reads the
//! obligation and return facts; the escape pass reads the effect facts.

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

/// What a native call does to the values it touches, for the escape and capture analysis. A read-only
/// call has neither flag set, so a borrowed value handed to it is not written.
#[derive(Clone, Copy, Default)]
pub struct Effect {
    /// Mutates or extends the receiver in place.
    pub mutates_receiver: bool,
    /// Persists or mutates an argument, so a value passed in is written.
    pub writes_args: bool,
}

/// A native's per-parameter accepted obligation set, its return, container preservation, and effect.
pub struct NativeSig {
    pub params: &'static [ObSet],
    pub ret: RetSig,
    pub container: Container,
    pub effect: Effect,
}

impl NativeSig {
    const fn new(params: &'static [ObSet], ret: RetSig) -> NativeSig {
        NativeSig { params, ret, container: Container::None, effect: Effect { mutates_receiver: false, writes_args: false } }
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
        // `freeze` downgrades its argument to immutable, which writes the value's capability.
        "freeze" => NativeSig { effect: Effect { mutates_receiver: false, writes_args: true }, ..NativeSig::new(&[ObSet::ANY], RetSig::CLEAN) },
        _ => return None,
    };
    Some(sig)
}

/// The signature of a native-type method, or `None` when the name is not a native method. Array and
/// dict methods share one table because native container types are not tracked in the type system,
/// so a call site cannot tell them apart. Splitting this per type needs native type tags first.
pub fn native_method(name: &str) -> Option<NativeSig> {
    let sig = match name {
        "length" => NativeSig::new(&[], RetSig::CLEAN),
        "size" => NativeSig::new(&[], RetSig::CLEAN),
        "has" => NativeSig::new(&[ObSet::ANY], RetSig::CLEAN),
        "remove" => NativeSig { effect: Effect { mutates_receiver: true, writes_args: false }, ..NativeSig::new(&[ObSet::ANY], RetSig::OPT) },
        // `push` stores its argument, so a pending element flows onto the array.
        "push" => NativeSig { container: Container::Preserves, effect: Effect { mutates_receiver: true, writes_args: true }, ..NativeSig::new(&[ObSet::ANY], RetSig::VOID) },
        _ => return None,
    };
    Some(sig)
}

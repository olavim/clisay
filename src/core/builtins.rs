//! The canonical list of built-in global names the VM predefines (`Vm::execute` registers each
//! via `define_native`). It is the single source of truth shared between the runtime — which
//! installs them — and name binding (`middle::bind`), which treats a reference to any *other*
//! undeclared name as a compile-time "undefined variable" error. The runtime asserts (in debug)
//! that the set it registers matches this list exactly, so the two cannot silently drift.
pub const NAMES: &[&str] = &["print", "time", "gcHeapSize", "gcCollect", "gcStress"];

/// Whether `name` is a predefined built-in global.
pub fn is_builtin(name: &str) -> bool {
    NAMES.contains(&name)
}

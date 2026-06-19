//! The canonical list of built-in global names the VM predefines. The runtime asserts (in debug)
//! that the set it registers matches this list exactly, so the two cannot silently drift.
pub const NAMES: &[&str] = &["print", "time", "gcHeapSize", "gcCollect", "gcStress"];

/// Whether `name` is a predefined built-in global.
pub fn is_builtin(name: &str) -> bool {
    NAMES.contains(&name)
}

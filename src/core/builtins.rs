//! The canonical list of built-in global names the VM predefines.
pub const NAMES: &[&str] = &["print", "time", "gcHeapSize", "gcCollect", "gcStress", "freeze", "Err"];

pub fn is_builtin(name: &str) -> bool {
    NAMES.contains(&name)
}

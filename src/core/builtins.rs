pub const NAMES: &[&str] = &["print", "time", "gcHeapSize", "gcCollect", "gcStress", "freeze", "Err"];

pub fn is_builtin(name: &str) -> bool {
    NAMES.contains(&name)
}

pub const NAMES: &[&str] = &["print", "time", "gcHeapSize", "gcCollect", "gcStress", "Err", "Ref"];

pub fn is_builtin(name: &str) -> bool {
    NAMES.contains(&name)
}

use super::gc::Gc;
use super::value::Value;

/// The capabilities the host interpreter provides to native functions.
pub trait Host {
    /// Push a value onto the stack.
    fn push(&mut self, value: Value);
    fn gc(&mut self) -> &mut Gc;
    fn share(&mut self, value: Value) -> Value;
    fn share_into(&mut self, container: Value, value: Value) -> Result<Value, anyhow::Error>;
    /// Run a garbage collection.
    fn collect(&mut self);
    fn print(&mut self, text: String);
}

pub struct Thrown(pub Value);

impl std::fmt::Debug for Thrown {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Thrown")
    }
}

impl std::fmt::Display for Thrown {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("a thrown value")
    }
}

impl std::error::Error for Thrown {}

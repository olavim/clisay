use super::gc::Gc;
use super::value::Value;

/// The capabilities the host interpreter provides to native functions.
pub trait Host {
    /// Push a value onto the stack.
    fn push(&mut self, value: Value);
    fn gc(&mut self) -> &mut Gc;
    /// Run a garbage collection.
    fn collect(&mut self);
    fn print(&mut self, text: String);
    fn code_index(&self) -> u32;
}

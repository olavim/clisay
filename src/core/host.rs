use super::gc::Gc;
use super::value::Value;

/// The capabilities the host interpreter provides to native functions.
pub trait Host {
    fn push(&mut self, value: Value);
    fn gc(&mut self) -> &mut Gc;
    /// Run a garbage collection.
    fn collect(&mut self);
    fn print(&mut self, text: String);
    /// The code index of the call site.
    fn code_index(&self) -> u32;
}

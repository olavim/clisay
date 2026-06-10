use super::gc::Gc;
use super::value::Value;

/// The capabilities the host interpreter provides to native functions.
pub trait Host {
    /// Push a value onto the operand stack (e.g. a native function's result).
    fn push(&mut self, value: Value);
    /// Access the collector (e.g. to read heap size or toggle stress mode).
    fn gc(&mut self) -> &mut Gc;
    /// Run a garbage collection.
    fn collect(&mut self);
    /// Emit a line of program output (routed through the host's capture).
    fn print(&mut self, text: String);
}

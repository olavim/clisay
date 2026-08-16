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
    /// Whether the running native's receiver is a slot the calling frame declared, so a value stored
    /// into it is reachable only through that frame. Answers false where nothing established it.
    fn receiver_is_frame_local(&self) -> bool;
    /// Tells the host a container took a value.
    fn note_containment(&mut self, container: Value, value: Value);
}

//! The shared value/heap substrate: the `Value` representation, heap `objects`,
//! the garbage collector, the VM value `stack`, and built-in `native` types.
//! Depended on by the compile pipeline (which compiles directly into live GC
//! objects) and by the `runtime` interpreter alike.

pub(crate) mod native;

pub mod value;
pub mod objects;
pub mod gc;
pub mod stack;
pub mod host;

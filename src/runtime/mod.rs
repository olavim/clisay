use std::collections::HashMap;
use std::hash::BuildHasherDefault;

use anyhow::bail;
use rustc_hash::FxHasher;
use smallvec::SmallVec;

use crate::Output;
use crate::core::objects::{ObjBoundMethod, ObjInstance};
use fnv::FnvHashSet;
use crate::core::value::ValueKind;
use crate::frontend::lex::{Diagnostic, SourcePosition};

use crate::core::native::array::NativeArray;
use crate::core::native::dict::NativeDict;
use crate::core::native::NativeType;
use crate::core::stack::{CachedStack, Stack};
use crate::core::value::{DictKey, Value};
use crate::core::gc::{Gc, GcTraceable};
use crate::core::host::Host;
use crate::core::objects::{self, BuiltinLayout, TypeMember, NativeFn, ObjArray, ObjDict, ObjType, ObjClosure, ObjFn, ObjNativeFn, ObjString, ObjUpvalue, Object, ObjectKind, TypeId};
use crate::ast::BuiltinType;

use crate::backend::bytecode::chunk::BytecodeChunk;
use crate::backend::bytecode::opcode::{self, OpCode};
use crate::middle::ir;

const MAX_STACK: usize = 16384;
const MAX_FRAMES: usize = 256;
const INDEX_CACHE_SIZE: usize = 2048;
const CALL_CACHE_SIZE: usize = 1024;

#[derive(Clone, Copy)]
struct IndexCache {
    /// The bytecode site, or `EMPTY_SITE` for an entry that answers nothing. A live site is an
    /// instruction pointer.
    site: usize,
    /// The member asked for.
    prop: *mut ObjString,
    /// The declaration, not the object.
    ty: TypeId,
    member: TypeMember
}

/// On a hit (same site, same callee value) the CALL path skips the callable/tag/arity
/// checks and jumps straight to the cached entry.
#[derive(Clone, Copy)]
struct CallCache {
    site: usize,
    callee: Value,
    closure: *mut ObjClosure,
    ip_start: usize,
    retain_mask: u64,
    needs_borrow_mark: u64
}

/// A site no instruction pointer can be, which is how an entry says it answers nothing.
const EMPTY_SITE: usize = usize::MAX;

impl IndexCache {
    const fn empty() -> IndexCache {
        IndexCache { site: EMPTY_SITE, prop: std::ptr::null_mut(), ty: TypeId::MAX, member: TypeMember::Field(0) }
    }
}

impl CallCache {
    /// An entry naming nothing. A collection resets to this, since what it named may be freed.
    const fn empty() -> CallCache {
        CallCache { site: EMPTY_SITE, callee: Value::NULL, closure: std::ptr::null_mut(), ip_start: 0, retain_mask: 0, needs_borrow_mark: u64::MAX }
    }
}

struct NativeTypes {
    array: *mut ObjType,
    dict: *mut ObjType,
    err: *mut ObjType
}

impl GcTraceable for NativeTypes {
    fn mark(&self, gc: &mut Gc) {
        gc.mark_object(self.array);
        gc.mark_object(self.dict);
        gc.mark_object(self.err);
    }
    
    fn fmt(&self) -> String {
        unimplemented!()
    }
    
    fn size(&self) -> usize {
        unimplemented!()
    }
}

#[derive(Clone, Copy)]
pub struct CallFrame {
    closure: *mut ObjClosure,
    return_ip: *const OpCode,
    stack_start: *mut Value,
    /// Whether a factory returning from this frame should seal (deep-freeze) its instance.
    seal: bool,
}

#[derive(Clone, Copy, PartialEq)]
pub enum TryKind {
    Catch,
    Defer,
}

#[derive(Clone, Copy)]
pub struct TryFrame {
    kind: TryKind,
    origin: *mut CallFrame,
    handler_ip: *const OpCode,
    stack_start: *mut Value,
}

pub struct Vm {
    /// Forced checks this run reached, for the coverage report.
    elisions_reached: FnvHashSet<usize>,
    pub(crate) gc: Gc,
    ip: *const OpCode,
    chunk: BytecodeChunk,
    globals: HashMap<*mut ObjString, Value, BuildHasherDefault<FxHasher>>,
    pub(crate) stack: Stack<Value, MAX_STACK>,
    frames: CachedStack<CallFrame, MAX_FRAMES>,
    try_frames: Vec<TryFrame>,
    tail_breadcrumbs: Vec<(*mut CallFrame, *mut ObjClosure)>,
    open_upvalues: Vec<*mut ObjUpvalue>,
    native_types: NativeTypes,
    index_cache: Box<[IndexCache]>,
    call_cache: Box<[CallCache]>,
    out: Vec<String>,
    /// Whether the receiver of the native about to run is a slot the calling frame declared.
    native_receiver_is_frame_local: bool,
    /// One bit per argument of the running native, set where its slot was borrowed at the call.
    native_borrowed_arguments: u64
}

macro_rules! as_short {
    ($l:expr, $r:expr) => { ($l as u16) | (($r as u16) << 8) }
}

mod accepts;
mod writes;
mod calls;
mod closures;
mod properties;
mod ops;
mod threaded;

#[cfg(any(debug_assertions, feature = "capture_output"))]
fn disassemble(chunk: &BytecodeChunk) {
    Output::println("=== Bytecode ===");
    Output::println(chunk.fmt());
    Output::println("================");
}

fn build_native_type(gc: &mut Gc, native_type: impl NativeType) -> *mut ObjType {
    let ty = native_type.build_type(gc);
    gc.alloc(ty)
}

/// Numbers the witnesses a type the VM built itself provides.
fn apply_witness_ids(ty: &mut ObjType, ids: &[(TypeId, u16)], provided: &[TypeId]) {
    let mut own: Vec<u16> = ids.iter()
        .filter(|(decl, _)| provided.contains(decl))
        .map(|(_, id)| *id)
        .collect();
    own.sort_unstable();
    ty.witness_ids = own.into_boxed_slice();
}

/// Builds `Err`'s runtime object from the layout the compiler resolved for its declaration.
fn build_err_type(gc: &mut Gc, ids: &[(TypeId, u16)], layout: &BuiltinLayout) -> *mut ObjType {
    let err = gc.intern("Err");
    let mut ty = ObjType::new(err);
    for (name, member) in &layout.members {
        ty.members.insert(gc.intern(name), *member);
    }
    ty.field_count = layout.field_count;
    ty.id = layout.id;
    ty.member_count = layout.member_count;

    let init = ObjNativeFn::new(err, 1, |vm, target, args| {
        let instance = target.as_object().as_instance_ptr();
        unsafe { (*instance).set(0, args[0]) };
        vm.push(target);
        Ok(())
    });
    ty.methods.insert(layout.factory_id, gc.alloc(init).into());
    ty.factory_id = Some(layout.factory_id);
    ty.provided.insert(layout.id);
    apply_witness_ids(&mut ty, ids, &[layout.id]);
    ty.build_template();
    gc.alloc(ty)
}

pub fn execute(chunk: BytecodeChunk, gc: Gc, _forced: bool) -> Result<Vec<String>, anyhow::Error> {
    Vm::execute(chunk, gc)
}

impl Host for Vm {
    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn gc(&mut self) -> &mut Gc {
        &mut self.gc
    }

    fn collect(&mut self) {
        self.start_gc();
    }

    fn print(&mut self, text: String) {
        self.out.push(text.clone());
        Output::println(text);
    }

    fn code_index(&self) -> u32 {
        self.current_pos_index()
    }

    fn receiver_is_frame_local(&self) -> bool {
        self.native_receiver_is_frame_local
    }

    fn argument_is_borrowed(&self, position: usize) -> bool {
        objects::mask_holds(self.native_borrowed_arguments, position)
    }

}

impl Vm {
    pub fn execute(chunk: BytecodeChunk, mut gc: Gc) -> Result<Vec<String>, anyhow::Error> {
        // The test harness reads this dump, so `capture_output` has to produce it in release too.
        #[cfg(any(debug_assertions, feature = "capture_output"))] {
            disassemble(&chunk);
        }

        let native_types = NativeTypes {
            array: build_native_type(&mut gc, NativeArray),
            dict: build_native_type(&mut gc, NativeDict),
            err: {
                let layout = chunk.builtin_layouts[BuiltinType::Err.index()].as_ref()
                    .expect("assembly rejects a chunk with a built-in layout missing");
                build_err_type(&mut gc, &chunk.witness_ids, layout)
            }
        };


        let mut vm = Vm {
            gc,
            ip: std::ptr::null(),
            chunk,
            globals: HashMap::default(),
            stack: Stack::new(),
            frames: CachedStack::new(),
            try_frames: Vec::new(),
            tail_breadcrumbs: Vec::new(),
            open_upvalues: Vec::new(),
            native_types,
            index_cache: vec![IndexCache::empty(); INDEX_CACHE_SIZE].into_boxed_slice(),
            call_cache: vec![CallCache::empty(); CALL_CACHE_SIZE].into_boxed_slice(),
            elisions_reached: FnvHashSet::default(),
            out: Vec::new(),
            native_receiver_is_frame_local: false,
            native_borrowed_arguments: 0
        };

        vm.stack.init();
        vm.frames.init();
        vm.ip = vm.chunk.code.as_ptr();

        vm.frames.push(CallFrame {
            closure: std::ptr::null_mut(),
            return_ip: std::ptr::null(),
            stack_start: vm.stack.top(),
            seal: false,
        });

        vm.define_native("print", 1, |vm, _target, args| {
            let value = args[0];
            let value_str = match value.kind() {
                ValueKind::Null => String::from("null"),
                ValueKind::Number => format!("{}", value.as_number()),
                ValueKind::Boolean => format!("{}", value.as_bool()),
                ValueKind::Object(ObjectKind::String) => format!("{}", value.as_object().as_string()),
                ValueKind::Object(_) => format!("{}", value.as_object().fmt())
            };
            vm.print(value_str);
            vm.push(Value::NULL);
            Ok(())
        });

        vm.define_native("time", 0, |vm, _target, _args| {
            let time = std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH).unwrap().as_millis() as f64;
            vm.push(Value::from(time));
            Ok(())
        });

        vm.define_native("gcHeapSize", 0, |vm, _target, _args| {
            let bytes = vm.gc().bytes_allocated as f64;
            vm.push(Value::from(bytes));
            Ok(())
        });

        vm.define_native("gcCollect", 0, |vm, _target, _args| {
            vm.collect();
            vm.push(Value::NULL);
            Ok(())
        });

        vm.define_native("gcStress", 1, |vm, _target, args| {
            vm.gc().stress = args[0].as_bool();
            vm.push(Value::NULL);
            Ok(())
        });

        vm.define_native("freeze", 1, |vm, _target, args| {
            objects::freeze_value(args[0], vm.code_index());
            vm.push(args[0]);
            Ok(())
        });

        let err_name = vm.gc.intern("Err");
        vm.globals.insert(err_name, Value::from(vm.native_types.err));

        debug_assert_eq!(vm.globals.len(), crate::core::builtins::NAMES.len(), "built-in registration drifted from core::builtins::NAMES");
        debug_assert!(crate::core::builtins::NAMES.iter().all(|n| vm.globals.contains_key(&vm.gc.intern(*n))),
            "a name in core::builtins::NAMES was not registered as a native");

        let ip = vm.ip;
        let top = vm.stack.top();
        let base = unsafe { (*vm.frames.top()).stack_start };
        let result = threaded::dispatch(&mut vm, ip, top, base);

        vm.report_elision_coverage();
        Ok(result?)
    }

    fn report_elision_coverage(&self) {
        if self.chunk.elisions.is_empty() {
            return;
        }
        eprintln!("forced checks: {} of {} elided sites reached",
            self.elisions_reached.len(), self.chunk.elisions.len());
    }

    fn at_elided_site(&mut self) -> bool {
        if self.chunk.elisions.is_empty() {
            return false;
        }
        let site = self.code_index_at(self.ip);
        let forced = self.chunk.elisions.contains(&site);
        if forced {
            self.elisions_reached.insert(site);
        }
        forced
    }

    #[cold]
    fn refuted_elision_error(&self, what: &str) -> Result<(), anyhow::Error> {
        self.raise(Diagnostic::new(format!("unsound elision: {what}"), self.get_source_position().clone())
            .with_help("the check pass proved this check unnecessary, and forcing it back on refuted that"))
    }

    fn stringify_frame(&self, frame: &CallFrame, ip: *const OpCode) -> String {
        let name = unsafe { &(*(*frame.closure).name).value };
        format!("\tat {} ({})", name, self.source_pos_at(ip))
    }

    pub(super) fn release_tail_breadcrumbs(&mut self) {
        let live = self.frames.top_ptr();
        while self.tail_breadcrumbs.last().is_some_and(|&(f, _)| f >= live) {
            self.tail_breadcrumbs.pop();
        }
    }

    #[inline]
    pub(super) fn record_tail_call_breadcrumb(&mut self, frame: *mut CallFrame, from: *mut ObjClosure, to: *mut ObjClosure) {
        if self.tail_breadcrumbs.last().is_some_and(|&(f, held)| f == frame && held == to) {
            return;
        }
        self.add_tail_call_breadcrumb(frame, from, to);
    }

    #[cold]
    #[inline(never)]
    fn add_tail_call_breadcrumb(&mut self, frame: *mut CallFrame, from: *mut ObjClosure, to: *mut ObjClosure) {
        // `from` is the previous `to`, so only the first call in a frame has it to add.
        if !self.tail_breadcrumbs.last().is_some_and(|&(f, _)| f == frame) {
            self.tail_breadcrumbs.push((frame, from));
        }
        if !self.holds_tail_call_breadcrumb(frame, to) {
            self.tail_breadcrumbs.push((frame, to));
        }
    }

    fn holds_tail_call_breadcrumb(&self, frame: *mut CallFrame, closure: *mut ObjClosure) -> bool {
        for &(f, held) in self.tail_breadcrumbs.iter().rev() {
            if f != frame {
                return false;
            }
            if held == closure {
                return true;
            }
        }
        false
    }

    fn stringify_op(&self, ip: *const OpCode) -> String {
        format!("\tat script ({})", self.source_pos_at(ip))
    }

    fn code_index_at(&self, ip: *const OpCode) -> usize {
        unsafe { ip.offset_from(self.chunk.code.as_ptr()) as usize - 1 }
    }

    fn source_pos_at(&self, ip: *const OpCode) -> &SourcePosition {
        &self.chunk.code_pos[self.code_index_at(ip)]
    }

    fn error(&self, message: impl Into<String>) -> Result<(), anyhow::Error> {
        self.raise(Diagnostic::new(message, self.get_source_position().clone()))
    }

    fn error_labeled(&self, message: impl Into<String>, label: impl Into<String>) -> Result<(), anyhow::Error> {
        self.raise(Diagnostic::new(message, self.get_source_position().clone()).with_label(label))
    }

    fn immutable_error(&self, target: Value) -> Result<(), anyhow::Error> {
        let mut diagnostic = Diagnostic::new(objects::IMMUTABLE_MUTATION, self.get_source_position().clone())
            .with_label("this value is immutable");
        if let Some(origin) = target.as_object().immutable_origin() {
            let pos = self.chunk.code_pos[origin as usize].clone();
            diagnostic = diagnostic.with_context_span(pos, "value made immutable here");
        }
        self.raise(diagnostic)
    }

    pub(super) fn readonly_receiver_error(&self, name: *mut ObjString, target: Value) -> Result<(), anyhow::Error> {
        let method = unsafe { &(*name).value };
        let mut diagnostic = Diagnostic::new(format!("`{method}` declares `mut this`, but its receiver is immutable"),
            self.get_source_position().clone())
            .with_label("this receiver cannot be mutated");
        if let Some(origin) = target.as_object().immutable_origin() {
            let pos = self.chunk.code_pos[origin as usize].clone();
            diagnostic = diagnostic.with_context_span(pos, "value made immutable here");
        }
        self.raise(diagnostic)
    }

    #[inline]
    pub(super) fn receiver_rejects_mut(&self, target: Value) -> bool {
        matches!(target.kind(), ValueKind::Object(_)) && target.as_object().is_immutable()
    }

    fn mutable_in_immutable_error(&self) -> Result<(), anyhow::Error> {
        self.raise(Diagnostic::new(objects::MUTABLE_IN_IMMUTABLE, self.get_source_position().clone())
            .with_label("this container is immutable")
            .with_help("an element is mutable; freeze it, or mark the container `mut`"))
    }

    fn raise(&self, diagnostic: Diagnostic) -> Result<(), anyhow::Error> {
        let frames: Vec<CallFrame> = self.frames.iter().collect();
        let mut ip = self.ip;
        let mut lines = Vec::new();
        for i in (1..frames.len()).rev() {
            lines.push(self.stringify_frame(&frames[i], ip));
            let at = unsafe { self.frames.top().sub(frames.len() - 1 - i) };
            self.push_rendered_tail_call_breadcrumbs(at, &mut lines);
            ip = frames[i].return_ip;
        }
        if frames.len() > 1 {
            lines.push(self.stringify_op(ip));
        }
        let trace = lines.join("\n");

        if trace.is_empty() {
            bail!("{}", diagnostic)
        }
        bail!("{}", diagnostic.with_trace(trace))
    }

    fn push_rendered_tail_call_breadcrumbs(&self, frame: *mut CallFrame, lines: &mut Vec<String>) {
        let mut held = self.tail_breadcrumbs.iter().filter(|(f, _)| *f == frame).peekable();
        if held.peek().is_none() {
            return;
        }
        lines.push("	at tail calls".to_string());
        for (_, closure) in held {
            let name = unsafe { &(*(**closure).name).value };
            // A breadcrumb has no call site, so it names where the function begins.
            let entry = unsafe { self.chunk.code.as_ptr().add((**closure).ip_start + 1) };
            lines.push(format!("	  {} ({})", name, self.source_pos_at(entry)));
        }
    }

    fn intern(&mut self, name: impl Into<String>) -> *mut ObjString {
        if self.gc.should_collect() {
            self.start_gc();
        }

        self.gc.intern(name)
    }

    fn alloc<T: GcTraceable>(&mut self, obj: T) -> *mut T
        where *mut T: Into<Object>
    {
        if self.gc.should_collect() {
            self.start_gc();
        }

        self.gc.alloc(obj)
    }

    fn define_native(&mut self, name: impl Into<String>, arity: u8, function: NativeFn) {
        let name_ref = self.gc.intern(name.into());
        let native = ObjNativeFn::new(name_ref, arity, function);
        let value = Value::from(self.gc.alloc(native));
        self.globals.insert(name_ref, value);
    }

    #[cfg(debug_assertions)]
    fn verify_roots(&self) {
        let (bottom, top) = (self.stack.bottom(), self.stack.top());
        for &upvalue in &self.open_upvalues {
            let location = unsafe { (*upvalue).location };
            assert!(location >= bottom && location < top, "an open upvalue points outside the live stack");
        }
        let mut previous = bottom;
        for frame in self.frames.iter() {
            assert!(frame.stack_start >= bottom && frame.stack_start <= top, "a frame starts outside the live stack");
            assert!(frame.stack_start >= previous, "frames are out of order");
            previous = frame.stack_start;
        }
    }

    fn start_gc(&mut self) {
        #[cfg(debug_assertions)]
        self.verify_roots();
        self.chunk.mark(&mut self.gc);
        self.native_types.mark(&mut self.gc);

        for (&name, value) in &self.globals {
            self.gc.mark_object(name);
            value.mark(&mut self.gc);
        }

        for &upvalue in &self.open_upvalues {
            self.gc.mark_object(upvalue);
        }

        for value in self.stack.iter() {
            value.mark(&mut self.gc);
        }

        for frame in self.frames.iter() {
            if !frame.closure.is_null() {
                self.gc.mark_object(frame.closure);
            }
        }

        // A function named in a trace may be reachable from nothing else.
        for (_, closure) in &self.tail_breadcrumbs {
            self.gc.mark_object(*closure);
        }


        for entry in self.call_cache.iter_mut() {
            *entry = CallCache::empty();
        }
        for entry in self.index_cache.iter_mut() {
            *entry = IndexCache::empty();
        }

        self.gc.trace();
        self.gc.sweep();
    }

    #[inline]
    pub fn read_next(&mut self) -> OpCode {
        let op = unsafe { *self.ip };
        self.ip = unsafe { self.ip.add(1) };
        op
    }

    pub fn get_source_position(&self) -> &SourcePosition {
        self.source_pos_at(self.ip)
    }

    pub fn current_pos_index(&self) -> u32 {
        self.code_index_at(self.ip) as u32
    }
}

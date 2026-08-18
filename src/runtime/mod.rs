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
    /// How much write-ownership was held when this frame began, so a return gives back what the
    /// body still holds.
    write_depth: usize,
    /// How many borrow marks were saved when this frame began.
    borrow_depth: usize,
}

/// Who holds an element's writer slot. A name lives in a frame slot, so its claim dies with the
/// frame. A container is identified by its value, whose lifetime that frame does not bound.
#[derive(Clone, Copy)]
pub enum WriteOwnershipHolder {
    Name(*mut Value),
    Container(Value),
    /// A `*mut` parameter whose frame is gone. The write-ownership was handed over and the taker
    /// never handed it on, so it belongs to nobody.
    Retired,
}

/// What a call puts in slot zero.
#[derive(Clone, Copy, PartialEq)]
pub enum ReceiverSlot {
    /// The slot holds the callee, so the call has no receiver.
    Callee,
    /// The caller lends its receiver for the call and gets it back, and the body may hand `this`
    /// on, so the borrow is written down where such a call can read it.
    BorrowedRecorded,
    Borrowed,
    Retained,
}

impl ReceiverSlot {
    pub fn declared(retain: bool, records: bool) -> ReceiverSlot {
        match (retain, records) {
            (true, _) => ReceiverSlot::Retained,
            (false, true) => ReceiverSlot::BorrowedRecorded,
            (false, false) => ReceiverSlot::Borrowed,
        }
    }
}

/// The root used to reach a target.
#[derive(Clone, Copy)]
pub enum WriteRoot {
    /// The write reaches its target through a binding, named by where it lives, as in `a[0] = 1`.
    /// The flag marks a path through the receiver.
    Named(*mut Value, bool),
    /// The write targets an element of an unnamed value, such as `get()[0][0] = 1`. The stash
    /// holds the container the element came from.
    Stashed(Value),
    /// The write targets an unnamed value directly, such as `get()[0] = 1`. This value has no name
    /// or container. There is nothing to compare a second writer against.
    NoRoot,
}

/// Write-ownership of one element, held by a name or by a container.
#[derive(Clone, Copy)]
pub struct WriteOwnership {
    pub value: Value,
    pub holder: WriteOwnershipHolder,
    pub at: u32,
    pub how: WriteOwnershipSource,
}

/// How a holder came by write-ownership, which is what a scope exit needs to dispose of it.
#[derive(Clone, Copy, PartialEq)]
pub enum WriteOwnershipSource {
    /// A name binding the value. The write-ownership returns to the source when the name dies.
    Bound,
    /// A store into a container. The claim re-keys to the container, which outlives the name.
    Given,
    /// A `*mut` hand-off to a parameter. The write-ownership does not come back, so the claim
    /// follows the value out of the frame or retires with it.
    Taken,
}

#[derive(Clone, Copy)]
pub struct TryFrame {
    origin: *mut CallFrame,
    handler_ip: *const OpCode,
    stack_start: *mut Value,
    /// The borrow-stack depth when the `try` began, restored on an unwind to this handler.
    borrow_depth: usize,
    write_depth: usize,
    /// The stash depth at the start of the try block.
    stash_depth: usize
}

/// An argument a resolved call said it only borrows, watched for as long as that call runs.
struct BorrowClaim {
    value: Value,
    /// Containers the callee put the argument in.
    into: Vec<Value>,
    /// The frame depth once the call pushed its own frame. The claim ends when the frames are
    /// that deep again.
    depth: usize,
    /// The lowest slot the call's frame owns. A store below it lands in a frame that outlives the call.
    stack_start: *mut Value,
    position: u8,
    site: usize,
}

pub struct Vm {
    /// Forced checks this run reached, for the coverage report.
    elisions_reached: FnvHashSet<usize>,
    /// Whether this run puts the claims the analysis made on trial.
    forced: bool,
    /// Arguments a call claimed it borrows, live until that call returns.
    borrow_claims: Vec<BorrowClaim>,
    /// Containers a finished call put a borrowed argument into. A collection settles them in place,
    /// so what is left after it is what outlived the call.
    settling_containments: Vec<(Value, u8, usize)>,
    /// How many claims the run put to a callee.
    claims_made: usize,
    claims_settled: usize,
    /// Write barriers that had to collect before they could answer, split by what the collection
    /// then said. A trace the barrier did not need is an escape no primitive recorded, so only
    /// those sites are worth naming.
    write_barrier_missed_sites: FnvHashSet<usize>,
    write_barrier_traced_missed: usize,
    write_barrier_traced_refused: usize,
    /// The write-ownership a scope exit would have released, each with the site it would have
    /// released at. Nothing acts on a prediction, so a wrong one costs a report rather than a
    /// second writer. The next collection settles the list and clears it, since none of it is a
    /// root.
    predicted_write_ownership_releases: Vec<(Value, usize)>,
    predicted_write_ownership_release_sites: FnvHashSet<usize>,
    refuted_write_ownership_release_sites: FnvHashSet<usize>,
    refuted_write_ownership_releases: usize,
    pub(crate) gc: Gc,
    ip: *const OpCode,
    chunk: BytecodeChunk,
    globals: HashMap<*mut ObjString, Value, BuildHasherDefault<FxHasher>>,
    pub(crate) stack: Stack<Value, MAX_STACK>,
    frames: CachedStack<CallFrame, MAX_FRAMES>,
    try_frames: Vec<TryFrame>,
    /// Values marked borrowed for an active call, each with its prior bit for nesting.
    borrows: Vec<(Value, bool)>,
    /// Values whose element writer slot is held, innermost last.
    write_ownerships: Vec<WriteOwnership>,
    root_stash: Vec<Value>,
    open_upvalues: Vec<*mut ObjUpvalue>,
    native_types: NativeTypes,
    index_cache: Box<[IndexCache]>,
    call_cache: Box<[CallCache]>,
    out: Vec<String>,
    /// Whether the receiver of the native about to run is a slot the calling frame declared. Set
    /// at an invoke that named a root, and cleared once the native has read it. Last in the struct
    /// so the dispatch loop's fields keep their offsets.
    native_receiver_is_frame_local: bool
}

macro_rules! as_short {
    ($l:expr, $r:expr) => { ($l as u16) | (($r as u16) << 8) }
}

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

pub fn execute(chunk: BytecodeChunk, gc: Gc, forced: bool) -> Result<Vec<String>, anyhow::Error> {
    Vm::execute(chunk, gc, forced)
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

    fn note_containment(&mut self, container: Value, value: Value) {
        self.note_claimed_containment(container, value);
    }
}

impl Vm {
    pub fn execute(chunk: BytecodeChunk, mut gc: Gc, forced: bool) -> Result<Vec<String>, anyhow::Error> {
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
            borrows: Vec::new(),
            write_ownerships: Vec::new(),
            root_stash: Vec::new(),
            open_upvalues: Vec::new(),
            native_types,
            index_cache: vec![IndexCache::empty(); INDEX_CACHE_SIZE].into_boxed_slice(),
            call_cache: vec![CallCache::empty(); CALL_CACHE_SIZE].into_boxed_slice(),
            elisions_reached: FnvHashSet::default(),
            forced,
            borrow_claims: Vec::new(),
            settling_containments: Vec::new(),
            claims_made: 0,
            claims_settled: 0,
            write_barrier_missed_sites: FnvHashSet::default(),
            write_barrier_traced_missed: 0,
            write_barrier_traced_refused: 0,
            predicted_write_ownership_releases: Vec::new(),
            predicted_write_ownership_release_sites: FnvHashSet::default(),
            refuted_write_ownership_release_sites: FnvHashSet::default(),
            refuted_write_ownership_releases: 0,
            out: Vec::new(),
            native_receiver_is_frame_local: false
        };

        vm.stack.init();
        vm.frames.init();
        vm.ip = vm.chunk.code.as_ptr();

        vm.frames.push(CallFrame {
            closure: std::ptr::null_mut(),
            return_ip: std::ptr::null(),
            stack_start: vm.stack.top(),
            seal: false,
            write_depth: 0,
            borrow_depth: 0,
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

        // The registered built-ins must match the list `middle::bind` checks references against,
        // or a valid call to a native would be rejected as an undefined variable (or vice versa).
        debug_assert_eq!(vm.globals.len(), crate::core::builtins::NAMES.len(), "built-in registration drifted from core::builtins::NAMES");
        debug_assert!(crate::core::builtins::NAMES.iter().all(|n| vm.globals.contains_key(&vm.gc.intern(*n))),
            "a name in core::builtins::NAMES was not registered as a native");

        let ip = vm.ip;
        let top = vm.stack.top();
        let base = unsafe { (*vm.frames.top()).stack_start };
        let result = threaded::dispatch(&mut vm, ip, top, base);

        // A release is only settled by a collection, and a program can make its last allocation
        // before its last release. Collect once more so nothing pending goes unchecked.
        if result.is_ok() && std::env::var_os("CLISAY_BARRIER_TRACES").is_some() {
            // The script's own locals are still rooted here, so a release the root scope made would
            // read as reachable. Drop them first, or every top-level container refutes.
            vm.stack.set_top(vm.stack.bottom());
            vm.start_gc();
        }

        vm.report_elision_coverage();
        vm.report_claim_coverage();
        vm.report_barrier_traces();
        Ok(result?)
    }

    /// How often a write barrier had to collect to answer. Every trace but a genuine refusal is an
    /// escape that reached no recording point, so this is the list of primitives still to cover.
    fn report_barrier_traces(&self) {
        if std::env::var_os("CLISAY_BARRIER_TRACES").is_none() {
            return;
        }

        // The two signals are independent. A wrongly released claim lets the write through, so it
        // takes no trace at all, and gating one report on the other hides exactly that case.
        if self.write_barrier_traced_missed > 0 || self.write_barrier_traced_refused > 0 {
            eprintln!("barrier traces: {} ({} missed release over {} sites, {} refused)",
                self.write_barrier_traced_missed + self.write_barrier_traced_refused, self.write_barrier_traced_missed,
                self.write_barrier_missed_sites.len(), self.write_barrier_traced_refused);
        }
        if self.refuted_write_ownership_releases > 0 {
            eprintln!("write-ownership release: {} refuted at {} sites",
                self.refuted_write_ownership_releases, self.refuted_write_ownership_release_sites.len());
        }
        for &site in self.refuted_write_ownership_release_sites.iter() {
            let pos = &self.chunk.code_pos[site];
            eprintln!("  refuted {}:{} {}", pos.source.name, pos.line, pos.snippet());
        }
        for &site in self.write_barrier_missed_sites.iter() {
            let pos = &self.chunk.code_pos[site];
            eprintln!("  {}:{} {}", pos.source.name, pos.line, pos.snippet());
        }
    }

    fn report_elision_coverage(&self) {
        if self.chunk.elisions.is_empty() {
            return;
        }
        eprintln!("forced checks: {} of {} elided sites reached",
            self.elisions_reached.len(), self.chunk.elisions.len());
    }

    fn report_claim_coverage(&self) {
        if self.claims_made == 0 {
            return;
        }
        eprintln!("forced claims: {} borrow claims, {} settled by a collection",
            self.claims_made, self.claims_settled);
    }

    /// Whether the instruction now executing is a check the analysis elided and forcing put back.
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

    /// Error for a forced check that failed.
    #[cold]
    fn refuted_elision(&self, what: &str) -> Result<(), anyhow::Error> {
        self.raise(Diagnostic::new(format!("unsound elision: {what}"), self.get_source_position().clone())
            .with_help("the check pass proved this check unnecessary, and forcing it back on refuted that"))
    }

    fn stringify_frame(&self, frame: &CallFrame, ip: *const OpCode) -> String {
        let name = unsafe { &(*(*frame.closure).name).value };
        format!("\tat {} ({})", name, self.pos_at(ip))
    }

    /// The top-level script frame, shown as the base of a call chain.
    fn stringify_base(&self, ip: *const OpCode) -> String {
        format!("\tat script ({})", self.pos_at(ip))
    }

    /// The code index of the instruction whose opcode sits just before `ip`.
    fn code_index_at(&self, ip: *const OpCode) -> usize {
        unsafe { ip.offset_from(self.chunk.code.as_ptr()) as usize - 1 }
    }

    /// The source position of the code byte just before `ip`. That is the instruction's own span
    /// for all but the operand bytes that carry a narrower one of their own.
    fn pos_at(&self, ip: *const OpCode) -> &SourcePosition {
        &self.chunk.code_pos[self.code_index_at(ip)]
    }

    fn error(&self, message: impl Into<String>) -> Result<(), anyhow::Error> {
        self.raise(Diagnostic::new(message, self.get_source_position().clone()))
    }

    /// A runtime error whose caret carries a label.
    fn error_labeled(&self, message: impl Into<String>, label: impl Into<String>) -> Result<(), anyhow::Error> {
        self.raise(Diagnostic::new(message, self.get_source_position().clone()).with_label(label))
    }

    /// Traps a mutation of an immutable value. The primary caret marks the mutation site, and a
    /// context caret points back at where the value became immutable when that site is known.
    fn error_immutable(&self, target: Value) -> Result<(), anyhow::Error> {
        let mut diagnostic = Diagnostic::new(objects::IMMUTABLE_MUTATION, self.get_source_position().clone())
            .with_label("this value is immutable");
        if let Some(origin) = target.as_object().immutable_origin() {
            let pos = self.chunk.code_pos[origin as usize].clone();
            diagnostic = diagnostic.with_context_span(pos, "value made immutable here");
        }
        self.raise(diagnostic)
    }

    /// Traps a call whose receiver cannot satisfy the method's declared `mut this`.
    pub(super) fn error_readonly_receiver(&self, name: *mut ObjString, target: Value) -> Result<(), anyhow::Error> {
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

    /// Whether a receiver fails a method's declared `mut this`.
    #[inline]
    pub(super) fn receiver_rejects_mut(&self, target: Value) -> bool {
        matches!(target.kind(), ValueKind::Object(_)) && target.as_object().is_immutable()
    }

    /// Traps a mutable element landing in an immutable container at construction.
    fn error_seal(&self) -> Result<(), anyhow::Error> {
        self.raise(Diagnostic::new(objects::MUTABLE_IN_IMMUTABLE, self.get_source_position().clone())
            .with_label("this container is immutable")
            .with_help("an element is mutable; freeze it, or mark the container `mut`"))
    }

    /// Attaches the call trace to a diagnostic and raises it.
    fn raise(&self, diagnostic: Diagnostic) -> Result<(), anyhow::Error> {
        // Each frame is paused on one instruction: the current `ip` for the top frame, and each
        // caller's saved `return_ip` for the frames below it. The base frame has no closure.
        let frames: Vec<CallFrame> = self.frames.iter().collect();
        let mut ip = self.ip;
        let mut lines = Vec::new();
        for i in (1..frames.len()).rev() {
            lines.push(self.stringify_frame(&frames[i], ip));
            ip = frames[i].return_ip;
        }
        // Show the top-level script as the base of the chain, but only when a function frame sits
        // above it. At top level the primary caret already marks the site.
        if frames.len() > 1 {
            lines.push(self.stringify_base(ip));
        }
        let trace = lines.join("\n");

        if trace.is_empty() {
            bail!("{}", diagnostic)
        }
        bail!("{}", diagnostic.with_trace(trace))
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

    /// Checks the pointer invariants a collection rests on. A root outside the live stack is either
    /// marked from dead slots or never closed. Both corrupt a value far from where the mistake is.
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

        for (value, _) in &self.borrows {
            value.mark(&mut self.gc);
        }

        for value in &self.root_stash {
            value.mark(&mut self.gc);
        }

        // Both caches key on raw pointers. A sweep can free an interned string or a type and the
        // next allocation can reuse the block, so a surviving entry would answer for another name.
        for entry in self.call_cache.iter_mut() {
            *entry = CallCache::empty();
        }
        for entry in self.index_cache.iter_mut() {
            *entry = IndexCache::empty();
        }

        self.gc.trace();
        self.refute_predicted_write_ownership_releases();
        self.settle_claimed_containments();
        self.prune_borrow_claims();
        self.prune_write_ownerships();
        self.gc.sweep();
    }

    fn settle_claimed_containments(&mut self) {
        #[cfg(debug_assertions)]
        assert!(self.gc.marks_valid(), "a containment settled outside the window where marks say what survived");
        self.settling_containments.retain(|(container, _, _)| container.is_marked());
    }

    fn prune_borrow_claims(&mut self) {
        #[cfg(debug_assertions)]
        assert!(self.gc.marks_valid(), "a claim pruned outside the window where marks say what survived");
        self.borrow_claims.retain_mut(|claim| {
            claim.into.retain(|container| container.is_marked());
            claim.value.is_marked()
        });
    }

    /// Refutes the write-ownership a scope exit would have released. A container still reachable
    /// here was reachable when the release would have fired, so firing it would have been wrong.
    fn refute_predicted_write_ownership_releases(&mut self) {
        #[cfg(debug_assertions)]
        assert!(self.gc.marks_valid(), "a prediction settled outside the window where marks say what survived");
        for (container, site) in std::mem::take(&mut self.predicted_write_ownership_releases) {
            if container.is_object() && container.as_object().is_marked() {
                self.refuted_write_ownership_release_sites.insert(site);
                self.refuted_write_ownership_releases += 1;
            }
        }
    }

    /// Drops the write-ownership records a collection made pointless.
    fn prune_write_ownerships(&mut self) {
        #[cfg(debug_assertions)]
        assert!(self.gc.marks_valid(), "a claim pruned outside the window where marks say what survived");
        self.write_ownerships.retain(|held| {
            if !held.value.is_object() || !held.value.as_object().is_marked() {
                return false;
            }
            // A container not reached by the trace no longer holds the value. Clearing the bit
            // allows subsequent writes to proceed without re-verifying.
            if let WriteOwnershipHolder::Container(container) = held.holder {
                if !container.is_object() || !container.as_object().is_marked() {
                    held.value.as_object().set_write_owned(false);
                    return false;
                }
            }
            true
        });
    }

    #[inline]
    pub fn read_next(&mut self) -> OpCode {
        let op = unsafe { *self.ip };
        self.ip = unsafe { self.ip.add(1) };
        op
    }

    pub fn get_source_position(&self) -> &SourcePosition {
        self.pos_at(self.ip)
    }

    /// The code index of the instruction currently executing, for recording where a value was frozen.
    pub fn current_pos_index(&self) -> u32 {
        self.code_index_at(self.ip) as u32
    }

}

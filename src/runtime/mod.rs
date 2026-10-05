use std::collections::HashMap;
use std::hash::BuildHasherDefault;

use anyhow::bail;
use rustc_hash::FxHasher;
use smallvec::SmallVec;

use crate::Output;
use crate::core::objects::{ObjBoundMethod, ObjInstance};
use fnv::FnvHashSet;
#[cfg(debug_assertions)]
use fnv::FnvHashMap;
use crate::core::value::ValueKind;
use crate::frontend::lex::{Diagnostic, SourcePosition};

use crate::core::native::array::NativeArray;
use crate::core::native::dict::NativeDict;
use crate::core::native::NativeTypeBuilder;
use crate::core::stack::{CachedStack, Stack};
use crate::core::value::{DictKey, FrameGeneration, Value};
use crate::core::gc::{Gc, GcTraceable};
use crate::core::host::{Host, Thrown};
use crate::core::objects::{self, BuiltinLayout, TypeMember, NativeFn, ObjArray, ObjDict, ObjType, ObjClosure, ObjFn, ObjNativeFn, ObjString, Object, ObjectKind, TypeId};
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
    /// The bytecode site, or `EMPTY_SITE`.
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
}

const EMPTY_SITE: usize = usize::MAX;

impl IndexCache {
    const fn empty() -> IndexCache {
        IndexCache { site: EMPTY_SITE, prop: std::ptr::null_mut(), ty: TypeId::MAX, member: TypeMember::Field(0) }
    }
}

impl CallCache {
    const fn empty() -> CallCache {
        CallCache { site: EMPTY_SITE, callee: Value::NULL, closure: std::ptr::null_mut(), ip_start: 0 }
    }
}

struct NativeTypes {
    array: *mut ObjType,
    dict: *mut ObjType,
    err: *mut ObjType,
    ref_type: *mut ObjType
}

impl GcTraceable for NativeTypes {
    fn mark(&self, gc: &mut Gc) {
        gc.mark_object(self.array);
        gc.mark_object(self.dict);
        gc.mark_object(self.err);
        gc.mark_object(self.ref_type);
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
    generation: FrameGeneration,
}

impl CallFrame {
    #[inline]
    fn new(closure: *mut ObjClosure, return_ip: *const OpCode, stack_start: *mut Value, generation: FrameGeneration) -> CallFrame {
        CallFrame { closure, return_ip, stack_start, generation }
    }
}

#[cfg(debug_assertions)]
#[derive(Clone, Copy)]
pub(super) struct RecordedRoot {
    pub(super) root: Value,
    pub(super) formed_on: Value,
    pub(super) placed: bool,
}

pub struct Vm {
    /// Forced checks this run reached, for the coverage report.
    forced_checks_reached: FnvHashSet<usize>,
    #[cfg(debug_assertions)]
    anchor_roots: FnvHashMap<usize, RecordedRoot>,
    pub(crate) gc: Gc,
    ip: *const OpCode,
    chunk: BytecodeChunk,
    globals: HashMap<*mut ObjString, Value, BuildHasherDefault<FxHasher>>,
    pub(crate) stack: Stack<Value, MAX_STACK>,
    frames: CachedStack<CallFrame, MAX_FRAMES>,
    next_frame_generation: FrameGeneration,
    tail_breadcrumbs: Vec<(*mut CallFrame, *mut ObjClosure)>,
    held: Vec<Value>,
    lowest_anchor_path: *mut Value,
    native_types: NativeTypes,
    index_cache: Box<[IndexCache]>,
    call_cache: Box<[CallCache]>,
    #[cfg(debug_assertions)]
    counting_forks: bool,
    #[cfg(debug_assertions)]
    forks: usize,
    #[cfg(debug_assertions)]
    forked_elements: usize,
    out: Vec<String>,
}

macro_rules! as_short {
    ($l:expr, $r:expr) => { ($l as u16) | (($r as u16) << 8) }
}

mod accepts;
mod anchors;
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

fn build_native_type(gc: &mut Gc, native_type: impl NativeTypeBuilder) -> *mut ObjType {
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

fn build_builtin_type(gc: &mut Gc, name: &str, layout: &BuiltinLayout, constants: &[Value], init: NativeFn) -> ObjType {
    let name = gc.intern(name);
    let mut ty = ObjType::new(name);
    for (member_name, member) in &layout.members {
        ty.members.insert(gc.intern(member_name), *member);
    }
    ty.field_count = layout.field_count;
    ty.id = layout.id;
    ty.member_count = layout.member_count;

    ty.methods.insert(layout.factory_id, gc.alloc(ObjNativeFn::new(name, 1, init)).into());
    ty.factory_id = Some(layout.factory_id);

    // The methods the prelude declares, compiled into constants by codegen.
    for (member, constant) in &layout.methods {
        ty.methods.insert(*member, constants[*constant as usize].as_object());
    }
    ty.var_fields = layout.var_fields;
    ty.field_witness_set_pool_ids = layout.field_witness_set_pool_ids.clone();
    ty.no_persist = layout.no_persist;
    ty.provided.insert(layout.id);
    ty
}

fn build_err_type(gc: &mut Gc, ids: &[(TypeId, u16)], layout: &BuiltinLayout, constants: &[Value]) -> *mut ObjType {
    let mut ty = build_builtin_type(gc, "Err", layout, constants, |vm, target, args| {
        let instance = target.as_object().as_instance_ptr();
        unsafe { ObjInstance::set(instance, 0, args[0]) };
        vm.push(target);
        Ok(())
    });
    apply_witness_ids(&mut ty, ids, &[layout.id]);
    ty.build_template();
    gc.alloc(ty)
}

fn build_ref_type(gc: &mut Gc, layout: &BuiltinLayout, constants: &[Value]) -> *mut ObjType {
    let mut ty = build_builtin_type(gc, "Ref", layout, constants, |vm, target, args| {
        let instance = target.as_object().as_instance_ptr();
        unsafe { ObjInstance::set(instance, objects::REF_VALUE_FIELD, args[0]) };
        unsafe { ObjInstance::set(instance, objects::REF_LOCK_FIELD, Value::from(false)) };
        objects::mark_as_ref(target);
        vm.push(target);
        Ok(())
    });
    ty.build_template();
    gc.alloc(ty)
}

pub fn execute(chunk: BytecodeChunk, gc: Gc) -> Result<Vec<String>, anyhow::Error> {
    Vm::execute(chunk, gc)
}

impl Host for Vm {
    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn gc(&mut self) -> &mut Gc {
        &mut self.gc
    }

    fn share(&mut self, value: Value) -> Value {
        Vm::share(self, value)
    }

    fn share_into(&mut self, container: Value, value: Value) -> Result<Value, anyhow::Error> {
        self.share_into_container(container, value)
    }

    fn collect(&mut self) {
        self.start_gc();
    }

    fn print(&mut self, text: String) {
        self.out.push(text.clone());
        Output::println(text);
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
                build_err_type(&mut gc, &chunk.witness_ids, layout, &chunk.constants)
            },
            ref_type: {
                let layout = chunk.builtin_layouts[BuiltinType::Ref.index()].as_ref()
                    .expect("assembly rejects a chunk with a built-in layout missing");
                build_ref_type(&mut gc, layout, &chunk.constants)
            }
        };


        let mut vm = Vm {
            gc,
            ip: std::ptr::null(),
            chunk,
            globals: HashMap::default(),
            stack: Stack::new(),
            frames: CachedStack::new(),
            next_frame_generation: FrameGeneration::default(),
            tail_breadcrumbs: Vec::new(),
            held: Vec::new(),
            lowest_anchor_path: std::ptr::null_mut::<Value>().wrapping_sub(1),
            native_types,
            index_cache: vec![IndexCache::empty(); INDEX_CACHE_SIZE].into_boxed_slice(),
            call_cache: vec![CallCache::empty(); CALL_CACHE_SIZE].into_boxed_slice(),
            #[cfg(debug_assertions)]
            counting_forks: std::env::var_os("CLISAY_FORK_COUNT").is_some(),
            #[cfg(debug_assertions)]
            forks: 0,
            #[cfg(debug_assertions)]
            forked_elements: 0,
            forced_checks_reached: FnvHashSet::default(),
            #[cfg(debug_assertions)]
            anchor_roots: FnvHashMap::default(),
            out: Vec::new(),
        };

        vm.stack.init();
        vm.frames.init();
        vm.ip = vm.chunk.code.as_ptr();

        let generation = vm.take_frame_generation();
        vm.frames.push(CallFrame::new(std::ptr::null_mut(), std::ptr::null(), vm.stack.top(), generation));

        vm.define_native("print", 1, |vm, _target, args| {
            let value = args[0];
            let value_str = match value.kind() {
                ValueKind::Null => String::from("null"),
                ValueKind::Number => format!("{}", value.as_number()),
                ValueKind::Boolean => format!("{}", value.as_bool()),
                ValueKind::Object(ObjectKind::String) => format!("{}", value.as_object().as_string()),
                ValueKind::Object(_) => format!("{}", value.as_object().fmt()),
                ValueKind::Anchor => String::from("anchor"),
                ValueKind::MemberKey => String::from("member"),
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

        let err_name = vm.gc.intern("Err");
        vm.globals.insert(err_name, Value::from(vm.native_types.err));

        let ref_name = vm.gc.intern("Ref");
        vm.globals.insert(ref_name, Value::from(vm.native_types.ref_type));

        debug_assert_eq!(vm.globals.len(), crate::core::builtins::NAMES.len(), "built-in registration drifted from core::builtins::NAMES");
        debug_assert!(crate::core::builtins::NAMES.iter().all(|n| vm.globals.contains_key(&vm.gc.intern(*n))),
            "a name in core::builtins::NAMES was not registered as a native");

        // An op throws by returning `Thrown`.
        let result = loop {
            let ip = vm.ip;
            let top = vm.stack.top();
            let stack_start = unsafe { (*vm.frames.top()).stack_start };
            match threaded::dispatch(&mut vm, ip, top, stack_start).map_err(|err| err.downcast::<Thrown>()) {
                Err(Ok(Thrown(value))) => if let Err(err) = vm.catch_thrown(value) { break Err(err) },
                Err(Err(err)) => break Err(err),
                Ok(()) => break Ok(std::mem::take(&mut vm.out)),
            }
        };

        vm.report_forced_checks_reached();
        Ok(result?)
    }

    fn report_forced_checks_reached(&self) {
        if self.chunk.forced_check_ends.is_empty() {
            return;
        }
        eprintln!("forced checks: {} of {} reached",
            self.forced_checks_reached.len(), self.chunk.forced_check_ends.len());
    }

    /// Whether the running instruction is a check that's been proven unnecessary.
    pub(super) fn at_forced_check(&mut self) -> bool {
        if !cfg!(debug_assertions) || self.chunk.forced_check_ends.is_empty() {
            return false;
        }

        let site = self.code_index_at(self.ip);
        let forced = self.chunk.forced_check_ends.contains(&site);
        if forced {
            self.forced_checks_reached.insert(site);
        }

        forced
    }

    #[cold]
    pub(super) fn refuted_elision_error(&self, what: &str) -> Result<(), anyhow::Error> {
        self.raise(Diagnostic::new(format!("unsound elision: {what}"), self.get_source_position().clone())
            .with_help("the check pass proved this check unnecessary, and forcing it back on refuted that"))
    }

    fn stringify_frame(&self, frame: &CallFrame, ip: *const OpCode) -> String {
        let name = unsafe { &(*(*frame.closure).name).value };
        format!("\tat {} ({})", name, self.source_pos_at(ip))
    }

    #[cold]
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
        self.raise(Diagnostic::new(message, self.error_position().clone()))
    }

    pub(super) fn error_help(&self, message: impl Into<String>, help: impl Into<String>) -> Result<(), anyhow::Error> {
        self.raise(Diagnostic::new(message, self.error_position().clone()).with_help(help))
    }

    #[cold]
    #[inline(never)]
    fn error_position(&self) -> &SourcePosition {
        let ip = self.frames_at_ips().map(|(_, ip)| ip).find(|&ip| !self.source_pos_at(ip).is_vm_source());
        self.source_pos_at(ip.unwrap_or(self.ip))
    }

    /// Each frame from the top down, with its ip.
    pub(super) fn frames_at_ips(&self) -> impl Iterator<Item = (*mut CallFrame, *const OpCode)> + '_ {
        let top = self.frames.top();
        self.frames.iter().rev().enumerate().scan(self.ip, move |ip, (depth, frame)| {
            let running = *ip;
            *ip = frame.return_ip;
            Some((unsafe { top.sub(depth) }, running))
        })
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
        self.maybe_collect();
        self.gc.intern(name)
    }

    #[inline]
    fn maybe_collect(&mut self) {
        if self.gc.should_collect() {
            self.start_gc();
        }
    }

    fn alloc_array(&mut self, elements: &[Value]) -> *mut ObjArray {
        self.maybe_collect();
        self.gc.alloc_array(elements, elements.len())
    }

    fn alloc_instance(&mut self, type_ptr: *mut ObjType) -> *mut ObjInstance {
        self.maybe_collect();
        self.gc.alloc_instance(type_ptr, unsafe { &(*type_ptr).template })
    }

    fn alloc<T: GcTraceable>(&mut self, obj: T) -> *mut T
        where *mut T: Into<Object>
    {
        self.maybe_collect();
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
        let mut previous = bottom;
        for frame in self.frames.iter() {
            assert!(frame.stack_start >= bottom && frame.stack_start <= top, "a frame starts outside the live stack");
            assert!(frame.stack_start >= previous, "frames are out of order");
            previous = frame.stack_start;
        }
    }

    #[inline]
    #[cfg(debug_assertions)]
    pub(super) fn report_forks(&self) {
        if self.counting_forks {
            eprintln!("forks: {} copying {} elements", self.forks, self.forked_elements);
        }
    }

    #[cfg(debug_assertions)]
    fn count_fork(&mut self, value: Value) {
        if !self.counting_forks {
            return;
        }
        self.forks += 1;
        self.forked_elements += objects::element_count(value);
    }

    pub(super) fn fork(&mut self, value: Value) -> Value {
        #[cfg(debug_assertions)]
        self.count_fork(value);
        if self.gc.should_collect() {
            let mark = self.hold(&[value]);
            self.start_gc();
            self.release_held(mark);
        }
        objects::copy_object(&mut self.gc, value)
    }

    #[inline]
    pub(super) fn share_in_place(&mut self, at: *mut Value) {
        let value = unsafe { *at };
        if !value.is_object() {
            return;
        }
        let header = value.as_object().as_header_ptr();
        if unsafe { (*header).has(objects::FLAG_IS_REF) } {
            return;
        }
        if unsafe { (*header).has(objects::FLAG_ON_ANCHOR_PATH) } {
            unsafe { *at = self.share_on_anchor_path(value) };
            return;
        }
        unsafe { (*header).set(objects::FLAG_SHARED) };
    }

    #[inline]
    pub(super) fn share(&mut self, value: Value) -> Value {
        if !value.is_object() {
            return value;
        }
        let header = value.as_object().as_header_ptr();
        if unsafe { (*header).has(objects::FLAG_IS_REF) } {
            return value;
        }
        if unsafe { (*header).has(objects::FLAG_ON_ANCHOR_PATH) } {
            return self.share_on_anchor_path(value);
        }
        unsafe { (*header).set(objects::FLAG_SHARED) };
        value
    }

    #[inline]
    pub(super) fn share_into_container(&mut self, container: Value, value: Value) -> Result<Value, anyhow::Error> {
        refuse_no_persist(value)?;
        Ok(self.share_into(container, value))
    }

    #[inline]
    #[cfg_attr(not(debug_assertions), allow(unused_variables))]
    pub(super) fn share_into(&mut self, container: Value, value: Value) -> Value {
        #[cfg(debug_assertions)]
        objects::debug_assert_store_is_acyclic(container, value, self.gc.object_count());
        self.share(value)
    }

    /// Copies a flagged value while any anchor path is live. Otherwise the flag is stale, so it
    /// comes off and the value is shared as usual.
    #[cold]
    fn share_on_anchor_path(&mut self, value: Value) -> Value {
        if self.anchor_path_is_live() {
            return self.copy_off_anchor_path(value);
        }
        let header = value.as_object().as_header_ptr();
        match unsafe { (*header).has(objects::FLAG_DETACHED) } {
            true => objects::clear_anchor_path_flags(value),
            false => unsafe { (*header).clear(objects::FLAG_ON_ANCHOR_PATH) },
        }
        unsafe { (*header).set(objects::FLAG_SHARED) };
        value
    }

    fn anchor_path_is_live(&mut self) -> bool {
        let mut at = self.lowest_anchor_path;
        while at < self.stack.top() {
            if unsafe { *at }.is_anchor_path() {
                self.lowest_anchor_path = at;
                return true;
            }
            at = unsafe { at.add(1) };
        }
        self.lowest_anchor_path = std::ptr::null_mut::<Value>().wrapping_sub(1);
        false
    }

    #[cold]
    fn copy_off_anchor_path(&mut self, value: Value) -> Value {
        let copy = self.fork(value);
        objects::mark_elements_shared_except_on_anchor_path(value, copy);
        let mark = self.hold(&[copy]);
        for (at, element) in objects::elements_on_anchor_path(copy) {
            let element = self.copy_off_anchor_path(element);
            objects::set_element(copy, &at, element);
        }
        self.release_held(mark);
        copy
    }

    pub(super) fn hold(&mut self, values: &[Value]) -> usize {
        debug_assert!(!values.iter().any(|v| v.is_anchor()),
            "an anchor names storage rather than holding a value, so there is nothing here to root");
        let mark = self.held.len();
        self.held.extend_from_slice(values);
        mark
    }

    pub(super) fn release_held(&mut self, mark: usize) {
        self.held.truncate(mark);
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

        for value in self.stack.iter() {
            value.mark(&mut self.gc);
        }

        for value in &self.held {
            value.mark(&mut self.gc);
        }

        #[cfg(debug_assertions)]
        for recorded in self.anchor_roots.values() {
            recorded.formed_on.mark(&mut self.gc);
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
    pub(super) fn read_byte_list<'a>(&mut self) -> &'a [u8] {
        let len = self.read_next() as usize;
        let list = unsafe { std::slice::from_raw_parts(self.ip, len) };
        self.ip = unsafe { self.ip.add(len) };
        list
    }

    pub fn read_next(&mut self) -> OpCode {
        let op = unsafe { *self.ip };
        self.ip = unsafe { self.ip.add(1) };
        op
    }

    pub fn get_source_position(&self) -> &SourcePosition {
        self.source_pos_at(self.ip)
    }
}

pub(super) fn throw_value<T>(value: Value) -> Result<T, anyhow::Error> {
    Err(Thrown(value).into())
}

#[inline]
pub(super) fn refuse_no_persist(value: Value) -> Result<(), anyhow::Error> {
    match objects::may_not_persist(value) {
        true => throw_value(value),
        false => Ok(()),
    }
}

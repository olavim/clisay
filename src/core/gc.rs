use std::alloc::{self, Layout};
use std::mem;

use fnv::FnvHashMap;
#[cfg(debug_assertions)]
use fnv::FnvHashSet;

use super::objects::{ObjArray, ObjClosure, ObjInstance, ObjString, ObjType, ObjectHeader, ObjectKind, Object, FLAG_MARKED};
use super::value::Value;

/// Every heap object is `repr(align(8))`, so a freed block can be reused for any later
/// object of the same size regardless of its concrete type.
const OBJ_ALIGN: usize = 8;

/// The collection threshold floor, and the value `next_gc` starts at.
const INITIAL_GC_THRESHOLD: usize = 1024 * 1024;

/// After each collection the next threshold is the live heap times this factor. Collection
/// frequency then scales with the live set instead of firing on every allocation.
const GC_GROW_FACTOR: usize = 2;

pub trait GcTraceable {
    fn fmt(&self) -> String;
    fn mark(&self, gc: &mut Gc);

    /// Marks pointers the object stores in a trailing array.
    unsafe fn mark_trailing(_ptr: *const Self, _gc: &mut Gc) where Self: Sized {}

    fn size(&self) -> usize;
    fn layout_size(&self) -> usize {
        mem::size_of_val(self)
    }
}

impl GcTraceable for String {
    fn fmt(&self) -> String {
        format!("\"{}\"", self)
    }

    fn mark(&self, _gc: &mut Gc) {}

    fn size(&self) -> usize {
        mem::size_of::<String>() + self.capacity()
    }
}

pub struct Gc {
    refs: Vec<Object>,
    strings: FnvHashMap<String, *mut ObjString>,
    reachable_refs: Vec<Object>,
    /// Recycled object blocks, bucketed by allocation size.
    /// Alignment is always `OBJ_ALIGN`.
    free_lists: FnvHashMap<usize, Vec<*mut u8>>,
    pub bytes_allocated: usize,
    next_gc: usize,
    /// When true, GC runs on every allocation.
    pub stress: bool,
    /// Blocks sitting on a free list. A pointer to one is dangling until the block is handed out
    /// again, so traversing one means `mark` missed the pointer that should have kept it alive.
    #[cfg(debug_assertions)]
    freed_blocks: FnvHashSet<usize>,
    /// Set by a trace and cleared by the sweep that consumes it.
    #[cfg(debug_assertions)]
    traced: bool
}

impl Gc {
    pub fn object_count(&self) -> usize {
        self.refs.len()
    }

    pub fn new() -> Gc {
        Gc {
            refs: Vec::new(),
            strings: FnvHashMap::default(),
            reachable_refs: Vec::new(),
            free_lists: FnvHashMap::default(),
            bytes_allocated: 0,
            next_gc: INITIAL_GC_THRESHOLD,
            // Collecting on every allocation is what makes the verifier see every intermediate
            // state, so a whole corpus can be run under it from the environment.
            stress: std::env::var_os("CLISAY_GC_STRESS").is_some(),
            #[cfg(debug_assertions)]
            freed_blocks: FnvHashSet::default(),
            #[cfg(debug_assertions)]
            traced: false
        }
    }

    /// Refuses a pointer to a block already handed back to a free list. Every traversal goes
    /// through `mark_object`, so this catches a dangling pointer wherever it is still reachable.
    #[cfg(debug_assertions)]
    fn assert_not_freed(&self, obj: Object) {
        assert!(!self.freed_blocks.contains(&(obj.as_header_ptr() as usize)),
            "traversed a pointer to a freed block");
    }

    pub fn alloc<T: GcTraceable>(&mut self, obj: T) -> *mut T
        where *mut T: Into<Object>
    {
        debug_assert_eq!(mem::align_of::<T>(), OBJ_ALIGN);
        self.bytes_allocated += obj.size();

        let obj_ptr = self.take_block(mem::size_of::<T>()) as *mut T;
        unsafe { std::ptr::write(obj_ptr, obj) };
        self.refs.push(obj_ptr.into());
        obj_ptr
    }

    /// Allocates a closure with its upvalues stored in a trailing array.
    pub fn alloc_closure(
        &mut self,
        name: *mut ObjString,
        arity: u8,
        ip_start: usize,
        count: usize,
        param_list_pool_id: u16,
        slot_witness_set_pool_id: u16
    ) -> *mut ObjClosure {
        let closure_ptr = self.alloc_block(ObjClosure {
            header: ObjectHeader::new(ObjectKind::Closure),
            name,
            arity,
            capture_count: count as u8,
            ip_start,
            param_list_pool_id,
            slot_witness_set_pool_id
        }, ObjClosure::alloc_size(count));
        unsafe { std::slice::from_raw_parts_mut(ObjClosure::captures_ptr(closure_ptr), count).fill(Value::unassigned()) };
        closure_ptr
    }

    /// Allocates an instance of `ty` with its member values stored inline, copied from `members`.
    pub fn alloc_instance(&mut self, ty: *mut ObjType, members: &[Value]) -> *mut ObjInstance {
        let instance_ptr = self.alloc_block(ObjInstance {
            header: ObjectHeader::new(ObjectKind::Instance),
            member_count: members.len() as u8,
            hash: 0,
            ty,
        }, ObjInstance::alloc_size(members.len()));
        unsafe { ObjInstance::values_mut(instance_ptr).copy_from_slice(members) };
        instance_ptr
    }

    /// Allocates an array holding a copy of `elements`.
    pub fn alloc_array(&mut self, elements: &[Value], capacity: usize) -> *mut ObjArray {
        debug_assert!(capacity >= elements.len(), "tried to allocate array with less capacity than elements");
        let array_ptr = self.alloc_block(ObjArray {
            header: ObjectHeader::new(ObjectKind::Array),
            hash: 0,
            data: std::ptr::null_mut(),
            len: elements.len() as u32,
            capacity: capacity as u32,
            inline_capacity: capacity as u32,
        }, ObjArray::alloc_size(capacity));
        unsafe {
            (*array_ptr).data = ObjArray::inline_ptr(array_ptr);
            ObjArray::elements_mut(array_ptr).copy_from_slice(elements);
        }
        array_ptr
    }

    /// Writes `head` at the start of a `size`-byte block and registers it.
    fn alloc_block<T>(&mut self, head: T, size: usize) -> *mut T
        where *mut T: Into<Object>
    {
        self.bytes_allocated += size;
        let ptr = self.take_block(size) as *mut T;
        unsafe { std::ptr::write(ptr, head) };
        self.refs.push(ptr.into());
        ptr
    }

    /// Returns a block of `size` bytes, reusing a recycled one if available.
    fn take_block(&mut self, size: usize) -> *mut u8 {
        if let Some(block) = self.free_lists.get_mut(&size).and_then(Vec::pop) {
            #[cfg(debug_assertions)]
            self.freed_blocks.remove(&(block as usize));
            return block;
        }
        let layout = unsafe { Layout::from_size_align_unchecked(size, OBJ_ALIGN) };
        let ptr = unsafe { alloc::alloc(layout) };
        if ptr.is_null() {
            alloc::handle_alloc_error(layout);
        }
        ptr
    }

    pub fn intern(&mut self, name: impl Into<String>) -> *mut ObjString {
        let name = name.into();
        if let Some(&obj) = self.strings.get(&name) {
            return obj
        }

        let gc_ref = self.alloc(ObjString::new(name.clone()));
        self.strings.insert(name, gc_ref);
        gc_ref
    }

    pub fn mark_object<T: Into<Object>>(&mut self, obj: T) {
        let obj: Object = obj.into();
        #[cfg(debug_assertions)]
        self.assert_not_freed(obj);
        unsafe {
            if !(*obj.as_header_ptr()).has(FLAG_MARKED) {
                (*obj.as_header_ptr()).set(FLAG_MARKED);
                self.reachable_refs.push(obj);
            }
        }
    }

    pub fn collect(&mut self) {
        self.trace();
        self.sweep();
    }

    /// Reaches everything the marked roots lead to. Nothing is freed yet, so a caller holding a
    /// weak reference can see whether its target survived before the sweep takes it.
    pub fn trace(&mut self) {
        while let Some(obj) = self.reachable_refs.pop() {
            obj.mark(self);
        }
        #[cfg(debug_assertions)]
        { self.traced = true; }
    }

    /// Frees whatever the trace did not reach.
    pub fn sweep(&mut self) {
        #[cfg(debug_assertions)]
        assert!(self.traced, "a sweep with no trace before it reads marks from the last cycle");
        self.sweep_strings();
        self.sweep_objects();
        // Scale the next threshold to the surviving live set, so collection frequency tracks live size.
        self.next_gc = self.bytes_allocated.saturating_mul(GC_GROW_FACTOR).max(INITIAL_GC_THRESHOLD);
        #[cfg(debug_assertions)]
        { self.traced = false; }
    }

    fn sweep_strings(&mut self) {
        self.strings.retain(|_, &mut obj_ptr| unsafe { (*obj_ptr).header.has(FLAG_MARKED) });
    }

    /// Frees the unmarked and recounts what survived; An object can grow after it's allocated.
    fn sweep_objects(&mut self) {
        let mut live = 0;
        for i in (0..self.refs.len()).rev() {
            let obj = self.refs[i];
            unsafe {
                if (*obj.as_header_ptr()).has(FLAG_MARKED) {
                    (*obj.as_header_ptr()).clear(FLAG_MARKED);
                    live += obj.size();
                } else {
                    self.free(i);
                }
            }
        }
        self.bytes_allocated = live;
    }

    fn free(&mut self, idx: usize) {
        let obj = self.refs[idx];
        let block = obj.as_header_ptr() as *mut u8;
        let layout_size = obj.free();
        // Retain the block for reuse rather than handing it back to the system allocator.
        self.free_lists.entry(layout_size).or_default().push(block);
        #[cfg(debug_assertions)]
        self.freed_blocks.insert(block as usize);
        self.refs.swap_remove(idx);
    }

    pub fn should_collect(&self) -> bool {
        self.stress || self.bytes_allocated > self.next_gc
    }
}

impl Drop for Gc {
    fn drop(&mut self) {
        // Drop all live objects
        for &obj in &self.refs {
            let block = obj.as_header_ptr() as *mut u8;
            let layout_size = obj.free();
            unsafe { alloc::dealloc(block, Layout::from_size_align_unchecked(layout_size, OBJ_ALIGN)) };
        }
        // Free memory of recycled blocks
        for (&size, blocks) in &self.free_lists {
            for &block in blocks {
                unsafe { alloc::dealloc(block, Layout::from_size_align_unchecked(size, OBJ_ALIGN)) };
            }
        }
    }
}
use std::alloc::{self, Layout};
use std::mem;

use fnv::FnvHashMap;

use super::objects::{ObjClosure, ObjString, ObjUpvalue, ObjectHeader, ObjectKind, Object};

/// Every heap object is `repr(align(8))`, so a freed block can back any later
/// object of the same size regardless of its concrete type. Blocks are bucketed
/// by size alone with this fixed alignment.
const OBJ_ALIGN: usize = 8;

pub trait GcTraceable {
    fn fmt(&self) -> String;
    fn mark(&self, gc: &mut Gc);

    /// Bytes attributed to this object for GC accounting: the struct plus any heap
    /// it owns separately (e.g. a `Vec`/`String`'s capacity).
    fn size(&self) -> usize;

    /// Size of the object's own allocation block, used to bucket it on the free
    /// list. Defaults to the struct size; types whose allocation includes a
    /// trailing array (e.g. `ObjClosure`) override this.
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

pub struct PresetIdentifiers {
    pub init: *mut ObjString,
    pub get: *mut ObjString,
    pub set: *mut ObjString
}

impl Default for PresetIdentifiers {
    fn default() -> Self {
        PresetIdentifiers {
            init: std::ptr::null_mut(),
            get: std::ptr::null_mut(),
            set: std::ptr::null_mut()
        }
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
    pub preset_identifiers: PresetIdentifiers
}

impl Gc {
    pub fn new() -> Gc {
        let mut gc = Gc {
            refs: Vec::new(),
            strings: FnvHashMap::default(),
            reachable_refs: Vec::new(),
            free_lists: FnvHashMap::default(),
            bytes_allocated: 0,
            next_gc: 1024 * 1024,
            stress: false,
            preset_identifiers: PresetIdentifiers::default()
        };

        gc.preset_identifiers.init = gc.intern("@init");
        gc.preset_identifiers.get = gc.intern("@get");
        gc.preset_identifiers.set = gc.intern("@set");
        gc
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

    /// Allocates a closure with its upvalues stored inline in a trailing array.
    /// The block is sized to the exact capture count. Closures of equal capture
    /// count recycle each other's blocks and no separate upvalue buffer is ever
    /// allocated.
    pub fn alloc_closure(
        &mut self,
        name: *mut ObjString,
        arity: u8,
        ip_start: usize,
        upvalues: &[*mut ObjUpvalue]
    ) -> *mut ObjClosure {
        let count = upvalues.len();
        let size = ObjClosure::alloc_size(count);
        self.bytes_allocated += size;

        let closure_ptr = self.take_block(size) as *mut ObjClosure;
        unsafe {
            std::ptr::write(closure_ptr, ObjClosure {
                header: ObjectHeader::new(ObjectKind::Closure),
                name,
                arity,
                upvalue_count: count as u8,
                ip_start
            });
            std::ptr::copy_nonoverlapping(
                upvalues.as_ptr(),
                (*closure_ptr).upvalues().as_ptr() as *mut *mut ObjUpvalue,
                count
            );
        }
        self.refs.push(closure_ptr.into());
        closure_ptr
    }

    /// Returns a block of `size` bytes (alignment `OBJ_ALIGN`), reusing a recycled
    /// one if available. The block is uninitialized; callers must write a valid
    /// object before it can be marked or freed.
    fn take_block(&mut self, size: usize) -> *mut u8 {
        if let Some(block) = self.free_lists.get_mut(&size).and_then(Vec::pop) {
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
        unsafe {
            if !(*obj.as_header_ptr()).marked {
                (*obj.as_header_ptr()).marked = true;
                self.reachable_refs.push(obj);
            }
        }
    }

    pub fn collect(&mut self) {
        self.mark_presets();
        self.mark_reachable();
        self.sweep_strings();
        self.sweep_objects();
    }

    fn mark_presets(&mut self) {
        self.mark_object(self.preset_identifiers.init);
        self.mark_object(self.preset_identifiers.get);
        self.mark_object(self.preset_identifiers.set);
    }

    fn mark_reachable(&mut self) {
        while let Some(obj) = self.reachable_refs.pop() {
            obj.mark(self);
        }
    }

    fn sweep_strings(&mut self) {
        self.strings.retain(|_, &mut obj_ptr| unsafe { (*obj_ptr).header.marked });
    }

    fn sweep_objects(&mut self) {
        for i in (0..self.refs.len()).rev() {
            let obj = &self.refs[i];
            unsafe {
                if (*obj.as_header_ptr()).marked {
                    (*obj.as_header_ptr()).marked = false;
                } else {
                    self.free(i);
                }
            }
        }
    }

    fn free(&mut self, idx: usize) {
        let obj = self.refs[idx];
        let block = obj.as_header_ptr() as *mut u8;
        let (accounted, layout_size) = obj.free();
        self.bytes_allocated -= accounted;
        // Retain the block for reuse rather than handing it back to the system allocator.
        self.free_lists.entry(layout_size).or_default().push(block);
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
            let (_, layout_size) = obj.free();
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
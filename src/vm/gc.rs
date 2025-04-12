use std::mem;

use fnv::FnvHashMap;

use super::objects::{ObjString, Object};

pub trait GcTraceable {
    fn fmt(&self) -> String;
    fn mark(&self, gc: &mut Gc);
    fn size(&self) -> usize;
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
    pub bytes_allocated: usize,
    next_gc: usize,
    pub preset_identifiers: PresetIdentifiers
}

impl Gc {
    pub fn new() -> Gc {
        let mut gc = Gc {
            refs: Vec::new(),
            strings: FnvHashMap::default(),
            reachable_refs: Vec::new(),
            bytes_allocated: 0,
            next_gc: 1024 * 1024,
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
        self.bytes_allocated += obj.size();
        let obj_ptr = Box::into_raw(Box::new(obj));
        let obj = obj_ptr.into();
        self.refs.push(obj);
        obj_ptr
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
        self.bytes_allocated -= self.refs[idx].free();
        self.refs.swap_remove(idx);
        // println!("gc:free: {}", obj.unwrap().data.fmt(self));
    }

    pub fn should_collect(&self) -> bool {
        self.bytes_allocated > self.next_gc
    }
}
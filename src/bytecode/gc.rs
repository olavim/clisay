use std::collections::HashMap;
use std::mem;

use super::objects::{ObjString, Object};

pub trait GcTraceable {
    fn fmt(&self, gc: &Gc) -> String;
    fn mark_refs(&self, gc: &mut Gc);
    fn size(&self) -> usize;
}

impl GcTraceable for String {
    fn fmt(&self, _gc: &Gc) -> String {
        format!("\"{}\"", self)
    }

    fn mark_refs(&self, _gc: &mut Gc) {}

    fn size(&self) -> usize {
        mem::size_of::<String>() + self.capacity()
    }
}

pub struct Gc {
    refs: Vec<Object>,
    strings: HashMap<String, *mut ObjString>,
    reachable_refs: Vec<Object>,
    pub bytes_allocated: usize,
    next_gc: usize
}

impl Gc {
    pub fn new() -> Gc {
        Gc { 
            refs: Vec::new(),
            strings: HashMap::new(),
            reachable_refs: Vec::new(),
            bytes_allocated: 0,
            next_gc: 1024 * 1024
        }
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
            if !(*obj.header).marked {
                (*obj.header).marked = true;
                self.reachable_refs.push(obj);
            }
        }
    }

    pub fn collect(&mut self) {
        self.mark_reachable();
        self.sweep_strings();
        self.sweep_objects();
    }

    fn mark_reachable(&mut self) {
        while let Some(obj) = self.reachable_refs.pop() {
            unsafe { (*obj.as_traceable()).mark_refs(self); }
        }
    }

    fn sweep_strings(&mut self) {
        self.strings.retain(|_, &mut obj_ptr| unsafe { (*obj_ptr).header.marked });
    }

    fn sweep_objects(&mut self) {
        for i in (0..self.refs.len()).rev() {
            let obj = &self.refs[i];
            unsafe {
                if (*obj.header).marked {
                    (*obj.header).marked = false;
                } else {
                    self.free(i);
                }
            }
        }
    }

    fn free(&mut self, idx: usize) {
        let obj = self.refs[idx].as_traceable();
        self.bytes_allocated -= unsafe { (*obj).size() };
        let _ = unsafe { Box::from_raw(obj) };
        self.refs.swap_remove(idx);
        // println!("gc:free: {}", obj.unwrap().data.fmt(self));
    }

    pub fn should_collect(&self) -> bool {
        self.bytes_allocated > self.next_gc
    }

    // pub fn get<T: GcTraceable + 'static>(&self, gc_ref: &GcRef<T>) -> &T {
    //     unsafe {
    //         let gc_obj = self.refs[gc_ref.index].as_ref().unwrap_unchecked().data.as_ref()
    //             as *const dyn GcTraceable
    //             as *const T;
    //         return &*gc_obj;
    //     }
    // }

    // pub fn get_mut<T: GcTraceable + 'static>(&mut self, gc_ref: &GcRef<T>) -> &mut T {
    //     unsafe {
    //         let gc_obj = self.refs[gc_ref.index].as_mut().unwrap_unchecked().data.as_mut()
    //             as *mut dyn GcTraceable
    //             as *mut T;
    //         return &mut *gc_obj;
    //     }
    // }
}
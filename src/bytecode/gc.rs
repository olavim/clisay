use std::{any::Any, collections::HashMap, fmt, hash, marker::PhantomData, mem};

pub trait GcTraceable {
    fn fmt(&self, f: &mut fmt::Formatter, gc: &Gc) -> fmt::Result;
    fn mark_refs(&self, gc: &mut Gc);
    fn size(&self) -> usize;
    fn as_any(&self) -> &dyn Any;
    fn as_any_mut(&mut self) -> &mut dyn Any;
}

impl GcTraceable for String {
    fn fmt(&self, f: &mut fmt::Formatter, _gc: &Gc) -> fmt::Result {
        write!(f, "\"{self}\"")
    }

    fn mark_refs(&self, _gc: &mut Gc) {}

    fn size(&self) -> usize {
        mem::size_of::<String>() + self.capacity()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

pub struct GcObjFormatter<'a, T: GcTraceable> {
    value: &'a T,
    gc: &'a Gc
}

impl<'a, T: GcTraceable> GcObjFormatter<'a, T> {
    pub fn new(value: &'a T, gc: &'a Gc) -> GcObjFormatter<'a, T> {
        return GcObjFormatter { value, gc };
    }
}

impl<'a, T: GcTraceable> fmt::Display for GcObjFormatter<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.value.fmt(f, self.gc)
    }
}

pub struct GcRef<T: GcTraceable> {
    index: usize,
    _marker: PhantomData<T>,
}

pub struct GcObj {
    data: Box<dyn GcTraceable>,
    marked: bool
}

impl GcObj {
    pub fn new(data: Box<dyn GcTraceable>) -> GcObj {
        return GcObj { data, marked: false };
    }
}

impl<T: GcTraceable> nohash_hasher::IsEnabled for GcRef<T> {}
impl<T: GcTraceable> Copy for GcRef<T> {}
impl<T: GcTraceable> Clone for GcRef<T> {
    #[inline]
    fn clone(&self) -> GcRef<T> {
        *self
    }
}
impl<T: GcTraceable> hash::Hash for GcRef<T> {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.index.hash(state)
    }
}

impl<T: GcTraceable> PartialEq for GcRef<T> {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index
    }
}

impl Eq for GcRef<String> {}

pub struct Gc {
    refs: Vec<Option<GcObj>>,
    strings: HashMap<String, GcRef<String>>,
    reachable_refs: Vec<usize>,
    freed_slots: Vec<usize>,
    pub bytes_allocated: usize,
    next_gc: usize
}

impl Gc {
    pub fn new() -> Gc {
        Gc { 
            refs: Vec::new(),
            strings: HashMap::new(),
            reachable_refs: Vec::new(),
            freed_slots: Vec::new(),
            bytes_allocated: 0,
            next_gc: 1024 * 1024
        }
    }

    pub fn alloc<T: GcTraceable + 'static>(&mut self, obj: T) -> GcRef<T> {
        self.bytes_allocated += obj.size();
        let obj = Some(GcObj::new(Box::new(obj)));

        if let Some(index) = self.freed_slots.pop() {
            self.refs[index] = obj;
            return GcRef { index, _marker: PhantomData };
        }

        self.refs.push(obj);
        return GcRef { index: self.refs.len() - 1, _marker: PhantomData };
    }

    pub fn intern(&mut self, name: impl Into<String>) -> GcRef<String> {
        let name = name.into();
        if let Some(&gc_ref) = self.strings.get(&name) {
            return gc_ref
        }

        let gc_ref = self.alloc(name.clone());
        self.strings.insert(name, gc_ref);
        gc_ref
    }

    pub fn mark_object<T: GcTraceable>(&mut self, gc_ref: &GcRef<T>) {
        if let Some(gc_obj) = self.refs[gc_ref.index].as_mut() {
            if gc_obj.marked {
                return;
            }

            gc_obj.marked = true;
            self.reachable_refs.push(gc_ref.index);
        }
    }

    pub fn collect(&mut self) {
        while let Some(idx) = self.reachable_refs.pop() {
            let gc_obj = self.refs[idx].take();
            gc_obj.as_ref().unwrap().data.mark_refs(self);
            self.refs[idx] = gc_obj;
        }

        self.strings.retain(|_, gc_ref| self.refs[gc_ref.index].as_ref().unwrap().marked);

        for i in 0..self.refs.len() {
            if let Some(gc_obj) = self.refs[i].as_mut() {
                if gc_obj.marked {
                    gc_obj.marked = false;
                } else {
                    self.bytes_allocated -= gc_obj.data.size();
                    self.refs[i].take();
                    self.freed_slots.push(i);
                }
            }
        }
    }

    pub fn should_collect(&self) -> bool {
        self.bytes_allocated > self.next_gc
    }

    pub fn get<T: GcTraceable + 'static>(&self, gc_ref: &GcRef<T>) -> &T {
        let gc_obj = self.refs[gc_ref.index].as_ref().unwrap().data.as_ref();
        return gc_obj.as_any().downcast_ref().unwrap();
    }

    pub fn get_mut<T: GcTraceable + 'static>(&mut self, gc_ref: &GcRef<T>) -> &mut T {
        let gc_obj = self.refs[gc_ref.index].as_mut().unwrap().data.as_mut();
        return gc_obj.as_any_mut().downcast_mut().unwrap();
    }
}
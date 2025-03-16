use std::{any::Any, collections::HashMap, fmt, hash, marker::PhantomData};

pub trait GcObj {
    fn fmt(&self, f: &mut fmt::Formatter, gc: &Gc) -> fmt::Result;
    fn as_any(&self) -> &dyn Any;
    fn as_any_mut(&mut self) -> &mut dyn Any;
}

impl GcObj for String {
    fn fmt(&self, f: &mut fmt::Formatter, _gc: &Gc) -> fmt::Result {
        write!(f, "{self}")
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

pub struct GcObjFormatter<'a, T: GcObj> {
    value: &'a T,
    gc: &'a Gc
}

impl<'a, T: GcObj> GcObjFormatter<'a, T> {
    pub fn new(value: &'a T, gc: &'a Gc) -> GcObjFormatter<'a, T> {
        return GcObjFormatter { value, gc };
    }
}

impl<'a, T: GcObj> fmt::Display for GcObjFormatter<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.value.fmt(f, self.gc)
    }
}

pub struct GcRef<T: GcObj> {
    index: usize,
    _marker: PhantomData<T>,
}

impl<T: GcObj> nohash_hasher::IsEnabled for GcRef<T> {}
impl<T: GcObj> Copy for GcRef<T> {}
impl<T: GcObj> Clone for GcRef<T> {
    #[inline]
    fn clone(&self) -> GcRef<T> {
        *self
    }
}
impl<T: GcObj> hash::Hash for GcRef<T> {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.index.hash(state)
    }
}

impl<T: GcObj> PartialEq for GcRef<T> {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index
    }
}

impl Eq for GcRef<String> {}

pub struct Gc {
    refs: Vec<Option<Box<dyn GcObj>>>,
    strings: HashMap<String, GcRef<String>>
}

impl Gc {
    pub fn new() -> Gc {
        return Gc { refs: Vec::new(), strings: HashMap::new() };
    }

    pub fn alloc<T: GcObj + 'static>(&mut self, obj: T) -> GcRef<T> {
        let index = self.refs.len();
        self.refs.push(Some(Box::new(obj)));
        return GcRef { index, _marker: PhantomData };
    }

    pub fn intern(&mut self, name: String) -> GcRef<String> {
        if let Some(&gc_ref) = self.strings.get(&name) {
            return gc_ref
        }

        let gc_ref = self.alloc(name.clone());
        self.strings.insert(name, gc_ref);
        gc_ref
    }

    pub fn get<T: GcObj + 'static>(&self, gc_ref: &GcRef<T>) -> &T {
        let gc_obj = self.refs[gc_ref.index].as_ref().unwrap().as_ref();
        return gc_obj.as_any().downcast_ref().unwrap();
    }

    pub fn get_mut<T: GcObj + 'static>(&mut self, gc_ref: &GcRef<T>) -> &mut T {
        let gc_obj = self.refs[gc_ref.index].as_mut().unwrap().as_mut();
        return gc_obj.as_any_mut().downcast_mut().unwrap();
    }
}
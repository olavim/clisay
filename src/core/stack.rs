use std::marker::PhantomData;

pub struct Stack<T, const N: usize> {
    values: Vec<T>,
    top: *mut T,
    bottom: *mut T,
    /// One past the last slot.
    end: *mut T
}

impl<'a, T: Copy, const N: usize> Stack<T, N> {
    pub fn new() -> Self {
        Self {
            values: vec![unsafe { std::mem::zeroed() }; N],
            top: std::ptr::null_mut(),
            bottom: std::ptr::null_mut(),
            end: std::ptr::null_mut()
        }
    }

    pub fn init(&mut self) {
        self.top = self.values.as_mut_ptr();
        self.bottom = self.values.as_mut_ptr();
        self.end = unsafe { self.values.as_mut_ptr().add(N) };
    }

    #[inline]
    pub fn top(&self) -> *mut T {
        self.top
    }

    #[inline]
    pub fn set_top(&mut self, top: *mut T) {
        self.top = top;
    }

    #[inline]
    pub fn offset(&self, offset: usize) -> *mut T {
        unsafe { self.top.sub(offset + 1) }
    }

    #[inline]
    pub fn push(&mut self, value: T) {
        unsafe {
            *self.top = value;
            self.top = self.top.add(1);
        }
    }

    #[inline]
    pub fn pop(&mut self) -> T {
        unsafe {
            self.top = self.top.sub(1);
            *self.top
        }
    }

    #[inline]
    pub fn pop_slice(&mut self, count: usize) -> Vec<T> {
        unsafe {
            self.top = self.top.sub(count);
            let slice = std::slice::from_raw_parts(self.top, count);
            Vec::from(slice)
        }
    }

    #[inline]
    pub fn truncate(&mut self, count: usize) {
        unsafe {
            self.top = self.top.sub(count);
        }
    }

    #[inline]
    pub fn peek(&self, offset: usize) -> T {
        unsafe { *self.top.sub(offset + 1) }
    }

    #[inline]
    pub fn set(&mut self, offset: usize, value: T) -> *mut T {
        unsafe {
            let ptr = self.top.sub(offset + 1);
            *ptr = value;
            ptr
        }
    }

    pub fn iter(&'a self) -> StackIter<'a, T, N> {
        StackIter {
            front: self.bottom,
            back: self.top,
            _marker: PhantomData
        }
    }

    #[inline]
    pub fn len(&self) -> usize {
        (self.top as isize - self.bottom as isize) as usize / std::mem::size_of::<T>()
    }

    #[inline]
    pub fn is_full(&self) -> bool {
        self.top >= self.end
    }
}

pub struct StackIter<'a, T: Copy, const N: usize> {
    front: *mut T,
    back: *mut T,
    _marker: PhantomData<&'a T>,
}

impl<'a, T: Copy, const N: usize> Iterator for StackIter<'a, T, N> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        if self.front >= self.back {
            return None;
        }

        let value = unsafe { *self.front };
        self.front = unsafe { self.front.add(1) };
        Some(value)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = (self.back as usize - self.front as usize) / std::mem::size_of::<T>();
        (len, Some(len))
    }
}

impl<'a, T: Copy, const N: usize> DoubleEndedIterator for StackIter<'a, T, N> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.back <= self.front {
            return None;
        }
        self.back = unsafe { self.back.sub(1) };
        Some(unsafe { *self.back })
    }
}

impl<'a, T: Copy, const N: usize> ExactSizeIterator for StackIter<'a, T, N> {}

pub struct CachedStack<T, const N: usize> {
    stack: Stack<T, N>,
    top: *mut T
}

impl<'a, T: Copy, const N: usize> CachedStack<T, N> {
    pub fn new() -> Self {
        Self {
            stack: Stack::new(),
            top: std::ptr::null_mut()
        }
    }

    pub fn init(&mut self) {
        self.stack.init();
    }

    #[inline]
    pub fn push(&mut self, value: T) {
        self.stack.push(value);
        self.top = unsafe { self.stack.top.sub(1) };
    }

    #[inline]
    pub fn pop(&mut self) -> T {
        let value = unsafe { *self.top };
        self.stack.truncate(1);
        self.top = unsafe { self.stack.top.sub(1) };
        value
    }

    #[inline]
    pub fn top(&self) -> *mut T {
        self.top
    }

    #[inline]
    pub fn set_top(&mut self, top: *mut T) {
        self.stack.set_top(top);
        self.top = unsafe { self.stack.top.sub(1) };
    }

    #[inline]
    pub fn top_ptr(&self) -> *mut T {
        self.stack.top
    }

    #[inline]
    pub fn is_full(&self) -> bool {
        self.stack.is_full()
    }

    #[inline]
    pub fn iter(&'a self) -> StackIter<'a, T, N> {
        self.stack.iter()
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.stack.len()
    }
}

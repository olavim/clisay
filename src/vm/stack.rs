
pub struct Stack<T, const N: usize> {
    values: [T; N],
    top: *mut T,
    bottom: *mut T
}

impl<'a, T: Copy, const N: usize> Stack<T, N> {
    pub fn new() -> Self {
        Self {
            values: [unsafe { std::mem::zeroed() }; N],
            top: std::ptr::null_mut(),
            bottom: std::ptr::null_mut()
        }
    }

    pub fn init(&mut self) {
        self.top = self.values.as_mut_ptr();
        self.bottom = self.values.as_mut_ptr();
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
            stack: self,
            curr: self.bottom
        }
    }
    
    #[inline]
    pub fn len(&self) -> usize {
        // unsafe { self.top.offset_from(self.bottom) as usize }
        (self.top as isize - self.bottom as isize) as usize / std::mem::size_of::<T>()
    }
}

pub struct StackIter<'a, T: Copy, const N: usize> {
    stack: &'a Stack<T, N>,
    curr: *mut T,
}

impl<'a, T: Copy, const N: usize> Iterator for StackIter<'a, T, N> {
    type Item = T;
    
    fn next(&mut self) -> Option<Self::Item> {
        if self.curr >= self.stack.top {
            return None;
        }

        let value = unsafe { *self.curr };
        self.curr = unsafe { self.curr.add(1) };
        Some(value)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let stack_len = self.stack.len();
        let offset = unsafe { self.curr.offset_from(self.stack.bottom) as usize };
        let len = stack_len - offset;
        (len, Some(len))
    }
}

impl<'a, T: Copy, const N: usize> DoubleEndedIterator for StackIter<'a, T, N> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.curr <= self.stack.bottom {
            return None;
        }
        self.curr = unsafe { self.curr.sub(1) };
        Some(unsafe { *self.curr })
    }
}

impl<'a, T: Copy, const N: usize> ExactSizeIterator for StackIter<'a, T, N> {}

pub struct CachedStack<T, const N: usize> {
    stack: Stack<T, N>,
    pub top: T
}

impl<'a, T: Copy, const N: usize> CachedStack<T, N> {
    pub fn new() -> Self {
        Self {
            stack: Stack::new(),
            top: unsafe { std::mem::zeroed() }
        }
    }

    pub fn init(&mut self) {
        self.stack.init();
    }

    pub fn push(&mut self, value: T) {
        self.top = value;
        self.stack.push(value);
    }

    pub fn pop(&mut self) -> T {
        self.stack.truncate(1);
        let value = self.top;
        self.top = self.stack.peek(0);
        value
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

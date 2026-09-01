use std::marker::PhantomData;

/// Slots allocated past `end`.
const SLACK: usize = 256;

/// One bit per slot, packed into words.
struct SlotBits {
    words: Vec<u64>,
}

impl SlotBits {
    fn new(slots: usize) -> SlotBits {
        SlotBits { words: vec![0; slots.div_ceil(64)] }
    }

    #[inline]
    fn set(&mut self, slot: usize) {
        debug_assert!(slot >> 6 < self.words.len(), "slot past the bits built for it");
        unsafe { *self.words.get_unchecked_mut(slot >> 6) |= 1u64 << (slot & 63) };
    }

    #[inline]
    fn clear(&mut self, slot: usize) {
        debug_assert!(slot >> 6 < self.words.len(), "slot past the bits built for it");
        unsafe { *self.words.get_unchecked_mut(slot >> 6) &= !(1u64 << (slot & 63)) };
    }

    #[inline]
    fn get(&self, slot: usize) -> bool {
        debug_assert!(slot >> 6 < self.words.len(), "slot past the bits built for it");
        unsafe { *self.words.get_unchecked(slot >> 6) & (1u64 << (slot & 63)) != 0 }
    }

    /// Clears the bits of the slots in `[from, to)`.
    fn clear_range(&mut self, from: usize, to: usize) {
        if from >= to {
            return;
        }
        debug_assert!((to - 1) >> 6 < self.words.len(), "range past the bits built for it");
        let ((first, low), (last, high)) = ((from >> 6, from & 63), (to >> 6, to & 63));
        // `!0 << low` is the bits from `low` up; `(1 << high) - 1` is the bits below `high`.
        if first == last {
            unsafe { *self.words.get_unchecked_mut(first) &= !((!0u64 << low) & ((1u64 << high) - 1)) };
            return;
        }
        unsafe { *self.words.get_unchecked_mut(first) &= !(!0u64 << low) };
        for word in first + 1..last {
            unsafe { *self.words.get_unchecked_mut(word) = 0 };
        }
        unsafe { *self.words.get_unchecked_mut(last) &= !((1u64 << high) - 1) };
    }
}

pub struct Stack<T, const N: usize> {
    values: Vec<T>,
    top: *mut T,
    bottom: *mut T,
    /// One past the last slot a program may use.
    end: *mut T,
    borrowed: SlotBits,
    borrowed_end: *mut T,
    borrow_origins: Vec<*mut T>,
}

impl<'a, T: Copy, const N: usize> Stack<T, N> {
    pub fn new() -> Self {
        Self {
            values: vec![unsafe { std::mem::zeroed() }; N + SLACK],
            top: std::ptr::null_mut(),
            bottom: std::ptr::null_mut(),
            end: std::ptr::null_mut(),
            borrowed: SlotBits::new(N + SLACK),
            borrowed_end: std::ptr::null_mut(),
            borrow_origins: vec![std::ptr::null_mut(); N + SLACK],
        }
    }

    pub fn init(&mut self) {
        self.top = self.values.as_mut_ptr();
        self.bottom = self.values.as_mut_ptr();
        self.end = unsafe { self.values.as_mut_ptr().add(N) };
        self.borrowed_end = self.bottom;
    }

    #[inline]
    fn slot_index(&self, at: *mut T) -> usize {
        (at as usize - self.bottom as usize) / std::mem::size_of::<T>()
    }

    #[inline]
    pub fn borrowed_end(&self) -> *mut T {
        self.borrowed_end
    }

    #[inline]
    pub fn mark_borrowed(&mut self, at: *mut T, origin: *mut T) {
        let after = unsafe { at.add(1) };
        if after > self.borrowed_end {
            // These slots are visible again, and what they held has gone, so their marks go too.
            self.clear_borrowed_between(self.borrowed_end, at);
            self.borrowed_end = after;
        }
        let slot = self.slot_index(at);
        self.borrowed.set(slot);
        unsafe { *self.borrow_origins.get_unchecked_mut(slot) = origin };
    }

    #[inline]
    pub fn borrow_outlives(&self, at: *mut T, frame_start: *mut T) -> bool {
        self.is_borrowed(at) && unsafe { *self.borrow_origins.get_unchecked(self.slot_index(at)) < frame_start }
    }

    #[inline]
    pub fn borrow_origin(&self, at: *mut T) -> *mut T {
        unsafe { *self.borrow_origins.get_unchecked(self.slot_index(at)) }
    }

    #[inline]
    pub fn clear_borrowed(&mut self, at: *mut T) {
        if at >= self.borrowed_end {
            return;
        }
        let slot = self.slot_index(at);
        self.borrowed.clear(slot);
    }

    #[inline]
    pub fn is_borrowed(&self, at: *mut T) -> bool {
        if at >= self.borrowed_end {
            return false;
        }
        self.borrowed.get(self.slot_index(at))
    }

    #[inline]
    pub fn prune_borrowed(&mut self, top: *mut T) {
        if top < self.borrowed_end {
            self.borrowed_end = top;
        }
    }

    /// Clears the marks on the slots in `[from, to)`.
    fn clear_borrowed_between(&mut self, from: *mut T, to: *mut T) {
        let (from, to) = (self.slot_index(from), self.slot_index(to));
        self.borrowed.clear_range(from, to);
    }

    /// One past the last slot a program may use.
    #[inline]
    pub fn end(&self) -> *mut T {
        self.end
    }

    #[inline]
    pub fn top(&self) -> *mut T {
        self.top
    }

    /// Only `verify_roots` asks, and that runs in debug builds alone.
    #[cfg(debug_assertions)]
    #[inline]
    pub fn bottom(&self) -> *mut T {
        self.bottom
    }

    #[inline]
    pub fn set_top(&mut self, top: *mut T) {
        self.prune_borrowed(top);
        self.top = top;
    }

    #[inline]
    pub fn offset(&self, offset: usize) -> *mut T {
        unsafe { self.top.sub(offset + 1) }
    }

    /// Writes without checking, because this is the hottest write in the VM. A push past `end` is
    /// over budget but still in bounds. Running past the slack as well is what this catches.
    #[inline]
    pub fn push(&mut self, value: T) {
        debug_assert!(self.top < unsafe { self.end.add(SLACK) }, "stack overflow: push past the slack");
        unsafe {
            *self.top = value;
            self.top = self.top.add(1);
        }
    }

    /// Whether `count` more values fit.
    #[inline]
    pub fn has_room(&self, count: usize) -> bool {
        (self.end as usize).saturating_sub(self.top as usize) / std::mem::size_of::<T>() >= count
    }

    #[inline]
    pub fn pop(&mut self) -> T {
        unsafe {
            self.top = self.top.sub(1);
            self.prune_borrowed(self.top);
            *self.top
        }
    }

    #[inline]
    pub fn pop_slice(&mut self, count: usize) -> Vec<T> {
        unsafe {
            self.top = self.top.sub(count);
            self.prune_borrowed(self.top);
            let slice = std::slice::from_raw_parts(self.top, count);
            Vec::from(slice)
        }
    }

    #[inline]
    pub fn truncate(&mut self, count: usize) {
        unsafe {
            self.top = self.top.sub(count);
        }
        self.prune_borrowed(self.top);
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
}

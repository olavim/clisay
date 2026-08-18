use super::*;

macro_rules! check_arity {
    ($vm:expr, $arg_count:expr, $arity:expr, $func_name:expr) => {
        if $arg_count != $arity as usize {
            let name = unsafe { &(*$func_name).value };
            return $vm.error(format!("{} expects {} arguments, but was called with {}", name, $arity, $arg_count));
        }
    }
}

/// Whether the element is free of any container but the one the path started from. Both the fast
/// answer and the one after a collection ask this, so it is named once.
fn no_other_write_owner(value: Value, root: Value) -> bool {
    match objects::recorded_holder(value) {
        objects::RecordedHolder::Nobody => true,
        objects::RecordedHolder::Container => objects::write_ownership_reaches(value, root),
    }
}

/// What a scope exit does with one write-ownership claim.
enum WriteOwnershipDisposition {
    /// Write-ownership was dropped, and whatever release it needed has already happened.
    Dropped,
    /// Write-ownership stays, under a holder that outlives the slot it named.
    Held(WriteOwnershipHolder),
}

/// The value an exit carries out of a frame, and the slot it lands in.
#[derive(Clone, Copy)]
pub struct Leaving {
    value: Value,
    lands_at: *mut Value,
}

impl Leaving {
    pub fn new(value: Value, lands_at: *mut Value) -> Leaving {
        Leaving { value, lands_at }
    }

    /// The slot this exit lands `value` in, if `value` is what it carries out.
    fn slot_for(&self, value: Value) -> Option<*mut Value> {
        (self.value == value).then_some(self.lands_at)
    }
}

/// Whether two records name the same holder.
fn same_holder(a: WriteOwnershipHolder, b: WriteOwnershipHolder) -> bool {
    match (a, b) {
        (WriteOwnershipHolder::Retired, _) | (_, WriteOwnershipHolder::Retired) => false,
        (WriteOwnershipHolder::Name(x), WriteOwnershipHolder::Name(y)) => x == y,
        (WriteOwnershipHolder::Container(x), WriteOwnershipHolder::Container(y)) => x == y,
        (WriteOwnershipHolder::Name(addr), WriteOwnershipHolder::Container(c))
        | (WriteOwnershipHolder::Container(c), WriteOwnershipHolder::Name(addr)) => unsafe { *addr == c },
    }
}

impl Vm {
    /// The stack-overflow error, kept off the hot call path. `#[cold]` +
    /// `#[inline(never)]` so the bulky error/trace formatting isn't inlined into
    /// the dispatch loop's `CALL` arm (which would bloat the hot loop body).
    #[cold]
    #[inline(never)]
    pub(super) fn stack_overflow(&mut self) -> anyhow::Error {
        self.error("Stack overflow").unwrap_err()
    }

    pub(super) fn push_frame(&mut self, closure: *mut ObjClosure, stack_start: *mut Value, ip_start: usize, seal: bool) -> Result<(), anyhow::Error> {
        if self.frames.is_full() {
            return Err(self.stack_overflow());
        }
        self.frames.push(CallFrame {
            closure,
            return_ip: self.ip,
            stack_start,
            seal,
            write_depth: self.write_ownerships.len(),
            borrow_depth: self.borrows.len(),
        });
        self.ip = unsafe { self.chunk.code.as_ptr().offset(ip_start as isize) };
        Ok(())
    }

    pub(super) fn op_call_mut(&mut self) -> Result<(), anyhow::Error> {
        let arg_count = self.read_next() as usize;
        let value = self.stack.peek(arg_count);
        self.call(arg_count, value, false)
    }

    /// `seal` rides onto the pushed frame so a factory's `RETURN_FAC` knows whether to freeze.
    pub(crate) fn call(&mut self, arg_count: usize, value: Value, seal: bool) -> Result<(), anyhow::Error> {
        if !value.is_callable() {
            return self.error(format!("{} is not callable", value.fmt()));
        }

        let object = value.as_object();
        let tag = object.tag();

        match tag {
            objects::TAG_CLOSURE => self.call_closure(arg_count, object.as_closure_ptr(), seal),
            objects::TAG_NATIVE_FUNCTION => self.call_native(arg_count, object.as_native_function_ptr()),
            objects::TAG_BOUND_METHOD => self.call_bound_method(arg_count, object.as_bound_method_ptr(), seal),
            objects::TAG_TYPE => self.call_type(arg_count, object.as_type_ptr(), seal),
            objects::TAG_FUNCTION => self.error(format!("{} is not callable", value.fmt())),
            _ => unsafe { std::hint::unreachable_unchecked() }
        }
    }

    pub(super) fn op_assert_immutable(&mut self) -> Result<(), anyhow::Error> {
        let forced = self.at_elided_site();
        let value = self.stack.peek(0);
        if !objects::is_mutable_container(value) {
            return Ok(());
        }
        if forced {
            return self.refuted_elision("a value proven immutable is mutable");
        }
        let position = self.get_source_position().clone();
        let label = format!("`{}` is mutable", position.snippet());
        self.raise(Diagnostic::new(objects::MUTABLE_IN_IMMUTABLE, position)
            .with_label(label)
            .with_help("an immutable value is immutable all the way down; freeze this value, or mark the construction `mut`"))
    }

    fn assert_no_other_write_owner(&mut self, value: Value, root: Value) -> Result<(), anyhow::Error> {
        if no_other_write_owner(value, root) {
            return Ok(());
        }
        // An owner nothing reaches any more is nobody, and only the collector knows which those
        // are. Ask it here rather than let incidental gc timing decide the answer.
        self.start_gc();
        if no_other_write_owner(value, root) {
            let site = self.code_index_at(self.ip);
            self.write_barrier_missed_sites.insert(site);
            self.write_barrier_traced_missed += 1;
            return Ok(());
        }
        self.write_barrier_traced_refused += 1;
        Err(self.wrote_given_element_error())
    }

    /// The trap for a write reaching an element through something that does not hold it.
    #[cold]
    #[inline(never)]
    fn wrote_given_element_error(&mut self) -> anyhow::Error {
        let here = self.get_source_position().clone();
        self.raise(Diagnostic::new(objects::WROTE_GIVEN_ELEMENT, here.clone())
            .with_label(format!("`{}` is written here", here.snippet()))
            .with_help("an element has one write-owner; reading it here is still fine, but to write it, go through the container it was stored into"))
            .unwrap_err()
    }

    #[cold]
    #[inline(never)]
    fn wrote_transferred_element_error(&mut self) -> anyhow::Error {
        let here = self.get_source_position().clone();
        self.raise(Diagnostic::new(objects::WROTE_TRANSFERRED_ELEMENT, here.clone())
            .with_label(format!("`{}` is written here", here.snippet()))
            .with_help("write-ownership was given away and nothing handed it back; reading is still fine"))
            .unwrap_err()
    }

    /// Whether calling this writes its receiver, which is what makes a call ask the
    /// write-ownership question.
    pub(super) fn callable_writes_receiver(&self, callable: Value) -> bool {
        if !callable.is_object() || callable.as_object().tag() != objects::TAG_BOUND_METHOD {
            return false;
        }
        let method = unsafe { &*callable.as_object().as_bound_method_ptr() }.method;
        match method.tag() {
            objects::TAG_NATIVE_FUNCTION => unsafe { &*method.as_native_function_ptr() }.mutates,
            objects::TAG_CLOSURE => unsafe { &*method.as_closure_ptr() }.mut_receiver,
            objects::TAG_FUNCTION => unsafe { &*method.as_function_ptr() }.mut_receiver,
            _ => false,
        }
    }

    /// Consumes the root a store writes through.
    pub(super) fn take_write_root(&mut self, kind: u8, operand: u8) -> WriteRoot {
        match kind {
            ir::WRITE_ROOT_LOCAL => WriteRoot::Named(self.slot_addr(operand as usize), false),
            ir::WRITE_ROOT_RECEIVER => WriteRoot::Named(self.slot_addr(operand as usize), true),
            ir::WRITE_ROOT_UPVALUE => {
                WriteRoot::Named(unsafe { (*self.get_upvalue(operand as usize)).location }, false)
            },
            ir::WRITE_ROOT_RECEIVER_UP => {
                WriteRoot::Named(unsafe { (*self.get_upvalue(operand as usize)).location }, true)
            },
            // An empty stash means the store was assembled without the push that feeds it.
            // NoRoot then makes the store ask who holds the target, rather than trust a root
            // it never received.
            ir::WRITE_ROOT_STASH => {
                debug_assert!(!self.root_stash.is_empty(), "a store named a stashed root and nothing stashed one");
                self.root_stash.pop().map_or(WriteRoot::NoRoot, WriteRoot::Stashed)
            },
            _ => WriteRoot::NoRoot,
        }
    }

    /// Refuses a write if something other than the target's own root already writes the target.
    /// A target nothing can write is not an error here.
    pub(super) fn ensure_writable(&mut self, target: Value, root: WriteRoot) -> Result<(), anyhow::Error> {
        self.settle_write_ownership(target, root)?;
        Ok(())
    }

    /// The same refusal occurs, and then the write's own root becomes the writer. `invoke_method`
    /// uses `ensure_writable` instead because the writes in the method body name their own writers.
    pub(super) fn claim_write_ownership_through(&mut self, target: Value, root: WriteRoot) -> Result<(), anyhow::Error> {
        if let Some(holder) = self.settle_write_ownership(target, root)? {
            self.record_write_ownership(target, holder, WriteOwnershipSource::Bound);
        }
        Ok(())
    }

    /// Settles the one-writer rule. It returns the holder this write makes the writer, or None if
    /// no writer is made.
    fn settle_write_ownership(&mut self, target: Value, root: WriteRoot) -> Result<Option<WriteOwnershipHolder>, anyhow::Error> {
        // Only a mutable container has write-ownership. A program that writes anything else is
        // refused by the store that asked.
        if !objects::is_mutable_container(target) {
            return Ok(None);
        }

        let (holder, root_value, root_is_receiver) = match root {
            WriteRoot::Named(addr, receiver) => (WriteOwnershipHolder::Name(addr), unsafe { *addr }, receiver),
            WriteRoot::Stashed(container) => (WriteOwnershipHolder::Container(container), container, false),
            // The write targets an unnamed value. There is no root to check and no name to become
            // the writer.
            WriteRoot::NoRoot => {
                self.refuse_unless_only_a_name_holds_it(target)?;
                return Ok(None);
            },
        };

        self.assert_no_other_write_owner(target, root_value)?;

        if self.is_borrowed_from_an_outer_write_owner(target) {
            return Ok(None);
        }

        // One answer settles both halves. Anything else holding the target must give way before
        // the write stands. A holder that already has the target keeps it.
        let already_holds_it = match self.write_owner_of(target) {
            Some(held) if same_holder(held, holder) => true,
            Some(_) => {
                self.refuse_unless_write_owner_is_gone(target)?;
                false
            },
            None => false,
        };

        // The write is valid. Writing through the root's name makes that name the writer. Writes
        // further down the path do not name a writer.
        match !root_is_receiver && target == root_value && !already_holds_it {
            true => Ok(Some(holder)),
            false => Ok(None),
        }
    }

    /// Rules for writing to unnamed values. A container holding the value counts as a second
    /// writer. A name holding the value does not.
    fn refuse_unless_only_a_name_holds_it(&mut self, target: Value) -> Result<(), anyhow::Error> {
        if !target.as_object().is_write_owned() {
            return Ok(());
        }
        if let Some(WriteOwnershipHolder::Name(addr)) = self.write_owner_of(target) {
            if unsafe { *addr } == target {
                return Ok(());
            }
        }
        self.refuse_unless_write_owner_is_gone(target)
    }

    /// Copies the top stack root onto the stash.
    pub(super) fn op_stash_root(&mut self) {
        self.root_stash.push(self.stack.peek(0));
    }

    /// The last question before refusing a write. A claim can name a slot holding something else
    /// now, or a container nothing reaches any more.
    fn refuse_unless_write_owner_is_gone(&mut self, value: Value) -> Result<(), anyhow::Error> {
        let Some(at) = self.write_ownership_index_of(value) else {
            return Ok(());
        };
        match self.write_ownerships[at].holder {
            WriteOwnershipHolder::Name(_) if self.name_write_owner_holds_another_value(at) => {
                self.drop_write_ownership(at);
                self.refuse_unless_write_owner_is_gone(value)
            },
            WriteOwnershipHolder::Name(_) => Err(self.second_writer_error(value)),
            WriteOwnershipHolder::Retired => Err(self.wrote_transferred_element_error()),
            WriteOwnershipHolder::Container(_) => {
                // Collecting here rather than on the next allocation is what keeps the answer the
                // same under `CLISAY_GC_STRESS`. The written value is on the stack, so it survives.
                self.start_gc();
                match self.write_owner_of(value) {
                    // Pruning removes a claim if the trace did not reach its container. Finding no
                    // holder means the holder is gone and the write is valid.
                    None => Ok(()),
                    Some(WriteOwnershipHolder::Container(_)) => Err(self.second_writer_error(value)),
                    Some(WriteOwnershipHolder::Name(_) | WriteOwnershipHolder::Retired) => unreachable!("a prune drops a container claim, it never hands one to a name or retires it"),
                }
            },
        }
    }

    /// Whether the running frame borrows this value from a write owner outside it. The lender is
    /// suspended, so its write-ownership does not refuse a write here and the borrowing name takes
    /// none of its own.
    fn is_borrowed_from_an_outer_write_owner(&self, target: Value) -> bool {
        if !target.as_object().is_borrowed() {
            return false;
        }
        if self.claimed_within_frame(target) {
            return false;
        }
        self.write_ownership_index_of(target)
            .is_some_and(|at| self.name_write_owner(at).is_some())
    }

    /// Whether the claim on `value` was made by the frame now running, rather than by something
    /// that reached the value before the call.
    fn claimed_within_frame(&self, value: Value) -> bool {
        let frame = self.frames.top();
        let depth = match frame.is_null() {
            true => 0,
            false => unsafe { (*frame).write_depth },
        };
        self.write_ownerships.iter().rposition(|held| held.value == value)
            .is_some_and(|at| at >= depth)
    }

    fn write_owner_of(&self, value: Value) -> Option<WriteOwnershipHolder> {
        self.write_ownership_index_of(value).map(|at| self.write_ownerships[at].holder)
    }

    fn write_ownership_index_of(&self, value: Value) -> Option<usize> {
        self.write_ownerships.iter().rposition(|held| held.value == value)
    }

    /// The slot a value's write owner is, when the write owner is a name on the value itself. A
    /// given write-ownership is held by the name of the container the value went into.
    fn name_write_owner(&self, at: usize) -> Option<*mut Value> {
        let held = self.write_ownerships[at];
        match held.holder {
            WriteOwnershipHolder::Name(addr) if held.how != WriteOwnershipSource::Given => Some(addr),
            _ => None,
        }
    }

    /// Whether the name that write-owns this value holds another value now.
    fn name_write_owner_holds_another_value(&self, at: usize) -> bool {
        self.name_write_owner(at)
            .is_some_and(|addr| unsafe { *addr } != self.write_ownerships[at].value)
    }

    /// Lets go of a write-ownership whose name holds another value now. The value is spoken for by
    /// nobody again, so the next write through a name takes it.
    fn drop_write_ownership(&mut self, at: usize) {
        let value = self.write_ownerships[at].value;
        self.release_write_ownership(value);
        self.write_ownerships.remove(at);
    }

    /// The opaque-call mode barrier. An argument the caller must keep alive may not be handed to a
    /// callee that consumes it, so this asserts the callee borrows the guarded parameter.
    pub(super) fn op_assert_no_retain(&mut self) -> Result<(), anyhow::Error> {
        let arg_count = self.read_next() as usize;
        let owed_idx = u16::from_le_bytes([self.read_next(), self.read_next()]);
        let count = self.read_next() as usize;
        let callee = self.stack.peek(arg_count);
        for _ in 0..count {
            let position = self.read_next() as usize;
            if self.callee_escapes(callee, position) {
                let name = self.get_source_position().snippet();
                return match self.owed_at(owed_idx, position) {
                    Some(owed) => self.error_labeled(
                        objects::retained_owed_value(owed),
                        format!("`{name}` owes '{owed}' but the callee retains it")),
                    None => self.error_labeled(
                        objects::RETAINED_BORROW,
                        format!("`{name}` is borrowed but the callee retains it")),
                };
            }
        }
        Ok(())
    }

    fn owed_at(&self, owed_idx: u16, position: usize) -> Option<&str> {
        self.chunk.owed_names[owed_idx as usize].iter()
            .find(|(p, _)| *p as usize == position)
            .map(|(_, name)| &**name)
    }

    /// A container taking the writer slot for an element it is given. The slot names the container
    /// rather than holding it yet, since a construction takes its elements before it is assigned.
    pub(super) fn op_transfer_write_ownership(&mut self) -> Result<(), anyhow::Error> {
        let slot = self.read_next() as usize;
        let holder = WriteOwnershipHolder::Name(self.slot_addr(slot));
        self.claim_write_ownership(holder, WriteOwnershipSource::Given)
    }

    /// A container taking the writer slot for an element it is given, for a container the body
    /// reaches as an upvalue. That one is built already, so the claim names the container itself.
    pub(super) fn op_transfer_write_ownership_up(&mut self) -> Result<(), anyhow::Error> {
        let idx = self.read_next() as usize;
        let container = unsafe { *(*self.get_upvalue(idx)).location };
        self.claim_write_ownership(WriteOwnershipHolder::Container(container), WriteOwnershipSource::Given)
    }

    /// A container taking the writer slot for an element it is given, where no binding names the
    /// container. It is still on the stack, `depth` below the element.
    pub(super) fn op_transfer_write_ownership_at(&mut self) -> Result<(), anyhow::Error> {
        let depth = self.read_next() as usize;
        let container = self.stack.peek(depth);
        self.claim_write_ownership(WriteOwnershipHolder::Container(container), WriteOwnershipSource::Given)
    }

    /// Records `holder` as the one name that may write the value on top.
    fn claim_write_ownership(&mut self, holder: WriteOwnershipHolder, how: WriteOwnershipSource) -> Result<(), anyhow::Error> {
        self.claim_write_ownership_of(self.stack.peek(0), holder, how)
    }

    /// Determines the writer for `value` and records it.
    fn claim_write_ownership_of(&mut self, value: Value, holder: WriteOwnershipHolder, how: WriteOwnershipSource) -> Result<(), anyhow::Error> {
        // Nothing can write an immutable value or a primitive.
        if !objects::is_mutable_container(value) {
            return Ok(());
        }
        if value.as_object().is_write_owned() {
            if self.holds_write_ownership(holder, value) {
                return Ok(());
            }
            // A parameter that retained its argument took the write-ownership and may hand it on,
            // so a store out of it continues the transfer rather than making a second writer.
            if self.took_write_ownership_by_retain(value) {
                self.blank_claims_on(value);
            } else {
                self.refuse_unless_write_owner_is_gone(value)?;
            }
        }
        self.record_write_ownership(value, holder, how);
        Ok(())
    }

    pub(super) fn hand_write_ownership_to_upvalue(&mut self, idx: usize, value: Value) -> Result<(), anyhow::Error> {
        if !self.borrow_claims.is_empty() {
            self.note_claimed_upvalue_store(idx, value)?;
        }
        if !self.took_write_ownership_by_retain(value) {
            return Ok(());
        }
        let holder = self.upvalue_holder(idx);
        let how = match holder {
            WriteOwnershipHolder::Container(_) => WriteOwnershipSource::Given,
            _ => WriteOwnershipSource::Bound,
        };
        self.claim_write_ownership_of(value, holder, how)
    }

    /// Who holds a value stored through an upvalue. A closed upvalue keeps the slot inside itself,
    /// so the address is no longer a stack slot a scope exit could ask about.
    fn upvalue_holder(&self, idx: usize) -> WriteOwnershipHolder {
        let upvalue = self.get_upvalue(idx);
        match unsafe { (*upvalue).is_closed() } {
            true => WriteOwnershipHolder::Container(Value::from(unsafe { (*self.frames.top()).closure })),
            false => WriteOwnershipHolder::Name(unsafe { (*upvalue).location }),
        }
    }

    /// Whether the claim standing over this value is one a retaining parameter took.
    fn took_write_ownership_by_retain(&self, value: Value) -> bool {
        self.write_ownerships.iter().rev().find(|held| held.value == value)
            .is_some_and(|held| held.how == WriteOwnershipSource::Taken)
    }

    fn record_write_ownership(&mut self, value: Value, holder: WriteOwnershipHolder, how: WriteOwnershipSource) {
        #[cfg(debug_assertions)]
        assert!(objects::is_mutable_container(value), "a value nothing can write took a write-ownership record");
        #[cfg(debug_assertions)]
        assert!(!self.write_ownerships.iter().any(|held| held.value == value), "a value took a second write-ownership record");
        value.as_object().set_write_owned(true);
        self.write_ownerships.push(WriteOwnership { value, holder, at: self.current_pos_index(), how });
    }

    /// Hands each retained argument's write-ownership to the parameter slot about to take it.
    pub(crate) fn transfer_argument_write_ownership(&mut self, masks: CallMasks, stack_start: *mut Value, arity: usize, receiver: ReceiverSlot) -> Result<(), anyhow::Error> {
        let CallMasks { retain_mask, escape_mask, needs_borrow_mark } = masks;
        // Slot zero is where a method's receiver lives. It takes the marker its declaration gave
        // it, the way a parameter takes its own.
        match receiver {
            ReceiverSlot::Retained => self.retain_slot(stack_start)?,
            ReceiverSlot::Borrowed => self.borrow_slot(stack_start, true),
            ReceiverSlot::Callee => {},
        }
        for position in 0..arity.min(64) {
            // Slot zero holds the receiver, so a parameter sits one above its position.
            let addr = unsafe { stack_start.add(position + 1) };
            let value = unsafe { *addr };
            #[cfg(debug_assertions)]
            self.assert_borrow_marks_agree(addr);
            // A parameter that does not retain its argument borrows it.
            if retain_mask & (1u64 << position) == 0 {
                // A slot only needs the mark where the body hands it to a call that might retain it.
                let marks_slot = self.forced || needs_borrow_mark & (1u64 << position) != 0;
                self.borrow_slot(addr, marks_slot);
                if self.forced && objects::is_mutable_container(value) && escape_mask & (1u64 << position) == 0 {
                    self.watch_borrow_claim(value, position as u8, stack_start);
                }
                continue;
            }
            // This parameter retains its argument.
            self.retain_slot(addr)?;
        }
        Ok(())
    }

    /// Marks what a slot holds as borrowed for the call, so the body may not let it outlive the call.
    /// A frozen value is marked too, because the declaration is what lends it, not its mutability.
    fn borrow_slot(&mut self, addr: *mut Value, marks_slot: bool) {
        let value = unsafe { *addr };
        if !objects::is_container(value) {
            if marks_slot {
                self.stack.mark_borrowed(addr);
            }
            return;
        }

        // The object's own bit answers for a container, so release marks no slot for one. Debug
        // builds mark it anyway, to keep the pruning under a cross-check the header can settle.
        #[cfg(debug_assertions)]
        self.stack.mark_borrowed(addr);

        self.borrows.push((value, value.as_object().is_borrowed()));
        value.as_object().set_borrowed(true);
    }

    /// Carries a borrow mark from one slot to another, so a binding of a borrowed value is borrowed too.
    #[inline]
    pub(super) fn carry_borrowed(&mut self, from: *mut Value, into: *mut Value) {
        match self.stack.is_borrowed(from) {
            true => self.stack.mark_borrowed(into),
            false => self.stack.clear_borrowed(into),
        }
    }

    /// A slot marked as borrowed holds a value some call borrowed, so an object in it carries the borrow bit
    /// too. The two are recorded by different mechanisms, and this is where they have to agree.
    #[cfg(debug_assertions)]
    fn assert_borrow_marks_agree(&self, addr: *mut Value) {
        let value = unsafe { *addr };
        if self.stack.is_borrowed(addr) && objects::is_container(value) {
            assert!(value.as_object().is_borrowed(), "a borrowed slot holds a container nothing marked borrowed");
        }
    }

    /// Hands the write-ownership of what a slot holds to that slot, for a `*` marker.
    fn retain_slot(&mut self, addr: *mut Value) -> Result<(), anyhow::Error> {
        if self.stack.is_borrowed(addr) {
            return Err(self.retained_borrow_error());
        }
        let value = unsafe { *addr };
        if !objects::is_container(value) {
            return Ok(());
        }
        if value.as_object().is_borrowed() {
            return Err(self.retained_borrow_error());
        }
        // Only a mutable value has write-ownership for the slot to take.
        if !objects::is_mutable_container(value) {
            return Ok(());
        }
        if value.as_object().is_write_retired() {
            return Err(self.retained_twice_error());
        }
        value.as_object().set_borrowed(false);
        let holder = WriteOwnershipHolder::Name(addr);
        if self.holds_write_ownership(holder, value) {
            return Ok(());
        }
        self.blank_claims_on(value);
        self.record_write_ownership(value, holder, WriteOwnershipSource::Taken);
        Ok(())
    }

    /// A retain of a value an earlier retain already took the write-ownership of.
    #[cold]
    #[inline(never)]
    fn retained_twice_error(&self) -> anyhow::Error {
        self.raise(Diagnostic::new(objects::RETAINED_TWICE, self.get_source_position().clone())
            .with_label("an earlier call already took its write-ownership"))
            .unwrap_err()
    }

    /// A retain that would carry a borrowed value past the frame that lent it.
    #[cold]
    #[inline(never)]
    fn retained_borrow_error(&self) -> anyhow::Error {
        self.raise(Diagnostic::new(objects::RETAINED_BORROW, self.get_source_position().clone())
            .with_label("this call takes a value the caller only lent"))
            .unwrap_err()
    }

    /// Blanks every claim over a value.
    fn blank_claims_on(&mut self, value: Value) {
        for held in self.write_ownerships.iter_mut() {
            if held.value == value {
                held.value = Value::NULL;
            }
        }
    }

    /// The address of a local in the running frame, which is what identifies a holder.
    pub(super) fn slot_addr(&self, slot: usize) -> *mut Value {
        unsafe { (*self.frames.top()).stack_start.add(slot) }
    }

    /// Whether this holder is the one already holding the value's writer slot.
    fn holds_write_ownership(&self, holder: WriteOwnershipHolder, value: Value) -> bool {
        self.write_ownerships.iter().rev().find(|held| held.value == value)
            .is_some_and(|held| same_holder(held.holder, holder))
    }

    /// The trap for a second name taking the writer slot for one element.
    #[cold]
    #[inline(never)]
    fn second_writer_error(&mut self, value: Value) -> anyhow::Error {
        let here = self.get_source_position().clone();
        let held = self.write_ownerships.iter().rev().find(|held| held.value == value).copied();
        let given = held.is_some_and(|held| held.how == WriteOwnershipSource::Given);
        let mut diagnostic = match given {
            true => Diagnostic::new(objects::WROTE_GIVEN_ELEMENT, here.clone())
                .with_label(format!("`{}` is written here", here.snippet())),
            false => Diagnostic::new(objects::SECOND_ELEMENT_WRITER, here.clone())
                .with_label("this takes a second writer for the element"),
        };
        if let Some(held) = held {
            let taken = self.chunk.code_pos[held.at as usize].clone();
            if taken.start != here.start {
                let label = match given {
                    true => format!("`{}` is stored here, which takes write-ownership", taken.snippet()),
                    false => "another name already writes it here".to_string(),
                };
                diagnostic = diagnostic.with_context_span(taken, label);
            }
        }
        let help = match given {
            true => "an element has one write-owner; reading it here is still fine, but to write it, go through the container it was stored into",
            false => "one element has one writer; let the other name go out of scope first, or only read through this one",
        };
        self.raise(diagnostic.with_help(help)).unwrap_err()
    }

    /// Gives back whatever the scope's own `count` values still hold.
    pub(super) fn op_release_write_ownership(&mut self) {
        let count = self.read_next() as usize;
        let floor = unsafe { self.stack.top().sub(count) };
        self.retire_slots(0, |addr| addr >= floor, None);
    }

    pub(super) fn op_pop_scope(&mut self) {
        let count = self.read_next() as usize;
        let expected_slot_count = self.read_next() as usize;
        let base = unsafe { self.stack.top().sub(count) };
        debug_assert_eq!(self.frame_slot_count(), expected_slot_count, "a scope left a stack its compiler did not expect");
        self.close_upvalues(base);
        self.retire_slots(0, |addr| addr >= base, None);
        self.stack.set_top(base);
    }

    fn frame_slot_count(&self) -> usize {
        let frame = self.frames.top();
        if frame.is_null() {
            return self.stack.len();
        }
        let start = unsafe { (*frame).stack_start };
        (self.stack.top() as usize - start as usize) / std::mem::size_of::<Value>()
    }

    /// Retires the write-ownerships from index `depth` on whose slot the predicate says is dying.
    fn retire_slots(&mut self, depth: usize, dying: impl Fn(*mut Value) -> bool, leaving: Option<Leaving>) {
        let mut kept = depth;
        for i in depth..self.write_ownerships.len() {
            let mut write_ownership = self.write_ownerships[i];
            let disposition = match write_ownership.holder {
                WriteOwnershipHolder::Name(addr) if dying(addr) =>
                    self.disposition_of(&write_ownership, addr, leaving, &dying),
                // The slot is not going, so the claim stands as it is.
                holder => WriteOwnershipDisposition::Held(holder),
            };
            let WriteOwnershipDisposition::Held(holder) = disposition else { continue };
            write_ownership.holder = holder;
            self.write_ownerships[kept] = write_ownership;
            kept += 1;
        }
        self.write_ownerships.truncate(kept);
    }

    /// What happens to a dying slot's write-ownership.
    fn disposition_of(&mut self, write_ownership: &WriteOwnership, addr: *mut Value,
        leaving: Option<Leaving>, dying: &impl Fn(*mut Value) -> bool) -> WriteOwnershipDisposition
    {
        let value = write_ownership.value;
        if value.is_null() {
            return WriteOwnershipDisposition::Dropped;
        }
        match write_ownership.how {
            // A binding only borrowed the write-ownership, so the source gets it back.
            WriteOwnershipSource::Bound => {
                self.release_write_ownership(value);
                WriteOwnershipDisposition::Dropped
            },
            WriteOwnershipSource::Taken => match leaving.and_then(|out| out.slot_for(value)) {
                Some(lands_at) => WriteOwnershipDisposition::Held(WriteOwnershipHolder::Name(lands_at)),
                None => match objects::recorded_holder(value) {
                    objects::RecordedHolder::Container => {
                        self.release_write_ownership(value);
                        WriteOwnershipDisposition::Dropped
                    },
                    objects::RecordedHolder::Nobody => {
                        value.as_object().set_write_retired(true);
                        WriteOwnershipDisposition::Held(WriteOwnershipHolder::Retired)
                    },
                },
            },
            // A container's claim belongs to the value in that slot, whose lifetime the slot does
            // not bound. Whether anything still reaches the container is settled at the next write.
            WriteOwnershipSource::Given => {
                let container = unsafe { *addr };
                if self.container_died_with_scope(container, dying) {
                    self.release_write_ownership_from(value, container);
                    return WriteOwnershipDisposition::Dropped;
                }
                WriteOwnershipDisposition::Held(WriteOwnershipHolder::Container(container))
            },
        }
    }

    fn container_died_with_scope(&self, container: Value, dying: &impl Fn(*mut Value) -> bool) -> bool {
        objects::can_own_writes(container)
            && matches!(objects::recorded_holder(container), objects::RecordedHolder::Nobody)
            && !container.as_object().is_escaped()
            && !self.named_by_a_surviving_slot(container, dying)
    }

    /// Releases the write-ownership `container` held over `element`, because the container went
    /// away with its scope. The element is not leaving the container, which died still holding it.
    /// What ends is the claim, so the element is spoken for by nobody again. The container is
    /// recorded so the next collection can refute the claim that it was gone.
    fn release_write_ownership_from(&mut self, element: Value, container: Value) {
        self.release_write_ownership(element);
        if element.is_object() && element.as_object().container_write_owner() == container {
            element.as_object().set_container_write_owner(Value::NULL);
        }
        let site = self.current_pos_index() as usize;
        self.predicted_write_ownership_releases.push((container, site));
        self.predicted_write_ownership_release_sites.insert(site);
    }

    /// Whether a slot that outlives this scope exit still names the container.
    fn named_by_a_surviving_slot(&self, container: Value, dying: &impl Fn(*mut Value) -> bool) -> bool {
        let mut slot = self.stack.bottom();
        let top = self.stack.top();
        while slot < top {
            if unsafe { *slot } == container && !dying(slot) {
                return true;
            }
            slot = unsafe { slot.add(1) };
        }
        false
    }

    /// Starts watching an argument the call said it only borrows.
    fn watch_borrow_claim(&mut self, value: Value, position: u8, stack_start: *mut Value) {
        // The frame is already pushed here, so its saved return address is what names the call.
        let site = self.code_index_at(unsafe { (*self.frames.top()).return_ip });
        self.claims_made += 1;
        self.borrow_claims.push(BorrowClaim {
            value,
            into: Vec::new(),
            depth: self.frames.len(),
            stack_start,
            position,
            site,
        });
    }

    /// Notes a container a watched argument was put into. Nothing is watched unless the run forces
    /// its claims, so this is a load and a branch on an ordinary store.
    #[inline]
    pub(super) fn note_claimed_containment(&mut self, container: Value, value: Value) {
        for claim in self.borrow_claims.iter_mut() {
            if claim.value == value && !claim.into.contains(&container) {
                claim.into.push(container);
            }
        }
    }

    /// What a store through an upvalue does to a watched argument. A closed upvalue is a container
    /// like any other, so the closure is recorded. An open one names a slot, and a slot below the
    /// call's own frame belongs to a frame that outlives it, which refutes the claim on the spot.
    fn note_claimed_upvalue_store(&mut self, idx: usize, value: Value) -> Result<(), anyhow::Error> {
        let slot = match self.upvalue_holder(idx) {
            WriteOwnershipHolder::Container(closure) => {
                self.note_claimed_containment(closure, value);
                return Ok(());
            },
            WriteOwnershipHolder::Name(slot) => slot,
            _ => return Ok(()),
        };
        if let Some(claim) = self.borrow_claims.iter().find(|claim| claim.value == value && slot < claim.stack_start) {
            return Err(self.refuted_claim("is stored where an outer frame keeps it", claim.position, claim.site));
        }
        Ok(())
    }

    /// Settles every claim whose call has just ended.
    pub(super) fn settle_borrow_claims(&mut self, leaving: Value) -> Result<(), anyhow::Error> {
        if self.borrow_claims.is_empty() {
            return Ok(());
        }
        let depth = self.frames.len();
        let ended: Vec<BorrowClaim> = self.borrow_claims.extract_if(.., |claim| claim.depth > depth).collect();
        // Handing the argument back keeps it as surely as storing it does.
        for claim in &ended {
            if claim.value == leaving {
                return Err(self.refuted_claim("is handed back to the caller", claim.position, claim.site));
            }
        }
        self.claims_settled += ended.iter().filter(|claim| !claim.into.is_empty()).count();
        self.settling_containments = ended.iter()
            .flat_map(|claim| claim.into.iter().map(|&container| (container, claim.position, claim.site)))
            .collect();
        if self.settling_containments.is_empty() {
            return Ok(());
        }
        // The returned value is off the stack here, so push it back or the collection misses a root.
        // Collect now rather than wait, so the answer does not depend on when the collector runs.
        self.stack.push(leaving);
        self.start_gc();
        self.stack.truncate(1);
        let refuted = self.settling_containments.pop();
        self.settling_containments.clear();
        match refuted {
            Some((_, position, site)) => Err(self.refuted_claim("outlives the call it was lent to", position, site)),
            None => Ok(()),
        }
    }

    #[cold]
    #[inline(never)]
    fn refuted_claim(&self, what: &str, position: u8, site: usize) -> anyhow::Error {
        let pos = self.chunk.code_pos[site].clone();
        self.raise(Diagnostic::new("unsound claim: a borrowed argument was kept", pos)
            .with_label(format!("argument {} {}", position + 1, what))
            .with_help("the escape analysis said this call only borrows the argument, and forcing the claim refuted that"))
            .unwrap_err()
    }

    /// Clears one value's writer slot. A value that holds none is left alone.
    fn release_write_ownership(&self, value: Value) {
        if value.is_object() && value.as_object().is_write_owned() {
            value.as_object().set_write_owned(false);
        }
    }

    /// Retires the claims a dying frame's own slots hold, for an exit that skips the scope releases.
    /// Every slot at or above the frame's start dies with it.
    pub(super) fn release_write_ownership_above(&mut self, depth: usize, stack_start: *mut Value, returning: Value) {
        self.retire_slots(depth, |addr| addr >= stack_start, Some(Leaving::new(returning, stack_start)));
        #[cfg(debug_assertions)]
        for held in &self.write_ownerships[depth..] {
            if let WriteOwnershipHolder::Name(addr) = held.holder {
                assert!(addr <= stack_start, "a write-ownership claim outlived the frame whose slot it names");
            }
        }
    }

    /// Whether a callable lets its argument at `position` escape, as opposed to borrowing it.
    fn callee_escapes(&self, callee: Value, position: usize) -> bool {
        if !callee.is_callable() {
            return false;
        }
        let object = callee.as_object();
        match object.tag() {
            objects::TAG_CLOSURE => unsafe { &*object.as_closure_ptr() }.escapes(position),
            objects::TAG_BOUND_METHOD => {
                let method = unsafe { &*object.as_bound_method_ptr() }.method;
                method.tag() == objects::TAG_CLOSURE && unsafe { &*method.as_closure_ptr() }.escapes(position)
            },
            objects::TAG_TYPE => {
                let init = unsafe { &*object.as_type_ptr() }.factory();
                matches!(init, Some(obj) if callable_escapes(obj, position))
            },
            _ => false,
        }
    }

    pub(super) fn call_native(&mut self, arg_count: usize, native_fn_ptr: *mut ObjNativeFn) -> Result<(), anyhow::Error> {
        let func = unsafe { &*native_fn_ptr };
        check_arity!(self, arg_count, func.arity as usize, func.name);
        let args = self.stack.pop_slice(arg_count);
        // The "target" is the first value in a call window. For method calls, this is the instance.
        let target = self.stack.pop();
        match (func.function)(self, target, args) {
            Ok(_) => Ok(()),
            // A native reports the code and the host owns the rendering, so a persist caught in a
            // primitive reads the same as one caught at a bytecode store.
            Err(err) => match err.downcast::<String>()? {
                msg if msg == objects::PERSISTED_BORROW => Err(self.persisted_borrow_error()),
                msg => self.error(msg),
            }
        }
    }

    fn call_closure(&mut self, arg_count: usize, closure_ptr: *mut ObjClosure, seal: bool) -> Result<(), anyhow::Error> {
        let closure = unsafe { &*closure_ptr };
        check_arity!(self, arg_count, closure.arity, closure.name);
        let stack_start = self.stack.offset(arg_count);
        self.push_frame(closure_ptr, stack_start, closure.ip_start, seal)?;
        self.transfer_argument_write_ownership(closure.call_masks(), stack_start, arg_count, ReceiverSlot::Callee)?;
        Ok(())
    }

    fn call_bound_method(&mut self, arg_count: usize, bound_method_ptr: *mut ObjBoundMethod, seal: bool) -> Result<(), anyhow::Error> {
        let bound_method = unsafe { &*bound_method_ptr };
        let method = bound_method.method;
        match method.tag() {
            objects::TAG_CLOSURE => {
                let closure_ptr = method.as_closure_ptr();
                let closure = unsafe { &*closure_ptr };
                if closure.mut_receiver && self.receiver_rejects_mut(bound_method.target) {
                    return self.error_readonly_receiver(closure.name, bound_method.target);
                }
                check_arity!(self, arg_count, closure.arity, closure.name);
                let stack_start = self.stack.set(arg_count, Value::from(bound_method.target));
                self.push_frame(closure_ptr, stack_start, closure.ip_start, seal)?;
                self.transfer_argument_write_ownership(closure.call_masks(), stack_start, arg_count, ReceiverSlot::declared(closure.retain_receiver))?;
            },
            objects::TAG_NATIVE_FUNCTION => {
                self.stack.set(arg_count, Value::from(bound_method.target));
                self.call_native(arg_count, method.as_native_function_ptr())?;
            },
            _ => unsafe { std::hint::unreachable_unchecked() }
        };
        Ok(())
    }

    /// Brace construction `C { f: v, ... }`. Reads the brace field ids, allocates the instance,
    /// sets the brace fields from the stack, then verifies `gives`. The stack holds
    /// `[C, brace values..]` in source order.
    pub(super) fn op_construct(&mut self) -> Result<(), anyhow::Error> {
        let field_count = self.read_next() as usize;
        let mut field_ids = [0u8; u8::MAX as usize + 1];
        for slot in field_ids.iter_mut().take(field_count) {
            *slot = self.read_next();
        }

        // A plain brace freezes the instance in place; `mut K{..}` (seal 0) leaves it mutable.
        let seal = self.read_next() != 0;

        let type_val = self.stack.peek(field_count);
        if !type_val.is_object() || type_val.as_object().tag() != objects::TAG_TYPE {
            return self.error(format!("Cannot construct: {} is not a type", type_val.fmt()));
        }
        let type_ptr = type_val.as_object().as_type_ptr();
        let ty = unsafe { &*type_ptr };

        let instance_ptr = self.alloc(ObjInstance::new(type_ptr));
        let instance = unsafe { &mut *instance_ptr };
        let instance_val = Value::from(instance_ptr);
        for j in 0..field_count {
            let value = self.stack.peek(field_count - 1 - j);
            // A frozen instance is immutable all the way down.
            if seal && objects::is_mutable_container(value) {
                return self.error_seal();
            }
            // A construction is fresh, so nothing outside reaches it yet. The store that takes it
            // out is what asks, and this record is what lets that store answer.
            objects::record_held_borrow(instance_val, value);
            instance.set(field_ids[j], value);
        }

        // A construction verifies each `gives` delegate actually provides its trait.
        for &(field_id, field_name, trait_name, trait_id) in ty.gives.iter() {
            let value = instance.get(field_id);
            let provides = matches!(value.kind(), ValueKind::Object(ObjectKind::Instance))
                && unsafe { &*(*value.as_object().as_instance_ptr()).ty }.provided.contains(&trait_id);
            if !provides {
                let msg = format!("Delegate field '{}' does not provide trait '{}'",
                    unsafe { &(*field_name).value }, unsafe { &(*trait_name).value });
                return self.error(msg);
            }
        }

        // A sealed brace freezes what it took, so it writes nothing and takes no write-ownership.
        if !seal {
            for j in 0..field_count {
                self.container_took(instance_val, instance.get(field_ids[j]))?;
            }
        }

        if seal {
            crate::core::objects::freeze_value(Value::from(instance_ptr), self.current_pos_index());
        }
        self.stack.truncate(field_count + 1);
        self.stack.push(Value::from(instance_ptr));
        Ok(())
    }

    fn call_type(&mut self, arg_count: usize, type_ptr: *mut ObjType, seal: bool) -> Result<(), anyhow::Error> {
        let ty = unsafe { &*type_ptr };
        let Some(factory_obj) = ty.factory() else {
            let name = unsafe { &(*ty.name).value };
            return self.error(format!("'{name}' has no factory; construct it with a brace like '{name}{{ .. }}'"));
        };
        match factory_obj.tag() {
            objects::TAG_FUNCTION => {
                let factory_ref = factory_obj.as_function_ptr();
                let factory = unsafe { &*factory_ref };
                check_arity!(self, arg_count, factory.arity, factory.name);

                let closure = self.create_closure(factory_ref);
                // Root the fresh closure on the value stack: it isn't reachable yet and
                // the instance allocation below can trigger GC.
                self.stack.push(Value::from(closure));
                let instance = self.alloc(ObjInstance::new(type_ptr));
                self.stack.pop();
                let stack_start = self.stack.set(arg_count, Value::from(instance));
                self.push_frame(closure.as_closure_ptr(), stack_start, factory.ip_start, seal)?;
                self.transfer_argument_write_ownership(factory.call_masks(), stack_start, arg_count, ReceiverSlot::Borrowed)?;
                Ok(())
            },
            objects::TAG_CLOSURE => {
                let closure_ptr = factory_obj.as_closure_ptr();
                let closure = unsafe { &*closure_ptr };
                check_arity!(self, arg_count, closure.arity, closure.name);

                let instance = self.alloc(ObjInstance::new(type_ptr));
                let stack_start = self.stack.set(arg_count, Value::from(instance));
                self.push_frame(closure_ptr, stack_start, closure.ip_start, seal)?;
                self.transfer_argument_write_ownership(closure.call_masks(), stack_start, arg_count, ReceiverSlot::Borrowed)?;
                Ok(())
            },
            // A native factory receives the fresh instance as its target and fills its fields. The
            // seal is applied here rather than in the native, so `mut K(..)` reaches a built-in
            // type the same way `RETURN_FAC` carries it out of a script factory.
            objects::TAG_NATIVE_FUNCTION => {
                let factory_native = factory_obj.as_native_function_ptr();
                let instance = self.alloc(ObjInstance::new(type_ptr));
                self.stack.set(arg_count, Value::from(instance));
                self.call_native(arg_count, factory_native)?;
                if seal {
                    objects::freeze_value(self.stack.peek(0), self.current_pos_index());
                }
                Ok(())
            },
            _ => unsafe { std::hint::unreachable_unchecked() }
        }
    }
}

/// Whether a callable persists the argument at `position`.
fn callable_escapes(obj: Object, position: usize) -> bool {
    match obj.tag() {
        objects::TAG_FUNCTION => unsafe { &*obj.as_function_ptr() }.escapes(position),
        objects::TAG_CLOSURE => unsafe { &*obj.as_closure_ptr() }.escapes(position),
        _ => false,
    }
}

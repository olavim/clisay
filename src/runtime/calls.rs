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
    !value.as_object().has_container_write_owner() || objects::write_ownership_reaches(value, root)
}

/// Whether two records name the same holder.
fn same_holder(a: WriteOwnershipHolder, b: WriteOwnershipHolder) -> bool {
    match (a, b) {
        (WriteOwnershipHolder::Dead | WriteOwnershipHolder::Retired, _)
        | (_, WriteOwnershipHolder::Dead | WriteOwnershipHolder::Retired) => false,
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

    /// The path-write barrier. A write through a path has no binding denoting the element it
    /// reaches, so it takes no writer slot of its own. It fails only when one is already held by
    /// something other than its own root.
    pub(super) fn op_assert_no_other_writer(&mut self) -> Result<(), anyhow::Error> {
        let slot = self.read_next() as usize;
        let root = self.slot_addr(slot);
        self.assert_no_other_writer(WriteOwnershipHolder::Name(root))
    }

    /// The same barrier for a path rooted in a binding the body reaches as an upvalue.
    pub(super) fn op_assert_no_other_writer_up(&mut self) -> Result<(), anyhow::Error> {
        let idx = self.read_next() as usize;
        let root = unsafe { (*self.get_upvalue(idx)).location };
        self.assert_no_other_writer(WriteOwnershipHolder::Name(root))
    }

    /// Lets the write through when the path's own root is what holds the element.
    fn assert_no_other_writer(&mut self, root: WriteOwnershipHolder) -> Result<(), anyhow::Error> {
        let value = self.stack.peek(0);
        if !value.is_object() {
            return Ok(());
        }
        let root_value = match root {
            WriteOwnershipHolder::Name(addr) => unsafe { *addr },
            WriteOwnershipHolder::Container(container) => container,
            WriteOwnershipHolder::Dead | WriteOwnershipHolder::Retired => Value::NULL,
        };
        self.assert_no_other_owner(value, root_value)?;
        if !value.as_object().is_write_owned() {
            return Ok(());
        }
        // A container that took an element writes it through its own paths. Only a path rooted
        // somewhere else is a second writer.
        if self.holds_write_ownership(value, root) {
            return Ok(());
        }
        self.refuse_unless_holder_is_gone(value)
    }

    /// The container half of the write barrier, asked of the element itself. Every store records
    /// what holds the value it stored, so this answers without anything codegen placed. A path
    /// rooted somewhere that does not hold the element is writing what another container owns.
    fn assert_no_other_owner(&mut self, value: Value, root: Value) -> Result<(), anyhow::Error> {
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

    /// The path-write barrier for a root no binding names, such as a call result.
    pub(super) fn op_assert_no_other_writer_root(&mut self) -> Result<(), anyhow::Error> {
        let value = self.stack.pop();
        let root = self.stack.pop();
        self.stack.push(value);
        self.assert_no_other_writer(WriteOwnershipHolder::Container(root))
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

    /// Makes sure the root of a `target` value has write-ownership of the target.
    /// The root of e.g. `a[i]` is `a`. The root of a local or upvalue is itself.
    pub(super) fn ensure_writer_is_root(&mut self, target: Value, kind: u8, operand: u8) -> Result<(), anyhow::Error> {
        if !target.as_object().is_write_owned() {
            return Ok(());
        }
        let root = match kind {
            ir::WRITE_ROOT_LOCAL => WriteOwnershipHolder::Name(self.slot_addr(operand as usize)),
            ir::WRITE_ROOT_UPVALUE => {
                WriteOwnershipHolder::Name(unsafe { (*self.get_upvalue(operand as usize)).location })
            },
            _ => return Ok(()),
        };
        if self.holds_write_ownership(target, root) {
            return Ok(());
        }
        self.refuse_unless_holder_is_gone(target)
    }

    /// Assert that nothing is holding the write-ownership of the value on top.
    pub(super) fn op_assert_no_writer(&mut self) -> Result<(), anyhow::Error> {
        let value = self.stack.peek(0);
        if !value.is_object() || !value.as_object().is_write_owned() {
            return Ok(());
        }
        self.refuse_unless_holder_is_gone(value)
    }

    /// The last question before refusing a write. A claim keyed to a container outlives every name
    /// that reached it, so it can name a container nothing holds any more, and only the collector
    /// knows which. Ask it here rather than refusing a write that is really free.
    fn refuse_unless_holder_is_gone(&mut self, value: Value) -> Result<(), anyhow::Error> {
        match self.holder_of(value) {
            // The bit and the record are set together and cleared together. Arriving here with the
            // bit and no record means one of the two moved without the other.
            None => unreachable!("a write-owned value has no record of who holds it"),
            Some(WriteOwnershipHolder::Name(_)) => Err(self.second_writer_error(value)),
            Some(WriteOwnershipHolder::Retired) => Err(self.wrote_transferred_element_error()),
            Some(WriteOwnershipHolder::Dead) => Ok(self.forget_claim(value)),
            Some(WriteOwnershipHolder::Container(_)) => {
                // Collecting here rather than on the next allocation is what keeps the answer the
                // same under `CLISAY_GC_STRESS`. The written value is on the stack, so it survives.
                self.start_gc();
                match self.holder_of(value) {
                    Some(WriteOwnershipHolder::Container(_)) => Err(self.second_writer_error(value)),
                    Some(WriteOwnershipHolder::Dead) => Ok(self.forget_claim(value)),
                    Some(WriteOwnershipHolder::Name(_) | WriteOwnershipHolder::Retired) => unreachable!("a prune clears a container, it never hands a claim to a name or retires one"),
                    None => unreachable!("the written value is a stack root, so the prune keeps its record"),
                }
            },
        }
    }

    /// Who holds this value's writer slot, if the table still says anyone does.
    fn holder_of(&self, value: Value) -> Option<WriteOwnershipHolder> {
        self.write_ownerships.iter().rev().find(|held| held.value == value).map(|held| held.holder)
    }

    /// Forgets a claim whose holder is gone. The element is writable from here on, so no later write
    /// pays for the collection this one asked for.
    fn forget_claim(&mut self, value: Value) {
        self.write_ownerships.retain(|held| held.value != value);
        self.release_write_ownership(value);
    }

    /// The persist barrier. A parameter may hold a mutable borrowed from the caller, and storing
    /// it somewhere that outlives the call would keep it reachable past the borrow.
    pub(super) fn op_assert_not_borrowed(&mut self) -> Result<(), anyhow::Error> {
        let forced = self.at_elided_site();
        let value = self.stack.peek(0);
        if !value.is_borrowed() {
            return Ok(());
        }
        if forced {
            return self.refuted_elision("a value proven unborrowed is borrowed");
        }
        let position = self.get_source_position().clone();
        let label = format!("`{}` is a mutable value borrowed from the caller", position.snippet());
        self.raise(Diagnostic::new(objects::PERSISTED_BORROW, position)
            .with_label(label)
            .with_help("a borrowed value cannot be stored where it outlives the borrow; take the parameter by `*mut` to own it, or `copy` it before storing"))
    }

    /// An argument the caller reads again after the call. The compiler could not tell whether the
    /// callee consumes it, so the reader demanded this proof that it only borrowed.
    pub(super) fn op_assert_not_consumed(&mut self) -> Result<(), anyhow::Error> {
        let arg_count = self.read_next() as usize;
        let count = self.read_next() as usize;
        let call_pos = self.get_source_position().clone();
        let callee = self.stack.peek(arg_count);
        for _ in 0..count {
            let position = self.read_next() as usize;
            if self.callee_escapes(callee, position) {
                let read = self.get_source_position().clone();
                let label = format!("`{}` used here", read.snippet());
                return self.raise(Diagnostic::new(objects::CONSUMED_ARGUMENT, read)
                    .with_label(label)
                    .with_context_span(call_pos, "this call took it"));
            }
        }
        Ok(())
    }

    /// The opaque-call mode barrier. An argument the caller must keep alive may not be handed to a
    /// callee that consumes it, so this asserts the callee borrows the guarded parameter.
    pub(super) fn op_assert_borrow(&mut self) -> Result<(), anyhow::Error> {
        let arg_count = self.read_next() as usize;
        let count = self.read_next() as usize;
        let callee = self.stack.peek(arg_count);
        for _ in 0..count {
            let position = self.read_next() as usize;
            if self.callee_escapes(callee, position) {
                let label = format!("the callee lets `{}` escape", self.get_source_position().snippet());
                return self.error_labeled(objects::ESCAPED_BORROW, label);
            }
        }
        Ok(())
    }

    /// Marks the listed argument positions borrowed for the following call, so a store of any of
    /// them traps. Each entry saves the prior bit for nesting; a matching `RELEASE_BORROW` restores.
    pub(super) fn op_mark_borrow(&mut self) {
        let arg_count = self.read_next() as usize;
        let count = self.read_next() as usize;
        for _ in 0..count {
            let position = self.read_next() as usize;
            let value = self.stack.peek(arg_count - 1 - position);
            let prev = value.is_borrowed();
            // A frozen `no persist` value is still marked: immutability stops mutation, not the
            // persist that the borrow bit traps.
            if value.is_object() {
                value.as_object().set_borrowed(true);
            }
            self.borrows.push((value, prev));
        }
    }

    /// Takes the writer slot for an element the compiler could not name. A literal key is settled
    /// statically, so only a computed one reaches here.
    pub(super) fn op_take_write_ownership(&mut self) -> Result<(), anyhow::Error> {
        let slot = self.read_next() as usize;
        self.take_write_ownership(slot, WriteOwnershipSource::Bound)
    }

    /// A container taking the writer slot for an element it is given. The slot names the container
    /// rather than holding it yet, since a construction takes its elements before it is assigned.
    pub(super) fn op_transfer_write_ownership(&mut self) -> Result<(), anyhow::Error> {
        let slot = self.read_next() as usize;
        let holder = WriteOwnershipHolder::Name(self.slot_addr(slot));
        self.claim_write(holder, WriteOwnershipSource::Given)
    }

    /// A container taking the writer slot for an element it is given, for a container the body
    /// reaches as an upvalue. That one is built already, so the claim names the container itself.
    pub(super) fn op_transfer_write_ownership_up(&mut self) -> Result<(), anyhow::Error> {
        let idx = self.read_next() as usize;
        let container = unsafe { *(*self.get_upvalue(idx)).location };
        self.claim_write(WriteOwnershipHolder::Container(container), WriteOwnershipSource::Given)
    }

    /// A container taking the writer slot for an element it is given, where no binding names the
    /// container. It is still on the stack, `depth` below the element.
    pub(super) fn op_transfer_write_ownership_at(&mut self) -> Result<(), anyhow::Error> {
        let depth = self.read_next() as usize;
        let container = self.stack.peek(depth);
        self.claim_write(WriteOwnershipHolder::Container(container), WriteOwnershipSource::Given)
    }

    /// Takes the writer slot for the value on top in the name of a local. The holder may take it
    /// again as often as it likes, so only a different holder is a second writer.
    fn take_write_ownership(&mut self, slot: usize, how: WriteOwnershipSource) -> Result<(), anyhow::Error> {
        let holder = WriteOwnershipHolder::Name(self.slot_addr(slot));
        self.claim_write(holder, how)
    }

    /// Records `holder` as the one name that may write the value on top.
    fn claim_write(&mut self, holder: WriteOwnershipHolder, how: WriteOwnershipSource) -> Result<(), anyhow::Error> {
        let value = self.stack.peek(0);
        // Nothing can write an immutable value or a primitive.
        if !objects::is_mutable_container(value) {
            return Ok(());
        }
        if value.as_object().is_write_owned() {
            if self.holds_write_ownership(value, holder) {
                return Ok(());
            }
            // A parameter that took write-ownership by `*mut` is entitled to hand it on, so a store
            // out of that parameter is the transfer continuing rather than a second writer.
            if self.took_write_ownership_by_move(value) {
                self.blank_claims_on(value);
            } else {
                self.refuse_unless_holder_is_gone(value)?;
            }
        }
        self.record_write_ownership(value, holder, how);
        Ok(())
    }

    /// Whether the claim standing over this value is one a `*mut` parameter took.
    fn took_write_ownership_by_move(&self, value: Value) -> bool {
        self.write_ownerships.iter().rev().find(|held| held.value == value)
            .is_some_and(|held| held.how == WriteOwnershipSource::Taken)
    }

    fn record_write_ownership(&mut self, value: Value, holder: WriteOwnershipHolder, how: WriteOwnershipSource) {
        #[cfg(debug_assertions)]
        assert!(!self.write_ownerships.iter().any(|held| held.value == value), "a value took a second write-ownership record");
        value.as_object().set_write_owned(true);
        self.write_ownerships.push(WriteOwnership { value, holder, at: self.current_pos_index(), how });
    }

    /// Hands each `*mut` argument's write-ownership to the parameter slot that is about to take it.
    pub(crate) fn transfer_argument_write_ownership(&mut self, move_mask: u64, stack_start: *mut Value, arity: usize) {
        if move_mask == 0 {
            return;
        }
        for position in 0..arity.min(64) {
            if move_mask & (1u64 << position) == 0 {
                continue;
            }
            // Slot zero holds the receiver, so a parameter sits one above its position.
            let addr = unsafe { stack_start.add(position + 1) };
            let value = unsafe { *addr };
            if !objects::is_mutable_container(value) {
                continue;
            }
            let holder = WriteOwnershipHolder::Name(addr);
            if self.holds_write_ownership(value, holder) {
                continue;
            }
            self.blank_claims_on(value);
            self.record_write_ownership(value, holder, WriteOwnershipSource::Taken);
        }
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
    fn slot_addr(&self, slot: usize) -> *mut Value {
        unsafe { (*self.frames.top()).stack_start.add(slot) }
    }

    /// Whether this holder is the one already holding the value's writer slot.
    fn holds_write_ownership(&self, value: Value, holder: WriteOwnershipHolder) -> bool {
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

    /// Gives back the slot a local holds, where its value is about to change.
    pub(super) fn op_release_write_ownership_at(&mut self) {
        let slot = self.read_next() as usize;
        let holder = self.slot_addr(slot);
        self.retire_slots(0, |addr| addr == holder, None);
    }

    /// Gives back whatever the scope's own `count` values still hold.
    pub(super) fn op_release_write_ownership(&mut self) {
        let count = self.read_next() as usize;
        let floor = unsafe { self.stack.top().sub(count) };
        self.retire_slots(0, |addr| addr >= floor, None);
    }

    /// Retires the write-ownerships from index `depth` on whose slot the predicate says is dying.
    fn retire_slots(&mut self, depth: usize, dying: impl Fn(*mut Value) -> bool, returning: Option<(Value, *mut Value)>) {
        let mut kept = depth;
        for i in depth..self.write_ownerships.len() {
            let mut held = self.write_ownerships[i];
            if let WriteOwnershipHolder::Name(addr) = held.holder {
                if dying(addr) {
                    let restated = !held.value.is_object()
                        || self.write_ownerships[i + 1..].iter().any(|later| later.value == held.value);
                    match held.how {
                        // A binding only borrowed the write-ownership, so the source gets it back.
                        WriteOwnershipSource::Bound => {
                            self.release_write_ownership(held.value);
                            continue;
                        },
                        // A `*mut` hand-off is not a loan. The value keeps travelling if it is on
                        // its way out, and otherwise the write-ownership belongs to nobody.
                        WriteOwnershipSource::Taken => {
                            match returning {
                                Some((value, lands_at)) if value == held.value =>
                                    held.holder = WriteOwnershipHolder::Name(lands_at),
                                // A later claim already says where the value went.
                                _ if restated => continue,
                                _ if held.value.as_object().has_container_write_owner() => {
                                    self.release_write_ownership(held.value);
                                    continue;
                                },
                                _ => {
                                    held.holder = WriteOwnershipHolder::Retired;
                                    held.value.as_object().set_write_retired(true);
                                },
                            }
                        },
                        // A container's claim belongs to the value in that slot, whose lifetime the
                        // slot does not bound. Whether anything still reaches the container is settled
                        // at the next write.
                        WriteOwnershipSource::Given => {
                            let container = unsafe { *addr };
                            if self.container_died_with_scope(container, &dying) {
                                self.release_write_ownership_from(held.value, container);
                                continue;
                            }
                            held.holder = WriteOwnershipHolder::Container(container);
                        },
                    }
                }
            }
            self.write_ownerships[kept] = held;
            kept += 1;
        }
        self.write_ownerships.truncate(kept);
    }

    /// Whether the container went away with the scope that is exiting. Every route out of a scope
    /// leaves one of these marks behind, so a container carrying none of them is reachable from
    /// nothing and the write-ownership it holds is nobody's.
    fn container_died_with_scope(&self, container: Value, dying: &impl Fn(*mut Value) -> bool) -> bool {
        objects::is_mutable_container(container)
            && !container.as_object().has_container_write_owner()
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

    /// Clears one value's writer slot. A value that holds none is left alone.
    fn release_write_ownership(&self, value: Value) {
        if value.is_object() && value.as_object().is_write_owned() {
            value.as_object().set_write_owned(false);
        }
    }

    /// Retires the claims a dying frame's own slots hold, for an exit that skips the scope releases.
    /// Every slot at or above the frame's start dies with it.
    pub(super) fn release_write_ownership_above(&mut self, depth: usize, stack_start: *mut Value, returning: Value) {
        self.retire_slots(depth, |addr| addr >= stack_start, Some((returning, stack_start)));
        #[cfg(debug_assertions)]
        for held in &self.write_ownerships[depth..] {
            if let WriteOwnershipHolder::Name(addr) = held.holder {
                assert!(addr <= stack_start, "a write-ownership claim outlived the frame whose slot it names");
            }
        }
    }

    /// Restores the last `count` marked borrows after a call returns.
    pub(super) fn op_release_borrow(&mut self) {
        let count = self.read_next() as usize;
        for _ in 0..count {
            if let Some((value, prev)) = self.borrows.pop() {
                if value.is_object() {
                    value.as_object().set_borrowed(prev);
                }
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
            Err(err) => self.error(err.downcast::<String>()?)
        }
    }

    fn call_closure(&mut self, arg_count: usize, closure_ptr: *mut ObjClosure, seal: bool) -> Result<(), anyhow::Error> {
        let closure = unsafe { &*closure_ptr };
        check_arity!(self, arg_count, closure.arity, closure.name);
        let stack_start = self.stack.offset(arg_count);
        self.push_frame(closure_ptr, stack_start, closure.ip_start, seal)?;
        self.transfer_argument_write_ownership(closure.move_mask, stack_start, arg_count);
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
                self.transfer_argument_write_ownership(closure.move_mask, stack_start, arg_count);
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
    /// `[C, brace values..]` in source order. A brace does not run the factory.
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
        for j in 0..field_count {
            let value = self.stack.peek(field_count - 1 - j);
            self.ensure_not_borrowed(value)?;
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
                self.give_write_ownership(Value::from(instance_ptr), instance.get(field_ids[j]))?;
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
                self.transfer_argument_write_ownership(factory.move_mask, stack_start, arg_count);
                Ok(())
            },
            objects::TAG_CLOSURE => {
                let closure_ptr = factory_obj.as_closure_ptr();
                let closure = unsafe { &*closure_ptr };
                check_arity!(self, arg_count, closure.arity, closure.name);

                let instance = self.alloc(ObjInstance::new(type_ptr));
                let stack_start = self.stack.set(arg_count, Value::from(instance));
                self.push_frame(closure_ptr, stack_start, closure.ip_start, seal)?;
                self.transfer_argument_write_ownership(closure.move_mask, stack_start, arg_count);
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

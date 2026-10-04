//! Forming an anchor, reading through one, and writing through one.

use crate::core::value::{DictKey, FrameGeneration, Value};
use crate::core::objects::{self, ObjInstance, ObjectKind};

use super::properties::{array_element, array_element_index};
use crate::core::native::array::NativeArray;
use super::Vm;
use crate::middle::ir;
#[cfg(debug_assertions)]
use super::RecordedRoot;

struct ValueLocation {
    at: *mut Value,
    witness_set_pool_id: Option<u16>,
}

fn address_value(at: *mut Value) -> Value {
    debug_assert!((at as u64) < 1 << 48, "an address fits below the NaN bits");
    Value::from_bits(at as u64)
}

impl Vm {
    pub(super) fn op_push_slot_anchor(&mut self) {
        let slot = self.read_next() as usize;
        let witness_set_pool_id = self.read_witness_set_pool_id();
        debug_assert_eq!(witness_set_pool_id, self.slot_witness_set_pool_id_at(slot as u8, self.code_index_at(self.ip)),
            "a slot anchor carries its slot's witness set");
        let generation = self.frame_generation();
        self.stack.push(Value::slot_anchor(self.anchor_slot_index(slot), witness_set_pool_id, generation));
    }

    /// Steps from the root on top through each saved key, marking every container it passes, and
    /// pushes an anchor naming the last container and key.
    pub(super) fn op_form_anchor_path(&mut self) -> Result<(), anyhow::Error> {
        let target = self.read_next() as usize;
        let dots = self.read_byte_list();
        let steps = dots.len();
        let frame = self.slot_addr(0);
        if steps > 1 {
            // Each step pushes its key and pops one container, so one slot of room covers the walk.
            if !self.stack.has_room(1) {
                return Err(self.stack_overflow());
            }

            for at in 0..steps - 1 {
                if objects::mark_on_anchor_path(self.stack.peek(0)) {
                    self.anchor_into_ref(self.stack.peek(0))?;
                }
                self.stack.push(unsafe { *frame.add(Value::anchor_key_slot(target, steps, at)) });
                self.step_for_write(dots[at] != 0)?;
            }
        }
        let container = self.stack.pop();
        let key = unsafe { *frame.add(target + Value::ANCHOR_KEY_OFFSET) };
        let field = self.refuse_unanchorable_path(container, key, dots[steps - 1] != 0)?;
        objects::mark_on_anchor_path(container);
        if self.stack.top() < self.lowest_anchor_path {
            self.lowest_anchor_path = self.stack.top();
        }
        let at = unsafe { frame.add(target) };
        unsafe { *at = container };
        let generation = self.frame_generation();
        let anchor = Value::anchor_path(self.stack_index(at), dots[steps - 1] != 0, generation);
        self.stack.push(field.map_or(anchor, |id| anchor.with_anchor_field(id)));
        Ok(())
    }

    /// Refuses when the root a path anchor starts from no longer contains the value it contained
    /// when the anchor was formed. The root is read through an anchor when its slot contains one.
    pub(super) fn op_check_anchor_root(&mut self) -> Result<(), anyhow::Error> {
        let root = self.read_next() as usize;
        let root_is_anchor = self.read_next() != 0;
        let formed_on = self.read_next() as usize;
        let root = unsafe { *self.slot_addr(root) };
        let now = match root_is_anchor {
            true => self.anchor_value(root)?,
            false => root,
        };
        if now != unsafe { *self.slot_addr(formed_on) } {
            return Err(self.detached_anchor_error());
        }
        Ok(())
    }

    pub(super) fn op_record_anchor_root(&mut self) -> Result<(), anyhow::Error> {
        let placed = self.read_next() != 0;
        let root = self.stack.pop();
        #[cfg(debug_assertions)]
        {
            let anchor = self.stack.peek(0);
            let formed_on = self.anchor_value(root)?;
            self.anchor_roots.insert(anchor.anchor_index(), RecordedRoot { root, formed_on, placed });
        }
        #[cfg(not(debug_assertions))]
        let _ = (placed, root);
        Ok(())
    }

    /// The root check the debug modes run in the primitives, against what formation recorded.
    #[cfg(debug_assertions)]
    fn check_recorded_root(&mut self, anchor: Value) -> Result<(), anyhow::Error> {
        let Some(&RecordedRoot { root, formed_on, placed }) = self.anchor_roots.get(&anchor.anchor_index()) else {
            return Ok(());
        };
        if self.anchor_value(root)? == formed_on {
            return Ok(());
        }
        match placed {
            true => self.refuted_elision_error("anchor root"),
            false => Err(self.detached_anchor_error()),
        }
    }

    fn frame_generation(&self) -> FrameGeneration {
        unsafe { (*self.frames.top()).generation }
    }

    fn anchor_slot_index(&self, slot: usize) -> usize {
        self.stack_index(self.slot_addr(slot))
    }

    fn stack_index(&self, at: *mut Value) -> usize {
        unsafe { at.offset_from(self.stack.bottom()) as usize }
    }

    pub(super) fn anchor_slot_addr(&self, anchor: Value) -> *mut Value {
        debug_assert!(anchor.is_anchor(), "an anchor operand holds an anchor");
        let at = unsafe { self.stack.bottom().add(anchor.anchor_index()) };
        #[cfg(debug_assertions)]
        self.assert_frame_still_owns(anchor, at);
        at
    }

    #[cfg(debug_assertions)]
    fn assert_copy_out_location_unchanged(&mut self, anchor: Value, at: *mut Value) {
        let now = self.anchor_location(anchor).ok().map(|location| location.at);
        assert_eq!(now, Some(at), "the value at an anchor moved while its copy was out");
    }

    #[cfg(debug_assertions)]
    fn assert_frame_still_owns(&self, anchor: Value, at: *mut Value) {
        let owner = self.frames.iter().filter(|frame| frame.stack_start <= at).last();
        let Some(owner) = owner else { return };
        assert_eq!(owner.generation, anchor.anchor_generation(),
            "an anchor names slot {} of a frame that is gone", anchor.anchor_index());
    }

    /// The container and key a path anchor names.
    fn anchor_path_pair(&mut self, anchor: Value) -> Result<(Value, Value), anyhow::Error> {
        #[cfg(debug_assertions)]
        self.check_recorded_root(anchor)?;
        let at = self.anchor_slot_addr(anchor);
        let (target, key) = unsafe { (*at, *at.add(Value::ANCHOR_KEY_OFFSET)) };
        if objects::is_detached(target) {
            return Err(self.detached_anchor_error());
        }
        Ok((target, key))
    }

    #[cold]
    #[inline(never)]
    fn detached_anchor_error(&self) -> anyhow::Error {
        self.error_help("The storage this anchor names was replaced", "form the anchor again after the store").unwrap_err()
    }

    #[inline]
    pub(super) fn anchor_value(&mut self, value: Value) -> Result<Value, anyhow::Error> {
        if !value.is_anchor() {
            return Ok(value);
        }
        if !value.is_anchor_path() {
            return Ok(unsafe { *self.anchor_slot_addr(value) });
        }
        let (target, key) = self.anchor_path_pair(value)?;
        self.path_value(value, target, key)
    }

    #[inline]
    fn path_value(&mut self, anchor: Value, target: Value, key: Value) -> Result<Value, anyhow::Error> {
        // An element or field read allocates nothing and calls nothing, so it's done here.
        // The generic read would take the pair back through the stack.
        if let Some(index) = array_element_index(target, key) {
            return Ok(array_element(target, index));
        }
        self.path_member_value(anchor, target, key)
    }

    #[inline(never)]
    fn path_member_value(&mut self, anchor: Value, target: Value, key: Value) -> Result<Value, anyhow::Error> {
        if let Some(id) = anchor.anchor_field() {
            return Ok(unsafe { ObjInstance::get(target.as_object().as_instance_ptr(), id) });
        }
        self.replay_path_read(target, key, anchor.anchor_is_dot())
    }

    /// Reads `target.key` or `target[key]`.
    fn replay_path_read(&mut self, target: Value, key: Value, dot: bool) -> Result<Value, anyhow::Error> {
        self.stack.push(target);
        self.stack.push(key);
        match dot {
            true => self.op_get_property()?,
            false => self.op_get_index()?,
        }
        Ok(self.stack.pop())
    }

    #[inline]
    fn anchor_value_for_write(&mut self, anchor: Value) -> Result<Value, anyhow::Error> {
        if !anchor.is_anchor() {
            return Ok(anchor);
        }
        if !anchor.is_anchor_path() {
            return Ok(unsafe { *self.anchor_slot_addr(anchor) });
        }
        let (target, key) = self.anchor_path_pair(anchor)?;
        objects::mark_elements_shared(target);
        self.path_value(anchor, target, key)
    }

    #[inline]
    pub(super) fn op_load_anchor_for_write(&mut self) -> Result<(), anyhow::Error> {
        let slot = self.read_next() as usize;
        let anchor = unsafe { *self.slot_addr(slot) };
        let value = self.receiver_for_write(anchor)?;
        self.stack.push(value);
        Ok(())
    }

    /// The value a write through `receiver` lands in.
    pub(super) fn receiver_for_write(&mut self, receiver: Value) -> Result<Value, anyhow::Error> {
        let value = self.anchor_value_for_write(receiver)?;
        if !objects::is_shared(value) {
            return Ok(value);
        }
        let forked = self.fork(value);
        if receiver.is_anchor() {
            self.write_through_anchor(receiver, forked)?;
        }
        Ok(forked)
    }

    fn write_through_anchor(&mut self, anchor: Value, value: Value) -> Result<(), anyhow::Error> {
        if !anchor.is_anchor_path() {
            unsafe { *self.anchor_slot_addr(anchor) = value };
            return Ok(());
        }
        let (target, key) = self.anchor_path_pair(anchor)?;
        self.stack.push(target);
        self.stack.push(key);
        self.stack.push(value);
        match anchor.anchor_is_dot() {
            true => self.set_property(true)?,
            false => self.set_index(true)?,
        }
        objects::clear_shared(self.stack.pop());
        Ok(())
    }

    pub(super) fn op_copy_object(&mut self) {
        let value = self.stack.peek(0);
        if !value.is_object() {
            return;
        }
        let copied = self.fork(value);
        self.stack.set(0, copied);
    }

    #[inline]
    pub(super) fn op_load_anchor(&mut self) -> Result<(), anyhow::Error> {
        let slot = self.read_next() as usize;
        let anchor = unsafe { *self.slot_addr(slot) };
        let value = self.anchor_value(anchor)?;
        self.stack.push(value);
        Ok(())
    }

    #[inline]
    pub(super) fn op_store_anchor(&mut self) -> Result<(), anyhow::Error> {
        let slot = self.read_next() as usize;
        let anchor = unsafe { *self.slot_addr(slot) };
        if !anchor.is_anchor_path() {
            let accepted = self.accept_anchor_write(anchor, self.stack.peek(0))?;
            let stored = accepted.value;
            self.write_slot(accepted);
            self.stack.set(0, stored);
            return Ok(());
        }
        let (target, key) = self.anchor_path_pair(anchor)?;
        if let Some(index) = array_element_index(target, key) {
            let stored = self.store_array_element(target, index, self.stack.peek(0))?;
            self.stack.set(0, stored);
            return Ok(());
        }
        self.store_through_member(anchor, target, key)
    }

    pub(super) fn op_copy_anchor_out(&mut self) {
        let anchor_slot = self.read_next();
        let value_slot = self.read_next();
        self.copy_anchor_out(self.slot_addr(0), anchor_slot, value_slot);
    }

    pub(super) fn op_copy_anchor_in(&mut self) -> Result<(), anyhow::Error> {
        let slot = self.read_next() as usize;
        let flags = self.read_next();
        let witness_set_pool_id = self.read_witness_set_pool_id();
        let anchor = unsafe { *self.slot_addr(slot) };
        let location = self.anchor_location(anchor)?;
        let value = unsafe { *location.at };

        // Slot 0 holds a receiver, which has no parameter to compare.
        if slot != 0 {
            self.admit_param_copy(slot - 1, flags, witness_set_pool_id, &location, value)?;
        }

        self.stack.push(value);
        if flags & ir::COPY_IN_WRITTEN != 0 {
            self.stack.push(address_value(location.at));
        }

        Ok(())
    }

    /// Refuses a location the parameter at `position` cannot be anchored to.
    #[inline]
    fn admit_param_copy(&mut self, position: usize, flags: u8, witness_set_pool_id: u16, location: &ValueLocation, value: Value) -> Result<(), anyhow::Error> {
        match location.witness_set_pool_id {
            Some(admits) => {
                debug_assert_ne!(admits, ir::NO_WITNESS_SET, "every slot and field an anchor can name records a witness set");
                if admits != witness_set_pool_id {
                    return Err(self.anchor_admits_mismatch());
                }
            },
            None => {
                if flags & ir::COPY_IN_ADMITS_NO_PERSIST != 0 {
                    return Err(self.anchor_admits_mismatch());
                }
                if objects::needs_accepts_check(value) {
                    self.check_value_accepted(value, self.param_witness_set_pool_id(position))?;
                }
            },
        }
        Ok(())
    }

    /// What the running closure's parameter at `position` admits.
    fn param_witness_set_pool_id(&self, position: usize) -> u16 {
        let closure = unsafe { (*self.frames.top()).closure };
        let param_list_pool_id = unsafe { (*closure).param_list_pool_id } & ir::PARAM_LIST_POOL_ID;
        self.chunk.param_list_pool[param_list_pool_id as usize][position] & !ir::PARAM_IS_ANCHOR
    }

    fn anchor_location(&mut self, anchor: Value) -> Result<ValueLocation, anyhow::Error> {
        if !anchor.is_anchor_path() {
            return Ok(ValueLocation { at: self.anchor_slot_addr(anchor), witness_set_pool_id: Some(anchor.anchor_witness_set_pool_id()) });
        }

        let (target, key) = self.anchor_path_pair(anchor)?;
        if let Some(id) = anchor.anchor_field() {
            let instance = target.as_object().as_instance_ptr();
            let witness_set_pool_id = Some(self.field_witness_set_pool_id(instance, id));
            return Ok(ValueLocation { at: unsafe { ObjInstance::member_ptr(instance, id) }, witness_set_pool_id });
        }

        self.path_location(target, key, anchor.anchor_is_dot())
    }

    /// Where the array element or dict value at `target[key]` is. A field anchor carries its id,
    /// so it never reaches here.
    #[inline]
    fn path_location(&mut self, target: Value, key: Value, dot: bool) -> Result<ValueLocation, anyhow::Error> {
        let at = match target.is_object().then(|| target.as_object().kind()) {
            Some(ObjectKind::Array) => {
                let array = target.as_object().as_array_ptr();
                NativeArray::element_index(key, unsafe { (*array).len } as usize)
                    .map(|index| unsafe { objects::ObjArray::get_ptr(array, index) })
            },
            Some(ObjectKind::Dict) if !dot => {
                let entries = unsafe { &mut (*target.as_object().as_dict_ptr()).entries };
                entries.get_mut(&DictKey(key)).map(|at| at as *mut Value)
            },
            _ => None,
        };
        match at {
            Some(at) => Ok(ValueLocation { at, witness_set_pool_id: None }),
            None => Err(self.unanchorable_path_error(target, key, dot)),
        }
    }

    fn refuse_unanchorable_path(&mut self, target: Value, key: Value, dot: bool) -> Result<Option<u8>, anyhow::Error> {
        let anchorable = match target.is_object().then(|| target.as_object().kind()) {
            Some(ObjectKind::Array) => {
                NativeArray::element_index(key, unsafe { (*target.as_object().as_array_ptr()).len } as usize).is_some()
            },
            Some(ObjectKind::Dict) if !dot => unsafe { &*target.as_object().as_dict_ptr() }.entries.contains_key(&DictKey(key)),
            _ => {
                if objects::is_ref(target) {
                    self.anchor_into_ref(target)?;
                }
                match self.field_at(target, key) {
                    Some((instance, id)) if unsafe { &*(*instance).ty }.anchorable_fields.has(id) => return Ok(Some(id)),
                    Some((instance, id)) => return Err(self.unanchorable_field_error(instance, id)),
                    None => false,
                }
            },
        };
        match anchorable {
            true => Ok(None),
            false => Err(self.unanchorable_path_error(target, key, dot)),
        }
    }

    #[cold]
    #[inline(never)]
    fn anchor_into_ref(&self, container: Value) -> Result<(), anyhow::Error> {
        if !self.get_source_position().is_vm_source() {
            return self.error("Cannot anchor into a `Ref`");
        }
        debug_assert!(super::properties::ref_locked(container.as_object().as_instance_ptr()),
            "the prelude anchors into a Ref only while it holds the lock");
        Ok(())
    }

    #[cold]
    #[inline(never)]
    fn unanchorable_path_error(&mut self, target: Value, key: Value, dot: bool) -> anyhow::Error {
        match self.replay_path_read(target, key, dot) {
            Err(error) => error,
            Ok(_) => self.error("Cannot anchor a method").unwrap_err(),
        }
    }

    #[inline]
    pub(super) fn copy_anchor_out(&mut self, stack_start: *mut Value, anchor_slot: u8, value_slot: u8) {
        let copy = unsafe { stack_start.add(value_slot as usize) };
        let at = unsafe { *copy.add(1) }.to_bits() as *mut Value;
        #[cfg(debug_assertions)]
        self.assert_copy_out_location_unchanged(unsafe { *stack_start.add(anchor_slot as usize) }, at);
        #[cfg(not(debug_assertions))]
        let _ = anchor_slot;
        unsafe { *at = *copy };
    }

    #[inline]
    fn store_through_member(&mut self, anchor: Value, target: Value, key: Value) -> Result<(), anyhow::Error> {
        if let Some(id) = anchor.anchor_field() {
            return self.store_named_field(target.as_object().as_instance_ptr(), id, true);
        }

        let value = self.stack.pop();
        self.stack.push(target);
        self.stack.push(key);
        self.stack.push(value);

        // The element test already failed, so an index store goes straight to the object store.
        match anchor.anchor_is_dot() {
            true => self.set_property(true),
            false => self.set_index_on_object(true),
        }
    }
}
